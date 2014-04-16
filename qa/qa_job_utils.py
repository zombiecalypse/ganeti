#
#

# Copyright (C) 2014 Google Inc.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301, USA.


"""QA utility functions for testing jobs

"""

import re
import threading
import time

from ganeti import constants
from ganeti import locking
from ganeti import utils

import qa_config
import qa_error

from qa_utils import AssertCommand, GetCommandOutput, GetObjectInfo


AVAILABLE_LOCKS = [locking.LEVEL_NODE, ]


def _GetOutputFromMaster(cmd, use_multiplexer=True, log_cmd=True):
  """ Gets the output of a command executed on master.

  """
  if isinstance(cmd, basestring):
    cmdstr = cmd
  else:
    cmdstr = utils.ShellQuoteArgs(cmd)

  # Necessary due to the stderr stream not being captured properly on the
  # buildbot
  cmdstr += " 2>&1"

  return GetCommandOutput(qa_config.GetMasterNode().primary, cmdstr,
                          use_multiplexer=use_multiplexer, log_cmd=log_cmd)


def ExecuteJobProducingCommand(cmd):
  """ Executes a command that contains the --submit flag, and returns a job id.

  @type cmd: list of string
  @param cmd: The command to execute, broken into constituent components.

  """
  job_id_output = _GetOutputFromMaster(cmd)

  possible_job_ids = re.findall("JobID: ([0-9]+)", job_id_output)
  if len(possible_job_ids) != 1:
    raise qa_error.Error("Cannot parse command output to find job id: output "
                         "is %s" % job_id_output)

  return int(possible_job_ids[0])


def _StartDelayFunction(locks, timeout):
  """ Starts the gnt-debug delay option with the given locks and timeout.

  """
  # The interruptible switch must be used
  cmd = ["gnt-debug", "delay", "-i", "--submit", "--no-master"]

  for node in locks.get(locking.LEVEL_NODE, []):
    cmd.append("-n%s" % node)

  cmd.append(str(timeout))

  job_id = ExecuteJobProducingCommand(cmd)
  job_info = GetObjectInfo(["gnt-job", "info", str(job_id)])
  execution_logs = job_info[0]["Opcodes"][0]["Execution log"]

  is_termination_info_fn = \
    lambda e: e["Content"][1] == constants.ELOG_DELAY_TEST
  filtered_logs = filter(is_termination_info_fn, execution_logs)

  if len(filtered_logs) != 1:
    raise qa_error.Error("Failure when trying to retrieve delay termination "
                         "information")

  _, _, (socket_path, ) = filtered_logs[0]["Content"]

  return socket_path


def _TerminateDelayFunction(termination_socket):
  """ Terminates the delay function by communicating with the domain socket.

  """
  AssertCommand("echo a | socat -u stdin UNIX-CLIENT:%s" % termination_socket)


def _GetNodeUUIDMap(nodes):
  """ Given a list of nodes, retrieves a mapping of their names to UUIDs.

  @type nodes: list of string
  @param nodes: The nodes to retrieve a map for. If empty, returns information
                for all the nodes.

  """
  cmd = ["gnt-node", "list", "--no-header", "-o", "name,uuid"]
  cmd.extend(nodes)
  output = _GetOutputFromMaster(cmd)
  return dict(map(lambda x: x.split(), output.splitlines()))


def _FindLockNames(locks):
  """ Finds the ids and descriptions of locks that given locks can block.

  @type locks: dict of locking level to list
  @param locks: The locks that gnt-debug delay is holding.

  @rtype: dict of string to string
  @return: The lock name to entity name map.

  For a given set of locks, some internal locks (e.g. ALL_SET locks) can be
  blocked even though they were not listed explicitly. This function has to take
  care and list all locks that can be blocked by the locks given as parameters.

  """
  lock_map = {}

  if locking.LEVEL_NODE in locks:
    node_locks = locks[locking.LEVEL_NODE]
    if node_locks == locking.ALL_SET:
      # Empty list retrieves all info
      name_uuid_map = _GetNodeUUIDMap([])
    else:
      name_uuid_map = _GetNodeUUIDMap(node_locks)

    for name in name_uuid_map:
      lock_map["node/%s" % name_uuid_map[name]] = name

    # If ALL_SET was requested explicitly, or there is at least one lock
    # Note that locking.ALL_SET is None and hence the strange form of the if
    if node_locks == locking.ALL_SET or node_locks:
      lock_map["node/[lockset]"] = "joint node lock"

  #TODO add other lock types here when support for these is added
  return lock_map


def _GetBlockingLocks():
  """ Finds out which locks are blocking jobs by invoking "gnt-debug locks".

  @rtype: list of string
  @return: The names of the locks currently blocking any job.

  """
  # Due to mysterious issues when a SSH multiplexer is being used by two
  # threads, we turn it off, and block most of the logging to improve the
  # visibility of the other thread's output
  locks_output = _GetOutputFromMaster("gnt-debug locks", use_multiplexer=False,
                                      log_cmd=False)

  # The first non-empty line is the header, which we do not need
  lock_lines = locks_output.splitlines()[1:]

  blocking_locks = []
  for lock_line in lock_lines:
    components = lock_line.split()
    if len(components) != 4:
      raise qa_error.Error("Error while parsing gnt-debug locks output, "
                           "line at fault is: %s" % lock_line)

    lock_name, _, _, pending_jobs = components

    if pending_jobs != '-':
      blocking_locks.append(lock_name)

  return blocking_locks


# TODO: Can this be done as a decorator? Implement as needed.
def RunWithLocks(fn, locks, timeout, *args, **kwargs):
  """ Runs the given function, acquiring a set of locks beforehand.

  @type fn: function
  @param fn: The function to invoke.
  @type locks: dict of string to list of string
  @param locks: The locks to acquire, per lock category.
  @type timeout: number
  @param timeout: The number of seconds the locks should be held before
                  expiring.

  This function allows a set of locks to be acquired in preparation for a QA
  test, to try and see if the function can run in parallel with other
  operations.

  Locks are acquired by invoking a gnt-debug delay operation which can be
  interrupted as needed. The QA test is then run in a separate thread, with the
  current thread observing jobs waiting for locks. When a job is spotted waiting
  for a lock held by the started delay operation, this is noted, and the delay
  is interrupted, allowing the QA test to continue.

  A default timeout is not provided by design - the test creator must make a
  good conservative estimate.

  """
  if filter(lambda l_type: l_type not in AVAILABLE_LOCKS, locks):
    raise qa_error.Error("Attempted to acquire locks that cannot yet be "
                         "acquired in the course of a QA test.")

  # The watcher may interfere by issuing its own jobs - therefore pause it
  AssertCommand(["gnt-cluster", "watcher", "pause", "12h"])

  # Find out the lock names prior to starting the delay function
  lock_name_map = _FindLockNames(locks)

  termination_socket = _StartDelayFunction(locks, timeout)

  qa_thread = threading.Thread(target=fn, args=args, kwargs=kwargs)
  qa_thread.start()

  blocking_owned_locks = []
  test_blocked = False

  try:
    while qa_thread.isAlive():
      blocking_locks = _GetBlockingLocks()
      blocking_owned_locks = \
        set(blocking_locks).intersection(set(lock_name_map))

      if blocking_owned_locks:
        test_blocked = True
        _TerminateDelayFunction(termination_socket)
        break

      # The sleeping time has been set arbitrarily
      time.sleep(5)
  except:
    # If anything goes wrong here, we should be responsible and terminate the
    # delay job
    _TerminateDelayFunction(termination_socket)
    raise

  qa_thread.join()

  if test_blocked:
    blocking_lock_names = map(lock_name_map.get, blocking_owned_locks)
    raise qa_error.Error("QA test succeded, but was blocked by the locks: %s" %
                         ", ".join(blocking_lock_names))
  else:
    _TerminateDelayFunction(termination_socket)

  # Revive the watcher
  AssertCommand(["gnt-cluster", "watcher", "continue"])