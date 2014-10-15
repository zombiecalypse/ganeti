#!/usr/bin/python
#

# Copyright (C) 2006, 2007, 2010, 2012, 2013 Google Inc.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
# 1. Redistributions of source code must retain the above copyright notice,
# this list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
# IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
# PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
# CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


"""Script for unittesting the bdev module"""


import os
import random
import unittest

from ganeti import compat
from ganeti import constants
from ganeti import errors
from ganeti import objects
from ganeti import utils
from ganeti.storage import bdev

import testutils


class TestRADOSBlockDevice(testutils.GanetiTestCase):
  """Tests for bdev.RADOSBlockDevice volumes

  """
  def setUp(self):
    """Set up input data"""
    testutils.GanetiTestCase.setUp(self)

    self.plain_output_old_ok = \
      testutils.ReadTestData("bdev-rbd/plain_output_old_ok.txt")
    self.plain_output_old_no_matches = \
      testutils.ReadTestData("bdev-rbd/plain_output_old_no_matches.txt")
    self.plain_output_old_extra_matches = \
      testutils.ReadTestData("bdev-rbd/plain_output_old_extra_matches.txt")
    self.plain_output_old_empty = \
      testutils.ReadTestData("bdev-rbd/plain_output_old_empty.txt")
    self.plain_output_new_ok = \
      testutils.ReadTestData("bdev-rbd/plain_output_new_ok.txt")
    self.plain_output_new_no_matches = \
      testutils.ReadTestData("bdev-rbd/plain_output_new_no_matches.txt")
    self.plain_output_new_extra_matches = \
      testutils.ReadTestData("bdev-rbd/plain_output_new_extra_matches.txt")
    # This file is completely empty, and as such it's not shipped.
    self.plain_output_new_empty = ""
    self.json_output_ok = testutils.ReadTestData("bdev-rbd/json_output_ok.txt")
    self.json_output_no_matches = \
      testutils.ReadTestData("bdev-rbd/json_output_no_matches.txt")
    self.json_output_extra_matches = \
      testutils.ReadTestData("bdev-rbd/json_output_extra_matches.txt")
    self.json_output_empty = \
      testutils.ReadTestData("bdev-rbd/json_output_empty.txt")
    self.output_invalid = testutils.ReadTestData("bdev-rbd/output_invalid.txt")

    self.volume_name = "d7ab910a-4933-4ffe-88d0-faf2ce31390a.rbd.disk0"
    self.test_unique_id = ("rbd", self.volume_name)
    self.test_params = {
      constants.LDP_POOL: "fake_pool"
      }

  def test_ParseRbdShowmappedJson(self):
    parse_function = bdev.RADOSBlockDevice._ParseRbdShowmappedJson

    self.assertEqual(parse_function(self.json_output_ok, self.volume_name),
                     "/dev/rbd3")
    self.assertEqual(parse_function(self.json_output_empty, self.volume_name),
                     None)
    self.assertEqual(parse_function(self.json_output_no_matches,
                     self.volume_name), None)
    self.assertRaises(errors.BlockDeviceError, parse_function,
                      self.json_output_extra_matches, self.volume_name)
    self.assertRaises(errors.BlockDeviceError, parse_function,
                      self.output_invalid, self.volume_name)

  def test_ParseRbdShowmappedPlain(self):
    parse_function = bdev.RADOSBlockDevice._ParseRbdShowmappedPlain

    self.assertEqual(parse_function(self.plain_output_new_ok,
                     self.volume_name), "/dev/rbd3")
    self.assertEqual(parse_function(self.plain_output_old_ok,
                     self.volume_name), "/dev/rbd3")
    self.assertEqual(parse_function(self.plain_output_new_empty,
                     self.volume_name), None)
    self.assertEqual(parse_function(self.plain_output_old_empty,
                     self.volume_name), None)
    self.assertEqual(parse_function(self.plain_output_new_no_matches,
                     self.volume_name), None)
    self.assertEqual(parse_function(self.plain_output_old_no_matches,
                     self.volume_name), None)
    self.assertRaises(errors.BlockDeviceError, parse_function,
                      self.plain_output_new_extra_matches, self.volume_name)
    self.assertRaises(errors.BlockDeviceError, parse_function,
                      self.plain_output_old_extra_matches, self.volume_name)
    self.assertRaises(errors.BlockDeviceError, parse_function,
                      self.output_invalid, self.volume_name)

  @testutils.patch_object(utils, "RunCmd")
  @testutils.patch_object(bdev.RADOSBlockDevice, "_UnmapVolumeFromBlockdev")
  @testutils.patch_object(bdev.RADOSBlockDevice, "Attach")
  def testRADOSBlockDeviceImport(self, attach_mock, unmap_mock, run_cmd_mock):
    """Test for bdev.RADOSBlockDevice.Import()"""
    # Set up the mock objects return values
    attach_mock.return_value = True
    run_cmd_mock.return_value = \
        utils.RunResult(0, None, "", "", "", utils.process._TIMEOUT_NONE, 0)

    # Create a fake rbd volume
    inst = bdev.RADOSBlockDevice(self.test_unique_id, [], 1024,
                                 self.test_params, {})
    # Desired output command
    import_cmd = [constants.RBD_CMD, "import",
                  "-p", inst.rbd_pool,
                  "-", inst.rbd_name]

    self.assertEqual(inst.Import(), import_cmd)

  @testutils.patch_object(bdev.RADOSBlockDevice, "Attach")
  def testRADOSBlockDeviceExport(self, attach_mock):
    """Test for bdev.RADOSBlockDevice.Export()"""
    # Set up the mock object return value
    attach_mock.return_value = True

    # Create a fake rbd volume
    inst = bdev.RADOSBlockDevice(self.test_unique_id, [], 1024,
                                 self.test_params, {})
    # Desired output command
    export_cmd = [constants.RBD_CMD, "export",
                  "-p", inst.rbd_pool,
                  inst.rbd_name, "-"]

    self.assertEqual(inst.Export(), export_cmd)


class TestExclusiveStoragePvs(unittest.TestCase):
  """Test cases for functions dealing with LVM PV and exclusive storage"""
  # Allowance for rounding
  _EPS = 1e-4
  _MARGIN = constants.PART_MARGIN + constants.PART_RESERVED + _EPS

  @staticmethod
  def _GenerateRandomPvInfo(rnd, name, vg):
    # Granularity is .01 MiB
    size = rnd.randint(1024 * 100, 10 * 1024 * 1024 * 100)
    if rnd.choice([False, True]):
      free = float(rnd.randint(0, size)) / 100.0
    else:
      free = float(size) / 100.0
    size = float(size) / 100.0
    attr = "a-"
    return objects.LvmPvInfo(name=name, vg_name=vg, size=size, free=free,
                             attributes=attr)

  def testGetStdPvSize(self):
    """Test cases for bdev.LogicalVolume._GetStdPvSize()"""
    rnd = random.Random(9517)
    for _ in range(0, 50):
      # Identical volumes
      pvi = self._GenerateRandomPvInfo(rnd, "disk", "myvg")
      onesize = bdev.LogicalVolume._GetStdPvSize([pvi])
      self.assertTrue(onesize <= pvi.size)
      self.assertTrue(onesize > pvi.size * (1 - self._MARGIN))
      for length in range(2, 10):
        n_size = bdev.LogicalVolume._GetStdPvSize([pvi] * length)
        self.assertEqual(onesize, n_size)

      # Mixed volumes
      for length in range(1, 10):
        pvlist = [self._GenerateRandomPvInfo(rnd, "disk", "myvg")
                  for _ in range(0, length)]
        std_size = bdev.LogicalVolume._GetStdPvSize(pvlist)
        self.assertTrue(compat.all(std_size <= pvi.size for pvi in pvlist))
        self.assertTrue(compat.any(std_size > pvi.size * (1 - self._MARGIN)
                                   for pvi in pvlist))
        pvlist.append(pvlist[0])
        p1_size = bdev.LogicalVolume._GetStdPvSize(pvlist)
        self.assertEqual(std_size, p1_size)

  def testComputeNumPvs(self):
    """Test cases for bdev.LogicalVolume._ComputeNumPvs()"""
    rnd = random.Random(8067)
    for _ in range(0, 1000):
      pvlist = [self._GenerateRandomPvInfo(rnd, "disk", "myvg")]
      lv_size = float(rnd.randint(10 * 100, 1024 * 1024 * 100)) / 100.0
      num_pv = bdev.LogicalVolume._ComputeNumPvs(lv_size, pvlist)
      std_size = bdev.LogicalVolume._GetStdPvSize(pvlist)
      self.assertTrue(num_pv >= 1)
      self.assertTrue(num_pv * std_size >= lv_size)
      self.assertTrue((num_pv - 1) * std_size < lv_size * (1 + self._EPS))

  def testGetEmptyPvNames(self):
    """Test cases for bdev.LogicalVolume._GetEmptyPvNames()"""
    rnd = random.Random(21126)
    for _ in range(0, 100):
      num_pvs = rnd.randint(1, 20)
      pvlist = [self._GenerateRandomPvInfo(rnd, "disk%d" % n, "myvg")
                for n in range(0, num_pvs)]
      for num_req in range(1, num_pvs + 2):
        epvs = bdev.LogicalVolume._GetEmptyPvNames(pvlist, num_req)
        epvs_set = compat.UniqueFrozenset(epvs)
        if len(epvs) > 1:
          self.assertEqual(len(epvs), len(epvs_set))
        for pvi in pvlist:
          if pvi.name in epvs_set:
            self.assertEqual(pvi.size, pvi.free)
          else:
            # There should be no remaining empty PV when less than the
            # requeste number of PVs has been returned
            self.assertTrue(len(epvs) == num_req or pvi.free != pvi.size)


class TestLogicalVolume(unittest.TestCase):
  """Tests for bdev.LogicalVolume."""
  def testParseLvInfoLine(self):
    """Tests for LogicalVolume._ParseLvInfoLine."""
    broken_lines = [
      "  toomuch#-wi-ao#253#3#4096.00#2#/dev/abc(20)",
      "  -wi-ao#253#3#4096.00#/dev/abc(20)",
      "  -wi-a#253#3#4096.00#2#/dev/abc(20)",
      "  -wi-ao#25.3#3#4096.00#2#/dev/abc(20)",
      "  -wi-ao#twenty#3#4096.00#2#/dev/abc(20)",
      "  -wi-ao#253#3.1#4096.00#2#/dev/abc(20)",
      "  -wi-ao#253#three#4096.00#2#/dev/abc(20)",
      "  -wi-ao#253#3#four#2#/dev/abc(20)",
      "  -wi-ao#253#3#4096..00#2#/dev/abc(20)",
      "  -wi-ao#253#3#4096.00#2.0#/dev/abc(20)",
      "  -wi-ao#253#3#4096.00#two#/dev/abc(20)",
      ]
    for broken in broken_lines:
      self.assertRaises(errors.BlockDeviceError,
                        bdev.LogicalVolume._ParseLvInfoLine, broken, "#")

    # Examples of good lines from "lvs":
    #  -wi-ao|253|3|4096.00|2|/dev/sdb(144),/dev/sdc(0)
    #  -wi-a-|253|4|4096.00|1|/dev/sdb(208)
    true_out = [
      ("-wi-ao", 253, 3, 4096.00, 2, ["/dev/abc"]),
      ("-wi-a-", 253, 7, 4096.00, 4, ["/dev/abc"]),
      ("-ri-a-", 253, 4, 4.00, 5, ["/dev/abc", "/dev/def"]),
      ("-wc-ao", 15, 18, 4096.00, 32, ["/dev/abc", "/dev/def", "/dev/ghi0"]),
      ]
    for exp in true_out:
      for sep in "#;|":
        pvs = ",".join("%s(%s)" % (d, i * 12) for (i, d) in enumerate(exp[-1]))
        lvs_line = (sep.join(("  %s", "%d", "%d", "%.2f", "%d", "%s")) %
                    (exp[0:-1] + (pvs,)))
        parsed = bdev.LogicalVolume._ParseLvInfoLine(lvs_line, sep)
        self.assertEqual(parsed, exp)

  @staticmethod
  def _FakeRunCmd(success, stdout):
    if success:
      exit_code = 0
    else:
      exit_code = 1
    return lambda cmd: utils.RunResult(exit_code, None, stdout, "", cmd,
                                       utils.process._TIMEOUT_NONE, 5)

  def testGetLvInfo(self):
    """Tests for LogicalVolume._GetLvInfo."""
    self.assertRaises(errors.BlockDeviceError, bdev.LogicalVolume._GetLvInfo,
                      "fake_path",
                      _run_cmd=self._FakeRunCmd(False, "Fake error msg"))
    self.assertRaises(errors.BlockDeviceError, bdev.LogicalVolume._GetLvInfo,
                      "fake_path", _run_cmd=self._FakeRunCmd(True, ""))
    self.assertRaises(errors.BlockDeviceError, bdev.LogicalVolume._GetLvInfo,
                      "fake_path", _run_cmd=self._FakeRunCmd(True, "BadStdOut"))
    good_line = "  -wi-ao|253|3|4096.00|2|/dev/abc(20)"
    fake_cmd = self._FakeRunCmd(True, good_line)
    good_res = bdev.LogicalVolume._GetLvInfo("fake_path", _run_cmd=fake_cmd)
    # If the same line is repeated, the result should be the same
    for lines in [
      [good_line] * 2,
      [good_line] * 3,
      ]:
      fake_cmd = self._FakeRunCmd(True, "\n".join(lines))
      same_res = bdev.LogicalVolume._GetLvInfo("fake_path", fake_cmd)
      self.assertEqual(same_res, good_res)

    # Complex multi-line examples
    one_line = "  -wi-ao|253|3|4096.00|2|/dev/sda(20),/dev/sdb(50),/dev/sdc(0)"
    fake_cmd = self._FakeRunCmd(True, one_line)
    one_res = bdev.LogicalVolume._GetLvInfo("fake_path", _run_cmd=fake_cmd)
    # These should give the same results
    for multi_lines in [
      ("  -wi-ao|253|3|4096.00|2|/dev/sda(30),/dev/sdb(50)\n"
       "  -wi-ao|253|3|4096.00|2|/dev/sdb(200),/dev/sdc(300)"),
      ("  -wi-ao|253|3|4096.00|2|/dev/sda(0)\n"
       "  -wi-ao|253|3|4096.00|2|/dev/sdb(20)\n"
       "  -wi-ao|253|3|4096.00|2|/dev/sdc(30)"),
      ("  -wi-ao|253|3|4096.00|2|/dev/sda(20)\n"
       "  -wi-ao|253|3|4096.00|2|/dev/sdb(50),/dev/sdc(0)"),
      ]:
      fake_cmd = self._FakeRunCmd(True, multi_lines)
      multi_res = bdev.LogicalVolume._GetLvInfo("fake_path", _run_cmd=fake_cmd)
      self.assertEqual(multi_res, one_res)

  @testutils.patch_object(bdev.LogicalVolume, "Attach")
  def testLogicalVolumeImport(self, attach_mock):
    """Tests for bdev.LogicalVolume.Import()"""
    # Set up the mock object return value
    attach_mock.return_value = True

    # Create a fake logical volume
    test_unique_id = ("ganeti",  "31225655-5775-4356-c212-e8b1e137550a.disk0")
    inst = bdev.LogicalVolume(test_unique_id, [], 1024, {}, {})

    # Desired output command
    import_cmd = [constants.DD_CMD,
                  "of=%s" % inst.dev_path,
                  "bs=%s" % constants.DD_BLOCK_SIZE,
                  "oflag=direct", "conv=notrunc"]

    self.assertEqual(inst.Import(), import_cmd)

  @testutils.patch_object(bdev.LogicalVolume, "Attach")
  def testLogicalVolumeExport(self, attach_mock):
    """Test for bdev.LogicalVolume.Export()"""
    # Set up the mock object return value
    attach_mock.return_value = True

    # Create a fake logical volume
    test_unique_id = ("ganeti",  "31225655-5775-4356-c212-e8b1e137550a.disk0")
    inst = bdev.LogicalVolume(test_unique_id, [], 1024, {}, {})

    # Desired output command
    export_cmd = [constants.DD_CMD,
                  "if=%s" % inst.dev_path,
                  "bs=%s" % constants.DD_BLOCK_SIZE,
                  "count=%s" % inst.size,
                  "iflag=direct"]

    self.assertEqual(inst.Export(), export_cmd)


class TestPersistentBlockDevice(testutils.GanetiTestCase):
  """Tests for bdev.PersistentBlockDevice volumes

  """
  def testPersistentBlockDeviceImport(self):
    """Test case for bdev.PersistentBlockDevice.Import()"""
    # Create a fake block device
    test_unique_id = (constants.BLOCKDEV_DRIVER_MANUAL, "/dev/abc")
    inst = bdev.PersistentBlockDevice(test_unique_id, [], 1024, {}, {})

    self.assertRaises(errors.BlockDeviceError,
                      bdev.PersistentBlockDevice.Import, inst)


if __name__ == "__main__":
  testutils.GanetiTestProgram()
