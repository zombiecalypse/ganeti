#!/usr/bin/python
#

# Copyright (C) 2014 Google Inc.
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


"""Tests for the instance utilities

"""

import mock
import unittest

from ganeti import constants
from ganeti.cmdlib import instance_utils

from testsupport import *

import testutils


def Disk(dev_type):
  return mock.Mock(dev_type=dev_type)


def Drbd():
  return Disk(constants.DT_DRBD8)


def Rbd():
  return Disk(constants.DT_RBD)


class AllDiskTemplateTest(unittest.TestCase):
  def testAllDiskless(self):
    self.assertTrue(instance_utils.AllDiskOfType([], [constants.DT_DISKLESS]))

  def testOrDiskless(self):
    self.assertTrue(instance_utils.AllDiskOfType(
        [], [constants.DT_DISKLESS, constants.DT_DRBD8]))

  def testOrDrbd(self):
    self.assertTrue(instance_utils.AllDiskOfType(
        [Drbd()], [constants.DT_DISKLESS, constants.DT_DRBD8]))

  def testOrRbd(self):
    self.assertTrue(instance_utils.AllDiskOfType(
        [Rbd()], [constants.DT_RBD, constants.DT_DRBD8]))

  def testNotRbd(self):
    self.assertFalse(instance_utils.AllDiskOfType(
        [Rbd()], [constants.DT_DRBD8]))

  def testNotDiskless(self):
    self.assertFalse(instance_utils.AllDiskOfType(
        [], [constants.DT_DRBD8]))

  def testNotRbdDiskless(self):
    self.assertFalse(instance_utils.AllDiskOfType(
        [Rbd()], [constants.DT_DISKLESS]))

  def testHeterogeneous(self):
    self.assertFalse(instance_utils.AllDiskOfType(
        [Rbd(), Drbd()], [constants.DT_DRBD8]))

  def testHeterogeneousDiskless(self):
    self.assertFalse(instance_utils.AllDiskOfType(
        [Rbd(), Drbd()], [constants.DT_DISKLESS]))


class AnyDiskTemplateTest(unittest.TestCase):
  def testAnyDiskless(self):
    self.assertTrue(instance_utils.AnyDiskOfType([], [constants.DT_DISKLESS]))

  def testOrDiskless(self):
    self.assertTrue(instance_utils.AnyDiskOfType(
        [], [constants.DT_DISKLESS, constants.DT_DRBD8]))

  def testOrDrbd(self):
    self.assertTrue(instance_utils.AnyDiskOfType(
        [Drbd()], [constants.DT_DISKLESS, constants.DT_DRBD8]))

  def testOrRbd(self):
    self.assertTrue(instance_utils.AnyDiskOfType(
        [Rbd()], [constants.DT_RBD, constants.DT_DRBD8]))

  def testNotRbd(self):
    self.assertFalse(instance_utils.AnyDiskOfType(
        [Rbd()], [constants.DT_DRBD8]))

  def testNotDiskless(self):
    self.assertFalse(instance_utils.AnyDiskOfType(
        [], [constants.DT_DRBD8]))

  def testNotRbdDiskless(self):
    self.assertFalse(instance_utils.AnyDiskOfType(
        [Rbd()], [constants.DT_DISKLESS]))

  def testHeterogeneous(self):
    self.assertTrue(instance_utils.AnyDiskOfType(
        [Rbd(), Drbd()], [constants.DT_DRBD8]))

  def testHeterogeneousDiskless(self):
    self.assertFalse(instance_utils.AnyDiskOfType(
        [Rbd(), Drbd()], [constants.DT_DISKLESS]))


if __name__ == "__main__":
  testutils.GanetiTestProgram()
