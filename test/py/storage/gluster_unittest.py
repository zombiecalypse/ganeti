#!/usr/bin/python
#

# Copyright (C) 2013 Google Inc.
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

"""Script for unittesting the ganeti.storage.gluster module"""

import os
import shutil
import tempfile
import unittest
import mock

from ganeti import errors
from ganeti.storage import filestorage
from ganeti.storage import gluster
from ganeti import utils

import testutils

class TestGlusterVolume(testutils.GanetiTestCase):

  testAddrIpv = {4: "203.0.113.42",
                 6: "2001:DB8::74:65:28:6:69",
                }

  @staticmethod
  def _MakeVolume(addr=None, port=9001,
                  run_cmd=NotImplemented,
                  vol_name="pinky"):

    addr = addr if addr is not None else TestGlusterVolume.testAddrIpv[4]

    return gluster.GlusterVolume(addr, port, vol_name, _run_cmd=run_cmd,
                                 _mount_point="/invalid")

  def setUp(self):
    testutils.GanetiTestCase.setUp(self)

    # Create some volumes.
    self.vol_a = TestGlusterVolume._MakeVolume()
    self.vol_a_clone = TestGlusterVolume._MakeVolume()
    self.vol_b = TestGlusterVolume._MakeVolume(vol_name="pinker")

  def testEquality(self):
    self.assertEqual(self.vol_a, self.vol_a_clone)

  def testInequality(self):
    self.assertNotEqual(self.vol_a, self.vol_b)

  def testHostnameResolution(self):
    vol_1 = TestGlusterVolume._MakeVolume(addr="localhost")
    self.assertTrue(vol_1.server_ip in ["127.0.0.1", "::1"],
                    msg="%s not an IP of localhost" % (vol_1.server_ip,))
    self.assertRaises(errors.ResolverError, lambda: \
      TestGlusterVolume._MakeVolume(addr="E_NOENT"))

  def testKVMMountStrings(self):
    # The only source of documentation I can find is:
    #   https://github.com/qemu/qemu/commit/8d6d89c
    # This test gets as close as possible to the examples given there,
    # within the limits of our implementation (no transport specification,
    #                                          no default port version).

    vol_1 = TestGlusterVolume._MakeVolume(addr=TestGlusterVolume.testAddrIpv[4],
                                          port=24007,
                                          vol_name="testvol")
    self.assertEqual(
      vol_1.GetKVMMountString("dir/a.img"),
      "gluster://203.0.113.42:24007/testvol/dir/a.img"
    )

    vol_2 = TestGlusterVolume._MakeVolume(addr=TestGlusterVolume.testAddrIpv[6],
                                          port=24007,
                                          vol_name="testvol")
    self.assertEqual(
      vol_2.GetKVMMountString("dir/a.img"),
      "gluster://[2001:db8:0:74:65:28:6:69]:24007/testvol/dir/a.img"
    )

    vol_3 = TestGlusterVolume._MakeVolume(addr="localhost",
                                          port=9001,
                                          vol_name="testvol")
    kvmMountString = vol_3.GetKVMMountString("dir/a.img")
    self.assertTrue(
      kvmMountString in
      ["gluster://127.0.0.1:9001/testvol/dir/a.img",
       "gluster://[::1]:9001/testvol/dir/a.img"],
      msg="%s is not volume testvol/dir/a.img on localhost" % (kvmMountString,)
    )

  def testFUSEMountStrings(self):
    vol_1 = TestGlusterVolume._MakeVolume(addr=TestGlusterVolume.testAddrIpv[4],
                                          port=24007,
                                          vol_name="testvol")
    self.assertEqual(
      vol_1._GetFUSEMountString(),
      "203.0.113.42:24007:testvol"
    )

    vol_2 = TestGlusterVolume._MakeVolume(addr=TestGlusterVolume.testAddrIpv[6],
                                          port=24007,
                                          vol_name="testvol")
    # This _ought_ to work. https://bugzilla.redhat.com/show_bug.cgi?id=764188
    self.assertEqual(
      vol_2._GetFUSEMountString(),
      "2001:db8:0:74:65:28:6:69:24007:testvol"
    )

    vol_3 = TestGlusterVolume._MakeVolume(addr="localhost",
                                          port=9001,
                                          vol_name="testvol")
    fuseMountString = vol_3._GetFUSEMountString()
    self.assertTrue(fuseMountString in
                    ["127.0.0.1:9001:testvol", "::1:9001:testvol"],
                    msg="%s not testvol on localhost:9001" % (fuseMountString,))


if __name__ == "__main__":
  testutils.GanetiTestProgram()
