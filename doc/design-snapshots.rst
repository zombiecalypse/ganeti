=======================
Ganeti snapshot support
=======================

This design document outlines a new feature for ganeti, which will allow
simple interface to take snapshots of disks.

Current state and shortcomings
==============================

Currently there is no way native to ganeti to snapshot disks or instances. If
the underlying disk type supports the operation, it is possible to do it
manually, but the user will have to find the corresponding LVM volumes
manually and take special care if they are DRBD volumes.

Backups of instances are hard to do, because they will either be copied in an
inconsistent state or have to be taken offline.

Proposed changes
================

There will be a ``snapshot`` command for `gnt-disk` and `gnt-instance` with
the following semantics:

  gnt-disk snapshot --snapshot-name=backup-diskname diskname

1. If the disk is DRBD or plain, a new device is created that contains the
   snapshot.
2. If the disk is a sharedfile or a file, then snapshotting is only possible
   on a disk that is detached or attached to an instance that is offline. If
   that is the case, a copy will be created.
3. If the disk is RBD, a ceph snapshot is created as a file.
4. If the disk is a Gluster device, snapshotting is currently not possible,
   but the gluster team works on this functionality. S

In any case,
