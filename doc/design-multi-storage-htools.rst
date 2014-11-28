==================================================
HTools support for multiple storage units per node
==================================================

.. contents:: :depth: 4

This design document describes changes to hbal and related components (first
and foremost LUXI), that will allow it to handle nodes that can't be considered
monolithic in regard to disk layout, for example because they have multiple
different storage units available.

Current state and shortcomings
==============================

Currently the htools assume that there is one storage unit per node and that it can
be arbitrarily split among instances. This leads to problems in clusters
where multiple storage units are present: There might be 10GB DRBD and 10GB
plain storage available on a node, for a total of 20GB. If an instance that
uses 15GB of a single type of storage is requested, it can't actually fit on
the node, but the current implementation of hail doesn't notice this.

This behaviour is clearly wrong, but the problem doesn't arise often in current
setup, due to the fact that instances currently only have a single
storage type.

For the node show action, RAPI only returns

* ``dfree``: The total amount of free disk space
* ``dtotal``: The total amount of disk space

which is insufficient for the same reasons.


Proposed changes
================

Definitions
-----------

* All disks have exactly one *desired storage unit*, which determines where and
  how the disk can be stored. If the disk is transfered, the desired storage
  unit remains unchanged. The desired storage unit includes specifics like the
  volume group in the case of LVM based storage.
* A *storage unit* is a specific storage location on a specific node. Storage
  units have exactly one desired storage unit they can contain. A storage unit
  further has a name, a total capacity, and a free capacity.
* For the purposes of this document a *disk* has a desired storage unit and a size.
* A *disk can be moved* to a node, if there is at least one storage unit on
  that node which can contain the desired storage unit of the disk and if the
  free capacity is at least the size of the disk.
* An *instance can be moved* to a node, if all its disks can be moved there
  one-by-one.

LUXI extension
--------------

The LUXI protocol is extended to include:

* ``storage``: a list of objects (storage units) with
  # Storage type
  # Amount free in MiB
  # Amount total in MiB

.. code-block:: javascript

    {
      "storage": [
        { "stype": ["drbd8", "xenvg", []]
        , "free": 2000,
        , "total": 4000
        },
        { "stype": ["file", "/path/to/storage1", []]
        , "free": 5000,
        , "total": 10000
        },
        { "stype": ["file", "/path/to/storage2", []]
        , "free": 1000,
        , "total": 20000
        },
        { "stype": ["lvm-vg", "xenssdvg", []]
        , "free": 1024,
        , "total": 1024
        }
      ]
    }

is an instance with an LVM volume group mirrored over DRBD, two file storage
directories, one half full, one mostly full, and a non-mirrored volume group.

The storage type ``drbd8`` needs to be added in order to differentiate between
mirrored storage and non-mirrored storage.

IAllocator protocol extension
-----------------------------

The same field is optionally present in the IAllocator protocol:

* a new "storage" column is added, which is a semicolon separated list of
  comma separated fields in the order
  #. ``stype``
  #. ``free``
  #. ``total``

For example:

    drbd8,2000,4000;file,5000,10000;file,1000,20000;lvm-vg,1024,1024

Interpretation
--------------

hbal and hail will use this information only if available, if the data file
doesn't contain the ``storage`` field the old algorithm is used.

If the node information contains the ``storage`` field, hbal and hail will
assume that only the space compatible with the disk's requirements is
available. For an instance to fit a node, all it's disks need to fit there
separately. For a disk to fit a node, a storage unit of the type of
the disk needs to have enough free space to contain it.

Balancing
---------

In order to determine a storage location for an instance, we collect analogous
metrics to the current total node free space metric -- namely the standard deviation
statistic of the free space per storage unit.

The full storage metric for a given desired storage unit is a weighted sum of
the standard deviation metric of the storage units.  The weights of the storage
units are proportional to the total of that storage unit and sum up to the
weight of space in the old implementation (1.0).

This is necessary to

#. Keep the metric compatible.
#. Avoid that the metric of a node with many storage units is dominated by them.

Note that the metric is independent of the storage type to be placed, but the
other types don't change the ranking of the possible placements.
