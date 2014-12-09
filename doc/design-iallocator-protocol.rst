=================================
Extensions to iallocator protocol
=================================

.. contents:: :depth: 4

This document describes how extensions in the protocol of iallocator are
handled. This makes some implicit assumptions explicit and will allow tools to
be forward compatible.

Current state
=============

The current state specifies the fields that the iallocator protocol will be
provided. While hail ignores fields that are not required, the protocol does
not currently state how to handle the fields. Strictly speaking a valid
implementation could therefore break by adding fields to the protocol.

Adding extensions
=================

Every extension must have a unique name containing only lowercase letters and
underscore. Every extension can add fields to existing objects or add new
types of objects.

Currently the inputs for the htools input data can be in the following forms:

* RAPI
* LUXI
* IAllocator JSON
* textual format
* simulated description (for testing)

Versioning in RAPI and LUXI assumes that the version of the htool and the
cluster correspond. Only the dumped formats need to be cross-version.

This leaves two protocols that are not explicitly extendable:

* The IAllocator JSON protocol
* The textual format

JSON protocol
-------------

We require users of the JSON interface to ignore all fields that are unknown to
them, thus enabling versions to read extended data formats.

If the extension adds a new type of object, it should be contained in the
top-level field with the extension name.

Textual data protocol
---------------------

The old protocol statically assumes that there are 4 or 5 tables, where the
pipe character is a reserved char. Further the code assumes that the number of
fields bound from above. This means that extensions necessarily break the old
interface (old htools will not be able to read new textual data files).

Sections are, as previously, separated by a single empty line.

Every section is a single table of ``|`` separated columns with newline
separated rows.

New data will be added either new sections or to the sections. The sections
currently are:

# Groups
# Nodes
# Instances
# Tags
# Optional: Ipolicy

Newly added sections must be optional and the implementation must provide
sensible defaults for new sections. New sections are added at the end of the
file. If a section is empty or the file ends before the section, the default
value is used.

Similarly new columns in existing tables are added at the end and must be
optional.

Example:

    group-00|fake-uuid-00|preferred||

    node-00|262144|65536|196608|2097152|2097152|8|N|fake-uuid-00|10||Y|10

    inst-00|128|0|1|running|Y|node-00||ext||1
    inst-01|128|0|1|running|Y|node-00||ext||1

    htools:migration:hv


    extension-data|1||3|4

This data file has an empty ipolicy section, but provides some extension data.
``||`` is an empty cell, where the default value is used.
