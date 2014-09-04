Ganeti Monitoring Plugins
=========================

The current system has a fixed number of data collectors that are reported, but
new additions like activation state allows for greater flexibility.
This document outlines a very simple and flexible plugin architecture for
adding custom data collectors.

Creating a custom data collector
--------------------------------

A custom data collector will be any path to an executable on the nodes and
communicate via the stdout. This executable is run when the data of the
collector is requested, namely on a request to ``/1/report/all`` or
``/1/report/<category>/<collector>`` for the MonD. It must return a valid JSON
reponse as its only output on stdout. If it fails to do so, an error message
will be returned instead.

There will be stateless and stateful collectors, where stateful collectors
have an additional command that is run in the configured interval to collect
the available data. They must not run longer than the timeout (suggested:
0.1s) and will be killed otherwise.

Setting up a custom data collector
----------------------------------

A new data collector is registered with ``gnt-cluster modify`` as follows:

  gnt-cluster modify \
    --custom-data-collector=name=<NAME>,report=<PATH>[,update=<PATH>]

The category is set to "custom", so the collector will be available as
/1/report/custom/<NAME> in MonD. NAME can't be any of the buildin data
collectors. If it is the name of an existing custom data collector, then that
data collector will be overwritten. To delete a custom data collector from the
reporting, use

  gnt-cluster modify --delete-custom-data-collector=<NAME>
