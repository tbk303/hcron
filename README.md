# hcron

A simple job scheduler for Haskell, which just runs some IO () action
at a given time.

Currently, most of the code is just a copy of the excellent cron package
found in the BuildBox package (http://hackage.haskell.org/package/BuildBox)
by Ben Lippmeier, slightly adjusted to execute arbitrary IO () actions.

Currently, no parallelism/concurrency is involved at all, the scheduler just
runs jobs and blocks while doing so. Adding support for these is one of the
major goals for the future. Nevertheless, you can run the whole scheduler
as some kind of worker by just forking it off using forkIO/forkOS.

License: BSD3, see LICENSE
