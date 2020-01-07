#!/bin/sh
killall beam.smp
cd /home/arksu/a1server/ebin_make
/usr/local/bin/erl -detached -eval 'application:start(a1).'
