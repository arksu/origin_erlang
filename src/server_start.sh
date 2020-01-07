#!/bin/sh
rm -f origin.log
rm -f collision.log
rm -f visible.log
rm -f grid_processor.log
erl -eval 'application:start(a1).'
