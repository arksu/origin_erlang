#!/bin/sh
test -d bin && echo "bin dir ok" || mkdir bin

cd src/ports/
make
cd ../..
