#!/bin/sh

srcdir=$1

./go <<EOF
,config ,load $srcdir/test/test-packages.scm
,open test-all
(test-all)
,exit
EOF
echo ""

# shouldn't need the last ,exit, but running with it for now