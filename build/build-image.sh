#!/bin/sh
# Build scsh's image.

srcdir=$1
lib=$2
image=$3
scheme48=$4
loads=$5

$scheme48 -a batch <<EOF
,translate =scshexternal/ $lib
,config ,load $srcdir/scheme/scsh-read.scm
,config ,open scsh-reader
,config ,set-reader scsh-read
,config ,load $loads
,user-package-is scsh-user
,user
,open scsh-reader
,set-reader scsh-read
(dump-scsh "$image")
EOF
