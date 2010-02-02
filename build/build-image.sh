#!/bin/sh
# Build scsh's image.

srcdir=$1
lib=$2
image=$3
scheme48=$4
loads=$5
opens=$6

$scheme48 -a batch <<EOF
,translate =scshexternal/ $lib
,config ,load $srcdir/scsh/scsh-read.scm
,config ,open scsh-reader
,config ,set-reader scsh-read
,open scsh-reader
,set-reader scsh-read
,config ,load $loads
,open $opens
,load-package scheme-with-scsh
(dump-scsh "$image")
EOF
