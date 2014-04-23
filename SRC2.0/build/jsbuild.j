#!/bin/sh

# compile all locally written .java files
#   presumes jsbuild.parser completed OK

. jsbuild.path

FILES=`jsbuild.jlist`

cd $JSIMSRC
jscomp $* $FILES
