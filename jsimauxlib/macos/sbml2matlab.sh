#!/bin/sh

# wrapper for sbtranslate
#  needed since forked process from JSim does not autoset DYLD_LIBRARY_PATH

DIR=`echo $0 | sed s/sbml2matlab.sh//`

cd $DIR

export DYLD_LIBRARY_PATH=$DIR

./sbml2matlab $*
