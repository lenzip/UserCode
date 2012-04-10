#!/bin/sh

export BUILD_HOME=$PWD
export TMPDIR=$BUILD_HOME/tmp

cd $BUILD_HOME/build

export VO_CMS_SW_DIR=$PWD
export SCRAM_ARCH=slc5_amd64_gcc462
export LANG C

time ../PKGTOOLS/cmsBuild -a slc5_amd64_gcc462 --cmsdist ../CMSDIST build rivet professor 
