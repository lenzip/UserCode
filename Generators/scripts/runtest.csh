#!/bin/csh

#set verbose

set jobn=`date +%b%d-%H%M%S`
set config=`echo ${1} | cut -f 1 -d'.'`

set OUTPUT=runtest_${config}_${jobn}.log

set WORKDIR=`pwd`

date >& $OUTPUT
uname -a >>& $OUTPUT 
cat /etc/redhat-release >>& $OUTPUT
echo "Current working directory: " $WORKDIR >>& $OUTPUT 

cd $CMSSW_BASE/src; cmsenv; cd $WORKDIR

showtags >>& $OUTPUT

edmPluginRefresh >>& $OUTPUT

time cmsRun -p ${1} >>& $OUTPUT

exit 0
