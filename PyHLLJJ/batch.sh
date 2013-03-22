#!/bin/bash
dir=$1
config=$2
startevent=$3

cd /afs/cern.ch/user/l/lenzip/work/CMG/CMGTools/production2/CMGTools/CMSSW_5_3_3_patch3/src/HiggsAna/PyHLLJJ
eval `scram runtime -sh`
mkdir -p $dir
python  ../../CMGTools/RootTools/python/fwlite/MultiLoop.py $dir $config -N 30000 -p 0 -i $startevent -f &> $dir/log${startevent}.txt 
