#!/bin/bash

logdir=/tmp/${USER}

mkdir testBuild
cd testBuild
mkdir tmp
mkdir build

cvs co -r CMSSW_4_1_6 CMSDIST
cvs co -r V00-16-05-03 PKGTOOLS
cvs co -r lenzip_201106090120 -d GeneratorsStuff UserCode/Lenzip/Generators
#cvs co -d GeneratorsStuff UserCode/Lenzip/Generators

cp GeneratorsStuff/externals/* CMSDIST/

cp GeneratorsStuff/scripts/mydoit.cfg .
cp GeneratorsStuff/scripts/build.sh .

cp mydoit.cfg template_mydoit.cfg
cat template_mydoit.cfg | sed -e "s#XXXX#sherpa#g" > sherpa_mydoit.cfg 
cat template_mydoit.cfg | sed -e "s#XXXX#rivet#g"  > rivet_mydoit.cfg 

cp sherpa_mydoit.cfg mydoit.cfg
./build.sh &> ${logdir}/sherpa_build.log
cp rivet_mydoit.cfg mydoit.cfg
./build.sh &> ${logdir}/rivet_build.log

SHERPA_BASE=${PWD}/build/slc5_amd64_gcc434/external/sherpa/1.3.0/
RIVET_BASE=${PWD}/build/slc5_amd64_gcc434/external/rivet/1.5.1/

cd ..

scram project CMSSW CMSSW_4_1_6
cat CMSSW_4_1_6/config/toolbox/slc5_amd64_gcc434/tools/selected/sherpa.xml | sed -e "s#version=\"1.2.3-cms\"#version=\"1.3.0\"#g" | sed -e "s#\"SHERPA_BASE\" default=\"/afs/cern.ch/cms/slc5_amd64_gcc434/external/sherpa/1.2.3-cms\"#\"SHERPA_BASE\" default=\"${SHERPA_BASE}\"#g" > CMSSW_4_1_6/config/toolbox/slc5_amd64_gcc434/tools/selected/sherpa_new.xml
mv CMSSW_4_1_6/config/toolbox/slc5_amd64_gcc434/tools/selected/sherpa_new.xml CMSSW_4_1_6/config/toolbox/slc5_amd64_gcc434/tools/selected/sherpa.xml

cat CMSSW_4_1_6/config/toolbox/slc5_amd64_gcc434/tools/selected/rivet.xml | sed -e "s#version=\"1.4.0-cms\"#version=\"1.5.1\"#g" | sed -e "s#\"RIVET_BASE\" default=\"/afs/cern.ch/cms/slc5_amd64_gcc434/external/rivet/1.4.0-cms\"#\"RIVET_BASE\" default=\"${RIVET_BASE}\"#g" > CMSSW_4_1_6/config/toolbox/slc5_amd64_gcc434/tools/selected/rivet_new.xml
mv CMSSW_4_1_6/config/toolbox/slc5_amd64_gcc434/tools/selected/rivet_new.xml CMSSW_4_1_6/config/toolbox/slc5_amd64_gcc434/tools/selected/rivet.xml

cd CMSSW_4_1_6
scram setup
scram tool info sherpa
scram tool info rivet

cd src

cvs co -r mn_201106092024 GeneratorInterface/SherpaInterface
cvs co -r V00-00-08 GeneratorInterface/RivetInterface
cp ../../testBuild/GeneratorsStuff/scripts/MC_LES_HOUCHES_SYSTEMATICS.cc GeneratorInterface/RivetInterface/src
cp ../../testBuild/GeneratorsStuff/scripts/rivet_analyze_cfg.py GeneratorInterface/RivetInterface/test
scram b
