#! /usr/bin/env python

import os, sys, logging, re
import tempfile
import getopt
import string
import threading, Queue
import subprocess

from optparse import OptionParser
from stat import *

class CommandBuilder:
  def __init__(self, name):
    self._name = name
  def build(self):
    try:
      os.cd(self._name)
      inputfile = open('job'+name+'.sh', 'w')
      infile += '#!/bin/bash'
      infile += 'echo "hello world from "'+name
    except Exception, e:
      print "Error: %s" % str(e)  
      
class LocalCommandBuilder(CommandBuilder):
  def __init__(self, name, directory, type, nevents, extension = '', cmsgen = ''):
    CommandBuilder.__init__(self, name);
    self._type=type
    self._nevents=nevents
    self._directory=directory 
    self._extension=extension
    self._cmsgen=cmsgen
  def build(self):
    if not (self._type is 'GEN' or self._type is 'REC'):
      print 'type in LocalCommandBuilder can be either GEN or REC, not '+str(self._type)
      return
    try:
      script = open(self._directory+'/job'+self._name+self._type+'.sh', 'w') 
      infile=''
      infile += '#!/bin/bash\n'
      infile += 'export SCRAM_ARCH=slc5_amd64_gcc434\n'
      if self._cmsgen != '':
        infile += 'echo '+self._cmsgen+' > cmsgen.cfg\n'
        infile += 'cmsGen.py --generator sherpa --number-of-events 1 --cfg cmsgen.cfg\n'
      if self._type is 'GEN':
        infile += 'cmsDriver.py Configuration/GenProduction/python/'+self._name+'.py -s GEN,VALIDATION:genvalid  --conditions auto:mc --datatier GEN-SIM-RAW --eventcontent RAWSIM --no_exec -n '+str(self._nevents)+' '+self._extension+'\n'
        infile += 'cat '+self._name+'_py_GEN_VALIDATION.py | sed -e "s#input = cms\.untracked#output = cms\.untracked#g" > tmp.py\n'
        infile += 'mv tmp.py '+self._name+'_py_GEN.py\n'
        infile += 'cmsRun '+self._name+'_py_GEN.py &>log_GEN.txt\n'
        infile += 'cmsDriver.py step3 -s HARVESTING:genHarvesting --harvesting AtJobEnd --conditions auto:mc  --mc --filein file:'+self._name+'_py_GEN_VALIDATION.root\n' 
      elif self._type is 'REC':
        infile += 'cmsDriver.py Configuration/GenProduction/python/'+self._name+'.py -s GEN,SIM --customise Configuration/GenProduction/timing_customize.py --conditions auto:mc --datatier GEN-SIM-RAW --eventcontent RAWSIM --no_exec -n '+str(self._nevents)+' '+self._extension+'\n'
        #infile += 'cmsRun '+self._name+'_py_GEN_SIM_DIGI_L1_DIGI2RAW_HLT.py &>log_REC.txt\n'
        infile += 'cat '+self._name+'_py_GEN_SIM.py | sed -e "s#input = cms\.untracked#output = cms\.untracked#g" > tmp.py\n'
        infile += 'mv tmp.py '+self._name+'_py_GEN_SIM.py\n' 
        infile += 'cmsRun '+self._name+'_py_GEN_SIM.py &>log_REC.txt\n'
      script.write(infile)
      os.chmod(self._directory+'/job'+self._name+self._type+'.sh', 0755)
      script.close()
      return str('cd '+self._directory+'; ./job'+self._name+self._type+'.sh')  
    except Exception, e:
      print "Error: %s" % str(e) 


class LXBATCHCommandBuilder(CommandBuilder):
  def __init__(self, name, directory, type, nevents, extension = '', cmsgen = ''):
    CommandBuilder.__init__(self, name);
    self._type=type
    self._nevents=nevents
    self._directory=directory
    self._extension=extension
    self._cmsgen=cmsgen
  def build(self):
    if not (self._type is 'GEN' or self._type is 'REC'):
      print 'type in LocalCommandBuilder can be either GEN or REC, not '+str(self._type)
      return
    try:
      cmsswbase = os.getenv('CMSSW_BASE')
      if cmsswbase is None:
        print "you have to run cmsenv in your area first!"
        return
      script = open(self._directory+'/job'+self._name+self._type+'.sh', 'w')
      infile=''
      infile += '#!/bin/bash\n'
      infile += 'export SCRAM_ARCH=slc5_amd64_gcc434\n'
      infile += 'pwd=`pwd`\n'
      infile += 'echo $pwd\n'
      infile += 'cd '+cmsswbase+'\n'
      infile += 'eval `scram runtime -sh`\n'
      infile += 'cd -\n'
      if self._cmsgen != '':
        infile += 'echo '+self._cmsgen+' > cmsgen.cfg\n'
        infile += 'cmsGen.py --generator sherpa --number-of-events 1 --cfg cmsgen.cfg\n'
      if self._type is 'GEN':
        #infile += 'cmsDriver.py Configuration/GenProduction/python/'+self._name+'.py -s GEN --customise Configuration/GenProduction/validation_customize.py --conditions START38_V8::All --datatier GEN-SIM-RAW --eventcontent RAWSIM --no_exec -n '+str(self._nevents)+' '+self._extension+'\n'
        infile += ''
        infile += 'cmsDriver.py Configuration/GenProduction/python/'+self._name+'.py -s GEN,VALIDATION:genvalid_dy  --conditions auto:mc --datatier GEN-SIM-RAW --eventcontent RAWSIM --no_exec -n '+str(self._nevents)+' '+self._extension+'\n'
        #infile += 'cat '+self._name+'_py_GEN.py | sed -e "s#input = cms\.untracked#output = cms\.untracked#g" > tmp.py\n'
        #infile += 'mv tmp.py '+self._name+'_py_GEN.py\n'
        infile += 'cmsRun '+self._name+'_py_GEN_VALIDATION.py &>log_GEN.txt\n'
        infile += 'cmsDriver.py step3 -s HARVESTING:genHarvesting --harvesting AtJobEnd --conditions auto:mc  --mc --filein file:'+self._name+'_py_GEN_VALIDATION.root\n'
      elif self._type is 'REC':
        #infile += 'cmsDriver.py Configuration/GenProduction/python/'+self._name+'.py -s GEN,SIM,DIGI,L1,DIGI2RAW,HLT --customise Configuration/GenProduction/timing_customize.py --conditions START38_V8::All --datatier GEN-SIM-RAW --eventcontent RAWSIM --no_exec -n '+str(self._nevents)+' '+self._extension+'\n'
        infile += 'cmsDriver.py Configuration/GenProduction/python/'+self._name+'.py -s GEN,SIM --customise Configuration/GenProduction/timing_customize.py --conditions auto:mc --datatier GEN-SIM-RAW --eventcontent RAWSIM --no_exec -n '+str(self._nevents)+' '+self._extension+'\n'
        #infile += 'cmsRun '+self._name+'_py_GEN_SIM_DIGI_L1_DIGI2RAW_HLT.py &>log_REC.txt\n'
        infile += 'cat '+self._name+'_py_GEN_SIM.py | sed -e "s#input = cms\.untracked#output = cms\.untracked#g" > tmp.py\n'
        infile += 'mv tmp.py '+self._name+'_py_GEN_SIM.py\n'  
        infile += 'cmsRun '+self._name+'_py_GEN_SIM.py &>log_REC.txt\n'
      
      infile += 'ls\n'
      castordir = '/castor/cern.ch/cms/store/cmst3/user/lenzip/Summer11/'
      infile += 'rfmkdir '+castordir+self._directory+'\n'
      infile += 'rfcp *.py '+castordir+self._directory+'\n'
      #infile += 'scp *.py luana:/data/lenzip/generatorValidation/Production412_patch1/'+self._directory+'\n'
      infile += 'for i in `ls *.root`; do rfcp $i '+castordir+self._directory+';done\n'
      #infile += 'scp *.root luana:/data/lenzip/generatorValidation/Production412_patch1/'+self._directory+'\n'
      infile += 'rfcp *.txt '+castordir+self._directory+'\n'
      #infile += 'scp *.txt luana:/data/lenzip/generatorValidation/Production412_patch1/'+self._directory+'\n'
      script.write(infile)
      os.chmod(self._directory+'/job'+self._name+self._type+'.sh', 0755)
      script.close()
      return str('cd '+self._directory+'; bsub -q 1nd job'+self._name+self._type+'.sh')
    except Exception, e:
      print "Error: %s" % str(e) 


class Worker(threading.Thread):
  def __init__(self, queue):
    threading.Thread.__init__(self)
    self.queue = queue
    self.status = -1
  def run(self):
    while True:
      try:
        command = self.queue.get()
        #print command
        process = subprocess.Popen(command, shell=True, stdout=subprocess.PIPE)
        process.wait()
        self.status = process.returncode
        print 'task finished with exit code '+str(self.status)
        self.queue.task_done()
      except Queue.Empty, e:
        break
      except Exception, e:
        print "Error: %s" % str(e)




numcores = os.sysconf('SC_NPROCESSORS_ONLN')
if numcores is None:
  numcores = 1

usage = "usage: %prog list [options]"
parser = OptionParser(usage=usage)
parser.add_option("--genonly",action="store_true", help="run only the gen processes (default=False)", default=False)
parser.add_option("--reconly",action="store_true", help="run only the rec processes (default=False)", default=False)
parser.add_option("--events-for-gen", action="store", help="number of events in the gen step (default=10000)", default=10000)
parser.add_option("--events-for-rec", action="store", help="number of events in the rec step (default=10)", default=10)
parser.add_option('-f', "--force", action="store_true", help="overwrite old results (default=False)", default=False)
parser.add_option('-c', "--cores", action="store", help="number of cores to use (default=%s)" %numcores, default=numcores)
parser.add_option('-b', "--batch", action="store_true", help="run on LXBATCH", default=False)
parser.add_option("-n", '--negate', action="store_true", help="do nothing, just prepare the jobs", default=False)


(options, args) = parser.parse_args()
if len(args)<1 :
  parser.print_help()
  sys.exit(1)

if options.genonly and options.reconly:
  print "Configuration Error: you cannot use both --genonly and --reconly"
  parser.print_help()
  sys.exit(1)
           
try:
  file = open(args[0], 'r')
except Exception, e:
  print "Error: %s" % str(e)
  sys.exit(1)


queue = Queue.Queue()

#start the workers
for i in range(int(options.cores)):
  worker = Worker(queue)
  worker.setDaemon(True)
  worker.start()

#fill the queue
for line in file:
  stripline = line.rstrip('\n')
  print stripline
  
  newstripline = stripline
  #check if the directory name starts with a number --> it is an MCDB article
  namecomponents = stripline.split(':')
  cmsgen = ""
  if len(namecomponents) == 0:
    print 'something wrong while parsing '+stripline+'. Doing nothing'
    continue
  if len(namecomponents) > 1 and "sherpa" in namecomponents[1]:
    print "this needs cmsGen"
    cmsgen = namecomponents[1]
    print "sherpack path "+cmsgen
    stripline = namecomponents[0]
    newstripline = stripline

    
 

  dummy=None

  try:
    dummy = int(namecomponents[0])
    newstripline = stripline.lstrip('0123456789_')
    print 'This is an MCDB article'
  except:
    print 'this is not an MCDB article'


  print newstripline
  extension = '' if dummy is None else '--filein=mcdb:'+namecomponents[0]  

  if os.path.exists(stripline) and options.force is False:
    print 'Directory '+stripline+' exists already, doing nothing.'
    print 'You can overwrite with the --force option'
    continue
  
  if not os.path.exists(stripline):
    os.mkdir(stripline)
  
  if not options.reconly:
    commandBuilder = LocalCommandBuilder(newstripline, stripline, 'GEN', options.events_for_gen, extension, cmsgen) if not options.batch else LXBATCHCommandBuilder(newstripline, stripline, 'GEN', options.events_for_gen, extension, cmsgen) 
    command = commandBuilder.build()
    if command is None:
      print 'problem building GEN command for '+stripline
    else: 
      if not options.negate:
        queue.put(command)

  if not options.genonly:
    commandBuilder = LocalCommandBuilder(newstripline, stripline, 'REC', options.events_for_rec, extension, cmsgen) if not options.batch else LXBATCHCommandBuilder(newstripline, stripline, 'REC', options.events_for_rec, extension,cmsgen)
    command = commandBuilder.build() 
    if command is None:
      print 'problem building REC command for '+stripline
    else:
      if not options.negate:
        queue.put(command)  
    
  
if not options.negate:
  queue.join()

  
