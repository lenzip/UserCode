#! /usr/bin/env python

import os, sys, logging, re
import tempfile
import getopt
import string
import threading, Queue
import subprocess

from optparse import OptionParser, Option
from stat import *

class CommandBuilder:
  def __init__(self,request, type, nevents, aqcut):
    self._request=request
    self._type = type
    self._nevents=nevents
    self._aqcut=aqcut
    self._outfilesroot=self._request._name+self._type+str(self._aqcut)
  
  def buildcmsDriver(self):
    if self._type is 'GEN':
      cmsdriver = 'cmsDriver.py '+self._request._pythonConfig+ \
                  ' -s GEN  --conditions auto:mc --datatier GEN-SIM --eventcontent RAWSIM --no_exec -n '+ \
                  str(self._nevents) + ' --python_filename '+ self._outfilesroot+'.py '+ \
                  ' --fileout '+self._outfilesroot+'.root'
      if (self._request._needInputFile):
        cmsdriver+=' --filein '+self._request._inputFile
      if (self._request._needEosID):
        cmsdriver+=' --filein '+self._request._eosID
      cmsdriver+='\n'
      return cmsdriver
    elif self._type is 'REC':
      cmsdriver = 'cmsDriver.py ' + self._request._pythonConfig + \
                ' -s GEN,SIM --customise Configuration/GenProduction/timing_customize.py --conditions auto:mc --datatier GEN-SIM-RAW --eventcontent RAWSIM --no_exec -n ' \
                +str(self._nevents)+ \
                ' --python_filename '+ self._outfilesroot+'.py '+ \
                ' --fileout '+self._outfilesroot+'.root'
      if (self._request._needInputFile):
        cmsdriver+=' --filein '+self._request._inputFile
      if (self._request._needEosID):
        cmsdriver+=' --filein '+self._request._eosID
      cmsdriver+='\n'
      return cmsdriver
    

class LocalCommandBuilder(CommandBuilder):
  #def __init__(self, name, directory, type, nevents, extension = '', cmsgen = '', aqcut = -1):
  def __init__(self, request, type, nevents, aqcut = -1):
    CommandBuilder.__init__(self, request, type, nevents, aqcut);
    self._aqcut=aqcut
  def build(self):
    if not (self._type is 'GEN' or self._type is 'REC'):
      print 'type in LocalCommandBuilder can be either GEN or REC, not '+str(self._type)
      return
    try:
      script = open(self._request._dir+'/job'+self._outfilesroot+'.sh', 'w') 
      infile=''
      infile += '#!/bin/bash\n'
      infile += 'export SCRAM_ARCH=slc5_amd64_gcc434\n'
      cmsdriver = self.buildcmsDriver()
      infile+=cmsdriver
      if self._type is 'GEN':
        if self._aqcut != -1:
          infile += 'echo "process.generator.jetMatching.MEMAIN_qcut='+str(self._aqcut)+'" >> '+ self._outfilesroot+'.py'+ '\n'
          infile += 'echo "process.generator.jetMatching.outTree_flag=1" >> ' + self._outfilesroot +'.py' + '\n'
        infile += 'cmsRun '+self._outfilesroot+'.py &> '+self._outfilesroot+'.log\n'
        if self._aqcut > -1:
          infile += 'mv events.tree '+self._outfilesroot+'.tree\n'
      elif self._type is 'REC':
        infile += 'cat '+self._outfilesroot+'.py | sed -e "s#input = cms\.untracked#output = cms\.untracked#g" > tmp.py\n'
        infile += 'mv tmp.py '+self._outfilesroot+'.py\n'
        infile += 'cmsRun '+self._outfilesroot+'.py &> '+self._outfilesroot+'.log\n'
      script.write(infile)
      os.chmod(self._request._dir+'/job'+self._outfilesroot+'.sh', 0755)
      script.close()
      return str('cd '+self._request._dir+'; ./job'+self._outfilesroot+'.sh')  
    except Exception, e:
      print "Error: %s" % str(e) 


class LXBATCHCommandBuilder(CommandBuilder):
  def __init__(self, request, type, nevents, aqcut = -1):
    CommandBuilder.__init__(self, request, type, nevents, aqcut);
    self._aqcut=aqcut
  def build(self):
    if not (self._type is 'GEN' or self._type is 'REC'):
      print 'type in LocalCommandBuilder can be either GEN or REC, not '+str(self._type)
      return
    try:
      cmsswbase = os.getenv('CMSSW_BASE')
      if cmsswbase is None:
        print "you have to run cmsenv in your area first!"
        return
      script = open(self._request._dir+'/job'+self._outfilesroot+'.sh', 'w')
      infile=''
      infile += '#!/bin/bash\n'
      infile += 'export SCRAM_ARCH=slc5_amd64_gcc434\n'
      infile += 'pwd=`pwd`\n'
      infile += 'echo $pwd\n'
      infile += 'cd '+cmsswbase+'\n'
      infile += 'eval `scram runtime -sh`\n'
      infile += 'cd -\n'
      cmsdriver = self.buildcmsDriver()
      infile += cmsdriver 
      if self._type is 'GEN':
        if self._aqcut != -1:
          infile += 'echo "process.generator.jetMatching.MEMAIN_qcut='+str(self._aqcut)+'" >> '+ self._outfilesroot+'.py'+ '\n'
          infile += 'echo "process.generator.jetMatching.outTree_flag=1" >> ' + self._outfilesroot+'.py' + '\n'
        infile += 'cmsRun '+self._outfilesroot+'.py &> '+self._outfilesroot+'.log\n'
        if self._aqcut > -1:
          infile += 'mv events.tree '+self._outfilesroot+'.tree\n' 
      elif self._type is 'REC':
        infile += 'cat '+self._outfilesroot+'.py | sed -e "s#input = cms\.untracked#output = cms\.untracked#g" > tmp.py\n'
        infile += 'mv tmp.py '+self._outfilesroot+'.py\n'  
        infile += 'cmsRun '+self._outfilesroot+'.py &> '+self._outfilesroot+'.log\n'
      infile += 'ls\n'
      castordir = '/store/eos/user/lenzip/Summer11/'
      infile += 'cmsMkdir '+castordir+self._request._dir+'\n'
      infile += 'cmsStage -f *.py '+castordir+self._request._dir+'\n'
      infile += 'cmsStage -f *.root '+castordir+self._request._dir+'\n'
      infile += 'cmsStage -f *.log '+castordir+self._request._dir+'\n'
      if self._aqcut > -1:
        infile += 'cmsStage -f *.tree '+castordir+self._request._dir+'\n'
      script.write(infile)
      os.chmod(self._request._dir+'/job'+self._outfilesroot+'.sh', 0755)
      script.close()
      return str('cd '+self._request._dir+'; bsub -q 1nd job'+self._outfilesroot+'.sh')
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


class ExtendedOption (Option):
  ACTIONS = Option.ACTIONS + ("extend",)
  STORE_ACTIONS = Option.STORE_ACTIONS + ("extend",)
  TYPED_ACTIONS = Option.TYPED_ACTIONS + ("extend",)
  ALWAYS_TYPED_ACTIONS = Option.ALWAYS_TYPED_ACTIONS + ("extend",)

  def take_action(self, action, dest, opt, value, values, parser):
    if action == "extend":
      lvalue = value.split(",")
      values.ensure_value(dest, []).extend(lvalue)
    else:
      Option.take_action(self, action, dest, opt, value, values, parser)

class Request:
  def __init__(self):
    self._pythonConfig=None
    self._inputFile=None
    self._needInputFile=False
    self._eosID=None
    self._needEosID=False
    self._dir=None     
    self._name=None     

class ConfigParser :
  def __init__(self, configfile):
    try:
      file = open(configfile, 'r')
    except Exception, e:
      print "Error: %s" % str(e)
    self._configfile=configfile

  def parse(self):
    requests=[]
    file = open(self._configfile, 'r')
    for line in file:
      line=line.rstrip('\n') 
      linesplit=line.split('#')
      try:
        request=Request()
        request._pythonConfig=linesplit[0]
        pythonconfigsplit=request._pythonConfig.split('/')
        dir=pythonconfigsplit[len(pythonconfigsplit)-1].rstrip('.py')
        request._name=dir
        print dir
        if len(linesplit)>1:
          if 'lhe:' in linesplit[1] or 'mcdb:' in linesplit[1]:
            eos=linesplit[1].rstrip(' ').lstrip(' ')
            request._needEosID=True
            request._eosID=eos
            dir+=eos.lstrip('lhe:').lstrip('mcdb:')            
          else:
            request._needInputFile=True
            request._inputFile=linesplit[1]
            filenamepathsplit=linesplit[1].lstrip('file:').split('/')
            filename=filenamepathsplit[len(filenamepathsplit)-1]
            dir+=filename.split('.')[0]
        request._dir=dir
        requests.append(request)   
      except Exception, e:
        print "Error: %s" % str(e)
      
    return requests
        


numcores = os.sysconf('SC_NPROCESSORS_ONLN')
if numcores is None:
  numcores = 1

usage  = "usage: %prog list [options]\n"
usage += "the file listing the samples to be processed should have the following format:\n"
usage += "for requests that don't need any input (e.g. pythia) one line should be added with just the config fragment\n"
usage += "for requests that need an input file the gen fragment should be followed by #file:<pfn> or #<lfn>\n"
usage += "for requests that need an input EOD id the gen fragment should be followed by #lhe:<id>\n"
usage += "example content:\n"
usage += "Configuration/GenProduction/python/EightTeV/MinBias_TuneZ2star_8TeV_pythia6_cff.py\n"
usage += "Configuration/GenProduction/python/EightTeV/Hadronizer_MgmMatchTuneZ2star_8TeV_madgraph_tauola_cff.py#file:myfile.lhe\n"
usage += "Configuration/GenProduction/python/EightTeV/Hadronizer_MgmMatchTuneZ2star_8TeV_madgraph_tauola_cff.py#lhe:5468\n"

parser = OptionParser(usage=usage,option_class=ExtendedOption)
parser.add_option("--genonly",action="store_true", help="run only the gen processes (default=False)", default=False)
parser.add_option("--reconly",action="store_true", help="run only the rec processes (default=False)", default=False)
parser.add_option("--events-for-gen", action="store", help="number of events in the gen step (default=10000)", default=10000)
parser.add_option("--events-for-rec", action="store", help="number of events in the rec step (default=10)", default=10)
parser.add_option('-f', "--force", action="store_true", help="overwrite old results (default=False)", default=False)
parser.add_option('-c', "--cores", action="store", help="number of cores to use (default=%s)" %numcores, default=numcores)
parser.add_option('-b', "--batch", action="store_true", help="run on LXBATCH", default=False)
parser.add_option("-n", '--negate', action="store_true", help="do nothing, just prepare the jobs", default=False)
parser.add_option("-s", '--scanqcut', action="store", help="min,max valued of the qcut", default=None)


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

if options.scanqcut != None:
  print "you requested a qcut scan, disabling SIM step"
  options.genonly = True
  options.reconly = False
  #we can do single core only in this case, for local runs, beccause the the name of events.tree file cannot be changed
  #so we have to run on job at a time and rename aftrwards
  options.cores = 1
  qcuts = options.scanqcut.split(',')
  if len(qcuts) != 2:
    print "error parsing input qcuts"
    sys.exit(1)
  minqcut = float(qcuts[0])
  maxqcut = float(qcuts[1])
  if minqcut > maxqcut:
    print "error minqcut has to be smaller than maxqcut"
    sys.exit(1)


queue = Queue.Queue()

#start the workers
for i in range(int(options.cores)):
  worker = Worker(queue)
  worker.setDaemon(True)
  worker.start()

#fill the queue
parser=ConfigParser(args[0])
requests=parser.parse() 

for request in requests:
  

  if os.path.exists(request._dir) and options.force is False:
    print 'Directory '+request._dir+' exists already, doing nothing.'
    print 'You can overwrite with the --force option'
    continue
  
  if not os.path.exists(request._dir):
    os.mkdir(request._dir)
 

  if not options.reconly:
    qcuts = []
    if (options.scanqcut != None):
      qcut = minqcut
      while (qcut < maxqcut):
        qcut += 1.0
        print qcut
        qcuts.append(qcut)
    else:    
      qcuts = [-1]
    for aqcut in qcuts:  
      commandBuilder = LocalCommandBuilder(request, 'GEN', options.events_for_gen, aqcut) if not options.batch else LXBATCHCommandBuilder(request, 'GEN', options.events_for_gen, aqcut) 
      command = commandBuilder.build()
      if command is None:
        print 'problem building GEN command for '+request._name
      else: 
        if not options.negate:
          queue.put(command)

  if not options.genonly:
    commandBuilder = LocalCommandBuilder(request, 'REC', options.events_for_rec) if not options.batch else LXBATCHCommandBuilder(request, 'REC', options.events_for_rec)
    command = commandBuilder.build() 
    if command is None:
      print 'problem building REC command for '+request._name
    else:
      if not options.negate:
        queue.put(command)  
    
  
if not options.negate:
  queue.join()

  
