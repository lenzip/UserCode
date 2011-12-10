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
  def __init__(self, name, directory, type, nevents, extension = '', cmsgen = '', aqcut = -1):
    CommandBuilder.__init__(self, name);
    self._type=type
    self._nevents=nevents
    self._directory=directory 
    self._extension=extension
    self._cmsgen=cmsgen
    self._aqcut=aqcut
  def build(self):
    if not (self._type is 'GEN' or self._type is 'REC'):
      print 'type in LocalCommandBuilder can be either GEN or REC, not '+str(self._type)
      return
    try:
      outfilesroot=self._name+self._type+str(self._aqcut)
      script = open(self._directory+'/job'+outfilesroot+'.sh', 'w') 
      infile=''
      infile += '#!/bin/bash\n'
      infile += 'export SCRAM_ARCH=slc5_amd64_gcc434\n'
      if self._cmsgen != '':
        infile += 'echo '+self._cmsgen+' > cmsgen.cfg\n'
        infile += 'cmsGen.py --generator sherpa --number-of-events 1 --cfg cmsgen.cfg\n'
      if self._type is 'GEN':
        infile += 'cmsDriver.py Configuration/GenProduction/python/'+self._name+ \
                  '.py -s GEN  --conditions auto:mc --datatier GEN-SIM --eventcontent RAWSIM --no_exec -n '+ \
                  str(self._nevents)+' '+self._extension+ \
                  ' --python_filename '+ outfilesroot+'.py '+ \
                  ' --fileout '+outfilesroot+'.root\n'
        if self._aqcut != -1:
          infile += 'echo "process.generator.jetMatching.MEMAIN_qcut='+str(self._aqcut)+'" >> '+ outfilesroot+'.py'+ '\n'
          infile += 'echo "process.generator.jetMatching.outTree_flag=1" >> ' + outfilesroot +'.py' + '\n'
        infile += 'cmsRun '+outfilesroot+'.py &> '+outfilesroot+'.log\n'
        infile += 'mv events.tree '+outfilesroot+'.tree\n'
      elif self._type is 'REC':
        infile += 'cmsDriver.py Configuration/GenProduction/python/'+self._name+ \
                  '.py -s GEN,SIM --customise Configuration/GenProduction/timing_customize.py --conditions auto:mc --datatier GEN-SIM-RAW --eventcontent RAWSIM --no_exec -n ' \
                  +str(self._nevents)+' '+self._extension+ \
                  ' --python_filename '+ outfilesroot+'.py '+ \
                  ' --fileout '+outfilesroot+'.root\n'
        infile += 'cat '+outfilesroot+'.py | sed -e "s#input = cms\.untracked#output = cms\.untracked#g" > tmp.py\n'
        infile += 'mv tmp.py '+outfilesroot+'.py\n'
        infile += 'cmsRun '+outfilesroot+'.py &> '+outfilesroot+'.log\n'
      script.write(infile)
      os.chmod(self._directory+'/job'+outfilesroot+'.sh', 0755)
      script.close()
      return str('cd '+self._directory+'; ./job'+outfilesroot+'.sh')  
    except Exception, e:
      print "Error: %s" % str(e) 


class LXBATCHCommandBuilder(CommandBuilder):
  def __init__(self, name, directory, type, nevents, extension = '', cmsgen = '', aqcut = -1):
    CommandBuilder.__init__(self, name);
    self._type=type
    self._nevents=nevents
    self._directory=directory
    self._extension=extension
    self._cmsgen=cmsgen
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
      outfilesroot=self._name+self._type+str(self._aqcut)
      script = open(self._directory+'/job'+outfilesroot+'.sh', 'w')
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
        infile += ''
        infile += 'cmsDriver.py Configuration/GenProduction/python/'+self._name+ \
                  '.py -s GEN  --conditions auto:mc --datatier GEN-SIM --eventcontent RAWSIM --no_exec -n '+ \
                  str(self._nevents)+' '+self._extension+ \
                  ' --python_filename '+ outfilesroot+'.py '+ \
                  ' --fileout '+outfilesroot+'.root\n'
        if self._aqcut != -1:
          infile += 'echo "process.generator.jetMatching.MEMAIN_qcut='+str(self._aqcut)+'" >> '+ outfilesroot+'.py'+ '\n'
          infile += 'echo "process.generator.jetMatching.outTree_flag=1" >> ' + outfilesroot+'.py' + '\n'
        infile += 'cmsRun '+outfilesroot+'.py &> '+outfilesroot+'.log\n'
        infile += 'mv events.tree '+outfilesroot+'.tree\n' 
      elif self._type is 'REC':
        infile += 'cmsDriver.py Configuration/GenProduction/python/'+self._name+ \
                  '.py -s GEN,SIM --customise Configuration/GenProduction/timing_customize.py --conditions auto:mc --datatier GEN-SIM-RAW --eventcontent RAWSIM --no_exec -n ' \
                  +str(self._nevents)+' '+self._extension+ \
                  ' --python_filename '+ outfilesroot+'.py '+ \
                  ' --fileout '+outfilesroot+'.root\n' 
        infile += 'cat '+outfilesroot+'.py | sed -e "s#input = cms\.untracked#output = cms\.untracked#g" > tmp.py\n'
        infile += 'mv tmp.py '+outfilesroot+'.py\n'  
        infile += 'cmsRun '+outfilesroot+'.py &> '+outfilesroot+'.log\n'
      infile += 'ls\n'
      castordir = '/store/eos/user/lenzip/Summer11/'
      infile += 'cmsMkdir '+castordir+self._directory+'\n'
      infile += 'cmsStage -f *.py '+castordir+self._directory+'\n'
      infile += 'cmsStage -f *.root '+castordir+self._directory+'\n'
      infile += 'cmsStage -f *.log '+castordir+self._directory+'\n'
      infile += 'cmsStage -f *.tree '+castordir+self._directory+'\n'
      script.write(infile)
      os.chmod(self._directory+'/job'+outfilesroot+'.sh', 0755)
      script.close()
      return str('cd '+self._directory+'; bsub -q 1nd job'+outfilesroot+'.sh')
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
for line in file:
  stripline = line.rstrip('\n')
  print stripline
  
  newstripline = stripline
  #check if the directory name starts with a number --> it is an MCDB article
  namecomponents = stripline.split('_')
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


  extension = '' if dummy is None else '--filein=mcdb:'+namecomponents[0]  

  if os.path.exists(stripline) and options.force is False:
    print 'Directory '+stripline+' exists already, doing nothing.'
    print 'You can overwrite with the --force option'
    continue
  
  if not os.path.exists(stripline):
    os.mkdir(stripline)
 
  print newstripline

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
      commandBuilder = LocalCommandBuilder(newstripline, stripline, 'GEN', options.events_for_gen, extension, cmsgen, aqcut) if not options.batch else LXBATCHCommandBuilder(newstripline, stripline, 'GEN', options.events_for_gen, extension, cmsgen, aqcut) 
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

  
