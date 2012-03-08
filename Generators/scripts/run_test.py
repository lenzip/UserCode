#! /usr/bin/env python

import os, sys, logging, re
import tarfile
import getopt
import string
import threading, Queue
import subprocess
import time

from optparse import OptionParser, Option
from stat import *

class CommandBuilder:
  def __init__(self,request, nevents):
    self._request=request
    self._nevents=nevents
    self._outfilesroot=self._request._name
  
  def buildWF(self):
    wf = ''
    for step in self._request._pythonConfigs:
      logname=step+".log"
      wf += "cmsRun "+step+"&>"+logname+";"
    wf += '\n'  
    return wf
    

class LocalCommandBuilder(CommandBuilder):
  def __init__(self, request, nevents, isBatch=False):
    CommandBuilder.__init__(self, request, nevents);
    self._isBatch = isBatch
  def build(self):
    try:
      script = open(self._request._dir+'/job'+self._outfilesroot+'.sh', 'w')
      index=0;
      for filename in self._request._pythonConfigs:
        configfile = open(self._request._dir+"/"+filename, 'w')
        content = self._request._pythonConfigFiles[index].read()
        configfile.write(content)
        configfile.close()
        index+=1
      infile=''
      infile += '#!/bin/bash\n'
      infile += 'export SCRAM_ARCH=slc5_amd64_gcc434\n'
      infile += 'export SCRAM_ARCH=slc5_amd64_gcc434\n'
      infile += 'export myrel='+self._request._release+'\n'
      infile += 'rel=`echo $myrel | sed -e "s/CMSSW_//g" | sed -e "s/_patch.*//g" | awk -F _ \'{print $1$2$3}\'`\n'
      infile += 'if [ $rel -gt 505 ]; then\n'
      infile += '  export SCRAM_ARCH=slc5_amd64_gcc462\n'
      infile += '  echo $SCRAM_ARCH\n'
      infile += 'fi\n'
      infile += 'scram p CMSSW '+self._request._release+'\n'
      infile += 'cd '+self._request._release+'\n'
      infile += 'eval `scram runtime -sh`\n'
      infile += 'cd -\n'
      wf=self.buildWF()
      infile+=wf
      script.write(infile)
      script.close()
      os.chmod(self._request._dir+'/job'+self._outfilesroot+'.sh', 0755)
      if self._isBatch:  
        return str('cd '+self._request._dir+'; bsub -q 1nd job'+self._outfilesroot+'.sh')
      return str('cd '+self._request._dir+'; ./job'+self._outfilesroot+'.sh')  
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
        commandAndId = self.queue.get()
        process = subprocess.Popen(commandAndId[0], shell=True, stdout=subprocess.PIPE)
        process.wait()
        self.status = process.returncode
        print commandAndId[1]+' finished with exit code '+str(self.status)
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
    self._pythonConfigs=[]
    self._pythonConfigFiles=[]
    self._prepId=None
    self._release=None
    self._dir=None     
    self._name=None     

class ConfigParser :
  def __init__(self, tarball):
    self._tarball = tarball

  def parse(self):
    requests=[]
    try:
      tarball = tarfile.open(self._tarball, 'r')
      #tarball.list()
      members = tarball.getmembers()
      for member in members:
        if member.isdir():
          dir=member.name
          break
      summaryFile = tarball.extractfile(dir+"/summary.txt")
    except Exception, e:
      print "Error: %s" % str(e)

    for line in summaryFile:
      line=line.rstrip('\n') 
      linesplit=line.split('\t')
      try:
        #this is the title of the table
        if "request ID" in line:
          continue  
        #for empty lines  
        if len(linesplit) < 3:
          continue
        #skip summary info  
        if "Total evts" in line:
          continue
        request=Request()
        request._prepId=linesplit[0]
        request._release=linesplit[1]
        for item in linesplit:
          if "config" in item:
            request._pythonConfigs.append(item)
            request._pythonConfigFiles.append(tarball.extractfile(dir+"/"+item))
        request._dir=request._prepId    
        request._name=request._prepId
        requests.append(request)   
      except Exception, e:
        print "Error: %s" % str(e)
      
    return requests
        


numcores = os.sysconf('SC_NPROCESSORS_ONLN')
if numcores is None:
  numcores = 1

usage  = "usage: %prog <tarball> [options]"

parser = OptionParser(usage=usage,option_class=ExtendedOption)
parser.add_option("--events", action="store", help="number of events to be run  (default=100)", default=100)
parser.add_option('-f', "--force", action="store_true", help="overwrite old results (default=False)", default=False)
parser.add_option('-c', "--cores", action="store", help="number of cores to use (default=%s)" %numcores, default=numcores)
#parser.add_option('-b', "--batch", action="store_true", help="run on LXBATCH", default=False)
parser.add_option("-n", '--negate', action="store_true", help="do nothing, just prepare the jobs", default=False)


(options, args) = parser.parse_args()
if len(args)<1 :
  parser.print_help()
  sys.exit(1)

try:
  tarball = tarfile.open(args[0], 'r')
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
parser=ConfigParser(args[0])
requests=parser.parse() 

for request in requests:
  

  if os.path.exists(request._dir) and options.force is False:
    print 'Directory '+request._dir+' exists already, doing nothing.'
    print 'You can overwrite with the --force option'
    continue
  
  if not os.path.exists(request._dir):
    os.mkdir(request._dir)
 

  commandBuilder = LocalCommandBuilder(request, options.events) #, options.batch)
  command = commandBuilder.build()
  if command is None:
    print 'problem building GEN command for '+request._name
  else: 
    if not options.negate:
      queue.put([command, request._prepId])
      time.sleep(10)

  
if not options.negate:
  queue.join()

  
