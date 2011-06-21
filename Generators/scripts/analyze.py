#! /usr/bin/env python

import os, sys, logging, re
import string
import fnmatch
import stat

from math import sqrt
from ROOT import TFile, TTree
from optparse import OptionParser
from stat import *

from ROOT import gSystem
gSystem.Load("libFWCoreFWLite")
from ROOT import AutoLibraryLoader
AutoLibraryLoader.enable()
gSystem.Load("libDataFormatsFWLite")
gSystem.Load("libDataFormatsPatCandidates")

usage = "usage: %prog list [options]"
parser = OptionParser(usage=usage)
parser.add_option("--notwikiline", action="store_true", help="do not print the formatted twiki line", default=False)
parser.add_option("--twikifile", action="store", help="twiki file name", default='twiki.txt')
parser.add_option("--force", action="store_true", help="overwrite twiki file", default=False)
parser.add_option("--requestnumber", action="store", help="starting request number", default=1)



(options, args) = parser.parse_args()
if len(args)<1 :
  parser.print_help()
  sys.exit(1)

try:
  file = open(args[0], 'r')
except Exception, e:
  print "Error: %s" % str(e)
  sys.exit(1)


def getNevents(filename):
  f=TFile(filename)
  tree = f.Get('Events')
  return tree.GetEntries()


twiki=''
if (not options.notwikiline):
  if os.path.exists(options.twikifile) and not options.force:
    print 'file '+options.twikifile+' exists, doing nothing. Rerun with --twikifile=something or with --force to overwrite'
    sys.exit(1)
  twikifile=open(options.twikifile, 'w')

requestnumber=int(options.requestnumber)

#fill the queue
for line in file:
  fullstripline = line.rstrip('\n')
  fullstripline_split=fullstripline.split(':')
  stripline = fullstripline_split[0]
  print stripline
  
  if not os.path.exists(stripline):
    print 'WARNING, path '+stripline+' not found'
    continue

  
  pythonconfig=''
  pythonFound=False
  articleid=''
  try:
    configfilename = ''
    for file2 in os.listdir(stripline):
      if fnmatch.fnmatch(file2, '*_GEN.py'):
        configfilename = file2 
        break
    configfile = open(stripline+'/'+configfilename, "r")   
    for line in configfile:
      if ('# with command line options:' in line):
        linesplit = line.split()
        pythonconfig=linesplit[5]
      if ('articleID' in line):
        #print 'found MCDB: '+line 
        linesplit = line.split("(")
        articleid=linesplit[1].rstrip('),\n')
    pythonconfig='http://cmssw.cvs.cern.ch/cgi-bin/cmssw.cgi/CMSSW/'+pythonconfig
    pythonFound=True
  
  except Exception, e:
    print "Error: %s" % str(e) 


  xsection=0
  foundXS = False
  ngeneventspythia=0
  try:
    logGEN = open(stripline+"/log_GEN.txt", "r")
    ilines = logGEN.readlines()
    for line in ilines:
      #search for error messages
      if ('MSG-e' in line) or ('MSG-w' in line) or ('Exception' in line) or ('ERROR' in line) or ('error' in line):
        print 'error message found in file :'+line.rstrip('\n')
      if ("0 All included subprocesses" in line):
        xsection_line = line.split()
        #print 'xsection is '+xsection_line[9] 
        xsection=xsection_line[9]
        xsection=float(xsection.replace('D', 'E'))
        ngeneventspythia=float(xsection_line[6])
        xsection*=1e9
        foundXS = True
    if foundXS is False :
      print 'Xsection not found for '+stripdirname
  except Exception, e:
    print "Error: %s" % str(e)

  time=0 
  foundTiming=False
  try:
    logREC = open(stripline+"/log_REC.txt", "r")
    for line in logREC:
      line=line.strip()
      #search for error messages
      if ('MSG-e' in line) or ('MSG-w' in line) or ('Exception' in line) or ('ERROR' in line) or ('error' in line):
        print 'error message found in file :'+line
    logREC2=open(stripline+"/log_REC.txt", "r")  
    for line in logREC2:
      line=line.strip()
      #print line
      if ('CPU Summary' in line):
        for i in range(4):
          try:
            line=logREC2.next().strip()
            print line
            if ('Total job' in line):
              timingline = line.split()
              time=float(timingline[3])
              foundTiming=True
          except: pass 
    if not foundTiming:
      print 'timing info not found for '+stripline    
  except Exception, e:
    print "Error: %s" % str(e) 
    sys.exit(1)

  size=0
  foundRootReco=False
  foundRootGen=False
  try:
    rootrecofilename = ''
    for file3 in os.listdir(stripline):
      if fnmatch.fnmatch(file3, '*GEN_SIM_DIGI_L1_DIGI2RAW_HLT.root'):
        rootrecofilename = file3
        foundRootReco=True
        break
    if not foundRootReco:
      print 'root file from RECO not found'
      break
    rootrecofile = stripline+'/'+rootrecofilename
    filestats=os.stat(rootrecofile)
    size=filestats[stat.ST_SIZE]
    size = size/1000
    entries=getNevents(rootrecofile)
    if (entries != 10):
      print "WARNING! nomber of events is "+str(entries)+', expected 10'
    size /= entries
    time /= entries

    for file4 in os.listdir(stripline):
      if fnmatch.fnmatch(file4, '*GEN.root'):
        rootgenfilename = file4
        foundRootGen=True
        break
    if not foundRootGen:
      print 'root file from GEN not found'
      break
    rootgenfile = stripline+'/'+rootgenfilename   
    neventsgen = getNevents(rootgenfile)
    if (neventsgen != 10000):
      print "WARNING! nomber of events is "+str(neventsgen)+', expected 10000'
    print 'ngeneventspythia '+str(ngeneventspythia)  
    filtereff=float(neventsgen)/ngeneventspythia  ####COMENTED FOR MC@NLO 

  except Exception, e:
    print "Error: %s" % str(e)

  print 'python configuration: '+pythonconfig
  print 'cross section: '+str(xsection)
  print 'time per event: '+str(time)
  print 'size per event: '+str(size)
  print 'filterefficiency: %.7f +- %.7f' % (filtereff, sqrt(filtereff*(1-filtereff)/ngeneventspythia))
  print 'articleID: '+articleid

  if (articleid != ''):
    articleid += " | "

  if (not options.notwikiline):
    #twiki+='| '+str(requestnumber)+' [[http://vdutta.web.cern.ch/vdutta/Fall10ProductionRAWglobal.html#request_'+str(requestnumber)+'][RAW]] [[http://vdutta.web.cern.ch/vdutta/Fall10ProductionRECOglobal.html#request_'+str(requestnumber)+'][RECO]] [[http://vdutta.web.cern.ch/vdutta/Fall10ProductionAODglobal.html#request_'+str(requestnumber)+'][AOD]] | EWK/P.Lenzi | '+fullstripline_split[1]+' | no | | no | '+str(xsection)+(' | %.1f | '% filtereff)+articleid+'[['+pythonconfig+'][GEN]] | '+str(time)+' | '+str(size)+' | '+fullstripline_split[2]+' | N/A | | *not yet* |\n' 
    twiki+='| '+str(requestnumber)+' | EWK/P.Lenzi | '+fullstripline_split[1]+' | no | | no | '+str(xsection)+(' | %.1f | '% filtereff)+articleid+'[['+pythonconfig+'][GEN]] | '+str(time)+' | '+str(size)+' | '+fullstripline_split[2]+' | N/A | | *not yet* |\n' 
  requestnumber+=1

  print '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'

if (not options.notwikiline):
  twikifile.write(twiki)
  twikifile.close()
