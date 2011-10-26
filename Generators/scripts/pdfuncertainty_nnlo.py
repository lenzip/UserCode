#!/usr/bin/env python

from __future__ import division
from math import *
import os
import sys

from optparse import OptionParser


#parse scale variations
def parsescale(dir, pdfname, pdfnumber, list):
  scalelist = ['downdown', 'downup', 'updown', 'upup']
  for i in scalelist:
    stripdirname = dir+"_"+pdfname+"_"+str(pdfnumber)+'_scale-'+i
    print stripdirname
    try:
      file = open(dir+"/"+stripdirname+"/log.txt", "r")
    except:
      print "could not open log file in dir "+stripdirname
    ilines = file.readlines()
    found = False
    for line in ilines:
      if line.find("Value of final tota integral is") > -1:
        xsection_line = line.split()
        print 'xsection is '+xsection_line[6]
        list.append(float(xsection_line[6]))
        found = True
    if found is False :
      print 'scale variation value not found for '+stripdirname          


# FEWZ cross sections are given in pb
def parse(dir, pdfname, number, list):
  #os.system("ls "+dir+"/ | grep "+pdfname)
  #os.system("ls "+dir+"/ | grep "+pdfname+"*_"+str(i)+"> tmp.txt")
  #os.system("ls "+dir+"/ | grep "+pdfname+"> tmp.txt")
  #tmpfile = open("tmp.txt", "r" ) 
  #igot = tmpfile.readlines()
  for i in range(number):
  #for dirname in igot:
    #stripdirname = dirname.rstrip('\n')
    stripdirname = dir+"_"+pdfname+"_"+str(i)
    print stripdirname
    try:
      file = open(dir+"/"+stripdirname+"/res120h.dat", "r")
    except:
      print "could not open log file in dir "+stripdirname
    ilines = file.readlines()
    found = False
    for line in ilines:
      if line.find("sigma") > -1:
        xsection_line = line.split()
        print 'xsection is '+xsection_line[2] 
        list.append(float(xsection_line[2]))
        found = True
    if found is False :
      print 'pdf value not found for '+stripdirname


def mstw_pdf(xsec):
  neigenvalues = (len(xsec)-1)//2
  deltap = 0.;
  deltan = 0.;
  #print "Number of PDF eigenvalues is: %s" % (neigenvalues)
  for eigenvalue in range(neigenvalues):
    xp = xsec[eigenvalue*2+1] 
    xn = xsec[eigenvalue*2+2] 
    deltap += (max(xp-xsec[0],xn-xsec[0],0))**2
    deltan += (max(xsec[0]-xp,xsec[0]-xn,0))**2
  #print 'MSTW central: %5.3f, PDF+ %5.3f, PDF- %5.3f' %(xsec[0], sqrt(deltap), sqrt(deltan))  
  output = [xsec[0], sqrt(deltap), sqrt(deltan)]
  return output

def mstw_pdf_naive(xsec):
  neigenvalues = (len(xsec)-1)//2
  deltasq = 0.;
  #print "Number of PDF eigenvalues is: %s" % (neigenvalues)
  for eigenvalue in range(neigenvalues):
    xp = xsec[eigenvalue*2+1]
    xn = xsec[eigenvalue*2+2] 
    deltasq += (xp-xn)**2

  output = [xsec[0], 0.5*sqrt(deltasq), 0.5*sqrt(deltasq)]
  return output

def mstw_pdf_alpha(xsec, xsecplus,  xsecminus):
  deltapdf = mstw_pdf(xsec)
  deltapdfplus = mstw_pdf(xsecplus)
  deltapdfminus = mstw_pdf(xsecminus)
  deltap = max(xsec[0]+deltapdf[1], xsecplus[0]+deltapdfplus[1], xsecminus[0]+deltapdfminus[1])-xsec[0]
  deltam = xsec[0]-min(xsec[0]-deltapdf[2], xsecplus[0]-deltapdfplus[2], xsecminus[0]-deltapdfminus[2])
  output = [xsec[0], deltap, deltam]
  return output
  
def cteq_pdf(xsec):
  neigenvalues = (len(xsec)-1)//2
  #print "Number of PDF eigenvalues is: %s" % (neigenvalues)
  deltasq = 0.;
  for eigenvalue in range(neigenvalues):
     deltasq += (xsec[eigenvalue*2+1] - xsec[eigenvalue*2+2])**2 
  
  output = [xsec[0], 0.5*sqrt(deltasq), 0.5*sqrt(deltasq)]
  return output

def cteq_alpha(xsec):
  if len(xsec) != 5:
    print "wrong number of pdfs in cteq_alpha: %3d instead of 5" % (len(xsec))
    sys.exit(1)
  delta=0.5*sqrt((xsec[0]-xsec[4])**2) 
  output = [xsec[2], delta, delta]
  return output

def cteq_pdf_alpha(xsec, xsecalpha):
  deltapdf = cteq_pdf(xsec)
  deltaalpha = cteq_alpha(xsecalpha)
  output = [deltapdf[0], (1/1.645)*sqrt(deltapdf[1]**2 + deltaalpha[1]**2), (1/1.645)*sqrt(deltapdf[2]**2 + deltaalpha[2]**2)]
  return output

def nnpdf_pdf(xsec):
  #print "Number of PDF eigenvalues is: %s" % (len(xsec))
  mean = 0.
  for eigenvalue in range(len(xsec)):
    mean += xsec[eigenvalue]
  mean/=len(xsec)

  sqerrorsum = 0.
  for eigenvalue in range(len(xsec)):
    sqerrorsum += (xsec[eigenvalue]-mean)**2
  sqerrorsum/=len(xsec)  
  error = sqrt(sqerrorsum)
  output = [mean, error, error]
  return output

def nnpdf_alpha(xsec, xsecplus, xsecminus):
  deltaxsec = nnpdf_pdf(xsec)
  deltaxsecplus = nnpdf_pdf(xsecplus)
  deltaxsecminus = nnpdf_pdf(xsecminus)

  errorplus = deltaxsecplus[0]-deltaxsec[0]
  errorminus = deltaxsecminus[0]-deltaxsec[0]
  output = [deltaxsec[0], errorplus, errorminus]
  return output
  
def nnpdf_alpha_pdf(xsec, xsecplus, xsecminus):
  pdferror = nnpdf_pdf(xsec)
  alphaerror = nnpdf_alpha(xsec, xsecplus, xsecminus)

  errorplus = sqrt(pdferror[1]**2+alphaerror[1]**2)
  errorminus = sqrt(pdferror[2]**2+alphaerror[2]**2)
  output = [pdferror[0], errorplus, errorminus]
  return output
    
    




xsections1 = []
xsections2 = []
xsections3 = []
scalevars = []

parser = OptionParser()
parser.add_option("--dir",action="store", help="directory containing xsections")#, dest="dir1")
parser.add_option("--dirb", action="store", help="directory containing additional contribution for the xsection", default=None)
parser.add_option("--pdf", action="store", help="name of PDF, can be MSTW, CTEQ, NNPDF", default=None)#, dest="pdf")
parser.add_option("--naivemstw", action="store_true", help="do calculation according to equation 1) instead of 7) 8) in http://arxiv.org/pdf/0905.3531v2", default=False)#, dest="pdf")
parser.add_option("--scale", action="store_true", help="evaluate scale uncertainty", default=False)

#print "pdf ", options.pdf

(options, args) = parser.parse_args()
if options.pdf != None and options.scale == False:
  if str(options.pdf) == 'MSTW':
    parse(options.dir, "MSTW2008NNLO68CL", 41, xsections1)
    parse(options.dir, "MSTW2008NNLO68CLALPHAPLUS", 41, xsections2)
    parse(options.dir, "MSTW2008NNLO68CLALPHAMINUS", 41, xsections3)
    if options.dirb != None:
      xsections1_2 = []
      xsections2_2 = []
      xsections3_2 = []
      parse(options.dirb, "MSTW2008NNLO68CL", 41, xsections1_2)
      parse(options.dirb, "MSTW2008NNLO68CLALPHAPLUS", 41, xsections2_2)
      parse(options.dirb, "MSTW2008NNLO68CLALPHAMINUS", 41, xsections3_2)
      for i in range(len(xsections1)):
        xsections1[i]+=xsections1_2[i]
        xsections2[i]+=xsections2_2[i]
        xsections3[i]+=xsections3_2[i]
 
  elif str(options.pdf) == "CTEQ":
    parse(options.dir, "cteq66.LHgrid", 45, xsections1)
    parse(options.dir, "cteq66alphas.LHgrid", 5, xsections2)
    if options.dirb != None:
      xsections1_2 = []
      xsections2_2 = []
      parse(options.dirb, "cteq66.LHgrid", 45, xsections1_2)
      parse(options.dirb, "cteq66alphas.LHgrid", 5, xsections2_2)
      for i in range(len(xsections1)):
        xsections1[i]+=xsections1_2[i]
      for i in range(len(xsections2)):
        xsections2[i]+=xsections2_2[i] 

  elif str(options.pdf) == "NNPDF":
    parse(options.dir, "NNPDF20_100.LHgrid", 101, xsections1)
    parse(options.dir, "NNPDF20_as_0121_100.LHgrid", 101, xsections2)
    parse(options.dir, "NNPDF20_as_0117_100.LHgrid", 101, xsections3)
    if options.dirb != None:
      xsections1_2 = []
      xsections2_2 = []
      xsections3_2 = []
      parse(options.dirb, "NNPDF20_100.LHgrid", 101, xsections1_2)
      parse(options.dirb, "NNPDF20_as_0121_100.LHgrid", 101, xsections2_2)
      parse(options.dirb, "NNPDF20_as_0117_100.LHgrid", 101, xsections3_2)
      for i in range(len(xsections1)):
        xsections1[i]+=xsections1_2[i]
        xsections2[i]+=xsections2_2[i]
        xsections3[i]+=xsections3_2[i]


elif options.scale != False:
  #scale variations
  parse(options.dir, "mstw2008nnlo", 1, xsections1)
  parsescale(options.dir, "mstw2008nnlo", 0, scalevars)
  if options.dirb != None:
    xsections1_2 = []
    scalevars_2 = []
    parse(options.dirb, "mstw2008nnlo", 1, xsections1_2)
    parsescale(options.dirb, "mstw2008nnlo", 0, scalevars_2)
    xsections1[0] += xsections1_2[0]
    for i in range(len(scalevars)):
      scalevars[i] += scalevars_2[i]

if str(options.pdf) == "MSTW" :
  if ( len(xsections1) != 41 or len(xsections2) != 41 or len(xsections3) != 41):
    print 'missing pdfs! found only %3d, %3d, %3d xsection values, expected 41' %(len(xsections1), len(xsections2), len(xsections3))
    sys.exit(1)
  else:
    if options.naivemstw:
      pdferror = mstw_pdf_naive(xsections1)
      print 'MSTW naive pdf error: %5.2f + %5.2f - %5.2f' %(pdferror[0], pdferror[1], pdferror[2])
    else:
      pdferror = mstw_pdf(xsections1)
      print 'MSTW pdf error: %5.2f + %5.2f - %5.2f' %(pdferror[0], pdferror[1], pdferror[2]) 
      pdfalphaerro = mstw_pdf_alpha(xsections1, xsections2, xsections3)
      print 'MSTW combined pdf-alpha error: %5.2f + %5.2f - %5.2f' %(pdfalphaerro[0], pdfalphaerro[1], pdfalphaerro[2])  
      print 'MSTW Value+delta,  %5.2f, Value-delta %5.2f' %(pdfalphaerro[0]+pdfalphaerro[1], pdfalphaerro[0]-pdfalphaerro[2])  

if str(options.pdf) == "NNPDF" :
  if (len(xsections1) != 101 or len(xsections2) != 101 or len(xsections3) != 101): 
    print 'missing pdfs! found only %3d, %3d, %3d xsection values, expected 101' %(len(xsections1), len(xsections2), len(xsections3))
    sys.exit(1)
  else:
    pdferror = nnpdf_pdf(xsections1)
    print 'NNPDF pdf error: %5.2f + %5.2f - %5.2f' %(pdferror[0], pdferror[1], pdferror[2])
    alphaerror = nnpdf_alpha(xsections1, xsections2, xsections3)
    print 'NNPDF alpha error: %5.2f + %5.2f - %5.2f' %(alphaerror[0], alphaerror[1], alphaerror[2])
    pdfalphaerror = nnpdf_alpha_pdf(xsections1, xsections2, xsections3)
    print 'NNPDF combined pdf-alpha error: %5.2f + %5.2f - %5.2f' %(pdfalphaerror[0], pdfalphaerror[1], pdfalphaerror[2])
    print 'NNPDF Value+delta,  %5.2f, Value-delta %5.2f' %(pdfalphaerror[0]+pdfalphaerror[1], pdfalphaerror[0]-pdfalphaerror[2])  
    
 
if str(options.pdf) == "CTEQ":
  if ( len(xsections1) != 45 or len(xsections2) != 5): 
      print 'missing pdfs! found only %3d, %3d xsection values, expected 5' %(len(xsections1), len(xsections2))
      sys.exit(1)
  else:
    pdferror = cteq_pdf(xsections1)
    print 'CTEQ66 pdf error: %5.2f + %5.2f - %5.2f' %(pdferror[0], pdferror[1], pdferror[2])
    alphaerror = cteq_alpha(xsections2)
    print 'CTEQ66 alpha error: %5.2f + %5.2f - %5.2f' %(alphaerror[0], alphaerror[1], alphaerror[2])
    pdfalphaerror = cteq_pdf_alpha(xsections1, xsections2)
    print 'CTEQ66 combined pdf-alpha error: %5.2f + %5.2f - %5.2f' %(pdfalphaerror[0], pdfalphaerror[1], pdfalphaerror[2]) 
    print 'CTEQ66 Value+delta,  %5.2f, Value-delta %5.2f' %(pdfalphaerror[0]+pdfalphaerror[1], pdfalphaerror[0]-pdfalphaerror[2])  

if options.scale == True:
  if (len(scalevars) != 4):
    print 'missing scale variations, expected 4 found %3d' %(len(scalevars))
    sys.exit(1)
  else:
    max = 0.
    min = 0.
    for i in range(len(scalevars)):
      cur = scalevars[i] - xsections1[0]
      if cur > max:
        max = cur
      if cur < min:
        min = cur
    print "scale variation: %5.2f + %5.2f - %5.2f" %(xsections1[0], max, abs(min))        

