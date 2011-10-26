#!/usr/bin/env python

from __future__ import division
from math import *
import os
import sys

from optparse import OptionParser


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
      file = open(dir+"/"+stripdirname+"/log.txt", "r")
    except:
      print "could not open log file in dir "+stripdirname  
    ilines = file.readlines()
    found = False
    for line in ilines:
      if line.find("Value of final tota integral is") > -1:
        xsection_line = line.split()
        list.append(float(xsection_line[6]))
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
    
    


#if len(sys.argv) < 3:
#  print "usage: ./pdfuncertainty directory pdfname1 [pdfname2] [pdfname3]"
#  sys.exit(1)

#onlypdferrors = False
#if len(sys.argv) is 3:
#  onlypdferrors = True


parser = OptionParser()
parser.add_option("--dir1",action="store", help="directory containing ratio numerator")#, dest="dir1")
parser.add_option("--dir2", action="store", help="directory containing ratio denominator")#, dest="dir2")
parser.add_option("--dir1b", action="store", help="directory containing additional contribution for the numerator", default=None)#, dest="dir2")
parser.add_option("--pdf", action="store", help="name of PDF, can be MSTW, CTEQ, NNPDF")#, dest="pdf")

ratios1 = []
ratios2 = []
ratios3 = []

#print "pdf ", options.pdf

(options, args) = parser.parse_args()
if str(options.pdf) == 'MSTW':
  xsections1n = []
  xsections2n = []
  xsections3n = []
  xsections1d = []
  xsections2d = []
  xsections3d = []
  parse(options.dir1, "MSTW2008nlo68cl.LHgrid", 41, xsections1n)
  parse(options.dir1, "mstw2008nlo_asp.LHgrid", 41, xsections2n)
  parse(options.dir1, "mstw2008nlo_asm.LHgrid", 41, xsections3n)
  parse(options.dir2, "MSTW2008nlo68cl.LHgrid", 41, xsections1d)
  parse(options.dir2, "mstw2008nlo_asp.LHgrid", 41, xsections2d)
  parse(options.dir2, "mstw2008nlo_asm.LHgrid", 41, xsections3d)
  if options.dir1b != None:
    xsections1n2 = []
    xsections2n2 = []
    xsections3n2 = []
    parse(options.dir1b, "MSTW2008nlo68cl.LHgrid", 41, xsections1n2)
    parse(options.dir1b, "mstw2008nlo_asp.LHgrid", 41, xsections2n2)
    parse(options.dir1b, "mstw2008nlo_asm.LHgrid", 41, xsections3n2)
    for i in range(len(xsections1n)):
      xsections1n[i]+=xsections1n2[i]
      xsections2n[i]+=xsections2n2[i]
      xsections3n[i]+=xsections3n2[i]
  for i in range(len(xsections1n)):   
    ratios1.append(xsections1n[i]/xsections1d[i])
    ratios2.append(xsections2n[i]/xsections2d[i])
    ratios3.append(xsections3n[i]/xsections3d[i])
elif str(options.pdf) == "CTEQ":
  xsections1n = []
  xsections2n = []
  xsections1d = []
  xsections2d = []
  parse(options.dir1, "cteq66.LHgrid", 45, xsections1n)
  parse(options.dir1, "cteq66alphas.LHgrid", 5, xsections2n)
  parse(options.dir2, "cteq66.LHgrid", 45, xsections1d)
  parse(options.dir2, "cteq66alphas.LHgrid", 5, xsections2d)
  if options.dir1b != None:
    xsections1n2 = []
    xsections2n2 = []
    parse(options.dir1b, "cteq66.LHgrid", 45, xsections1n2)
    parse(options.dir1b, "cteq66alphas.LHgrid", 5, xsections2n2)
    for i in range(len(xsections1n)):
      xsections1n[i]+=xsections1n2[i]
    for i in range(len(xsections2n)):
      xsections2n[i]+=xsections2n2[i]
  for i in range(len(xsections1n)):
    ratios1.append(xsections1n[i]/xsections1d[i])
  for i in range(len(xsections2n)):  
    ratios2.append(xsections2n[i]/xsections2d[i])
elif str(options.pdf) == "NNPDF":
  xsections1n = []
  xsections2n = []
  xsections3n = []
  xsections1d = []
  xsections2d = []
  xsections3d = []
  parse(options.dir1, "NNPDF20_100.LHgrid", 101, xsections1n)
  parse(options.dir1, "NNPDF20_as_0121_100.LHgrid", 101, xsections2n)
  parse(options.dir1, "NNPDF20_as_0117_100.LHgrid", 101, xsections3n)
  parse(options.dir2, "NNPDF20_100.LHgrid", 101, xsections1d)
  parse(options.dir2, "NNPDF20_as_0121_100.LHgrid", 101, xsections2d)
  parse(options.dir2, "NNPDF20_as_0117_100.LHgrid", 101, xsections3d)
  if options.dir1b != None:
    xsections1n2 = []
    xsections2n2 = []
    xsections3n2 = []
    parse(options.dir1b, "NNPDF20_100.LHgrid", 101, xsections1n2)
    parse(options.dir1b, "NNPDF20_as_0121_100.LHgrid", 101, xsections2n2)
    parse(options.dir1b, "NNPDF20_as_0117_100.LHgrid", 101, xsections3n2)
    for i in range(len(xsections1n)):
      xsections1n[i]+=xsections1n2[i]
      xsections2n[i]+=xsections2n2[i]
      xsections3n[i]+=xsections3n2[i]
  for i in range(len(xsections1n)):
    ratios1.append(xsections1n[i]/xsections1d[i])
    ratios2.append(xsections2n[i]/xsections2d[i])
    ratios3.append(xsections3n[i]/xsections3d[i]) 
else:
  print str(options.pdf)+" is not a supported pdf"
  sys.exit(1)


if str(options.pdf) == "MSTW" :
  if ( len(ratios1) != 41 or len(ratios2) != 41 or len(ratios3) != 41 ):
    print 'missing pdfs! found only %3d, %3d, %3d xsection values, expected 41' %(len(ratios1), len(ratios2), len(ratios3))
    sys.exit(1)
  else:
    pdferror = mstw_pdf(ratios1)
    print 'MSTW pdf error: %5.3f + %5.3f - %5.3f' %(pdferror[0], pdferror[1], pdferror[2]) 
    #if (onlypdferrors is False):
    pdfalphaerro = mstw_pdf_alpha(ratios1, ratios2, ratios3)
    print 'MSTW combined pdf-alpha error: %5.3f + %5.3f - %5.3f' %(pdfalphaerro[0], pdfalphaerro[1], pdfalphaerro[2])  
    print 'MSTW Value+delta,  %5.2f, Value-delta %5.2f' %(pdfalphaerro[0]+pdfalphaerro[1], pdfalphaerro[0]-pdfalphaerro[2])

if str(options.pdf) == 'NNPDF':
  if ( len(ratios1) != 101 or len(ratios2) != 101 or len(ratios3) != 101 ): 
    print 'missing pdfs! found only %3d, %3d, %3d xsection values, expected 101' %(len(ratios1), len(ratios2), len(ratios3))
    sys.exit(1)
  else:
    pdferror = nnpdf_pdf(ratios1)
    print 'NNPDF pdf error: %5.3f + %5.3f - %5.3f' %(pdferror[0], pdferror[1], pdferror[2])
    #if (onlypdferrors is False):
    alphaerror = nnpdf_alpha(ratios1, ratios2, ratios3)
    print 'NNPDF alpha error: %5.3f + %5.3f - %5.3f' %(alphaerror[0], alphaerror[1], alphaerror[2])
    pdfalphaerror = nnpdf_alpha_pdf(ratios1, ratios2, ratios3)
    print 'NNPDF combined pdf-alpha error: %5.3f + %5.3f - %5.3f' %(pdfalphaerror[0], pdfalphaerror[1], pdfalphaerror[2])
    print 'NNPDF Value+delta,  %5.2f, Value-delta %5.2f' %(pdfalphaerror[0]+pdfalphaerror[1], pdfalphaerror[0]-pdfalphaerror[2])
    
 
if str(options.pdf) == "CTEQ":
  if (len(ratios1) != 45 or len(ratios2) != 5 ): 
      print 'missing pdfs! found only %3d, %3d xsection values, expected 5' %(len(ratios1), len(ratios2))
      sys.exit(1)
  else:
    pdferror = cteq_pdf(ratios1)
    print 'CTEQ66 pdf error: %5.3f + %5.3f - %5.3f' %(pdferror[0], pdferror[1], pdferror[2])
    #if (onlypdferrors is False):
    alphaerror = cteq_alpha(ratios2)
    print 'CTEQ66 alpha error: %5.3f + %5.3f - %5.3f' %(alphaerror[0], alphaerror[1], alphaerror[2])
    pdfalphaerror = cteq_pdf_alpha(ratios1, ratios2)
    print 'CTEQ66 combined pdf-alpha error: %5.3f + %5.3f - %5.3f' %(pdfalphaerror[0], pdfalphaerror[1], pdfalphaerror[2]) 
    print 'CTEQ66 Value+delta,  %5.2f, Value-delta %5.2f' %(pdfalphaerror[0]+pdfalphaerror[1], pdfalphaerror[0]-pdfalphaerror[2])




