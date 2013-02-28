#!/usr/bin/env python
import os
import gzip

sel_ele = 0
sel_mu  = 0
sel_tot = 0
error   = 0
for root, dirs, files in os.walk('./prod/DYJetsToLL_M-10To50filter_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM_1361049677/CMGV3_Jobs/'):
    for name in files:
        filename = os.path.join(root, name)
        
 #       print filename
        if filename.endswith("STDOUT.gz"):
            f = gzip.open(filename, 'r')
            goodlines_ele = []
            goodlines_mu = []
                        
            for line in f:
                if "preselEle" in line:
                    goodlines_ele.append(line) 
                    #words = goodlines[0].split()
                    #for word in words:
                elif "preselMu" in line:
                    goodlines_mu.append(line)
                elif "FatalSystemSignal" in line:
                    error += 1
                    print "error in file: ", filename
                                                            
            if len(goodlines_ele) and len(goodlines_mu):        
                words_ele = goodlines_ele[0].split()
                words_mu  = goodlines_mu[0].split()
                         
#            print words[4]
            sel_ele = sel_ele + int(words_ele[4])
            sel_mu  = sel_mu  + int(words_mu[4])
                        
#    print "sel_ele", sel_ele

print "sel_ele ", sel_ele    
print "sel_mu  ", sel_mu
print "sel_tot ", sel_ele + sel_mu
