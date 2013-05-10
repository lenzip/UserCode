 #!/usr/bin/env python
import os
import gzip

sel_ele = 0
sel_mu  = 0
sel_tot = 0
error   = 0
n_tot   = 0
file    = False
for root, dirs, files in os.walk('/afs/cern.ch/work/l/lenzip/CMG/CMGTools/production2/CMGTools/CMSSW_5_3_3_patch3/src/HiggsAna/PyHLLJJ/GluGluToHToZZTo2L2Q_M-200_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM_1367314528/CMGV6_Jobs/'):
    for name in files:
        filename = os.path.join(root, name)
        file = False
#        print filename
        if filename.endswith("STDOUT.gz"):
            f = gzip.open(filename, 'r')
            file = True
        elif filename.endswith("STDOUT"):
            f = open(filename, 'r')
            file = True

        if file:
#            print filename            
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
            sel_ele = sel_ele + float(words_ele[4])
            sel_mu  = sel_mu  + float(words_mu[4])
            n_tot   = n_tot   + float(words_ele[3])
                        
#    print "sel_ele", sel_ele

print "sel_ele = ", sel_ele    
print "sel_mu  = ", sel_mu
print "sel_tot = ", sel_ele + sel_mu
print "n tot   = ", n_tot
print "eff     = ", (sel_ele + sel_mu)/n_tot 
