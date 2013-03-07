#!/usr/bin/env python
import os,string,sys,commands,time,ConfigParser
from ROOT import *
from array import array
import numpy
# legenda:
# step 0--> all
# step 1--> after njet cut
# step 2--> after cut on energy
# step 3--> after 2 tau candidate
# step 4--> after testID
# step 5--> after findhz

if len(sys.argv)!=5:
    print "Usage:",sys.argv[0]+" <plot directory> <step> <maxevent> <batch 1=yes>"
    sys.exit(1)

plot_dire=sys.argv[1]+"/"
stepplot=int(sys.argv[2])
maxevent=int(sys.argv[3])
batch=int(sys.argv[4])
if batch==1:
    print "working in batch mode, no plot will be shown"
    gROOT.SetBatch(True)
    

print "Parameters:",plot_dire,stepplot,maxevent,batch

os.system("mkdir -p "+plot_dire)


lumi=20000
def_condition = " and (event.J1Pt > 15 and event.J2Pt > 15. and event.VBFJ1Pt > 15. and event.VBFJ2Pt > 15.)"
def_condition += " and ((event.M1Pt > 15 and event.M2Pt > 10.) or (event.E1Pt > 20 and event.E2Pt > 10.))"
mclist=[
    #passo il peso direttamente
    #["vbfh125_v5_4.root",(1.578*0.0264*0.14)*0.19,"VBFH125Match","VBFH125 matched", "event.isDecayMatched and event.isVBFMatched", ], 
    ["/tmp/lenzip/vbfh125_v5_4.root",(1.578*0.0264*0.14)*0.19,"VBFH125Match","VBFH125 matched", "event.isDecayMatched and event.isVBFMatched"+def_condition, ], 
    ["/tmp/lenzip/vbfh125_v5_4.root",(1.578*0.0264*0.14)*0.19,"VBFH125NoMatch","VBFH125 not matched", "(not event.isDecayMatched or not event.isVBFMatched)"+def_condition, ], 
    #["/tmp/lenzip/vbfh125_v5_4.root",(1.578*0.0264*0.14)*0.19,"VBFH125Match","VBFH125 matched", "event.isDecayMatched"+def_condition, ], 
    #["/tmp/lenzip/vbfh125_v5_4.root",(1.578*0.0264*0.14)*0.19,"VBFH125NoMatch","VBFH125 not matched", "(not event.isDecayMatched or not event.isVBFMatched)"+def_condition], 
    ["/tmp/lenzip/ggh125_v5.root",(19.52*0.0264*0.14)*0.143385404875,"GGH125","GGH125", "True"+def_condition], 
    #["/tmp/lenzip/ggh125_v5.root",(19.52*0.0264*0.14)*0.143385404875,"GGH125","GGH125", "event.isDecayMatched"+def_condition], 
    ["/tmp/lenzip/dy50.root",(3503.71)*0.0834175089149,"DY50", "DY50", "True"+def_condition], 
    ["/tmp/lenzip/dy10to50.root",(13124*0.069)*0.0465582665252,"DY10to50", "DY10to50", "True"+def_condition], 
    ]
treename="hjjllcombinatorial_hjjllanalyzer"

# luminosity to normalize (in pb-1)

# step at which the plot should be made
#stepplot=6

# define special histograms
step_h=[]
genrec_s3_t1=[]
genrec_s3_t2=[]
mzh_h2=[]
dgen1_vs_iso_h2=[]
dgen2_vs_iso_h2=[]

# Define all others
# syntax: name, variable, nibn,xmin,xmax,visualize, tag (no strange character here),title,xlabel,ylabel 

step_label=["all","njet>4","ejet>10","2 taucand","2 good taucand","jet sele","mzmh cut","btag"]

def_plot=true
h1_list=[
    ["ZJJMass" ,"event.ZJJMass" ,100,0,200,def_plot, "", "", "Mass J1J2 [GeV]", ""],
    ["J1Pt" ,"event.J1Pt" ,100,0,200,def_plot, "", "", "pT J1 [GeV]", ""],
    ["J2Pt" ,"event.J2Pt" ,100,0,200,def_plot, "", "", "pT J2 [GeV]", ""],
    ["J1Eta" ,"event.J1Eta" ,100,-5,5,def_plot, "", "", "#eta J1", ""],
    ["J2Eta" ,"event.J2Eta" ,100,-5,5,def_plot, "", "", "#eta J2", ""],
    ["VBFJ1Pt" ,"event.VBFJ1Pt" ,100,0,200,def_plot, "", "", "pT VBF J1 [GeV]", ""],
    ["VBFJ2Pt" ,"event.VBFJ2Pt" ,100,0,200,def_plot, "", "", "pT VBF J2 [GeV]", ""],
    ["VBFJ1Eta" ,"event.VBFJ1Eta" ,100,-5,5,def_plot, "", "", "#eta VBF J1", ""],
    ["VBFJ2Eta" ,"event.VBFJ2Eta" ,100,-5,5,def_plot, "", "", "#eta VBF J2", ""],
    ["ZEEMass" ,"event.ZEEMass" ,100,0,200,def_plot, "", "", "Mass e+e- [GeV]", ""],
    ["ZMMMass" ,"event.ZMMMass" ,100,0,200,def_plot, "", "", "Mass #mu+#mu- [GeV]", ""],
    ["HMMJJMass" ,"event.HMMJJMass" ,100,0,400,def_plot, "", "", "Mass H(#mu+#mu-jj)[GeV]", ""],
    ["HEEJJMass" ,"event.HEEJJMass" ,100,0,400,def_plot, "", "", "Mass (e+e-ll)[GeV]", ""],
    ["HEEJJmassVBF" ,"event.HEEJJmassVBF" ,100,0,1000,def_plot, "", "", "Mass VBF pair for H(e+e-jj) [GeV]", ""],
    ["HMMJJmassVBF" ,"event.HMMJJmassVBF" ,100,0,1000,def_plot, "", "", "Mass VBF pair for H(#mu+#mu-jj) [GeV]", ""],
    ["HMMJJdetaVBF" ,"event.HMMJJdetaVBF" ,40,0,10,def_plot, "", "", "#Delta#eta VBF pair for H(#mu+#mu-jj) [GeV]", ""],
    ["HEEJJdetaVBF" ,"event.HEEJJdetaVBF" ,40,0,10,def_plot, "", "", "#Delta#eta VBF pair for H(e+e-jj) [GeV]", ""],
    ["HMMJJDeltaPhiZ" ,"abs(event.HMMJJDeltaPhiZ)" ,40,0,4,def_plot, "", "", "#Delta#phi Z(#mu+#mu-)Z(jj)", ""],
    ["HEEJJDeltaPhiZ" ,"abs(event.HEEJJDeltaPhiZ)" ,40,0,4,def_plot, "", "", "#Delta#phi Z(e+e-)Z(jj)", ""],
    ["HMMJJDeltaPhiZJ1" ,"abs(event.HMMJJDeltaPhiZJ1)" ,40,0,4,def_plot, "", "", "#Delta#phiZ(#mu+#mu-)J1", ""],
    ["HEEJJDeltaPhiZJ1" ,"abs(event.HEEJJDeltaPhiZJ1)" ,40,0,4,def_plot, "", "", "#Delta#phiZ(e+e-)J1", ""],
    ["HMMJJhelcosthetaZl1" ,"event.HMMJJhelcosthetaZl1" ,30,-1.5,1.5,def_plot, "", "", "helcosthetaZl1 H(#mu+#mu-jj)", ""],
    ["HMMJJhelcosthetaZl2" ,"event.HMMJJhelcosthetaZl2" ,20,-0.5,1.5, def_plot, "", "", "helcosthetaZl2 H(#mu+#mu-jj)", ""],
    ["HEEJJhelcosthetaZl1" ,"event.HEEJJhelcosthetaZl1" ,30,-1.5,1.5,def_plot, "", "", "helcosthetaZl1 H(e+e-jj)", ""],
    ["HEEJJhelcosthetaZl2" ,"event.HEEJJhelcosthetaZl2" ,20,-0.5,1.5, def_plot, "", "", "helcosthetaZl2 H(e+e-jj)", ""],
    ["HMMJJSumAbsEtaJ1J2" ,"event.HMMJJSumAbsEtaJ1J2" ,20,0,5., def_plot, "", "|#eta(J1)|+|#eta(J2)|, H(e+e-jj)", "", ""],
    ["HEEJJSumAbsEtaJ1J2" ,"event.HEEJJSumAbsEtaJ1J2" ,20,0,5., def_plot, "", "|#eta(J1)|+|#eta(J2)|, H(#mu+#mu-jj)", "", ""],
    ["HEEJJClassifier" ,"event.HEEJJClassifier" ,50,-0.8, 0.2, def_plot, "", "", "BDT classifier, H(e+e-jj)", ""],
    ["HMMJJClassifier" ,"event.HMMJJClassifier", 50,-0.8, 0.2, def_plot, "", "", "BDT classifier, H(#mu+#mu-jj)", ""],
]
    
    
linecolors=[2, 6, 3, 4, 5]
fillcolors=[0, 4, 4, 4, 4]
fillstyles=[0, 3013, 3013, 3013, 3013]
smooth=[False, False, False, False, False]

h1glob=[]
for index in range(0,len(mclist)):
    mc=mclist[index]
    tag=mc[2]

    step_h.append(TH1F("step_"+tag,"step_"+tag,10,0,10))
    step_h[index].SetLineColor(linecolors[index])
    step_h[index].SetLineWidth(2)
    step_h[index].SetMarkerColor(linecolors[index])
    step_h[index].SetFillColor(fillcolors[index])
    step_h[index].SetFillStyle(fillstyles[index])
    for bin in range(1,len(step_label)+1):
        step_h[index].GetXaxis().SetBinLabel(bin,step_label[bin-1])    
#        step_h[index].LabelsOption("v","X")
    h1loc = []    
    for h1 in range(0,len(h1_list)):
        param=h1_list[h1]
        h1loc.append(TH1F(param[0]+tag,param[0]+tag,param[2],param[3],param[4]))  
        h1loc[len(h1loc)-1].SetLineColor(linecolors[index])
        h1loc[len(h1loc)-1].SetLineWidth(2)
        h1loc[len(h1loc)-1].SetMarkerColor(linecolors[index])
        #if index != 0:
        h1loc[len(h1loc)-1].SetFillStyle(fillstyles[index]);
        h1loc[len(h1loc)-1].SetFillColor(fillcolors[index])
    h1glob.append(h1loc)            

#maxevent=100000000
# now loop on tree and project
nhtt=0
nhtt_sel=0
nhbbtt=0
nhbbtt_sel=0
gROOT.ProcessLine(".L ~/tdrStyle.C");
setTDRStyle()
#gROOT.ProcessLine(".L ~/tdrStyle.C");
for index,mc in enumerate(mclist):
    rootfile=mc[0]
    xsec=mc[1]
    print xsec
    tag=mc[2]

    neventsprocessed = 0
    lastevent=-1
    treefile=TFile.Open(rootfile)
    print "opening ",rootfile
    tree=treefile.Get(treename)
    nevents=tree.GetEntries()
    nevents=min(nevents,maxevent)
    # loop on tree entries
    #weight=mc[1] #xsec*lumi/nevents
    
    h1loc=h1glob[index]
    read=0
    #nevents passing default selection
    npass = 0
    passpresel = False
    for event  in tree:
        if read>=nevents:
            break
        read+=1
        if read % 10000 ==1:
            print "Reading event:",read,'/',nevents
        if (event.eventNumber != lastevent):
          lastevent = event.eventNumber
          neventsprocessed += 1
          if passpresel:
            npass+=1
          passpresel = False

        addcut = eval(mc[4]) 
            
        #addcut = ( addcut and event.mu1recPt>5. ) or ( addcut and event.e1recPt>5. ) and addcut
        #addcut = ( event.dimuonTrigger or event.dielectronTrigger ) and addcut


        #for bin in range(0,int(event.step)+1):
        #    step_h[index].Fill(bin)
                
        #if event.step>=stepplot and addcut:
        if addcut:
            passpresel = True
            # here we can put all plots after selection
            for i,h1 in enumerate(h1loc):
                param=h1_list[i]
        #        print param[1]
                #h1.Fill(eval(param[1]),weight)
                h1.Fill(eval(param[1]))
    weight = xsec/neventsprocessed * lumi
    print xsec, weight, neventsprocessed, npass
    for i,h1 in enumerate(h1loc):
      h1.Scale(weight)
    treefile.Close()
    # renormalize step_h histo for efficiencies
    norm=step_h[index].GetBinContent(1)
    print norm
    if norm==0: norm=-1
    for bin in range(0,step_h[index].GetNbinsX()):
        step_h[index].SetBinContent(bin,step_h[index].GetBinContent(bin)/norm)

# first prepare legenda
#yheaderstart=.95-.023*len(mclist)
leg_hist = TLegend(0.30,0.85,0.98,0.98);
leg_hist.SetFillColor(0)# Have a white background
leg_hist.SetTextSize(0.03)
leg_hist.SetNColumns(2)

leg_hist2 = TLegend(0.30,0.85,0.98,0.98);
leg_hist2.SetFillColor(0)# Have a white background
leg_hist2.SetTextSize(0.03)
leg_hist2.SetNColumns(2)
#text_lumi = TText(60,320,"L = 500 fb-1");

c1=TCanvas("step","step")
#c1.SetFillColor(kWhite)
#c1.SetFillStyle(1001)
#gStyle.SetOptStat(0)
 
first=True
# legenda

#c1.Divide(1,2)
for index in range(0,len(mclist)):
    opt="same"
    mc=mclist[index]
    if (first):
        first=False
        opt=""
#    print index,opt
#    step_h[index].Draw(opt)
    if index==0:
      leg_hist2.AddEntry(step_h[index],mc[3],"l")
    if index == 1:
      leg_hist.AddEntry( step_h[index], "All backgrounds", "lf");
      leg_hist2.AddEntry( step_h[index], "All backgrounds", "lf");
    leg_hist.AddEntry(step_h[index],mc[3],"l")


canv=[]
#gStyle.SetOptStat(11111111)
for i,h1 in enumerate(h1_list):
    plot=h1[5]
    if not plot:
        continue
    tag=h1[0]
    canv.append(TCanvas("c_"+tag,"c_"+tag, 1000, 500))
    canv[len(canv)-1].Divide(2)
    #canv[len(canv)-1].SetLogy()
    #canv[len(canv)-1].
    canv[len(canv)-1].cd(1)  
    stackbyhand = TH1F(tag,tag,h1[2],h1[3],h1[4])
    if h1[6] != "":
      stackh_h = THStack(h1[6], h1[7])
    else:  
      stackh_h = THStack(tag, tag)
    for index in range(1,len(mclist)) : 
    #for index in range(len(mclist)-1,-1,-1):
    #for index in range(len(mclist)-1,0,-1):
        h1loc=h1glob[index]
        print index
        if smooth[index]:
          h1loc[i].Smooth(1, "R")
        stackbyhand.Add(h1loc[i])  
        stackh_h.Add(h1loc[i])
    themax = max(stackbyhand.GetMaximum(), (h1glob[0])[i].GetMaximum()) 
    #stackbyhand.GetYaxis().SetRangeUser(0.02, 1.3*themax)
    stackh_h.Draw()
    if h1[8] != "":
      stackh_h.GetXaxis().SetTitle(h1[8])
    if h1[9] != "":
      stackh_h.GetYaxis().SetTitle(h1[9])
    (h1glob[0])[i].Scale(5000)
    #(h1glob[0])[i].GetYaxis().SetRangeUser(0.02, 1.3*themax)
    stackh_h.SetMaximum(20*themax)
    stackh_h.SetMinimum(0.1)
    (h1glob[0])[i].Draw("sames")
    leg_hist.Draw("sames")
    gPad.SetLogy()
    gPad.Update()
    #text_lumi.Draw()
    canv[len(canv)-1].cd(2)
    #if h1[6] != "":
    #  normstackh_h = THStack(h1[6]+"norm", h1[7]+"norm")
    #else:
    #  normstackh_h = THStack(tag+"norm", tag+"norm")
    #for index1 in range(len(mclist)-1,-1,-1):
    #for index in range(len(mclist)-1,0,-1):
    #    h1loc=h1glob[index1]
    #    print index1
    #    if smooth[index1]:
    #      h1loc[i].Smooth(1, "R")
    #    h1loc2 = h1loc[i].Clone()
    #    h1loc2.SetName=str(h1loc[i].GetName())+"norm"
    #    h1loc2.SetTitle=str(h1loc[i].GetTitle())+"norm"
    #    normstackh_h.Add(h1loc2.Scale(1/stackbyhand.Integral()))

    stackbyhand.GetXaxis().SetTitle(h1[8])
    stackbyhand.GetYaxis().SetTitle(h1[9])
    stackbyhand.SetLineColor(fillcolors[2])
    stackbyhand.SetLineWidth(2)
    #stackbyhand.DrawNormalized()
    stackbyhand.Scale(1/stackbyhand.Integral())
    h1clone = (h1glob[0])[i].Clone()
    h1clone.Scale(1/h1clone.Integral())
    themax = max(stackbyhand.GetMaximum(), h1clone.GetMaximum())
    stackbyhand.GetYaxis().SetRangeUser(0, 1.3*themax)
    stackbyhand.Draw()
    h1clone.Draw("sames")
    #(h1glob[0])[i].Clone().DrawNormalized("sames")
    leg_hist2.Draw("sames")
    #if h1[8] != "":
    #  normstackh_h.GetXaxis().SetTitle(h1[8])
    #if h1[9] != "":
    #  normstackh_h.GetYaxis().SetTitle(h1[9])
    #leg_hist.Draw("sames")
    canv[len(canv)-1].Print(plot_dire+"/"+tag+".png")
    canv[len(canv)-1].Print(plot_dire+"/"+tag+".C")




#stackmzh_h2 = THStack("mzmh", "mzmh")
#for index in range(0,len(mclist)):
#    stackmzh_h2.Add(mzh_h2[index])
#stackmzh_h2.Draw("box")


a=raw_input("hit a key to exit...")
