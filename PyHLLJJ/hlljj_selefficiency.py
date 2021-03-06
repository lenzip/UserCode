#!/usr/bin/env python
import os,string,sys,commands,time,ConfigParser
from ROOT import *
from array import array
import numpy
if len(sys.argv)!=6:
    print "Usage:",sys.argv[0]+" <plot directory> <step numerator> <step denominator> <maxevent> <batch 1=yes>"
    sys.exit(1)

plot_dire=sys.argv[1]+"/"
stepnum=int(sys.argv[2])
stepden=int(sys.argv[3])
maxevent=int(sys.argv[4])
batch=int(sys.argv[5])
if batch==1:
    print "working in batch mode, no plot will be shown"
    gROOT.SetBatch(True)
    

print "Parameters:",plot_dire,stepnum,stepden,maxevent,batch

os.system("mkdir -p "+plot_dire)



mclist=[

    ["checkPresel/VBFH125noselection_13/hjjlltreeproducer_hjjllanalyzer/hjjlltreeproducer_hjjllanalyzer_tree.root",5.90*0.0538378,"VBFH125","VBFH125"],
    ]
treename="hjjlltreeproducer_hjjllanalyzer"

# define special histograms
step_h=[]

# Define all others
# syntax: name, variable, nibn,xmin,xmax,visualize, tag (no strange character here),title,xlabel,ylabel 

def_plot=true
h_list=[
    ["leadingmu_pt","event.mu1recPt", 100,0,200,def_plot, "", "", "", ""],
    ["leadinge_pt","event.e1recPt", 100,0,200,def_plot, "", "", "", ""],
    ["leadingmu_eta","event.mu1recEta", 50,-3,3,def_plot, "", "", "", ""],
    ["leadinge_eta","event.e1recEta", 50,-3,3,def_plot, "", "", "", ""],
    ["subleadingmu_pt","event.mu2recPt", 100,0,200,def_plot, "", "", "", ""],
    ["subleadinge_pt","event.e2recPt", 100,0,200,def_plot, "", "", "", ""],
    ["subleadingmu_eta","event.mu2recEta", 50,-3,3,def_plot, "", "", "", ""],
    ["subleadinge_eta","event.e2recEta", 50,-3,3,def_plot, "", "", "", ""],
#    ["mee","event.mee", 200,0,200,def_plot, "", "", "", ""],
#    ["mmumu","event.mmumu", 200,0,200,def_plot, "", "", "", ""],
    ]
    
steps = ["2 leptons pt > 10", " 2 leptons with id", "2 leptons with id and iso"]

linecolors=[2, 4]

graphglob=[]
hnumglob=[]
hdenglob=[]

mc=mclist[0]
tag=mc[2]
tagnum=tag+"_step_"+str(stepnum)
tagden=tag+"_step_"+str(stepden)

numsinglemu = TH1F('numsinglemu', 'numsinglemu',100,0,200) 
densinglemu = TH1F('densinglemu', 'densinglemu',100,0,200) 

for h in range(0,len(h_list)):
  param=h_list[h]
  hnumglob.append(TH1F(param[0]+tagnum,param[0]+tagnum,param[2],param[3],param[4]))  
  hnumglob[len(hnumglob)-1].SetLineColor(linecolors[0])
  hnumglob[len(hnumglob)-1].SetLineWidth(2)
  hnumglob[len(hnumglob)-1].SetMarkerColor(linecolors[0])
  hnumglob[len(hnumglob)-1].Sumw2()

  hdenglob.append(TH1F(param[0]+tagden,param[0]+tagden,param[2],param[3],param[4]))     
  hdenglob[len(hdenglob)-1].SetLineColor(linecolors[1])
  hdenglob[len(hdenglob)-1].SetLineWidth(2)
  hdenglob[len(hdenglob)-1].SetMarkerColor(linecolors[1])
  hdenglob[len(hdenglob)-1].Sumw2()
#maxevent=100000000
# now loop on tree and project
nhtt=0
nhtt_sel=0
nhbbtt=0
nhbbtt_sel=0

for index,mc in enumerate(mclist):
    rootfile=mc[0]
    tag=mc[2]
    treefile=TFile.Open(rootfile)
    print "opening ",rootfile
    tree=treefile.Get(treename)
    nevents=tree.GetEntries()
    nevents=min(nevents,maxevent)
    # loop on tree entries
    #hnumloc=hnumglob[index]
    #hdenloc=hdenglob[index]
    read=0
    weight=1
    for event  in tree:
        if read>=nevents:
            break
        read+=1
        if read % 10000 ==1:
            print "Reading event:",read,'/',nevents
        

        addcut = True
            
        addcut = ( addcut and ( (event.iszee and event.truezlepmass>12 and event.truezlepmass < 75) or ((event.iszmumu and event.truezlepmass>12 and event.truezlepmass < 75)) ) )
        #addcut = ( addcut and ( (event.iszee and event.truezlepmass > 75) or ((event.iszmumu and event.truezlepmass>75 )) ) )

        if event.step>=stepnum and addcut:
            # here we can put all plots after selection
            numsinglemu.Fill(event.mu1recPt, event.weight)
            numsinglemu.Fill(event.mu2recPt, event.weight)
            for i,hnum in enumerate(hnumglob):
                param=h_list[i]
                hnum.Fill(eval(param[1]), event.weight)
       
        if event.step>=stepden and addcut:
            # here we can put all plots after selection
            densinglemu.Fill(event.mu1recPt, event.weight)
            densinglemu.Fill(event.mu2recPt, event.weight)
            for j,hden in enumerate(hdenglob):
                param=h_list[j]
                hden.Fill(eval(param[1]), event.weight) 
            
    treefile.Close()
    #now make the efficiency plots
    for i,hnum in enumerate(hnumglob):
      graphglob.append(TGraphAsymmErrors(hnum, hdenglob[i]))
      graphglob[len(graphglob)-1].SetNameTitle((h_list[i])[0]+"_eff_"+steps[stepnum-1]+" versus "+steps[stepden-1],(h_list[i])[0]+"_eff_"+steps[stepnum-1]+" versus "+steps[stepden-1])

          
    
# now we can plot them


# first prepare legenda
#yheaderstart=.95-.023*len(mclist)
#leg_hist = TLegend(0.45,0.55,0.65,0.85);
#leg_hist.SetFillColor(0)# Have a white background
#leg_hist.SetTextSize(0.03)
#text_lumi = TText(60,320,"L = 500 fb-1");

first=True
# legenda

#c1.Divide(1,2)
#for index in range(0,len(mclist)):
#    opt="same"
#    mc=mclist[index]
#    if (first):
#        first=False
#        opt=""
#    print index,opt
#    step_h[index].Draw(opt)
#    if index == 1:
#      leg_hist.AddEntry( step_h[index], "All backgrounds", "lf");
#    leg_hist.AddEntry(step_h[index],mc[3],"l")



canv=[]
gStyle.SetOptStat(11111111)
for i,h1 in enumerate(h_list):
    plot=h1[5]
    if not plot:
        continue
    tag=h1[0]
    canv.append(TCanvas("c_"+tag,"c_"+tag))
    canv[len(canv)-1].Divide(1,2)
    canv[len(canv)-1].cd(1)
    hdenglob[i].Draw()
    hdenglob[i].GetXaxis().SetTitle(h1[8])
    hdenglob[i].GetYaxis().SetTitle(h1[9])
    hnumglob[i].Draw("sames")
    canv[len(canv)-1].cd(2)
    graphglob[i].GetXaxis().SetTitle(h1[8])
    graphglob[i].GetYaxis().SetTitle("#epsilon")
    graphglob[i].Draw("AP")
    
    #leg_hist.Draw()
    #text_lumi.Draw()
    canv[len(canv)-1].Print(plot_dire+"/"+tag+".png")
    canv[len(canv)-1].Print(plot_dire+"/"+tag+".C")


testcanvas = TCanvas("test","test")
testcanvas.Divide(1,2)
testcanvas.cd(1)
numsinglemu.Draw()
densinglemu.Draw("sames")
testcanvas.cd(2)
testgraph = TGraphAsymmErrors(numsinglemu, densinglemu)
testgraph.Draw('AP')
testcanvas.Print(plot_dire+"/test.png")
testcanvas.Print(plot_dire+"/test.C")

#stackmzh_h2 = THStack("mzmh", "mzmh")
#for index in range(0,len(mclist)):
#    stackmzh_h2.Add(mzh_h2[index])
#stackmzh_h2.Draw("box")


a=raw_input("hit a key to exit...")
