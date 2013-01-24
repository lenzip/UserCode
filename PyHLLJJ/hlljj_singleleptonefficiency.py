#!/usr/bin/env python
import os,string,sys,commands,time,ConfigParser
from ROOT import *
from array import array
import numpy
steps = ["Kinematics", "mvaId", "Iso"]
if len(sys.argv)!=6:
    print "Usage:",sys.argv[0]+" <plot directory> <step numerator> <step denominator> <maxevent> <batch 1=yes>"
    sys.exit(1)

plot_dire=sys.argv[1]+"/"
stepnum=sys.argv[2]
stepden=sys.argv[3]
if (stepnum not in steps) or (stepden not in steps):
  print "only the following steps are allowed",steps
  sys.exit(1)
maxevent=int(sys.argv[4])
batch=int(sys.argv[5])
if batch==1:
    print "working in batch mode, no plot will be shown"
    gROOT.SetBatch(True)
    

print "Parameters:",plot_dire,stepnum,stepden,maxevent,batch

os.system("mkdir -p "+plot_dire)



mclist=[

    ["checkPresel/VBFH125noselection_51/hjjlltreeproducerEff_hjjllanalyzer/hjjlltreeproducerEff_hjjllanalyzer_tree.root",5.90*0.0538378,"VBFH125","VBFH125"],
    ]
treename="hjjlltreeproducerEff_hjjllanalyzer"

# define special histograms
step_h=[]

# Define all others
# syntax: name, variable, nibn,xmin,xmax,visualize, tag (no strange character here),title,xlabel,ylabel 

def_plot=true
h_list=[
    ["mu_pt","event.murecPt", 100,0,200,def_plot, "", "", "", "", "murec"],
    ["e_pt","event.erecPt", 100,0,200,def_plot, "", "", "", "", "erec"],
    ["mu_eta","event.murecEta", 50,-3,3,def_plot, "", "", "", "", "murec"],
    ["e_eta","event.erecEta", 50,-3,3,def_plot, "", "", "", "", "erec"],
    ]
    

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

def evalCondition(finalstep, flavor):
  condition = True
  laststep = "Undefined"
  for step in steps:
    if laststep == finalstep:
      break
    thisstep = eval('event.'+flavor+step)

    if thisstep > 0: 
      thiscondition = True
    else:
      thiscondition = False
    condition = condition and thiscondition
    laststep = step
  return condition

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
        addcut = addcut and (event.iszee or event.iszmumu)
        addcut = addcut and (event.murecGenMatch == True or event.erecGenMatch == True)

        #addcut = ( addcut and ( (event.iszee and event.truezlepmass>12 and event.truezlepmass < 75) or ((event.iszmumu and event.truezlepmass>12 and event.truezlepmass < 75)) ) )
        #addcut = ( addcut and ( (event.iszee and event.truezlepmass > 75) or ((event.iszmumu and event.truezlepmass>75 )) ) )


        # here we can put all plots after selection
        for i,hnum in enumerate(hnumglob):
          param=h_list[i]
          if addcut and evalCondition(stepnum, param[10]) :
            hnum.Fill(eval(param[1]), event.weight)
       
        for j,hden in enumerate(hdenglob):
          param=h_list[j]
          if addcut and evalCondition(stepden, param[10]) :
            hden.Fill(eval(param[1]), event.weight)
            
    treefile.Close()
    #now make the efficiency plots
    for i,hnum in enumerate(hnumglob):
      graphglob.append(TGraphAsymmErrors(hnum, hdenglob[i]))
      graphglob[len(graphglob)-1].SetNameTitle((h_list[i])[0]+"_eff_"+stepnum+" versus "+stepden,(h_list[i])[0]+"_eff_"+stepnum+" versus "+stepden)

          
    
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


#testcanvas = TCanvas("test","test")
#testcanvas.Divide(1,2)
#testcanvas.cd(1)
#numsinglemu.Draw()
#densinglemu.Draw("sames")
#testcanvas.cd(2)
#testgraph = TGraphAsymmErrors(numsinglemu, densinglemu)
#testgraph.Draw('AP')
#testcanvas.Print(plot_dire+"/test.png")
#testcanvas.Print(plot_dire+"/test.C")

#stackmzh_h2 = THStack("mzmh", "mzmh")
#for index in range(0,len(mclist)):
#    stackmzh_h2.Add(mzh_h2[index])
#stackmzh_h2.Draw("box")


a=raw_input("hit a key to exit...")
