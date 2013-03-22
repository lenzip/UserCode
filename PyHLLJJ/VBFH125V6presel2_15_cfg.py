import CMGTools.RootTools.fwlite.Config as cfg
from HiggsAna.PyHLLJJ.analhjjll_cff import *
#hjjllAna.matchgen=False

VBFH125 = cfg.MCComponent(
    name = 'VBFH125',
    files = [
#125    
'root://eoscms//eos/cms/store/user/lenzip/CMG/VBF_HToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_0.root',    
'root://eoscms//eos/cms/store/user/lenzip/CMG/VBF_HToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_1.root',    
'root://eoscms//eos/cms/store/user/lenzip/CMG/VBF_HToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_2.root',    
'root://eoscms//eos/cms/store/user/lenzip/CMG/VBF_HToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_3.root',    
'root://eoscms//eos/cms/store/user/lenzip/CMG/VBF_HToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_4.root',    
'root://eoscms//eos/cms/store/user/lenzip/CMG/VBF_HToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_5.root',    
'root://eoscms//eos/cms/store/user/lenzip/CMG/VBF_HToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_6.root',    
'root://eoscms//eos/cms/store/user/lenzip/CMG/VBF_HToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_7.root',  
#120
"root://eoscms//eos/cms/store/user/lenzip/CMG/VBF_HToZZTo2L2Q_M-120_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_0.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/VBF_HToZZTo2L2Q_M-120_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_1.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/VBF_HToZZTo2L2Q_M-120_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_2.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/VBF_HToZZTo2L2Q_M-120_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_3.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/VBF_HToZZTo2L2Q_M-120_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_4.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/VBF_HToZZTo2L2Q_M-120_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_5.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/VBF_HToZZTo2L2Q_M-120_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_6.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/VBF_HToZZTo2L2Q_M-120_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_7.root",
#130
"root://eoscms//eos/cms/store/user/lenzip/CMG/VBF_HToZZTo2L2Q_M-130_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_0.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/VBF_HToZZTo2L2Q_M-130_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_1.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/VBF_HToZZTo2L2Q_M-130_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_2.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/VBF_HToZZTo2L2Q_M-130_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_3.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/VBF_HToZZTo2L2Q_M-130_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_4.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/VBF_HToZZTo2L2Q_M-130_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_5.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/VBF_HToZZTo2L2Q_M-130_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_6.root"

#  'file:cmgTuple.root'
    ],
    
    xSection = 0.00114477 * 11050000, 
    nGenEvents = 6972564, # dummy 
    triggers = [],
    intLumi = 1000,
    effCorrFactor = 1 )

selectedComponents = [VBFH125]

VBFH125.splitFactor = 1
    
config = cfg.Config( components = selectedComponents,
                     sequence = sequence )
