import CMGTools.RootTools.fwlite.Config as cfg
from HiggsAna.PyHLLJJ.analhjjll_cff import *
hjjllAna.matchgen=False

VBFH125noselection = cfg.MCComponent(
    name = 'VBFH125noselection',
    files = [
'root://eoscms//eos/cms/store/cmst3/user/lenzip/CMG/VBF_HToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV2_noselection/cmgTuple_0.root',    
'root://eoscms//eos/cms/store/cmst3/user/lenzip/CMG/VBF_HToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV2_noselection/cmgTuple_1.root',    
'root://eoscms//eos/cms/store/cmst3/user/lenzip/CMG/VBF_HToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV2_noselection/cmgTuple_2.root',    
'root://eoscms//eos/cms/store/cmst3/user/lenzip/CMG/VBF_HToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV2_noselection/cmgTuple_3.root',    
'root://eoscms//eos/cms/store/cmst3/user/lenzip/CMG/VBF_HToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV2_noselection/cmgTuple_4.root',    
#'root://eoscms//eos/cms/store/cmst3/user/lenzip/CMG/VBF_HToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV2_noselection/cmgTuple_5.root',    
'root://eoscms//eos/cms/store/cmst3/user/lenzip/CMG/VBF_HToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV2_noselection/cmgTuple_6.root',    
'root://eoscms//eos/cms/store/cmst3/user/lenzip/CMG/VBF_HToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV2_noselection/cmgTuple_7.root',    
#  'file:../HLLJJCommon/prod/cmgTuple.root'
    ],
    
    xSection = 0.00114477 * 11050000, 
    nGenEvents = 6972564, # dummy 
    triggers = [],
    intLumi = 1000,
    effCorrFactor = 1 )

selectedComponents = [VBFH125noselection]

VBFH125noselection.splitFactor = 1
    
config = cfg.Config( components = selectedComponents,
                     sequence = sequence )
