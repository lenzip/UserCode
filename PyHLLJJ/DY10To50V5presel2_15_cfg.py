import CMGTools.RootTools.fwlite.Config as cfg
from HiggsAna.PyHLLJJ.analhjjll_cff import *
hjjllAna.matchgen=False

DY10To50 = cfg.MCComponent(
    name = 'DY10To50',
    files = [
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-10To50filter_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV5/cmgTuple_108.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-10To50filter_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV5/cmgTuple_11.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-10To50filter_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV5/cmgTuple_115.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-10To50filter_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV5/cmgTuple_12.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-10To50filter_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV5/cmgTuple_15.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-10To50filter_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV5/cmgTuple_29.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-10To50filter_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV5/cmgTuple_40.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-10To50filter_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV5/cmgTuple_44.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-10To50filter_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV5/cmgTuple_50.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-10To50filter_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV5/cmgTuple_51.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-10To50filter_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV5/cmgTuple_61.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-10To50filter_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV5/cmgTuple_70.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-10To50filter_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV5/cmgTuple_81.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-10To50filter_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV5/cmgTuple_82.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-10To50filter_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV5/cmgTuple_88.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-10To50filter_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV5/cmgTuple_9.root'
    ],
    
    xSection = 0.00114477 * 11050000, 
    nGenEvents = 6972564, # dummy 
    triggers = [],
    intLumi = 1000,
    effCorrFactor = 1 )

selectedComponents = [DY10To50]

DY10To50.splitFactor = 1
    
config = cfg.Config( components = selectedComponents,
                     sequence = sequence )
