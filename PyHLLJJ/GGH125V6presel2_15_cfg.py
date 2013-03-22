import CMGTools.RootTools.fwlite.Config as cfg
from HiggsAna.PyHLLJJ.analhjjll_cff import *
hjjllAna.matchgen=True
hjjllAna.matchvbfgen=False

GGH125 = cfg.MCComponent(
    name = 'GGH125',
    files = [
"root://eoscms//eos/cms/store/user/lenzip/CMG/GluGluToHToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_0.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/GluGluToHToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_1.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/GluGluToHToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_10.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/GluGluToHToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_11.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/GluGluToHToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_12.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/GluGluToHToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_13.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/GluGluToHToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_14.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/GluGluToHToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_15.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/GluGluToHToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_16.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/GluGluToHToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_17.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/GluGluToHToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_18.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/GluGluToHToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_19.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/GluGluToHToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_2.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/GluGluToHToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_20.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/GluGluToHToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_21.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/GluGluToHToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_22.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/GluGluToHToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_23.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/GluGluToHToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_24.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/GluGluToHToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_25.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/GluGluToHToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_26.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/GluGluToHToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_27.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/GluGluToHToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_28.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/GluGluToHToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_29.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/GluGluToHToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_3.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/GluGluToHToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_30.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/GluGluToHToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_31.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/GluGluToHToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_32.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/GluGluToHToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_33.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/GluGluToHToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_4.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/GluGluToHToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_5.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/GluGluToHToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_6.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/GluGluToHToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_7.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/GluGluToHToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_8.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/GluGluToHToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_9.root"
    ],
    
    xSection = 0.00114477 * 11050000, 
    nGenEvents = 6972564, # dummy 
    triggers = [],
    intLumi = 1000,
    effCorrFactor = 1 )

selectedComponents = [GGH125]

GGH125.splitFactor = 1
    
config = cfg.Config( components = selectedComponents,
                     sequence = sequence )
