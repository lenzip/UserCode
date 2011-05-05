
import FWCore.ParameterSet.Config as cms

source = cms.Source(
	"PoolSource",

	noEventSort = cms.untracked.bool(True),
	duplicateCheckMode = cms.untracked.string("noDuplicateCheck"),
	fileNames = cms.untracked.vstring()
)
source.fileNames.extend([
'/store/cmst3/user/lenzip/CMG/SusyLP/TTJets_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_10_1_FqU.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/TTJets_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_11_1_O9n.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/TTJets_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_12_1_jpU.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/TTJets_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_13_1_vlH.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/TTJets_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_14_1_tPE.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/TTJets_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_15_1_o4B.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/TTJets_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_16_1_53y.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/TTJets_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_17_1_Z64.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/TTJets_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_18_1_Jrx.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/TTJets_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_19_1_d0A.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/TTJets_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_1_1_Nwt.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/TTJets_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_20_1_nHh.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/TTJets_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_21_1_TDq.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/TTJets_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_22_1_lwO.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/TTJets_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_23_1_RAB.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/TTJets_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_24_1_SRL.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/TTJets_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_25_1_Z4V.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/TTJets_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_26_1_xuW.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/TTJets_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_27_1_k4G.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/TTJets_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_2_1_4lB.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/TTJets_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_3_1_vE2.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/TTJets_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_4_1_3ku.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/TTJets_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_5_1_zSI.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/TTJets_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_6_1_idI.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/TTJets_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_7_1_VP7.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/TTJets_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_8_1_Yxl.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/TTJets_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_9_1_Wkz.root'
])
