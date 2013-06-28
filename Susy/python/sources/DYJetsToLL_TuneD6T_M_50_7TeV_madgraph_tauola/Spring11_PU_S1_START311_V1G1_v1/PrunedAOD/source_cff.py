
import FWCore.ParameterSet.Config as cms

source = cms.Source(
	"PoolSource",

	noEventSort = cms.untracked.bool(True),
	duplicateCheckMode = cms.untracked.string("noDuplicateCheck"),
	fileNames = cms.untracked.vstring()
)
source.fileNames.extend([
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_10_1_Q7M.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_11_1_hJB.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_11_2_IoO.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_12_1_TFw.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_13_0_tnB.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_14_0_hYr.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_15_0_ObB.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_15_1_uXG.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_16_0_1S3.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_17_0_pOY.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_18_0_f6X.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_19_0_Zzh.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_1_1_Z3I.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_20_0_R1g.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_21_0_uCs.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_22_0_w7O.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_23_0_Dw6.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_24_0_x7Q.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_25_0_Wq0.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_26_0_wFk.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_27_0_nry.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_28_0_g1L.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_28_1_udt.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_29_0_tzn.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_2_1_N4G.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_30_0_4rS.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_30_1_mx4.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_31_0_YCN.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_32_0_bwm.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_33_0_9QT.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_34_0_tkI.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_35_0_uDj.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_36_0_3OD.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_37_0_4sc.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_38_0_SEe.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_39_0_ZJM.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_39_1_Bel.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_3_1_aMe.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_40_0_h1X.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_41_0_CrV.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_42_0_NVu.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_42_1_Een.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_43_0_9j3.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_44_0_0JU.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_45_0_39W.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_45_1_tC1.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_46_0_AGh.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_47_0_i3T.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_48_0_U4L.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_49_0_eRw.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_4_1_tds.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_4_2_9zC.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_50_0_3Cf.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_51_0_8LP.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_51_1_fur.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_52_0_BZ2.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_53_0_4IF.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_5_1_f4C.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_6_1_X0l.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_6_2_3lp.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_7_1_Ods.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_7_2_7h7.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_8_1_gS6.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/DYJetsToLL_TuneD6T_M-50_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_9_1_5Bi.root'
])