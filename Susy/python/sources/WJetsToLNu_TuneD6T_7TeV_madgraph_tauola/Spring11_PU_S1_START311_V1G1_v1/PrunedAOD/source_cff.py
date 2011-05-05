
import FWCore.ParameterSet.Config as cms

source = cms.Source(
	"PoolSource",

	noEventSort = cms.untracked.bool(True),
	duplicateCheckMode = cms.untracked.string("noDuplicateCheck"),
	fileNames = cms.untracked.vstring()
)
source.fileNames.extend([
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_100_1_TUN.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_101_1_rCH.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_102_1_4BM.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_103_0_hIM.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_104_0_8Rl.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_105_0_GAX.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_108_0_Aho.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_109_0_bQv.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_10_1_0lJ.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_110_0_UP3.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_111_0_z5v.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_112_0_s20.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_113_0_NOk.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_114_0_Acc.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_115_0_pPp.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_117_0_yhP.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_11_1_ElD.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_120_0_KHD.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_121_0_zQ0.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_122_0_yMu.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_123_0_6oy.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_124_0_v3v.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_126_0_BXj.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_127_0_iEh.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_129_0_EV8.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_12_1_ifz.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_131_0_lhE.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_132_0_dfu.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_133_0_Zup.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_134_0_XDb.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_135_0_NVo.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_136_0_DoV.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_138_0_at9.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_139_0_vwn.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_13_1_F0f.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_140_0_8CB.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_141_0_5YS.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_142_0_DL9.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_143_0_1fg.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_144_0_5Bz.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_145_0_AXQ.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_147_0_oCh.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_148_0_IdO.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_149_0_ZYI.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_14_1_HUg.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_151_0_hFh.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_153_0_TDb.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_154_0_Q3X.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_155_0_CTQ.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_15_1_VhU.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_162_0_xzI.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_163_0_Mpg.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_164_0_ddn.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_165_0_MDy.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_166_0_SOm.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_167_0_BCr.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_168_0_Gki.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_169_0_SPf.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_16_1_ZTQ.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_170_0_QwM.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_171_0_EJe.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_172_0_vud.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_173_0_vNi.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_175_0_J08.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_176_0_Qxr.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_178_0_6cj.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_179_0_V5G.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_17_1_wTe.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_180_0_CKf.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_181_0_EzV.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_182_0_2co.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_183_0_iKi.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_185_0_IgW.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_186_0_KVS.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_188_0_3Jq.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_189_0_wOu.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_190_0_4Ol.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_191_0_GFA.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_192_0_IJZ.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_193_0_ea8.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_194_1_4Xb.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_195_1_pZB.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_196_1_q3l.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_197_1_kBt.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_198_1_Exq.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_199_1_QOV.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_19_1_zfo.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_1_1_O6x.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_200_1_YYm.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_201_1_SDK.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_202_1_mtz.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_204_1_fBj.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_205_1_4QN.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_206_1_Tk3.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_207_1_B0h.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_208_1_wid.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_20_1_bnk.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_210_1_MRx.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_211_1_Cab.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_212_1_pcF.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_213_1_ldy.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_214_1_V72.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_215_1_2Ss.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_216_2_85Z.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_217_1_bla.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_218_1_7jk.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_219_1_cN9.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_220_1_7Zo.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_221_1_023.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_222_1_qja.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_223_1_ar9.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_225_1_l6H.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_227_1_Xh7.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_228_1_PR3.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_229_1_Lud.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_230_1_vO8.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_231_1_K6d.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_232_1_7jK.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_233_1_uHl.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_234_1_uiC.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_235_1_F1t.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_236_1_wXz.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_237_1_uxL.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_238_1_158.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_239_1_7OV.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_23_1_NY0.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_240_1_skV.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_241_1_66O.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_242_1_xvk.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_243_1_egb.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_244_1_BkP.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_245_1_VLL.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_246_1_dXL.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_247_1_zYn.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_248_1_TO4.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_249_1_WGm.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_24_1_i2f.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_251_1_tXd.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_252_1_FjC.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_255_1_jB1.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_256_1_1yM.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_257_1_Edg.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_258_1_GrK.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_259_1_bDV.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_25_1_NY4.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_260_1_c6G.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_261_1_p92.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_262_1_khR.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_263_1_Z6v.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_264_1_7Gp.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_265_1_oGt.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_266_1_G5j.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_268_1_rXt.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_26_1_aoi.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_270_1_Swr.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_271_1_MAJ.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_272_1_iD7.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_274_1_B46.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_276_1_e6O.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_277_1_y3s.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_279_1_kLy.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_27_1_R5f.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_280_1_6V3.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_281_1_qLg.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_284_1_JEw.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_287_1_GEs.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_289_1_Oy9.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_28_1_uMw.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_292_1_K4J.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_293_1_GDv.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_294_1_ULs.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_295_1_7nm.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_29_1_tSJ.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_2_1_FlC.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_31_1_92G.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_32_1_QP9.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_33_1_TUi.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_35_1_GXi.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_36_1_ow3.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_37_1_vXt.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_38_1_JA9.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_39_1_CrT.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_3_1_sDK.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_41_1_s7m.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_42_1_84K.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_44_1_53x.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_45_1_yQ4.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_46_1_BL7.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_47_1_G0c.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_48_1_WLn.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_49_1_tA2.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_4_1_NIC.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_51_1_pub.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_53_1_5i4.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_54_1_lnA.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_56_1_0rP.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_57_1_56v.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_58_1_9RC.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_59_1_nwr.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_5_1_I9Q.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_60_1_2A8.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_62_1_4XB.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_63_1_Hyz.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_64_1_FQ6.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_65_1_mKM.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_66_1_FQs.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_68_1_YE6.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_69_1_3PL.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_70_1_w3m.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_71_1_FlD.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_72_1_6xj.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_74_1_9eq.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_75_1_mxs.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_76_1_dzH.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_77_1_Ykf.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_78_1_tTn.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_79_1_vyH.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_80_1_1lG.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_81_1_0kI.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_82_1_0mO.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_83_1_ATo.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_84_1_hf1.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_85_1_NWY.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_86_1_blT.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_87_1_LiL.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_88_1_I73.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_8_1_p6z.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_91_1_Jk8.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_92_1_te5.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_93_1_mil.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_94_1_a74.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_95_1_Tl3.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_96_1_nvI.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_97_1_KYx.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_98_1_lLj.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_99_1_QCQ.root',
'/store/cmst3/user/lenzip/CMG/SusyLP/WJetsToLNu_TuneD6T_7TeV-madgraph-tauola/Spring11-PU_S1_START311_V1G1-v1/AODSIM/PrunedAOD/prunedAOD_9_1_jpK.root'
])
