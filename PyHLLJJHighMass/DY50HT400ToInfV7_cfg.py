import CMGTools.RootTools.fwlite.Config as cfg
from HiggsAna.PyHLLJJHighMass.analhjjll_cff import *
hjjllAna.matchgen=False

DY50HT400ToInf = cfg.MCComponent(
    name = 'DY50HT400ToInf',
    files = [
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_0.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_1.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_10.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_11.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_12.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_13.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_14.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_15.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_16.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_17.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_18.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_19.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_2.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_20.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_21.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_22.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_23.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_24.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_25.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_26.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_27.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_28.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_29.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_3.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_30.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_31.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_32.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_33.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_34.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_35.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_36.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_37.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_38.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_39.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_4.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_40.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_41.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_42.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_43.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_44.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_45.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_46.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_47.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_48.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_49.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_5.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_50.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_51.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_52.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_53.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_54.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_55.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_56.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_57.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_58.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_59.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_6.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_60.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_61.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_62.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_63.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_64.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_65.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_66.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_67.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_68.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_69.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_7.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_70.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_71.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_72.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_73.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_74.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_75.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_76.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_77.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_78.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_79.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_8.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_80.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_81.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_82.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_83.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_84.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_85.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_86.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_87.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_88.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_89.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_9.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_90.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_91.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_92.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_93.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_94.root',
'root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_HT-400ToInf_TuneZ2Star_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV7/cmgTuple_newId_vbf_95.root'    
    ],
    
    xSection = 0.00114477 * 11050000, 
    nGenEvents = 6972564, # dummy 
    triggers = [],
    intLumi = 1000,
    effCorrFactor = 1 )

selectedComponents = [DY50HT400ToInf]

DY50HT400ToInf.splitFactor = 1
    
config = cfg.Config( components = selectedComponents,
                     sequence = sequence )
