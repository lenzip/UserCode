import CMGTools.RootTools.fwlite.Config as cfg
from HiggsAna.PyHLLJJ.analhjjll_cff import *
hjjllAna.matchgen=False

DY50 = cfg.MCComponent(
    name = 'DY50',
    files = [
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_0.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_101.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_103.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_108.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_11.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_113.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_114.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_115.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_118.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_12.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_120.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_123.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_124.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_125.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_126.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_13.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_133.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_135.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_136.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_141.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_143.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_145.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_152.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_155.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_156.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_157.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_159.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_16.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_161.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_162.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_163.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_165.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_169.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_17.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_170.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_171.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_178.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_187.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_188.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_189.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_19.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_191.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_192.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_197.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_198.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_199.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_207.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_208.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_21.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_210.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_212.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_214.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_219.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_227.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_228.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_231.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_232.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_233.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_239.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_241.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_250.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_251.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_255.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_256.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_258.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_26.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_260.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_261.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_265.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_267.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_270.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_272.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_273.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_280.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_284.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_286.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_297.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_298.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_299.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_3.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_301.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_309.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_310.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_315.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_317.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_319.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_320.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_322.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_323.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_324.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_328.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_333.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_335.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_336.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_341.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_343.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_347.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_348.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_350.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_352.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_353.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_358.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_361.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_367.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_369.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_370.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_374.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_375.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_379.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_380.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_381.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_388.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_389.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_39.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_393.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_395.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_396.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_397.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_398.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_399.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_404.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_407.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_410.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_415.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_421.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_422.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_425.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_431.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_435.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_437.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_438.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_439.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_442.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_450.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_451.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_461.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_467.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_474.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_476.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_478.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_481.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_483.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_486.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_488.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_493.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_494.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_497.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_5.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_500.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_504.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_505.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_515.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_57.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_59.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_61.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_63.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_68.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_69.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_7.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_70.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_77.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_79.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_8.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_80.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_81.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_86.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_9.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_90.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_95.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_96.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_97.root",
"root://eoscms//eos/cms/store/user/lenzip/CMG/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6/cmgTuple_99.root"
    ],
    
    xSection = 0.00114477 * 11050000, 
    nGenEvents = 6972564, # dummy 
    triggers = [],
    intLumi = 1000,
    effCorrFactor = 1 )

selectedComponents = [DY50]

DY50.splitFactor = 1
    
config = cfg.Config( components = selectedComponents,
                     sequence = sequence )