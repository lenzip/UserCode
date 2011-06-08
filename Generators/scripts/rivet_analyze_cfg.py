import FWCore.ParameterSet.Config as cms

process = cms.Process("Rivet")
process.load("FWCore.MessageLogger.MessageLogger_cfi")

process.load("SimGeneral.HepPDTESSource.pythiapdt_cfi")

process.maxEvents = cms.untracked.PSet(
    input = cms.untracked.int32(-1)
)
process.source = cms.Source("PoolSource",
    #fileNames = cms.untracked.vstring('file:/data/lenzip/Generators/integration/CMSSW_4_1_6/work/scales/centralPlusWeights/centralPlusWeights.root')
    fileNames = cms.untracked.vstring('file:/data/lenzip/Generators/integration/CMSSW_4_1_6/work/test/sherpa_7TeV_WToLNu_MENLOPS_HTflat_cff_py_GEN.root')
)

process.dummy = cms.EDAnalyzer("DummyHepMCAnalyzer",
    src = cms.InputTag("generator"),
    dumpHepMC = cms.untracked.bool(True),
    checkPDG = cms.untracked.bool(False)
)

process.load("GeneratorInterface.RivetInterface.rivetAnalyzer_cfi")
process.rivetAnalyzer.AnalysisNames = cms.vstring('MC_LES_HOUCHES_SYSTEMATICS_CMS')

#process.p = cms.Path(process.dummy+process.rivetAnalyzer)
process.p = cms.Path(process.rivetAnalyzer)

