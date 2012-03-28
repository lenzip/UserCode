import FWCore.ParameterSet.Config as cms

process = cms.Process("TEST")
process.maxEvents = cms.untracked.PSet(input = cms.untracked.int32(20))
process.source = cms.Source("EmptySource")

process.generator = cms.EDFilter("HZHAGeneratorFilter",
  CardsPath = cms.string("GeneratorInterface/HZHAInterface/data/hzha04.cards")
)

process.GEN = cms.OutputModule("PoolOutputModule",
	fileName = cms.untracked.string('hzha.root')
)
process.p = cms.Path(process.generator)
process.outpath = cms.EndPath(process.GEN)
