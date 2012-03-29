import FWCore.ParameterSet.Config as cms

process = cms.Process("TEST")
process.maxEvents = cms.untracked.PSet(input = cms.untracked.int32(20))
process.source = cms.Source("EmptySource")

#HZHA generator 
process.generator = cms.EDFilter("HZHAGeneratorFilter",
  CardsPath = cms.string("GeneratorInterface/HZHAInterface/data/hzha04.cards")
)

#make gen particles and all that stuff
process.load("SimGeneral.HepPDTESSource.pythiapdt_cfi")
process.load("PhysicsTools.HepMCCandAlgos.genParticles_cfi")
process.load("RecoJets.Configuration.RecoGenJets_cff")
process.load("RecoMET.Configuration.RecoGenMET_cff")
process.load("RecoJets.Configuration.GenJetParticles_cff")
process.load("RecoMET.Configuration.GenMETParticles_cff")
process.genJetMET = cms.Sequence(process.genJetParticles*process.recoGenJets+process.genMETParticles*process.recoGenMET)

#ourtput module
process.GEN = cms.OutputModule("PoolOutputModule",
	fileName = cms.untracked.string('hzha.root')
)

#paths
process.p = cms.Path(process.generator+
                    process.genParticles+
                    process.genJetMET)
process.outpath = cms.EndPath(process.GEN)
