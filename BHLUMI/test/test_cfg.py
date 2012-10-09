import FWCore.ParameterSet.Config as cms


process = cms.Process("TEST")
process.maxEvents = cms.untracked.PSet(input = cms.untracked.int32(1000000))
process.source = cms.Source("EmptySource")

process.load("IOMC.RandomEngine.IOMC_cff")
process.RandomNumberGeneratorService.generator.initialSeed = 8899
#HZHA generator 
process.generator = cms.EDFilter("BHLUMIGeneratorFilter",
  CardsPath = cms.string("GeneratorInterface/BHLUMI/data/lmdcards.dat")
)

#make gen particles and all that stuff
process.load("SimGeneral.HepPDTESSource.pythiapdt_cfi")
process.load("PhysicsTools.HepMCCandAlgos.genParticles_cfi")
process.load("RecoJets.Configuration.GenJetParticles_cff")

process.TFileService = cms.Service("TFileService",
  fileName = cms.string('bhlumi_plots.root')
)
process.ana = cms.EDAnalyzer('LumiAnalyzer',
  src = cms.InputTag("generator")
)

#ourtput module
process.GEN = cms.OutputModule("PoolOutputModule",
	fileName = cms.untracked.string('bhlumi.root')
)

#paths
process.p = cms.Path(process.generator+
                    process.genParticles+
                    process.ana
                     )
#process.outpath = cms.EndPath(process.GEN)
