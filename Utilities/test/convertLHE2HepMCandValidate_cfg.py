import FWCore.ParameterSet.Config as cms

process = cms.Process("convertLHE2HepMC")
process.load("FWCore.MessageLogger.MessageLogger_cfi")
process.MessageLogger.categories=cms.untracked.vstring('FwkJob'
                                                           ,'FwkReport'
                                                           ,'FwkSummary'
                                                           ,'Root_NoDictionary'
                                                           ,'Generator'
                                                           ,'LHEInterface'
                                                           )
    

process.MessageLogger.cerr.INFO = cms.untracked.PSet(limit = cms.untracked.int32(-1))
process.MessageLogger.cerr.Generator = cms.untracked.PSet(limit = cms.untracked.int32(0))
process.MessageLogger.cerr.LHEInterface = cms.untracked.PSet(limit = cms.untracked.int32(0))
process.MessageLogger.cerr.FwkReport.reportEvery = cms.untracked.int32(10000)


process.maxEvents = cms.untracked.PSet(
    input = cms.untracked.int32(300000)
)
process.source = cms.Source("LHESource",
    fileNames = cms.untracked.vstring()
)
process.source.fileNames = ([
'/store/lhe/5006/7TeV_z1jets_run20001to20020_1processes_unweighted_events.lhe',
'/store/lhe/5006/7TeV_z1jets_run20021to20040_1processes_unweighted_events.lhe',
])

process.load("Lenzip.Utilities.lheCOMWeightProducer")
process.load("GeneratorInterface.LHEInterface.lhe2HepMCConverter_cfi")
process.load("PhysicsTools.HepMCCandAlgos.genParticles_cfi")
process.genParticles.src = 'lhe2HepMCConverter'
process.load('SimGeneral.HepPDTESSource.pythiapdt_cfi')
process.load("RecoJets.Configuration.RecoGenJets_cff")
process.load("RecoMET.Configuration.RecoGenMET_cff")
process.load("RecoJets.Configuration.GenJetParticles_cff")
process.load("RecoMET.Configuration.GenMETParticles_cff")

process.load("Validation.EventGenerator.BasicGenValidation_cff")                                                                                            
process.basicGenParticleValidation.hepmcCollection  = 'lhe2HepMCConverter'
process.mbueAndqcdValidation.hepmcCollection        = 'lhe2HepMCConverter'
process.basicHepMCValidation.hepmcCollection        = 'lhe2HepMCConverter'
process.drellYanEleValidation.hepmcCollection       = 'lhe2HepMCConverter'
process.drellYanMuoValidation.hepmcCollection       = 'lhe2HepMCConverter'
process.wMinusEleValidation.hepmcCollection         = 'lhe2HepMCConverter'
process.wPlusEleValidation.hepmcCollection          = 'lhe2HepMCConverter'
process.wMinusMuoValidation.hepmcCollection         = 'lhe2HepMCConverter'
process.wPlusMuoValidation.hepmcCollection          = 'lhe2HepMCConverter'
process.tauValidation.hepmcCollection               = 'lhe2HepMCConverter'
process.duplicationChecker.generatedCollection      = 'lhe2HepMCConverter'
process.duplicationChecker.searchForLHE             = True 

process.validationSequence = cms.Sequence(process.duplicationChecker * 
                                          process.basicHepMCValidation *
                                          process.basicGenParticleValidation * 
                                          process.tauValidation *
                                          process.mbueAndqcd_seq *
                                          process.wEleValidation *
                                          process.wMuoValidation *
                                          process.drellYanEleValidation *
                                          process.drellYanMuoValidation)

process.load('Configuration.EventContent.EventContent_cff')                                                                                                   
process.load('Configuration/StandardSequences/EndOfProcess_cff')                                                                                              
process.load('SimGeneral.HepPDTESSource.pythiapdt_cfi')
ANALYSISEventContent = cms.PSet(
  outputCommands = cms.untracked.vstring('drop *')
)
ANALYSISEventContent.outputCommands.extend(process.MEtoEDMConverterFEVT.outputCommands)

process.out = cms.OutputModule("PoolOutputModule",
  fileName = cms.untracked.string('output7TeV.root'),
  outputCommands = ANALYSISEventContent.outputCommands
)

# DQM Services

from DQMServices.Components.DQMEnvironment_cfi import *

DQMStore = cms.Service("DQMStore")

dqmSaver.convention = 'Offline'
dqmSaver.workflow = '/BasicHepMCValidation/Workflow/GEN'

process.p = cms.Path( process.lhe2HepMCConverter + 
                      process.genParticles +
                      process.genJetParticles + 
                      process.recoGenJets + 
                      process.genMETParticles +
                      process.recoGenMET +
                      process.validationSequence + 
                      process.endOfProcess )
process.e = cms.EndPath(process.out)
