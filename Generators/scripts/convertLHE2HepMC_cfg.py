import FWCore.ParameterSet.Config as cms

process = cms.Process("convertLHE2HepMC")
process.load("FWCore.MessageLogger.MessageLogger_cfi")

process.maxEvents = cms.untracked.PSet(
    input = cms.untracked.int32(-1)
)
process.source = cms.Source("LHESource",
    fileNames = cms.untracked.vstring()
)
process.source.fileNames = ([
        '/store/lhe/3589/7TeV_wz2l2q_run50001_unweighted_events_qcut18_mgPostv2.lhe',
        '/store/lhe/3589/7TeV_wz2l2q_run50002_unweighted_events_qcut18_mgPostv2.lhe',
        '/store/lhe/3589/7TeV_wz2l2q_run50003_unweighted_events_qcut18_mgPostv2.lhe',
        '/store/lhe/3589/7TeV_wz2l2q_run50004_unweighted_events_qcut18_mgPostv2.lhe',
        '/store/lhe/3589/7TeV_wz2l2q_run50005_unweighted_events_qcut18_mgPostv2.lhe',
        '/store/lhe/3589/7TeV_wz2l2q_run50007_unweighted_events_qcut18_mgPostv2.lhe',
        '/store/lhe/3589/7TeV_wz2l2q_run50008_unweighted_events_qcut18_mgPostv2.lhe',
        '/store/lhe/3589/7TeV_wz2l2q_run50009_unweighted_events_qcut18_mgPostv2.lhe',
        '/store/lhe/3589/7TeV_wz2l2q_run50010_unweighted_events_qcut18_mgPostv2.lhe',
        '/store/lhe/3589/7TeV_wz2l2q_run50011_unweighted_events_qcut18_mgPostv2.lhe',
        '/store/lhe/3589/7TeV_wz2l2q_run50012_unweighted_events_qcut18_mgPostv2.lhe',
        '/store/lhe/3589/7TeV_wz2l2q_run50013_unweighted_events_qcut18_mgPostv2.lhe',
        '/store/lhe/3589/7TeV_wz2l2q_run50014_unweighted_events_qcut18_mgPostv2.lhe',
        '/store/lhe/3589/7TeV_wz2l2q_run50016_unweighted_events_qcut18_mgPostv2.lhe',
        '/store/lhe/3589/7TeV_wz2l2q_run50017_unweighted_events_qcut18_mgPostv2.lhe',
        '/store/lhe/3589/7TeV_wz2l2q_run50018_unweighted_events_qcut18_mgPostv2.lhe',
        '/store/lhe/3589/7TeV_wz2l2q_run50019_unweighted_events_qcut18_mgPostv2.lhe',
        '/store/lhe/3589/7TeV_wz2l2q_run50020_unweighted_events_qcut18_mgPostv2.lhe',
        '/store/lhe/3589/7TeV_wz2l2q_run50021_unweighted_events_qcut18_mgPostv2.lhe',
        '/store/lhe/3589/7TeV_wz2l2q_run50022_unweighted_events_qcut18_mgPostv2.lhe',
        '/store/lhe/3589/7TeV_wz2l2q_run50023_unweighted_events_qcut18_mgPostv2.lhe',
        '/store/lhe/3589/7TeV_wz2l2q_run50024_unweighted_events_qcut18_mgPostv2.lhe',
        '/store/lhe/3589/7TeV_wz2l2q_run50025_unweighted_events_qcut18_mgPostv2.lhe',
        '/store/lhe/3589/7TeV_wz2l2q_run50026_unweighted_events_qcut18_mgPostv2.lhe',
        '/store/lhe/3589/7TeV_wz2l2q_run50027_unweighted_events_qcut18_mgPostv2.lhe',
        '/store/lhe/3589/7TeV_wz2l2q_run50028_unweighted_events_qcut18_mgPostv2.lhe'
])

process.load("GeneratorInterface.LHEInterface.lhe2HepMCConverter_cfi")
process.load("PhysicsTools.HepMCCandAlgos.genParticles_cfi")
process.genParticles.src = 'lhe2HepMCConverter'
process.load('SimGeneral.HepPDTESSource.pythiapdt_cfi')
process.load("RecoJets.Configuration.RecoGenJets_cff")
process.load("RecoMET.Configuration.RecoGenMET_cff")
process.load("RecoJets.Configuration.GenJetParticles_cff")
process.load("RecoMET.Configuration.GenMETParticles_cff")

#process.filter = cms.EDFilter("MCSingleParticleFilter",
#  moduleLabel = cms.untracked.string('lhe2HepMCConverter'), 
#  ParticleID  = cms.untracked.vint32(15, -15, 1, -1, 2, -2, 3, -3, 4, -4, 21),
#  MinPt       = cms.untracked.vdouble(0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.),
#  MinEta      = cms.untracked.vdouble(-1000., -1000., -1000., -1000., -1000., -1000., -1000., -1000., -1000., -1000., -1000.),
#  MaxEta      = cms.untracked.vdouble(1000., 1000., 1000., 1000., 1000., 1000., 1000., 1000., 1000., 1000., 1000.),
#  Status      = cms.untracked.vint32(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
#)

#process.out = cms.OutputModule("PoolOutputModule",
#    splitLevel = cms.untracked.int32(0),
#    eventAutoFlushCompressedSize = cms.untracked.int32(5242880),
#    fileName = cms.untracked.string('out.root'),
#    outputCommands = cms.untracked.vstring('drop *',
#                                           'keep *_lhe2HepMCConverter_*_*', 
#                                           'keep *_*gen*_*_*',
#                                           'keep *_*Jet*_*_*'),
#    SelectEvents = cms.untracked.PSet(
#                 SelectEvents = cms.vstring('p')
#    ),
#    dataset = cms.untracked.PSet(
#        filterName = cms.untracked.string('p'),
#        dataTier = cms.untracked.string('GEN')
#    )
#)

#process.load("Validation.EventGenerator.MBUEandQCDValidation_cff")                                                                                           
process.load("Validation.EventGenerator.BasicHepMCValidation_cff")                                                                                            
#process.mbueAndqcdValidation.hepmcCollection = 'lhe2HepMCConverter'                                                                                         
process.basicHepMCValidation.hepmcCollection = 'lhe2HepMCConverter'                                                                                 
process.load('Configuration.EventContent.EventContent_cff')                                                                                                   
process.load('Configuration/StandardSequences/EndOfProcess_cff')                                                                                              
process.load('SimGeneral.HepPDTESSource.pythiapdt_cfi')
ANALYSISEventContent = cms.PSet(
        outputCommands = cms.untracked.vstring('drop *')
        )
ANALYSISEventContent.outputCommands.extend(process.MEtoEDMConverterFEVT.outputCommands)

process.out = cms.OutputModule("PoolOutputModule",
                                   fileName = cms.untracked.string('output.root'),
                                   outputCommands = ANALYSISEventContent.outputCommands
                               )

# DQM Services

from DQMServices.Components.DQMEnvironment_cfi import *

DQMStore = cms.Service("DQMStore")

dqmSaver.convention = 'Offline'
dqmSaver.workflow = '/BasicHepMCValidation/Workflow/GEN'

process.p = cms.Path(process.lhe2HepMCConverter + 
                      process.genParticles +
                      process.genJetParticles + 
                      process.recoGenJets + 
                      process.genMETParticles +
                      process.recoGenMET
                      #+ ~process.filter
                      +process.basicHepMCValidation+process.endOfProcess
                      )
process.e = cms.EndPath(process.out)
