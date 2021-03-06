## import skeleton process
from PhysicsTools.PatAlgos.patTemplate_cfg import *


# process.load('CMGTools.Common.sources.CMSSW_4_1_3.RelValTTbar_Tauola.GEN_SIM_RECO.START311_V2_PU_E7TeV_AVE_2_BX156_v1.source_cff')
process.load('Lenzip.Susy.sources.WJetsToLNu_TuneD6T_7TeV_madgraph_tauola.Spring11_PU_S1_START311_V1G1_v1.PrunedAOD.source_cff')

process.maxEvents = cms.untracked.PSet( input = cms.untracked.int32(-1) )

process.maxLuminosityBlocks = cms.untracked.PSet( 
    input = cms.untracked.int32(-1)
    )

process.options   = cms.untracked.PSet( wantSummary = cms.untracked.bool(False))
runOnMC = True
runStdPAT = False
pickRelVal = False

#process.load("CMGTools.Common.sources.SingleElectron.Run2011A_PromptReco_v1.AOD.source_cff")

if pickRelVal: 
    from PhysicsTools.PatAlgos.tools.cmsswVersionTools import pickRelValInputFiles

    # process.source.fileNames = cms.untracked.vstring('file:prunedAOD.root')
    process.source.fileNames = cms.untracked.vstring('file:AODNoSim.root')
    # process.source.fileNames = cms.untracked.vstring('file:AOD.root')
    print 'need to define a relval'
    sys.exit(1)
    process.source = cms.Source(
        "PoolSource",
        fileNames = cms.untracked.vstring(
        pickRelValInputFiles( cmsswVersion  = 'CMSSW_4_1_2'
                              #, relVal        = 'RelValLM1_sfts'
                              , relVal = relVal
                              # , globalTag     = 'START311_V2'
                              , globalTag = tag
                              , numberOfFiles = 999
                              )
        )
        )


print process.source.fileNames

process.out.fileName = cms.untracked.string('patTuple_PATandPF2PATandCMG.root' )

# load the PAT config
process.load("PhysicsTools.PatAlgos.patSequences_cff")


# Configure PAT to use PF2PAT instead of AOD sources
# this function will modify the PAT sequences. It is currently 
# not possible to run PF2PAT+PAT and standart PAT at the same time
from PhysicsTools.PatAlgos.tools.pfTools import *

# An empty postfix means that only PF2PAT is run,
# otherwise both standard PAT and PF2PAT are run. In the latter case PF2PAT
# collections have standard names + postfix (e.g. patElectronPFlow)  
postfix = "PFlow"
jetAlgo="AK5"
usePF2PAT(process,runPF2PAT=True, jetAlgo=jetAlgo, runOnMC=runOnMC, postfix=postfix) 
# to run second PF2PAT+PAT with differnt postfix uncomment the following lines
# and add it to path
#postfix2 = "PFlow2"
#jetAlgo2="AK7"
#usePF2PAT(process,runPF2PAT=True, jetAlgo=jetAlgo2, runOnMC=True, postfix=postfix2)

# to use tau-cleaned jet collection uncomment the following:
#useTauCleanedPFJets(process, jetAlgo=jetAlgo, postfix=postfix) 

# to switch default tau to HPS tau uncomment the following:
#adaptPFTaus(process,"hpsPFTau",postfix=postfix)


if runOnMC == False:
    # removing MC matching for standard PAT sequence
    # for the PF2PAT+PAT sequence, it is done in the usePF2PAT function
    removeMCMatchingPF2PAT( process, '' ) 
else:
  #photons are off, but the photonMCmatcher is not for some reason. Remove it manually
  thePATsequence = getattr(process,"patPF2PATSequence"+postfix)
  thePATsequence.remove(process.photonMatchPFlow)
  setattr(process, "patPF2PATSequence"+postfix, thePATsequence)
# Let it run
# process.load('CMGTools.Susy.fullyHadronic_cff')

process.p = cms.Path(
#    process.startupSequence +                      
    getattr(process,"patPF2PATSequence"+postfix) 
#    second PF2PAT
#    + getattr(process,"patPF2PATSequence"+postfix2)
)

# removing standard PAT sequence
# if not postfix=="":
if runStdPAT : 
    process.p += process.patDefaultSequence



# process.load("CMGTools.Common.runInfoAccounting_cfi")
# process.outpath += process.runInfoAccounting



# Add PF2PAT output to the created file
from PhysicsTools.PatAlgos.patEventContent_cff import patEventContentNoCleaning
process.out.outputCommands = cms.untracked.vstring('drop *',
                                                   'keep recoPFCandidates_particleFlow_*_*',
                                                   # Vertex info
                                                   'keep recoVertexs_*_*_*',
                                                   'keep recoBeamSpot_*_*_*',
                                                   # Trigger
                                                   'keep L1GlobalTriggerObjectMapRecord_*_*_*',
                                                   'keep *_TriggerResults_*_*',
                                                   'keep *_hltTriggerSummaryAOD_*_*',
                                                   *patEventContentNoCleaning ) 

from CMGTools.Common.eventContent.runInfoAccounting_cff import runInfoAccounting
process.out.outputCommands += runInfoAccounting

# print process.out.outputCommands

# top projections in PF2PAT:

process.pfNoPileUpPFlow.enable = cms.bool(True) 
process.pfNoMuonPFlow.enable = cms.bool(True) 
process.pfNoElectronPFlow.enable = cms.bool(True)
process.pfNoTauPFlow.enable = cms.bool(True)
process.pfNoJetPFlow.enable = cms.bool(True)

# process.TFileService = cms.Service(
#    "TFileService",
#    fileName = cms.string("histograms.root")
#    )

process.MessageLogger.cerr.FwkReport.reportEvery = 10

# to relax the muon isolation, uncomment the following:
process.pfIsolatedMuonsPFlow.combinedIsolationCut = 0.25
process.pfIsolatedElectronsPFlow.combinedIsolationCut = 0.25


# CMG tuple
process.load('CMGTools.Common.analysis_cff')
process.p += process.analysisSequence

if not runStdPAT:
    process.analysisSequence.remove( process.caloJetSequence )
    process.analysisSequence.remove( process.caloMetSequence )

from CMGTools.Common.eventContent.everything_cff import everything 

process.outcmg = cms.OutputModule(
    "PoolOutputModule",
    fileName = cms.untracked.string('tree_CMG.root'),
    # save only events passing the full path
    SelectEvents   = cms.untracked.PSet( SelectEvents = cms.vstring('p') ),
    # save PAT Layer 1 output; you need a '*' to
    # unpack the list of commands 'patEventContent'
    outputCommands = everything 
    )

process.outpath += process.outcmg

if runOnMC:
    process.load("CMGTools.Common.runInfoAccounting_cfi")
    #process.outpath += process.runInfoAccounting

process.TFileService = cms.Service("TFileService",
                                   fileName = cms.string("histograms_CMG.root"))
