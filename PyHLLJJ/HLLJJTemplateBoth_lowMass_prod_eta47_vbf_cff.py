import FWCore.ParameterSet.Config as cms
from PhysicsTools.PatAlgos.tools.helpers import *
import copy
from HiggsAna.HLLJJCommon.cmdLine import options
options.parseArguments()
options.selection = 'presel'
#options.maxEvents = 400
#options.output = "cmgTupleVBF_etaMod_noVBFcuts_.root" 
print options.selection 

runOnMC      = True
runOld5XGT = False
runOnFastSim = False

process = cms.Process("CMG")

###########
# Options #
###########
process.maxEvents = cms.untracked.PSet( input = cms.untracked.int32(options.maxEvents))
#process.maxEvents = cms.untracked.PSet( input = cms.untracked.int32(100))

process.load("FWCore.MessageLogger.MessageLogger_cfi")
process.MessageLogger.cerr.FwkReport.reportEvery = 10
process.options   = cms.untracked.PSet( wantSummary = cms.untracked.bool(True),
                                        #SkipEvent = cms.untracked.vstring('ProductNotFound')
                                        )

##GIULIO
process.genSelectorVBF = cms.EDFilter("GenParticleSelector",
  src = cms.InputTag("genParticles"),
  cut = cms.string(' (abs(pdgId) <= 6 ) '+
                   ' && (status==3) ')# +
                   #' && abs(mother.pdgId) <= 6 '+
                   #' && mother.numberOfDaughters()==2 '+
                   #' && ( abs(mother.daughter(0).pdgId)==25 || abs(mother.daughter(1).pdgId)==25) ')
                   #' && (daughter(0).status()==2) '+
                   #' && mother.pdgId() != 23'
                   #' && abs(mother.pdgId) <= 6 '+
                   #' && pt>3')
                   #' && mother.numberOfDaughters()==2 '+
                   #' && ( abs(mother.daughter(0).pdgId)==22 || abs(mother.daughter(0).pdgId)==23 || abs(mother.daughter(0).pdgId)==24 '+
                   #' ||   abs(mother.daughter(1).pdgId)==22 || abs(mother.daughter(1).pdgId)==23 || abs(mother.daughter(1).pdgId)==24)')
)

##/GIULIO
########################################################
## Conditions 
########################################################

process.load("Configuration.StandardSequences.GeometryDB_cff")
process.load("Configuration.StandardSequences.FrontierConditions_GlobalTag_cff")
process.load("Configuration.StandardSequences.MagneticField_38T_cff")

from CMGTools.Common.Tools.getGlobalTag import getGlobalTag

process.GlobalTag.globaltag = getGlobalTag( runOnMC, runOld5XGT )
print 'Global tag       : ', process.GlobalTag.globaltag

###########
# Input   #
###########
#fullname  = "HiggsAna.HLLJJCommon.datasets." + options.infile
####fullname  = "HiggsAna.HLLJJCommon.datasets.summer12_GluGluToHToZZ4lTEST525_cff" 
#print 'Importing dataset from '
#print fullname
#process.load(fullname)
from CMGTools.Production.datasetToSource import *
process.source = datasetToSource(
    'CMS',
    '/VBF_HToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM'
#    '/GluGluToHToZZTo2L2Q_M-125_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM'
#    '/DYJetsToLL_M-10To50filter_8TeV-madgraph/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM'
#    '/DYJetsToLL_M-50_TuneZ2Star_8TeV-madgraph-tarball/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM'    
#    '/TTTo2L2Nu2B_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM'
    )

###################BEGIN CMG SPECIFIC
from CMGTools.Common.Tools.cmsswRelease import isNewerThan

sep_line = '-'*67
print sep_line
print 'CMG PAT-tuplizer, contact Colin before any modification'
print sep_line

print sep_line
print process.source.fileNames
print sep_line 

print 'loading the main CMG sequence'

process.load('CMGTools.Common.PAT.PATCMG_cff')
process.load('CMGTools.RootTools.utils.vertexWeight.vertexWeight_cff')

if runOnMC is False:
    # removing MC stuff
    print 'removing MC stuff, as we are running on Data'

    process.patElectrons.addGenMatch = False
    process.makePatElectrons.remove( process.electronMatch )
    
    process.patMuons.addGenMatch = False
    process.makePatMuons.remove( process.muonMatch )
    
    process.PATCMGSequence.remove( process.PATCMGGenSequence )
    process.PATCMGJetSequence.remove( process.jetMCSequence )
    process.PATCMGJetSequence.remove( process.patJetFlavourId )
    process.patJets.addGenJetMatch = False
    process.patJets.addGenPartonMatch = False

    if isNewerThan('CMSSW_5_2_0'):
        process.PATCMGJetSequenceCHSpruned.remove( process.jetMCSequenceCHSpruned )
        process.patJetsCHSpruned.addGenJetMatch = False
        process.patJetsCHSpruned.addGenPartonMatch = False

    process.PATCMGTauSequence.remove( process.tauGenJets )
    process.PATCMGTauSequence.remove( process.tauGenJetsSelectorAllHadrons )
    process.PATCMGTauSequence.remove( process.tauGenJetMatch )
    process.PATCMGTauSequence.remove( process.tauMatch )
    process.patTaus.addGenJetMatch = False
    process.patTaus.addGenMatch = False

    process.patMETs.addGenMET = False 
    process.patMETsRaw.addGenMET = False 

    # adding L2L3Residual corrections
    process.patJetCorrFactors.levels.append('L2L3Residual')
    if isNewerThan('CMSSW_5_2_0'):
        process.patJetCorrFactorsCHSpruned.levels.append('L2L3Residual')


print 'cloning the jet sequence to build PU chs jets'

from PhysicsTools.PatAlgos.tools.helpers import cloneProcessingSnippet
process.PATCMGJetCHSSequence = cloneProcessingSnippet(process, process.PATCMGJetSequence, 'CHS')
process.PATCMGJetCHSSequence.insert( 0, process.ak5PFJetsCHS )
from CMGTools.Common.Tools.visitorUtils import replaceSrc
replaceSrc( process.PATCMGJetCHSSequence, 'ak5PFJets', 'ak5PFJetsCHS')
replaceSrc( process.PATCMGJetCHSSequence, 'particleFlow', 'pfNoPileUp')
process.patJetCorrFactorsCHS.payload = 'AK5PFchs'
process.selectedPatJetsCHS.cut = 'pt()>10'


########################################################
## Path definition
########################################################
process.PATCMGSequence.remove(process.PATCMGTauSequence)


process.dump = cms.EDAnalyzer('EventContentAnalyzer')

process.load('CMGTools.Common.PAT.addFilterPaths_cff')
process.p = cms.Path(
    process.prePathCounter + 
    process.PATCMGSequence +
    process.PATCMGJetCHSSequence 
    )

process.p += process.postPathCounter

pathsplit = str(process.p).split("+")
for module in pathsplit:
  print module

########################################################
## Below, stuff that you probably don't want to modify
########################################################



## Geometry and Detector Conditions (needed for a few patTuple production steps)

from CMGTools.Common.PAT.patCMGSchedule_cff import getSchedule
process.schedule = getSchedule(process, runOnMC, runOnFastSim)

## MessageLogger
process.load("FWCore.MessageLogger.MessageLogger_cfi")
#process.MessageLogger.cerr.FwkReport.reportEvery = 10

## Options and Output Report
#process.options   = cms.untracked.PSet( wantSummary = cms.untracked.bool(False) )

print sep_line

print 'Fastjet instances (dominating our processing time...):'
from CMGTools.Common.Tools.visitorUtils import SeqVisitor
v = SeqVisitor('FastjetJetProducer')
process.p.visit(v)

print sep_line

print 'starting CMSSW'

if not runOnMC and isNewerThan('CMSSW_5_2_0'):
    process.pfJetMETcorr.jetCorrLabel = cms.string("ak5PFL1FastL2L3Residual")

####################END OF CMG SPECIFIC

###################
# JSON Filtering  #
###################
#only do this for data
if options.mcordata == "DATA" and options.json!="" :
    import PhysicsTools.PythonAnalysis.LumiList as LumiList
    import FWCore.ParameterSet.Types as CfgTypes
    myLumis = LumiList.LumiList(filename = options.json).getCMSSWString().split(',')
    process.source.lumisToProcess = CfgTypes.untracked(CfgTypes.VLuminosityBlockRange())
    process.source.lumisToProcess.extend(myLumis)


#################
# Event filter  #
#################

process.load('HLTrigger.HLTfilters.triggerResultsFilter_cfi')
process.triggerResultsFilter.triggerConditions = cms.vstring('HLT_primaryVertexFilterPath',
                                                             'HLT_noscrapingFilterPath',
                                                             'HLT_hcalLaserEventFilterPath',
                                                             'HLT_HBHENoiseFilterPath',
                                                             #'HLT_totalKinematicsFilterPath' #only for Madgraph MC
                                                             )
process.triggerResultsFilter.l1tResults = ''                                                  

#process.badEventFilter = cms.EDFilter("HLTHighLevel",
#                                     TriggerResultsTag =
#                                      cms.InputTag("TriggerResults"), #,"","CMG"),
#                                      HLTPaths =
#                                      cms.vstring('primaryVertexFilterPath',
#                                                  'noscrapingFilterPath',
#                                                  'hcalLaserEventFilterPath',
#                                                  'HBHENoiseFilterPath',
#                                                  #'totalKinematicsFilterPath' #only for Madgraph MC
#                                                  ),
#                                      eventSetupPathsKey = cms.string(''),
#                                      # how to deal with multiple triggers: True (OR) accept if ANY is true, False
#                                      #(AND) accept if ALL are true
#                                      andOr = cms.bool(False), 
#                                      throw = cms.bool(True)    # throw exception on unknown path names
#                                      ) 

###########
# Output  #
###########
process.out = cms.OutputModule("PoolOutputModule",
                               fileName = cms.untracked.string(options.output),
                               outputCommands = cms.untracked.vstring('drop *',
##                                                                       'keep cmgPFJets_*_*_CMG',
##                                                                       'keep cmgElectron*_*_*_CMG',
##                                                                       'keep cmgMuon*_*_*_CMG',
##                                                                       'keep edmTriggerResults_TriggerResults_*_CMG',
                                                                      'keep double*_*_*_CMG',
##                                                                       'keep recoVertexs*_*_*_CMG',
                                                                      #'keep '
                                                                      )
                               )
#if options.selection != "none":
process.out.SelectEvents = cms.untracked.PSet( SelectEvents = cms.vstring() )

#if options.selection == "full":
#    if options.lepton == "both" or options.lepton == "ele":
#        process.out.SelectEvents.SelectEvents.append("cmgHZZEE")
#        process.out.SelectEvents.SelectEvents.append("cmgHZZEESideband")
#    if options.lepton == "both" or options.lepton == "mu":
#        process.out.SelectEvents.SelectEvents.append("cmgHZZMM")
#        process.out.SelectEvents.SelectEvents.append("cmgHZZMMSideband")
#if options.selection == "presel":
if options.lepton == "both" or options.lepton == "ele":
  process.out.SelectEvents.SelectEvents.append("preselEle")
if options.lepton == "both" or options.lepton == "mu":
  process.out.SelectEvents.SelectEvents.append("preselMu")
    
#print process.out.SelectEvents.SelectEvents

#process.outpath = cms.EndPath(process.out)
#from CMGTools.Common.PAT.patCMGSchedule_cff import getSchedule
#process.schedule = getSchedule(process, runOnMC, runOnFastSim)
#process.schedule.append( process.outpath )

process.TFileService = cms.Service("TFileService",
                                   fileName = cms.string(options.secondaryOutput))
print options.content
for content in options.content:
    
    if content == "all":
        process.out.outputCommands.append( 'keep *' )

    if content == "ele":
        process.out.outputCommands.append( 'keep *_*patElectronsWithTrigger*_*_*' )        
        process.out.outputCommands.append( 'keep *_*selectedPatElectrons*_*_*' ) # we keep the pat::Muon
        process.out.outputCommands.append( 'keep cmgElectron*_*_*_CMG')
        process.out.outputCommands.append( 'drop *_cmgHiggs*_*_CMG')
        process.out.outputCommands.append( 'keep *_cmgHiggsSel*_*_CMG')
        process.out.outputCommands.append( 'drop *_cmgDiElectronDiJet*_*_CMG')
    if content == "mu":
        process.out.outputCommands.append( 'keep *_*patMuonsWithTrigger*_*_*' ) 
        process.out.outputCommands.append( 'keep *_*selectedPatMuons*_*_*' ) 
        process.out.outputCommands.append('keep cmgMuon*_*_*_CMG')
        process.out.outputCommands.append( 'drop *_cmgHiggs*_*_CMG')
        process.out.outputCommands.append( 'keep *_cmgHiggsSel*_*_CMG')
        process.out.outputCommands.append( 'drop *_cmgDiMuonDiJet*_*_CMG')
    if content == "gen":
        process.out.outputCommands.append( 'keep *_*genParticles*_*_*' ) 
        process.out.outputCommands.append( 'keep *_ak5GenJets_*_*' )
        process.out.outputCommands.append( 'keep GenEventInfoProduct*_*_*_*' )
        process.out.outputCommands.append( 'keep *_genSelector*_*_*' )


    if content == "jets":
       ##  process.out.outputCommands.append( 'keep *_custom*Jets*_*_*' )
##         process.out.outputCommands.append( 'keep patJets_selected*_*_*' ) 
##         process.out.outputCommands.append( 'keep *_*_rho_*' )
##         process.out.outputCommands.append( 'keep *_patMETs*_*_*' )
        process.out.outputCommands.append( 'keep cmgPFJets_*_*_CMG')
        process.out.outputCommands.append( 'keep cmgBaseMETs_*_*_CMG')
        process.out.outputCommands.append( 'keep cmgMET*_*_*_CMG')
        process.out.outputCommands.append( 'keep *_addPileupInfo_*_*')
        process.out.outputCommands.append( 'keep *_VBF*_*_*')
        

    if content == "trigger":
        process.out.outputCommands.append( 'keep *_TriggerResults_*_*' )
        process.out.outputCommands.append( 'keep *_*PrimaryVertices_*_*' )

################
# Ele Sequence #
################
process.load('CMGTools.Common.selections.cutSummaryMuon_cfi')
process.load('HiggsAna.HEEJJ.electron_cff') 
process.load('HiggsAna.HEEJJ.diElectron_cff')
process.zeesummary = process.cutSummaryMuon.clone(inputCollection = cms.InputTag("cmgDiElectron2L2Q"))
process.load('HiggsAna.HEEJJ.skims.selEventsElectron_cfi')
process.load('HiggsAna.HEEJJ.skims.selEventsZ_cff')
process.load('HiggsAna.HEEJJ.selections.heejjElectronId_cfi')
process.cmgElectron2L2Q.cuts.loosemvaid = cms.string('sourcePtr().electronID(\"mvaNonTrigV0\") > 0.7')
process.electronPreselNoIso.cut = cms.string("getSelection(\"cuts_kinematics\") "
                                             +"&& getSelection(\"cuts_loosemvaid\") "
#                                             +"&& getSelection(\"cuts_\") "
                                             +"&& getSelection(\"cuts_HLTPatch\")") 

#process.load('HiggsAna.HLLJJCommon.histograms.recoLeptonHistos_cff')
#process.recoEleHistos = process.recoLeptonHistos.clone(src="electronPresel")
#process.cmgDiElectron2L2Q.cfg.leg1Collection = cms.InputTag("cmgElectron2L2Q") 
#process.cmgDiElectron2L2Q.cfg.leg2Collection = cms.InputTag("cmgElectron2L2Q")
process.selectedElectronCandFilter.src = cms.InputTag("cmgElectron2L2Q")

process.analysisSequenceElectrons = cms.Sequence(
    process.eleSequence2L2Q +
    process.selectedElectronSequence2L2Q +
    ##process.recoEleHistosHLLJJ +
    process.diElectronSequence2L2Q +
    process.selectedZSequence
    )


#test
#process.test = cms.Path(process.analysisSequenceElectrons)
#process.schedule = cms.Schedule(process.p)
#process.schedule.append(process.test)
#process.schedule.append(process.outpath)

#############
# MC Hack   #
#############
if options.mcordata == "DATA":
    process.genParticles = cms.EDProducer("DummyGenProducer")
    process.eleSequence2L2Q.insert(0,process.genParticles)
else:
    process.load("HiggsAna.HLLJJCommon.gen_cff")
    process.eleSequence2L2Q.insert(0,process.genHistoSequence)

##############
# PU weights #
##############
process.load('HiggsAna.HLLJJCommon.PUweights_cff')
process.eleSequence2L2Q.insert(0,process.PUseq)

##############
# Rho 2.5    #
##############
# not needed in 2012, rho for iso calculated in PAT-tuple
#process.load('RecoJets.Configuration.RecoPFJets_cff')
#process.kt6PFJetsForIsolation = process.kt6PFJets.clone(Rho_EtaMax = 2.5)
#process.RhoPath=cms.Path(process.kt6PFJetsForIsolation)

################
# Mu  Sequence #
################
process.load('CMGTools.Common.selections.cutSummaryMuon_cfi')  #needed?
process.load('HiggsAna.HMMJJ.muon_cff')
process.load('HiggsAna.HMMJJ.diMuon_cff')
process.zmumusummary = process.cutSummaryMuon.clone(inputCollection = cms.InputTag("cmgDiMuon"))
process.load('HiggsAna.HMMJJ.skims.selEventsMuon_cff')
process.load('HiggsAna.HMMJJ.skims.selEventsZ_cff')
process.load('HiggsAna.HMMJJ.higgs_cff')
#process.load('HiggsAna.HLLJJCommon.histograms.recoLeptonHistos_cff')
#process.recoMuHistos = process.recoLeptonHistos.clone(src = "muonPresel")
#process.cmgDiMuon2L2Q.cfg.leg1Collection = cms.InputTag("cmgMuon2L2Q")
#process.cmgDiMuon2L2Q.cfg.leg2Collection = cms.InputTag("cmgMuon2L2Q")
process.selectedMuonCandFilter.src = cms.InputTag("cmgMuon2L2Q")

process.muonSequence2L2Q.insert(0,process.PUseq)

process.analysisSequenceMuons = cms.Sequence(
    process.muonSequence2L2Q
    + process.selectedMuonSequence2L2Q
#    + process.recoMuHistos
    + process.diMuonSequence2L2Q
    + process.selectedZSequence
)

################
# Jet Sequence #
################
process.load('HiggsAna.HLLJJCommon.jet_cff')
process.load('HiggsAna.HLLJJCommon.factories.cmgDiJet_cfi')
process.load('HiggsAna.HLLJJCommon.factories.cmgDiJetKinFit_cfi')
process.load('HiggsAna.HLLJJCommon.skims.selEventsPFJet_cff')
process.load('HiggsAna.HLLJJCommon.skims.selEventsZjj_cff')
#process.selectedJetCandFilter.src = cms.InputTag("cmgJet2L2Q")
#process.ZjjCand.cut.jetKinematics.eta = cms.string('abs(eta()()) < 4.7')  
#process.cmgDiJet2L2Q.cfg.leg1Collection = cms.InputTag("cmgJet2L2Q")
#process.cmgDiJet2L2Q.cfg.leg2Collection = cms.InputTag("cmgJet2L2Q")
#extend the range where you define jets to build Higgs candidates up to 4.7 
process.cmgJetRaw.cuts.jetKinematics.eta = 'abs(eta()) < 4.7' 

genVBF = cms.PSet( src = cms.InputTag("genSelectorVBF"),
    preselection        = cms.string(""),
    deltaR              = cms.double(0.5),
    checkRecoComponents = cms.bool(False), # don't check if they share some AOD object ref
    pairCut             = cms.string(""),
    algorithm = cms.string('byDeltaR'),
    requireNoOverlaps = cms.bool(False), # overlaps don't cause the electron to be discared
  )

process.cmgJetR2.checkOverlaps.genVBF =  genVBF
process.cmgJetR2.checkOverlaps.genParton.algorithm = cms.string('byDeltaR')
#process.cmgJet2L2Q.cuts =cms.PSet( genPartonGiulio = cms.string('sourcePtr().get().hasOverlaps(\'genParton\')'), 
#                                 genVBF = cms.string('sourcePtr().get().hasOverlaps(\'genVBF\')') )
process.VBFPairsAll.cuts.genVBF = cms.PSet( genVBF = cms.string('leg1.sourcePtr().get().hasOverlaps(\'genVBF\') && leg2.sourcePtr().get().hasOverlaps(\'genVBF\')'))

print process.cmgJetR2.checkOverlaps
process.VBFPairsAll.cuts.vbf.deta="abs(leg1.eta - leg2.eta) > 1.5"
process.VBFPairsAll.cuts.vbf.mass="mass > 20"

process.analysisSequenceJets = cms.Sequence(
    process.genSelectorVBF +
    process.jetSequence +
    process.selectedJetSequence +
    process.diJetSequence +
    process.selectedZjjSequence #+
    #process.cmgDiJetKinFit
    )

########################################
# Higgs Sequences (preselection level) #
########################################
process.load('HiggsAna.HEEJJ.higgs_cff')
cloneProcessingSnippet(process,process.higgsSequence, "Ele")
process.load('HiggsAna.HMMJJ.higgs_cff')
cloneProcessingSnippet(process,process.higgsSequence, "Mu")


process.analysisSequenceHZZEE = cms.Sequence(
    process.analysisSequenceElectrons +
#    process.analysisSequenceMuons +
    process.analysisSequenceJets +
    process.higgsSequenceEle
    )

process.analysisSequenceHZZMM = cms.Sequence(
#    process.analysisSequenceElectrons +
    process.analysisSequenceMuons +
    process.analysisSequenceJets +
    process.higgsSequenceMu
    )

process.badEventFilters = cms.Sequence(process.primaryVertexFilter+
                                       process.noscraping+
                                       process.hcalLaserEventFilter+
                                       process.HBHENoiseFilter)

#process.schedule = cms.Schedule(process.p)
#preselections need to have their own paths only if we want to select all events passing up to preselection level
##if options.selection == "presel":
##    process.preselEle = cms.Path(process.analysisSequenceHZZEE)
##    process.preselMu = cms.Path(process.analysisSequenceHZZMM)
##    process.schedule.append(process.preselEle)
##    process.schedule.append(process.preselMu)
if options.selection == "presel":
  #muons
  process.cmgDiMuonSel2L2Q.filter=True 
  process.ZmmCand.filter=True
  process.selectedHiggsCandFilter.minNumber=1
  process.selectedZmmCandFilter.minNumber=1
  process.selectedMuonCandFilter.minNumber=2
  #electrons
  process.cmgDiElectronSel2L2Q.filter=True
  process.ZeeCand.filter=True
  process.selectedElectronCandFilter.minNumber = 2
  process.selectedHiggsCandFilter.minNumber = 1
  process.selectedZeeCandFilter.minNumber = 1
  #jets
  process.selectedJetCandFilter.src = cms.InputTag("jetIDJet")
  process.selectedJetCandFilter.minNumber=2
  process.selectedJetCandFilter.filter = False
  process.jetCountFilter.minNumber=2
  process.selectedZjjCandFilter.minNumber=1
  #higgs candidates
  #process.cmgDiElectronDiJetHiggsEle.cuts.vbfoverlap = process.diElectronDiJetHiggsFactory.overlapcut
  #uncomment the 2 lines below to require at least 1 VBF candidate (2 lep, 4 jets, no overlap) 
  process.cmgHiggsSelEle.cut = '!getSelection("cuts_overlap") && !vbfptr.isNull()'
  process.cmgHiggsSelMu.cut  = '!getSelection("cuts_overlap") && !vbfptr.isNull()'
  #minimum number of candidates
  process.selectedHiggsCandFilterEle.minNumber = 1
  process.selectedHiggsCandFilterMu.minNumber = 1
    

process.weights = cms.Path(process.vertexWeightSequence)
process.preselEle = cms.Path(process.badEventFilters+process.analysisSequenceHZZEE)
process.preselMu = cms.Path(process.badEventFilters+process.analysisSequenceHZZMM)
process.schedule.append(process.weights)
process.schedule.append(process.preselEle)
process.schedule.append(process.preselMu)


process.OutPath = cms.EndPath(process.triggerResultsFilter*process.out)
process.schedule.append(process.OutPath)    

##############
## Low mass ##
##############

# gen level
process.genHCompositZll.cuts.zmumu.zmumu.leg1_kinematics.pt = cms.string('(leg1().pt() > 10 && leg2().pt() > 20)||(leg1().pt() > 20 && leg2().pt() > 10)')
process.genHCompositZll.cuts.zmumu.zmumu.mass = cms.string('mass() >= 12 ') #&& mass() <= 75')
process.genHCompositZll.cuts.zee.zee.pt = cms.string('(leg1().pt() > 5 && leg2().pt() > 10)||(leg1().pt() > 10 && leg2().pt() > 5)')
process.genHCompositZll.cuts.zee.zee.mass = cms.string('mass() >= 12 ') #&& mass() <= 75')
process.genHCompositeLeptons.cuts.eKinematics.pt = cms.string('pt() > 5')
process.genHCompositeLeptons.cuts.muKinematics.pt = cms.string('pt() > 5')

# reco level
process.cmgElectron2L2Q.cuts.kinematics.pt = cms.string('pt() > 10')
process.cmgDiElectron2L2Q.cuts.zee_kinematics.mass = cms.string('mass() >= 12')
process.cmgDiElectron2L2Q.cuts.zee_kinematics.pt = cms.string('(leg1().pt() > 10 && leg2().pt() > 10)')

#||(leg1().pt() > 10 && leg2().pt() > 5)')
process.cmgMuon2L2Q.cuts.kinematics.pt = cms.string('pt() > 10')
process.cmgDiMuon2L2Q.cuts.zmumu.mass = cms.string('mass() >= 12 ')
process.cmgDiMuon2L2Q.cuts.zmumu.leg1_kinematics = cms.string('(leg1().pt() > 10 && leg2().pt() > 10)')
#||(leg1().pt() > 10 && leg2().pt() > 5)')

process.cmgDiJet2L2Q.cuts.zjj.mass = ("mass>20")
process.cmgDiElectronSel2L2Q.cut = 'getSelection("cuts_zee_kinematics")'
#id for preselected electrons
process.mvaNoTrigEleId.mvaTrigId = cms.string('sourcePtr().electronID(\"mvaNonTrigV0\")>0.7' )
#process.cmgElectron2L2Q.cuts.loosemvaid.mvaTrigId= cms.string('sourcePtr().electronID(\"mvaNonTrigV0\") > 0.7')
process.HLTPatch.endcap = cms.string('abs(sourcePtr().get().superCluster().get().eta()) > 1.4442 || (hadronicOverEm()<0.10 && SigmaIEtaIEta()<0.011 && abs(deltaPhiSuperClusterTrackAtVtx()) < 0.15 && abs(deltaEtaSuperClusterTrackAtVtx())<0.01)')
process.HLTPatch.barrel = cms.string('abs(sourcePtr().get().superCluster().get().eta()) < 1.566  || (hadronicOverEm()<0.075 && SigmaIEtaIEta()<0.031 && abs(deltaPhiSuperClusterTrackAtVtx()) < 0.10 && abs(deltaEtaSuperClusterTrackAtVtx())<0.01)')
#isolation for preselected electrons
process.electronPresel.relisocut = cms.double(0.4)
process.electronPresel.EAvals = cms.vdouble(0.13, 0.14, 0.07, 0.09, 0.11, 0.11, 0.14)

#id for preselected muons
process.muonPresel.relisocut = cms.double(0.2)
process.muonPresel.EAvals = cms.vdouble(0.13, 0.14, 0.07, 0.09, 0.11, 0.11, 0.14)

process.eKinematics.pt = cms.string('pt() > 10')
process.zee_kinematics.pt = cms.string('(leg1().pt() > 10 && leg2().pt() > 10)')
#||(leg1().pt() > 10 && leg2().pt() > 5)')
process.zee_kinematics.mass = cms.string('mass() >= 12 ')
process.zee.leg1_kinematics.pt = cms.string('pt() > 10')

process.muKinematics.pt = cms.string('pt() > 10')
process.zmumu.leg1_kinematics.pt = cms.string('(leg1().pt() > 10 && leg2().pt() > 10)')
#||(leg1().pt() > 10 && leg2().pt() > 5)')

#Zll candidates selections
process.ZeeCand.cut = cms.string( "getSelection(\"cuts_zee_kinematics\") && getSelection(\"cuts_charge\")" )
process.zee.mass = cms.string('mass() >= 12 ')
process.zmumu.mass = cms.string('mass() >= 12 ')

#Higgs candidates selections
process.cmgDiElectronDiJetEle.cuts.kinematics.mass = cms.string('mass >= 50')
#process.cmgDiMuonDiJetEle.cuts.kinematics.mass = cms.string('mass >= 50')

#jet cuts 
#process.jetKinematics.eta = cms.string('abs(eta()) < 4.7')

########################################
# Final Selection Sequence             #
########################################
###
####default is electrons
###process.load("HiggsAna.HLLJJCommon.FinalSelection_cff")
###cloneProcessingSnippet(process,process.cmgSeq, "Ele")
###cloneProcessingSnippet(process,process.cmg0Seqtag, "Ele")
###cloneProcessingSnippet(process,process.cmg1Seqtag, "Ele")
###cloneProcessingSnippet(process,process.cmg2Seqtag, "Ele")
###    
####muons need filter types + inputs adjusted
###cloneProcessingSnippet(process,process.cmgSeq, "Mu")
###massSearchReplaceParam(process.cmgSeqMu,"_TypedParameterizable__type","CmgDiElectronDiJetHiggsSelector","CmgDiMuonDiJetHiggsSelector")
###massSearchReplaceParam(process.cmgSeqMu,"_TypedParameterizable__type","DiElectronDiJetHiggsLDProducer","DiMuonDiJetHiggsLDProducer")
###massSearchReplaceParam(process.cmgSeqMu,"_TypedParameterizable__type","DiElectronDiJetHiggsQGLDSelector","DiMuonDiJetHiggsQGLDSelector")
###massSearchReplaceParam(process.cmgSeqMu,"_TypedParameterizable__type","CmgDiElectronDiJetHiggsMerger","CmgDiMuonDiJetHiggsMerger")
###massSearchReplaceParam(process.cmgSeqMu,"_TypedParameterizable__type","DiElectronDiJetHiggsBestCandidateSelector","DiMuonDiJetHiggsBestCandidateSelector")
###massSearchReplaceParam(process.cmgSeqMu,"_TypedParameterizable__type","DiElectronDiJetHiggsTagger","DiMuonDiJetHiggsTagger")
###massSearchReplaceAnyInputTag(process.cmgSeqMu,cms.InputTag("cmgHiggsSelKinFitEle"),cms.InputTag("cmgHiggsSelKinFitMu"))
###massSearchReplaceAnyInputTag(process.cmgSeqMu,cms.InputTag("cmgHiggsSelEle"),cms.InputTag("cmgHiggsSelMu"))
###cloneProcessingSnippet(process,process.cmg0Seqtag, "Mu")
###cloneProcessingSnippet(process,process.cmg1Seqtag, "Mu")
###cloneProcessingSnippet(process,process.cmg2Seqtag, "Mu")
###massSearchReplaceAnyInputTag(process.cmg0SeqtagMu,cms.InputTag("BestSelectorKinFitEle"),cms.InputTag("BestSelectorKinFitMu"),verbose=False,moduleLabelOnly=True)
###massSearchReplaceAnyInputTag(process.cmg1SeqtagMu,cms.InputTag("BestSelectorKinFitEle"),cms.InputTag("BestSelectorKinFitMu"),verbose=False,moduleLabelOnly=True)
###massSearchReplaceAnyInputTag(process.cmg2SeqtagMu,cms.InputTag("BestSelectorKinFitEle"),cms.InputTag("BestSelectorKinFitMu"),verbose=False,moduleLabelOnly=True)
###
###
####collect adjusted sequences into paths
###if options.lepton == "both" or options.lepton == "ele":
###    process.cmgHZZEE = cms.Path(process.badEventFilter+process.analysisSequenceHZZEE     + process.cmgSeqEle )
###    process.cmgHZZEE0Tag = cms.Path(process.badEventFilter+process.analysisSequenceHZZEE + process.cmgSeqEle + process.cmg0SeqtagEle)
###    process.cmgHZZEE1Tag = cms.Path(process.badEventFilter+process.analysisSequenceHZZEE + process.cmgSeqEle + process.cmg1SeqtagEle)
###    process.cmgHZZEE2Tag = cms.Path(process.badEventFilter+process.analysisSequenceHZZEE + process.cmgSeqEle + process.cmg2SeqtagEle)
###
###if options.lepton == "both" or options.lepton == "mu":
###    process.cmgHZZMM = cms.Path(process.badEventFilter+process.analysisSequenceHZZMM  + process.cmgSeqMu  )
###    process.cmgHZZMM0Tag = cms.Path(process.badEventFilter+process.analysisSequenceHZZMM + process.cmgSeqMu + process.cmg0SeqtagMu)
###    process.cmgHZZMM1Tag = cms.Path(process.badEventFilter+process.analysisSequenceHZZMM + process.cmgSeqMu + process.cmg1SeqtagMu)
###    process.cmgHZZMM2Tag = cms.Path(process.badEventFilter+process.analysisSequenceHZZMM + process.cmgSeqMu + process.cmg2SeqtagMu)
###
###
###
###########################################
#### Sideband Sequence                    #
###########################################
###
####default is electrons
###process.load("HiggsAna.HLLJJCommon.sideband_cff")
###cloneProcessingSnippet(process,process.sidebandSequence, "Ele")
###
####muons need filter types + inputs adjusted
###cloneProcessingSnippet(process,process.sidebandSequence, "Mu")
###massSearchReplaceParam(process.sidebandSequenceMu,"_TypedParameterizable__type","CmgDiElectronDiJetHiggsSelector","CmgDiMuonDiJetHiggsSelector")
###massSearchReplaceParam(process.sidebandSequenceMu,"_TypedParameterizable__type","DiElectronDiJetHiggsBestCandidateSelector","DiMuonDiJetHiggsBestCandidateSelector")
###massSearchReplaceAnyInputTag(process.sidebandSequenceMu,cms.InputTag("BestSelectorKinFitEle","primary"),cms.InputTag("BestSelectorKinFitMu","primary"))
###massSearchReplaceAnyInputTag(process.sidebandSequenceMu,cms.InputTag("VBFTaggerEle"),cms.InputTag("VBFTaggerMu"))
###massSearchReplaceAnyInputTag(process.sidebandSequenceMu,cms.InputTag("VBFTaggerSecondaryEle"),cms.InputTag("VBFTaggerSecondaryMu"))
###
####collect adjusted sequences into paths ---> final paths are defined here !!!!
###if options.lepton == "both" or options.lepton == "ele":
###    process.cmgHZZEESideband = cms.Path(process.badEventFilter+process.analysisSequenceHZZEE + process.cmgSeqEle + process.sidebandSequenceEle)
###    process.cmgHZZEESideband.replace(process.FinalFilterEle,~process.FinalFilterEle) # invert signal band selection
###    process.schedule.append(process.cmgHZZEESideband)
###
###if options.lepton == "both" or options.lepton == "mu":
###    process.cmgHZZMMSideband = cms.Path(process.badEventFilter+process.analysisSequenceHZZMM + process.cmgSeqMu + process.sidebandSequenceMu)
###    process.cmgHZZMMSideband.replace(process.FinalFilterMu,~process.FinalFilterMu) # invert signal band selection
###    process.schedule.append(process.cmgHZZMMSideband)
###
###
###
###########################################
#### Input switch                         #
###########################################
###if options.runinspain  :
###    process.cmgElectron.cuts.wp95c.iso='test_bit(sourcePtr().electronID(\"eidVBTFRel95\"),1)'
###    process.cmgElectron.cuts.wp95c.conversionVeto='test_bit(sourcePtr().electronID(\"eidVBTFRel95\"),2)'
###    
###    #remove duplicate jet-ide sequence
###    process.jetSequence.remove(process.puJetIdSequence)
###    process.jetSequence.remove(process.qglikelihood)
###
###    # replace input for MVA Id remove MC matching
###    process.cmgJetRaw.cfg.PFJetFactory.puMvas=cms.VInputTag()
###    process.cmgJetRaw.cfg.PFJetFactory.puIds=cms.VInputTag()
###
###    for seqname in process.sequences_():
###        #print seqname
###        #print process.sequences_()[seqname]
###        # replace input electrons 
###        massSearchReplaceAnyInputTag( process.sequences_()[seqname] , cms.InputTag("patElectronsWithTrigger"),cms.InputTag("selectedPatElectronsAK5"))
###        #replace muons
###        massSearchReplaceAnyInputTag( process.sequences_()[seqname] , cms.InputTag("patMuonsWithTrigger"),cms.InputTag("selectedPatMuonsAK5"))
###        #replace jets
###        massSearchReplaceAnyInputTag( process.sequences_()[seqname] , cms.InputTag("selectedPatJets"),cms.InputTag("customPFJetsNoPUSub"))
###        #replace MET
###        massSearchReplaceAnyInputTag( process.sequences_()[seqname] , cms.InputTag("patMETsRaw"),cms.InputTag("patMETsAK5"))
