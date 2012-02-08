import FWCore.ParameterSet.Config as cms

process = cms.Process("ANA")

process.load("FWCore.MessageService.MessageLogger_cfi")
process.MessageLogger.categories=cms.untracked.vstring('FwkJob'
                                                           ,'FwkReport'
                                                           ,'FwkSummary'
                                                           ,'Root_NoDictionary'
                                                           ,'Generator'
                                                           ,'LHEInterface'
                                                           )
    

process.MessageLogger.cerr.INFO = cms.untracked.PSet(limit = cms.untracked.int32(-1))
process.MessageLogger.cerr.Generator = cms.untracked.PSet(limit = cms.untracked.int32(0))
process.MessageLogger.cerr.LHEInterface = cms.untracked.PSet(limit = cms.untracked.int32(10000))
process.MessageLogger.cerr.FwkReport.reportEvery = cms.untracked.int32(10000)


process.maxEvents = cms.untracked.PSet( input = cms.untracked.int32(-1) )

process.source = cms.Source("LHESource",
    # replace 'myfile.root' with the source file you want to use
    #fileNames = cms.untracked.vstring("/store/lhe/5484/WJetsToLNu_8TeV-madgraph_14599.lhe")
    #fileNames = cms.untracked.vstring("/store/lhe/5484/WJetsToLNu_8TeV-madgraph_14600.lhe")
    #fileNames = cms.untracked.vstring("/store/lhe/2924/7TeV_wjets_smzerobmass_run410_unweighted_events_qcut20_mgPost.lhe")
    #fileNames = cms.untracked.vstring('/store/lhe/5468/DYJetsToLL_M-50_8TeV-madgraph_12489.lhe')
    #fileNames = cms.untracked.vstring('/store/lhe/5468/DYJetsToLL_M-50_8TeV-madgraph_12490.lhe')
    #fileNames = cms.untracked.vstring('/store/lhe/5468/DYJetsToLL_M-50_8TeV-madgraph_12491.lhe')
    #fileNames = cms.untracked.vstring('/store/lhe/5468/DYJetsToLL_M-50_8TeV-madgraph_12492.lhe')
    #fileNames = cms.untracked.vstring('/store/lhe/5468/DYJetsToLL_M-50_8TeV-madgraph_12493.lhe')
    fileNames = cms.untracked.vstring('/store/lhe/5468/DYJetsToLL_M-50_8TeV-madgraph_12494.lhe')
    #fileNames = cms.untracked.vstring('/store/lhe/5468/DYJetsToLL_M-50_8TeV-madgraph_12495.lhe')
    #fileNames = cms.untracked.vstring('/store/lhe/5468/DYJetsToLL_M-50_8TeV-madgraph_12496.lhe')
    #fileNames = cms.untracked.vstring('/store/lhe/5468/DYJetsToLL_M-50_8TeV-madgraph_12497.lhe')
    #fileNames = cms.untracked.vstring('/store/lhe/5468/DYJetsToLL_M-50_8TeV-madgraph_12498.lhe')
)

process.TFileService = cms.Service("TFileService",
    fileName = cms.string('matrix.root')
)


process.matrix = cms.EDAnalyzer('BinningAnalyzer',
  src = cms.InputTag("source")
)


process.p = cms.Path(process.matrix)
