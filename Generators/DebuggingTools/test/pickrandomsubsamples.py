# Auto generated configuration file
# using: 
# Revision: 1.284.2.5 
# Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/PyReleaseValidation/python/ConfigBuilder.py,v 
# with command line options: MCDBtoEDM --conditions auto:mc -s NONE --eventcontent RAWSIM --datatier GEN --no_exec -n 5000000 --filein lhe:5468
import FWCore.ParameterSet.Config as cms

process = cms.Process('PICK')

# import of standard configurations
#process.load('Configuration.StandardSequences.Services_cff')
#process.load('SimGeneral.HepPDTESSource.pythiapdt_cfi')
process.load('FWCore.MessageService.MessageLogger_cfi')
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

process.RandomNumberGeneratorService = cms.Service("RandomNumberGeneratorService",
  random = cms.PSet(
    initialSeed = cms.untracked.uint32(234567),
    engineName = cms.untracked.string('HepJamesRandom')
  )
)  


process.maxEvents = cms.untracked.PSet(
    input = cms.untracked.int32(-1)
)

# Input source
process.source = cms.Source("PoolSource",
    fileNames = (cms.untracked.vstring('file:MCDBtoEDM_NONE.root'))
)

process.options = cms.untracked.PSet(
)

nsubsamples=200

process.random=cms.EDProducer("RandomNumberProducer",
  limit = cms.uint32(nsubsamples)
)  

for i in range(1,nsubsamples):
  namefilter="filter"+str(i)
  print namefilter
  filter = cms.EDFilter("RandomNumberFilter",
    Number = cms.uint32(i),
    src = cms.InputTag("random")
  )
  namepath="path"+str(i)
  setattr(process, namefilter, filter)
  path=cms.Path(process.random+getattr(process, namefilter))
  setattr(process, namepath, path)
  print getattr(process, namepath) 
  nameout = "out"+str(i)
  out = cms.OutputModule("PoolOutputModule",
    fileName = cms.untracked.string('pseudo'+str(i)+'.root'),
    SelectEvents = cms.untracked.PSet(
      SelectEvents = cms.vstring(namepath)
    )
  )
  setattr(process, nameout, out)
  nameendpath='end'+str(i)
  end=cms.EndPath(getattr(process, nameout))
  setattr(process, nameendpath, end)  

#expanded = process.dumpPython()
#print expanded
