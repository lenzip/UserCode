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
    duplicateCheckMode = cms.untracked.string('noDuplicateCheck'),
    fileNames = (cms.untracked.vstring(
"file:MCDBtoEDM_1.root",
"file:MCDBtoEDM_10.root",
"file:MCDBtoEDM_100.root",
"file:MCDBtoEDM_101.root",
"file:MCDBtoEDM_102.root",
"file:MCDBtoEDM_103.root",
"file:MCDBtoEDM_104.root",
"file:MCDBtoEDM_105.root",
"file:MCDBtoEDM_106.root",
"file:MCDBtoEDM_107.root",
"file:MCDBtoEDM_108.root",
"file:MCDBtoEDM_109.root",
"file:MCDBtoEDM_11.root",
"file:MCDBtoEDM_110.root",
"file:MCDBtoEDM_111.root",
"file:MCDBtoEDM_112.root",
"file:MCDBtoEDM_113.root",
"file:MCDBtoEDM_114.root",
"file:MCDBtoEDM_115.root",
"file:MCDBtoEDM_116.root",
"file:MCDBtoEDM_117.root",
"file:MCDBtoEDM_118.root",
"file:MCDBtoEDM_119.root",
"file:MCDBtoEDM_12.root",
"file:MCDBtoEDM_120.root",
"file:MCDBtoEDM_121.root",
"file:MCDBtoEDM_122.root",
"file:MCDBtoEDM_123.root",
"file:MCDBtoEDM_124.root",
"file:MCDBtoEDM_125.root",
"file:MCDBtoEDM_126.root",
"file:MCDBtoEDM_127.root",
"file:MCDBtoEDM_128.root",
"file:MCDBtoEDM_129.root",
"file:MCDBtoEDM_13.root",
"file:MCDBtoEDM_130.root",
"file:MCDBtoEDM_131.root",
"file:MCDBtoEDM_132.root",
"file:MCDBtoEDM_133.root",
"file:MCDBtoEDM_134.root",
"file:MCDBtoEDM_135.root",
"file:MCDBtoEDM_136.root",
"file:MCDBtoEDM_137.root",
"file:MCDBtoEDM_138.root",
"file:MCDBtoEDM_139.root",
"file:MCDBtoEDM_14.root",
"file:MCDBtoEDM_140.root",
"file:MCDBtoEDM_141.root",
"file:MCDBtoEDM_142.root",
"file:MCDBtoEDM_143.root",
"file:MCDBtoEDM_144.root",
"file:MCDBtoEDM_145.root",
"file:MCDBtoEDM_146.root",
"file:MCDBtoEDM_147.root",
"file:MCDBtoEDM_148.root",
"file:MCDBtoEDM_149.root",
"file:MCDBtoEDM_15.root",
"file:MCDBtoEDM_150.root",
"file:MCDBtoEDM_151.root",
"file:MCDBtoEDM_152.root",
"file:MCDBtoEDM_153.root",
"file:MCDBtoEDM_154.root",
"file:MCDBtoEDM_155.root",
"file:MCDBtoEDM_156.root",
"file:MCDBtoEDM_157.root",
"file:MCDBtoEDM_158.root",
"file:MCDBtoEDM_159.root",
"file:MCDBtoEDM_16.root",
"file:MCDBtoEDM_160.root",
"file:MCDBtoEDM_161.root",
"file:MCDBtoEDM_162.root",
"file:MCDBtoEDM_163.root",
"file:MCDBtoEDM_164.root",
"file:MCDBtoEDM_165.root",
"file:MCDBtoEDM_166.root",
"file:MCDBtoEDM_167.root",
"file:MCDBtoEDM_168.root",
"file:MCDBtoEDM_169.root",
"file:MCDBtoEDM_17.root",
"file:MCDBtoEDM_170.root",
"file:MCDBtoEDM_171.root",
"file:MCDBtoEDM_172.root",
"file:MCDBtoEDM_173.root",
"file:MCDBtoEDM_174.root",
"file:MCDBtoEDM_175.root",
"file:MCDBtoEDM_176.root",
"file:MCDBtoEDM_177.root",
"file:MCDBtoEDM_178.root",
"file:MCDBtoEDM_179.root",
"file:MCDBtoEDM_18.root",
"file:MCDBtoEDM_180.root",
"file:MCDBtoEDM_181.root",
"file:MCDBtoEDM_182.root",
"file:MCDBtoEDM_183.root",
"file:MCDBtoEDM_184.root",
"file:MCDBtoEDM_185.root",
"file:MCDBtoEDM_186.root",
"file:MCDBtoEDM_187.root",
"file:MCDBtoEDM_188.root",
"file:MCDBtoEDM_189.root",
"file:MCDBtoEDM_19.root",
"file:MCDBtoEDM_190.root",
"file:MCDBtoEDM_191.root",
"file:MCDBtoEDM_192.root",
"file:MCDBtoEDM_193.root",
"file:MCDBtoEDM_194.root",
"file:MCDBtoEDM_195.root",
"file:MCDBtoEDM_196.root",
"file:MCDBtoEDM_197.root",
"file:MCDBtoEDM_198.root",
"file:MCDBtoEDM_199.root",
"file:MCDBtoEDM_2.root",
"file:MCDBtoEDM_20.root",
"file:MCDBtoEDM_200.root",
"file:MCDBtoEDM_21.root",
"file:MCDBtoEDM_22.root",
"file:MCDBtoEDM_23.root",
"file:MCDBtoEDM_24.root",
"file:MCDBtoEDM_25.root",
"file:MCDBtoEDM_26.root",
"file:MCDBtoEDM_27.root",
"file:MCDBtoEDM_28.root",
"file:MCDBtoEDM_29.root",
"file:MCDBtoEDM_3.root",
"file:MCDBtoEDM_30.root",
"file:MCDBtoEDM_31.root",
"file:MCDBtoEDM_32.root",
"file:MCDBtoEDM_33.root",
"file:MCDBtoEDM_34.root",
"file:MCDBtoEDM_35.root",
"file:MCDBtoEDM_36.root",
"file:MCDBtoEDM_37.root",
"file:MCDBtoEDM_38.root",
"file:MCDBtoEDM_39.root",
"file:MCDBtoEDM_4.root",
"file:MCDBtoEDM_40.root",
"file:MCDBtoEDM_41.root",
"file:MCDBtoEDM_42.root",
"file:MCDBtoEDM_43.root",
"file:MCDBtoEDM_44.root",
"file:MCDBtoEDM_45.root",
"file:MCDBtoEDM_46.root",
"file:MCDBtoEDM_47.root",
"file:MCDBtoEDM_48.root",
"file:MCDBtoEDM_49.root",
"file:MCDBtoEDM_5.root",
"file:MCDBtoEDM_50.root",
"file:MCDBtoEDM_51.root",
"file:MCDBtoEDM_52.root",
"file:MCDBtoEDM_53.root",
"file:MCDBtoEDM_54.root",
"file:MCDBtoEDM_55.root",
"file:MCDBtoEDM_56.root",
"file:MCDBtoEDM_57.root",
"file:MCDBtoEDM_58.root",
"file:MCDBtoEDM_59.root",
"file:MCDBtoEDM_6.root",
"file:MCDBtoEDM_60.root",
"file:MCDBtoEDM_61.root",
"file:MCDBtoEDM_62.root",
"file:MCDBtoEDM_63.root",
"file:MCDBtoEDM_64.root",
"file:MCDBtoEDM_65.root",
"file:MCDBtoEDM_66.root",
"file:MCDBtoEDM_67.root",
"file:MCDBtoEDM_68.root",
"file:MCDBtoEDM_69.root",
"file:MCDBtoEDM_7.root",
"file:MCDBtoEDM_70.root",
"file:MCDBtoEDM_71.root",
"file:MCDBtoEDM_72.root",
"file:MCDBtoEDM_73.root",
"file:MCDBtoEDM_74.root",
"file:MCDBtoEDM_75.root",
"file:MCDBtoEDM_76.root",
"file:MCDBtoEDM_77.root",
"file:MCDBtoEDM_78.root",
"file:MCDBtoEDM_79.root",
"file:MCDBtoEDM_8.root",
"file:MCDBtoEDM_80.root",
"file:MCDBtoEDM_81.root",
"file:MCDBtoEDM_82.root",
"file:MCDBtoEDM_83.root",
"file:MCDBtoEDM_84.root",
"file:MCDBtoEDM_85.root",
"file:MCDBtoEDM_86.root",
"file:MCDBtoEDM_87.root",
"file:MCDBtoEDM_88.root",
"file:MCDBtoEDM_89.root",
"file:MCDBtoEDM_9.root",
"file:MCDBtoEDM_90.root",
"file:MCDBtoEDM_91.root",
"file:MCDBtoEDM_92.root",
"file:MCDBtoEDM_93.root",
"file:MCDBtoEDM_94.root",
"file:MCDBtoEDM_95.root",
"file:MCDBtoEDM_96.root",
"file:MCDBtoEDM_97.root",
"file:MCDBtoEDM_98.root",
"file:MCDBtoEDM_99.root"
    ))
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

expanded = process.dumpPython()
print expanded
