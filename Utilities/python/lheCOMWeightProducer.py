import FWCore.ParameterSet.Config as cms

lheCOMWeightProducer = cms.EDProducer("LHECOMWeightProducer",
  lheSrc = cms.InputTag("source"),
  PdfSetNames = cms.string("cteq6ll.LHpdf"),
  OriginalECMS = cms.double(8000),
  NewECMS = cms.double(7000)
)
