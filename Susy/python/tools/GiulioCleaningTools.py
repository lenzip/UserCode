import FWCore.ParameterSet.Config as cms

def cleanLeadingParicles( process, postfix):
    """
    Mdifies
    """

    #selection of the leading lepton  
    process.leadingMuon = cms.EDFilter('LargestPtPFCandSelector',
      src = cms.InputTag('pfAllMuons'+postfix),
      maxNumber = cms.uint32(1))
    process.leadingElectron = cms.EDFilter('LargestPtPFCandSelector',
      src = cms.InputTag('pfAllElectrons'+postfix),
      maxNumber = cms.uint32(1))
    process.twoLeadingLeptons = cms.EDProducer('PFCandViewPFMerger',
      src = cms.VInputTag('leadingMuon', 'leadingElectron'))
    process.leadingLepton = cms.EDFilter('LargestPtPFCandSelector',
      src = cms.InputTag('twoLeadingLeptons'),
      maxNumber = cms.uint32(1))

    #we need to run first the AllMuon and AllElectron producers
    allmu = getattr(process, 'pfAllMuons'+postfix)
    alle  = getattr(process, 'pfAllElectrons'+postfix)
    alle.src = 'pfNoPileUp'+postfix
    process.allleptons = cms.Sequence(allmu + alle)
    process.leadingLeptonSelectionSequence = cms.Sequence(process.allleptons + 
                                                          process.leadingMuon + 
                                                          process.leadingElectron + 
                                                          process.twoLeadingLeptons + 
                                                          process.leadingLepton)

    #process.leadingLeptonSelectionSequence = cms.Sequence(process.leadingMuon + process.leadingElectron + process.twoLeadingLeptons + process.leadingLepton)

    process.pfNoMuonAK5LeadingLepton.verbose = cms.untracked.bool(True)
    process.pfNoElectronAK5LeadingLepton.verbose = cms.untracked.bool(True)
    #removal of the leading lepton
    getattr(process,"pfNoMuon"+postfix).enable = True
    getattr(process,"pfNoMuon"+postfix).topCollection = src = cms.InputTag('leadingLepton')
    getattr(process,"pfNoElectron"+postfix).enable = True 
    getattr(process,"pfNoElectron"+postfix).topCollection = src = cms.InputTag('leadingLepton')
    # line below because we don't want to remove the muons (pfNoElectron sequence had as input pfNoMuon by default) 
    getattr(process,"pfNoElectron"+postfix).bottomCollection = src = cms.InputTag('pfNoPileUp'+postfix)
    
    pfnopu = getattr(process, 'pfNoPileUpSequence'+postfix) 
    pfnopu += process.leadingLeptonSelectionSequence
    setattr ( process, 'pfNoPileUpSequence'+postfix, pfnopu)
  
