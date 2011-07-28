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

    process.leadingLeptonSelectionSequence = cms.Sequence(process.leadingMuon )
    #process.leadingLeptonSelectionSequence = cms.Sequence(process.leadingMuon + process.leadingElectron + process.twoLeadingLeptons + process.leadingLepton)

    process.pfNoMuonAK5LeadingLepton.verbose = cms.untracked.bool(True)
    process.pfNoElectronAK5LeadingLepton.verbose = cms.untracked.bool(True)
    #removal of the leading lepton
    getattr(process,"pfNoMuon"+postfix).enable = True
    getattr(process,"pfNoMuon"+postfix).topCollection = src = cms.InputTag('leadingMuon')
    getattr(process,"pfNoElectron"+postfix).enable = False 
    #getattr(process,"pfNoElectron"+postfix).topCollection = src = cms.InputTag('leadingElectron')
    getattr(process,"pfNoElectron"+postfix).bottomCollection = src = cms.InputTag('pfNoPileUp'+postfix)
    
    #getattr(process, 'patPF2PATSequence'+postfix).insert(0,(process.leadingMuonSelectionSequence))
    seq = getattr(process, 'pfMuonSequence'+postfix)
    seq += process.leadingMuon
    setattr (process, 'pfMuonSequence'+postfix, seq )
    #getattr(process, 'pfNoPileUpSequence'+postfix) += process.leadingLeptonSelectionSequence
  
