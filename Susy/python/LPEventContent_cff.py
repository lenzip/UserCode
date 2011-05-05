import FWCore.ParameterSet.Config as cms

LPEventContent = cms.untracked.vstring(
  'keep *_LPWMuNu_*_*',
  'keep *_LPPF*JetSel_*_*',
  'keep *_LPMHTPFJet30*Sel_*_*',
  'keep *_LPMuMHT_*_*'
  # 'keep *_LPMHTPFJets30_*_*'
)
