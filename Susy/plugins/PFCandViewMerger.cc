#include "FWCore/Framework/interface/MakerMacros.h"
#include "Lenzip/Susy/interface/PFMerger.h"
#include "DataFormats/ParticleFlowCandidate/interface/PFCandidate.h"
#include "DataFormats/Common/interface/View.h"

//typedef PFMerger<edm::View<reco::PFCandidate>, std::vector<reco::PFCandidate> > PFCandViewPFMerger;
typedef PFMerger<std::vector<reco::PFCandidate>, std::vector<reco::PFCandidate> > PFCandViewPFMerger;

DEFINE_FWK_MODULE( PFCandViewPFMerger );

