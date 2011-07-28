#include "FWCore/Framework/interface/MakerMacros.h"
#include "CommonTools/UtilAlgos/interface/ObjectSelector.h"
#include "CommonTools/UtilAlgos/interface/SortCollectionSelector.h"
#include "CommonTools/Utils/interface/PtComparator.h"
#include "DataFormats/ParticleFlowCandidate/interface/PFCandidate.h"
#include "DataFormats/ParticleFlowCandidate/interface/PFCandidateFwd.h"
#include "Lenzip/Susy/interface/SortPFCandidateSelectorDefinition.h"

typedef ObjectSelector<
          pf2pat::SortPFCandidateSelectorDefinition<
            GreaterByPt<reco::PFCandidate>
          >
        > LargestPtPFCandSelector;

DEFINE_FWK_MODULE( LargestPtPFCandSelector );
