#ifndef Lenzip_SortPFCandidateSelectorDefinition
#define Lenzip_SortPFCandidateSelectorDefinition

#include "DataFormats/ParticleFlowCandidate/interface/PFCandidateFwd.h"
#include "DataFormats/ParticleFlowCandidate/interface/PFCandidate.h"
#include "CommonTools/ParticleFlow/interface/PFCandidateSelectorDefinition.h"

namespace pf2pat {

  template <typename Comparator>
  class SortPFCandidateSelectorDefinition : public PFCandidateSelectorDefinition {
  
    typedef const typename collection::value_type * reference;
    typedef std::pair<reference, size_t> pair;

  private:
    struct PairComparator {
      PairComparator(const Comparator & cmp) : cmp_(cmp) { }
      bool operator()(const pair & t1, const pair & t2) const {
        return cmp_(*t1.first, *t2.first);
      } 
      Comparator cmp_;
    };

    unsigned int maxNumber_;
    PairComparator compare_;


  public:
    SortPFCandidateSelectorDefinition ( const edm::ParameterSet & cfg ) :
      maxNumber_( cfg.getParameter< unsigned int >( "maxNumber" ) ),
      compare_(Comparator()){ }
    
    
    void select( const HandleToCollection & hc, 
                 const edm::EventBase & e,
                 const edm::EventSetup& s
                 ) {
      selected_.clear();
    
      assert( hc.isValid() );
   
      std::vector<pair> v;
      for(size_t idx = 0; idx < hc->size(); ++ idx)
        v.push_back(std::make_pair( &(*hc)[idx], idx) );
      std::sort(v.begin(), v.end(), compare_);

      for( unsigned key=0; key < maxNumber_ && key < v.size(); ++key ) { 
          selected_.push_back( reco::PFCandidate(*(v[key].first)) );
          reco::PFCandidatePtr ptrToMother( hc, v[key].second );
          selected_.back().setSourceCandidatePtr( ptrToMother );
      }
    }

  };

}

#endif
