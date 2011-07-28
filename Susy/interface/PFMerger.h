#ifndef Lenzip_PFMerger_h
#define Lenzip_PFMerger_h
/** \class Merger

  it is able to produce std::vector in output

 */
#include "FWCore/Framework/interface/EDProducer.h"
#include "FWCore/Framework/interface/Event.h"
#include "FWCore/ParameterSet/interface/ParameterSet.h"
#include "FWCore/Utilities/interface/InputTag.h"
#include "DataFormats/Common/interface/CloneTrait.h"
#include <vector>

template<typename InputCollection, 
         typename OutputCollection = InputCollection,
         typename P = typename edm::clonehelper::CloneTrait<InputCollection>::type>
class PFMerger : public edm::EDProducer {
public:
  /// constructor from parameter set

  explicit PFMerger( const edm::ParameterSet& );
  /// destructor

  ~PFMerger();

private:
  /// process an event

  virtual void produce( edm::Event&, const edm::EventSetup& );
  /// vector of strings

  typedef std::vector<edm::InputTag> vtag;
  /// labels of the collections to be merged

  vtag src_;
};

template<typename InputCollection, typename OutputCollection, typename P>
PFMerger<InputCollection, OutputCollection, P>::PFMerger( const edm::ParameterSet& par ) : 
  src_( par.template getParameter<vtag>( "src" ) ) {
  produces<OutputCollection>();
}

template<typename InputCollection, typename OutputCollection, typename P>
PFMerger<InputCollection, OutputCollection, P>::~PFMerger() {
}

template<typename InputCollection, typename OutputCollection, typename P>
void PFMerger<InputCollection, OutputCollection, P>::produce( edm::Event& evt, const edm::EventSetup& ) {
  std::auto_ptr<OutputCollection> coll( new OutputCollection );
  for( vtag::const_iterator s = src_.begin(); s != src_.end(); ++ s ) {
    edm::Handle<InputCollection> h;
    evt.getByLabel( * s, h );
    for( typename InputCollection::const_iterator c = h->begin(); c != h->end(); ++c ) {
      coll->push_back( * c );
    }
  }
  evt.put( coll );
}

#endif

