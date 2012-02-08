// -*- C++ -*-
//
// Package:    RandomNumberFilter
// Class:      RandomNumberFilter
// 
/**\class RandomNumberFilter RandomNumberFilter.cc Giulio/RandomNumberFilter/src/RandomNumberFilter.cc

 Description: [one line class summary]

 Implementation:
     [Notes on implementation]
*/
//
// Original Author:  Piergiulio Lenzi,40 1-B01,+41227671638,
//         Created:  Wed Jan 25 20:11:39 CET 2012
// $Id$
//
//


// system include files
#include <memory>

// user include files
#include "FWCore/Framework/interface/Frameworkfwd.h"
#include "FWCore/Framework/interface/EDFilter.h"

#include "FWCore/Framework/interface/Event.h"
#include "FWCore/Framework/interface/MakerMacros.h"

#include "FWCore/ParameterSet/interface/ParameterSet.h"

//
// class declaration
//

class RandomNumberFilter : public edm::EDFilter {
   public:
      explicit RandomNumberFilter(const edm::ParameterSet&);
      ~RandomNumberFilter();

      static void fillDescriptions(edm::ConfigurationDescriptions& descriptions);

   private:
      virtual void beginJob() ;
      virtual bool filter(edm::Event&, const edm::EventSetup&);
      virtual void endJob() ;
      
      virtual bool beginRun(edm::Run&, edm::EventSetup const&);
      virtual bool endRun(edm::Run&, edm::EventSetup const&);
      virtual bool beginLuminosityBlock(edm::LuminosityBlock&, edm::EventSetup const&);
      virtual bool endLuminosityBlock(edm::LuminosityBlock&, edm::EventSetup const&);

      // ----------member data ---------------------------
      uint32_t _number;
      edm::InputTag _src;
};

//
// constants, enums and typedefs
//

//
// static data member definitions
//

//
// constructors and destructor
//
RandomNumberFilter::RandomNumberFilter(const edm::ParameterSet& iConfig)
{
   //now do what ever initialization is needed
   _number = iConfig.getParameter<uint32_t>("Number");
   _src = iConfig.getParameter<edm::InputTag>("src");
}


RandomNumberFilter::~RandomNumberFilter()
{
 
   // do anything here that needs to be done at desctruction time
   // (e.g. close files, deallocate resources etc.)

}


//
// member functions
//

// ------------ method called on each new Event  ------------
bool
RandomNumberFilter::filter(edm::Event& iEvent, const edm::EventSetup& iSetup)
{
   using namespace edm;
   Handle<uint32_t> pIn;
   iEvent.getByLabel(_src,pIn);

   //uint32_t mod = *pIn%_number;
   double mod = *pIn/double(_number);
   //std::cout << "mod(" << *pIn << "," << _number << ")="<< mod << std::endl;

   if (mod == 1) return true;
   return false;
}

// ------------ method called once each job just before starting event loop  ------------
void 
RandomNumberFilter::beginJob()
{
}

// ------------ method called once each job just after ending the event loop  ------------
void 
RandomNumberFilter::endJob() {
}

// ------------ method called when starting to processes a run  ------------
bool 
RandomNumberFilter::beginRun(edm::Run&, edm::EventSetup const&)
{ 
  return true;
}

// ------------ method called when ending the processing of a run  ------------
bool 
RandomNumberFilter::endRun(edm::Run&, edm::EventSetup const&)
{
  return true;
}

// ------------ method called when starting to processes a luminosity block  ------------
bool 
RandomNumberFilter::beginLuminosityBlock(edm::LuminosityBlock&, edm::EventSetup const&)
{
  return true;
}

// ------------ method called when ending the processing of a luminosity block  ------------
bool 
RandomNumberFilter::endLuminosityBlock(edm::LuminosityBlock&, edm::EventSetup const&)
{
  return true;
}

// ------------ method fills 'descriptions' with the allowed parameters for the module  ------------
void
RandomNumberFilter::fillDescriptions(edm::ConfigurationDescriptions& descriptions) {
  //The following says we do not know what parameters are allowed so do no validation
  // Please change this to state exactly what you do use, even if it is no parameters
  edm::ParameterSetDescription desc;
  desc.setUnknown();
  descriptions.addDefault(desc);
}
//define this as a plug-in
DEFINE_FWK_MODULE(RandomNumberFilter);
