// -*- C++ -*-
//
// Package:    RandomNumberProducer
// Class:      RandomNumberProducer
// 
/**\class RandomNumberProducer RandomNumberProducer.cc Giulio/RandomNumberProducer/src/RandomNumberProducer.cc

 Description: [one line class summary]

 Implementation:
     [Notes on implementation]
*/
//
// Original Author:  Piergiulio Lenzi,40 1-B01,+41227671638,
//         Created:  Wed Jan 25 20:02:58 CET 2012
// $Id$
//
//


// system include files
#include <memory>

// user include files
#include "FWCore/Framework/interface/Frameworkfwd.h"
#include "FWCore/Framework/interface/EDProducer.h"

#include "FWCore/Framework/interface/Event.h"
#include "FWCore/Framework/interface/MakerMacros.h"

#include "FWCore/ParameterSet/interface/ParameterSet.h"


#include "FWCore/Framework/interface/EventSetup.h"
#include "FWCore/Framework/interface/ESHandle.h"
#include "FWCore/Framework/interface/ModuleFactory.h"
#include "FWCore/Framework/interface/ESProducer.h"
#include "FWCore/ServiceRegistry/interface/Service.h"
#include "FWCore/Utilities/interface/RandomNumberGenerator.h"
#include "FWCore/Utilities/interface/Exception.h"
#include "CLHEP/Random/RandomEngine.h"
#include "CLHEP/Random/RandFlat.h"

#include <memory>

//
// class declaration
//

class RandomNumberProducer : public edm::EDProducer {
   public:
      explicit RandomNumberProducer(const edm::ParameterSet&);
      ~RandomNumberProducer();

      static void fillDescriptions(edm::ConfigurationDescriptions& descriptions);

   private:
      virtual void beginJob() ;
      virtual void produce(edm::Event&, const edm::EventSetup&);
      virtual void endJob() ;
      
      virtual void beginRun(edm::Run&, edm::EventSetup const&);
      virtual void endRun(edm::Run&, edm::EventSetup const&);
      virtual void beginLuminosityBlock(edm::LuminosityBlock&, edm::EventSetup const&);
      virtual void endLuminosityBlock(edm::LuminosityBlock&, edm::EventSetup const&);

      // ----------member data ---------------------------
      uint32_t _limit;
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
RandomNumberProducer::RandomNumberProducer(const edm::ParameterSet& iConfig)
{
   _limit = iConfig.getParameter<uint32_t>("limit");
 
   //register your products
   produces<uint32_t>();

  
}


RandomNumberProducer::~RandomNumberProducer()
{
 
   // do anything here that needs to be done at desctruction time
   // (e.g. close files, deallocate resources etc.)

}


//
// member functions
//

// ------------ method called to produce the data  ------------
void
RandomNumberProducer::produce(edm::Event& iEvent, const edm::EventSetup& iSetup)
{
   using namespace edm;
   
   edm::Service<edm::RandomNumberGenerator> rng;
   if(!rng.isAvailable()) {
    throw cms::Exception("Configuration")
       << "The RandomNumberProducer module requires the RandomNumberGeneratorService\n"
          "which appears to be absent.  Please add that service to your configuration\n"
          "or remove the modules that require it." << std::endl;
   }

   CLHEP::RandFlat flat(rng->getEngine());

   uint32_t n = flat.fireInt(1,_limit);
   // is put into the Event
   std::auto_ptr<uint32_t> pOut(new uint32_t(n));
   iEvent.put(pOut);
 
}

// ------------ method called once each job just before starting event loop  ------------
void 
RandomNumberProducer::beginJob()
{
}

// ------------ method called once each job just after ending the event loop  ------------
void 
RandomNumberProducer::endJob() {
}

// ------------ method called when starting to processes a run  ------------
void 
RandomNumberProducer::beginRun(edm::Run&, edm::EventSetup const&)
{
}

// ------------ method called when ending the processing of a run  ------------
void 
RandomNumberProducer::endRun(edm::Run&, edm::EventSetup const&)
{
}

// ------------ method called when starting to processes a luminosity block  ------------
void 
RandomNumberProducer::beginLuminosityBlock(edm::LuminosityBlock&, edm::EventSetup const&)
{
}

// ------------ method called when ending the processing of a luminosity block  ------------
void 
RandomNumberProducer::endLuminosityBlock(edm::LuminosityBlock&, edm::EventSetup const&)
{
}

// ------------ method fills 'descriptions' with the allowed parameters for the module  ------------
void
RandomNumberProducer::fillDescriptions(edm::ConfigurationDescriptions& descriptions) {
  //The following says we do not know what parameters are allowed so do no validation
  // Please change this to state exactly what you do use, even if it is no parameters
  edm::ParameterSetDescription desc;
  desc.setUnknown();
  descriptions.addDefault(desc);
}

//define this as a plug-in
DEFINE_FWK_MODULE(RandomNumberProducer);
