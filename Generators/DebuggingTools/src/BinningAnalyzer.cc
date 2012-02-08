// -*- C++ -*-
//
// Package:    BinningAnalyzer
// Class:      BinningAnalyzer
// 
/**\class BinningAnalyzer BinningAnalyzer.cc Giulio/BinningAnalyzer/src/BinningAnalyzer.cc

 Description: [one line class summary]

 Implementation:
     [Notes on implementation]
*/
//
// Original Author:  Piergiulio Lenzi,40 1-B01,+41227671638,
//         Created:  Tue Nov  8 15:57:17 CET 2011
// $Id$
//
//


// system include files
#include <memory>

// user include files
#include "FWCore/Framework/interface/Frameworkfwd.h"
#include "FWCore/Framework/interface/EDAnalyzer.h"

#include "FWCore/Framework/interface/Event.h"
#include "FWCore/Framework/interface/MakerMacros.h"

#include "FWCore/ParameterSet/interface/ParameterSet.h"

#include "SimDataFormats/GeneratorProducts/interface/LHEEventProduct.h"

#include "FWCore/ServiceRegistry/interface/Service.h"
#include "CommonTools/UtilAlgos/interface/TFileService.h"
#include "TH2F.h"
#include "TTree.h"
//
// class declaration
//

class BinningAnalyzer : public edm::EDAnalyzer {
   public:
      explicit BinningAnalyzer(const edm::ParameterSet&);
      ~BinningAnalyzer();

      static void fillDescriptions(edm::ConfigurationDescriptions& descriptions);


   private:
      virtual void beginJob() ;
      virtual void analyze(const edm::Event&, const edm::EventSetup&);
      virtual void endJob() ;

      virtual void beginRun(edm::Run const&, edm::EventSetup const&);
      virtual void endRun(edm::Run const&, edm::EventSetup const&);
      virtual void beginLuminosityBlock(edm::LuminosityBlock const&, edm::EventSetup const&);
      virtual void endLuminosityBlock(edm::LuminosityBlock const&, edm::EventSetup const&);

      // ----------member data ---------------------------
      edm::InputTag _src;
      TH2D* _matrix;
      //TH2D* _matrixNormByRow;
      //TH2D* _matrixNormByColumn;
      //TH2D* _normalizationPlotRow;
      //TH2D* _normalizationPlotColumn;
      TTree* _tree;
      int _njet;
      double _ht;
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
BinningAnalyzer::BinningAnalyzer(const edm::ParameterSet& iConfig):
_src(iConfig.getParameter<edm::InputTag>("src"))
{
   edm::Service<TFileService> fs;
   //now do what ever initialization is needed
   double htbins[] = {0, 50, 100, 200, 250, 300, 400, 8000};
   _matrix            = fs->make<TH2D>("htVsjet", "HT vs #jets", 5, -0.5, 4.5, 7, htbins);
   //_matrixNormByRow   = fs->make<TH2D>("htVsjetNormRow", "HT vs #jets norm by rows", 5, -0.5, 4.5, 5, htbins);
   //_matrixNormByColumn   = fs->make<TH2D>("htVsjetNormColumn", "HT vs #jets norm by column", 5, -0.5, 4.5, 5, htbins);
   _tree                 = fs->make<TTree>("tree", "tree");
   //_normalizationPlotRow = new TH2D("normalizationPlotRow", "HT vs #jets norm by rows", 5, -0.5, 4.5, 5, htbins);
   //_normalizationPlotColumn = new TH2D("normalizationPlotColumn", "HT vs #jets norm by columnss", 5, -0.5, 4.5, 5, htbins);
   _tree->Branch("_njet", &_njet, "_njet/I");
   _tree->Branch("_ht", &_ht, "_ht/D");
}


BinningAnalyzer::~BinningAnalyzer()
{
 
   // do anything here that needs to be done at desctruction time
   // (e.g. close files, deallocate resources etc.)
   //delete _normalizationPlotRow;
   //delete _normalizationPlotColumn;

}


//
// member functions
//

// ------------ method called for each event  ------------
void
BinningAnalyzer::analyze(const edm::Event& iEvent, const edm::EventSetup& iSetup)
{
   using namespace edm;



   Handle<LHEEventProduct> pIn;
   iEvent.getByLabel(_src,pIn);

   //loop over particles and find partons and compute ht
   _njet = 0;
   _ht = 0;
   //std::cout << pIn->hepeup().NUP << " particles" << std::endl;
   for (int i = 0; i < pIn->hepeup().NUP; ++i){
    if (abs(pIn->hepeup().IDUP[i])<6 ||abs(pIn->hepeup().IDUP[i])==21){
      ++_njet;
      //std::cout << (pIn->hepeup().PUP[i])[0] << " , " << (pIn->hepeup().PUP[i])[1] << std::endl;
      double pt = sqrt( std::pow((pIn->hepeup().PUP[i])[0], 2) +
                        std::pow((pIn->hepeup().PUP[i])[1], 2) ); 
      _ht += pt;                  
    }
   }
   _njet-=2;
/*
   for (int i = 0; i < 5; ++i){
     _normalizationPlotRow ->Fill(i, ht);
   }
   double htbins[] = {0, 200, 250, 300, 400, 8000};
   for (int i = 0; i < 5; ++i){
     _normalizationPlotColumn->Fill(njet-2, htbins[i]+1);
   }
*/
   _tree->Fill();

   _matrix->Fill(_njet, _ht);
   //_matrixNormByRow->Fill(njet-2, ht);
   //_matrixNormByColumn->Fill(njet-2, ht);
   
}


// ------------ method called once each job just before starting event loop  ------------
void 
BinningAnalyzer::beginJob()
{
}

// ------------ method called once each job just after ending the event loop  ------------
void 
BinningAnalyzer::endJob() 
{
  //_matrixNormByRow->Divide(_normalizationPlotRow);
  //_matrixNormByColumn->Divide(_normalizationPlotColumn);
}

// ------------ method called when starting to processes a run  ------------
void 
BinningAnalyzer::beginRun(edm::Run const&, edm::EventSetup const&)
{
}

// ------------ method called when ending the processing of a run  ------------
void 
BinningAnalyzer::endRun(edm::Run const&, edm::EventSetup const&)
{
}

// ------------ method called when starting to processes a luminosity block  ------------
void 
BinningAnalyzer::beginLuminosityBlock(edm::LuminosityBlock const&, edm::EventSetup const&)
{
}

// ------------ method called when ending the processing of a luminosity block  ------------
void 
BinningAnalyzer::endLuminosityBlock(edm::LuminosityBlock const&, edm::EventSetup const&)
{
}

// ------------ method fills 'descriptions' with the allowed parameters for the module  ------------
void
BinningAnalyzer::fillDescriptions(edm::ConfigurationDescriptions& descriptions) {
  //The following says we do not know what parameters are allowed so do no validation
  // Please change this to state exactly what you do use, even if it is no parameters
  edm::ParameterSetDescription desc;
  desc.setUnknown();
  descriptions.addDefault(desc);
}

//define this as a plug-in
DEFINE_FWK_MODULE(BinningAnalyzer);
