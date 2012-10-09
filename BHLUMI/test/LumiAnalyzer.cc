// -*- C++ -*-
//
// Package:    LumiAnalyzer
// Class:      LumiAnalyzer
// 
/**\class LumiAnalyzer LumiAnalyzer.cc Giulio/LumiAnalyzer/src/LumiAnalyzer.cc

 Description: [one line class summary]

 Implementation:
     [Notes on implementation]
*/
//
// Original Author:  Piergiulio Lenzi,40 1-B01,+41227671638,
//         Created:  Tue Nov  8 15:57:17 CET 2011
// $Id: LumiAnalyzer.cc,v 1.1 2012/10/09 18:00:40 lenzip Exp $
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

#include "SimDataFormats/GeneratorProducts/interface/HepMCProduct.h"

#include "FWCore/ServiceRegistry/interface/Service.h"
#include "CommonTools/UtilAlgos/interface/TFileService.h"
#include "TH2F.h"
#include "TTree.h"
//
// class declaration
//

class LumiAnalyzer : public edm::EDAnalyzer {
   public:
      explicit LumiAnalyzer(const edm::ParameterSet&);
      ~LumiAnalyzer();

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
      
      TH1F *p1_E;
      TH1F *p2_E;
      TH1F *q1_E;
      TH1F *q2_E;

      TH1F *abscosth_p2;
      TH1F *abscosth_q2;

      TH1F *abscosth_p2z;
      TH1F *abscosth_q2z;

      TH1F *abscosth_min;
      TH1F *abscosth_min_np;

      TH1F *ph_E;
      TH1F *ISR;

      TH1F *abscosth_ph;
      TH1F *weight;
      TH1F *logweight;

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
LumiAnalyzer::LumiAnalyzer(const edm::ParameterSet& iConfig):
_src(iConfig.getParameter<edm::InputTag>("src"))
{
   edm::Service<TFileService> fs;
   //now do what ever initialization is needed
   p1_E = fs->make<TH1F>("p1_E","p1_E",100,110,130);
   p2_E = fs->make<TH1F>("p2_E","p2_E",520,0,130);
   q1_E = fs->make<TH1F>("q1_E","p1_E",100,110,130);
   q2_E = fs->make<TH1F>("q2_E","p2_E",520,0,130);

   abscosth_p2 = fs->make<TH1F>("abscosth_p2","abscosth_p2",100,0.6,1.01);
   abscosth_q2 = fs->make<TH1F>("abscosth_q2","abscosth_q2",100,0.6,1.01);

   abscosth_p2z = fs->make<TH1F>("abscosth_p2z","abscosth_p2z",100,0.999,1.01);
   abscosth_q2z = fs->make<TH1F>("abscosth_q2z","abscosth_q2z",100,0.999,1.01);

   abscosth_min = fs->make<TH1F>("abscosth_min","abscosth_min",100,0.6,1.01);
   abscosth_min_np = fs->make<TH1F>("abscosth_min_np","abscosth_min_np",100,0.6,1.01);

   ph_E = fs->make<TH1F>("ph_E","ph_E",100,0.,10.);
   ISR = fs->make<TH1F>("ISR","ISR",200,0.,40.);

   abscosth_ph = fs->make<TH1F>("abscosth_ph","abscosth_ph",100,0.9,1.01);
   weight = fs->make<TH1F>("weight","weight",1100,-10.,100.);
   logweight = fs->make<TH1F>("logweight","logweight",1000,-10.,10.);  
}


LumiAnalyzer::~LumiAnalyzer()
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
LumiAnalyzer::analyze(const edm::Event& iEvent, const edm::EventSetup& iSetup)
{
   using namespace edm;



   Handle<HepMCProduct> pIn;
   iEvent.getByLabel(_src,pIn);

   //loop over particles
   double cth_p2 = -1.;
   double cth_q2 = -1.;
   unsigned int nph = 0;
   double e_isr=0;
   const HepMC::GenEvent* gen = pIn->GetEvent();
   HepMC::GenEvent::particle_const_iterator ipart;
   double wgt = gen->weights()[0];
   weight->Fill(wgt);
   if (wgt>0)
     logweight->Fill(log10(wgt));
   for (ipart = gen->particles_begin(); ipart != gen->particles_end(); ++ipart){
      if (abs((*ipart)->status()) != 1) continue;
      if ((*ipart)->pdg_id() == 11 and (*ipart)->status() == -1){
        p1_E->Fill((*ipart)->momentum().e(),wgt);
      }
      if ((*ipart)->pdg_id() == -11 and (*ipart)->status() == -1){
        q1_E->Fill((*ipart)->momentum().e(),wgt);
      }  
      if ((*ipart)->pdg_id() == 11 and (*ipart)->status() == 1){ 
        p2_E->Fill((*ipart)->momentum().e(),wgt); 
        cth_p2=fabs(cos((*ipart)->momentum().theta()));
        abscosth_p2->Fill(cth_p2, wgt);
        abscosth_p2z->Fill(cth_p2,wgt);
      }
      if ((*ipart)->pdg_id() == -11 and (*ipart)->status() == 1){   
        q2_E->Fill((*ipart)->momentum().e(),wgt);
        cth_q2=fabs(cos((*ipart)->momentum().theta()));
        abscosth_q2->Fill(cth_q2, wgt);
        abscosth_q2z->Fill(cth_q2,wgt);
      }
      if ((*ipart)->pdg_id() == 22 and (*ipart)->status() == 1){
        nph += 1;
        ph_E->Fill((*ipart)->momentum().e(),wgt);
        double cthph=fabs(cos((*ipart)->momentum().theta()));
        abscosth_ph->Fill(cthph, wgt);
        if (cthph>cos(5./180.*M_PI)){
          e_isr+=(*ipart)->momentum().e();
        }
      }
      abscosth_min->Fill(std::min(cth_p2,cth_q2),wgt);
      if (nph == 0)
        abscosth_min_np->Fill(std::min(cth_p2,cth_q2),wgt); 
      ISR->Fill(e_isr, wgt);
   } 

}


// ------------ method called once each job just before starting event loop  ------------
void 
LumiAnalyzer::beginJob()
{
}

// ------------ method called once each job just after ending the event loop  ------------
void 
LumiAnalyzer::endJob() 
{
  //_matrixNormByRow->Divide(_normalizationPlotRow);
  //_matrixNormByColumn->Divide(_normalizationPlotColumn);
}

// ------------ method called when starting to processes a run  ------------
void 
LumiAnalyzer::beginRun(edm::Run const&, edm::EventSetup const&)
{
}

// ------------ method called when ending the processing of a run  ------------
void 
LumiAnalyzer::endRun(edm::Run const&, edm::EventSetup const&)
{
}

// ------------ method called when starting to processes a luminosity block  ------------
void 
LumiAnalyzer::beginLuminosityBlock(edm::LuminosityBlock const&, edm::EventSetup const&)
{
}

// ------------ method called when ending the processing of a luminosity block  ------------
void 
LumiAnalyzer::endLuminosityBlock(edm::LuminosityBlock const&, edm::EventSetup const&)
{
}

// ------------ method fills 'descriptions' with the allowed parameters for the module  ------------
void
LumiAnalyzer::fillDescriptions(edm::ConfigurationDescriptions& descriptions) {
  //The following says we do not know what parameters are allowed so do no validation
  // Please change this to state exactly what you do use, even if it is no parameters
  edm::ParameterSetDescription desc;
  desc.setUnknown();
  descriptions.addDefault(desc);
}

//define this as a plug-in
DEFINE_FWK_MODULE(LumiAnalyzer);
