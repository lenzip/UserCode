#include <memory>

#include "FWCore/Framework/interface/Frameworkfwd.h"
#include "FWCore/Framework/interface/EDProducer.h"

#include "FWCore/Framework/interface/Event.h"
#include "FWCore/Framework/interface/MakerMacros.h"

#include "FWCore/ParameterSet/interface/ParameterSet.h"

#include "SimDataFormats/GeneratorProducts/interface/LHEEventProduct.h"

//
// class declaration
//
class LHECOMWeightProducer : public edm::EDProducer {
   public:
      explicit LHECOMWeightProducer(const edm::ParameterSet&);
      ~LHECOMWeightProducer();

   private:
      virtual void beginJob() ;
      virtual void produce(edm::Event&, const edm::EventSetup&);
      virtual void endJob() ;

      edm::InputTag lheTag_;
      std::vector<std::string> pdfSetNames_;
      std::vector<std::string> pdfShortNames_;
      double _origECMS;
      double _newECMS;
};

#include "SimDataFormats/GeneratorProducts/interface/GenEventInfoProduct.h"
namespace LHAPDF {
      void initPDFSet(int nset, const std::string& filename, int member=0);
      int numberPDF(int nset);
      void usePDFMember(int nset, int member);
      double xfx(int nset, double x, double Q, int fl);
      double getXmin(int nset, int member);
      double getXmax(int nset, int member);
      double getQ2min(int nset, int member);
      double getQ2max(int nset, int member);
      void extrapolate(bool extrapolate=true);
}

/////////////////////////////////////////////////////////////////////////////////////
LHECOMWeightProducer::LHECOMWeightProducer(const edm::ParameterSet& pset) :
 lheTag_(pset.getParameter<edm::InputTag> ("lheSrc")),
 pdfSetNames_(pset.getParameter<std::vector<std::string> > ("PdfSetNames")),
 _origECMS(pset.getParameter< double > ("OriginalECMS")),
 _newECMS(pset.getParameter< double > ("NewECMS"))
{
      if (pdfSetNames_.size()>3) {
            edm::LogWarning("") << pdfSetNames_.size() << " PDF sets requested on input. Using only the first 3 sets and ignoring the rest!!";
            pdfSetNames_.erase(pdfSetNames_.begin()+3,pdfSetNames_.end());
      }

      for (unsigned int k=0; k<pdfSetNames_.size(); k++) {
            size_t dot = pdfSetNames_[k].find_first_of('.');
            size_t underscore = pdfSetNames_[k].find_first_of('_');
            if (underscore<dot) {
                  pdfShortNames_.push_back(pdfSetNames_[k].substr(0,underscore));
            } else {
                  pdfShortNames_.push_back(pdfSetNames_[k].substr(0,dot));
            }
            produces<std::vector<double> >(pdfShortNames_[k].data());
      }
} 

/////////////////////////////////////////////////////////////////////////////////////
LHECOMWeightProducer::~LHECOMWeightProducer(){}

/////////////////////////////////////////////////////////////////////////////////////
void LHECOMWeightProducer::beginJob() {
      for (unsigned int k=1; k<=pdfSetNames_.size(); k++) {
            LHAPDF::initPDFSet(k,pdfSetNames_[k-1]);
      }
}

/////////////////////////////////////////////////////////////////////////////////////
void LHECOMWeightProducer::endJob(){}

/////////////////////////////////////////////////////////////////////////////////////
void LHECOMWeightProducer::produce(edm::Event& iEvent, const edm::EventSetup&) {

      if (iEvent.isRealData()) return;

      edm::Handle<LHEEventProduct> lheevent;
      iEvent.getByLabel(lheTag_, lheevent);

      float Q = lheevent->hepeup().SCALUP;

      int id1        = lheevent->hepeup().IDUP[0];
      double x1      = lheevent->hepeup().PUP[0][2]/(_origECMS/2);
      double x1prime = lheevent->hepeup().PUP[0][2]/(_newECMS/2);
      //double pdf1 = pdfstuff->pdf()->xPDF.first;

      int id2        = lheevent->hepeup().IDUP[1];
      double x2      = lheevent->hepeup().PUP[1][2]/(_origECMS/2);
      double x2prime = lheevent->hepeup().PUP[1][2]/(_newECMS/2);
      //double pdf2 = pdfstuff->pdf()->xPDF.second; 

      // Put PDF weights in the event
      for (unsigned int k=1; k<=pdfSetNames_.size(); ++k) {
            std::auto_ptr<std::vector<double> > weights (new std::vector<double>);
            unsigned int nweights = 1;
            if (LHAPDF::numberPDF(k)>1) nweights += LHAPDF::numberPDF(k);
            weights->reserve(nweights);
        
            for (unsigned int i=0; i<nweights; ++i) {
                  LHAPDF::usePDFMember(k,i);
                  double oldpdf1 = LHAPDF::xfx(k, x1, Q, id1)/x1;
                  double oldpdf2 = LHAPDF::xfx(k, x2, Q, id2)/x2;
                  double newpdf1 = LHAPDF::xfx(k, x1prime, Q, id1)/x1prime;
                  double newpdf2 = LHAPDF::xfx(k, x2prime, Q, id2)/x2prime;
                  weights->push_back(newpdf1/oldpdf1*newpdf2/oldpdf2);
            }
            iEvent.put(weights,pdfShortNames_[k-1]);
      }
}

DEFINE_FWK_MODULE(LHECOMWeightProducer);
