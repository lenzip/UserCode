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
      std::string pdfSetName_;
      std::string pdfShortName_;
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
 pdfSetName_(pset.getParameter<std::string> ("PdfSetNames")),
 _origECMS(pset.getParameter< double > ("OriginalECMS")),
 _newECMS(pset.getParameter< double > ("NewECMS"))
{
  size_t dot = pdfSetName_.find_first_of('.');
  size_t underscore = pdfSetName_.find_first_of('_');
  if (underscore<dot) {
        pdfShortName_ = pdfSetName_.substr(0,underscore);
  } else {
        pdfShortName_ = pdfSetName_.substr(0,dot);
  }
  produces<double>(pdfShortName_.data());
} 

/////////////////////////////////////////////////////////////////////////////////////
LHECOMWeightProducer::~LHECOMWeightProducer(){}

/////////////////////////////////////////////////////////////////////////////////////
void LHECOMWeightProducer::beginJob() {
  LHAPDF::initPDFSet(1,pdfSetName_);
}

/////////////////////////////////////////////////////////////////////////////////////
void LHECOMWeightProducer::endJob(){}

/////////////////////////////////////////////////////////////////////////////////////
void LHECOMWeightProducer::produce(edm::Event& iEvent, const edm::EventSetup&) {

      using namespace std;
      bool verbose = false;

      if (iEvent.isRealData()) return;

      edm::Handle<LHEEventProduct> lheevent;
      iEvent.getByLabel(lheTag_, lheevent);

      float Q = lheevent->hepeup().SCALUP;

      int id1        = lheevent->hepeup().IDUP[0];
      double x1      = fabs(lheevent->hepeup().PUP[0][2]/(_origECMS/2));
      double x1prime = fabs(lheevent->hepeup().PUP[0][2]/(_newECMS/2));
      //double pdf1 = pdfstuff->pdf()->xPDF.first;

      int id2        = lheevent->hepeup().IDUP[1];
      double x2      = fabs(lheevent->hepeup().PUP[1][2]/(_origECMS/2));
      double x2prime = fabs(lheevent->hepeup().PUP[1][2]/(_newECMS/2));
      //double pdf2 = pdfstuff->pdf()->xPDF.second;

      if (verbose){
        cout << "*******LHECOMWeightProducer*******" << endl;
        cout << " Q  : " << Q << endl; 
        cout << " id1: " << id1 << endl;
        cout << " x1 : " << x1  << endl;
        cout << " x1': " << x1prime << endl;

        cout << " id2: " << id2 << endl;
        cout << " x2 : " << x2  << endl;
        cout << " x2': " << x2prime << endl;
      } 

      //gluon is 0 in the LHAPDF numbering
      if (id1 == 21)
        id1 = 0;
      if (id2 == 21)
        id2 = 0;

      // Put PDF weights in the event
      if (verbose)
        cout << " Set : " << pdfSetName_ << endl;
     
      LHAPDF::usePDFMember(1,0);
      double oldpdf1 = LHAPDF::xfx(1, x1, Q, id1)/x1;
      double oldpdf2 = LHAPDF::xfx(1, x2, Q, id2)/x2;
      double newpdf1 = LHAPDF::xfx(1, x1prime, Q, id1)/x1prime;
      double newpdf2 = LHAPDF::xfx(1, x2prime, Q, id2)/x2prime;
      if (verbose) {
        cout << "     xfx1 : " << oldpdf1 << endl;
        cout << "     xfx2 : " << oldpdf2 << endl;

        cout << "     xfx1': " << newpdf1 << endl;
        cout << "     xfx2': " << newpdf2 << endl;
        cout << "     weight:" << (newpdf1/oldpdf1)*(newpdf2/oldpdf2) << endl;
      }
      std::auto_ptr<double> weight (new double((newpdf1/oldpdf1)*(newpdf2/oldpdf2)));
      iEvent.put(weight,pdfShortName_);
}

DEFINE_FWK_MODULE(LHECOMWeightProducer);
