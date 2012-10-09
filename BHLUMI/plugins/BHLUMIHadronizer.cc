#include <iostream>
#include <sstream>
#include <string>
#include <memory>
#include <stdint.h>


#include "HepMC/GenEvent.h"
#include "HepMC/HEPEVT_Wrapper.h"
#include "HepMC/IO_HEPEVT.h"

#include "GeneratorInterface/Core/interface/ParameterCollector.h"
#include "GeneratorInterface/Core/interface/BaseHadronizer.h"
#include "GeneratorInterface/Core/interface/GeneratorFilter.h"
#include "GeneratorInterface/Core/interface/HadronizerFilter.h"
#include "GeneratorInterface/Core/interface/RNDMEngineAccess.h"
#include "FWCore/ParameterSet/interface/FileInPath.h"
#include "GeneratorInterface/Pythia6Interface/interface/Pythia6Service.h"
#include "GeneratorInterface/Core/interface/FortranCallback.h"


HepMC::IO_HEPEVT conv;

extern "C" {
  void init_(const char*, int*, double*);
  void bhlumi_(int*, double*, int*);
//}

//extern "C" {
  extern struct {
    double p1[4], q1[4], p2[4], q2[4], phot[4][100];
    int nphot;
  } momset_;
  
  extern struct {
    double wtmod, wtcru1, wtcru2, wtset[300];
  } wgtall_;
}

class BHLUMIHadronizer : public gen::BaseHadronizer {
public:
  BHLUMIHadronizer(const edm::ParameterSet &params);
  ~BHLUMIHadronizer();
  
  bool readSettings( int ) { return true; }
  bool initializeForInternalPartons();
  bool declareStableParticles(const std::vector<int> &pdgIds);
  bool declareSpecialSettings( const std::vector<std::string> ) { return true; }
  bool generatePartonsAndHadronize();
  bool decay();
  void statistics(){};
  bool residualDecay();
  void finalizeEvent();
  const char *classname() const { return "BHLUMIHadronizer"; }
  
private:
  HepMC::GenEvent* convert() const; 
  //gen::Pythia6Service* fPy6Service;
  std::string _configPath;
  double _xpar[100];
  int _npar[100];
  
};

BHLUMIHadronizer::BHLUMIHadronizer(const edm::ParameterSet &params) :
  BaseHadronizer(params),
  //fPy6Service( new gen::Pythia6Service() ),
  _configPath(params.getParameter<std::string>("CardsPath"))
   
{
}

BHLUMIHadronizer::~BHLUMIHadronizer()
{
}

bool BHLUMIHadronizer::initializeForInternalPartons()
{
  
  edm::FileInPath file( _configPath.c_str() );
  const char* filename = file.fullPath().c_str();
  //std::cout << file.fullPath() << std::endl;
  init_(filename, _npar, _xpar);
  int mode = -1;
  bhlumi_(&mode, _xpar, _npar);
  std::cout << momset_.p1[0] << ","<< momset_.p1[1] << ","<<momset_.p1[2]<< ","<< momset_.p1[3]<< std::endl;
  return true;
}

#if 0
// naive Sherpa HepMC status fixup //FIXME 
static int getStatus(const HepMC::GenParticle *p)
{
  return status;
}
#endif

//FIXME
bool BHLUMIHadronizer::declareStableParticles(const std::vector<int> &pdgIds)
{
#if 0
  for(std::vector<int>::const_iterator iter = pdgIds.begin();
      iter != pdgIds.end(); ++iter)
    if (!markStable(*iter))
      return false;
  
  return true;
#else
  return false;
#endif
}


bool BHLUMIHadronizer::generatePartonsAndHadronize()
{
  //gen::Pythia6Service::InstanceWrapper guard(fPy6Service);
  //gen::FortranCallback::getInstance()->resetIterationsPerEvent();
  //get the next event
  int mode = 0;
  bhlumi_(&mode, _xpar, _npar);
  //convert to hepmc
  //std::cout << momset_.p1[0] << ","<< momset_.p1[1] << ","<<momset_.p1[2]<< ","<< momset_.p1[3]<< std::endl;
  event().reset(convert());
  return true;
}

bool BHLUMIHadronizer::decay()
{
	return true;
}

bool BHLUMIHadronizer::residualDecay()
{
	return true;
}

void BHLUMIHadronizer::finalizeEvent()
{
}

HepMC::GenEvent* BHLUMIHadronizer::convert() const{
  HepMC::GenEvent *evt = new HepMC::GenEvent();
  HepMC::GenVertex* v = new HepMC::GenVertex();
  evt->add_vertex(v);
  //p is electron q is positron
  //add incoming electron/positron
  HepMC::FourVector pp1(momset_.p1[0], momset_.p1[1], momset_.p1[2], momset_.p1[3]);
  HepMC::GenParticle* p1 = new HepMC::GenParticle(pp1, 11, 1);
  v->add_particle_in(p1);
  HepMC::FourVector pq1(momset_.q1[0], momset_.q1[1], momset_.q1[2], momset_.q1[3]);
  HepMC::GenParticle* q1 = new HepMC::GenParticle(pq1, -11, 1);
  v->add_particle_in(q1);
  //add outgoing electron/positron 
  HepMC::FourVector pp2(momset_.p2[0], momset_.p2[1], momset_.p2[2], momset_.p2[3]);
  HepMC::GenParticle* p2 = new HepMC::GenParticle(pp2, 11, 1);
  v->add_particle_out(p2);
  HepMC::FourVector pq2(momset_.q2[0], momset_.q2[1], momset_.q2[2], momset_.q2[3]);
  HepMC::GenParticle* q2 = new HepMC::GenParticle(pq2, -11, 1);
  v->add_particle_out(q2);
  //now add photons
  for (int i = 0; i < momset_.nphot; ++i){
    HepMC::FourVector pg(momset_.phot[0][i], momset_.phot[1][i], momset_.phot[2][i], momset_.phot[3][i]);
    HepMC::GenParticle* g = new HepMC::GenParticle(pg, 22, 1);
    v->add_particle_out(g);
  }  

  evt->weights().push_back(wgtall_.wtmod);
  return evt;
}
//GETTER for the external random numbers
//DECLARE_GETTER(CMS_SHERPA_RNG_Getter,"CMS_SHERPA_RNG",ATOOLS::External_RNG,ATOOLS::RNG_Key);

//ATOOLS::External_RNG *CMS_SHERPA_RNG_Getter::operator()(const ATOOLS::RNG_Key &) const
//{ return new CMS_SHERPA_RNG(); }

//void CMS_SHERPA_RNG_Getter::PrintInfo(std::ostream &str,const size_t) const
//{ str<<"CMS_SHERPA_RNG interface"; }

//double CMS_SHERPA_RNG::Get(){
//   return randomEngine->flat();
//   }
   
#include "GeneratorInterface/ExternalDecays/interface/ExternalDecayDriver.h"

typedef edm::GeneratorFilter<BHLUMIHadronizer, gen::ExternalDecayDriver> BHLUMIGeneratorFilter;
DEFINE_FWK_MODULE(BHLUMIGeneratorFilter);
