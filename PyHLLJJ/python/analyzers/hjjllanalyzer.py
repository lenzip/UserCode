import operator 
import itertools
import copy

from ROOT import TLorentzVector

from CMGTools.RootTools.fwlite.Analyzer import Analyzer
from CMGTools.RootTools.fwlite.Event import Event
from CMGTools.RootTools.statistics.Counter import Counter, Counters
from CMGTools.RootTools.fwlite.AutoHandle import AutoHandle
from CMGTools.RootTools.physicsobjects.PhysicsObjects import Jet, GenParticle,Electron,Muon
from FWCore.ParameterSet.Types import InputTag 

from CMGTools.RootTools.utils.DeltaR import deltaR, deltaPhi
from math import pi, sqrt, acos
from sets import Set
import numpy

#from HiggsAna.PyHLLJJ.kinfitters import DiJetKinFitter

standardsteps = ['All events', 
                 'ptl1>10, ptl2>10', 
                 'ptl1>10, ptl2>10 with id', 
                 'ptl1>10, ptl2>10 with id and iso',
                 '2 jets pt>15',
                 '2 jets pt>15 and id',
                 '50<mjj<140']
        
class hjjllanalyzer( Analyzer ):

    def declareHandles(self):
        super(hjjllanalyzer, self).declareHandles()
        self.handles['jets'] = AutoHandle ('cmgJet2L2Q',
                                           'std::vector<cmg::PFJet>')
        self.mchandles['genParticles'] = AutoHandle( 'genParticles',
                                                     'std::vector<reco::GenParticle>' )
        self.handles['allmuons'] = AutoHandle( 'cmgMuon2L2Q', 'std::vector<cmg::Muon>' )                      
        self.handles['muons'] = AutoHandle( 'muonPresel',
                                            'std::vector<cmg::Muon>' )
        self.handles['allelectrons'] = AutoHandle( 'cmgElectron2L2Q', 'std::vector<cmg::Electron>' )    
        self.handles['electrons'] = AutoHandle( 'electronPresel',
                                                'std::vector<cmg::Electron>' )
        self.handles['trigger'] = AutoHandle(('TriggerResults', "", "HLT"), 'edm::TriggerResults')                                           
        self.handles['rho'] = AutoHandle(("kt6PFJetsForIso","rho"), "double")
        self.handles['hmumujj'] = AutoHandle('cmgHiggsSelKinFitMu', 'vector<cmg::HiggsCandidate<cmg::DiObject<cmg::Muon,cmg::Muon>,cmg::DiObject<cmg::PFJet,cmg::PFJet> > >')
        #self.handles['hmumujjnofit'] = AutoHandle('cmgHiggsSelMu', 'vector<cmg::HiggsCandidate<cmg::DiObject<cmg::Muon,cmg::Muon>,cmg::DiObject<cmg::PFJet,cmg::PFJet> > >')

        self.handles['heejj'] = AutoHandle('cmgHiggsSelKinFitEle', 'vector<cmg::HiggsCandidate<cmg::DiObject<cmg::Electron,cmg::Electron>,cmg::DiObject<cmg::PFJet,cmg::PFJet> > >')
        #self.handles['heejjnofit'] = AutoHandle('cmgHiggsSelEle', 'vector<cmg::HiggsCandidate<cmg::DiObject<cmg::Electron,cmg::Electron>,cmg::DiObject<cmg::PFJet,cmg::PFJet> > >')

        self.handles['PUweight'] = AutoHandle('vertexWeightSummer12MC53XHCPData', 'double')

        self.handles['vertices'] = AutoHandle('goodOfflinePrimaryVertices', "vector<reco::Vertex>" )

        
        #self.handles['pf'] = AutoHandle ('particleFlow',
        #                                 'std::vector<reco::PFCandidate>')

    def buildMCinfo(self, event):
        self.isHZZ = False
        self.zlep = [] 
        self.zhad = []
        self.vbfjets = [] 

        for ptc in self.mchandles['genParticles'].product():
            if ptc.status() != 3 :
              #print ptc.status()
              continue
            if abs(ptc.pdgId()) < 6 and ptc.pt()>2:
              isfromZ = False
              for i in range(ptc.numberOfMothers()):
                if ptc.mother(i).pdgId() == 23:
                  isfromZ = True
                  break
              if not isfromZ :
                hasstatus3dau = False
                for i in range(ptc.numberOfDaughters()):
                  if ptc.daughter(i).status()==3:
                    hasstatus3dau = True
                    break
                if hasstatus3dau:    
                  self.vbfjets.append(ptc)
                elif not (ptc.mother(0).status() == 3 and ptc.mother(0).pt()>2):
                  self.vbfjets.append(ptc)
            if abs(ptc.pdgId())==25:
              #print "Higgs"
              ndau = ptc.numberOfDaughters()
              #print "ndau=",ndau
              #for i in range(ndau):
              #  print "dauid=",ptc.daughter(i).pdgId()
              #for some reason the third daughter is another H  
              if ndau == 3:
                if ptc.daughter(0).pdgId() ==23 and ptc.daughter(1).pdgId() ==23:
                  if abs(ptc.daughter(0).daughter(0).pdgId())>=11 and abs(ptc.daughter(0).daughter(0).pdgId())<=16:
                    self.zlep.append(ptc.daughter(0))
                    self.zhad.append(ptc.daughter(1))
                  else :
                    self.zlep.append(ptc.daughter(1))
                    self.zhad.append(ptc.daughter(0))
                  self.isHZZ = True
                  #print "HZZ!"

    def addStandardCounter(self,name):
        self.counters.addCounter(name)
        counter = self.counters.counter(name)
        for step in standardsteps:
          counter.register(step)

    def fillStandardStep(self, basecounter, step, mass, weight):
        self.counters.counter(basecounter).inc(standardsteps[step], weight)
        if mass > 12. and mass < 75.:
          self.counters.counter(basecounter+'_lowmass').inc(standardsteps[step], weight)
        elif mass > 75.:
          self.counters.counter(basecounter+'_highmass').inc(standardsteps[step], weight)
  
    def beginLoop(self):
        super(hjjllanalyzer,self).beginLoop()
        self.counters.addCounter('h_gen')
        count = self.counters.counter('h_gen')
        count.register('All events')
        count.register('All h events')
        count.register('h->jjll')

        self.addStandardCounter('countall')
        self.addStandardCounter('countall_lowmass')
        self.addStandardCounter('countall_highmass')
        
        self.addStandardCounter('countmuon')
        self.addStandardCounter('countmuon_lowmass')
        self.addStandardCounter('countmuon_highmass')

        self.addStandardCounter('countelectron')
        self.addStandardCounter('countelectron_lowmass')
        self.addStandardCounter('countelectron_highmass')

    def isEMCmatched(self,lepton):
        #return lepton.getSelection("cuts_genLepton") 
        return lepton.sourcePtr().get().hasOverlaps("genLeptons") 
    
    def isMuMCmatched(self,lepton):
        return lepton.sourcePtr().get().hasOverlaps("genLeptons") 


    def process(self, iEvent, event):
        self.readCollections( iEvent )

        self.buildMCinfo(iEvent)

        eventNumber = iEvent.eventAuxiliary().id().event()
        myEvent = Event(event.iEv)
        setattr(event, self.name, myEvent)
        event = myEvent
        event.genVBFdeltaPhi = -99.
        #if len(self.vbfjets) != 2:
        #  print "Warning: ",len(self.vbfjets)," VBF partons"
        #if len(self.vbfjets) > 2:
        #  event.genVBFdeltaPhi = deltaPhi(self.vbfjets[0].phi(), self.vbfjets[1].phi())
        event.step=0  
        event.alljets = []
        event.highptjets = []          
        event.leadingmuons = []
        event.highptmuons = []
        event.highptelectrons = []
        event.leadingelectrons = []
        event.dimuonmass = -1
        event.dielectronmass = -1
        event.deltaeta = -1;
        event.deltaphi = -99;
        event.mjj = -1;
        event.ht = -1
        event.iszmumu = False
        event.iszee = False
        event.mmumu = -99
        event.mee = -99
        event.myweight = 1.
        event.nvertices = -1
        event.vertices = []
        event.rho = -99
        event.truezlepmass= -1
        event.truezlep = []
        event.truezhad = []
        event.dielectronTrigger  = -1
        event.dielectronHtTrigger  = -1
        event.dimuonTrigger = -1
        event.dimuonHtTrigger = -1
        #higgs candidates
        event.hmumujj = []
        event.heejj = []
        event.mumu = []
        event.ee = []
        event.jj = []
        event.hbest = []
        event.deltaPhiLJ = []
        event.deltaPhiJJ  = -1
        event.deltaPhiZJ1 = -1
        event.deltaPhiZJ2 = -1
        event.deltaPhiZJ = [] 


        if not self.handles['electrons'].isValid():
          #print "invalid collection!"
          return
     
        if not self.handles['PUweight'].isValid():
          return
        puw = self.handles['PUweight'].product()
        event.myweight = event.myweight*puw[0] 
        if not self.handles['vertices'].isValid():
          return
        event.nvertices=self.handles['vertices'].product().size()  
        for vertex in self.handles['vertices'].product():
            if vertex.isValid() and vertex.ndof()>4:
              event.vertices.append(vertex)
        def getSumPt(v):
          sumpt = 0
          #TODO NON RIESCO a loopare sugli iteratori del c++  
          #for t in v.tracks_:
          #  print t.pt()
          #  sumpt += t.pt()
          return sumpt
        event.vertices.sort(key=lambda a: getSumPt(a), reverse=True)       
 
        if not self.handles['rho'].isValid():
          return
        event.rho = self.handles['rho'].product()[0]  
          
########################BEGIN SELECTION PART        
        self.counters.counter('countall').inc('All events', event.myweight)
        self.counters.counter('countall_lowmass').inc('All events', event.myweight)
        self.counters.counter('countall_highmass').inc('All events', event.myweight)
        self.counters.counter('countelectron').inc('All events', event.myweight)
        self.counters.counter('countelectron_lowmass').inc('All events', event.myweight)
        self.counters.counter('countelectron_highmass').inc('All events', event.myweight)
        self.counters.counter('countmuon').inc('All events', event.myweight)
        self.counters.counter('countmuon_lowmass').inc('All events', event.myweight)
        self.counters.counter('countmuon_highmass').inc('All events', event.myweight)

        if len(self.zlep) != 1:
          print "problem with true zlep, number of daughters is",len(self.zlep)
          return
        if abs(self.zlep[0].daughter(0).pdgId())==11:
          event.iszee = True
        elif abs(self.zlep[0].daughter(0).pdgId())==13:
          event.iszmumu = True

        event.truezlep = self.zlep
        event.truezhad = self.zhad
        
        event.truezlepmass = self.zlep[0].p4().mass()

        #iEvent is of type ChainEvent

        trigger = iEvent.object().triggerResultsByName('HLT') #self.handles['trigger'].product()
        #trignames = iEvent.object().triggerNames(self.handles['trigger'].product())
        #for i in range(self.handles['trigger'].product().size()):
        #    print trignames.triggerName(i)
            
        event.dimuonTrigger = 1 if trigger.accept('HLT_Mu13_Mu8_v17') else 0
        event.dimuonHtTrigger = 1 if trigger.accept('HLT_DoubleMu8_Mass8_PFHT175_v6') else 0
        event.dielectronTrigger = 1 if trigger.accept('HLT_Ele17_CaloIdT_CaloIsoVL_TrkIdVL_TrkIsoVL_Ele8_CaloIdT_CaloIsoVL_TrkIdVL_TrkIsoVL_v17')  else 0
        event.dielectronHtTrigger = 1 if trigger.accept('HLT_DoubleEle8_CaloIdT_TrkIdVL_Mass8_PFHT175_v6') else 0

        # 2 leptons, pt1>10., pt2 > 5
        for electron in self.handles['allelectrons'].product():
          event.leadingelectrons.append(Electron(electron))
          if electron.pt()>10.:
            event.highptelectrons.append(Electron(electron))

        event.leadingelectrons.sort(key=lambda a: a.pt(), reverse = True)
        event.highptelectrons.sort(key=lambda a: a.pt(), reverse = True)

        for muon in self.handles['allmuons'].product():
          event.leadingmuons.append(Muon(muon))
          if muon.pt()>10.:
            event.highptmuons.append(Muon(muon))
            
        event.leadingmuons.sort(key=lambda a: a.pt(), reverse = True)
        event.highptmuons.sort(key=lambda a: a.pt(), reverse = True)

        if ( (len(event.highptmuons)>1 and not (event.highptmuons[0].pt()<10)) and 
            self.isMuMCmatched(event.highptmuons[0]) and self.isMuMCmatched(event.highptmuons[1])) :
          event.step += 1
          self.fillStandardStep('countall',event.step, event.truezlepmass, event.myweight)
          self.fillStandardStep('countmuon',event.step, event.truezlepmass, event.myweight)

        elif ( (len(event.highptelectrons)>1 and not (event.highptelectrons[0].pt()<10)) and 
              self.isEMCmatched(event.highptelectrons[0]) and self.isEMCmatched(event.highptelectrons[1]) ):
          event.step += 1
          self.fillStandardStep('countall',event.step, event.truezlepmass, event.myweight)
          self.fillStandardStep('countelectron',event.step, event.truezlepmass, event.myweight)
        else:
          return  
          

        # 2 leptons, pt1>10., pt2 > 5 with id
        event.leadingelectrons_id = []
        for electron in event.leadingelectrons:
          if electron.getSelection("cuts_kinematics") and \
             (electron.getSelection("cuts_cutBasedLoose_eidEE") or \
              electron.getSelection("cuts_cutBasedLoose_eidEB") ) and \
             electron.getSelection("cuts_HLTPatch") :
            event.leadingelectrons_id.append(Electron(electron))
        
        event.leadingmuons_id = []
        for muon in event.leadingmuons:
          if muon.getSelection("cuts_kinematics") and muon.getSelection("cuts_tightPFmuon"):
            event.leadingmuons_id.append(Muon(muon))

        if (len(event.leadingmuons_id)>1 and not event.leadingmuons_id[0].pt < 10 and 
            self.isMuMCmatched(event.leadingmuons_id[0]) and self.isMuMCmatched(event.leadingmuons_id[1]) ):
          event.step += 1
          self.fillStandardStep('countall',event.step, event.truezlepmass, event.myweight)
          self.fillStandardStep('countmuon',event.step, event.truezlepmass, event.myweight)

        elif  (len(event.leadingelectrons_id)>1 and not event.leadingelectrons_id[0].pt < 10 and
              self.isEMCmatched(event.leadingelectrons_id[0]) and self.isEMCmatched(event.leadingelectrons_id[1])):
          event.step += 1
          self.fillStandardStep('countall',event.step, event.truezlepmass, event.myweight)
          self.fillStandardStep('countelectron',event.step, event.truezlepmass, event.myweight)
        else:
          return
        
        # 2 leptons, pt1>10., pt2 > 5 with id and iso
        # N.B. now access muonPresel and electronPresel
        event.preselElectrons = []
        for electron in self.handles['electrons'].product():
          if electron.pt()>10.:
            event.preselElectrons.append(Electron(electron))

        event.preselMuons = []
        for muon in self.handles['muons'].product():
          if muon.pt() > 10:
            event.preselMuons.append(Muon(muon))
            

        if (len(event.preselMuons)>1 and not event.preselMuons[0].pt < 10 and 
            self.isMuMCmatched(event.preselMuons[0]) and self.isMuMCmatched(event.preselMuons[1])):
          event.step += 1
          self.fillStandardStep('countall',event.step, event.truezlepmass, event.myweight)
          self.fillStandardStep('countmuon',event.step, event.truezlepmass, event.myweight)

        elif  (len(event.preselElectrons)>1 and not event.preselElectrons[0].pt < 10 and 
              self.isEMCmatched(event.preselElectrons[0]) and self.isEMCmatched(event.preselElectrons[1])):
          event.step += 1
          self.fillStandardStep('countall',event.step, event.truezlepmass, event.myweight)
          self.fillStandardStep('countelectron',event.step, event.truezlepmass, event.myweight)
        else:
          return
 
        # now dilepton mass requirement
#        event.dimuons = []
#        for mu1 in range(len(event.preselMuons)):
#          for mu2 in range(mu1,len(event.preselMuons)):
#             dimu = event.preselMuons[mu1].p4() + event.preselMuons[mu2].p4() 
#             if dimu.mass()>12. and dimu.mass()<75. and event.preselMuons[mu1].pt() > 10.:
#               event.dimuons.append(dimu)
#
#        event.dielectrons = []
#        for e1 in range(len(event.preselElectrons)):
#          for e2 in range(e1,len(event.preselElectrons)):
#             die = event.preselElectrons[e1].p4() + event.preselElectrons[e2].p4()       
#             if die.mass()>12. and die.mass()<75. and event.preselElectrons[e1].pt() > 10.:
#               event.dielectrons.append(die)  

        #2 jets with pt > 15
        #event.highptjets = []
        if not self.handles['jets'].isValid():
            return
            #print "no valid jet collection"
        for jet in self.handles['jets'].product():
          if jet.pt()>15.:
            event.highptjets.append(Jet(jet))
            #print "jet pt: ", jet.pt()
        if len(event.highptjets)>1:
          event.step += 1
          self.fillStandardStep('countall',event.step, event.truezlepmass, event.myweight)
          self.fillStandardStep('countelectron',event.step, event.truezlepmass, event.myweight)
        else:
          return

        # 2 jets with id
        event.highptjets_id = []
        for jet in event.highptjets:
          if jet.getSelection("cuts_jetKinematics") and jet.getSelection("cuts_looseJetId"):
            event.highptjets_id.append(Jet(jet))
       
        if len(event.highptjets_id)>1:
          event.step += 1
          self.fillStandardStep('countall',event.step, event.truezlepmass, event.myweight)
          self.fillStandardStep('countelectron',event.step, event.truezlepmass, event.myweight)
        else:
          return

        # 2 jets with id and invariant mass
        event.dijets = []
        for jet1 in range(len(event.highptjets_id)):
          for jet2 in range(jet1, len(event.highptjets_id)):
            dijet = event.highptjets_id[jet1].p4() +event.highptjets_id[jet2].p4()
            if dijet.mass() > 50 and dijet.mass()<140:
              event.dijets.append(dijet)

        if len(event.dijets):
          event.step += 1
          self.fillStandardStep('countall',event.step, event.truezlepmass, event.myweight)
          self.fillStandardStep('countelectron',event.step, event.truezlepmass, event.myweight)
        else:   
          return
################END SELECTION PART
        
        triggerjets = []
        for jet in event.highptjets:
          if jet.pt() > 20.:
            triggerjets.append(Jet(jet))

        for jet in triggerjets:
            event.ht = event.ht + jet.pt()

        #sort triggerjets in rapidity
        triggerjets.sort(key=lambda a: a.rapidity(), reverse = True )
        if ( len(triggerjets)>=2 ):
          event.deltaeta = triggerjets[0].rapidity() - triggerjets[len(triggerjets)-1].rapidity()
          event.deltaphi = deltaPhi(triggerjets[0].phi(), triggerjets[len(triggerjets)-1].phi())
          event.mjj = ( triggerjets[0].p4() + triggerjets[len(triggerjets)-1].p4() ).mass()
         

        if self.handles['hmumujj'].isValid() and len(self.handles['hmumujj'].product()) > 0:
          hmumujj = self.handles['hmumujj'].product()[0]
          event.hmumujj.append(hmumujj)
          event.mumu.append(hmumujj.leg1().leg1())           
          #event.mumu.append(hmumujj.leg1())
          event.jj.append(hmumujj.leg2())

        if self.handles['heejj'].isValid() and len(self.handles['heejj'].product()) > 0:
          if len(event.hmumujj) != 0:
            print "WARNING! found and heejj when and hmumujj is already present"
          heejj = self.handles['heejj'].product()[0]
          event.heejj.append(heejj)
          #uncomment to look at electron from Higgs and not dielectron
          event.ee.append(heejj.leg1().leg1())
          #event.ee.append(heejj.leg1())
          event.jj.append(heejj.leg2())
  
        if len(event.hmumujj):
          event.hbest.append(hmumujj)
          event.deltaPhiLJ.append( abs( deltaPhi(event.hbest[0].leg1().leg1().phi(), 
                                      event.hbest[0].leg2().leg1().phi()) ) )
          event.deltaPhiLJ.append( abs( deltaPhi(event.hbest[0].leg1().leg1().phi(), 
                                      event.hbest[0].leg2().leg2().phi()) ) )
          event.deltaPhiLJ.append( abs( deltaPhi(event.hbest[0].leg1().leg2().phi(), 
                                      event.hbest[0].leg2().leg1().phi()) ) )
          event.deltaPhiLJ.append( abs( deltaPhi(event.hbest[0].leg1().leg2().phi(), 
                                      event.hbest[0].leg2().leg2().phi()) ) )
          event.deltaPhiZJ.append( abs( deltaPhi(event.hbest[0].leg1().phi(), 
                                      event.hbest[0].leg2().leg1().phi()) ) )
          event.deltaPhiZJ.append( abs( deltaPhi(event.hbest[0].leg1().phi(), 
                                      event.hbest[0].leg2().leg2().phi()) ) )
          if event.hbest[0].leg2().leg1().pt() > event.hbest[0].leg2().leg2().pt():
              event.deltaPhiZJ1 = abs( deltaPhi(event.hbest[0].leg1().phi(), 
                                      event.hbest[0].leg2().leg1().phi()) )
              event.deltaPhiZJ2= abs( deltaPhi(event.hbest[0].leg1().phi(), 
                                      event.hbest[0].leg2().leg2().phi()) )
          else:
              event.deltaPhiZJ1= abs( deltaPhi(event.hbest[0].leg1().phi(), 
                                    event.hbest[0].leg2().leg2().phi()) )
              event.deltaPhiZJ2= abs( deltaPhi(event.hbest[0].leg1().phi(), 
                                    event.hbest[0].leg2().leg1().phi()) )


        event.deltaPhiLJ.sort()
        event.deltaPhiZJ.sort()      

            

        
