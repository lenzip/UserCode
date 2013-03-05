from CMGTools.RootTools.analyzers.TreeAnalyzer import TreeAnalyzer
import math

class hjjlltreeproducerEff( TreeAnalyzer ):
    '''Tree producer for the HZ, H->tt  analysis.'''

    def declareVariables(self):

        def var( varName ):
            self.tree.addVar('float', varName)
        def boolVar(varName ):
            self.tree.addVar('int', varName)

        def electronVars( pName ):
            var('{pName}Mass'.format(pName=pName))
            var('{pName}Pt'.format(pName=pName))
            var('{pName}Energy'.format(pName=pName))
            var('{pName}Eta'.format(pName=pName))
            var('{pName}Phi'.format(pName=pName))
            var('{pName}Charge'.format(pName=pName))
            var('{pName}SigmaIetaIeta'.format(pName=pName))
            var('{pName}DeltaPhiSC'.format(pName=pName))
            var('{pName}DeltaEtaSC'.format(pName=pName))
            var('{pName}HoverE'.format(pName=pName))
            var('{pName}P'.format(pName=pName))
            var('{pName}IsEB'.format(pName=pName))
            var('{pName}IsEE'.format(pName=pName))
            boolVar('{pName}Kinematics'.format(pName=pName))
            boolVar('{pName}Id'.format(pName=pName))
            boolVar('{pName}mvaId'.format(pName=pName))
            boolVar('{pName}Iso'.format(pName=pName))
            boolVar('{pName}GenMatch'.format(pName=pName))
            
        def muonVars( pName ):
            var('{pName}Mass'.format(pName=pName))
            var('{pName}Pt'.format(pName=pName))
            var('{pName}Energy'.format(pName=pName))
            var('{pName}Eta'.format(pName=pName))
            var('{pName}Phi'.format(pName=pName))
            var('{pName}Charge'.format(pName=pName))
            boolVar('{pName}Kinematics'.format(pName=pName))
            boolVar('{pName}Id'.format(pName=pName))
            boolVar('{pName}mvaId'.format(pName=pName))
            boolVar('{pName}Iso'.format(pName=pName))
            boolVar('{pName}GenMatch'.format(pName=pName)) 


            
        muonVars('murec')
        electronVars('erec')

        var('weight')
        var('iszee')
        var('iszmumu')
        var('truezlepmass')

        self.tree.book()


    def process(self, iEvent, event):

        def fill( varName, value ):
            setattr( self.tree.s, varName, value )

        def computeCorrectedIso(particle, EAvals, EAetaBins, rho):
            chIso=particle.chargedHadronIso() #only charged hadrons
            chAllIso=particle.chargedAllIso() #ch hadrons + ele + mu
            nhIso=particle.neutralHadronIso()
            phIso=particle.photonIso()

            uncorr_iso=chIso+nhIso+phIso
            uncorr_reliso=uncorr_iso/particle.pt()

            etaLept=abs(particle.eta())
            binIndex=-1
            #loop over eta bins for finding the right one
            for i in range(len(EAetaBins)-1):
              if(etaLept>=EAetaBins[i] and etaLept<EAetaBins[i+1]):
                binIndex=i;
            effArea=0.0;
            if(binIndex>=0):    
              effArea=EAvals[binIndex];

            corr_nhIso=(nhIso+phIso)-effArea*rho;
            if (corr_nhIso<0.0):
              corr_nhIso=0.0;
            corr_iso=chIso+corr_nhIso;
            corr_reliso=corr_iso/particle.pt(); 
            return corr_reliso

        def fMuonVars( pName, particle, rho):
            fill('{pName}Mass'.format(pName=pName), particle.mass() )
            fill('{pName}Pt'.format(pName=pName), particle.pt() )
            fill('{pName}Energy'.format(pName=pName), particle.energy() )
            fill('{pName}Eta'.format(pName=pName), particle.eta() )
            fill('{pName}Phi'.format(pName=pName), particle.phi() )
            fill('{pName}Charge'.format(pName=pName), particle.charge() )
            if particle.pt() > 10.:
              fill('{pName}Kinematics'.format(pName=pName), True)
            else :
              fill('{pName}Kinematics'.format(pName=pName), False )
            if particle.getSelection("cuts_kinematics") and particle.getSelection("cuts_tightPFmuon"):
              fill('{pName}Id'.format(pName=pName), True)
            else:
              fill('{pName}Id'.format(pName=pName), False)

            EAvals = [0.674, 0.565, 0.442, 0.515, 0.821, 0.66, 0.0]
            relisocut = 0.12
            EAetaBins = [0.0, 1.0, 1.5, 2.0, 2.2, 2.3, 2.4, 999.0]
            
            if computeCorrectedIso(particle, EAvals, EAetaBins, rho) < relisocut:
              fill('{pName}Iso'.format(pName=pName), True)
            else:
              fill('{pName}Iso'.format(pName=pName), False)

            if particle.sourcePtr().get().hasOverlaps("genLeptons") :
              fill('{pName}GenMatch'.format(pName=pName), True)
            else:
              fill('{pName}GenMatch'.format(pName=pName), False)
            
        def fElectronVars( pName, particle, rho ):
            fill('{pName}Mass'.format(pName=pName), particle.mass() )
            fill('{pName}Pt'.format(pName=pName), particle.pt() )
            fill('{pName}Energy'.format(pName=pName), particle.energy() )
            fill('{pName}Eta'.format(pName=pName), particle.eta() )
            fill('{pName}Phi'.format(pName=pName), particle.phi() )
            fill('{pName}Charge'.format(pName=pName), particle.charge() )
            fill('{pName}SigmaIetaIeta'.format(pName=pName), particle.sigmaIetaIeta() )
            fill('{pName}DeltaPhiSC'.format(pName=pName), particle.deltaPhiSuperClusterTrackAtVtx() )
            fill('{pName}DeltaEtaSC'.format(pName=pName), particle.deltaEtaSuperClusterTrackAtVtx() )
            fill('{pName}HoverE'.format(pName=pName), particle.hadronicOverEm() )
            #fill('{pName}Dxy'.format(pName=pName), particle.dxy() )
            #fill('{pName}Dz'.format(pName=pName), particle.dz() )
            fill('{pName}P'.format(pName=pName), particle.sourcePtr().eSuperClusterOverP() )
            if particle.pt() > 10.:
              fill('{pName}Kinematics'.format(pName=pName), True)
            else :
              fill('{pName}Kinematics'.format(pName=pName), False )
            if particle.getSelection("cuts_kinematics") and \
              (particle.getSelection("cuts_cutBasedLoose_eidEE") or \
               particle.getSelection("cuts_cutBasedLoose_eidEB") ) and \
              particle.getSelection("cuts_HLTPatch") :
              fill('{pName}Id'.format(pName=pName), True)
            else:
              fill('{pName}Id'.format(pName=pName), False)
            if ( ( particle.getSelection('cuts_premvaTrig') and particle.sourcePtr().electronID("mvaTrigV0")>0.01 ) ) : #or
           #    (  not particle.getSelection('cuts_premvaTrig') and particle.sourcePtr().electronID("mvaNonTrigV0")>0.01) ):
              fill('{pName}mvaId'.format(pName=pName), True)
            else:
              fill('{pName}mvaId'.format(pName=pName), False)


            EAvals = [0.1, 0.12, 0.085, 0.11, 0.12, 0.12, 0.13]
            relisocut = 0.15
            EAetaBins = [0.0, 1.0, 1.479, 2.0, 2.2, 2.3, 2.4, 999.0] 

            if computeCorrectedIso(particle, EAvals, EAetaBins, rho) < relisocut:
              fill('{pName}Iso'.format(pName=pName), True)
            else:
              fill('{pName}Iso'.format(pName=pName), False)    

            if particle.sourcePtr().get().hasOverlaps("genLeptons") :
              fill('{pName}GenMatch'.format(pName=pName), True)
            else:
              fill('{pName}GenMatch'.format(pName=pName), False)
   


        def fgenParticleVars( pName, particle ):
            fill('g_{pName}Mass'.format(pName=pName), particle.mass() )
            fill('g_{pName}Pt'.format(pName=pName), particle.pt() )
            fill('g_{pName}Energy'.format(pName=pName), particle.energy() )
            fill('g_{pName}Eta'.format(pName=pName), particle.eta() )
#
        subevent = getattr( event, self.cfg_ana.anaName )
        #fill('weight', subevent.myweight)
        #fill('iszee', subevent.iszee)
        #fill('iszmumu', subevent.iszmumu)
        #fill('truezlepmass', subevent.truezlepmass)
        if subevent.iszmumu:
          for muon in subevent.leadingmuons:
            fill('weight', subevent.myweight)
            fill('iszee', subevent.iszee)
            fill('iszmumu', subevent.iszmumu)
            fill('truezlepmass', subevent.truezlepmass) 
            fMuonVars('murec', muon, subevent.rho)
            self.tree.fill()
        elif subevent.iszee:
          for electron in subevent.leadingelectrons:
            fill('weight', subevent.myweight)
            fill('iszee', subevent.iszee)
            fill('iszmumu', subevent.iszmumu)
            fill('truezlepmass', subevent.truezlepmass)
            fElectronVars('erec', electron, subevent.rho)
            self.tree.fill()  
