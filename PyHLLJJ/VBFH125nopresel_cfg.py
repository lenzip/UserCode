import CMGTools.RootTools.fwlite.Config as cfg
from HiggsAna.PyHLLJJ.analhjjll_cff import *


VBFH125nopresel = cfg.MCComponent(
    name = 'VBFH125nopresel',
    files = [
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/none/VBF_HToZZTo2L2Q_M_125_8TeV_powheg_pythia6_CMG/cmgTuple_0.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/none/VBF_HToZZTo2L2Q_M_125_8TeV_powheg_pythia6_CMG/cmgTuple_1.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/none/VBF_HToZZTo2L2Q_M_125_8TeV_powheg_pythia6_CMG/cmgTuple_2.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/none/VBF_HToZZTo2L2Q_M_125_8TeV_powheg_pythia6_CMG/cmgTuple_3.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/none/VBF_HToZZTo2L2Q_M_125_8TeV_powheg_pythia6_CMG/cmgTuple_4.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/none/VBF_HToZZTo2L2Q_M_125_8TeV_powheg_pythia6_CMG/cmgTuple_5.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/none/VBF_HToZZTo2L2Q_M_125_8TeV_powheg_pythia6_CMG/cmgTuple_6.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/none/VBF_HToZZTo2L2Q_M_125_8TeV_powheg_pythia6_CMG/cmgTuple_7.root'
    ],
    
    xSection = 0.00114477 * 11050000, 
    nGenEvents = 6972564, # dummy 
    triggers = [],
    intLumi = 1000,
    effCorrFactor = 1 )

selectedComponents = [VBFH125nopresel]

VBFH125nopresel.splitFactor = 1
    
config = cfg.Config( components = selectedComponents,
                     sequence = sequence )
