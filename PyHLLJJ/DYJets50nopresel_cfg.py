import CMGTools.RootTools.fwlite.Config as cfg
from HiggsAna.PyHLLJJ.analhjjll_cff import *


DYJets50nopresel = cfg.MCComponent(
    name = 'DYJets50nopresel',
    files = [
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/none/DYJetsMadgraph-v2/cmgTuple_0.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/none/DYJetsMadgraph-v2/cmgTuple_1.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/none/DYJetsMadgraph-v2/cmgTuple_10.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/none/DYJetsMadgraph-v2/cmgTuple_100.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/none/DYJetsMadgraph-v2/cmgTuple_101.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/none/DYJetsMadgraph-v2/cmgTuple_102.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/none/DYJetsMadgraph-v2/cmgTuple_103.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/none/DYJetsMadgraph-v2/cmgTuple_104.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/none/DYJetsMadgraph-v2/cmgTuple_105.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/none/DYJetsMadgraph-v2/cmgTuple_106.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/none/DYJetsMadgraph-v2/cmgTuple_107.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/none/DYJetsMadgraph-v2/cmgTuple_108.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/none/DYJetsMadgraph-v2/cmgTuple_109.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/none/DYJetsMadgraph-v2/cmgTuple_11.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/none/DYJetsMadgraph-v2/cmgTuple_110.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/none/DYJetsMadgraph-v2/cmgTuple_111.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/none/DYJetsMadgraph-v2/cmgTuple_112.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/none/DYJetsMadgraph-v2/cmgTuple_113.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/none/DYJetsMadgraph-v2/cmgTuple_114.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/none/DYJetsMadgraph-v2/cmgTuple_115.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/none/DYJetsMadgraph-v2/cmgTuple_116.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/none/DYJetsMadgraph-v2/cmgTuple_117.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/none/DYJetsMadgraph-v2/cmgTuple_118.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/none/DYJetsMadgraph-v2/cmgTuple_119.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/none/DYJetsMadgraph-v2/cmgTuple_12.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/none/DYJetsMadgraph-v2/cmgTuple_120.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/none/DYJetsMadgraph-v2/cmgTuple_121.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/none/DYJetsMadgraph-v2/cmgTuple_122.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/none/DYJetsMadgraph-v2/cmgTuple_123.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/none/DYJetsMadgraph-v2/cmgTuple_124.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/none/DYJetsMadgraph-v2/cmgTuple_125.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/none/DYJetsMadgraph-v2/cmgTuple_126.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/none/DYJetsMadgraph-v2/cmgTuple_127.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/none/DYJetsMadgraph-v2/cmgTuple_128.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/none/DYJetsMadgraph-v2/cmgTuple_129.root',
],
    
    xSection = 0.00114477 * 11050000, 
    nGenEvents = 6972564, # dummy 
    triggers = [],
    intLumi = 1000,
    effCorrFactor = 1 )

selectedComponents = [DYJets50nopresel]

DYJets50nopresel.splitFactor = 1
    
config = cfg.Config( components = selectedComponents,
                     sequence = sequence )
