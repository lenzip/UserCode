import CMGTools.RootTools.fwlite.Config as cfg
from HiggsAna.PyHLLJJ.analhjjll_cff import *


DYJets50 = cfg.MCComponent(
    name = 'DYJets50',
    files = [
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_0.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_1.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_10.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_100.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_101.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_102.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_103.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_104.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_105.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_106.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_107.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_108.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_109.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_11.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_110.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_111.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_112.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_113.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_114.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_115.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_116.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_117.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_118.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_119.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_12.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_120.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_121.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_122.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_123.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_124.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_125.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_126.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_127.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_128.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_129.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_13.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_130.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_131.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_132.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_133.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_134.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_135.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_136.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_137.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_138.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_139.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_14.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_140.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_141.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_142.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_143.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_145.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_146.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_147.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_148.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_149.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_15.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_150.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_151.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_152.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_153.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_154.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_155.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_156.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_157.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_158.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_159.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_16.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_160.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_161.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_162.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_163.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_164.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_165.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_166.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_167.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_168.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_169.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_17.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_170.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_171.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_172.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_173.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_174.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_175.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_176.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_177.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_178.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_179.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_18.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_180.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_181.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_182.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_186.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_187.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_188.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_189.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_19.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_190.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_191.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_192.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_193.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_194.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_195.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_196.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_197.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_198.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_199.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_2.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_20.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_200.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_201.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_202.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_203.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_205.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_206.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_207.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_208.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_209.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_21.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_211.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_213.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_214.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_215.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_217.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_219.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_22.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_220.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_221.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_223.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_224.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_225.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_229.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_23.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_230.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_231.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_232.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_233.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_234.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_235.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_236.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_237.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_238.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_239.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_24.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_241.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_242.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_243.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_244.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_245.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_246.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_248.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_25.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_250.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_252.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_253.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_255.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_256.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_258.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_259.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_26.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_260.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_262.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_263.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_264.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_265.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_266.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_267.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_268.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_269.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_27.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_270.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_271.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_275.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_276.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_277.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_279.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_28.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_280.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_281.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_288.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_289.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_29.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_292.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_293.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_294.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_298.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_299.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_3.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_30.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_300.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_301.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_302.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_303.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_305.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_307.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_309.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_31.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_310.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_315.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_319.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_32.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_320.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_321.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_33.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_34.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_35.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_36.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_37.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_38.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_4.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_40.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_41.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_42.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_43.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_44.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_46.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_47.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_48.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_49.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_5.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_50.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_51.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_52.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_53.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_54.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_55.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_56.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_57.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_58.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_59.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_6.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_60.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_61.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_62.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_63.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_64.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_66.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_67.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_68.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_69.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_7.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_70.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_71.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_72.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_73.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_74.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_75.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_76.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_77.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_78.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_79.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_8.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_80.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_81.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_82.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_83.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_84.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_85.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_86.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_87.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_88.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_89.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_9.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_90.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_91.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_92.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_93.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_94.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_95.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_96.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_97.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_98.root',
'root://eoscms//eos/cms/store/cmst3/user/lenzip/cmgTuple/2012/Summer12/presel/DYJetsMadgraph-v2/cmgTuple_99.root'    
    ],
    
    xSection = 0.00114477 * 11050000, 
    nGenEvents = 6972564, # dummy 
    triggers = [],
    intLumi = 1000,
    effCorrFactor = 1 )

selectedComponents = [DYJets50]

DYJets50.splitFactor = 1
    
config = cfg.Config( components = selectedComponents,
                     sequence = sequence )
