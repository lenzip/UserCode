for num in 400 425 450 475 500 525 550 575 600
do

    filename=VBFH${num}V6presel4_VBF30_cfg.py
    echo "Creating ${filename}"

    echo "import CMGTools.RootTools.fwlite.Config as cfg"       >  $filename
    echo "from HiggsAna.PyHLLJJHighMass.analhjjll_cff import *" >> $filename
    echo "hjjllAna.jetptmin = 30."                              >> $filename
    echo "hjjllAna.minjets = 4."                                >> $filename
    echo "VBFH${num} = cfg.MCComponent("                        >> $filename
    echo "name = 'VBFH${num}',"                                 >> $filename
    echo "files = ["                                            >> $filename
    cmsLs "/store/user/lenzip/CMG/VBF_HToZZTo2L2Q_M-${num}_8TeV-powheg-pythia6/Summer12_DR53X-PU_S10_START53_V7A-v1/AODSIM/CMGV6" | grep cmgTuple | awk '{print "\x27root://eoscms//eos/cms"$5"\x27,"}' >> $filename
    echo "]," >> $filename
    echo "xSection = 0.00114477 * 11050000," >> $filename
    echo "nGenEvents = 6972564, # dummy "    >> $filename
    echo "triggers = [],"                    >> $filename
    echo "intLumi = 1000,"                   >> $filename 
    echo "effCorrFactor = 1 )"               >> $filename
    echo "selectedComponents = [VBFH${num}]" >> $filename
    echo "VBFH${num}.splitFactor = 1"        >> $filename

    echo "config = cfg.Config( components = selectedComponents, sequence = sequence )" >> $filename

#    echo "Running...."
#    python  ../../CMGTools/RootTools/python/fwlite/MultiLoop.py V6_vbf $filename -N 100000 -p 0 -f     
done