      INTEGER MDCY,MDME,KFDP
      PARAMETER (L1MST=200, L1PAR=200)
      PARAMETER (L2PAR=500, L2PARF=2000 )
      PARAMETER (LJNPAR=4000)
      COMMON /PYDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)
      COMMON /PYDAT2/ KCHG(L2PAR,3),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)
      COMMON /PYDAT3/ MDCY(500,3),MDME(8000,2),BRAT(8000),
     &                KFDP(8000,5)
      COMMON /PYDAT4/ CHAF(L2PAR*2)
      CHARACTER*16 CHAF
      COMMON /PYJETS/ N7LU,NPAD,K7LU(LJNPAR,5),P7LU(LJNPAR,5),
     &                V7LU(LJNPAR,5)
C
