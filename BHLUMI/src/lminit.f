      SUBROUTINE INIT(datacard, npar,xpar)
*     **********************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
* histograms in labeled common   
      PARAMETER( PI = 3.1415926535897932D0 )
      COMMON / cglib / b(50000)
      COMMON / inout  / ninp,nout
      DIMENSION NPAR(*),XPAR(*)
      COMMON / MOMSET / P1(4),Q1(4),P2(4),Q2(4),PHOT(100,4),NPHOT
      COMMON / WGTALL / WTMOD,WTCRU1,WTCRU2,WTSET(300) 

      CHARACTER*5 tname,dname
      CHARACTER*1024 datacard
      
      SAVE

* here it starts
      CALL glimit(50000) 

!---------------
! Standard Input          
C      ninp =7
! This OPEN on unix machine is not necessary
! Read test_name and data_set_name         
! Read initial (root) random number seed          
      NINP3=10
      OPEN(NINP3,FILE=datacard)
      READ( NINP3,'(A5,1X,A5)') tname,dname
      WRITE(   6,'(A5,1X,A5)') tname,dname
C      READ(NINP3,'(I10)') ijklin
C      READ(NINP3,'(I10)') ntotin
C      READ(NINP3,'(I10)') ntot2n
C not needed, we use CMSSW rng      
C      CALL marini(ijklin,ntotin,ntot2n)
!=======================================================
      READ( NINP3,'(8I2)')   KAT1,KAT2,KAT3,KAT4,KAT5,KAT6
      READ( NINP3,'(I10)')   NEVT,KEYOPT,KEYRAD,KEYTRI
      READ( NINP3,'(F10.0)') CMSENE,TMING,TMAXG,VMAXG,XK0
      READ( NINP3,'(F10.0)') TMINW,TMAXW,TMINN,TMAXN,VMAXE  
      READ( NINP3,'(I10)')   NPHI,NTHE
      CLOSE(NINP3)
!=======================================================
! control output
      WRITE(NOUT,'(6A6/6I6)')
     $ 'KAT1','KAT2','KAT3','KAT4','KAT5','KAT6',
     $  KAT1 , KAT2 , KAT3 , KAT4 , KAT5 , KAT6
      WRITE(NOUT,'(4A12/4I12)')
     $  'NEVT','KEYRAD','KEYOPT','KEYTRI',
     $   NEVT,  KEYRAD , KEYOPT , KEYTRI
      WRITE(NOUT,'(5A12/5F12.6)')
     $ 'CMSENE','TMING','TMAXG','VMAXG','XK0',
     $  CMSENE , TMING , TMAXG , VMAXG , XK0
      WRITE(NOUT,'(5A12/5F12.6)')
     $  'TMINW','TMAXW','TMINN','TMAXN','VMAXE',
     $   TMINW , TMAXW , TMINN , TMAXN , VMAXE
      WRITE(NOUT,'(6A12/6F12.6)')
     $  'TMING','TMAXG','TMINW','TMAXW','TMINN','TMAXN',
     $   TMING , TMAXG , TMINW , TMAXW , TMINN , TMAXN
      WRITE(NOUT,'(2A12/2I12)')
     $  'NPHI','NTHE',
     $   NPHI,  NTHE
      KEYGEN = MOD(KEYOPT,10000)/1000
C      Write(*,*) "GIULIO KeyGEN",KEYGEN
      IF(KEYGEN.EQ.3) THEN
! input data for --- BHLUM2 ---
! Born limiting values for the transfer
       TRMINB =CMSENE**2*(1D0-COS(TMING))/2D0
       TRMAXB =CMSENE**2*(1D0-COS(TMAXG))/2D0
       TRMIN = TRMINB
       TRMAX = TRMAXB
       EPSCM = XK0
       NPAR(1)=KEYOPT
       NPAR(2)=KEYRAD
C       write(*,*) "Giulio KEYRAD",NPAR(2)
       XPAR(1)=CMSENE
       XPAR(2)=TRMIN
       XPAR(3)=TRMAX
       XPAR(4)=EPSCM
      ELSEIF(KEYGEN.EQ.2) THEN
! input data for --- LUMLOG ---
       NPAR(1) = KEYOPT
       NPAR(2) = KEYRAD
       XPAR(1) = CMSENE
       XPAR(2) = TMING*180/PI
       XPAR(3) = TMAXG*180/PI
       XPAR(4) = XK0
       XPAR(5) = VMAXG
      ELSEIF(KEYGEN.EQ.1) THEN 
! input data for --- OLDBIS ---
       NPAR(1)= KEYOPT
       NPAR(2)= KEYRAD
       XPAR(1)= CMSENE
       XPAR(2)= TMING*180/PI
       XPAR(3)= TMAXG*180/PI
       XPAR(4)= XK0
       XPAR(5)= VMAXG
       XPAR(6)= 0D0
      ELSE
       WRITE(6,*) '++++ WRONG KEYOPT=',KEYOPT
      STOP
      ENDIF

      END
