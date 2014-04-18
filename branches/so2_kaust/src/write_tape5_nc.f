      PROGRAM WRITE_TAPE5_NC

C***************************************************
cProgram version created by: Jennifer Delamere January, 1999
c
cCOMPILE INFORMATION: F90
c
cInput: Namelist which includes beginning wavenumber,
c     ending wavenumber and the gases to be included.
c      
cOutput: Tape 5's for generating the absorption coefficients
c     which include ONLY the major gases included in the k's
c     AND which includes the continuum contribution for theses gases
c     EXCEPT water vapor (which will be included separately).
c
cNaming Convention:
c     One major gas: 'tape5nc-Txx-n09' where the xx [01,..,05] 
c     indicates the lower atmosphere and 5 reference temperature
c     levels and xx [06,..,10] indicates upper atmosphere and 5
c     reference temperature levels.      
c
c     Two major gas: 'tape5nc-Txx-n'nnum'' where the xx [01,..,05] 
c     indicates the lower atmosphere and 5 reference temperature
c     levels and xx [06,..,10] indicates upper atmosphere and 5
c     reference temperature levels.  'nnum' indicates the binary    
c     species parameter(eta) with n01 equaling only gas2 present and
c     n09 equaling only gas1 present (this corresponds to the n09 
c    for one major gas.
C***************************************************
      PARAMETER (MXMOL=9)
      CHARACTER*2 FNUM(99),nnum(9)
      CHARACTER*50 TAPE5
      CHARACTER*20 rec_2_1_lo,rec_2_1_hi
      PARAMETER (MXL=200)
      PARAMETER (MXREF = 50)
      DIMENSION PRESS(0:MXL),T0(MXL),ALT(0:MXL)
      DIMENSION ALTREF(MXREF),PREF(MXREF),TREF(MXREF)
      DIMENSION AMOL(MXMOL,MXREF)
      DIMENSION RHOTOT(MXL),W(mxmol,mxl),W_orig(mxmol,mxl)
      DIMENSION ETA(9),ICN(2)
      DIMENSION WVN_LCOUPLE(10)
      integer igas_minor_l(mxmol,1),igas_minor_u(mxmol,1)
      NAMELIST /PAR/ WAVENUMBER1,WAVENUMBER2,IGAS1_L,IGAS2_L,IGAS1_U,
     &                   IGAS2_U ,igas_minor_l,igas_minor_u,nmol

C     Bolztman's constant is a factor of 1E-3 off to compensate for
C     the units of pressure.
      
      DATA BOLTZ /1.38044E-19 /
      DATA AVOGAD / 6.022045E+23 /
      DATA WTWAT /18.015/
      DATA  TZERO / 273.15 /                           
      DATA  PZERO / 1013.25 /
      DATA RHOFAC /1.0006/

      DATA WVN_LCOUPLE/612.0,619.0,667.0,677.0,714.0,722.0,
     &     735.0,742.0,791.0,796.0/

      DATA ETA/0.0,0.125,0.25,0.375,0.5,0.625,0.75,0.875,1.0/
      DATA NNUM/'01','02','03','04','05','06','07','08',
     &     '09'/
      DATA FNUM/'01','02','03','04','05','06','07','08','09',
     &     '10','11','12','13','14','15','16','17','18','19',
     &     '20','21','22','23','24','25','26','27','28','29',
     &     '30','31','32','33','34','35','36','37','38','39',
     &     '40','41','42','43','44','45','46','47','48','49',
     &     '50','51','52','53','54','55','56','57','58','59',
     &     '60','61','62','63','64','65','66','67','68','69',
     &     '70','71','72','73','74','75','76','77','78','79',
     &     '80','81','82','83','84','85','86','87','88','89',
     &     '90','91','92','93','94','95','96','97','98','99'/
c Define the continuum to be ON where icn(1) => lower atmosphere and 
c icn(2) => upper atmosphere.
      DATA icn /6,6/

      include 'std_atmos.f'

      DATA rec_2_1_lo /' 1 13 7   1.000000  '/
      DATA rec_2_1_hi /' 1 47 7   1.000000  '/


C Read in Namelist that provides the information concerning wavelength and
C gases for each level.
      READ(*,PAR)
      write(rec_2_1_lo(6:9),'i4') nmol
      write(rec_2_1_hi(6:9),'i4') nmol

C This adjustment is required by LBLRTM to generate the necessary optical depths.
C Note that when line coupling present, must extend the LBLRTM calculations to
C outside the boundary of this region.
      WAVENUM1=WAVENUMBER1-5.0
      WAVENUM2=WAVENUMBER2+5.0
      DO 500 IWVN=1,10,2
         IF (WAVENUM1 .GE. WVN_LCOUPLE(IWVN) .AND.
     &        WAVENUM1 .LE. WVN_LCOUPLE(IWVN+1)) THEN
              WAVENUM1 = WVN_LCOUPLE(IWVN)-5.0
         ENDIF
 500     CONTINUE
      DO 510 IWVN=1,10,2
         IF (WAVENUM2 .GE. WVN_LCOUPLE(IWVN) .AND.
     &        WAVENUM2 .LE. WVN_LCOUPLE(IWVN+1)) THEN
            WAVENUM2 = WVN_LCOUPLE(IWVN+1)+5.0
         ENDIF
 510  CONTINUE


C Temporary solution for setting up number of points going into the calculation of k.
      DVOUT = 3.0e-5
      IF (WAVENUM1.GE.125.AND.WAVENUM1.LT.240.)  DVOUT=0.00002
      IF (WAVENUM1.GE.240.AND.WAVENUM1.LE.325.)  DVOUT=0.00004
      IF (WAVENUM1.GT.325.)  DVOUT=0.00005
      IF (WAVENUM1.GT.1300.)  DVOUT=0.0002
      IF (WAVENUM1.GT.1700.)  DVOUT=0.0003
      IF (WAVENUM1.GT.2590.)  DVOUT=0.00045

C Pressure Profile provides the grid on which the k's will be stored. These
c therefore provide the pressure layers used in TAPE5.
      OPEN(10,FILE='PRESSURE.PROFILE',FORM='FORMATTED')
      ALT(0) = 0.0
      IPTHAK = 3
      DELTAT = 15.
      READ(10,9000)NLEV

      LEVDUP = 0
      IREF = 2

C     Calculate the interpolated values (in pressure) of temperature 
C     and mixing ratios of species 1-nmol corresponding to a MLS standard
C     atmosphere.

      PRESS(0) = 1000.
      DO 1000 LEV = 1, NLEV
         READ(10,9001)PRESS(LEV)
         P = PRESS(LEV)

C     Find the layer at which a switch will be made from "keying off" the
C     amount of one species below to "keying off" of another above.  
         IF (((P-115.)*(PRESS(LEV-1)-115.)) .LT. 0.0) LEVDUP = LEV

C     Don't interpolate if pressure higher than MLS ground pressure.
C     Instead, take the values for this pressure.
         IF (P .GE. PREF(1)) THEN
            T0(LEV) = TREF(1)
            do im=1,nmol
               w_orig(im,lev) = amol(im,1)*1.e-6
            end do  
         ELSE
 900        CONTINUE
            IF (P .GE. PREF(IREF)) THEN
C     Interpolate in ln(pressure).
               FRAC = (ALOG(P)-ALOG(PREF(IREF-1)))/
     &              (ALOG(PREF(IREF))-ALOG(PREF(IREF-1)))
               T0(LEV) = TREF(IREF-1) + FRAC*(TREF(IREF)-TREF(IREF-1))
               do im=1,nmol
                  W_ORIG(im,LEV) = AMOL(im,IREF-1) + FRAC* 
     &              (AMOL(im,IREF)-AMOL(im,IREF-1))
                  W_ORIG(im,LEV) = W_ORIG(im,LEV) * 1.E-06
               end do  
            ELSE
               IREF= IREF + 1
               GO TO 900
            ENDIF
         ENDIF
         RHOTOT(LEV) = RHOFAC*P/(BOLTZ*T0(LEV))
 1000 CONTINUE
      CLOSE(10)


C NO MAJOR GASES IN THE LOWER ATMOSPHERE

      IF (IGAS1_L .EQ. 0 .AND. IGAS2_L .EQ. 0) GOTO 3900

C  ONE MAJOR GAS, LOWER ATMOSPHERE : Write tape5's for situation 
c     with one major gas.  Value of all gases EXCEPT the major gas are set to 0.0.
      IF (IGAS2_L. EQ. 0) THEN 

C Determine values for continuum scale factor.  For the mappee run, want all
C scale factors set to 0.0 EXCEPT that of the mappee. Water vapor as the
C mappee is always set to 0.0

         XSELFL = 0.0
         XFRGNL = 0.0
         XCO2CL = 0.0
         XO3CNL = 0.0
         XO2CNL = 0.0
         XN2CNL = 0.0
         XRAYLL = 0.0

         IF (IGAS1_L .EQ. 2 ) XCO2CL = 1.0
         IF (IGAS1_L .EQ. 3 ) XO3CNL = 1.0
         IF (IGAS1_L .EQ. 7 ) XO2CNL = 1.0

         W = 0.0
         W(IGAS1_L,:) = W_ORIG(IGAS1_L,:)
         INDEX=1
         DO 2500 ITEMP = -2, 2
            TAPE5 = 'tape5nc-T'//FNUM(INDEX)//'-n09'
            OPEN(20,FILE=TAPE5,FORM='FORMATTED')
            WRITE(20,100)
            WRITE(20,101) '1        2         3         4         5',
     &           '         6         7         8         9'
            WRITE(20,102) '123456789-123456789-123456789-123456789-',
     &           '123456789-123456789-123456789-123456789-'
            WRITE(20,103)
            WRITE(20,104) ' HI=1 F4=1 CN=',ICN(1),
     &           ' AE=0 EM=0 SC=0 FI=0',
     &           ' PL=0 TS=0 AM=0 MG=1 LA=0    1        00   00'
            WRITE(20,105) XSELFL,XFRGNL,XCO2CL,XO3CNL,XO2CNL,XN2CNL,XRAYLL
            WRITE(20,106) WAVENUM1,WAVENUM2,dvout
            WRITE(20,107) rec_2_1_lo,
     &           'MIDLATITUDE SUMM H1=   0.00 ',
     &           'H2= 70.00   ', 'ANG=   0.000  LEN= 0 '
            TEMP = T0(1) + ITEMP*DELTAT
            WATER = W(1,1)*RHOTOT(1)/(1.+W(1,1))
            RHODRY = RHOTOT(1)-WATER
            WRITE(20,9023) PRESS(1),TEMP,IPTHAK
            BROAD=RHODRY*1.E5*(1-sum(W(2:nmol,1)))
            WRITE(20,9015)W(1:7,1),BROAD
            if (nmol.gt.7) then
               WRITE(20,9015)W(8:nmol,1)
            endif
            DO 2000 LEV = 2, LEVDUP
               TEMP = T0(LEV) + ITEMP*DELTAT
               WATER = W(1,LEV)*RHOTOT(LEV)/(1.+W(1,LEV))
               RHODRY = RHOTOT(LEV)-WATER
               WRITE (20,9014) PRESS(LEV),TEMP,IPTHAK
               BROAD = RHODRY*1.E5*(1.-sum(W(2:nmol,LEV)))
               WRITE(20,9015)W(1:7,LEV),BROAD
               if (nmol.gt.7) then
                  WRITE(20,9015)W(8:nmol,lev)
               endif
 2000       CONTINUE
            WRITE(20,109)
            CLOSE(20)
            INDEX = INDEX + 1
 2500 CONTINUE

C  TWO MAJOR GASES, LOWER ATMOSPHERE : Write tape5's for situation 
c     with two major gases.  Value of all gases EXCEPT the major gas are set to 0.0.
      ELSE IF (IGAS2_L .NE. 0) THEN 
         DO 3750 IETA = 1,9

C Determine values for continuum scale factor.  For the mappee run, want all
C scale factors set to 0.0 EXCEPT that of the mappee. Water vapor as the
C mappee is always set to 0.0

            XSELFL = 0.0
            XFRGNL = 0.0
            XCO2CL = 0.0
            XO3CNL = 0.0
            XO2CNL = 0.0
            XN2CNL = 0.0
            XRAYLL = 0.0

            W=0.0
            IF (IETA .EQ. 1) THEN
               W(IGAS1_L,:) = 0.0
               W(IGAS2_L,:) = W_ORIG(IGAS2_L,:)
               IF (IGAS2_L .EQ. 2) XCO2CL = 1.0
               IF (IGAS2_L .EQ. 3) XO3CNL = 1.0
               IF (IGAS2_L .EQ. 7) XO2CNL = 1.0
            ELSE
               IF (IETA .EQ. 9) THEN      
                  W(IGAS1_L,:) = W_ORIG(IGAS1_L,:)
                  W(IGAS2_L,:) = 0.0
                  IF (IGAS1_L .EQ. 2) XCO2CL = 1.0
                  IF (IGAS1_L .EQ. 3) XO3CNL = 1.0
                  IF (IGAS1_L .EQ. 7) XO2CNL = 1.0
               ELSE
                  IF (IGAS1_L .EQ. 2 .OR. IGAS2_L .EQ. 2) XCO2CL = 1.0
                  IF (IGAS1_L .EQ. 3 .OR. IGAS2_L .EQ. 3) XO3CNL = 1.0
                  IF (IGAS1_L .EQ. 7 .OR. IGAS2_L .EQ. 7) XO2CNL = 1.0
                  W(IGAS1_L,:) = W_ORIG(IGAS1_L,:)*
     &                 ETA(IETA)/(1. - ETA(IETA))
                  W(IGAS2_L,:) = W_ORIG(IGAS2_L,:)
               ENDIF
            ENDIF
            INDEX = 1
            INDEX2 = IETA
            DO 3500 ITEMP = -2, 2
               TAPE5 = 'tape5nc-T'//FNUM(INDEX)//'-n'//NNUM(INDEX2)
               OPEN(20,FILE=TAPE5,FORM='FORMATTED')
               WRITE(20,100)
            WRITE(20,101) '1        2         3         4         5',
     &           '         6         7         8         9'
            WRITE(20,102) '123456789-123456789-123456789-123456789-',
     &           '123456789-123456789-123456789-123456789-'
            WRITE(20,103)
            WRITE(20,104) ' HI=1 F4=1 CN=',ICN(1),
     &           ' AE=0 EM=0 SC=0 FI=0',
     &           ' PL=0 TS=0 AM=0 MG=1 LA=0    1        00   00'
            WRITE(20,105) XSELFL,XFRGNL,XCO2CL,XO3CNL,XO2CNL,XN2CNL,XRAYLL
            WRITE(20,106) WAVENUM1,WAVENUM2,DVOUT
            WRITE(20,107) rec_2_1_lo,
     &              'MIDLATITUDE SUMM H1=   0.00 ',
     &              'H2= 70.00   ', 'ANG=   0.000  LEN= 0 '
               TEMP = T0(1) + ITEMP*DELTAT
               WATER = W(1,1)*RHOTOT(1)/(1.+W(1,1))
               RHODRY = RHOTOT(1)-WATER
               WRITE(20,9023) PRESS(1),TEMP,IPTHAK
               BROAD=RHODRY*1.E5*(1-sum(W(2:nmol,1)))
               WRITE(20,9015)W(1:7,1),BROAD
               if (nmol.gt.7) then
                  WRITE(20,9015)W(8:nmol,1)
               endif
               DO 3000 LEV = 2, LEVDUP
                  TEMP = T0(LEV) + ITEMP*DELTAT
                  WATER = W(1,LEV)*RHOTOT(LEV)/(1.+W(1,LEV))
                  RHODRY = RHOTOT(LEV)-WATER
                  WRITE (20,9014) PRESS(LEV),TEMP,IPTHAK
                  BROAD = RHODRY*1.E5*(1.-sum(W(2:nmol,LEV)))
                  WRITE(20,9015)W(1:7,LEV),BROAD
                  if (nmol.gt.7) then
                     WRITE(20,9015)W(8:nmol,LEV)
                  endif

 3000          CONTINUE
               WRITE(20,109)
               CLOSE(20)
               INDEX = INDEX + 1
 3500       CONTINUE
 3750    CONTINUE
      ENDIF

 3900 CONTINUE

C NO MAJOR GASES IN THE LOWER ATMOSPHERE
      IF (IGAS1_U .EQ. 0 .AND. IGAS2_U .EQ. 0) GOTO 5900

C  ONE MAJOR GAS, UPPER ATMOSPHERE : Write tape5's for situation 
c     with one major gas.  Value of all gases EXCEPT the major gas are set to 0.0.
      IF (IGAS2_U .EQ. 0  .and. igas1_u .ne. 0) THEN
         XSELFU = 0.0
         XFRGNU = 0.0
         XCO2CU = 0.0
         XO3CNU = 0.0
         XO2CNU = 0.0
         XN2CNU = 0.0
         XRAYLU = 0.0

         IF (IGAS1_U .EQ. 2) XCO2CU = 1.0
         IF (IGAS1_U .EQ. 3) XO3CNU = 1.0
         IF (IGAS1_U .EQ. 7) XO2CNU = 1.0

         W = 0.0
         W(IGAS1_U,:) = W_ORIG(IGAS1_U,:)
         DO 4500 ITEMP = -2, 2
            TAPE5 = 'tape5nc-T'//FNUM(INDEX)//'-n09'
            OPEN(20,FILE=TAPE5,FORM='FORMATTED')
            WRITE(20,100)
            WRITE(20,101) '1        2         3         4         5',
     &           '         6         7         8         9'
c            WRITE(20,101)
            WRITE(20,102) '123456789-123456789-123456789-123456789-',
     &           '123456789-123456789-123456789-123456789-'
            WRITE(20,103)
            WRITE(20,104) ' HI=1 F4=1 CN=',ICN(2),
     &           ' AE=0 EM=0 SC=0 FI=0',
     &           ' PL=0 TS=0 AM=0 MG=1 LA=0    1        00   00'
            WRITE(20,105) XSELFU,XFRGNU,XCO2CU,XO3CNU,XO2CNU,XN2CNU,XRAYLU
            WRITE(20,106) WAVENUM1,WAVENUM2,dvout
            WRITE(20,107) rec_2_1_hi,
     &           'MIDLATITUDE SUMM H1=   0.00 ',
     &           'H2= 70.00   ', 'ANG=   0.000  LEN= 0 '
            TEMP = T0(LEVDUP) + ITEMP*DELTAT
            WATER = W(1,LEVDUP)*RHOTOT(LEVDUP)/(1.+W(1,LEVDUP))
            RHODRY = RHOTOT(LEVDUP)-WATER
            WRITE(20,9023) PRESS(LEVDUP),TEMP,IPTHAK
            BROAD = RHODRY*1.E5*(1.-sum(W(2:nmol,LEVDUP)))
            WRITE(20,9015)W(1:7,levdup),BROAD
            if (nmol.gt.7) then
               WRITE(20,9015)W(8:nmol,levdup)
            endif
            DO 4000 LEV = LEVDUP+1, NLEV
               TEMP = T0(LEV) + ITEMP*DELTAT
               WATER = W(1,LEV)*RHOTOT(LEV)/(1.+W(1,LEV))
               RHODRY = RHOTOT(LEV)-WATER
               WRITE (20,9014) PRESS(LEV),TEMP,IPTHAK
               BROAD = RHODRY*1.E5*(1.-sum(W(2:nmol,LEV)))
               WRITE(20,9015)W(1:7,lev),BROAD
               if (nmol.gt.7) then
                  WRITE(20,9015)W(8:nmol,lev)
               endif
 4000       CONTINUE
            WRITE(20,109)
            CLOSE(20)
            INDEX = INDEX + 1
 4500 CONTINUE

C     TWO MAJOR GASES, UPPER ATMOSPHERE : Write tape5's for situation 
c     with two major gases.  Value of all gases EXCEPT the major gas are set to 0.0.
      ELSE IF (IGAS2_U .NE. 0) THEN
         DO 5750 IETA=1,9,2
            XSELFU = 0.0
            XFRGNU = 0.0
            XCO2CU = 0.0
            XO3CNU = 0.0
            XO2CNU = 0.0
            XN2CNU = 0.0
            XRAYLU = 0.0

            W=0.0
            IF (IETA .EQ. 1) THEN
               W(IGAS1_U,:) = 0.0
               W(IGAS2_U,:) = W_ORIG(IGAS2_U,:)
               IF (IGAS2_U .EQ. 2) XCO2CU = 1.0
               IF (IGAS2_U .EQ. 3) XO3CNU = 1.0
               IF (IGAS2_U .EQ. 7) XO2CNU = 1.0
            ELSE
               IF (IETA .EQ. 9) THEN      
                  W(IGAS1_U,:) = W_ORIG(IGAS1_U,:)
                  W(IGAS2_U,:) = 0.0
                  IF (IGAS1_U .EQ. 2 ) XCO2CU = 1.0
                  IF (IGAS1_U .EQ. 3 ) XO3CNU = 1.0
                  IF (IGAS1_U .EQ. 7 ) XO2CNU = 1.0
               ELSE
                  W(IGAS1_U,:) = W_ORIG(IGAS1_U,:)
     &                 *ETA(IETA)/(1. - ETA(IETA))
                  W(IGAS2_U,:) = W_ORIG(IGAS2_U,:)
                  IF (IGAS1_U .EQ. 2 .OR. IGAS2_U .EQ. 2) XCO2CU = 1.0
                  IF (IGAS1_U .EQ. 3 .OR. IGAS2_U .EQ. 3) XO3CNU = 1.0
                  IF (IGAS1_U .EQ. 7 .OR. IGAS2_U .EQ. 7) XO2CNU = 1.0
               ENDIF
            ENDIF
            INDEX = 6
            INDEX2 = IETA
            DO 5500 ITEMP = -2, 2
               TAPE5 = 'tape5nc-T'//FNUM(INDEX)//'-n'//nnum(index2)
               OPEN(20,FILE=TAPE5,FORM='FORMATTED')
               WRITE(20,100)
            WRITE(20,101) '1        2         3         4         5',
     &           '         6         7         8         9'
            WRITE(20,102) '123456789-123456789-123456789-123456789-',
     &           '123456789-123456789-123456789-123456789-'
               WRITE(20,103)
            WRITE(20,104) ' HI=1 F4=1 CN=',ICN(2),
     &              ' AE=0 EM=0 SC=0 FI=0',
     &              ' PL=0 TS=0 AM=0 MG=1 LA=0    1        00   00'
            WRITE(20,105) XSELFU,XFRGNU,XCO2CU,XO3CNU,XO2CNU,XN2CNU,XRAYLU
            WRITE(20,106) WAVENUM1,WAVENUM2,DVOUT
            WRITE(20,107) rec_2_1_hi,
     &              'MIDLATITUDE SUMM H1=   0.00 ',
     &              'H2= 70.00   ', 'ANG=   0.000  LEN= 0 '
               TEMP = T0(LEVDUP) + ITEMP*DELTAT
               WATER = W(1,LEVDUP)*RHOTOT(LEVDUP)/(1.+W(1,LEVDUP))
               RHODRY = RHOTOT(LEVDUP)-WATER
               WRITE(20,9023) PRESS(LEVDUP),TEMP,IPTHAK
               BROAD = RHODRY*1.E5*(1.-sum(W(2:nmol,LEVDUP)))
               WRITE(20,9015)W(1:7,levdup),BROAD
               if (nmol.gt.7) then
                  WRITE(20,9015)W(8:nmol,levdup)
               endif
               DO 5000 LEV = LEVDUP+1, NLEV
                  TEMP = T0(LEV) + ITEMP*DELTAT
                  WATER = W(1,LEV)*RHOTOT(LEV)/(1.+W(1,LEV))
                  RHODRY = RHOTOT(LEV)-WATER
                  WRITE (20,9014) PRESS(LEV),TEMP,IPTHAK
                  BROAD = RHODRY*1.E5*(1.-sum(W(2:nmol,LEV)))
                  WRITE(20,9015)W(1:7,lev),BROAD
                  if (nmol.gt.7) then
                     WRITE(20,9015)W(8:nmol,lev)
                  endif
 5000          CONTINUE
               WRITE(20,109)
               CLOSE(20)
               INDEX = INDEX + 1
 5500 CONTINUE
 5750 CONTINUE
      ENDIF
 5900 continue


 100  FORMAT('TAPE5 FOR MLS')
 101  FORMAT(A40,A40)
 102  FORMAT(2(A40))
 103  FORMAT('$ STANDARD MID-LATITUDE SUMMER ATMOSPHERE')
 104  FORMAT(A14,i1,A20,A45)
 105  FORMAT(7F10.4)
 106  FORMAT(2f10.3,70x,e10.3)
 107  FORMAT(A20,A28,A12,A13)
 109  FORMAT('%%%%%')

 8005 FORMAT('       DATA W_L_MLS(',i1,',1:13)/')
 8010 FORMAT('     &',1P,5(g12.4,','))
 8015 FORMAT('     &',1P,2(g12.4,','),g12.4,'/')
 8020 FORMAT('       DATA W_H_MLS(',i1,',1:47)/')
 8025 FORMAT('     &',1P,g12.4,',',g12.4,'/')

 9000 FORMAT(I3)
 9001 FORMAT(G18.7)
 9010 FORMAT(A80)
 9011 FORMAT(A100)
 9014 FORMAT (1P,G15.7,G14.5,9X,I2,23X,0P)
 9015 FORMAT (1P,8G15.7,0P)
 9023 FORMAT (1P,G15.7,G10.4,13X,I2,0P)
 9031 FORMAT('     &    ',1P,E11.5,0P,'/')
 9032 FORMAT('     &    ',1P,E11.5,',',E11.5,0P,'/')
 9033 FORMAT('     &    ',1P,2(E11.5,','),E11.5,0P,'/')
 9034 FORMAT('     &    ',1P,3(E11.5,','),E11.5,0P,'/')
 9035 FORMAT('     &    ',1P,4(E11.5,','),E11.5,0P,'/')
 9041 FORMAT('     &    ',1P,E11.4,0P,'/')
 9042 FORMAT('     &    ',1P,E11.4,',',E11.4,0P,'/')
 9043 FORMAT('     &    ',1P,2(E11.4,','),E11.4,0P,'/')
 9044 FORMAT('     &    ',1P,3(E11.4,','),E11.4,0P,'/')
 9045 FORMAT('     &    ',1P,4(E11.4,','),E11.4,0P,'/')
 9048 FORMAT('      DATA RATREF42 /')
 9049 FORMAT('      DATA PREFLOG /')
 9050 FORMAT('      DATA PREF /')
 9051 FORMAT('      DATA TREF /')
 9052 FORMAT('      DATA RATREF41 /')
 9053 FORMAT('     &    ',1P,5(E11.5,','),0P)
 9055 FORMAT('     &    ',1P,5(E11.4,','),0P)
 9056 FORMAT('     &    ',1P,4(E11.4,','),E11.4,0P,'/')
 9071 FORMAT('      DIMENSION PREF(',I3,')')
 9072 FORMAT('      DIMENSION PREFLOG(',I3,')')
 9073 FORMAT('      DIMENSION TREF(',I3,')')
 9074 FORMAT('      DIMENSION RATREF41(',I2,')')
 9075 FORMAT('      DIMENSION RATREF42(',I2,':',I3,')')

      STOP 
      END


    
      
