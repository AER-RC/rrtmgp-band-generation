      PROGRAM WRITE_TAPE5_CSH
C***************************************************
cAmended: April 2014 
cProgram version created by: Jennifer Delamere January, 1999
c
cCOMPILE INFORMATION: F90
c
cInput: Namelist which includes beginning wavenumber,
c     ending wavenumber and the gases to be included.
c      
cOutput: Tape 5's for generating the absorption coefficients
c     which include ALL 7 major MLS gases AND all continua.
c
cNaming Convention:
c     One major gas: 'tape5-Txx-n09' where the xx [01,..,05] 
c     indicates the lower atmosphere and 5 reference temperature
c     levels and xx [06,..,10] indicates upper atmosphere and 5
c     reference temperature levels.      
c
c     Two major gas: 'tape5-Txx-n'nnum'' where the xx [01,..,05] 
c     indicates the lower atmosphere and 5 reference temperature
c     levels and xx [06,..,10] indicates upper atmosphere and 5
c     reference temperature levels.  'nnum' indicates the binary    
c     species parameter(eta) with n01 equaling only gas2 present and
c     n09 equaling only gas1 present (this corresponds to the n09 
c    for one major gas.
C***************************************************
      CHARACTER*3 FNUM(101)
      CHARACTER*50 TAPE5
      PARAMETER (MXL=200)
      PARAMETER (MXREF = 50)
      DIMENSION PRESS(0:MXL),T0(MXL),ALT(0:MXL)
      DIMENSION ALTREF(MXREF),PREF(MXREF),TREF(MXREF),AMOL1(MXREF)
      DIMENSION AMOL2(MXREF),AMOL3(MXREF),AMOL4(MXREF),AMOL5(MXREF)
      DIMENSION AMOL6(MXREF),AMOL7(MXREF),AMOL8(MXREF)
      DIMENSION RHOTOT(MXL),W(7,mxl),W_orig(7,mxl)
      DIMENSION WS(2,MXL,5,101)
      INTEGER ISAT(MXL,5)
      DIMENSION ETA(101),ICN(2)
      DIMENSION WVN_LCOUPLE(10)
      integer igas_minor_l(7,1),igas_minor_u(7,1)
      NAMELIST /PAR/ WAVENUMBER1,WAVENUMBER2,IGAS1_L,IGAS2_L,IGAS1_U,
     &                   IGAS2_U,igas_minor_l,igas_minor_u 

C     Bolztman's constant is a factor of 1E-3 off to compensate for
C     the units of pressure.
      
      DATA BOLTZ /1.38044E-19 /
      DATA AVOGAD / 6.022045E+23 /
      Data WTWAT /18.015/
      DATA  TZERO / 273.15 /                           
      DATA  PZERO / 1013.25 /
      DATA RHOFAC /1.0006/
      DATA ALOSMT / 2.6867775E+19 /
      DATA C1 / 18.9766 /,C2 / -14.9595 /,C3 / -2.4388 /

      DATA WVN_LCOUPLE/612.0,619.0,667.0,677.0,714.0,722.0,
     &     735.0,742.0,791.0,796.0/

      DATA ETA/
     &         0.00,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,
     &         0.10,0.11,0.12,0.13,0.14,0.15,0.16,0.17,0.18,0.19,
     &         0.20,0.21,0.22,0.23,0.24,0.25,0.26,0.27,0.28,0.29,
     &         0.30,0.31,0.32,0.33,0.34,0.35,0.36,0.37,0.38,0.39,
     &         0.40,0.41,0.42,0.43,0.44,0.45,0.46,0.47,0.48,0.49,
     &         0.50,0.51,0.52,0.53,0.54,0.55,0.56,0.57,0.58,0.59,
     &         0.60,0.61,0.62,0.63,0.64,0.65,0.66,0.67,0.68,0.69,
     &         0.70,0.71,0.72,0.73,0.74,0.75,0.76,0.77,0.78,0.79,
     &         0.80,0.81,0.82,0.83,0.84,0.85,0.86,0.87,0.88,0.89,
     &         0.90,0.91,0.92,0.93,0.94,0.95,0.96,0.97,0.98,0.99,
     &         1.00/
      DATA FNUM/'01','02','03','04','05','06','07','08','09',
     &     '10','11','12','13','14','15','16','17','18','19',
     &     '20','21','22','23','24','25','26','27','28','29',
     &     '30','31','32','33','34','35','36','37','38','39',
     &     '40','41','42','43','44','45','46','47','48','49',
     &     '50','51','52','53','54','55','56','57','58','59',
     &     '60','61','62','63','64','65','66','67','68','69',
     &     '70','71','72','73','74','75','76','77','78','79',
     &     '80','81','82','83','84','85','86','87','88','89',
     &     '90','91','92','93','94','95','96','97','98','99',
     &     '100','101'/
c Define the continuum to be ON where icn(1) => lower atmosphere and 
c icn(2) => upper atmosphere.
      DATA ICN /6,6/
C     The following data corresponds to an MLS standard atmosphere.
      include 'std_atmos.f'

C Useful functions
      SATDEN(TFRAC) = TFRAC*B*
     &          EXP(C1+C2*TFRAC+C3*TFRAC**2)*1.0E-6
      VMRTODEN(WV,RHOA) = (WV)/(1.0+WV)*RHOA
      DENTOVMR(WV,RHOA) = WV/(RHOA-WV)
      B = AVOGAD/WTWAT

C Read in Namelist that provides the information concerning wavelength and
C gases for each level.
      READ(*,PAR)

C This adjustment is required by LBLRTM to generate the necessary optical depths.
C Note that when line coupling present, must extend the LBLRTM calculations to
C outside the boundary of this region.

      WAVENUM1 = WAVENUMBER1 - 5.0
      WAVENUM2 = WAVENUMBER2 + 5.0

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

C Determine values for continuum scale factor.  For the mapper run, want all
C scale factors set to 1.0

      XSELF = 1.0
      XFRGN = 1.0
      XCO2C = 1.0
      XO3CN = 1.0
      XO2CN = 1.0
      XN2CN = 1.0
      XRAYL = 0.0


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
      B = AVOGAD/WTWAT
      ALT(0) = 0.0
      IPTHAK = 3
      DELTAT = 15.
      READ(10,9000)NLEV

      LEVDUP = 0
      IREF = 2

C     Calculate the interpolated values (in pressure) of temperature 
C     and mixing ratios of species 1-7 corresponding to a MLS standard
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
            W_ORIG(1,LEV) = AMOL1(1) * 1.E-06
            W_ORIG(2,LEV) = AMOL2(1) * 1.E-06
            W_ORIG(3,LEV) = AMOL3(1) * 1.E-06
            W_ORIG(4,LEV) = AMOL4(1) * 1.E-06
            W_ORIG(5,LEV) = AMOL5(1) * 1.E-06
            W_ORIG(6,LEV) = AMOL6(1) * 1.E-06
            W_ORIG(7,LEV) = AMOL7(1) * 1.E-06
         ELSE
 900        CONTINUE
            IF (P .GE. PREF(IREF)) THEN
C     Interpolate in ln(pressure).
               FRAC = (ALOG(P)-ALOG(PREF(IREF-1)))/
     &              (ALOG(PREF(IREF))-ALOG(PREF(IREF-1)))
               T0(LEV) = TREF(IREF-1) + FRAC*(TREF(IREF)-TREF(IREF-1))
               W_ORIG(1,LEV) = AMOL1(IREF-1) + FRAC*
     &              (AMOL1(IREF)-AMOL1(IREF-1))
               W_ORIG(1,LEV) = W_ORIG(1,LEV) * 1.E-06
               W_ORIG(2,LEV) = AMOL2(IREF-1) + FRAC*
     &              (AMOL2(IREF)-AMOL2(IREF-1))
               W_ORIG(2,LEV) = W_ORIG(2,LEV) * 1.E-06
               W_ORIG(3,LEV) = AMOL3(IREF-1) + FRAC*
     &              (AMOL3(IREF)-AMOL3(IREF-1))
               W_ORIG(3,LEV) = W_ORIG(3,LEV) * 1.E-06
               W_ORIG(4,LEV) = AMOL4(IREF-1) + FRAC*
     &              (AMOL4(IREF)-AMOL4(IREF-1))
               W_ORIG(4,LEV) = W_ORIG(4,LEV) * 1.E-06
               W_ORIG(5,LEV) = AMOL5(IREF-1) + FRAC*
     &              (AMOL5(IREF)-AMOL5(IREF-1))
               W_ORIG(5,LEV) = W_ORIG(5,LEV) * 1.E-06
               W_ORIG(6,LEV) = AMOL6(IREF-1) + FRAC*
     &              (AMOL6(IREF)-AMOL6(IREF-1))
               W_ORIG(6,LEV) = W_ORIG(6,LEV) * 1.E-06
               W_ORIG(7,LEV) = AMOL7(IREF-1) + FRAC*
     &              (AMOL7(IREF)-AMOL7(IREF-1))         
               W_ORIG(7,LEV) = W_ORIG(7,LEV) * 1.E-06
            ELSE
               IREF= IREF + 1
               GO TO 900
            ENDIF
         ENDIF
         RHOTOT(LEV) = RHOFAC*P/(BOLTZ*T0(LEV))
 1000 CONTINUE
      CLOSE(10)
C  TWO MAJOR GASES, LOWER ATMOSPHERE : Write tape5's for situation 
c     with two major gases.
c JSD
	OPEN(25,FILE='RHCALCS-csh-sat.txt',FORM='FORMATTED')
        WRITE(25,*) 'ETA,P,T,DENNUM/DENSAT'

	DO 3600 IP=1,13

C Compute eta = 1 case as it doesn't depend on temperature
          WS(1,IP,1:5,1) = 0.
          WS(2,IP,1:5,1) = W_ORIG(2,IP)

          DO 3605 ITEMP = -2,2
C Set up the temperature structure
            ITP = ITEMP+3
            TEMP = T0(IP)+ITEMP*DELTAT

C Compute the saturation density
            TFRAC = TZERO/TEMP
            PFRAC = PRESS(IP)/PZERO
            DENSAT = SATDEN(TFRAC)
            DENSATOVER = 1.05*SATDEN(TFRAC)
            RHOAIR = ALOSMT*PFRAC*TFRAC

C Calculate the MR of 1.05 the saturation value.
            WSAT = DENTOVMR(DENSAT,RHOAIR)
            WSATOVER = DENTOVMR(DENSATOVER,RHOAIR)

C Compute eta = 2,100 case
            DO 3610 IA = 2,100
             WS(1,IP,ITP,IA) = W_ORIG(1,IP)*
     &         ETA(IA)/(1. - ETA(IA))
             WTMP = WS(1,IP,ITP,IA)
             DENNUM = VMRTODEN(WS(1,IP,ITP,IA),RHOAIR)
             DENRAT = DENNUM/DENSAT
             WRITE(25,*) ETA(IA),PRESS(IP),TEMP,
     &         DENRAT
             IF (DENRAT .GT. 1.05) THEN
               WS(1,IP,ITP,IA) = WSATOVER
               WS(2,IP,ITP,IA) =
     &           ((1.0-ETA(IA))/ETA(IA))*
     &           (W_ORIG(2,IP)/
     &           W_ORIG(1,IP))*
     &           WS(1,IP,ITP,IA)
               ETACALC=WS(1,IP,ITP,IA)/
     &         (WS(1,IP,ITP,IA)+(W_ORIG(1,IP)/
     &         W_ORIG(2,IP))*WS(2,IP,ITP,IA))
             WRITE(25,*) 'CHANGING W',ETA(ia),
     &         PRESS(ip),TEMP,
     &         DENRAT,WTMP,WS(1,IP,ITP,IA),etacalc
             WRITE(25,*) 'CALCETA',etacalc
             ELSE
               WS(2,IP,ITP,IA) = W_ORIG(2,IP)
             ENDIF
 3610        CONTINUE

c Handle eta = 101 case
c DENRAT is handed over from eta=0.99 case.
           WS(2,IP,ITP,101) = 0.
           WRITE(25,*) ETA(101),PRESS(IP),TEMP,
     &       DENRAT
           IF (DENRAT .GT. 1.05) THEN
             WS(1,IP,ITP,101) = WSATOVER
             WRITE(25,*) 'CHANGING W',eta(101),
     &         PRESS(ip),TEMP,
     &         DENRAT,WS(1,IP,ITP,101),etacalc
           ELSE
             WS(1,IP,ITP,101) = W_ORIG(1,IP)
           ENDIF

 3605      CONTINUE
 3600    CONTINUE

         DO 3750 IETA = 1,101

            INDEX = 1
            INDEX2 = IETA
            DO 3500 ITEMP = -2, 2
               ITP = ITEMP+3
               TAPE5 = 'tape5-T'//trim(FNUM(ITEMP+3))//
     &           '-n'//trim(FNUM(IETA))
               OPEN(20,FILE=TAPE5,FORM='FORMATTED')
               WRITE(20,100)
            WRITE(20,101) '1        2         3         4         5',
     &           '         6         7         8         9'
c               WRITE(20,101)
            WRITE(20,102) '123456789-123456789-123456789-123456789-',
     &           '123456789-123456789-123456789-123456789-'
               WRITE(20,103)
            WRITE(20,104) ' HI=1 F4=1 CN=',ICN(1),
     &              ' AE=0 EM=0 SC=0 FI=0',
     &              ' PL=0 TS=0 AM=0 MG=1 LA=0    1        00   00'
            WRITE(20,105) XSELF,XFRGN,XCO2C,XO3CN,XO2CN,XN2CN,XRAYL
            WRITE(20,106) WAVENUM1,WAVENUM2,DVOUT
            WRITE(20,107) ' 1 13 7   1.000000  ', 
     &              'MIDLATITUDE SUMM H1=   0.00 ',
     &              'H2= 70.00   ', 'ANG=   0.000  LEN= 0 '


               DO 3000 LEV = 1, LEVDUP
                TEMP = T0(LEV) + ITEMP*DELTAT
                RHOTOTD = RHOFAC*PRESS(LEV)/(BOLTZ*TEMP)
                WATER = WS(1,LEV,ITP,IETA)*RHOTOTD/
     &            (1.+WS(1,LEV,ITP,IETA))
                RHODRY = RHOTOT(LEV)-WATER
                BROAD = RHODRY*1.E5*(1.-WS(2,LEV,ITP,IETA)
     &                -W_ORIG(3,LEV)-W_ORIG(4,LEV)
     &                -W_ORIG(5,LEV)-W_ORIG(6,LEV)-W_ORIG(7,LEV))
                IF (LEV .EQ. 1) THEN
                  WRITE(20,9023) PRESS(1),TEMP,IPTHAK
                ELSE
                  WRITE (20,9014) PRESS(LEV),TEMP,IPTHAK
                ENDIF
                WRITE(20,9015)
     &             WS(1,LEV,ITP,IETA),
     &             WS(2,LEV,ITP,IETA),
     &             W_ORIG(3,LEV),W_ORIG(4,LEV),
     &             W_ORIG(5,LEV),W_ORIG(6,LEV),
     &             W_ORIG(7,LEV),BROAD
 3000          CONTINUE
               WRITE(20,109)
               CLOSE(20)
               INDEX = INDEX + 1
 3500       CONTINUE
 3750    CONTINUE
c      ENDIF


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


    
      
