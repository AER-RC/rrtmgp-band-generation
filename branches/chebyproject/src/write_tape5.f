      PROGRAM WRITE_TAPE5
C***************************************************
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
      CHARACTER*2 FNUM(99),nnum(9)
      CHARACTER*50 TAPE5
      PARAMETER (MXL=200)
      PARAMETER (MXREF = 50)
      DIMENSION PRESS(0:MXL),T0(MXL),ALT(0:MXL)
      DIMENSION ALTREF(MXREF),PREF(MXREF),TREF(MXREF),AMOL1(MXREF)
      DIMENSION AMOL2(MXREF),AMOL3(MXREF),AMOL4(MXREF),AMOL5(MXREF)
      DIMENSION AMOL6(MXREF),AMOL7(MXREF),AMOL8(MXREF)
      DIMENSION RHOTOT(MXL),W(7,mxl),W_orig(7,mxl)
      DIMENSION ETA(9),ICN(2)
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
      DATA ICN /1,1/
C     The following data corresponds to an MLS standard atmosphere.
      DATA ALTREF /    0.0,       1.0,       2.0,       3.0,       4.0,  
     *                 5.0,       6.0,       7.0,       8.0,       9.0,  
     *                10.0,      11.0,      12.0,      13.0,      14.0,  
     *                15.0,      16.0,      17.0,      18.0,      19.0,  
     *                20.0,      21.0,      22.0,      23.0,      24.0,  
     *                25.0,      27.5,      30.0,      32.5,      35.0,  
     *                37.5,      40.0,      42.5,      45.0,      47.5,  
     *                50.0,      55.0,      60.0,      65.0,      70.0,  
     *                75.0,      80.0,      85.0,      90.0,      95.0,  
     *               100.0,     105.0,     110.0,     115.0,     120.0/  
      DATA PREF /1.013E+03, 9.020E+02, 8.020E+02, 7.100E+02, 6.280E+02,  
     *           5.540E+02, 4.870E+02, 4.260E+02, 3.720E+02, 3.240E+02,  
     *           2.810E+02, 2.430E+02, 2.090E+02, 1.790E+02, 1.530E+02,  
     *           1.300E+02, 1.110E+02, 9.500E+01, 8.120E+01, 6.950E+01,  
     *           5.950E+01, 5.100E+01, 4.370E+01, 3.760E+01, 3.220E+01,  
     *           2.770E+01, 1.907E+01, 1.320E+01, 9.300E+00, 6.520E+00,  
     *           4.640E+00, 3.330E+00, 2.410E+00, 1.760E+00, 1.290E+00,  
     *           9.510E-01, 5.150E-01, 2.720E-01, 1.390E-01, 6.700E-02,  
     *           3.000E-02, 1.200E-02, 4.480E-03, 1.640E-03, 6.250E-04,  
     *           2.580E-04, 1.170E-04, 6.110E-05, 3.560E-05, 2.270E-05/  
      DATA TREF /   294.20,    289.70,    285.20,    279.20,    273.20,  
     *              267.20,    261.20,    254.70,    248.20,    241.70,  
     *              235.30,    228.80,    222.30,    215.80,    215.70,  
     *              215.70,    215.70,    215.70,    216.80,    217.90,  
     *              219.20,    220.40,    221.60,    222.80,    223.90,  
     *              225.10,    228.45,    233.70,    239.00,    245.20,  
     *              251.30,    257.50,    263.70,    269.90,    275.20,  
     *              275.70,    269.30,    257.10,    240.10,    218.10,  
     *              196.10,    174.10,    165.10,    165.00,    178.30,  
     *              190.50,    222.20,    262.40,    316.80,    380.00/  
      DATA AMOL1 /                                                      
     *           1.876E+04, 1.378E+04, 9.680E+03, 5.984E+03, 3.813E+03,  
     *           2.225E+03, 1.510E+03, 1.020E+03, 6.464E+02, 4.129E+02,  
     *           2.472E+02, 9.556E+01, 2.944E+01, 8.000E+00, 5.000E+00,  
     *           3.400E+00, 3.300E+00, 3.200E+00, 3.150E+00, 3.200E+00,  
     *           3.300E+00, 3.450E+00, 3.600E+00, 3.850E+00, 4.000E+00,  
     *           4.200E+00, 4.450E+00, 4.700E+00, 4.850E+00, 4.950E+00,  
     *           5.000E+00, 5.100E+00, 5.300E+00, 5.450E+00, 5.500E+00,  
     *           5.500E+00, 5.350E+00, 5.000E+00, 4.400E+00, 3.700E+00,  
     *           2.950E+00, 2.100E+00, 1.330E+00, 8.500E-01, 5.400E-01,  
     *           4.000E-01, 3.400E-01, 2.800E-01, 2.400E-01, 2.000E-01/  
      DATA AMOL2 /                                                      
     *           3.550E+02, 3.550E+02, 3.550E+02, 3.550E+02, 3.550E+02,  
     *           3.550E+02, 3.550E+02, 3.550E+02, 3.550E+02, 3.550E+02,  
     *           3.550E+02, 3.550E+02, 3.550E+02, 3.550E+02, 3.550E+02,  
     *           3.550E+02, 3.550E+02, 3.550E+02, 3.550E+02, 3.550E+02,  
     *           3.550E+02, 3.550E+02, 3.550E+02, 3.550E+02, 3.550E+02,  
     *           3.550E+02, 3.550E+02, 3.550E+02, 3.550E+02, 3.550E+02,  
     *           3.550E+02, 3.550E+02, 3.550E+02, 3.550E+02, 3.550E+02,  
     *           3.550E+02, 3.550E+02, 3.550E+02, 3.550E+02, 3.550E+02,  
     *           3.550E+02, 3.530E+02, 3.430E+02, 3.330E+02, 2.890E+02,  
     *           2.080E+02, 1.180E+02, 6.400E+01, 4.300E+01, 3.800E+01/  
      DATA AMOL3 /                                                      
     *           3.017E-02, 3.337E-02, 3.694E-02, 4.222E-02, 4.821E-02,  
     *           5.512E-02, 6.408E-02, 7.764E-02, 9.126E-02, 1.111E-01,  
     *           1.304E-01, 1.793E-01, 2.230E-01, 3.000E-01, 4.400E-01,  
     *           5.000E-01, 6.000E-01, 7.000E-01, 1.000E+00, 1.500E+00,  
     *           2.000E+00, 2.400E+00, 2.900E+00, 3.400E+00, 4.000E+00,  
     *           4.800E+00, 6.000E+00, 7.000E+00, 8.100E+00, 8.900E+00,  
     *           8.700E+00, 7.550E+00, 5.900E+00, 4.500E+00, 3.500E+00,  
     *           2.800E+00, 1.800E+00, 1.300E+00, 8.000E-01, 4.000E-01,  
     *           1.900E-01, 2.000E-01, 5.700E-01, 7.500E-01, 7.000E-01,  
     *           4.000E-01, 2.000E-01, 5.000E-02, 5.000E-03, 5.000E-04/  
      DATA AMOL4 /                                                      
     *           3.200E-01, 3.200E-01, 3.200E-01, 3.200E-01, 3.200E-01,  
     *           3.200E-01, 3.200E-01, 3.200E-01, 3.195E-01, 3.163E-01,  
     *           3.096E-01, 2.989E-01, 2.936E-01, 2.860E-01, 2.800E-01,  
     *           2.724E-01, 2.611E-01, 2.421E-01, 2.174E-01, 1.843E-01,  
     *           1.607E-01, 1.323E-01, 1.146E-01, 1.035E-01, 9.622E-02,  
     *           8.958E-02, 8.006E-02, 6.698E-02, 4.958E-02, 3.695E-02,  
     *           2.519E-02, 1.736E-02, 1.158E-02, 7.665E-03, 5.321E-03,  
     *           3.215E-03, 2.030E-03, 1.397E-03, 1.020E-03, 7.772E-04,  
     *           6.257E-04, 5.166E-04, 4.352E-04, 3.727E-04, 3.237E-04,  
     *           2.844E-04, 2.524E-04, 2.260E-04, 2.039E-04, 1.851E-04/  
      DATA AMOL5 /                                                      
     *           1.500E-01, 1.450E-01, 1.399E-01, 1.349E-01, 1.312E-01,  
     *           1.303E-01, 1.288E-01, 1.247E-01, 1.185E-01, 1.094E-01,  
     *           9.962E-02, 8.964E-02, 7.814E-02, 6.374E-02, 5.025E-02,  
     *           3.941E-02, 3.069E-02, 2.489E-02, 1.966E-02, 1.549E-02,  
     *           1.331E-02, 1.232E-02, 1.232E-02, 1.307E-02, 1.400E-02,  
     *           1.521E-02, 1.722E-02, 1.995E-02, 2.266E-02, 2.487E-02,  
     *           2.716E-02, 2.962E-02, 3.138E-02, 3.307E-02, 3.487E-02,  
     *           3.645E-02, 3.923E-02, 4.673E-02, 6.404E-02, 1.177E-01,  
     *           2.935E-01, 6.815E-01, 1.465E+00, 2.849E+00, 5.166E+00,  
     *           1.008E+01, 1.865E+01, 2.863E+01, 3.890E+01, 5.000E+01/  
      DATA AMOL6 /                                                      
     *           1.700E+00, 1.700E+00, 1.700E+00, 1.700E+00, 1.697E+00,  
     *           1.687E+00, 1.672E+00, 1.649E+00, 1.629E+00, 1.615E+00,  
     *           1.579E+00, 1.542E+00, 1.508E+00, 1.479E+00, 1.451E+00,  
     *           1.422E+00, 1.390E+00, 1.356E+00, 1.323E+00, 1.281E+00,  
     *           1.224E+00, 1.154E+00, 1.066E+00, 9.730E-01, 8.800E-01,  
     *           7.888E-01, 7.046E-01, 6.315E-01, 5.592E-01, 5.008E-01,  
     *           4.453E-01, 3.916E-01, 3.389E-01, 2.873E-01, 2.384E-01,  
     *           1.944E-01, 1.574E-01, 1.500E-01, 1.500E-01, 1.500E-01,  
     *           1.500E-01, 1.500E-01, 1.500E-01, 1.400E-01, 1.300E-01,  
     *           1.200E-01, 1.100E-01, 9.500E-02, 6.000E-02, 3.000E-02/  
      DATA AMOL7 /                                                      
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  
     *           2.090E+05, 2.090E+05, 2.000E+05, 1.900E+05, 1.800E+05,  
     *           1.600E+05, 1.400E+05, 1.200E+05, 9.400E+04, 7.250E+04/  
      DATA AMOL8 /                                                      
     *           2.496E+19, 2.257E+19, 2.038E+19, 1.843E+19, 1.666E+19,  
     *           1.503E+19, 1.351E+19, 1.212E+19, 1.086E+19, 9.716E+18,  
     *           8.656E+18, 7.698E+18, 6.814E+18, 6.012E+18, 5.141E+18,  
     *           4.368E+18, 3.730E+18, 3.192E+18, 2.715E+18, 2.312E+18,  
     *           1.967E+18, 1.677E+18, 1.429E+18, 1.223E+18, 1.042E+18,  
     *           8.919E+17, 6.050E+17, 4.094E+17, 2.820E+17, 1.927E+17,  
     *           1.338E+17, 9.373E+16, 6.624E+16, 4.726E+16, 3.398E+16,  
     *           2.500E+16, 1.386E+16, 7.668E+15, 4.196E+15, 2.227E+15,  
     *           1.109E+15, 4.996E+14, 1.967E+14, 7.204E+13, 2.541E+13,  
     *           9.816E+12, 3.816E+12, 1.688E+12, 8.145E+11, 4.330E+11/  


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

C Temporary solution for setting up number of points going into the calculation of k.
      IF (WAVENUM1.GE.125.AND.WAVENUM1.LT.240.)  DVOUT=0.00002
      IF (WAVENUM1.GE.240.AND.WAVENUM1.LE.325.)  DVOUT=0.00004
      IF (WAVENUM1.GT.325.)  DVOUT=0.00005

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

C  ONE MAJOR GAS, LOWER ATMOSPHERE : Write tape5's for situation 
c     with one major gas.  
      IF (IGAS2_L. EQ. 0) THEN 
         W = W_ORIG
         INDEX=1
         DO 2500 ITEMP = -2, 2
            TAPE5 = 'tape5-T'//FNUM(INDEX)//'-n09'
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
            WRITE(20,106) WAVENUM1,WAVENUM2,dvout
            WRITE(20,107) ' 1 13 7   1.000000  ', 
     &           'MIDLATITUDE SUMM H1=   0.00 ',
     &           'H2= 70.00   ', 'ANG=   0.000  LEN= 0 '
            TEMP = T0(1) + ITEMP*DELTAT
            WATER = W(1,1)*RHOTOT(1)/(1.+W(1,1))
            RHODRY = RHOTOT(1)-WATER
            WRITE(20,9023) PRESS(1),TEMP,IPTHAK
            BROAD=RHODRY*1.E5*(1-W(2,1)-W(3,1)-W(4,1)-W(5,1)
     &           -W(6,1)-W(7,1))
            WRITE(20,9015)W(1,1),W(2,1),W(3,1),W(4,1),
     &           W(5,1),W(6,1),W(7,1),BROAD
            DO 2000 LEV = 2, LEVDUP
               TEMP = T0(LEV) + ITEMP*DELTAT
               WATER = W(1,LEV)*RHOTOT(LEV)/(1.+W(1,LEV))
               RHODRY = RHOTOT(LEV)-WATER
               WRITE (20,9014) PRESS(LEV),TEMP,IPTHAK
               BROAD = RHODRY*1.E5*(1.-W(2,LEV)-W(3,LEV)-W(4,LEV)-
     &              W(5,LEV)-W(6,LEV)-W(7,LEV))
               WRITE(20,9015)W(1,LEV),W(2,LEV),W(3,LEV),W(4,LEV),
     &              W(5,LEV),W(6,LEV),W(7,LEV),BROAD
 2000       CONTINUE
            WRITE(20,109)
            CLOSE(20)
            INDEX = INDEX + 1
 2500 CONTINUE
C  ONE MAJOR GAS, LOWER ATMOSPHERE : Write tape5's for situation 
c     with two major gases. 
      ELSE IF (IGAS2_L .NE. 0) THEN 
         DO 3750 IETA = 1,9
            W = W_ORIG
            IF (IETA .EQ. 1) THEN
               W(IGAS1_L,:) = 0.
               W(IGAS2_L,:) = W_ORIG(IGAS2_L,:)
            ELSE
               IF (IETA .EQ. 9) THEN      
                  W(IGAS1_L,:) = W_ORIG(IGAS1_L,:)
                  W(IGAS2_L,:) = 0.
               ELSE
                  W(IGAS1_L,:) = W_ORIG(IGAS1_L,:)
     &                 *ETA(IETA)/(1. - ETA(IETA))
                  W(IGAS2_L,:) = W_ORIG(IGAS2_L,:)
               ENDIF
            ENDIF
            INDEX = 1
            INDEX2 = IETA
            DO 3500 ITEMP = -2, 2
               TAPE5 = 'tape5-T'//FNUM(INDEX)//'-n'//NNUM(INDEX2)
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
               WRITE(20,106) WAVENUM1,WAVENUM2,DVOUT
            WRITE(20,107) ' 1 13 7   1.000000  ', 
     &              'MIDLATITUDE SUMM H1=   0.00 ',
     &              'H2= 70.00   ', 'ANG=   0.000  LEN= 0 '
               TEMP = T0(1) + ITEMP*DELTAT
               WATER = W(1,1)*RHOTOT(1)/(1.+W(1,1))
               RHODRY = RHOTOT(1)-WATER
               WRITE(20,9023) PRESS(1),TEMP,IPTHAK
               BROAD=RHODRY*1.E5*(1-W(2,1)-W(3,1)-W(4,1)-W(5,1)
     &              -W(6,1)-W(7,1))
               WRITE(20,9015)W(1,1),W(2,1),W(3,1),W(4,1),
     &              W(5,1),W(6,1),W(7,1),BROAD
               DO 3000 LEV = 2, LEVDUP
                  TEMP = T0(LEV) + ITEMP*DELTAT
                  WATER = W(1,LEV)*RHOTOT(LEV)/(1.+W(1,LEV))
                  RHODRY = RHOTOT(LEV)-WATER
                  WRITE (20,9014) PRESS(LEV),TEMP,IPTHAK
                  BROAD = RHODRY*1.E5*(1.-W(2,LEV)-W(3,LEV)-W(4,LEV)
     &              -W(5,LEV)-W(6,LEV)-W(7,LEV))
                  WRITE(20,9015)W(1,LEV),W(2,LEV),W(3,LEV),W(4,LEV),
     &           W(5,LEV),W(6,LEV),W(7,LEV),BROAD
 3000          CONTINUE
               WRITE(20,109)
               CLOSE(20)
               INDEX = INDEX + 1
 3500       CONTINUE
 3750    CONTINUE
      ENDIF

C  ONE MAJOR GAS, UPPER ATMOSPHERE : Write tape5's for situation 
c     with one major gas.  
      IF (IGAS2_U .EQ. 0) THEN
         W = W_ORIG
         DO 4500 ITEMP = -2, 2
            TAPE5 = 'tape5-T'//FNUM(INDEX)//'-n09'
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
            WRITE(20,106) WAVENUM1,WAVENUM2,dvout
            WRITE(20,107) ' 1 47 7   1.000000  ', 
     &           'MIDLATITUDE SUMM H1=   0.00 ',
     &           'H2= 70.00   ', 'ANG=   0.000  LEN= 0 '
            TEMP = T0(LEVDUP) + ITEMP*DELTAT
            WATER = W(1,LEVDUP)*RHOTOT(LEVDUP)/(1.+W(1,LEVDUP))
            RHODRY = RHOTOT(LEVDUP)-WATER
            WRITE(20,9023) PRESS(LEVDUP),TEMP,IPTHAK
            BROAD = RHODRY*1.E5*(1.-W(2,LEVDUP)-W(3,LEVDUP)-W(4,LEVDUP)
     &        -W(5,LEVDUP)-W(6,LEVDUP)-W(7,LEVDUP))
            WRITE(20,9015)W(1,LEVDUP),W(2,LEVDUP),W(3,LEVDUP),
     &           W(4,LEVDUP),W(5,LEVDUP),W(6,LEVDUP),W(7,LEVDUP),BROAD
            DO 4000 LEV = LEVDUP+1, NLEV
               TEMP = T0(LEV) + ITEMP*DELTAT
               WATER = W(1,LEV)*RHOTOT(LEV)/(1.+W(1,LEV))
               RHODRY = RHOTOT(LEV)-WATER
               WRITE (20,9014) PRESS(LEV),TEMP,IPTHAK
               BROAD = RHODRY*1.E5*(1.-W(2,LEV)-W(3,LEV)-W(4,LEV)
     &              -W(5,LEV)-W(6,LEV)-W(7,LEV))
               WRITE(20,9015)W(1,LEV),W(2,LEV),W(3,LEV),W(4,LEV),
     &           W(5,LEV),W(6,LEV),W(7,LEV),BROAD
 4000       CONTINUE
            WRITE(20,109)
            CLOSE(20)
            INDEX = INDEX + 1
 4500    CONTINUE

C     TWO MAJOR GAS, UPPER ATMOSPHERE : Write tape5's for situation 
c     with two major gases.  
      ELSE IF (IGAS2_U .NE. 0) THEN
         DO 5750 IETA=1,9,2
            W = W_ORIG
            IF (IETA .EQ. 1) THEN
               W(IGAS1_U,:) = 0.
               W(IGAS2_U,:) = W_ORIG(IGAS2_U,:)
            ELSE
               IF (IETA .EQ. 9) THEN      
                  W(IGAS1_U,:) = W_ORIG(IGAS1_U,:)
                  W(IGAS2_U,:) = 0.
               ELSE
                  W(IGAS1_U,:) = W_ORIG(IGAS1_U,:)
     &                 *ETA(IETA)/(1. - ETA(IETA))
                  W(IGAS2_L,:) = W_ORIG(IGAS2_L,:)
               ENDIF
            ENDIF
            INDEX = 6
            INDEX2 = IETA
            DO 5500 ITEMP = -2, 2
               TAPE5 = 'tape5-T'//FNUM(INDEX)//'-n'//nnum(index2)
               OPEN(20,FILE=TAPE5,FORM='FORMATTED')
               WRITE(20,100)
            WRITE(20,101) '1        2         3         4         5',
     &           '         6         7         8         9'
c               WRITE(20,101)
            WRITE(20,102) '123456789-123456789-123456789-123456789-',
     &           '123456789-123456789-123456789-123456789-'
               WRITE(20,103)
            WRITE(20,104) ' HI=1 F4=1 CN=',ICN(2),
     &              ' AE=0 EM=0 SC=0 FI=0',
     &              ' PL=0 TS=0 AM=0 MG=1 LA=0    1        00   00'
               WRITE(20,106) WAVENUM1,WAVENUM2,DVOUT
            WRITE(20,107) ' 1 47 7   1.000000  ', 
     &              'MIDLATITUDE SUMM H1=   0.00 ',
     &              'H2= 70.00   ', 'ANG=   0.000  LEN= 0 '
               TEMP = T0(LEVDUP) + ITEMP*DELTAT
               WATER = W(1,LEVDUP)*RHOTOT(LEVDUP)/(1.+W(1,LEVDUP))
               RHODRY = RHOTOT(LEVDUP)-WATER
               WRITE(20,9023) PRESS(LEVDUP),TEMP,IPTHAK
               BROAD = RHODRY*1.E5*(1.-W(2,LEVDUP)-W(3,LEVDUP)
     &              -W(4,LEVDUP)-W(5,LEVDUP)-W(6,LEVDUP)-W(7,LEVDUP))
               WRITE(20,9015)W(1,LEVDUP),W(2,LEVDUP),W(3,LEVDUP),
     &             W(4,LEVDUP),W(5,LEVDUP),W(6,LEVDUP),W(7,LEVDUP),BROAD
               DO 5000 LEV = LEVDUP+1, NLEV
                  TEMP = T0(LEV) + ITEMP*DELTAT
                  WATER = W(1,LEV)*RHOTOT(LEV)/(1.+W(1,LEV))
                  RHODRY = RHOTOT(LEV)-WATER
                  WRITE (20,9014) PRESS(LEV),TEMP,IPTHAK
                  BROAD = RHODRY*1.E5*(1.-W(2,LEV)-W(3,LEV)-W(4,LEV)
     &                 -W(5,LEV)-W(6,LEV)-W(7,LEV))
                  WRITE(20,9015)W(1,LEV),W(2,LEV),W(3,LEV),W(4,LEV),
     &                 W(5,LEV),W(6,LEV),W(7,LEV),BROAD
 5000          CONTINUE
               WRITE(20,109)
               CLOSE(20)
               INDEX = INDEX + 1
 5500 CONTINUE
 5750 CONTINUE
      ENDIF

c Generate data for kdis_sort
c      open(50,file='initial_mls')
c      do 8040 iii=1,7
c      write(50,8005) iii
c      write(50,8010) w_orig(iii,1:5)
c      write(50,8010) w_orig(iii,6:10)
c      write(50,8015) w_orig(iii,11:13)
c 8040 continue
      
c      write(50,*) press(1:5)
c      write(50,*) press(6:10)
c      write(50,*) press(11:13)

c      ii=13
c      do 8042 i=1,9
c         write(50,*) press(ii:ii+4)
c         ii = ii  + 5
c 8042     continue
c          write(50,*) press(58:59)


c      write(50,*) t0(1:5)
c      write(50,*) t0(6:10)
c     write(50,*) t0(11:13)
c     ii=13
c      do 8043 i=1,9
c         write(50,*) t0(ii:ii+4)
c         ii=ii+5
c 8043    continue
c          write(50,*) t0(58:59)
c         close(50)
c      do 8050 iii=1,7
c         write(50,8020) iii
c         ijj=levdup
c         do 8047 ij=1,91
c            write(50,8010) w_orig(iii,ijj:ijj+4)
c            ijj=ijj + 5
c 8047 continue       
c         write(50,8025) w_orig(iii,46:47)
c 8050 continue
c      close(50)



 100  FORMAT('TAPE5 FOR MLS')
 101  FORMAT(A40,A40)
 102  FORMAT(2(A40))
 103  FORMAT('$ STANDARD MID-LATITUDE SUMMER ATMOSPHERE')
 104  FORMAT(A14,i1,A20,A45)
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


    
      
