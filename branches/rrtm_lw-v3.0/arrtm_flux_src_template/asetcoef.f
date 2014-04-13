C     path:      %P%
C     revision:  %I%
C     created:   %G%  %U%
C     presently: %H%  %T%
      SUBROUTINE ASETCOEF

C     Purpose:  For a given atmosphere, calculate the indices and
C     fractions related to the pressure and temperature interpolations.
C     Also calculate the values of the integrated Planck functions 
C     for each band at the level and layer temperatures.

      PARAMETER (MXLAY = 203)
      PARAMETER (NBANDS = 16)
      PARAMETER (NBANDSLG = 16)
      PARAMETER (MG =16)

C  Input      
      COMMON /CONTROL/  NUMANGS, IOUT, ISTART, IEND
      COMMON /PROFILE/  NLAYERS,PAVEL(MXLAY),TAVEL(MXLAY),
     &                  PZ(0:MXLAY),TZ(0:MXLAY)
      COMMON /SURFACE/  TBOUND,IREFLECT,SEMISS(NBANDS)
      COMMON /SPECIES/  COLDRY(MXLAY),WKL(35,MXLAY),WBROAD(MXLAY),
     &                  COLMOL(MXLAY),NMOL

C  Output
      COMMON /PROFDATA/ LAYTROP,LAYSWTCH,LAYLOW,COLH2O(MXLAY),
     &                  COLCO2(MXLAY),COLO3(MXLAY),COLN2O(MXLAY),
     &                  COLCH4(MXLAY),COLO2(MXLAY),CO2MULT(MXLAY),
     &                  COLCO(MXLAY)
      COMMON /INTFAC/   FAC00(MXLAY),FAC01(MXLAY),
     &                  FAC10(MXLAY),FAC11(MXLAY)
      common /intfrac/  fp(mxlay)
      COMMON /INTIND/   JP(MXLAY),JT(MXLAY),JT1(MXLAY)
      COMMON /SELF/     SELFFAC, SELFFRAC, INDSELF
      COMMON /FOREIGN/  FORFAC(MXLAY), FORFRAC(MXLAY), INDFOR(MXLAY)
      COMMON /PLNKDAT/  PLANKLAY(MXLAY,NBANDS),
     &                  PLANKLEV(0:MXLAY,NBANDS),PLANKBND(NBANDS)
      COMMON /MLS_REF/  PREF,PREFLOG,TREF,W_MLS

C  Internal
c      common /avgplnk_01/ totplnk(181,1)
c      COMMON /AVGPLNK/  TOTPLNK(181,nbandslg), TOTPLK16(181)
      real totplnk(181,nbandslg),totplk16(181)
C  --------

      COMMON /HVERSN/    HVRRTM,HVRREG,HVRRTR,HVRATM,HVRSET,HVRTAU,
     *                   HVDUM1(4),HVRUTL,HVREXT

      CHARACTER*8 HVRRTM,HVRREG,HVRRTR,HVRATM,HVRSET,HVRTAU,
     *            HVDUM1,HVRUTL,HVREXT

      DIMENSION SELFFAC(MXLAY),SELFFRAC(MXLAY),INDSELF(MXLAY)
      DIMENSION PREF(59),PREFLOG(59),TREF(59)
      REAL W_MLS(7,59)

C ****************** START OF EXECUTABLE CODE ***************************

      INCLUDE 'avplank_01.f'
      INCLUDE 'avplank_other.f'

      HVRSET = '%I%'

      STPFAC = 296./1013.

      INDBOUND = TBOUND - 159.
      IF (INDBOUND .LT. 1) THEN
         INDBOUND = 1
      ELSEIF (INDBOUND .GT. 180) THEN
         INDBOUND = 180
      ENDIF
      TBNDFRAC = TBOUND - 159. - FLOAT(INDBOUND)
      INDLEV0 = TZ(0) - 159.
      IF (INDLEV0 .LT. 1) THEN
         INDLEV0 = 1
      ELSEIF (INDLEV0 .GT. 180) THEN
         INDLEV0 = 180
      ENDIF
      T0FRAC = TZ(0) - 159. - FLOAT(INDLEV0)
      LAYTROP = 0
      LAYSWTCH = 0
      LAYLOW = 0
      DO 7000 LAY = 1, NLAYERS
C        Calculate the integrated Planck functions for each band at the
C        surface, level, and layer temperatures.
         INDLAY = TAVEL(LAY) - 159.
         IF (INDLAY .LT. 1) THEN
            INDLAY = 1
         ELSEIF (INDLAY .GT. 180) THEN
            INDLAY = 180
         ENDIF
         TLAYFRAC = TAVEL(LAY) - 159. - FLOAT(INDLAY)
         INDLEV = TZ(LAY) - 159.
         IF (INDLEV .LT. 1) THEN
            INDLEV = 1
         ELSEIF (INDLEV .GT. 180) THEN
            INDLEV = 180
         ENDIF
         TLEVFRAC = TZ(LAY) - 159. - FLOAT(INDLEV)
         DO 3500 IBAND = 1, 15
            IF (LAY.EQ.1) THEN
               DBDTLEV = TOTPLNK(INDBOUND+1,IBAND)
     &              - TOTPLNK(INDBOUND,IBAND)
               PLANKBND(IBAND) = SEMISS(IBAND) * 
     &              (TOTPLNK(INDBOUND,IBAND) + TBNDFRAC * DBDTLEV)
               DBDTLEV = TOTPLNK(INDLEV0+1,IBAND)-TOTPLNK(INDLEV0,IBAND)
               PLANKLEV(0,IBAND) = TOTPLNK(INDLEV0,IBAND) + 
     &              T0FRAC * DBDTLEV
            ENDIF
            DBDTLEV = TOTPLNK(INDLEV+1,IBAND) - TOTPLNK(INDLEV,IBAND)
            DBDTLAY = TOTPLNK(INDLAY+1,IBAND) - TOTPLNK(INDLAY,IBAND)
            PLANKLAY(LAY,IBAND) = TOTPLNK(INDLAY,IBAND) + 
     &           TLAYFRAC * DBDTLAY
            PLANKLEV(LAY,IBAND) = TOTPLNK(INDLEV,IBAND) + 
     &           TLEVFRAC * DBDTLEV
 3500    CONTINUE

C        For band 16, if radiative transfer will be performed on just
C        this band, use integrated Planck values up to 3000 cm-1.  
C        If radiative transfer will be performed across all 16 bands,
C        then include in the integrated Planck values for this band
C        contributions from 2600 cm-1 to infinity.
         IF (ISTART .EQ. 16) THEN
            IF (LAY.EQ.1) THEN
               DBDTLEV = TOTPLK16(INDBOUND+1) - TOTPLK16(INDBOUND)
               PLANKBND(IBAND) = SEMISS(IBAND) * 
     &              (TOTPLK16(INDBOUND) + TBNDFRAC * DBDTLEV)
               DBDTLEV = TOTPLNK(INDLEV0+1,IBAND)-TOTPLNK(INDLEV0,IBAND)
               PLANKLEV(0,IBAND) = TOTPLK16(INDLEV0) + 
     &              T0FRAC * DBDTLEV
            ENDIF
            DBDTLEV = TOTPLK16(INDLEV+1) - TOTPLK16(INDLEV)
            DBDTLAY = TOTPLK16(INDLAY+1) - TOTPLK16(INDLAY)
            PLANKLAY(LAY,IBAND) = TOTPLK16(INDLAY) + 
     &           TLAYFRAC * DBDTLAY
            PLANKLEV(LAY,IBAND) = TOTPLK16(INDLEV) + 
     &           TLEVFRAC * DBDTLEV
         ELSE
            IF (LAY.EQ.1) THEN
               DBDTLEV = TOTPLNK(INDBOUND+1,IBAND)
     &              - TOTPLNK(INDBOUND,IBAND)
               PLANKBND(IBAND) = SEMISS(IBAND) * 
     &              (TOTPLNK(INDBOUND,IBAND) + TBNDFRAC * DBDTLEV)
               DBDTLEV = TOTPLNK(INDLEV0+1,IBAND)-TOTPLNK(INDLEV0,IBAND)
               PLANKLEV(0,IBAND) = TOTPLNK(INDLEV0,IBAND) + 
     &              T0FRAC * DBDTLEV
            ENDIF
            DBDTLEV = TOTPLNK(INDLEV+1,IBAND) - TOTPLNK(INDLEV,IBAND)
            DBDTLAY = TOTPLNK(INDLAY+1,IBAND) - TOTPLNK(INDLAY,IBAND)
            PLANKLAY(LAY,IBAND) = TOTPLNK(INDLAY,IBAND) + 
     &           TLAYFRAC * DBDTLAY
            PLANKLEV(LAY,IBAND) = TOTPLNK(INDLEV,IBAND) + 
     &           TLEVFRAC * DBDTLEV
         ENDIF

C        Find the two reference pressures on either side of the
C        layer pressure.  Store them in JP and JP1.  Store in FP the
C        fraction of the difference (in ln(pressure)) between these
C        two values that the layer pressure lies.
         PLOG = ALOG(PAVEL(LAY))
         JP(LAY) = INT(36. - 5*(PLOG+0.04))
         IF (JP(LAY) .LT. 1) THEN
            JP(LAY) = 1
         ELSEIF (JP(LAY) .GT. 58) THEN
            JP(LAY) = 58
         ENDIF
         JP1 = JP(LAY) + 1
         FP(LAY) = 5. *(PREFLOG(JP(LAY)) - PLOG)

C        Determine, for each reference pressure (JP and JP1), which
C        reference temperature (these are different for each  
C        reference pressure) is nearest the layer temperature but does
C        not exceed it.  Store these indices in JT and JT1, resp.
C        Store in FT (resp. FT1) the fraction of the way between JT
C        (JT1) and the next highest reference temperature that the 
C        layer temperature falls.
         JT(LAY) = INT(3. + (TAVEL(LAY)-TREF(JP(LAY)))/15.)
         IF (JT(LAY) .LT. 1) THEN
            JT(LAY) = 1
         ELSEIF (JT(LAY) .GT. 4) THEN
            JT(LAY) = 4
         ENDIF
         FT = ((TAVEL(LAY)-TREF(JP(LAY)))/15.) - FLOAT(JT(LAY)-3)
         JT1(LAY) = INT(3. + (TAVEL(LAY)-TREF(JP1))/15.)
         IF (JT1(LAY) .LT. 1) THEN
            JT1(LAY) = 1
         ELSEIF (JT1(LAY) .GT. 4) THEN
            JT1(LAY) = 4
         ENDIF
         FT1 = ((TAVEL(LAY)-TREF(JP1))/15.) - FLOAT(JT1(LAY)-3)
         WATER = WKL(1,LAY)/COLDRY(LAY)
         SCALEFAC = PAVEL(LAY) * STPFAC / TAVEL(LAY)

C        If the pressure is less than ~100mb, perform a different
C        set of species interpolations.
         IF (PLOG .LE. 4.56) GO TO 5300
         LAYTROP =  LAYTROP + 1
C        For one band, the "switch" occurs at ~300 mb. 
         IF (PLOG .GE. 5.76) LAYSWTCH = LAYSWTCH + 1
         IF (PLOG .GE. 6.62) LAYLOW = LAYLOW + 1
C
         FORFAC(LAY) = SCALEFAC / (1.+WATER)
         FACTOR = (332.0-TAVEL(LAY))/36.0
         INDFOR(LAY) = MIN(2, MAX(1, INT(FACTOR)))
         FORFRAC(LAY) = FACTOR - FLOAT(INDFOR(LAY))
C        Set up factors needed to separately include the water vapor
C        self-continuum in the calculation of absorption coefficient.

         SELFFAC(LAY) = WATER * FORFAC(LAY)
         FACTOR = (TAVEL(LAY)-188.0)/7.2
         INDSELF(LAY) = MIN(9, MAX(1, INT(FACTOR)-7))
         SELFFRAC(LAY) = FACTOR - FLOAT(INDSELF(LAY) + 7)

C        Calculate needed column amounts.
         COLH2O(LAY) = 1.E-20 * WKL(1,LAY)
         COLCO2(LAY) = 1.E-20 * WKL(2,LAY)
         COLO3(LAY) = 1.E-20 * WKL(3,LAY)
         COLN2O(LAY) = 1.E-20 * WKL(4,LAY)
         COLCO(LAY) = 1.E-20 * WKL(5,LAY)
         COLCH4(LAY) = 1.E-20 * WKL(6,LAY)
         COLO2(LAY) = 1.E-20 * WKL(7,LAY)
         IF (COLCO2(LAY) .EQ. 0.) COLCO2(LAY) = 1.E-32 * COLDRY(LAY)
         IF (COLN2O(LAY) .EQ. 0.) COLN2O(LAY) = 1.E-32 * COLDRY(LAY)
         IF (COLCH4(LAY) .EQ. 0.) COLCH4(LAY) = 1.E-32 * COLDRY(LAY)
C        Using E = 1334.2 cm-1.
         CO2REG = 3.55E-24 * COLDRY(LAY)
         CO2MULT(LAY)= (COLCO2(LAY) - CO2REG) *
     &        272.63*EXP(-1919.4/TAVEL(LAY))/(8.7604E-4*TAVEL(LAY))
         GO TO 5400

C        Above LAYTROP.
 5300    CONTINUE

         FORFAC(LAY) = SCALEFAC / (1.+WATER)
         FACTOR = (TAVEL(LAY)-188.0)/36.0
         INDFOR(LAY) = 3
         FORFRAC(LAY) = FACTOR - 1.0

C        Calculate needed column amounts.
         COLH2O(LAY) = 1.E-20 * WKL(1,LAY)
         COLCO2(LAY) = 1.E-20 * WKL(2,LAY)
         COLO3(LAY) = 1.E-20 * WKL(3,LAY)
         COLN2O(LAY) = 1.E-20 * WKL(4,LAY)
         COLCH4(LAY) = 1.E-20 * WKL(6,LAY)
         COLO2(LAY) = 1.E-20 * WKL(7,LAY)
         IF (COLCO2(LAY) .EQ. 0.) COLCO2(LAY) = 1.E-32 * COLDRY(LAY)
         IF (COLN2O(LAY) .EQ. 0.) COLN2O(LAY) = 1.E-32 * COLDRY(LAY)
         IF (COLCH4(LAY) .EQ. 0.) COLCH4(LAY) = 1.E-32 * COLDRY(LAY)
         CO2REG = 3.55E-24 * COLDRY(LAY)
         CO2MULT(LAY)= (COLCO2(LAY) - CO2REG) *
     &        272.63*EXP(-1919.4/TAVEL(LAY))/(8.7604E-4*TAVEL(LAY))
 5400    CONTINUE

C        We have now isolated the layer ln pressure and temperature,
C        between two reference pressures and two reference temperatures 
C        (for each reference pressure).  We multiply the pressure 
C        fraction FP with the appropriate temperature fractions to get 
C        the factors that will be needed for the interpolation that yields
C        the optical depths (performed in routines TAUGBn for band n).`

         COMPFP = 1. - FP(LAY)
         FAC10(LAY) = COMPFP * FT
         FAC00(LAY) = COMPFP * (1. - FT)
         FAC11(LAY) = FP(LAY) * FT1
         FAC01(LAY) = FP(LAY) * (1. - FT1)

 7000 CONTINUE

      RETURN
      END


      BLOCK DATA BLOCK_ATMREF
      DIMENSION PREF(59),PREFLOG(59),TREF(59),W_MLS(7,59)
      COMMON /MLS_REF/  PREF,PREFLOG,TREF,W_MLS
C     These pressures are chosen such that the ln of the first pressure
C     has only a few non-zero digits (i.e. ln(PREF(1)) = 6.96000) and
C     each subsequent ln(pressure) differs from the previous one by 0.2.
      DATA PREF /
     &    1.05363E+03,8.62642E+02,7.06272E+02,5.78246E+02,4.73428E+02,
     &    3.87610E+02,3.17348E+02,2.59823E+02,2.12725E+02,1.74164E+02,
     &    1.42594E+02,1.16746E+02,9.55835E+01,7.82571E+01,6.40715E+01,
     &    5.24573E+01,4.29484E+01,3.51632E+01,2.87892E+01,2.35706E+01,
     &    1.92980E+01,1.57998E+01,1.29358E+01,1.05910E+01,8.67114E+00,
     &    7.09933E+00,5.81244E+00,4.75882E+00,3.89619E+00,3.18993E+00,
     &    2.61170E+00,2.13828E+00,1.75067E+00,1.43333E+00,1.17351E+00,
     &    9.60789E-01,7.86628E-01,6.44036E-01,5.27292E-01,4.31710E-01,
     &    3.53455E-01,2.89384E-01,2.36928E-01,1.93980E-01,1.58817E-01,
     &    1.30029E-01,1.06458E-01,8.71608E-02,7.13612E-02,5.84256E-02,
     &    4.78349E-02,3.91639E-02,3.20647E-02,2.62523E-02,2.14936E-02,
     &    1.75975E-02,1.44076E-02,1.17959E-02,9.65769E-03/
      DATA PREFLOG /
     &     6.9600E+00, 6.7600E+00, 6.5600E+00, 6.3600E+00, 6.1600E+00,
     &     5.9600E+00, 5.7600E+00, 5.5600E+00, 5.3600E+00, 5.1600E+00,
     &     4.9600E+00, 4.7600E+00, 4.5600E+00, 4.3600E+00, 4.1600E+00,
     &     3.9600E+00, 3.7600E+00, 3.5600E+00, 3.3600E+00, 3.1600E+00,
     &     2.9600E+00, 2.7600E+00, 2.5600E+00, 2.3600E+00, 2.1600E+00,
     &     1.9600E+00, 1.7600E+00, 1.5600E+00, 1.3600E+00, 1.1600E+00,
     &     9.6000E-01, 7.6000E-01, 5.6000E-01, 3.6000E-01, 1.6000E-01,
     &    -4.0000E-02,-2.4000E-01,-4.4000E-01,-6.4000E-01,-8.4000E-01,
     &    -1.0400E+00,-1.2400E+00,-1.4400E+00,-1.6400E+00,-1.8400E+00,
     &    -2.0400E+00,-2.2400E+00,-2.4400E+00,-2.6400E+00,-2.8400E+00,
     &    -3.0400E+00,-3.2400E+00,-3.4400E+00,-3.6400E+00,-3.8400E+00,
     &    -4.0400E+00,-4.2400E+00,-4.4400E+00,-4.6400E+00/
C     These are the temperatures associated with the respective 
C     pressures for the MLS standard atmosphere. 
      DATA TREF /
     &     2.9420E+02, 2.8799E+02, 2.7894E+02, 2.6925E+02, 2.5983E+02,
     &     2.5017E+02, 2.4077E+02, 2.3179E+02, 2.2306E+02, 2.1578E+02,
     &     2.1570E+02, 2.1570E+02, 2.1570E+02, 2.1706E+02, 2.1858E+02,
     &     2.2018E+02, 2.2174E+02, 2.2328E+02, 2.2479E+02, 2.2655E+02,
     &     2.2834E+02, 2.3113E+02, 2.3401E+02, 2.3703E+02, 2.4022E+02,
     &     2.4371E+02, 2.4726E+02, 2.5085E+02, 2.5457E+02, 2.5832E+02,
     &     2.6216E+02, 2.6606E+02, 2.6999E+02, 2.7340E+02, 2.7536E+02,
     &     2.7568E+02, 2.7372E+02, 2.7163E+02, 2.6955E+02, 2.6593E+02,
     &     2.6211E+02, 2.5828E+02, 2.5360E+02, 2.4854E+02, 2.4348E+02,
     &     2.3809E+02, 2.3206E+02, 2.2603E+02, 2.2000E+02, 2.1435E+02,
     &     2.0887E+02, 2.0340E+02, 1.9792E+02, 1.9290E+02, 1.8809E+02,
     &     1.8329E+02, 1.7849E+02, 1.7394E+02, 1.7212E+02/

       DATA (W_MLS(1,IP),IP=1,12) /
     &  1.8760E-02, 1.2223E-02, 5.8909E-03, 2.7675E-03, 1.4065E-03,
     &  7.5970E-04, 3.8876E-04, 1.6542E-04, 3.7190E-05, 7.4765E-06,
     &  4.3082E-06, 3.3319E-06/
       DATA (W_MLS(1,IP),IP=13,59)/
     &  3.2039E-06,  3.1619E-06,  3.2524E-06,  3.4226E-06,  3.6288E-06,
     &  3.9148E-06,  4.1488E-06,  4.3081E-06,  4.4420E-06,  4.5778E-06,
     &  4.7087E-06,  4.7943E-06,  4.8697E-06,  4.9260E-06,  4.9669E-06,
     &  4.9963E-06,  5.0527E-06,  5.1266E-06,  5.2503E-06,  5.3571E-06,
     &  5.4509E-06,  5.4830E-06,  5.5000E-06,  5.5000E-06,  5.4536E-06,
     &  5.4047E-06,  5.3558E-06,  5.2533E-06,  5.1436E-06,  5.0340E-06,
     &  4.8766E-06,  4.6979E-06,  4.5191E-06,  4.3360E-06,  4.1442E-06,
     &  3.9523E-06,  3.7605E-06,  3.5722E-06,  3.3855E-06,  3.1988E-06,
     &  3.0121E-06,  2.8262E-06,  2.6407E-06,  2.4552E-06,  2.2696E-06,
     &  4.3360E-06,  4.1442E-06/
       DATA (W_MLS(2,IP),IP=1,12)/
     &  3.5500E-04,  3.5500E-04,  3.5500E-04,  3.5500E-04,  3.5500E-04,
     &  3.5500E-04,  3.5500E-04,  3.5500E-04,  3.5500E-04,  3.5500E-04,
     &  3.5500E-04,  3.5500E-04/
       DATA (W_MLS(2,IP),IP=13,59)/
     &  3.5500E-04,  3.5500E-04,  3.5500E-04,  3.5500E-04,  3.5500E-04,
     &  3.5500E-04,  3.5500E-04,  3.5500E-04,  3.5500E-04,  3.5500E-04,
     &  3.5500E-04,  3.5500E-04,  3.5500E-04,  3.5500E-04,  3.5500E-04,
     &  3.5500E-04,  3.5500E-04,  3.5500E-04,  3.5500E-04,  3.5500E-04,
     &  3.5500E-04,  3.5500E-04,  3.5500E-04,  3.5500E-04,  3.5500E-04,
     &  3.5500E-04,  3.5500E-04,  3.5500E-04,  3.5500E-04,  3.5500E-04,
     &  3.5500E-04,  3.5500E-04,  3.5500E-04,  3.5500E-04,  3.5500E-04,
     &  3.5500E-04,  3.5500E-04,  3.5500E-04,  3.5500E-04,  3.5500E-04,
     &  3.5500E-04,  3.5471E-04,  3.5427E-04,  3.5384E-04,  3.5340E-04,
     &  3.5500E-04,  3.5500E-04/
       DATA (W_MLS(3,IP),IP=1,12)/
     &  3.0170E-08,  3.4725E-08,  4.2477E-08,  5.2759E-08,  6.6944E-08,
     &  8.7130E-08,  1.1391E-07,  1.5677E-07,  2.1788E-07,  3.2443E-07,
     &  4.6594E-07,  5.6806E-07/
       DATA (W_MLS(3,IP),IP=13,59)/
     &  6.9607E-07,  1.1186E-06,  1.7618E-06,  2.3269E-06,  2.9577E-06,
     &  3.6593E-06,  4.5950E-06,  5.3189E-06,  5.9618E-06,  6.5113E-06,
     &  7.0635E-06,  7.6917E-06,  8.2577E-06,  8.7082E-06,  8.8325E-06,
     &  8.7149E-06,  8.0943E-06,  7.3307E-06,  6.3101E-06,  5.3672E-06,
     &  4.4829E-06,  3.8391E-06,  3.2827E-06,  2.8235E-06,  2.4906E-06,
     &  2.1645E-06,  1.8385E-06,  1.6618E-06,  1.5052E-06,  1.3485E-06,
     &  1.1972E-06,  1.0482E-06,  8.9926E-07,  7.6343E-07,  6.5381E-07,
     &  5.4419E-07,  4.3456E-07,  3.6421E-07,  3.1194E-07,  2.5967E-07,
     &  2.0740E-07,  1.9146E-07,  1.9364E-07,  1.9582E-07,  1.9800E-07,
     &  7.6343E-07,  6.5381E-07/
       DATA (W_MLS(4,IP),IP=1,12)/
     &  3.2000E-07,  3.2000E-07,  3.2000E-07,  3.2000E-07,  3.2000E-07,
     &  3.1965E-07,  3.1532E-07,  3.0383E-07,  2.9422E-07,  2.8495E-07,
     &  2.7671E-07,  2.6471E-07/
       DATA (W_MLS(4,IP),IP=13,59)/
     &  2.4285E-07,  2.0955E-07,  1.7195E-07,  1.3749E-07,  1.1332E-07,
     &  1.0035E-07,  9.1281E-08,  8.5463E-08,  8.0363E-08,  7.3372E-08,
     &  6.5975E-08,  5.6039E-08,  4.7090E-08,  3.9977E-08,  3.2979E-08,
     &  2.6064E-08,  2.1066E-08,  1.6592E-08,  1.3017E-08,  1.0090E-08,
     &  7.6249E-09,  6.1159E-09,  4.6672E-09,  3.2857E-09,  2.8484E-09,
     &  2.4620E-09,  2.0756E-09,  1.8551E-09,  1.6568E-09,  1.4584E-09,
     &  1.3195E-09,  1.2072E-09,  1.0948E-09,  9.9780E-10,  9.3126E-10,
     &  8.6472E-10,  7.9818E-10,  7.5138E-10,  7.1367E-10,  6.7596E-10,
     &  6.3825E-10,  6.0981E-10,  5.8600E-10,  5.6218E-10,  5.3837E-10,
     &  9.9780E-10,  9.3126E-10/
       DATA (W_MLS(5,IP),IP=1,12)/
     &  1.5000E-07,  1.4306E-07,  1.3474E-07,  1.3061E-07,  1.2793E-07,
     &  1.2038E-07,  1.0798E-07,  9.4238E-08,  7.9488E-08,  6.1386E-08,
     &  4.5563E-08,  3.3475E-08/
       DATA (W_MLS(5,IP),IP=13,59)/
     &  2.5118E-08,  1.8671E-08,  1.4349E-08,  1.2501E-08,  1.2407E-08,
     &  1.3472E-08,  1.4900E-08,  1.6079E-08,  1.7156E-08,  1.8616E-08,
     &  2.0106E-08,  2.1654E-08,  2.3096E-08,  2.4340E-08,  2.5643E-08,
     &  2.6990E-08,  2.8456E-08,  2.9854E-08,  3.0943E-08,  3.2023E-08,
     &  3.3101E-08,  3.4260E-08,  3.5360E-08,  3.6397E-08,  3.7310E-08,
     &  3.8217E-08,  3.9123E-08,  4.1303E-08,  4.3652E-08,  4.6002E-08,
     &  5.0289E-08,  5.5446E-08,  6.0603E-08,  6.8946E-08,  8.3652E-08,
     &  9.8357E-08,  1.1306E-07,  1.4766E-07,  1.9142E-07,  2.3518E-07,
     &  2.7894E-07,  3.5001E-07,  4.3469E-07,  5.1938E-07,  6.0407E-07,
     &  6.8946E-08,  8.3652E-08/
       DATA (W_MLS(6,IP),IP=1,12) /
     &  1.7000E-06,  1.7000E-06,  1.6999E-06,  1.6904E-06,  1.6671E-06,
     &  1.6351E-06,  1.6098E-06,  1.5590E-06,  1.5120E-06,  1.4741E-06,
     &  1.4385E-06,  1.4002E-06/
       DATA (W_MLS(6,IP),IP=13,59)/
     &  1.3573E-06,  1.3130E-06,  1.2512E-06,  1.1668E-06,  1.0553E-06,
     &  9.3281E-07,  8.1217E-07,  7.5239E-07,  7.0728E-07,  6.6722E-07,
     &  6.2733E-07,  5.8604E-07,  5.4769E-07,  5.1480E-07,  4.8206E-07,
     &  4.4943E-07,  4.1702E-07,  3.8460E-07,  3.5200E-07,  3.1926E-07,
     &  2.8646E-07,  2.5498E-07,  2.2474E-07,  1.9588E-07,  1.8295E-07,
     &  1.7089E-07,  1.5882E-07,  1.5536E-07,  1.5304E-07,  1.5072E-07,
     &  1.5000E-07,  1.5000E-07,  1.5000E-07,  1.5000E-07,  1.5000E-07,
     &  1.5000E-07,  1.5000E-07,  1.5000E-07,  1.5000E-07,  1.5000E-07,
     &  1.5000E-07,  1.5000E-07,  1.5000E-07,  1.5000E-07,  1.5000E-07,
     &  1.5000E-07,  1.5000E-07/
       DATA (W_MLS(7,IP),IP=1,12)/
     &  0.2090    ,  0.2090    ,  0.2090    ,  0.2090    ,  0.2090    ,
     &  0.2090    ,  0.2090    ,  0.2090    ,  0.2090    ,  0.2090    ,
     &  0.2090    ,  0.2090    /
       DATA (W_MLS(7,IP),IP=13,59)/
     &  0.2090    ,  0.2090    ,  0.2090    ,  0.2090    ,  0.2090    ,
     &  0.2090    ,  0.2090    ,  0.2090    ,  0.2090    ,  0.2090    ,
     &  0.2090    ,  0.2090    ,  0.2090    ,  0.2090    ,  0.2090    ,
     &  0.2090    ,  0.2090    ,  0.2090    ,  0.2090    ,  0.2090    ,
     &  0.2090    ,  0.2090    ,  0.2090    ,  0.2090    ,  0.2090    ,
     &  0.2090    ,  0.2090    ,  0.2090    ,  0.2090    ,  0.2090    ,
     &  0.2090    ,  0.2090    ,  0.2090    ,  0.2090    ,  0.2090    ,
     &  0.2090    ,  0.2090    ,  0.2090    ,  0.2090    ,  0.2090    ,
     &  0.2090    ,  0.2090    ,  0.2090    ,  0.2090    ,  0.2090    ,
     &  0.2090    ,  0.2090    /

       END
