C     path:      $Source$
C     author:    $Author: jdelamer $
C     revision:  $Revision: 11497 $
C     created:   $Date: 2010-07-07 17:10:53 -0400 (Wed, 07 Jul 2010) $
C
C  --------------------------------------------------------------------------
C |                                                                          |
C |  Copyright 2002, 2003, Atmospheric & Environmental Research, Inc. (AER). |
C |  This software may be used, copied, or redistributed as long as it is    |
C |  not sold and this copyright notice is reproduced on each copy made.     |
C |  This model is provided as is without any express or implied warranties. |
C |                       (http://www.rtweb.aer.com/)                        |
C |                                                                          |
C  --------------------------------------------------------------------------

      SUBROUTINE RTRCLDMR

C *** This program calculates the upward fluxes, downward fluxes,
C     and heating rates for an arbitrary cloudy atmosphere.  The input 
C     to this program is the atmospheric profile, including cloud 
C     properties, and all Planck function information. Only one angle 
C     is used from standard Gaussian quadrature. 

C     Clouds are treated with maximum/random overlap scheme
      PARAMETER (MG=16)
      PARAMETER (MXLAY=603)
      PARAMETER (MXANG = 4)
      PARAMETER (NBANDS = 16)
      PARAMETER (NTBL = 10000,TBLINT=10000.0)

      IMPLICIT DOUBLE PRECISION (V)                                     

      COMMON /CONSTANTS/ FLUXFAC,HEATFAC
      COMMON /CONSTS/ PI,PLANCK,BOLTZ,CLIGHT,AVOGAD,ALOSMT,GASCON,
     *                RADCN1,RADCN2,GRAV,CPDAIR,AIRMWT,SECDY 
      COMMON /FEATURES/  NG(NBANDS),NSPA(NBANDS),NSPB(NBANDS)
      COMMON /BANDS/     WAVENUM1(NBANDS),WAVENUM2(NBANDS),
     &                   DELWAVE(NBANDS)
      COMMON /CONTROL/  NUMANGS, ISCAT, NSTR, 
     &                  IOUT, ISTART, IEND, ICLD
      COMMON /PROFILE/   NLAYERS,PAVEL(MXLAY),TAVEL(MXLAY),
     &                   PZ(0:MXLAY),TZ(0:MXLAY)
      COMMON /SURFACE/   TBOUND,IREFLECT,SEMISS(NBANDS)
      COMMON /CLOUDDAT/  NCBANDS,CLDFRAC(MXLAY),TAUCLOUD(MXLAY,NBANDS)
      COMMON /PLNKDAT/   PLANKLAY(MXLAY,NBANDS),
     &                   PLANKLEV(0:MXLAY,NBANDS),PLANKBND(NBANDS)
      COMMON /PLANKG/    FRACS(MXLAY,MG)
      COMMON /TAUGCOM/   TAUG(MXLAY,MG)
      COMMON /OUTPUT/    TOTUFLUX(0:MXLAY), TOTDFLUX(0:MXLAY),
     &                   FNET(0:MXLAY), HTR(0:MXLAY)
      COMMON /RTTBL/     BPADE,
     &                   TAUTBL(0:NTBL),TRANS(0:NTBL),TF(0:NTBL)

      COMMON /CVRRTX/    HNAMRTX,HVRRTX

      CHARACTER*18       HNAMRTX,HVRRTX
                                       
      DIMENSION BBUGAS(MXLAY)
      DIMENSION BBUTOT(MXLAY)
      DIMENSION ATRANS(MXLAY)
      DIMENSION UFLUX(0:MXLAY),DFLUX(0:MXLAY)
      DIMENSION DRAD(0:MXLAY),URAD(0:MXLAY)
      DIMENSION ODCLD(MXLAY,NBANDS)
      DIMENSION ATOT(MXLAY)
      DIMENSION ABSCLD(MXLAY,NBANDS)
      DIMENSION IPAT(16,0:2)

C Dimensions for cloud overlap adjustment
      DIMENSION ICLDLYR(MXLAY)
      DIMENSION FACCLD1(MXLAY+1),FACCLD2(MXLAY+1)
      DIMENSION FACCLR1(MXLAY+1),FACCLR2(MXLAY+1),ISTCLD(MXLAY+1)
      DIMENSION FACCMB1(MXLAY+1),FACCMB2(MXLAY+1)
      DIMENSION FACCLD1D(0:MXLAY),FACCLD2D(0:MXLAY),FACCLR1D(0:MXLAY)
      DIMENSION FACCLR2D(0:MXLAY),ISTCLDD(0:MXLAY),FACCMB1D(0:MXLAY)
      DIMENSION FACCMB2D(0:MXLAY)

C     These arrays indicate the spectral 'region' (used in the 
C     calculation of ice cloud optical depths) corresponding
C     to each spectral band.  See cldprop.f for more details.
      DATA IPAT /1,1,1,1,1,1,1,1,1, 1, 1, 1, 1, 1, 1, 1,
     &           1,2,3,3,3,4,4,4,5, 5, 5, 5, 5, 5, 5, 5,
     &           1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16/

C *** These weights correspond to angles of 24.30,53.81,77.75
C     degrees, resp.  They are used when "numerical" Gaussian
C     quadrature is chosen.
      DATA SECDIFF /1.66/
      DATA WTDIFF /0.5/
      DATA REC_6 /0.166667/

      HVRRTX = '$Revision: 11497 $'

      URAD(0) = 0.0
      DRAD(0) = 0.0
      TOTUFLUX(0) = 0.0
      TOTDFLUX(0) = 0.0
      DO 200 LAY = 1, NLAYERS
         URAD(LAY) = 0.0
         DRAD(LAY) = 0.0
         TOTUFLUX(LAY) = 0.0
         TOTDFLUX(LAY) = 0.0
         DO 100 IB = 1, NCBANDS
            IF (CLDFRAC(LAY) .GE. 1.E-6) THEN
               ODCLD(LAY,IB) = SECDIFF * TAUCLOUD(LAY,IB)
               TRANSCLD = EXP(-ODCLD(LAY,IB))
               ABSCLD(LAY,IB) = 1. - TRANSCLD
               ICLDLYR(LAY) = 1
            ELSE
               ODCLD(LAY,IB) = 0.0
               ABSCLD(LAY,IB) = 0.0
               ICLDLYR(LAY) = 0
            ENDIF
 100     CONTINUE
 200  CONTINUE

C *** Maximum/Random cloud overlap parameter

      ISTCLD(1) = 1
      ISTCLDD(NLAYERS) = 1
      DO 220 LEV = 1, NLAYERS

         IF (ICLDLYR(LEV).EQ.1) THEN
C  Maximum/random cloud overlap
            ISTCLD(LEV+1) = 0
            IF (LEV .EQ. NLAYERS) THEN
               FACCLD1(LEV+1) = 0.
               FACCLD2(LEV+1) = 0.
               FACCLR1(LEV+1) = 0.
               FACCLR2(LEV+1) = 0.
               FACCMB1(LEV+1) = 0.
               FACCMB2(LEV+1) = 0.
            ELSEIF (CLDFRAC(LEV+1) .GE. CLDFRAC(LEV)) THEN
               FACCLD1(LEV+1) = 0.
               FACCLD2(LEV+1) = 0.
               IF (ISTCLD(LEV) .EQ. 1) THEN
                  FACCLR1(LEV+1) = 0.
                  FACCLR2(LEV+1) = 0.
                  IF (CLDFRAC(LEV) .LT. 1.) FACCLR2(LEV+1) =
     &                 (CLDFRAC(LEV+1)-CLDFRAC(LEV))/(1.-CLDFRAC(LEV))
                  FACCLR2(LEV) = 0.
                  FACCLD2(LEV) = 0.
               ELSE
                  FMAX = MAX(CLDFRAC(LEV),CLDFRAC(LEV-1))
                  IF (CLDFRAC(LEV+1) .GT. FMAX) THEN
                     FACCLR1(LEV+1) = RAT2
                     FACCLR2(LEV+1) = (CLDFRAC(LEV+1)-FMAX)/(1.-FMAX)
                  ELSEIF (CLDFRAC(LEV+1) .LT. FMAX) THEN
                     FACCLR1(LEV+1) = (CLDFRAC(LEV+1)-CLDFRAC(LEV))/
     &                    (CLDFRAC(LEV-1)-CLDFRAC(LEV))
                     FACCLR2(LEV+1) = 0.
                  ELSE
                     FACCLR1(LEV+1) = RAT2
                     FACCLR2(LEV+1) = 0.
                  ENDIF
               ENDIF
               IF (FACCLR1(LEV+1).GT.0. .OR. FACCLR2(LEV+1).GT.0.) THEN
                  RAT1 = 1.
                  RAT2 = 0.
               ELSE
                  RAT1 = 0.
                  RAT2 = 0.
               ENDIF
            ELSE
               FACCLR1(LEV+1) = 0.
               FACCLR2(LEV+1) = 0.
               IF (ISTCLD(LEV) .EQ. 1) THEN
                  FACCLD1(LEV+1) = 0.
                  FACCLD2(LEV+1) = (CLDFRAC(LEV)-CLDFRAC(LEV+1))/
     &                 CLDFRAC(LEV)

                  FACCLR2(LEV) = 0.
                  FACCLD2(LEV) = 0.
               ELSE
                  FMIN = MIN(CLDFRAC(LEV),CLDFRAC(LEV-1))
                  IF (CLDFRAC(LEV+1) .LE. FMIN) THEN
                     FACCLD1(LEV+1) = RAT1
                     FACCLD2(LEV+1) = (FMIN-CLDFRAC(LEV+1))/FMIN
                  ELSE
                     FACCLD1(LEV+1) = (CLDFRAC(LEV)-CLDFRAC(LEV+1))/
     &                    (CLDFRAC(LEV)-FMIN)
                     FACCLD2(LEV+1) = 0.
                  ENDIF
               ENDIF
               IF (FACCLD1(LEV+1).GT.0. .OR. FACCLD2(LEV+1).GT.0.) THEN
                  RAT1 = 0.
                  RAT2 = 1.
               ELSE
                  RAT1 = 0.
                  RAT2 = 0.
               ENDIF
            ENDIF
            FACCMB1(LEV+1) = FACCLR1(LEV+1) * FACCLD2(LEV) *
     &           CLDFRAC(LEV-1) 
            FACCMB2(LEV+1) = FACCLD1(LEV+1) * FACCLR2(LEV) *
     &           (1. - CLDFRAC(LEV-1)) 
         ELSE
            ISTCLD(LEV+1) = 1
         ENDIF
220   CONTINUE

      DO 320 LEV = NLAYERS, 1, -1
         IF (ICLDLYR(LEV).EQ.1) THEN
            ISTCLDD(LEV-1) = 0
            IF (LEV .EQ. 1) THEN
               FACCLD1D(LEV-1) = 0.
               FACCLD2D(LEV-1) = 0.
               FACCLR1D(LEV-1) = 0.
               FACCLR2D(LEV-1) = 0.
               FACCMB1D(LEV-1) = 0.
               FACCMB2D(LEV-1) = 0.
            ELSEIF (CLDFRAC(LEV-1) .GE. CLDFRAC(LEV)) THEN
               FACCLD1D(LEV-1) = 0.
               FACCLD2D(LEV-1) = 0.
               IF (ISTCLDD(LEV) .EQ. 1) THEN
                  FACCLR1D(LEV-1) = 0.
                  FACCLR2D(LEV-1) = 0.
                  IF (CLDFRAC(LEV) .LT. 1.) FACCLR2D(LEV-1) =
     &                 (CLDFRAC(LEV-1)-CLDFRAC(LEV))/(1.-CLDFRAC(LEV))
                  FACCLR2D(LEV) = 0.
                  FACCLD2D(LEV) = 0.
               ELSE
                  FMAX = MAX(CLDFRAC(LEV),CLDFRAC(LEV+1))
                  IF (CLDFRAC(LEV-1) .GT. FMAX) THEN
                     FACCLR1D(LEV-1) = RAT2
                     FACCLR2D(LEV-1) = (CLDFRAC(LEV-1)-FMAX)/(1.-FMAX)
                  ELSEIF (CLDFRAC(LEV-1) .LT. FMAX) THEN
                     FACCLR1D(LEV-1) = (CLDFRAC(LEV-1)-CLDFRAC(LEV))/
     &                    (CLDFRAC(LEV+1)-CLDFRAC(LEV))
                     FACCLR2D(LEV-1) = 0.
                  ELSE
                     FACCLR1D(LEV-1) = RAT2
                     FACCLR2D(LEV-1) = 0.
                  ENDIF
               ENDIF
               IF (FACCLR1D(LEV-1).GT.0. .OR. FACCLR2D(LEV-1).GT.0.)THEN
                  RAT1 = 1.
                  RAT2 = 0.
               ELSE
                  RAT1 = 0.
                  RAT2 = 0.
               ENDIF
            ELSE
               FACCLR1D(LEV-1) = 0.
               FACCLR2D(LEV-1) = 0.
               IF (ISTCLDD(LEV) .EQ. 1) THEN
                  FACCLD1D(LEV-1) = 0.
                  FACCLD2D(LEV-1) = (CLDFRAC(LEV)-CLDFRAC(LEV-1))/
     &                 CLDFRAC(LEV)
                  FACCLR2D(LEV) = 0.
                  FACCLD2D(LEV) = 0.
               ELSE
                  FMIN = MIN(CLDFRAC(LEV),CLDFRAC(LEV+1))
                  IF (CLDFRAC(LEV-1) .LE. FMIN) THEN
                     FACCLD1D(LEV-1) = RAT1
                     FACCLD2D(LEV-1) = (FMIN-CLDFRAC(LEV-1))/FMIN
                  ELSE
                     FACCLD1D(LEV-1) = (CLDFRAC(LEV)-CLDFRAC(LEV-1))/
     &                    (CLDFRAC(LEV)-FMIN)
                     FACCLD2D(LEV-1) = 0.
                  ENDIF
               ENDIF
               IF (FACCLD1D(LEV-1).GT.0. .OR. FACCLD2D(LEV-1).GT.0.)THEN
                  RAT1 = 0.
                  RAT2 = 1.
               ELSE
                  RAT1 = 0.
                  RAT2 = 0.
               ENDIF
            ENDIF
            FACCMB1D(LEV-1) = FACCLR1D(LEV-1) * FACCLD2D(LEV) *
     &           CLDFRAC(LEV+1) 
            FACCMB2D(LEV-1) = FACCLD1D(LEV-1) * FACCLR2D(LEV) *
     &           (1. - CLDFRAC(LEV+1))
         ELSE
            ISTCLDD(LEV-1) = 1
         ENDIF
320   CONTINUE

C *** Loop over frequency bands.
      DO 6000 IBAND = ISTART, IEND
         IF (NCBANDS .EQ. 1) THEN
            IB = IPAT(IBAND,0)
         ELSEIF (NCBANDS .EQ.  5) THEN
            IB = IPAT(IBAND,1)
         ELSEIF (NCBANDS .EQ. 16) THEN
            IB = IPAT(IBAND,2)
         ENDIF

         IF (IBAND .EQ. 1) THEN
            CALL TAUGB1
         ELSEIF (IBAND .EQ. 2) THEN
            CALL TAUGB2
         ELSEIF (IBAND .EQ. 3) THEN
            CALL TAUGB3
         ELSEIF (IBAND .EQ. 4) THEN
            CALL TAUGB4
         ELSEIF (IBAND .EQ. 5) THEN
            CALL TAUGB5
         ELSEIF (IBAND .EQ. 6) THEN
            CALL TAUGB6
         ELSEIF (IBAND .EQ. 7) THEN
            CALL TAUGB7
         ELSEIF (IBAND .EQ. 8) THEN
            CALL TAUGB8
         ELSEIF (IBAND .EQ. 9) THEN
            CALL TAUGB9
         ELSEIF (IBAND .EQ. 10) THEN
            CALL TAUGB10
         ELSEIF (IBAND .EQ. 11) THEN
            CALL TAUGB11
         ELSEIF (IBAND .EQ. 12) THEN
            CALL TAUGB12
         ELSEIF (IBAND .EQ. 13) THEN
            CALL TAUGB13
         ELSEIF (IBAND .EQ. 14) THEN
            CALL TAUGB14
         ELSEIF (IBAND .EQ. 15) THEN
            CALL TAUGB15
         ELSEIF (IBAND .EQ. 16) THEN
            CALL TAUGB16
         ENDIF
        
C ***    Loop over g-channels.
         IG = 1
 1000    CONTINUE
C ***    Radiative transfer starts here.
         RADLD = 0.
         ICLDDN = 0 
C ***    Downward radiative transfer loop.  
C           Here are some variable definitions:
C             ODTOT      optical depth of gas and cloud
C             ATRANS     absorptivity for gas only
C             ATOT       absorptivity for gas and cloud
C             TFACGAS    gas-only Pade factor, used for Planck fn
C             TFACTOT    gas and cloud Pade factor, used for Planck fn
C             BBDGAS     gas-only Planck function for downward rt
C             BBDTOT     gas and cloud Planck function for downward rt
C             BBUTOT     gas and cloud Planck function for upward calc.
C             GASSRC     source radiance due to gas only

         DO 2500 LEV = NLAYERS, 1, -1

               PLFRAC = FRACS(LEV,IG)
               BLAY = PLANKLAY(LEV,IBAND)
               DPLANKUP = PLANKLEV(LEV,IBAND) - BLAY
               DPLANKDN = PLANKLEV(LEV-1,IBAND) - BLAY
               ODEPTH = SECDIFF * TAUG(LEV,IG)
               IF (ODEPTH .LT. 0.0) ODEPTH = 0.0

               IF (ICLDLYR(LEV).EQ.1) THEN
                  ODTOT = ODEPTH + ODCLD(LEV,IB)
                  IF (ODTOT .LT. 0.06) THEN
                     ATRANS(LEV) = ODEPTH - 0.5*ODEPTH*ODEPTH
                     ODEPTH_REC = REC_6*ODEPTH
                     GASSRC = PLFRAC*(BLAY+DPLANKDN*ODEPTH_REC)
     &                  *ATRANS(LEV)

                     ATOT(LEV) =  ODTOT - 0.5*ODTOT*ODTOT
                     ODTOT_REC = REC_6*ODTOT
                     BBDTOT =  PLFRAC * (BLAY+DPLANKDN*ODTOT_REC)
                  
                     BBUGAS(LEV) =  PLFRAC *
     &                 (BLAY+DPLANKUP*ODEPTH_REC)
                     BBUTOT(LEV) =  PLFRAC * 
     &                 (BLAY+DPLANKUP*ODTOT_REC)
                  ELSEIF (ODEPTH .LE. 0.06) THEN
                     ATRANS(LEV) = ODEPTH - 0.5*ODEPTH*ODEPTH
                     ODEPTH_REC = REC_6*ODEPTH
                     GASSRC = PLFRAC*(BLAY+DPLANKDN*ODEPTH_REC)
     &                  *ATRANS(LEV)

                     ODTOT = ODEPTH + ODCLD(LEV,IB)
                     TBLIND = ODTOT/(BPADE+ODTOT)
                     ITTOT = TBLINT*TBLIND + 0.5
                     TFACTOT = TF(ITTOT)
                     BBDTOT = PLFRAC * (BLAY + TFACTOT*DPLANKDN)
                     ATOT(LEV) = 1. - TRANS(ITTOT)

                     BBUGAS(LEV) = PLFRAC * 
     &                 (BLAY + DPLANKUP*ODEPTH_REC)
                     BBUTOT(LEV) = PLFRAC * 
     &                 (BLAY + TFACTOT * DPLANKUP)
                  ELSE
                     TBLIND = ODEPTH/(BPADE+ODEPTH)
                     ITGAS = TBLINT*TBLIND+0.5
                     ODEPTH = TAUTBL(ITGAS)
                     ATRANS(LEV) = 1. - TRANS(ITGAS)
                     TFACGAS = TF(ITGAS)
                     GASSRC = ATRANS(LEV) * PLFRAC * 
     &                    (BLAY + TFACGAS*DPLANKDN)

                     ODTOT = ODEPTH + ODCLD(LEV,IB)
                     TBLIND = ODTOT/(BPADE+ODTOT)
                     ITTOT = TBLINT*TBLIND + 0.5
                     TFACTOT = TF(ITTOT)
                     BBDTOT = PLFRAC * (BLAY + TFACTOT*DPLANKDN)
                     ATOT(LEV) = 1. - TRANS(ITTOT)

                     BBUGAS(LEV) = PLFRAC * 
     &                 (BLAY + TFACGAS * DPLANKUP)
                     BBUTOT(LEV) = PLFRAC * 
     &                 (BLAY + TFACTOT * DPLANKUP)
                  ENDIF

                  IF (ISTCLDD(LEV) .EQ. 1) THEN
                     CLDRADD = CLDFRAC(LEV) * RADLD
                     CLRRADD = RADLD - CLDRADD
                     OLDCLD = CLDRADD
                     OLDCLR = CLRRADD
                     RAD = 0.
                  ENDIF
                  TTOT = 1. - ATOT(LEV)
                  CLDSRC = BBDTOT * ATOT(LEV)
                  CLDRADD = CLDRADD * TTOT + 
     &                 CLDFRAC(LEV) * CLDSRC
                  CLRRADD = CLRRADD * (1.-ATRANS(LEV)) +
     &                 (1. - CLDFRAC(LEV)) * GASSRC
                  radld = cldradd + clrradd
                  DRAD(LEV-1) = DRAD(LEV-1) + RADLD

                  RADMOD = RAD * 
     &                 (FACCLR1D(LEV-1) * (1.-ATRANS(LEV)) +
     &                 FACCLD1D(LEV-1) *  TTOT) - 
     &                 FACCMB1D(LEV-1) * GASSRC + 
     &                 FACCMB2D(LEV-1) * CLDSRC

                  OLDCLD = CLDRADD - RADMOD
                  OLDCLR = CLRRADD + RADMOD
                  RAD = -RADMOD + FACCLR2D(LEV-1)*OLDCLR -
     &                 FACCLD2D(LEV-1)*OLDCLD
                  CLDRADD = CLDRADD + RAD
                  CLRRADD = CLRRADD - RAD
               ELSE
                  IF (ODEPTH .LE. 0.06) THEN
                     ATRANS(LEV) = ODEPTH-0.5*ODEPTH*ODEPTH
                     ODEPTH = REC_6*ODEPTH
                     BBD = PLFRAC*(BLAY+DPLANKDN*ODEPTH)
                     BBUGAS(LEV) = PLFRAC*
     &                    (BLAY+DPLANKUP*ODEPTH)
                  ELSE
                     TBLIND = ODEPTH/(BPADE+ODEPTH)
                     ITR = TBLINT*TBLIND+0.5
                     TRANSC = TRANS(ITR)
                     ATRANS(LEV) = 1.-TRANSC
                     TAUSFAC = TF(ITR)
                     BBD = PLFRAC*(BLAY+TAUSFAC*DPLANKDN)
                     BBUGAS(LEV) = PLFRAC * 
     &                    (BLAY + TAUSFAC * DPLANKUP)
                  ENDIF   
                  RADLD = RADLD + (BBD-RADLD)*ATRANS(LEV)
                  DRAD(LEV-1) = DRAD(LEV-1) + RADLD
                ENDIF
 2500       CONTINUE

C ***    Upward radiative transfer.
         RAD0 = FRACS(1,IG) * PLANKBND(IBAND)
C        Add in reflection of surface downward radiance.
         REFLECT = 1. - SEMISS(IBAND)
         IF (IREFLECT .EQ. 1) THEN
C           Specular reflection.
            RADLU = RAD0 + REFLECT * RADLD
         ELSE
C           Lambertian reflection.
            RAD = 2. * RADLD*WTDIFF
            RADLU = RAD0 + REFLECT * RAD
         ENDIF
         URAD(0) = URAD(0) + RADLU
         DO 2600 LEV = 1, NLAYERS
            IF (ICLDLYR(LEV) .EQ. 1) THEN
               GASSRC = BBUGAS(LEV) * ATRANS(LEV)
               IF (ISTCLD(LEV) .EQ. 1) THEN
                  CLDRADU = CLDFRAC(LEV) * RADLU
                  CLRRADU = RADLU - CLDRADU
                  OLDCLD = CLDRADU
                  OLDCLR = CLRRADU
                  RAD = 0.
               ENDIF
               TTOT = 1. - ATOT(LEV)
               CLDSRC = BBUTOT(LEV) * ATOT(LEV)
               CLDRADU = CLDRADU * TTOT + 
     &              CLDFRAC(LEV) * CLDSRC
               CLRRADU = CLRRADU * (1.0-ATRANS(LEV)) +
     &              (1. - CLDFRAC(LEV)) * GASSRC
C     Total sky radiance
               RADLU = CLDRADU + CLRRADU
               URAD(LEV) = URAD(LEV) + RADLU
               RADMOD = RAD * 
     &              (FACCLR1(LEV+1)*(1.0-ATRANS(LEV))+
     &              FACCLD1(LEV+1) *  TTOT) - 
     &              FACCMB1(LEV+1) * GASSRC + 
     &              FACCMB2(LEV+1) * CLDSRC
               OLDCLD = CLDRADU - RADMOD
               OLDCLR = CLRRADU + RADMOD
               RAD = -RADMOD + FACCLR2(LEV+1)*OLDCLR -
     &           FACCLD2(LEV+1)*OLDCLD
               CLDRADU = CLDRADU + RAD
               CLRRADU = CLRRADU - RAD
            ELSE
               RADLU = RADLU + (BBUGAS(LEV)-RADLU)*ATRANS(LEV)
               URAD(LEV) = URAD(LEV) + RADLU
            ENDIF

 2600    CONTINUE

         IG = IG + 1
         IF (IG .LE. NG(IBAND)) GO TO 1000
         
C ***    Process longwave output from band.
C ***    Calculate upward, downward, and net flux.
         DO 5000 LEV = NLAYERS, 0, -1
            UFLUX(LEV) = URAD(LEV)*WTDIFF
            DFLUX(LEV) = DRAD(LEV)*WTDIFF
            URAD(LEV) = 0.0
            DRAD(LEV) = 0.0
            TOTUFLUX(LEV) = TOTUFLUX(LEV) + UFLUX(LEV) * DELWAVE(IBAND)
            TOTDFLUX(LEV) = TOTDFLUX(LEV) + DFLUX(LEV) * DELWAVE(IBAND)
 5000    CONTINUE
 6000 CONTINUE

      TOTUFLUX(0) = TOTUFLUX(0) * FLUXFAC
      TOTDFLUX(0) = TOTDFLUX(0) * FLUXFAC

      FNET(0) = TOTUFLUX(0) - TOTDFLUX(0)
      DO 7000 LEV = 1, NLAYERS

         TOTUFLUX(LEV) = TOTUFLUX(LEV) * FLUXFAC
         TOTDFLUX(LEV) = TOTDFLUX(LEV) * FLUXFAC
         FNET(LEV) = TOTUFLUX(LEV) - TOTDFLUX(LEV)
         L = LEV - 1

C        Calculate Heating Rates.
         HTR(L)=HEATFAC*(FNET(L)-FNET(LEV))/(PZ(L)-PZ(LEV)) 
 7000 CONTINUE
      HTR(NLAYERS) = 0.0

 9000 CONTINUE

      RETURN
      END   
