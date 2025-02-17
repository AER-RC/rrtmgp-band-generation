C     path:      %P%
C     revision:  %I%
C     created:   %G%  %U%
C     presently: %H%  %T%
      SUBROUTINE ARTREG

C *** This program calculates the upward fluxes, downward fluxes,
C     and heating rates for an arbitrary atmosphere.  The input to
C     this program is the atmospheric profile and all needed Planck
C     function information.  First-order standard Gaussian quadrature 
C     is used for the angle integration.

      PARAMETER (MG=16)
      PARAMETER (MXLAY=203)
      PARAMETER (MXANG = 4)
      PARAMETER (NBANDS = 16)

      IMPLICIT DOUBLE PRECISION (V)                                     

      COMMON /CONSTANTS/ PI,FLUXFAC,HEATFAC
      COMMON /FEATURES/  NG(NBANDS),NSPA(NBANDS),NSPB(NBANDS)
      COMMON /BANDS/     WAVENUM1(NBANDS),WAVENUM2(NBANDS),
     &                   DELWAVE(NBANDS)
      COMMON /CONTROL/   NUMANGS, IOUT, ISTART, IEND
      COMMON /PROFILE/   NLAYERS,PAVEL(MXLAY),TAVEL(MXLAY),
     &                   PZ(0:MXLAY),TZ(0:MXLAY)
      COMMON /SURFACE/   TBOUND,IREFLECT,SEMISS(NBANDS)
      COMMON /PLNKDAT/   PLANKLAY(MXLAY,NBANDS),
     &                   PLANKLEV(0:MXLAY,NBANDS),PLANKBND(NBANDS)
      COMMON /PLANKG/    FRACS(MXLAY,MG)                                       
      COMMON /TAUGCOM/   TAUG(MXLAY,MG)
      COMMON /OUTPUT/    TOTUFLUX(0:MXLAY), TOTDFLUX(0:MXLAY),
     &                   FNET(0:MXLAY), HTR(0:MXLAY)

      COMMON /HVERSN/    HVRRTM,HVRREG,HVRRTR,HVRATM,HVRSET,HVRTAU,
     *                   HVDUM1(4),HVRUTL,HVREXT
      COMMON /RADIANCE/  UPANG,ANG,DIG(MG*NBANDS)
      CHARACTER*8 HVRRTM,HVRREG,HVRRTR,HVRATM,HVRSET,HVRTAU,
     *            HVDUM1,HVRUTL,HVREXT

      DIMENSION ATRANS(MXLAY,MXANG),BBU(MXLAY,MXANG), RAD(MXANG)
      DIMENSION UFLUX(0:MXLAY),DFLUX(0:MXLAY)
      DIMENSION DRAD(0:MXLAY-1,MXANG),URAD(0:MXLAY,MXANG)
      DIMENSION SECANG(MXANG),ANGWEIGH(MXANG)
      DIMENSION SECREG(MXANG,MXANG),WTREG(MXANG,MXANG)

C *** When standard first-order Gaussian quadrature is chosen as
C     the method to approximate the integral over angles that yields
C     flux from radiances, then SECREG(I,J) is the secant of the Ith  
C     (out of a total of J angles) and WTREG(I,J) is the corresponding
C     weight.
      DATA SECDIFF / 1./
      data secreg(1,1) /1.0/
c     DATA SECREG(1,1) / 1.5/
      DATA SECREG(2,2) / 2.81649655/, SECREG(1,2) / 1.18350343/
      DATA SECREG(3,3) / 4.70941630/, SECREG(2,3) / 1.69338507/
      DATA SECREG(1,3) / 1.09719858/
      DATA SECREG(4,4) / 7.15513024/, SECREG(3,4) / 2.40148179/
      DATA SECREG(2,4) / 1.38282560/, SECREG(1,4) / 1.06056257/
      DATA WTREG(1,1) / 1.00000000/
c      DATA WTREG(1,1) / 0.50000000/
      DATA WTREG(2,2) /0.1819586183/, WTREG(1,2) /0.3180413817/
      DATA WTREG(3,3) /0.0698269799/, WTREG(2,3) /0.2292411064/
      DATA WTREG(1,3) /0.2009319137/
      DATA WTREG(4,4) /0.0311809710/, WTREG(3,4) /0.1298475476/
      DATA WTREG(2,4) /0.2034645680/, WTREG(1,4) /0.1355069134/

      HVRREG = '%I%'
      
      RADSUM = 0.
      NUMANG = ABS(NUMANGS)
      AFAC = (1.0/0.278)

C *** Load angle data in arrays depending on angular quadrature scheme.
      DO 100 IANG = 1, NUMANG
         SECANG(IANG) = SECREG(IANG,NUMANG)
         ANGWEIGH(IANG) = WTREG(IANG,NUMANG)
 100  CONTINUE
      IF (NUMANGS .EQ. -1) SECANG(1) = SECDIFF
      
      SECANG(1) = 1./COS(PI * ANG / 180.)
      DO 200 LAY = 0, NLAYERS
         TOTUFLUX(LAY) = 0.0
         TOTDFLUX(LAY) = 0.0
         DO 150 IANG = 1, NUMANG
            URAD(LAY,IANG) = 0.
            DRAD(LAY,IANG) = 0.
 150     CONTINUE
 200  CONTINUE

C *** Loop over frequency bands.
      DO 6000 IBAND = ISTART, IEND
         IF (IBAND .EQ. 1) THEN
            CALL TAUGB1
         ENDIF

C ***    Loop over g-channels.
         IG = 1
 1000    CONTINUE
         
C ***    Loop over each angle for which the radiance is to be computed.
         DO 3000 IANG = 1, NUMANG
C ***       Radiative transfer starts here.
            RADLD = 0.
C ***       Downward radiative transfer.  
            DO 2500 LEV = NLAYERS, 1, -1
               BLAY = PLANKLAY(LEV,IBAND)
               PLFRAC = FRACS(LEV,IG)
               ODEPTH = SECANG(IANG) * TAUG(LEV,IG)
               ATRANS(LEV,IANG) = 1. - EXP(-ODEPTH)
               DPLANKUP = PLANKLEV(LEV,IBAND) - BLAY
               DPLANKDN = PLANKLEV(LEV-1,IBAND) - BLAY
               TAUSFAC = ODEPTH/(AFAC+ODEPTH)
               BBD = PLFRAC * (BLAY + TAUSFAC * DPLANKDN)
               RADLD = RADLD + (BBD-RADLD)*ATRANS(LEV,IANG)
               DRAD(LEV-1,IANG) = DRAD(LEV-1,IANG) + RADLD
               BBU(LEV,IANG) = PLFRAC * (BLAY + TAUSFAC * DPLANKUP)
 2500       CONTINUE
            RAD(IANG) = RADLD
            RADSUM = RADSUM + ANGWEIGH(IANG) * RADLD
 3000    CONTINUE 

         RAD0 = FRACS(1,IG) * PLANKBND(IBAND)
         REFLECT = 1. - SEMISS(IBAND)
         DO 4000 IANG = 1, NUMANG
C           Add in reflection of surface downward radiance.
            IF (IREFLECT .EQ. 1) THEN
C              Specular reflection.
               RADLU = RAD0 + REFLECT * RAD(IANG)
            ELSE
C              Lambertian reflection.
               RADLU = RAD0 + 2. * REFLECT * RADSUM
            ENDIF
            URAD(0,IANG) = URAD(0,IANG) + RADLU
C ***       Upward radiative transfer.
            DO 3500 LEV = 1, NLAYERS
               RADLU = RADLU + (BBU(LEV,IANG)-RADLU)*ATRANS(LEV,IANG)
               URAD(LEV,IANG) = URAD(LEV,IANG) + RADLU
 3500       CONTINUE
 4000    CONTINUE
         RADSUM = 0.

         IG = IG + 1
         IF (IG .LE. 16) GO TO 1000

C ***    Calculate upward, downward, and net flux.
         DO 5000 LEV = NLAYERS, 0, -1
            UFLUX(LEV) = 0.0
            DFLUX(LEV) = 0.0
            DO 4500 IANG = 1, NUMANG
               UFLUX(LEV) = UFLUX(LEV) + URAD(LEV,IANG)*ANGWEIGH(IANG)
               DFLUX(LEV) = DFLUX(LEV) + DRAD(LEV,IANG)*ANGWEIGH(IANG)
               URAD(LEV,IANG) = 0.0
               DRAD(LEV,IANG) = 0.0
 4500       CONTINUE
c            TOTUFLUX(LEV) = TOTUFLUX(LEV) + UFLUX(LEV) * DELWAVE(IBAND)
c            TOTDFLUX(LEV) = TOTDFLUX(LEV) + DFLUX(LEV) * DELWAVE(IBAND)

            TOTUFLUX(LEV) = TOTUFLUX(LEV) + UFLUX(LEV) 
            TOTDFLUX(LEV) = TOTDFLUX(LEV) + DFLUX(LEV) 
 5000    CONTINUE
 6000 CONTINUE 

      DO 7000 LEV = 0, NLAYERS
         TOTUFLUX(LEV) = TOTUFLUX(LEV) * 1.D4
         TOTDFLUX(LEV) = TOTDFLUX(LEV)  * 1.D4
         FNET(LEV) = TOTUFLUX(LEV) - TOTDFLUX(LEV)
 7000 CONTINUE

C *** Calculate Heating Rates.
      HTR(NLAYERS) = 0.0
      DO 8000 LEV  = NLAYERS-1, 0, -1
         LAY = LEV + 1
         HTR(LEV)=HEATFAC*(FNET(LEV)-FNET(LAY))/(PZ(LEV)-PZ(LAY)) 
 8000 CONTINUE

      RETURN
      END   


