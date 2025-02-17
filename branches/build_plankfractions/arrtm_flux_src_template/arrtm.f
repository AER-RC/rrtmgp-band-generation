C     path:      %P%
C     revision:  %I%
C     created:   %G%  %U%
C     presently: %H%  %T%
****************************************************************************
*                                                                          *
*                               RRTM                                       *
*                                                                          *
*                                                                          *
*                                                                          *
*                   A RAPID RADIATIVE TRANSFER MODEL                       *
*                                                                          *
*                                                                          *
*            ATMOSPHERIC AND ENVIRONMENTAL RESEARCH, INC.                  *
*                        840 MEMORIAL DRIVE                                *
*                        CAMBRIDGE, MA 02139                               *
*                                                                          *
*                                                                          *
*                           ELI J. MLAWER                                  *
*                         STEVEN J. TAUBMAN~                               *
*                         SHEPARD A. CLOUGH                                *
*                                                                          *
*                                                                          *
*                         ~currently at GFDL                               *
*                                                                          *
*                                                                          *
*                                                                          *
*                       email:  mlawer@aer.com                             *
*                                                                          *
*        The authors wish to acknowledge the contributions of the          *
*        following people:  Patrick D. Brown, Michael J. Iacono,           *
*        Ronald E. Farren, Luke Chen, Robert Bergstrom.                    *
*                                                                          *
****************************************************************************

       PROGRAM RRTM
                    
C *** This program is the driver for RRTM, the AER rapid model.  
C     For each atmosphere the user wishes to analyze, this routine
C     a) calls READPROF to read in the atmospheric profile
C     b) calls SETCOEF to calculate various quantities needed for 
C        the radiative transfer algorithm
C     c) calls RTR or RTREG (depending on angular quadrature
C         method) to do the radiative transfer calculation
C     d) writes out the upward, downward, and net flux for each
C        level and the heating rate for each layer

      PARAMETER (MXLAY=203)
      PARAMETER (MG = 16)
      PARAMETER (NBANDS = 16)
      integer iwvn

      integer igas_minor_u(7,1),igas_minor_l(7,1)
      integer isortplanck(2)

      common /INPUT_PAR/igas1_l,igas2_l,igas1_u,igas2_u,
     &     igas_minor_l,igas_minor_u,isortplanck

      COMMON /CONSTANTS/ PI,FLUXFAC,HEATFAC
      COMMON /FEATURES/  NG(NBANDS),NSPA(NBANDS),NSPB(NBANDS)
      COMMON /PRECISE/   ONEMINUS
      COMMON /BANDS/     WAVENUM1(NBANDS),WAVENUM2(NBANDS),
     &                   DELWAVE(NBANDS)
      COMMON /CONTROL/   NUMANGS, IOUT, ISTART, IEND, ICLD
      COMMON /PROFILE/   NLAYERS,PAVEL(MXLAY),TAVEL(MXLAY),
     &                   PZ(0:MXLAY),TZ(0:MXLAY)
      COMMON /OUTPUT/    TOTUFLUX(0:MXLAY), TOTDFLUX(0:MXLAY),
     &                   FNET(0:MXLAY), HTR(0:MXLAY)
      COMMON /HVERSN/    HVRRTM,HVRREG,HVRRTR,HVRATM,HVRSET,HVRTAU,
     *                   HVRRGC,HVRRTC,HVRCLD,HVRDUM,HVRUTL,HVREXT
      COMMON /HVRSNB/    HVRKG(NBANDS)

      CHARACTER*8 HVRRTM,HVRREG,HVRRTR,HVRATM,HVRSET,HVRTAU,
     *            HVRRGC,HVRRTC,HVRCLD,HVRDUM,HVRUTL,HVREXT
      CHARACTER*8 HVRKG
      CHARACTER PAGE

      NAMELIST /par/ WAVENUMber1,WAVENUMber2,IGAS1_L,IGAS2_L,IGAS1_U,
     &                   IGAS2_U,igas_minor_l,igas_minor_l,igas_minor_u

      namelist /iplanck/ isortplanck

      read(*,par)
      read(*,iplanck)

      wavenum1(1) = wavenumber1
      wavenum2(1) = wavenumber2
      delwave(1)=wavenum2(1) - wavenum1(1)

      ONEMINUS = 1. - 1.E-6
      PI = 2.*ASIN(1.)
      FLUXFAC = PI * 2.D4  

      IWR = 10
      PAGE = CHAR(12)
      
C     Multiple atmospheres not yet implemented. 
      NUMATMOS = 1
      DO 4000 IATMOS = 1, NUMATMOS

C ***    Input atmospheric profile from INPUT_RRTM.
         CALL READPROF

         ISTART = 1
         IEND = 16
         IFLAG = IOUT

 1000    CONTINUE
         IF (IFLAG .GT. 0 .AND. IFLAG .LE. 40) THEN
            ISTART = IFLAG
            IEND = IFLAG
         ENDIF


C ***    Calculate information needed by the radiative transfer routine
C        that is specific to this atmosphere, especially some of the 
C        coefficients and indices needed to compute the optical depths
C        by interpolating data from stored reference atmospheres. 
         icldatm = 0
         IF (ICLD .EQ. 1) CALL CLDPROP(ICLDATM)

         CALL ASETCOEF

C ***    Call the radiative transfer routine.
         IF (NUMANGS .EQ. 0 .AND. ICLDATM .EQ. 0) THEN
            CALL ARTR
         ELSEIF (NUMANGS .EQ. 0 .AND. ICLDATM .EQ. 1) THEN
            CALL RTRCLD
         ELSEIF (ICLDATM .EQ. 1) THEN
            CALL RTREGCLD
         ELSE
            CALL ARTREG
         ENDIF
         IF (IOUT .LT. 0) GO TO 4000

C ***    Process output for this atmosphere.
         OPEN (IWR,FILE='OUTPUT_RRTM',FORM='FORMATTED')
         WRITE(IWR,9899)WAVENUM1(ISTART),WAVENUM2(IEND)
         WRITE(IWR,9900)
         WRITE(IWR,9901)
C
         DO 3000 I = NLAYERS, 0, -1
            IF (PZ(I) .LT. 1.E-2) THEN
               WRITE(IWR,9952) I,PZ(I),TOTUFLUX(I),TOTDFLUX(I),
     &              FNET(I), HTR(I)
            ELSEIF (PZ(I) .LT. 1.E-1) THEN
               WRITE(IWR,9953) I,PZ(I),TOTUFLUX(I),TOTDFLUX(I),
     &              FNET(I), HTR(I)
            ELSEIF (PZ(I) .LT. 1.) THEN
               WRITE(IWR,9954) I,PZ(I),TOTUFLUX(I),TOTDFLUX(I),
     &              FNET(I), HTR(I)
            ELSEIF (PZ(I) .LT. 10.) THEN
               WRITE(IWR,9955) I,PZ(I),TOTUFLUX(I),TOTDFLUX(I),
     &              FNET(I), HTR(I)
            ELSEIF (PZ(I) .LT. 100.) THEN
               WRITE(IWR,9956) I,PZ(I),TOTUFLUX(I),TOTDFLUX(I),
     &              FNET(I), HTR(I)
            ELSEIF (PZ(I) .LT. 1000.) THEN
               WRITE(IWR,9957) I,PZ(I),TOTUFLUX(I),TOTDFLUX(I),
     &              FNET(I), HTR(I)
            ELSE
               WRITE(IWR,9958) I,PZ(I),TOTUFLUX(I),TOTDFLUX(I),
     &              FNET(I), HTR(I)
            ENDIF
 3000    CONTINUE
         WRITE(IWR,9903)PAGE
         
         IF (IOUT .LE. 40 .OR. IFLAG .EQ. 16) GO TO 3500
         IF (IFLAG .EQ. 99) THEN
            IFLAG = 1
         ELSEIF (IOUT .EQ. 99) THEN
            IFLAG = IFLAG + 1
         ENDIF
         GO TO 1000

 3500    CONTINUE

C
C ***    Output module version numbers
C
         WRITE(IWR,9910) HVRRTM,HVRATM,HVRRTR,HVRRTC,HVRREG,HVRRGC,
     *        HVRSET,HVRCLD,HVRUTL,HVRTAU,(HVRKG(NB),NB=1,NBANDS)
         CLOSE(IWR)

 4000 CONTINUE
 9400 FORMAT(1X,'Downwelling radiance is vertical.')
 9405 FORMAT(1X,'Upwelling radiance was chosen to be computed at ', 
     &F6.3,' degrees.')
 9401 FORMAT(1X,'Radiances are in units of W/(m2 sr).')
 9402 FORMAT(1X,'Surface downwelling radiance: ',F16.6)
 9403 FORMAT(1X,'TOA upwelling radiance:       ',F16.6)
 9404 FORMAT(1X)

c 9952 FORMAT(1X,I3,9X,F7.6,3X,F8.4,6X,F8.4,6X,F9.4,10X,F9.5)
c 9953 FORMAT(1X,I3,9X,F6.5,4X,F8.4,6X,F8.4,6X,F9.4,10X,F9.5)
c 9954 FORMAT(1X,I3,9X,F5.4,5X,F8.4,6X,F8.4,6X,F9.4,10X,F9.5)
c 9955 FORMAT(1X,I3,8X,F5.3,6X,F8.4,6X,F8.4,6X,F9.4,10X,F9.5)
c 9956 FORMAT(1X,I3,7X,F5.2,7X,F8.4,6X,F8.4,6X,F9.4,10X,F9.5)
c 9957 FORMAT(1X,I3,6X,F5.1,8X,F8.4,6X,F8.4,6X,F9.4,10X,F9.5)
c 9958 FORMAT(1X,I3,5X,F5.0,9X,F8.4,6X,F8.4,6X,F9.4,10X,F9.5)

 9952 FORMAT(1X,I3,6X,F7.6,3X,1P,E13.6,2X,E13.6,2X,E13.6,3X,E13.6) 
 9953 FORMAT(1X,I3,6X,F6.5,4X,1P,E13.6,2X,E13.6,2X,E13.6,3X,E13.6) 
 9954 FORMAT(1X,I3,6X,F5.4,5X,1P,E13.6,2X,E13.6,2X,E13.6,3X,E13.6) 
 9955 FORMAT(1X,I3,5X,F5.3,6X,1P,E13.6,2X,E13.6,2X,E13.6,3X,E13.6) 
 9956 FORMAT(1X,I3,4X,F5.2,7X,1P,E13.6,2X,E13.6,2X,E13.6,3X,E13.6) 
 9957 FORMAT(1X,I3,3X,F5.1,8X,1P,E13.6,2X,E13.6,2X,E13.6,3X,E13.6) 
 9958 FORMAT(1X,I3,2X,F5.0,9X,1P,E13.6,2X,E13.6,2X,E13.6,3X,E13.6) 

 9899 FORMAT(1X,'Wavenumbers: ',F6.1,' - ',F6.1,' cm-1')
 9900 FORMAT(1X,'LEVEL    PRESSURE   UPWARD FLUX   DOWNWARD FLUX    NET
     &FLUX       HEATING RATE')
 9901 FORMAT(1X,'            mb          W/m2          W/m2           W/
     &m2          degree/day')
 9902 FORMAT(1X,I3,3X,F11.6,4X,1P,2(G12.6,2X),G13.6,3X,G16.9,0P)
 9903 FORMAT(A)
 9910 FORMAT('  Modules and versions used in this calculation:',/,/,5X,
     *        '    rrtm.f: ',6X,A8,10X, 'rrtatm.f: ',6X,A8,/,5X,
     *        '     rtr.f: ',6X,A8,10X, 'rtrcld.f: ',6X,A8,/,5X, 
     *        '   rtreg.f: ',6X,A8,8X, 'rtregcld.f: ',6X,A8,/,5X, 
     *        ' setcoef.f: ',6X,A8,9X, 'cldprop.f: ',6X,A8,/,5X,
     *        'util_xxx.f: ',6X,A8,10X, 'taumol.f: ',6X,A8,/,5X,
     *        '  k_gB01.f: ',6X,A8,10X, 'k_gB02.f: ',6X,A8,/,5X,
     *        '  k_gB03.f: ',6X,A8,10X, 'k_gB04.f: ',6X,A8,/,5X,
     *        '  k_gB05.f: ',6X,A8,10X, 'k_gB06.f: ',6X,A8,/,5X,
     *        '  k_gB07.f: ',6X,A8,10X, 'k_gB08.f: ',6X,A8,/,5X,
     *        '  k_gB09.f: ',6X,A8,10X, 'k_gB10.f: ',6X,A8,/,5X,
     *        '  k_gB11.f: ',6X,A8,10X, 'k_gB12.f: ',6X,A8,/,5X,
     *        '  k_gB13.f: ',6X,A8,10X, 'k_gB14.f: ',6X,A8,/,5X,
     *        '  k_gB15.f: ',6X,A8,10X, 'k_gB16.f: ',6X,A8,/)

      STOP
      END

C************************  SUBROUTINE READPROF  *****************************C

      SUBROUTINE READPROF                                                     
                                                                         
C     Read in atmospheric profile.

      IMPLICIT DOUBLE PRECISION (V)                                      
                                                                         
      PARAMETER (MXLAY=203)
      PARAMETER (NBANDS = 16)
      PARAMETER (MAXINPX=35)
      PARAMETER (MAXXSEC=4)
      PARAMETER (MAXPROD = MXLAY*MAXXSEC)

      DIMENSION ALTZ(0:MXLAY),IXTRANS(14),SEMIS(NBANDS)

      COMMON /CONTROL/  NUMANGS, IOUT, ISTART, IEND, ICLD
      COMMON /PROFILE/  NLAYERS,PAVEL(MXLAY),TAVEL(MXLAY),
     &                  PZ(0:MXLAY),TZ(0:MXLAY)
      COMMON /SURFACE/  TBOUND,IREFLECT,SEMISS(NBANDS)
      COMMON /SPECIES/  COLDRY(MXLAY),WKL(35,MXLAY),WBRODL(MXLAY),
     &                  COLMOL(MXLAY),NMOL
      COMMON /IFIL/     IRD,IPR,IPU,IDUM(15)
      COMMON /XSECCTRL/ NXMOL,IXINDX(MAXINPX)
      COMMON /XSEC/     WX(MAXXSEC,MXLAY)
      COMMON /PATHX/    IXMAX,NXMOL0,IXINDX0(MAXINPX),WX0(MAXINPX,MXLAY)    
      COMMON /XRRTATM/  IXSECT

      CHARACTER*80 FORM1(0:1),FORM2(0:1),FORM3(0:1)
      CHARACTER*1 CTEST, CDOLLAR, CDUM

      DATA CDOLLAR /'$'/
      DATA IXTRANS /0,0,0,1,2,3,0,0,0,0,0,4,0,0/

      FORM1(0) = '(3F10.4,A3,I2,1X,2(F7.2,F8.3,F7.2))'
      FORM2(0) = '(3F10.4,A3,I2,23X,(F7.2,F8.3,F7.2))'
      FORM3(0) = '(8E10.3)'
      FORM1(1) = '(G15.7,G10.4,G10.4,A3,I2,1X,2(G7.2,G8.3,G7.2))'
      FORM2(1) = '(G15.7,G10.4,G10.4,A3,I2,23X,(G7.2,G8.3,G7.2))'
      FORM3(1) = '(8G15.7)'

      IXMAX = MAXINPX
      IRD = 9
      OPEN (IRD,FILE='INPUT_RRTM',FORM='FORMATTED')

 1000 CONTINUE
      READ (IRD,9010,END=8800) CTEST
      IF (CTEST .NE. CDOLLAR) GO TO 1000

      READ (IRD,9011) IATM, IXSECT, NUMANGS, IOUT, ICLD
      print*,'readin numangs',numangs,icld
C     If clouds are present, read in appropriate input file, IN_CLD_RRTM.
      IF (ICLD .EQ. 1) CALL READCLD

C     Read in surface information.
      READ (IRD,9012) TBOUND,IEMISS,IREFLECT,(SEMIS(I),I=1,16)
      DO 1500 IBAND = 1, NBANDS
         SEMISS(IBAND) = 1.0
         IF (IEMISS .EQ. 1 .AND. SEMIS(1) .NE. 0.) THEN
            SEMISS(IBAND) = SEMIS(1)
         ELSEIF (IEMISS .EQ. 2) THEN
            IF (SEMIS(IBAND) .NE. 0.) THEN
               SEMISS(IBAND) = SEMIS(IBAND)
            ENDIF
         ENDIF
 1500 CONTINUE

      IF (IATM .EQ. 0) THEN
         READ (IRD,9013) IFORM,NLAYERS,NMOL
         IF (NMOL.EQ.0) NMOL = 7                                    
         READ (IRD,FORM1(IFORM)) PAVEL(1),TAVEL(1),SECNTK,CINP,
     &        IPTHAK,ALTZ(0),PZ(0),TZ(0),ALTZ(1),PZ(1),TZ(1)
         READ (IRD,FORM3(IFORM)) (WKL(M,1),M=1,7), WBRODL(1)
         IF(NMOL .GT. 7) READ (IRD,FORM3(IFORM)) (WKL(M,1),M=8,NMOL)

         DO 2000 L = 2, NLAYERS
            READ (IRD,FORM2(IFORM)) PAVEL(L),TAVEL(L),SECNTK,CINP,
     &           IPTHRK,ALTZ(L),PZ(L),TZ(L)
            READ (IRD,FORM3(IFORM)) (WKL(M,L),M=1,7), WBRODL(L)
            IF(NMOL .GT. 7) READ (IRD,FORM3(IFORM)) (WKL(M,L),M=8,NMOL)
 2000    CONTINUE                                                            
           
         IF (IXSECT .EQ. 1) THEN                                 
            READ (IRD,9300) NXMOL0
            NXMOL = NXMOL0
            CALL XSIDENT(IRD)
            READ (IRD,9301) IFORMX
C     
            DO 3000 L = 1, NLAYERS       
               READ (IRD,9010) CDUM
               READ (IRD, FORM3(IFORMX)) (WX0(M,L),M=1,7),WBRODX    
               IF (NXMOL0 .GT. 7) READ (IRD,FORM3(IFORMX)) 
     &              (WX0(M,L),M=8,NXMOL0)
 3000       CONTINUE
         ENDIF
      ELSE
         IPU = 7
         IPR = 66
         OPEN(UNIT=IPR,FILE='TAPE6',STATUS='UNKNOWN')
         CALL RRTATM
         IF (IXSECT .EQ. 1) THEN
            DO 3300 MX = 1, NXMOL0
               IXINDX(MX) = IXTRANS(IXINDX0(MX))
 3300       CONTINUE
         ENDIF
      ENDIF
      IF (TBOUND .LT. 0) TBOUND = TZ(0)

C     Test for mixing ratio input.
      IMIX = 1
      DO 3500 M = 1, NMOL
         IF (WKL(M,1) .GT. 1.0) THEN
            IMIX = 0
            GO TO 3600
         ENDIF
 3500 CONTINUE
 3600 CONTINUE

      IF (IXSECT .EQ. 1) THEN
         IMIXX = 0
         IF (WX0(1,1) .LE. 1.0) IMIXX = 1
      ENDIF
      DO 5000 L = 1, NLAYERS
         SUMMOL = 0.0
         DO 4100 IMOL = 2, NMOL
            SUMMOL = SUMMOL + WKL(IMOL,L)
 4100    CONTINUE
         IF (IMIX .EQ. 1) THEN
            COLDRY(L) = WBRODL(L) / (1. - SUMMOL)
            DO 4200 IMOL = 1, NMOL
               WKL(IMOL,L) = COLDRY(L) * WKL(IMOL,L)
 4200       CONTINUE
         ELSE
            COLDRY(L) = WBRODL(L) + SUMMOL
         ENDIF
         IF (IXSECT .EQ. 1) THEN
            DO 4400 IX = 1, NXMOL0
               IF (IXINDX(IX) .NE. 0) THEN
                  IF (IMIXX .EQ. 1) THEN
                     WX(IXINDX(IX),L) = COLDRY(L) * WX0(IX,L) * 1.E-20
                  ELSE
                     WX(IXINDX(IX),L) = WX0(IX,L) * 1.E-20
                  ENDIF
               ENDIF
 4400       CONTINUE
         ENDIF
 5000 CONTINUE

      CLOSE(IRD)
      GO TO 9000

 8800 CONTINUE
      STOP ' INVALID INPUT_RRTM '

 9000 CONTINUE

 9010 FORMAT (A1)
 9011 FORMAT (49X,I1,19X,I1,13X,I2,2X,I3,4X,I1)
 9012 FORMAT (E10.3,1X,I1,2X,I1,16E5.3)
 9013 FORMAT (1X,I1,I3,I5)                                     
 9300 FORMAT (I5)
 9301 FORMAT (1X,I1)

      RETURN
      END 

C************************  SUBROUTINE READCLD  *****************************C

      SUBROUTINE READCLD

C     Purpose:  To read in IN_CLD_RRTM, the file that contains input 
C               cloud properties.

      PARAMETER (MXLAY=203)
      PARAMETER (NBANDS = 16)

      COMMON /PROFILE/   NLAYERS,PAVEL(MXLAY),TAVEL(MXLAY),
     &                   PZ(0:MXLAY),TZ(0:MXLAY)
      COMMON /CLOUDIN/   INFLAG,CLDDAT1(MXLAY),CLDDAT2(MXLAY),
     &                   ICEFLAG,LIQFLAG,CLDDAT3(MXLAY),CLDDAT4(MXLAY)
      COMMON /CLOUDDAT/  NCBANDS,CLDFRAC(MXLAY),TAUCLOUD(NBANDS,MXLAY)

      CHARACTER*1 CTEST, CPERCENT

      DATA CPERCENT /'%'/
      IRDCLD = 11

      OPEN(IRDCLD,FILE='IN_CLD_RRTM',FORM='FORMATTED')

C     Read in cloud input option.  
      READ(IRDCLD,9050) INFLAG, ICEFLAG, LIQFLAG
      DO 500 LAY = 1, NLAYERS
         CLDFRAC(LAY) = 0.
 500  CONTINUE

 1000 CONTINUE
C     For INFLAG = 0 or 1, for each cloudy layer only LAY, FRAC, and
C     DAT1 are pertinent.  If CTEST = '%', then there are no more 
C     cloudy layers to process.
      READ (IRDCLD,9100,END=9000) CTEST,LAY,FRAC,DAT1,DAT2,DAT3,DAT4
      IF (CTEST .EQ. CPERCENT) GO TO 9000
      CLDFRAC(LAY) = FRAC
      CLDDAT1(LAY) = DAT1
      CLDDAT2(LAY) = DAT2
      CLDDAT3(LAY) = DAT3
      CLDDAT4(LAY) = DAT4
      GO TO 1000

 9000 CONTINUE
      CLOSE(IRDCLD)

 9050 FORMAT (3X,I2,4X,I1,4X,I1)
 9100 FORMAT (A1,1X,I3,5E10.5)

      RETURN
      END

C************************  SUBROUTINE XSIDENT  *****************************C

      SUBROUTINE XSIDENT(IRD)
C                                                                         
C     This subroutine identifies which cross-sections are to be used.

      PARAMETER (MAXINPX=35)
      PARAMETER (MAXXSEC=4)

      IMPLICIT DOUBLE PRECISION (V)                                     ! 
C                                                                         
      COMMON /XSECCTRL/ NXMOL,IXINDX(MAXINPX)
C                                                                         
C     NXMOL     - number of cross-sections input by user
C     IXINDX(I) - index of cross-section molecule corresponding to Ith
C                 cross-section specified by user
C                 = 0 -- not allowed in RRTM
C                 = 1 -- CCL4
C                 = 2 -- CFC11
C                 = 3 -- CFC12
C                 = 4 -- CFC22
C                                                                         
C     XSNAME=NAMES, ALIAS=ALIASES OF THE CROSS-SECTION MOLECULES          
C                                                                         
      CHARACTER*10 XSNAME(MAXINPX),ALIAS(MAXXSEC,4),BLANK               
C                                                                         
      DATA (ALIAS(1,I),I=1,4)/                                           
     *    'CCL4      ', 'CCL3F     ', 'CCL2F2    ', 'CHCLF2    '/ 
      DATA (ALIAS(2,I),I=1,4)/                                           
     *    ' ZZZZZZZZ ', 'CFCL3     ', 'CF2CL2    ', 'CHF2CL    '/         
      DATA (ALIAS(3,I),I=1,4)/                                           
     *    ' ZZZZZZZZ ', 'CFC11     ', 'CFC12     ', 'CFC22     '/         
      DATA (ALIAS(4,I),I=1,4)/                                           
     *    ' ZZZZZZZZ ', 'F11       ', 'F12       ', 'F22       '/        

      DATA BLANK / '          '/                                          
C                                                                         
      DO 10 I = 1, NXMOL                                                 
         XSNAME(I) = BLANK                                                
   10 CONTINUE                                                            
C                                                                         
C     READ IN THE NAMES OF THE MOLECULES                                  
C                                                                         
      IF (NXMOL.GT.7) THEN                                               
         READ (IRD,'(7A10)') (XSNAME(I),I=1,7)                            
         READ (IRD,'(8A10)') (XSNAME(I),I=8,NXMOL)                       
      ELSE                                                                
         READ (IRD,'(7A10)') (XSNAME(I),I=1,NXMOL)                       
      ENDIF                                                               
C                                                                         
C     MATCH THE NAMES READ IN AGAINST THE NAMES STORED IN ALIAS           
C     AND DETERMINE THE INDEX VALUE.  
      IXMAX = 4                                                          
      DO 40 I = 1, NXMOL                                                 
C        Left-justify all inputed names.                                      
         CALL CLJUST (XSNAME(I),10)
         IXINDX(I) = 0
         DO 20 J = 1, IXMAX
            IF ((XSNAME(I).EQ.ALIAS(1,J)) .OR.                            
     *          (XSNAME(I).EQ.ALIAS(2,J)) .OR.                            
     *          (XSNAME(I).EQ.ALIAS(3,J)) .OR.                            
     *          (XSNAME(I).EQ.ALIAS(4,J))) THEN                           
               IXINDX(I) = J                                              
            ENDIF                                                         
   20    CONTINUE
   40 CONTINUE                                                            

      RETURN
      END


      BLOCK DATA

      PARAMETER (MG = 16)
      PARAMETER (NBANDS = 16)
      PARAMETER (MXLAY=203)
      PARAMETER (MAXXSEC=4)
      PARAMETER (MAXPROD = MXLAY*MAXXSEC)

      COMMON /CONSTANTS/ PI,FLUXFAC,HEATFAC
      COMMON /FEATURES/  NG(NBANDS),NSPA(MG),NSPB(MG)
      COMMON /BANDS/     WAVENUM1(NBANDS),WAVENUM2(NBANDS),
     &                   DELWAVE(NBANDS)
      COMMON /XSEC/     WX(MAXXSEC,MXLAY)
c
      COMMON /HVERSN/ HVRRTM,HVRREG,HVRRTR,HVRATM,HVRSET,HVRTAU,
     *                HVDUM1(4),HVRUTL,HVREXT

      CHARACTER*8 HVRRTM,HVRREG,HVRRTR,HVRATM,HVRSET,HVRTAU,
     *            HVDUM1,HVRUTL,HVREXT

      DATA WAVENUM1(1) /10./, WAVENUM2(1) /250./, DELWAVE(1) /240./
      DATA WAVENUM1(2) /250./, WAVENUM2(2) /500./, DELWAVE(2) /250./
      DATA WAVENUM1(3) /500./, WAVENUM2(3) /630./, DELWAVE(3) /130./
      DATA WAVENUM1(4) /630./, WAVENUM2(4) /700./, DELWAVE(4) /70./
      DATA WAVENUM1(5) /700./, WAVENUM2(5) /820./, DELWAVE(5) /120./
      DATA WAVENUM1(6) /820./, WAVENUM2(6) /980./, DELWAVE(6) /160./
      DATA WAVENUM1(7) /980./, WAVENUM2(7) /1080./, DELWAVE(7) /100./
      DATA WAVENUM1(8) /1080./, WAVENUM2(8) /1180./, DELWAVE(8) /100./
      DATA WAVENUM1(9) /1180./, WAVENUM2(9) /1390./, DELWAVE(9) /210./
      DATA WAVENUM1(10) /1390./,WAVENUM2(10) /1480./,DELWAVE(10) /90./
      DATA WAVENUM1(11) /1480./,WAVENUM2(11) /1800./,DELWAVE(11) /320./
      DATA WAVENUM1(12) /1800./,WAVENUM2(12) /2080./,DELWAVE(12) /280./
      DATA WAVENUM1(13) /2080./,WAVENUM2(13) /2250./,DELWAVE(13) /170./
      DATA WAVENUM1(14) /2250./,WAVENUM2(14) /2380./,DELWAVE(14) /130./
      DATA WAVENUM1(15) /2380./,WAVENUM2(15) /2600./,DELWAVE(15) /220./
      DATA WAVENUM1(16) /2600./,WAVENUM2(16) /3000./,DELWAVE(16) /400./

      DATA NG /16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16/
      DATA NSPA /1,1,10,9,9,1,9,1,11,1,1,9,9,1,9,9/
      DATA NSPB /1,1, 5,6,5,0,1,1, 1,1,1,0,0,1,0,0/

C     HEATFAC is the factor by which one must multiply delta-flux/ 
C     delta-pressure, with flux in w/m-2 and pressure in mbar, to get 
C     the heating rate in units of degrees/day.  It is equal to 
C           (g)x(#sec/day)x(1e-5)/(specific heat of air at const. p)
C        =  (9.8066)(3600)(1e-5)/(1.004)
      DATA HEATFAC /8.4391/

      DATA WX /MAXPROD*0.0/
c
      DATA HVRRTM / '2.18' /,        HVRREG / 'NOT USED' /,
     *     HVRRTR / 'NOT USED' /,   HVRATM / 'NOT USED' /,
     *     HVRSET / 'NOT USED' /,   HVRTAU / 'NOT USED' /,
     *     HVDUM1 / 4*'NOT USED' /, HVRUTL / 'NOT USED' /,
     *     HVREXT / 'NOT USED' /


      END




