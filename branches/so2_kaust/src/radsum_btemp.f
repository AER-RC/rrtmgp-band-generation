C     path:      /home/rc1/aer_radsum/src/SCCS/s.radsum.f
C     revision:  1.7
C     created:   09/06/95  09:33:33
C     presently: 10/03/96  09:27:33
C------------------------------------------------------------------------------
      PROGRAM RADSUM 
C
C******************************************************************************
C
C     REVSD. FROM RADDR TO COMPUTE RADIANCE SUMS OVER MODEL LAYERS BY 
C
C                                       M. J. IACONO, S. A. CLOUGH
C                                       A.E.R. INC., 26 MARCH 1990
C
C     GENERALIZED, CORRECTED - R. D. WORSHAM   30 AUGUST 1991
C     
C     CORRECTED FOR THREE ANGLE SUMMATION - M. J. IACONO  6 DECEMBER 1991  
C
C     REMOVED NEED FOR PRESSURES IN RADINIT -  P. D. BROWN FEB 1995
C
C     CHANGED TO V1, V2 INPUT, CLEANED, FIXED BUGS - E. J. MLAWER MARCH 1995
C
C     ***REMINDER**************************************************************
C     ** THIS CODE MUST BE COMPILED THE SAME WAY AS LBLRTM, WHETHER SINGLE OR *
C     ** DOUBLE PRECISION IS USED, IN ORDER FOR THE INPUT TO BE READ PROPERLY.*
C     *************************************************************************
C
C******************************************************************************
C
C     READS PANELS OF LBLRTM RADIANCE OUTPUT AT INTERVALS OF DV WAVENUMBERS,
C     AND SUMS OVER GROUPS OF OUTINRAT*DV WAVENUMBERS FOR FLUX OUTPUT. 
C     NANG ANGLES AND FIRST MOMENT QUADRATURE ARE USED FOR THE FLUX SUMMATION.
C
C     INPUT:
C     I)   TAPE31, TAPE32, TAPE33:  LBLRTM OUTPUT FILES OF DOWNWELLING 
C            RADIANCE (ONE FILE FOR EACH ANGLE)
C     II)  TAPE61, TAPE62, TAPE63:  LBLRTM OUTPUT FILES OF UPWELLING 
C            RADIANCE (ONE FILE FOR EACH ANGLE)
C     III) RADINIT -- CONTAINS:     
C          V1:  THE BEGINNING WAVENUMBER OF THE FIRST OUTPUT FLUX GROUP
C          V2:  THE ENDING WAVENUMBER OF THE FINAL OUTPUT FLUX GROUP
C          OUTINRAT:  THE FACTOR BY WHICH THE INCOMING DV (DVP) SHOULD BE
C               MULTIPLIED TO GET THE WIDTH OF EACH OUTPUT GROUP
C          NANG:  THE NUMBER OF ANGLES 
C          NLEV:  THE NUMBER OF LEVELS 
C          TBND:  THE SURFACE TEMPERATURE
C          IQUAD:  FLAG FOR QUADRATURE METHOD 
C                  = 0 FOR STANDARD FIRST-ORDER QUADRATURE
C                  = 1 FOR SPECIAL 3 ANGLES IDENTICAL TO THOSE USED IN RRTM
C     RESTRICTIONS ON INPUT:
C     1.  NANG IS FORCED TO BE 3 WHEN IQUAD = 1.
C     2.  NANG MUST BE <= 3.
C     3.  NLEV MUST BE LESS THAN 200.
C     4.  TO INSURE THAT AN EVEN NUMBER OF OUTPUT GROUPS FIT BETWEEN V1 AND
C         V2, THIS RELATIONSHIP MUST HOLD:  V2 - V1 = N * OUTINRAT * DVP,
C         WHERE N IS AN INTEGER.
C
C     OUTPUT:
C     I) flxupdn.dat
C        CONTAINS (AT OUTINRAT*DV WAVENUMBER INTERVALS): 
C              - LBLRTM PRESSURE LEVELS
C              - THE UPWARD, DOWNWARD, AND NET (UP-DOWN) FLUXES FOR ALL
C                LBLRTM LEVELS
C              - THE HEATING RATE FOR EACH LAYER, COMPUTED FROM NET FLUX,
C                INDEXED AND WRITTEN WITH THE BOTTOM LEVEL OF THE LAYER
C******************************************************************************
C 
      PARAMETER (MXFSC=200,LIMMAX=2500,MXANGL=3)
C
      IMPLICIT DOUBLE PRECISION (V)
C
      DOUBLE PRECISION XID,SEC,HMOLID,XALTZ,YID
      INTEGER KFILD(MXANGL),KFILU(MXANGL),LFILE,OUTINRAT
      REAL HTR,NETRAD
      CHARACTER*8 HVRRAD
      CHARACTER*1 DUM
C
      DIMENSION BOUND(LIMMAX)
      DIMENSION SRADD(LIMMAX,MXANGL),SRADU(LIMMAX,MXANGL)
      DIMENSION RADTTD(MXFSC,LIMMAX),RADTTU(MXFSC,LIMMAX),PRESLV(MXFSC)
      dimension radblk (mxfsc,limmax)
      DIMENSION HTR(MXFSC,LIMMAX),NETRAD(MXFSC,LIMMAX),PRETHK(MXFSC)
      DIMENSION FILHDR(2),PNLHDR(2),IWDF(2),IWDP(2)
      dimension filter(1000)
C
      COMMON /MANE/ RADD(LIMMAX,MXANGL),RADU(LIMMAX,MXANGL)
      COMMON /FILHDR/ XID(10),SEC,P0,T0,HMOLID(60),XALTZ(4),WK(60),
     *                PZL,PZU,TZL,TZU,WBROAD,DVT,V1V,V2V,TBOUND,
     *                EMISIV,FSCDID(17),NMOL,LAYRS,YID1,YID(10),LSTWDF
      COMMON /PNLHDR/ V1P,V2P,DVP,NLIM,LSTWDP 
      COMMON /CONSTS/ PI,PLANCK,BOLTZ,CLIGHT,AVOG,RADCN1,RADCN2
C
      EQUIVALENCE (FILHDR(1),XID(1)), (PNLHDR(1),V1P),
     *            (XID(1),IWDF(1))  , (V1P,IWDP(1)),
     *            (FSCDID(1),IHIRAC), (FSCDID(2),ILBLF4),
     *            (FSCDID(3),ICNTNM), (FSCDID(4),IAERSL),
     *            (FSCDID(5),IEMIT) , (FSCDID(6),ISCAN),
     *            (FSCDID(7),IPLOT) , (FSCDID(8),IPATHL), 
     *            (FSCDID(9),JRAD)  , (FSCDID(10),ITEST),
     *            (FSCDID(11),IMRG) , (FSCDID(12),SCNID),
     *            (FSCDID(13),HWHM) , (FSCDID(14),IDABS),
     *            (FSCDID(15),IATM) , (FSCDID(16),LAYR1),
     *            (FSCDID(17),NLAYFS)
C 
C     Assign SCCS version number to module
C
      DATA HVRRAD / '1.7' /

C     HEATFC is the factor one must multiply DELTA-FLUX/DELTA-PRESSURE, 
C     with flux in W/M-2 and pressure in Mb, to get the heating rate in
C     units of Degrees/day.  It is equal to 
C           (g)x(#sec/day)x(1e-5)/(specific heat of air at const. p)
C        =  (9.8066)(3600)(1e-5)/(1.004)
      DATA HEATFC /8.4391/
C
C     Initialize variables
      DATA PRESLV / MXFSC*0.0 /
C
C******************************************************************************
C 

C     Open filter function file and read in values of filter function.
      OPEN(11,FILE='TAPE5.FILTER',FORM='FORMATTED')
      READ(11,*)DUM
      READ(11,*)DUM
      READ(11,9876)V1F,DVF,NP
      READ(11,*)DUM
      DO 233 IFIL = 1, NP
         READ(11,9877)FILTER(IFIL)
 233  CONTINUE
      CLOSE(11)

      PI = 2.*ASIN(1.)
      RADCN1 = 2.*PLANCK*CLIGHT*CLIGHT*1.E-07
      RADCN2 = PLANCK*CLIGHT/BOLTZ
      EPS = 1.E-4

C     All incoming data have been processed.  For each output group,
C     calculate surface flux (using surface temperature) by summing
C     the Planck function computed at intervals of DV wavenumbers.

      outinrat = np - 1
      v1 = v1f
      v2 = v1f + outinrat*dvf
      dvp = dvf
         OUT = (V2 - V1)/(FLOAT(OUTINRAT)*DVP)
         NOUT = INT (OUT + EPS)
C
C        Compute width of output groups and wavenumbers of boundaries 
C        of output groups.
         OUTDV = FLOAT(OUTINRAT) * DVP
         DO 70 K = 1, NOUT
            BOUND(K) = V1 + OUTDV * FLOAT(K-1)
 70      CONTINUE
         BOUND(NOUT+1) = V2
      DV = OUTDV/FLOAT(OUTINRAT)

      OPEN(12,FILE='BTEMP_CHART',FORM='FORMATTED')

      tbnd = 200.0
      do 200 i=i,1500 
         fsum=0.0
         rvbar = 0.0
         XKT = TBND/RADCN2
         DO 150 IOUT = 1, NOUT
            FSUM = 0.
            DO 160 K = 1, OUTINRAT
               RVBAR = BOUND(IOUT) + DV * (FLOAT(K-1) )
               FSUM = FSUM + BBFCN(RVBAR,XKT) * DV * 1.E04 * filter(k)
 160     CONTINUE
 150  CONTINUE
      write(12,800) tbnd,fsum
      tbnd = tbnd+0.1
 200  continue
      close(12)
C
C     Formats:
C     
 800  FORMAT(F13.1,2x,1P,e13.6)
 900  FORMAT(2F10.2,2I5,F8.1,I5)
 910  FORMAT(2I5)
 920  FORMAT(' ')
 930  FORMAT('WAVENUMBER BAND: ',F8.2,' -',F8.2,' CM -1',15X,
     *       'RADSUM SCCS RADIANCE version ',A8)
 940  FORMAT(' Number of levels: ',i3,4x,
     *       'Surface Temperature (K): ',f10.4)
 950  FORMAT(' LEV   PRESSURE        FLUX UP       FLUX DOWN',
     *       ' ',/,
     *       '          MB             W/M2           W/M2  ')
 9952 FORMAT(1X,I3,6X,F7.6,3X,1P,E13.6,2X,E13.6,2X,E13.6,3X,E13.6) 
 9953 FORMAT(1X,I3,6X,F6.5,4X,1P,E13.6,2X,E13.6,2X,E13.6,3X,E13.6)
 9954 FORMAT(1X,I3,6X,F5.4,5X,1P,E13.6,2X,E13.6,2X,E13.6,3X,E13.6) 
 9955 FORMAT(1X,I3,5X,F5.3,6X,1P,E13.6,2X,E13.6,2X,E13.6,3X,E13.6) 
 9956 FORMAT(1X,I3,4X,F5.2,7X,1P,E13.6,2X,E13.6,2X,E13.6,3X,E13.6) 
 9957 FORMAT(1X,I3,3X,F5.1,8X,1P,E13.6,2X,E13.6,2X,E13.6,3X,E13.6) 
 9958 FORMAT(1X,I3,2X,F5.0,9X,1P,E13.6,2X,E13.6,2X,E13.6,3X,E13.6) 

 9876 FORMAT(F10.3,F10.4,I5)
 9877 FORMAT(E15.8)
 960  FORMAT(1X,0P,I2,1P,2X,E13.6,2X,E13.6,2X,E13.6,2X,E13.6,2X,E18.11) 
C
 9999 STOP
C
      END 
C
C------------------------------------------------------------------------------
C
      SUBROUTINE OPNFIL(KFILD,KFILU,NANG,LFILE,IOPT)
C 
C******************************************************************************
C     THIS SUBROUTINE OPENS THE NEEDED FILES. 
C 
      INTEGER KFILD(NANG), KFILU(NANG), LFILE
      CHARACTER TAPE*4,KFIL*6,CFORM*11
C
      DATA CFORM/'UNFORMATTED'/
C
      DATA TAPE/'TAPE'/
C 
      DO 10 I = 1,NANG
         IF (IOPT.EQ.0) THEN
            KFILD(I) = 30+I
            KFILU(I) = 60+I
            WRITE(KFIL,'(A4,I2.2)') TAPE,KFILD(I)
            OPEN (KFILD(I),FILE=KFIL,FORM=CFORM)
            WRITE(KFIL,'(A4,I2.2)') TAPE,KFILU(I)
            OPEN (KFILU(I),FILE=KFIL,FORM=CFORM)
         ENDIF  
         REWIND KFILD(I)
         REWIND KFILU(I)
  10  CONTINUE
C     
      IF (IOPT.EQ.0) THEN
         LFILE = 21
         OPEN (LFILE,FILE='radupdn.dat')
         REWIND LFILE
      ENDIF
C

      RETURN
      END 
C
C------------------------------------------------------------------------------
C
      SUBROUTINE RHEAD(XID,SV,DSV,NTOT,NVAR)
C 
C******************************************************************************
C     This subroutine extracts information from XID.
C 
      REAL XID(10), SV, DSV 
      INTEGER NTOT, NVAR
      CHARACTER*80 HEADER 
C 
      PRINT 10,XID
      WRITE(HEADER,20) XID
      READ(HEADER,30) SV,DSV,NTOT,NVAR
C
      RETURN
C
C     Formats:
C     
 10   FORMAT(' XID',15A8) 
 20   FORMAT(10A8)
 30   FORMAT(8X,1PE12.4,17X,1PE12.4,8X,I5,5X,I5,8X) 
C
      END 
C
C------------------------------------------------------------------------------
C
      FUNCTION NWDL(IWD,ILAST)                                           
C******************************************************************************
      DIMENSION IWD(*)                                                   
      ILAST = -654321                                                      
      DO 10 I = 1,9000
         IF(IWD(I).NE.ILAST) GO TO 10                                       
         NWDL = I-1
         GO TO 12
 10   CONTINUE
 12   RETURN
      END
C
C------------------------------------------------------------------------------
C
      SUBROUTINE ENDFIL(IFILE)
C******************************************************************************
      DIMENSION IDUM(6)
      DATA IDUM /6*-99/
      CALL BUFOUT(IFILE,IDUM,6)
      RETURN
      END
C
C------------------------------------------------------------------------------
C
      FUNCTION BBFCN(XVI,XKT)
C
C     Function BBFCN calculates the black body function for
C     wavenumber value XVI
C
      COMMON /CONSTS/ PI,PLANCK,BOLTZ,CLIGHT,AVOG,RADCN1,RADCN2
C
      IF (XKT.GT.0.0) THEN
C
         XVIOKT = XVI/XKT
C
         IF (XVIOKT.LE.0.01) THEN
            BBFCN = RADCN1*(XVI**2) *XKT/(1.+0.5*XVIOKT)
         ELSEIF (XVIOKT.LE.80.0) THEN
            BBFCN = RADCN1*(XVI**3)/(EXP(XVIOKT)-1.)
         ELSE
            BBFCN = 0.
         ENDIF
      ELSE
         BBFCN = 0.
      ENDIF
C
      RETURN
      END
C     
C
C------------------------------------------------------------------------------
C
      BLOCK DATA
C
      COMMON /CONSTS/ PI,PLANCK,BOLTZ,CLIGHT,AVOG,RADCN1,RADCN2
      DATA PLANCK/6.626176E-27/, BOLTZ/1.380662E-16/,
     *     CLIGHT/2.99792458E10/, AVOG/6.022045E23/
C
      END
C
C------------------------------------------------------------------------------
C




