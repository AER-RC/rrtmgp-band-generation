      PROGRAM COMPUTE_BBODY_PROFILE_MLS

      IMPLICIT DOUBLE PRECISION (V)

      parameter(mxlay=203)
      parameter (nbtp = 2000)
      CHARACTER*1 DUM
      dimension filter(1000)
      dimension ilevl(mxlay),pmbl(mxlay),uradl(mxlay)
      dimension ilev(mxlay),pmb(mxlay),urad(mxlay)
      dimension radbt(nbtp),tbt(nbtp),tbt_levl(mxlay),tbt_lev(mxlay)

      CHARACTER*50 FILE_LBL, FILE_ARRTM, OUTFILE
      INTEGER NLEV

      COMMON /CONSTS/ PI,PLANCK,BOLTZ,CLIGHT,AVOG,RADCN1,RADCN2

      NAMELIST /BTINFO/ FILE_LBL, FILE_ARRTM, OUTFILE, NLEV

      read (*,BTINFO)

      PI = 2.*ASIN(1.)
      RADCN1 = 2.*PLANCK*CLIGHT*CLIGHT*1.E-07
      RADCN2 = PLANCK*CLIGHT/BOLTZ

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

      dv = dvf
      outinrat = np-1
      tbnd_start = 150.0
      do 300 i=1,nbtp 
         tbt(i) = tbnd_start + 0.1*float(i-1)
         rvbar = 0.0
         XKT = tbt(i)/RADCN2
            FSUM = 0.
            RVBAR = 0.0
            DO 260 K = 1, OUTINRAT
               RVBAR = v1f + DV * (FLOAT(K-1) )
               FSUM = FSUM + BBFCN(RVBAR,XKT) * DV * 1.E04 * filter(k)
 260     CONTINUE
         RADBT(i) = FSUM
 300  continue

C     Open LBLRTM file and read in values of upward radiance

      OPEN(11,FILE=FILE_LBL,FORM='FORMATTED')
      READ(11,*)DUM
      READ(11,*)DUM
      READ(11,*)DUM
      READ(11,*)DUM

      do 400 i=1,nlev
         read(11,*) ilevl(i),pmbl(i),uradl(i)
         call locate(radbt,nbtp,uradl(i),nloc)        
         if (nloc .eq. 0) then
            print*,'WARNING: EXTRAPOLATING BT ', gmu_inv 
            ja = 1
            jb = 2
         else if (nloc .eq. npts) then
            print*,'WARNING: EXTRAPOLATING BT ', gmu_inv 
            ja = npts - 1
            jb = npts
         else
            ja = nloc
            jb = nloc+1
         endif
         p = (uradl(i) - radbt(ja))/(radbt(jb)-radbt(ja))
         tbt_levl(i) = p*tbt(jb) + (1.0-p)*tbt(ja)
 400    continue 
      close(11)

C     Open ARRTM file and read in values of upward radiance

      OPEN(11,FILE=FILE_ARRTM,FORM='FORMATTED',status='old')
      READ(11,*)DUM
      READ(11,*)DUM
      READ(11,*)DUM
      do 410 i=1,nlev
         read(11,*) ilev(i),pmb(i),urad(i)
         call locate(radbt,nbtp,urad(i),nloc)        
         if (nloc .eq. 0) then
            print*,'WARNING: EXTRAPOLATING BT ', gmu_inv 
            ja = 1
            jb = 2
         else if (nloc .eq. npts) then
            print*,'WARNING: EXTRAPOLATING BT ', gmu_inv 
            ja = npts - 1
            jb = npts
         else
            ja = nloc
            jb = nloc+1
         endif
         p = (urad(i) - radbt(ja))/(radbt(jb)-radbt(ja))
         tbt_lev(i) = p*tbt(jb) + (1.0-p)*tbt(ja)
 410    continue 
      close(11)

      open(11,file=outfile)
      write(11,950) 
      j = nlev
      do 500 n=1,nlev
            If (PMBL(N) .LT. 1.E-2) THEN
               WRITE(11,9952) j-1, PMBL(N), TBT_LEVL(N), 
     &              tbt_lev(n)
            ELSEIF (PMBL(N) .LT. 1.E-1) THEN
               WRITE(11,9953) j-1, PMBL(N), TBT_LEVL(N), 
     &               tbt_lev(n)
            ELSEIF (PMBL(N) .LT. 1.) THEN
               WRITE(11,9954) j-1, PMBL(N), TBT_LEVL(N), 
     &              tbt_lev(n)
            ELSEIF (PMBL(N) .LT. 10.) THEN
               WRITE(11,9955) j-1, PMBL(N), TBT_LEVL(N), 
     &              tbt_lev(n)
            ELSEIF (PMBL(N) .LT. 100.) THEN
               WRITE(11,9956) j-1, PMBL(N), TBT_LEVL(N), 
     &              tbt_lev(n)
            ELSEIF (PMBL(N) .LT. 1000.) THEN
               WRITE(11,9957) j-1, PMBL(N), TBT_LEVL(N), 
     &              tbt_lev(n)
            ELSE
               WRITE(11,9958) j-1, PMBL(N), TBT_LEVL(N), 
     &              tbt_lev(n)
            ENDIF
            j = j-1
 500     continue
      close(11)


 950  FORMAT(' LEV   PRESSURE       BTEMP(LBL)    BTEMP(ARRTM)',
     *       ' ',/,
     *       '          MB               K            K  ')
 9952 FORMAT(1X,I3,6X,F7.6,3X,1P,E13.6,2X,E13.6) 
 9953 FORMAT(1X,I3,6X,F6.5,4X,1P,E13.6,2X,E13.6)
 9954 FORMAT(1X,I3,6X,F5.4,5X,1P,E13.6,2X,E13.6) 
 9955 FORMAT(1X,I3,5X,F5.3,6X,1P,E13.6,2X,E13.6) 
 9956 FORMAT(1X,I3,4X,F5.2,7X,1P,E13.6,2X,E13.6) 
 9957 FORMAT(1X,I3,3X,F5.1,8X,1P,E13.6,2X,E13.6) 
 9958 FORMAT(1X,I3,2X,F5.0,9X,1P,E13.6,2X,E13.6) 
 9876 FORMAT(F10.3,F10.4,I5)
 9877 FORMAT(E15.8)

      end
      
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

C------------------------------------------------------------------------------
C

      SUBROUTINE LOCATE(XX,N,X,J)

C     GIVEN AN ARRAY XX(1:N), AND GIVEN A VALUE X, RETURN A VALUE J
C     SUCH THAT X IS BETWEEN XX(J) AND XX(J+1). XX(1:N) MUST BE MONOTONIC,
C     IN EITHER ASCENDING OR DESCENDING ORDER. A VALUE OF RETURNED OF J=0
C     OR J=N SIGNIFIES THAT X IS OUT OF RANGE.
C     OBTAINED FROM NUMERICAL RECIPES FOR FORTRAN 77.

      INTEGER J,N
      REAL X,XX(N)
      INTEGER JL,JM,JU
      JL=0
      JU=N+1
 10   IF(JU-JL.GT.1) THEN
         JM=(JU+JL)/2
         IF((XX(N).GE.XX(1)).EQV.(X.GE.XX(JM)))THEN
            JL=JM
         ELSE
            JU=JM
         ENDIF
      GOTO 10
      ENDIF
      IF(X.EQ.XX(1))THEN
         J=1
      ELSE IF(X.EQ.XX(N))THEN
         J=N-1
      ELSE
         J=JL
      ENDIF
      RETURN
      END
