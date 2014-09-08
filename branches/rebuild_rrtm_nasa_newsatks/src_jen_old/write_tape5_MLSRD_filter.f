
      PROGRAM WRITE_TAPE5_MLSRD
c----------------------------------------------------------------
c     CREATED: June 1998, Jennifer Simmons
c     PROGRAM: Creates one of two of the TAPE5 file for generation 
c     of fluxes. Information changes can occur through the 
c     data statements or more sophisticated techniques can be
c     developed at a later date.
c
c     INPUT FILES REQUIRED:
c     template_wavelength - two line file with the boundaries of 
c     wavelength region of interest
c     
c     OUTPUT FILES GENERATED:
c     TAPE5_MLSRD - Tape 5 file to generate the flux information
c     from LBLRTM. This is the second of two files needed to calculate
c     fluxes. The first file, TAPE5_MLSOD, is generated in the a
c     script file.
c     RADINIT - Input file for executable program radsum.x. For a 
c     definition of the file, see radsum.f
c----------------------------------------------------------------

      character*1 DUM
      INTEGER I
      REAL WAVENUM1, WAVENUM2, WV1,WV2
      DIMENSION NAMETAPE(2),ANGLE(2)
      DIMENSION WVN_LCOUPLE(10)

      DATA WVN_LCOUPLE/612.0,619.0,667.0,677.0,714.0,722.0,
     &     735.0,742.0,791.0,796.0/

      DATA NAMETAPE/31,61/
      DATA ANGLE/1.0,1.0/
      DATA JEMIT/1/,JFN/0/,JVAR/0/,SAMPL/0/,NPTS/0/
      DATA TBND/294.2/,NANG/1/,NLEV/52/,IQUAD/0/
      DATA ICN/1/
      integer igas_minor_l(7,1),igas_minor_u(7,1)
      namelist /par/ wavenumber1,wavenumber2,igas1_l,igas2_l,
     &               igas1_u,igas2_u,igas_minor_l,igas_minor_u

      read(*,par)

      OPEN(11,FILE='TAPE5.FILTER',FORM='FORMATTED')
      READ(11,*)DUM
      READ(11,*)DUM
      READ(11,9876)V1F,DVF,NP
      READ(11,*)DUM
      CLOSE(11)
C This adjustment is required by LBLRTM to generate the necessary optical depths.
C Note that when line coupling present, must extend the LBLRTM calculations to
C outside the boundary of this region.

      WV1 = WAVENUMBER1 - 5.0
      WV2 = WAVENUMBER2 + 5.0

      DO 500 IWVN=1,10,2
         IF (WV1 .GE. WVN_LCOUPLE(IWVN) .AND.
     &        WV1 .LE. WVN_LCOUPLE(IWVN+1)) THEN
              WV1 = WVN_LCOUPLE(IWVN)-5.0
         ENDIF
 500     CONTINUE
      DO 510 IWVN=1,10,2
         IF (WV2 .GE. WVN_LCOUPLE(IWVN) .AND.
     &        WV2 .LE. WVN_LCOUPLE(IWVN+1)) THEN
            WV2 = WVN_LCOUPLE(IWVN+1)+5.0
         ENDIF
 510  CONTINUE
      
      WVHM1=wavenumber1
      WVHM2=wavenumber2

      HWHM = DVF/2.0

      if (wv1.ge.125.and.wv1.lt.240.)  dvout=0.00002
      if (wv1.ge.240.and.wv1.le.325.)  dvout=0.00004
      if (wv1.gt.325.)  dvout=0.00005

      OPEN (UNIT=12, FILE='template_mlsod')
      WRITE(12,100)
      WRITE(12,101) '1        2         3         4         5',
     &           '         6         7         8         9'
      WRITE(12,102) '123456789-123456789-123456789-123456789-',
     &           '123456789-123456789-123456789-123456789-'
      WRITE(12,103)
      WRITE(12,104) ' HI=1 F4=1 CN=',ICN,
     &           ' AE=0 EM=0 SC=0 FI=0',
     &           ' PL=0 TS=0 AM=0 MG=1 LA=0    1        00   00'
      WRITE(12,106) WV1,WV2,DVOUT
      close(12)

      OPEN (13, FILE='TAPE5_MLSRD')

      WRITE(13, 100)
      WRITE(13,101) '1        2         3         4         5',
     &           '         6         7         8         9'
      WRITE(13,102) '123456789-123456789-123456789-123456789-',
     &           '123456789-123456789-123456789-123456789-'

      WRITE(13, 103)
      WRITE(13,104) ' HI=0 F4=0 CN=',ICN,
     &           ' AE=0 EM=1 SC=0 FI=0',
     &           ' PL=0 TS=0 AM=0 MG35 LA=0             00   00'
      WRITE(13, 108) WV1,WV2
      WRITE(13, 109)
      WRITE(13, 111)
      WRITE(13, 112) HWHM,WVHM1,WVHM2,JEMIT,JFN,JVAR,SAMPL,
     & NAMETAPE(1),NPTS
      WRITE(13, 113) ANGLE(1)


      WRITE(13, 103)
            WRITE(13,104) ' HI=0 F4=0 CN=',ICN,
     &           ' AE=0 EM=1 SC=0 FI=0',
     &           ' PL=0 TS=0 AM=0 MG36 LA=0             00   00'
      WRITE(13, 108) WV1,WV2
      WRITE(13, 110)
      WRITE(13, 111)
      WRITE(13, 112) HWHM,WVHM1,WVHM2,JEMIT,JFN,JVAR,SAMPL,
     & NAMETAPE(2),NPTS
      WRITE(13, 113) ANGLE(2)

      WRITE(13,120)
      close(13)

      OPEN (14,FILE='RADINIT')
      WRITE(14,130) WAVENUMBER1,WAVENUMBER2,NANG,NLEV,TBND,IQUAD
      WRITE(14,131)
      CLOSE(14)


 100  FORMAT('TAPE5 FOR MLS')
 101  FORMAT(A40,A40)
 102  FORMAT(2(A40))
 103  FORMAT('$  STANDARD MLS ATMOSPHERE')
 104  FORMAT(A14,i1,A20,A45)
 106  FORMAT(2f10.3,70x,e10.3)
 107  FORMAT(A20,A28,A12,A13)

 108  FORMAT(2f10.3,40x,'-1        -1')
 109  FORMAT('   0.     0.')
 110  FORMAT(' 294.2    1.')
 111  FORMAT('ODint_',50x,'51')
 112  FORMAT(1f10.7,2f10.3,3x,i2,3x,i2,3x,i2,f10.4,15x,i5,i5)
 113  FORMAT(f9.7)
 120  FORMAT('%%%%%')

 130  FORMAT(2F10.2,2I5,F8.1,I5,'<')
 131  FORMAT(' -1.')
 9876 FORMAT(F10.3,F10.4,I5)
      END





