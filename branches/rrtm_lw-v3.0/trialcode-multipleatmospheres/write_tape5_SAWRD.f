
      PROGRAM WRITE_TAPE5_TRPRD
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
c     TAPE5_SAWRD - Tape 5 file to generate the flux information
c     from LBLRTM. This is the second of two files needed to calculate
c     fluxes. The first file, TAPE5_SAWOD, is generated in the a
c     script file.
c     RADINIT - Input file for executable program radsum.x. For a 
c     definition of the file, see radsum.f
c----------------------------------------------------------------

      INTEGER I
      REAL WAVENUM1, WAVENUM2, WV1,WV2
      DIMENSION NAMETAPE(6),ANGLE(6)

      DATA NAMETAPE/31,32,33,61,62,63/
      DATA ANGLE/0.82,0.41,0.273333,0.82,0.41,0.273333/
      DATA HWHM/0.25/,JEMIT/1/,JFN/0/,JVAR/0/,SAMPL/0/,NPTS/0/
      DATA TBND/257.2/,NANG/3/,NLEV/60/,IQUAD/1/
      DATA ICN/1/
      integer igas_minor_l(7,1),igas_minor_u(7,1)
      namelist /par/ wavenumber1,wavenumber2,igas1_l,igas2_l,
     &               igas1_u,igas2_u,igas_minor_l,igas_minor_u

      read(*,par)

      wv1=wavenumber1-5.0
      wv2=wavenumber2+5.0

      WVHM1=wavenumber1-0.25
      WVHM2=wavenumber2+0.25
      DVOUT = 3.0e-5
      if (wv1.ge.125.and.wv1.lt.240.)  dvout=0.00002
      if (wv1.ge.240.and.wv1.le.325.)  dvout=0.00004
      if (wv1.gt.325.)  dvout=0.00005
      IF (WAVENUM1.GT.1300.)  DVOUT=0.0002
      IF (WAVENUM1.GT.1700.)  DVOUT=0.0003
      IF (WAVENUM1.GT.2590.)  DVOUT=0.00045

      OPEN (UNIT=12, FILE='template_sawod',STATUS='NEW')
      WRITE(12,100)
      WRITE(12,101) '1        2         3         4         5',
     &           '         6         7         8         9'
      WRITE(12,102) '123456789-123456789-123456789-123456789-',
     &           '123456789-123456789-123456789-123456789-'
      WRITE(12,103)
      WRITE(12,104) ' HI=1 F4=1 CN=',ICN,
     &           ' AE=0 EM=0 SC=0 FI=0',
     &           ' PL=0 TS=0 AM=0 MG=1 LA=0    1        00   00'
      print*,'wv1',wv1,wv2
      WRITE(12,106) WV1,WV2,DVOUT
c      WRITE(12,107) ' 1 59 7   1.000000  ', 
c     &           'SUBARCTIC   WINT H1=   0.00 ',
c     &           'H2= 70.00   ', 'ANG=   0.000  LEN= 0 '
c      write(12,107)' 1 59    7  1.000000',
c     &'SUBARCTIC   WINT H1=  63.251 H2=    0.00 ANG= 180.000 LEN= 0'
      close(12)

      OPEN (13, FILE='TAPE5_SAWRD')

      WRITE(13, 100)
      WRITE(13,101) '1        2         3         4         5',
     &           '         6         7         8         9'
      WRITE(13,102) '123456789-123456789-123456789-123456789-',
     &           '123456789-123456789-123456789-123456789-'
      DO 50 I=1,3
      WRITE(13, 103)
      WRITE(13,104) ' HI=0 F4=0 CN=',ICN,
     &           ' AE=0 EM=1 SC=0 FI=0',
     &           ' PL=0 TS=0 AM=0 MG35 LA=0             00   00'
      WRITE(13, 108) WV1,WV2
      WRITE(13, 109)
      WRITE(13, 111)
      WRITE(13, 112) HWHM,WVHM1,WVHM2,JEMIT,JFN,JVAR,SAMPL,
     & NAMETAPE(I),NPTS
      WRITE(13, 113) ANGLE(I)
 50   CONTINUE

      DO 60 I=4,6 
      WRITE(13, 103)
            WRITE(13,104) ' HI=0 F4=0 CN=',ICN,
     &           ' AE=0 EM=1 SC=0 FI=0',
     &           ' PL=0 TS=0 AM=0 MG36 LA=0             00   00'
      WRITE(13, 108) WV1,WV2
      WRITE(13, 110)
      WRITE(13, 111)
      WRITE(13, 112) HWHM,WVHM1,WVHM2,JEMIT,JFN,JVAR,SAMPL,
     & NAMETAPE(I),NPTS
      WRITE(13, 113) ANGLE(I)
 60   CONTINUE
      WRITE(13,120)
      close(13)

      OPEN (14,FILE='RADINIT')
      WRITE(14,130) WV1+5.,WV2-5.,NANG,NLEV,TBND,IQUAD
      WRITE(14,131)
      CLOSE(14)


 100  FORMAT('TAPE5 FOR SAW')
 101  FORMAT(A40,A40)
 102  FORMAT(2(A40))
 103  FORMAT('$  STANDARD SAW ATMOSPHERE')
 104  FORMAT(A14,i1,A20,A45)
 106  FORMAT(2f10.3,70x,e10.3)
c 107  FORMAT(A20,A28,A12,A13)
 107  format(a20,a60)
 108  FORMAT(2f10.3,40x,'-1        -1')
 109  FORMAT('   0.     0.')
 110  FORMAT(' 257.2    0.')
 111  FORMAT('ODint_',50x,'59')
 112  FORMAT(3f10.3,3x,i2,3x,i2,3x,i2,f10.4,15x,i5,i5)
 113  FORMAT(f9.7)
 120  FORMAT('%%%%%')

 130  FORMAT(2F10.2,2I5,F8.1,I5,'<')
 131  FORMAT(' -1.')

      END





