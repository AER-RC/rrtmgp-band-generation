      PROGRAM WRITE_TAPE5_OD_MINORS

C***************************************************
cProgram version created by: Karen Cady-Pereira, October 2000
c
cCOMPILE INFORMATION: F90
c
cInput: Namelist which includes beginning wavenumber,
c     ending wavenumber and the gases to be included.
c     Also atmosphere type
c      
cOutput: Tape 5's for generating the optical depths as part of
c     the flux calculation process. write_tape5_MLSRD creates the
c     other necessary files. 
C     This code creates TAPE5 for the special case of processing only some  of the 
c     minor gases. In this case we want to create a TAPE5 with all major and some
c     of the minor species, and nothing else.
c
C***************************************************

      CHARACTER*73 RCRD21
      PARAMETER (MXL=200)
      CHARACTER*65 RCRD211 (MXL)
      DIMENSION AMOL(7,MXL)
      DIMENSION W(7)
      DIMENSION P(MXL), T(MXL), RHOTOT(MXL)
      dimension h2(mxl)
      DIMENSION ICN(2)
      DIMENSION WVN_LCOUPLE(10)

      DATA WVN_LCOUPLE/612.0,619.0,667.0,677.0,714.0,722.0,
     &     735.0,742.0,791.0,796.0/

      character*12 template
      character*3 upc_atmos(3)
      character*40 atm_cmnt2(3)
      character*13 atm_cmnt1
      character*26 tape5
      character*3 atmos
      character*5 atmos_level

      integer igas_minor_l(7,1),igas_minor_u(7,1)
      integer isortplanck(2)

      NAMELIST /PAR/ WAVENUMBER1,WAVENUMBER2,IGAS1_L,IGAS2_L,IGAS1_U,
     &                   IGAS2_U ,igas_minor_l,igas_minor_u

      NAMELIST /IPLANCK/ ISORTPLANCK
      NAMELIST /ATM/ ATMOS, ATMOS_LEVEL

C     Bolztman's constant is a factor of 1E-3 off to compensate for
C     the units of pressure and the fact that number density is required in
c     cm-3.
      
      DATA BOLTZ /1.38044E-19 /
      DATA AVOGAD / 6.022045E+23 /
      DATA WTWAT /18.015/
      DATA  TZERO / 273.15 /                           
      DATA  PZERO / 1013.25 /
      DATA RHOFAC /1.0006/
     
      DATA upc_atmos /'MLS', 'SAW', 'TRP'/
      DATA atm_cmnt2 / 
     &     '  STANDARD MLS ATMOSPHERE','  STANDARD SAW ATMOSPHERE',
     &     '   TROPICAL ATMOSPHERE ADJUSTED TO RIDGEWAYS GLA MODEL'/
 
c Define the continuum to be ON where icn(1) => lower atmosphere and 
c icn(2) => upper atmosphere.

      DATA icn /1,1/
      DATA iform /1/

      nminors = 7

C Read in Namelist that provides the information concerning wavelength and
C gases for each level.

      READ(*,PAR)
      READ(*,IPLANCK)
      READ(*,ATM)

C  Check that input_param is setup for  either the upper OR 
c  lower atmosphere (code cannot do both levels at once)

      if (atmos_level .ne. "UPPER" .and. 
     &    atmos_level .ne. "LOWER") then
         print *, "Must choose UPPER or LOWER atmosphere"
	 print *, " Will not run write_tape5_not_all_minors"
	 stop
      endif

C  Determine atmospheric index

      if (atmos .eq. 'mls' ) then
         iatm = 1
      elseif (atmos .eq. 'saw' ) then
         iatm = 2
      elseif (atmos .eq. 'trp' ) then
         iatm = 3
      else
         print *, "Incorrect atmosphere in input_param"
	 stop
      endif

C  Determine output filename

      if (atmos_level .eq. "LOWER")
     &      tape5 = "TAPE5_"//upc_atmos(iatm)//"OD_MINORS"
      print *, tape5
      if (atmos_level .eq. "UPPER")
     &      tape5 = "TAPE5_"//upc_atmos(iatm)//"OD_MINORS"
      print *, tape5

C  Determine template (51 layer atmospheric profile and tape5 header

      template = "template_"//atmos
      print *, template
      atm_cmnt1 = "TAPE5 FOR "//upc_atmos(iatm) 

C This adjustment is required by LBLRTM to generate the necessary optical depths.

      WAVENUM1=WAVENUMBER1-5.0
      WAVENUM2=WAVENUMBER2+5.0

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

      DVOUT = 3.0e-5
      IF (WAVENUM1.GE.125.AND.WAVENUM1.LT.240.)  DVOUT=0.00002
      IF (WAVENUM1.GE.240.AND.WAVENUM1.LE.325.)  DVOUT=0.00004
      IF (WAVENUM1.GT.325.)  DVOUT=0.00005
      IF (WAVENUM1.GT.1300.)  DVOUT=0.0002
      IF (WAVENUM1.GT.1700.)  DVOUT=0.0003
      IF (WAVENUM1.GT.2590.)  DVOUT=0.00045

c  Open output and write out first six lines

      OPEN (UNIT=12, FILE=tape5)
      WRITE(12,'(a13)') atm_cmnt1 
      WRITE(12,101) '1        2         3         4         5',
     &           '         6         7         8         9'
      WRITE(12,102) '123456789-123456789-123456789-123456789-',
     &           '123456789-123456789-123456789-123456789-'
      WRITE(12,103) atm_cmnt2(iatm)
      WRITE(12,104)
     & ' HI=1 F4=1 CN=', ICN(1), ' AE=0 EM=0 SC=0 FI=0',
     & ' PL=0 TS=0 AM=0 MG=1 LA=0    1        00   00'
      WRITE(12,106) WAVENUM1, WAVENUM2, DVOUT


C Open file containing atmospheric profiles used for flux calculation

      OPEN (10, FILE= template, STATUS = 'OLD')

C  Read in first line and extract number of levels

      read(10,'(a73)') RCRD21
      WRITE(12,'(a73)') RCRD21
      read(RCRD21,'(3x,i2)') nlev
      print *, nlev
      
c  Read in profile values

      DO 1000 LEV = 1, NLEV
         READ(10,9001) P(LEV), T(LEV), rcrd211(LEV)
         READ (rcrd211(lev),'(39x,f6.2)') hlay
         
         h2(lev) = hlay
         if (lev .gt. 1) h2(lev) = h2(lev) - h1
         READ(10,9002) (AMOL(I,LEV),I=1,7)

         RHOTOT(LEV) = RHOFAC*P(LEV)/(BOLTZ*T(LEV))

	 IF (((P(LEV)-115.)*(P(LEV-1)-115.)) .LT. 0.0) LEVDUP = LEV
         h1 = hlay
1000  CONTINUE
      CLOSE(10)

      print *,'levdup = ',levdup

C  Note: 1.e5*h2(lev) factor in the calculation of BROAD roughly converts from cm3 to cm2 (layer
c        amount)
         
      DO LEV = 1, NLEV
	 W = 0.0
	 if(igas1_l.ne.0 .and. lev.le.levdup) W(IGAS1_L) = AMOL(IGAS1_L,LEV)
	 if(igas2_l.ne.0 .and. lev.le.levdup) W(IGAS2_L) = AMOL(IGAS2_L,LEV)
	 if(igas1_u.ne.0 .and. lev.gt.levdup) W(IGAS1_U) = AMOL(IGAS1_U,LEV)
	 if(igas2_u.ne.0 .and. lev.gt.levdup) W(IGAS2_U) = AMOL(IGAS2_U,LEV)

         do im = 1, nminors
	    if(igas_minor_l(im,1) .ne.0 .and. lev.le.levdup) 
     &         W(im) = AMOL(im,LEV)
	    if(igas_minor_u(im,1) .ne.0 .and. lev.gt.levdup) 
     &         W(im) = AMOL(im,LEV)
         end do 

	 WRITE(12,9023) P(LEV),T(LEV), RCRD211(LEV)
         if (atmos .ne. 'saw') then 
            WATER = W(1)*RHOTOT(LEV)/(1.+W(1))
	    RHODRY = RHOTOT(LEV)-WATER
	    BROAD=RHODRY*1.E5*h2(lev)*(1-W(2)-W(3)-W(4)-W(5)-
     &           W(6)-W(7))
         else 
	    BROAD=1.E5*h2(lev)*rhotot(lev)-W(1)-W(2)-W(3)-W(4)-W(5)-
     &           W(6)-W(7)
         endif
	 WRITE(12,9015) W, BROAD
      ENDDO


      WRITE(12,109)
      CLOSE (12)

 101  FORMAT(A40,A40)
 102  FORMAT(2(A40))
 103  FORMAT('$',a40)
 104  FORMAT(A14,i1,A20,A45)
 106  FORMAT(2f10.3,70x,e10.3)
 109  FORMAT('%%%%%')

 9001 FORMAT (e15.7, f10.4, a65)
 9002 FORMAT (7e15.7)
 9015 FORMAT (1P,8G15.7,0P)
 9023 FORMAT (G15.7,G10.5,A65)

      STOP 
      END


    
      
