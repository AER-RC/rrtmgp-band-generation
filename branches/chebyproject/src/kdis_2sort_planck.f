      PROGRAM KDIS_2SORT_MINOR
C***************************************************
cProgram version created by: Jennifer Delamere January, 1999
cModified from routine received from E. Mlawer, AER, Inc.
cCOMPILE INFORMATION: F90
c
cRoutine is designed to sort LBLRTM optical depths to
c     generate the k-distributions. This particular model
c     is tailored for the UPPER atmosphere and handles
c     both one or two major gases (utilizing Mlawer's 
c     binary species parameter).
c
cInput:  Namelist which includes beginning wavenumber,
c     ending wavenumber and the gases to be included.
c     ODint_nn - Optical depth file from LBLRTM which
c     includes information from all MLS gases.
c     NCint_nn - Optical depth file from LBLRTM which
c     includes information from only MLS major gases
c     and continua EXCEPT water vapor.
c
cOutput:  CK_ABS.DAT (f90 unformatted file) containing the
c     the absorption coefficients as a function of pressure, 
c     temperature, and g-band.
c
cVariable definitions:
c     XKL(level) - The array that contains the optical information
c     from LBL runs with all absorbers and continua.
c     XCONT(level) - The array that contains the optical information
c     from LBL runs with ONLY the major gases of interest, including
c     continua EXCEPT the water vapor (self and foreign) continuum.
c     CONTAVG(ig) - The average optical depth per g-band.
c     COLUMFAC - (1.e20 / column amount W) where 1.e20 is a
c     factor used to help ease of computing. 
c     ABSCOEF = CONTAVG(IG)*COLUMFAC or 
c                            CONTAVG(IG)*1.e20/Column amount
C***************************************************
      PARAMETER (NLINES=5000000,MLAYERS=47,MG=16)

      IMPLICIT DOUBLE PRECISION (V)
      DOUBLE PRECISION XID(10),SECANT,HMOL(60),XALTZ(4),YID(10)
      EXTERNAL TIME

      INTEGER TIME,T1,T2
      REAL PAVE,TAVE,WK(64),WBROAD,DELV,TBOUND,EMISIV,
     & FSCDID(17),YI1
      REAL DVP,Y(2400),Z(2400),fcoef(0:3000),fcoe(0:300)

      CHARACTER*50 FILE1,FILE2,FILE4,FILE5

      CHARACTER*2 FNUM(MLAYERS)

      DIMENSION NPANEL(MLAYERS)
      DIMENSION P(MLAYERS),T(MLAYERS)
      DIMENSION NLINE(MLAYERS)
      DIMENSION G0(MG),G1(MG),WT(MG)
      DIMENSION XKL(NLINES),XCONT(NLINES)
      DIMENSION NPTS0(16),VCONTTOT(16)
      DIMENSION VCONTAVG(16),CONTAVG(16)
      DIMENSION IINDEX(NLINES),XTEMP(NLINES),J0MIN(16),J0MAX(16)
      dimension igas_minor_l(7,1),igas_minor_u(7,1)
      dimension isortplanck(2)
      DIMENSION W_L_MLS(7,13)
      DIMENSION W_H_MLS(7,47)
      
      NAMELIST /PAR/ WAVENUMBER1,WAVENUMBER2,IGAS1_L,IGAS2_L,
     &               IGAS1_U,IGAS2_U,igas_minor_l,igas_minor_u
      namelist /igaspar/ igas_on,iatmtype
      namelist /iplanck/ isortplanck

      data igas_on/0/
      DATA NLAYERS/47/
      DATA INFINITY/50000/
      DATA NLINE/MLAYERS*0/
      DATA FNUM/'1','2','3','4','5','6','7','8','9','10',
     &    '11','12','13','14','15','16','17','18','19','20',
     &    '21','22','23','24','25','26','27','28','29','30',
     &    '31','32','33','34','35','36','37','38','39','40',
     &    '41','42','43','44','45','46','47'/
      DATA WT/
     &     0.1527534276,0.1491729617,0.1420961469,0.1316886544,
     &     0.1181945205,0.1019300893,0.0832767040,0.0626720116,
     &     0.0424925,0.0046269894,0.0038279891,0.0030260086,
     &     0.0022199750,0.0014140010,0.000533,0.000075/
       DATA W_L_MLS(1,1:13)/
     &  1.8760E-02,  1.2223E-02,  5.8909E-03,  2.7675E-03,  1.4065E-03,
     &  7.5970E-04,  3.8876E-04,  1.6542E-04,  3.7190E-05,  7.4765E-06,
     &  4.3082E-06,  3.3319E-06,  3.2039E-06/
       DATA W_L_MLS(2,1:13)/
     &  3.5500E-04,  3.5500E-04,  3.5500E-04,  3.5500E-04,  3.5500E-04,
     &  3.5500E-04,  3.5500E-04,  3.5500E-04,  3.5500E-04,  3.5500E-04,
     &  3.5500E-04,  3.5500E-04,  3.5500E-04/
       DATA W_L_MLS(3,1:13)/
     &  3.0170E-08,  3.4725E-08,  4.2477E-08,  5.2759E-08,  6.6944E-08,
     &  8.7130E-08,  1.1391E-07,  1.5677E-07,  2.1788E-07,  3.2443E-07,
     &  4.6594E-07,  5.6806E-07,  6.9607E-07/
       DATA W_L_MLS(4,1:13)/
     &  3.2000E-07,  3.2000E-07,  3.2000E-07,  3.2000E-07,  3.2000E-07,
     &  3.1965E-07,  3.1532E-07,  3.0383E-07,  2.9422E-07,  2.8495E-07,
     &  2.7671E-07,  2.6471E-07,  2.4285E-07/
       DATA W_L_MLS(5,1:13)/
     &  1.5000E-07,  1.4306E-07,  1.3474E-07,  1.3061E-07,  1.2793E-07,
     &  1.2038E-07,  1.0798E-07,  9.4238E-08,  7.9488E-08,  6.1386E-08,
     &  4.5563E-08,  3.3475E-08,  2.5118E-08/
       DATA W_L_MLS(6,1:13)/
     &  1.7000E-06,  1.7000E-06,  1.6999E-06,  1.6904E-06,  1.6671E-06,
     &  1.6351E-06,  1.6098E-06,  1.5590E-06,  1.5120E-06,  1.4741E-06,
     &  1.4385E-06,  1.4002E-06,  1.3573E-06/
       DATA W_L_MLS(7,1:13)/
     &  0.2090    ,  0.2090    ,  0.2090    ,  0.2090    ,  0.2090    ,
     &  0.2090    ,  0.2090    ,  0.2090    ,  0.2090    ,  0.2090    ,
     &  0.2090    ,  0.2090    ,  0.2090    /
       DATA W_H_MLS(1,1:47)/
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
       DATA W_H_MLS(2,1:47)/
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
       DATA W_H_MLS(3,1:47)/
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
       DATA W_H_MLS(4,1:47)/
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
       DATA W_H_MLS(5,1:47)/
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
       DATA W_H_MLS(6,1:47)/
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
       DATA W_H_MLS(7,1:47)/
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

      DATA HCSQRD /5.955309E-13/,  HCOK /1.438786/
C     *** Statement function to calculate the Planck function

      BBFN(V,TE) = 2.*HCSQRD*V**3/(EXP(V/(TE/HCOK)) -1.)

C  ************ START OF EXECUTABLE STATEMENTS **********

      READ (*,PAR)
      read (*,iplanck)
      read(*,igaspar)

      V1USER = WAVENUMBER1
      V2USER = WAVENUMBER2

      ICONT = 17
      INF = 8
      IBIN =  7

      FILE4  = 'CK_ABS_PL.DAT'
      OPEN(IBIN,FILE=FILE4,  FORM='FORMATTED')

      t1=time()

C  *************** NUMERICAL INTEGRATION SECTION ****
C     GET GAUSS POINTS AND WEIGHTS AND CONSTRUCT THE GAUSSIAN    
C     INTERVALS FOR INTEGRATION.

C     *** NG is the number of gauss points desired, interval of 0 to 1, 
C         number of subintervals: 1, WT is the weight for a gauss point,
C         G is the cumulative probability function at a gauss point, and
C         IER is an error code (see GAUSL1 subroutine). Weights and g(k)
C         returned from subroutine.


C  ********************** NUMERICAL INTEGRATION SECTION *********************  C

C     GET GAUSS POINTS AND WEIGHTS AND CONSTRUCT THE GAUSSIAN    
C     INTERVALS FOR INTEGRATION.
      G1(0) = 0.
      DO 300 I = 1, MG
         G0(I) = G1(I-1)
         G1(I) = G0(I)  + WT(I)
 300  CONTINUE

C  *********** CALCULATION LOOP OVER ATMOSPHERIC LAYERS **********  C
c      DO 5000 LEVEL=1, NLAYERS
         level = isortplanck(igas_on)
         print*,'level',level
C         Otherwise, read from the ODKD files (from LBLRTM).
c          The NC- files represent the OD output from LBLRTM where you only 
c          have one key species and all other species are set to zero.
         IF (LEVEL .LT. 10) THEN
            FILE1 = 'ODint_0'//FNUM(LEVEL)
c            FILE2 = 'NCint_0'//FNUM(LEVEL)
         ELSE 
            FILE1 = 'ODint_'//FNUM(LEVEL)
c            FILE2 = 'NCint_'//FNUM(LEVEL)
         ENDIF

         OPEN(INF,FILE=FILE1,FORM='UNFORMATTED')
c         OPEN(ICONT,FILE=FILE2,FORM='UNFORMATTED')

C     *** READ LBLRTM FILE HEADER.
C     *** V1 : Red boundary of wide band (multiple panels)
C     *** V2 : Blue boundary of wide band (multiple panels)
C     *** DELV : small spectral interval for the layer - also, generally 
C         equal to the small spectral interval for each panel, DVP
C     *** WBROAD : width of the broad band (several panels)

C           WRITE (*,*)LEVEL
c INF file contains the information from all gases in the MLS
c ICONT file contains the information from only major gases in MLS,
c     with continua information except from water vapor.
           READ(INF,ERR=5001,END=5001) XID,SECANT,PAVE,TAVE,HMOL,XALTZ,
     &        WK,WBROAD,DELV,V1,V2,TBOUND,EMISIV,FSCDID,
     &        NMOL,LAYER,YI1,YID(10),LSTWDF
c           READ(ICONT,ERR=5001,END=5001) XID,SECANT,PAVE,TAVE,HMOL,
c     &        XALTZ,WK,WBROAD,DELV,V1,V2,TBOUND,EMISIV,FSCDID,
c     &        NMOL,LAYER,YI1,YID(10),LSTWDF
           IF (LAYER .GT. NLAYERS) THEN
              WRITE (*,*) 'TOO MANY LAYERS; CHANGE COMPILE-TIME PARMS'
              WRITE (*,*) 'LBLRTM THINKS THE LAYER IS',LAYER
           ENDIF


C  **************** BEGIN LOOP OVER PANELS FOR A GIVENS LAYER ****************  C

C     *** START COUNTING K'S FOR THIS LAYER

C     *** (INFINITY MEANS UNTIL EJECTED FROM THE LOOP)
C     *** READ THE LBLRTM PANEL HEADER FOR THE NEXT SPECTRAL BAND.
C     *** V1P, V2P : Red and blue boundaries of a panel. This spans
C         many spectral divisions, but generally does not span the
C         wide band.
C     *** DVP : small spectral interval for a panel
C     *** NLIM : number of values of Y, the optical thickness, per panel
C     *** NLINE : number of spectral points (values of Y) for each
C         layer (for every panel)

           NLINE(LEVEL) = 0
           DO 2000 IPANEL=1,INFINITY
                READ(INF) V1P,V2P,DVP,NLIM
c                READ(ICONT) V1P,V2P,DVP,NLIM
                IF (NLIM.EQ.-99) GO TO 2001
                IF ((NLINE(LEVEL)+NLIM) .GT. NLINES) THEN
                   PRINT*,'WARNING: NLINES EXCEEDED'
                   STOP
                ENDIF

C     *** READ THE OPTICAL DEPTHS FOR THIS PANEL; 
C         MOVE THEM INTO VARIABLE FOR ABSORPTION COEFFICIENTS

C     *** The Y's are the optical depths for each frequency
C	  in the LBLRTM output "panel".

 1              READ(INF) (Y(J), J=1,NLIM)
c                READ(ICONT) (Z(J), J=1,NLIM)

C     *** Move optical depths, Y, into XKL These optical depths are for 
C the entire atmosphere  including water vapor, the continuum, all other pertinent 
C gases etc. The including water vapor, the continuum, all other pertinent gases etc. 
C  The XCONT array is the primary absorber for the band. You are sorting 
C XCONT according to XKL. This is to keep consistency when you have multiple 
C key species.

                DO 2005 ILINE = 1, NLIM
                     IF(Y(ILINE) .LE. 0.0) Y(ILINE) = 0.0
                     XKL(ILINE+NLINE(LEVEL)) = Y(ILINE)
c                     IF(Z(ILINE) .LE. 0.0) Z(ILINE) = 0.0
c	             XCONT(ILINE+NLINE(LEVEL)) = Z(ILINE)
2005	        CONTINUE

	        NLINE(LEVEL) = NLINE(LEVEL) + NLIM
2000       CONTINUE
2001       CONTINUE

           NPANEL(LAYER) = IPANEL -1


C  **************** END OF LOOP OVER PANELS FOR A GIVEN LAYER ***************  C
C     *** CHOOSE DESIRED SPECTRAL RANGE

           IF ((V1USER.LT.V1).OR.(V1USER.GT.V2)) THEN
              WRITE(*,*) 'V1USER OUT OF BOUNDS'
              STOP
            ELSE IF ((V2USER.LT.V1).OR.(V2USER.GT.V2)) THEN
              WRITE(*,*) 'V2USER OUT OF BOUNDS'
              STOP
            ELSE IF (V2USER.LE.V1USER) THEN
	      WRITE(*,*) 'V2USER < V1USER'
	      STOP
           ENDIF
           V2P = V1 + ((NLINE(LEVEL)-1)*DELV)
           J1 = NLINE(LEVEL) - INT((V2P-V1USER)/DELV)
           J2 = INT(((V2USER-V1)/DELV) + 1.D0)
           NLINE(LEVEL) = J2 - J1 + 1

C COMMENT FROM ORIGINAL PROGRAM
C.. The g-space box is now considered to span NLINE(LEVEL) + 1/2 full line
C.. widths.  A full width is 2 / (2.*NLINE(LEVEL) + 1).
C.. The line positions are at 2*N / (2.*NLINE(LEVEL) + 1)
C.. The width boundaries are at (2*N - 1) / (2.*NLINE(LEVEL) + 1)
C.. where N is a given line number.  The right line boundary of the last
C.. line is at g = 1.  The is a space of one half width between g = 0.
C.. and the left boundary of the first line.  The mapping factor to map
C.. from spectral space to g-space is 2. / (NLINE(LEVEL) + 1)


C.. OMISSION OF DELTA-V COMPENSATED IN AER_RRTM BY NOT USING AVERAGE VALUES
C.. OF THE PLANCK INTEGRAL
           DO 4020 ILINE = J1, J2
                XKL(ILINE-J1+1) = XKL(ILINE)
                vv = v1 + (iline-1)*delv
                XCONT(ILINE-J1+1) = BBFN(VV,TAVE)
4020       CONTINUE
C     ***     Sort absorption coefficients

C     NLINE(LEVEL) is the the number of absorption coeff/optical depths to be
C     sorted. XKL is the array of absorption coeff. that is to be sorted in ascending
C     order. XCONT is sorted according to XKL.

           CALL SORT2(NLINE(LEVEL),XKL,XCONT,XTEMP,IINDEX)
           TOTPLANK = (PLANK(V1USER,TAVE)-PLANK(V2USER,TAVE))/
     &          (V2USER-V1USER)           
           print*,'tot',tave,PLANK(V1USER,TAVE),PLANK(V2USER,TAVE)
c The following if statement tests if the binary parameter method should
c     be implemented in the case for 2 major gases.
c              IF (IGAS2_U .EQ. 0) THEN
c                 COLUMFAC = WK(IGAS1_U)
c                 COLUMFAC = 1.E20/COLUMFAC
c              ELSE 
c                 COLUMFAC = WK(IGAS1_U) + 
c     &                (W_H_MLS(IGAS1_U,LEVEL)/W_H_MLS(IGAS2_U,LEVEL))
c     &                *WK(IGAS2_U)
c                 COLUMFAC = 1.E20/COLUMFAC
c              ENDIF
c           columfac = wk(igas_on)
c           columfac = 1.e20/columfac
           XLSPACE = FLOAT(NLINE(LEVEL)) + 0.5
           DO 3337 IG = 1, 16
              VCONTTOT(IG) = 0.
              J0MIN(IG) = G0(IG)*XLSPACE + 1
              J0MAX(IG) = G1(IG)*XLSPACE
              IF(IG.EQ.15)J0MAX(IG) = MIN0(J0MAX(IG),NLINE(LEVEL)-1)
              IF(IG.EQ.16)J0MIN(IG) = MIN0(J0MIN(IG),NLINE(LEVEL))
              NPTS0(IG) = J0MAX(IG) - J0MIN(IG) + 1
              DO 3338 IELI=J0MIN(IG),J0MAX(IG)
                 VCONTTOT(IG)=VCONTTOT(IG)+XCONT(IELI)
 3338         CONTINUE
              VCONTAVG(IG) = VCONTTOT(IG)/DFLOAT(NPTS0(IG))
              CONTAVG(IG) = VCONTAVG(IG)
c              ABSCOEF = CONTAVG(IG)*COLUMFAC
              ABSCOEF = CONTAVG(IG)*WT(IG)/TOTPLANK
              WRITE(IBIN,*) ABSCOEF              
 3337      CONTINUE

C     The k values are now in ascending order in the array XKL. The probability
C     of any one k occurring is 1/NLINE(LEVEL). Since the k's are evenly
C     spaced, the cumulative probablity for the first k (the smallest) is
C     1/NLINE, the cumulative probablity for the second k is 2/NLINE, the
C     cumulative probability for the last k (the largest) is NLINE/NLINE=1.
C     Thus the cumulative probability for a given (sorted) line is the line
C     number times the reciprocal of the total number of lines.

           CLOSE(INF)
           CLOSE(ICONT)
c 5000 CONTINUE

 5001 CONTINUE


C  ********** END OF MAIN CALCULATION LOOP OVER ATMOSPHERIC LAYERS **********  


      CLOSE(IBIN)
      T2=TIME()
      PRINT*,'EXITING THE SORTING ROUTINE PROGRAM',T2-T1

      STOP
      END

c****************SORTING ROUTINE; NUMERICAL RECIPES************
c Numerical Recipes provided the sort routines used here.
c Sorts array RA(1:n) into ascending order while making the corresponding 
c rearrangements of arrays RB(1:n). An index is constructed with the 
c IWKSP.
c
cInput into subroutine:
c     N - Number of elements to be sorted.
c     RA(1:N) - Primary array on which to perform sort. 
c     RB(1:N) - Secondary array that follows sort.
c     WKSP - Temporary array
c     IWKSP -Index for sorted array.
C******************************************************************  
      SUBROUTINE SORT2(N,RA,RB,WKSP,IWKSP)
      DIMENSION RA(N),RB(N),WKSP(N),IWKSP(N)

c      CALL INDEXHEAP(N,RA,IWKSP)
c      call indexquick(n,ra,iwksp)

c SORTING: If you want to preserve the index for sorting more than 
c two arrays, uncomment 'indexquick' or 'indexheap' (quick is faster). 
c Then uncomment the do loops that follow the sorting,
c comment the quicksort call. That is do 11, do 12, do 13, do 14.

c      CALL INDEXHEAP(N,RA,IWKSP)
c      call indexquick(n,ra,iwksp)

      call quicksort(n,ra,rb)

c      DO 11 J=1,N
c        WKSP(J)=RA(J)
c11    CONTINUE
c      DO 12 J=1,N
c        RA(J)=WKSP(IWKSP(J))
c12    CONTINUE
c      DO 13 J=1,N
c        WKSP(J)=RB(J)
c13    CONTINUE
c      DO 14 J=1,N
c        RB(J)=WKSP(IWKSP(J))
c14    CONTINUE
      RETURN
      END

C**********************QUICKSORT ROUTINE*****************

      SUBROUTINE QUICKSORT(N,ARR,BRR)
      PARAMETER (M=7,NSTACK=50,FM=7875.,FA=211.,FC=1663.
     *    ,FMI=1.2698413E-4)
      DIMENSION ARR(N),BRR(N),ISTACK(NSTACK)
      JSTACK=0
      L=1
      IR=N
      FX=0.
10    IF(IR-L.LT.M)THEN
        DO 13 J=L+1,IR
          A=ARR(J)
          B=BRR(J)
          DO 11 I=J-1,1,-1
            IF(ARR(I).LE.A)GO TO 12
            ARR(I+1)=ARR(I)
            BRR(I+1)=BRR(I)
11        CONTINUE
          I=0
12        ARR(I+1)=A
          BRR(I+1)=B
13      CONTINUE
        IF(JSTACK.EQ.0)RETURN
        IR=ISTACK(JSTACK)
        L=ISTACK(JSTACK-1)
        JSTACK=JSTACK-2
      ELSE
        I=L
        J=IR
        FX=MOD(FX*FA+FC,FM)
        IQ=L+(IR-L+1)*(FX*FMI)
        A=ARR(IQ)
        B=BRR(IQ)
        ARR(IQ)=ARR(L)
        BRR(IQ)=BRR(L)
20      CONTINUE
21        IF(J.GT.0)THEN
            IF(A.LT.ARR(J))THEN
              J=J-1
              GO TO 21
            ENDIF
          ENDIF
          IF(J.LE.I)THEN
            ARR(I)=A
            BRR(I)=B
            GO TO 30
          ENDIF
          ARR(I)=ARR(J)
          BRR(I)=BRR(J)
          I=I+1
22        IF(I.LE.N)THEN
            IF(A.GT.ARR(I))THEN
              I=I+1
              GO TO 22
            ENDIF
          ENDIF
          IF(J.LE.I)THEN
            ARR(J)=A
            BRR(J)=B
            I=J
            GO TO 30
          ENDIF
          ARR(J)=ARR(I)
          BRR(J)=BRR(I)
          J=J-1
        GO TO 20
30      JSTACK=JSTACK+2
        IF(JSTACK.GT.NSTACK)PAUSE 'NSTACK MUST BE MADE LARGER.'
        IF(IR-I.GE.I-L)THEN
          ISTACK(JSTACK)=IR
          ISTACK(JSTACK-1)=I+1
          IR=I-1
        ELSE
          ISTACK(JSTACK)=I-1
          ISTACK(JSTACK-1)=L
          L=I+1
        ENDIF
      ENDIF
      GO TO 10
      END

C**********************USEFUL INDEXING ROUTINE************
      SUBROUTINE INDEXQUICK(N,ARR,indx)
c This is the quicksort technique. Format from Numerical Recipes F77.      
      PARAMETER (M=7,NSTACK=50,FM=7875.,FA=211.,FC=1663.
     *    ,FMI=1.2698413E-4)
      DIMENSION ARR(N),ISTACK(NSTACK),indx(n)
      integer indxt,itemp
      JSTACK=0
      L=1
      IR=N
      FX=0.
      do 9 j=1,n
         indx(j)=j
 9        continue
10    IF(IR-L.LT.M)THEN
        DO 13 J=L+1,IR
           indxt=indx(j)
           a=arr(indxt)
c          A=ARR(J)
          DO 11 I=J-1,1,-1
             if(arr(indx(i)).le.a) go to 12
c            IF(ARR(I).LE.A)GO TO 12
             indx(i+1)=indx(i)
c            ARR(I+1)=ARR(I)
11        CONTINUE
          I=0
 12       indx(i+1)=indxt
c12        ARR(I+1)=A
13      CONTINUE
        IF(JSTACK.EQ.0)RETURN
        IR=ISTACK(JSTACK)
        L=ISTACK(JSTACK-1)
        JSTACK=JSTACK-2
      ELSE
        I=L
        J=IR
        FX=MOD(FX*FA+FC,FM)
        IQ=L+(IR-L+1)*(FX*FMI)
        itemp=indx(iq)
        a=arr(itemp)
c        A=ARR(IQ)
        indx(iq)=indx(l)
c        ARR(IQ)=ARR(L)
20      CONTINUE
21        IF(J.GT.0)THEN
c            IF(A.LT.ARR(J))THEN
          if(a.lt.arr(indx(j))) then
              J=J-1
              GO TO 21
            ENDIF
          ENDIF
          IF(J.LE.I)THEN
             indx(i)=itemp
c            ARR(I)=A
            GO TO 30
          ENDIF
          indx(i)=indx(j)
c          ARR(I)=ARR(J)
          I=I+1
22        IF(I.LE.N)THEN
c            IF(A.GT.ARR(I))THEN
          if(a.gt.arr(indx(i))) then
              I=I+1
              GO TO 22
            ENDIF
          ENDIF
          IF(J.LE.I)THEN
c            ARR(J)=A
             indx(j)=itemp
           I=J
            GO TO 30
          ENDIF
c          ARR(J)=ARR(I)
          indx(j)=indx(i)
          J=J-1
        GO TO 20
30      JSTACK=JSTACK+2
        IF(JSTACK.GT.NSTACK)PAUSE 'NSTACK must be made larger.'
        IF(IR-I.GE.I-L)THEN
          ISTACK(JSTACK)=IR
          ISTACK(JSTACK-1)=I+1
          IR=I-1
        ELSE
          ISTACK(JSTACK)=I-1
          ISTACK(JSTACK-1)=L
          L=I+1
        ENDIF
      ENDIF
      GO TO 10
      END

c**************USEFUL INDEXING ROUTINE****************
      SUBROUTINE INDEXHEAP(N,ARRIN,INDX)
c Heapsort Method.
      DIMENSION ARRIN(N),INDX(N)
      DO 11 J=1,N
        INDX(J)=J
11    CONTINUE
      IF(N.EQ.1)RETURN
      L=N/2+1
      IR=N
10    CONTINUE
        IF(L.GT.1)THEN
          L=L-1
          INDXT=INDX(L)
          Q=ARRIN(INDXT)
        ELSE
          INDXT=INDX(IR)
          Q=ARRIN(INDXT)
          INDX(IR)=INDX(1)
          IR=IR-1
          IF(IR.EQ.1)THEN
            INDX(1)=INDXT
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
20      IF(J.LE.IR)THEN
          IF(J.LT.IR)THEN
            IF(ARRIN(INDX(J)).LT.ARRIN(INDX(J+1)))J=J+1
          ENDIF
          IF(Q.LT.ARRIN(INDX(J)))THEN
            INDX(I)=INDX(J)
            I=J
            J=J+J 
         ELSE
            J=IR+1
          ENDIF
        GO TO 20
        ENDIF
        INDX(I)=INDXT
      GO TO 10
      END

C*******************************  FUNCTION PLANK  *****************************C

      FUNCTION PLANK(V,T)
      IMPLICIT DOUBLE PRECISION (V)
      PARAMETER( PLANCK = 6.626176E-27, BOLTZ = 1.380662E-16,              
     &           CLIGHT = 2.99792458E10 )                
      PARAMETER( RADCN1 = 2.*PLANCK*CLIGHT*CLIGHT*1.E-7 )
      PARAMETER( RADCN2 = PLANCK*CLIGHT/BOLTZ) 
      
      XKT = T / RADCN2
      X   = V / XKT
                                                                            
      IF (X .LT. 1.) THEN                                              
         PLANK = 6.4939400 -  X**3 *                               
     &        (1./3.- X/8. +  X**2/60. -  X**4/5040. +          
     &        X**6/272160. -  X**8/13305600)          
      ENDIF                                                             
                                                                             
      IF ((X .GE. 1.)  .AND.  (X .LE. 50.)) THEN                            
         PLANK = 0.                                                      

         DO 100 K = 1, 10                                            
            FK  = K                                                     
            FKX = FK * X
            
            PLANK = PLANK + (EXP(-FKX) / FK**4) *
     &           (((FKX+3.)*FKX+6.)*FKX+6.)
 100     CONTINUE                                                       
                                                                            
      ENDIF                                                             
                                                                            
      PLANK = PLANK * RADCN1 * XKT**4

      RETURN                                                            
      END                                                               
