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
      PARAMETER (NLINES=15000000,MLAYERS=47,MG=16)
      PARAMETER (MXMOL=9)

      IMPLICIT DOUBLE PRECISION (V)
      DOUBLE PRECISION XID(10),SECANT,HMOL(60),XALTZ(4),YID(10)
      EXTERNAL TIME

      INTEGER TIME,T1,T2
      REAL PAVE,TAVE,WK(64),WBROAD,DELV,TBOUND,EMISIV,
     & FSCDID(17),YI1
      REAL DVP,Y(2400),Z(2400)

      CHARACTER*50 FILE1,FILE2,FILE4,FILE5

      DIMENSION NPANEL(MLAYERS)
      DIMENSION P(MLAYERS),T(MLAYERS)
      DIMENSION NLINE(MLAYERS)
      DIMENSION G0(MG),G1(MG),WT(MG)
      DIMENSION XKL(NLINES),XCONT(NLINES)
      DIMENSION NPTS0(16),VCONTTOT(16)
      DIMENSION VCONTAVG(16),CONTAVG(16)
      DIMENSION J0MIN(16),J0MAX(16)
      dimension igas_minor_l(mxmol,1),igas_minor_u(mxmol,1)
      DIMENSION W_L_MLS(mxmol,13)
      DIMENSION W_H_MLS(mxmol,47)
      
      NAMELIST /PAR/ WAVENUMBER1,WAVENUMBER2,IGAS1_L,IGAS2_L,
     &               IGAS1_U,IGAS2_U,igas_minor_l,igas_minor_u,nmol
      namelist /igaspar/ igas_on,iatmtype

      data igas_on/0/
      DATA NLAYERS/47/
      DATA INFINITY/50000/
      DATA NLINE/MLAYERS*0/
C This code should be toggled depending on whether new or old
C LBL (old -> ODint_01; new -> ODint_001)
C      CHARACTER*3 FNUM(MLAYERS)
C      DATA FNUM/'001','002','003','004','005','006','007',
C     &    '008','009','010','011','012','013','014','015',
C     &    '016','017','018','019','020','021','022','023',
C     &    '024','025','026','027','028','029','030','031',
C     &    '032','033','034','035','036','037','038','039',
C     &    '040','041','042','043','044','045','046','047'/
      CHARACTER*2 FNUM(MLAYERS)
      DATA FNUM/'01','02','03','04','05','06','07',
     &    '08','09','10','11','12','13','14','15',
     &    '16','17','18','19','20','21','22','23',
     &    '24','25','26','27','28','29','30','31',
     &    '32','33','34','35','36','37','38','39',
     &    '40','41','42','43','44','45','46','47'/
      DATA WT/
     &     0.1527534276,0.1491729617,0.1420961469,0.1316886544,
     &     0.1181945205,0.1019300893,0.0832767040,0.0626720116,
     &     0.0424925,0.0046269894,0.0038279891,0.0030260086,
     &     0.0022199750,0.0014140010,0.000533,0.000075/


       include 'std_atmos_kdis.f'
C  ************ START OF EXECUTABLE STATEMENTS **********

      READ (*,PAR)
      read(*,igaspar)
      V1USER = WAVENUMBER1
      V2USER = WAVENUMBER2

      ICONT = 17
      INF = 8
      IBIN =  7
      FILE4  = 'CK_ABS_MINOR.DAT'
      OPEN(IBIN,FILE=FILE4,  FORM='UNFORMATTED')

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
      if (iatmtype .eq. 1) then
         level = igas_minor_l(igas_on,1)
      else if (iatmtype .eq. 2) then
         level = igas_minor_u(igas_on,1)
      endif
C         Otherwise, read from the ODKD files (from LBLRTM).
c          The NC- files represent the OD output from LBLRTM where you only 
c          have one key species and all other species are set to zero.
         FILE1 = 'ODint_'//FNUM(LEVEL)
         FILE2 = 'NCint_'//FNUM(LEVEL)

         OPEN(INF,FILE=FILE1,FORM='UNFORMATTED')
         OPEN(ICONT,FILE=FILE2,FORM='UNFORMATTED')

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
           READ(ICONT,ERR=5001,END=5001) XID,SECANT,PAVE,TAVE,HMOL,
     &        XALTZ,WK,WBROAD,DELV,V1,V2,TBOUND,EMISIV,FSCDID,
     &        NMOL,LAYER,YI1,YID(10),LSTWDF
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
                READ(ICONT) V1P,V2P,DVP,NLIM
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
                READ(ICONT) (Z(J), J=1,NLIM)

C     *** Move optical depths, Y, into XKL These optical depths are for 
C the entire atmosphere  including water vapor, the continuum, all other pertinent 
C gases etc. The including water vapor, the continuum, all other pertinent gases etc. 
C  The XCONT array is the primary absorber for the band. You are sorting 
C XCONT according to XKL. This is to keep consistency when you have multiple 
C key species.

                DO 2005 ILINE = 1, NLIM
                     IF(Y(ILINE) .LE. 0.0) Y(ILINE) = 0.0
                     XKL(ILINE+NLINE(LEVEL)) = Y(ILINE)
                     IF(Z(ILINE) .LE. 0.0) Z(ILINE) = 0.0
	             XCONT(ILINE+NLINE(LEVEL)) = Z(ILINE)
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
                XCONT(ILINE-J1+1) = XCONT(ILINE)
4020       CONTINUE
C     ***     Sort absorption coefficients

C     NLINE(LEVEL) is the the number of absorption coeff/optical depths to be
C     sorted. XKL is the array of absorption coeff. that is to be sorted in ascending
C     order. XCONT is sorted according to XKL.

           CALL SORT2(NLINE(LEVEL),XKL,XCONT)
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
           columfac = wk(igas_on)
           columfac = 1.e20/columfac
           XLSPACE = DBLE(NLINE(LEVEL)) + 0.5
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
              VCONTAVG(IG) = VCONTTOT(IG)/DBLE(NPTS0(IG))
              CONTAVG(IG) = VCONTAVG(IG)
              ABSCOEF = CONTAVG(IG)*COLUMFAC
              print *,ig,abscoef
              WRITE(IBIN) ABSCOEF              
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

C F77/Numerical Recipes
C Chapter 8
C Sorts an array arr(1:n) into ascending order using Quicksort, while making the
C corresponding rearrangement of the array brr(1:n).
C Note: http://www.fing.edu.uy/if/cursos/fiscomp/extras/numrec/book/bookfpdf.html.
C Thanks to special permission from Cambridge University Press, we are able to 
C bring you the complete Numerical Recipes in Fortran 77 book On-Line! 
C f77_sources.tar.gz
      SUBROUTINE sort2(n,arr,brr)
      INTEGER n,M,NSTACK
      REAL arr(n),brr(n)
      PARAMETER (M=7,NSTACK=50)
      INTEGER i,ir,j,jstack,k,l,istack(NSTACK)
      REAL a,b,temp
      jstack=0
      l=1
      ir=n
1     if(ir-l.lt.M)then
        do 12 j=l+1,ir
          a=arr(j)
          b=brr(j)
          do 11 i=j-1,l,-1
            if(arr(i).le.a)goto 2
            arr(i+1)=arr(i)
            brr(i+1)=brr(i)
11        continue
          i=l-1
2         arr(i+1)=a
          brr(i+1)=b
12      continue
        if(jstack.eq.0)return
        ir=istack(jstack)
        l=istack(jstack-1)
        jstack=jstack-2
      else
        k=(l+ir)/2
        temp=arr(k)
        arr(k)=arr(l+1)
        arr(l+1)=temp
        temp=brr(k)
        brr(k)=brr(l+1)
        brr(l+1)=temp
        if(arr(l).gt.arr(ir))then
          temp=arr(l)
          arr(l)=arr(ir)
          arr(ir)=temp
          temp=brr(l)
          brr(l)=brr(ir)
          brr(ir)=temp
        endif
        if(arr(l+1).gt.arr(ir))then
          temp=arr(l+1)
          arr(l+1)=arr(ir)
          arr(ir)=temp
          temp=brr(l+1)
          brr(l+1)=brr(ir)
          brr(ir)=temp
        endif
        if(arr(l).gt.arr(l+1))then
          temp=arr(l)
          arr(l)=arr(l+1)
          arr(l+1)=temp
          temp=brr(l)
          brr(l)=brr(l+1)
          brr(l+1)=temp
        endif
        i=l+1
        j=ir
        a=arr(l+1)
        b=brr(l+1)
3       continue
          i=i+1
        if(arr(i).lt.a)goto 3
4       continue
          j=j-1
        if(arr(j).gt.a)goto 4
        if(j.lt.i)goto 5
        temp=arr(i)
        arr(i)=arr(j)
        arr(j)=temp
        temp=brr(i)
        brr(i)=brr(j)
        brr(j)=temp
        goto 3
5       arr(l+1)=arr(j)
        arr(j)=a
        brr(l+1)=brr(j)
        brr(j)=b
        jstack=jstack+2
        if(jstack.gt.NSTACK)pause 'NSTACK too small in sort2'
        if(ir-i+1.ge.j-l)then
          istack(jstack)=ir
          istack(jstack-1)=i
          ir=j-1
        else
          istack(jstack)=j-1
          istack(jstack-1)=l
          l=i
        endif
      endif
      goto 1
      END
