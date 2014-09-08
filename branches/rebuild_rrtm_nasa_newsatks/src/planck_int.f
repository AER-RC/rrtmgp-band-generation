      program comput_planck
      real wnumlo, wnumhi, plk_val(181)
      integer i,j,k,ii,i1,i2,nbands
      integer igas_minor_l(9,1),igas_minor_u(9,1)
      namelist /par/ wavenumber1,wavenumber2, igas1_l,igas2_l,
     &               igas1_u,igas2_u,igas_minor_l,igas_minor_u,
     &               nmol
      t=160.

      nbands=1

      read(*,par)

      open(10,file='avplank_01.f')

      do 10 i=1,181
      plk_val(i)= plkavg(wavenumber1,wavenumber2,t)/
     &                  ((wavenumber2-wavenumber1)*1.0E4)

      t=t+1.
 10   continue


      do 25 k=1,nbands
         i1=1
         i2=5
         do 20 j=1,4
            if (j.eq.1) write(10,100) k
            if (j.eq.2) write(10,101) k
            if (j.eq.3) write(10,102) k
            if (j.eq.4) write(10,103) k
            
            if (j.lt.4) then
               do 15 i=1,9
               write (10,105) (plk_val(ii),ii=i1,i2)
               i1=i1+5
               i2=i2+5
 15            continue
               write (10,104) (plk_val(ii),ii=i1,i2)
               i1=i1+5
               i2=i2+5
            else if (j.eq.4) then
               do 16 i=1,6
               write (10,105) (plk_val(ii),ii=i1,i2)                  
               i1=i1+5
               i2=i2+5            
 16       continue
               write (10,106) plk_val(181)
            endif
 20       continue
 25       continue
      

      close(10)

c 92   format(6x,'PARAMETER (NBANDSLG = 16)')
c 94   format(6x,'COMMON /AVGPLNK/  TOTPLNK(181,NBANDSLG),TOTPLK16(181)',
c     &     //)

 100  format ('      DATA (TOTPLNK(I,',i3,'),I=1,50)/')
 101  format ('      DATA (TOTPLNK(I,',i3,'),I=51,100)/')
 102  format ('      DATA (TOTPLNK(I,',i3,'),I=101,150)/')
 103  format ('      DATA (TOTPLNK(I,',i3,'),I=151,181)/')
 104  format ('     &',4(es11.5,','),es11.5,'/')
 105  format ('     &',5(es11.5,','))
 106  format ('     &',es11.5,'/')

      end

	REAL FUNCTION  PLKAVG( WNUMLO, WNUMHI, T )

C        COMPUTES PLANCK FUNCTION INTEGRATED BETWEEN TWO WAVENUMBERS

C  NOTE ** CHANGE 'R1MACH' TO 'D1MACH' TO RUN IN DOUBLE PRECISION

C  I N P U T :  WNUMLO : LOWER WAVENUMBER ( INV CM ) OF SPECTRAL
C                           INTERVAL
C               WNUMHI : UPPER WAVENUMBER
C               T      : TEMPERATURE (K)

C  O U T P U T :  PLKAVG : INTEGRATED PLANCK FUNCTION ( WATTS/SQ M )
C                           = INTEGRAL (WNUMLO TO WNUMHI) OF
C                              2H C**2  NU**3 / ( EXP(HC NU/KT) - 1)
C                              (WHERE H=PLANCKS CONSTANT, C=SPEED OF
C                              LIGHT, NU=WAVENUMBER, T=TEMPERATURE,
C                              AND K = BOLTZMANN CONSTANT)

C  REFERENCE : SPECIFICATIONS OF THE PHYSICAL WORLD: NEW VALUE
C                 OF THE FUNDAMENTAL CONSTANTS, DIMENSIONS/N.B.S.,
C                 JAN. 1974

C  METHOD :  FOR  -WNUMLO-  CLOSE TO  -WNUMHI-, A SIMPSON-RULE
C            QUADRATURE IS DONE TO AVOID ILL-CONDITIONING; OTHERWISE

C            (1)  FOR 'WNUMLO' OR 'WNUMHI' SMALL,
C                 INTEGRAL(0 TO WNUMLO/HI) IS CALCULATED BY EXPANDING
C                 THE INTEGRAND IN A POWER SERIES AND INTEGRATING
C                 TERM BY TERM;

C            (2)  OTHERWISE, INTEGRAL(WNUMLO/HI TO INFINITY) IS
C                 CALCULATED BY EXPANDING THE DENOMINATOR OF THE
C                 INTEGRAND IN POWERS OF THE EXPONENTIAL AND
C                 INTEGRATING TERM BY TERM.

C  ACCURACY :  AT LEAST 6 SIGNIFICANT DIGITS, ASSUMING THE
C              PHYSICAL CONSTANTS ARE INFINITELY ACCURATE

C  ERRORS WHICH ARE NOT TRAPPED:

C      * POWER OR EXPONENTIAL SERIES MAY UNDERFLOW, GIVING NO
C        SIGNIFICANT DIGITS.  THIS MAY OR MAY NOT BE OF CONCERN,
C        DEPENDING ON THE APPLICATION.

C      * SIMPSON-RULE SPECIAL CASE IS SKIPPED WHEN DENOMINATOR OF
C        INTEGRAND WILL CAUSE OVERFLOW.  IN THAT CASE THE NORMAL
C        PROCEDURE IS USED, WHICH MAY BE INACCURATE IF THE
C        WAVENUMBER LIMITS (WNUMLO, WNUMHI) ARE CLOSE TOGETHER.
C ----------------------------------------------------------------------
C                                   *** ARGUMENTS
	REAL     T, WNUMLO, WNUMHI
C                                   *** LOCAL VARIABLES

C        A1,2,... :  POWER SERIES COEFFICIENTS
C        C2       :  H * C / K, IN UNITS CM*K (H = PLANCKS CONSTANT,
C                      C = SPEED OF LIGHT, K = BOLTZMANN CONSTANT)
C        D(I)     :  EXPONENTIAL SERIES EXPANSION OF INTEGRAL OF
C                       PLANCK FUNCTION FROM WNUMLO (I=1) OR WNUMHI
C                       (I=2) TO INFINITY
C        EPSIL    :  SMALLEST NUMBER SUCH THAT 1+EPSIL .GT. 1 ON
C                       COMPUTER
C        EX       :  EXP( - V(I) )
C        EXM      :  EX**M
C        MMAX     :  NO. OF TERMS TO TAKE IN EXPONENTIAL SERIES
C        MV       :  MULTIPLES OF 'V(I)'
C        P(I)     :  POWER SERIES EXPANSION OF INTEGRAL OF
C                       PLANCK FUNCTION FROM ZERO TO WNUMLO (I=1) OR
C                       WNUMHI (I=2)
C        PI       :  3.14159...
C        SIGMA    :  STEFAN-BOLTZMANN CONSTANT (W/M**2/K**4)
C        SIGDPI   :  SIGMA / PI
C        SMALLV   :  NUMBER OF TIMES THE POWER SERIES IS USED (0,1,2)
C        V(I)     :  C2 * (WNUMLO(I=1) OR WNUMHI(I=2)) / TEMPERATURE
C        VCUT     :  POWER-SERIES CUTOFF POINT
C        VCP      :  EXPONENTIAL SERIES CUTOFF POINTS
C        VMAX     :  LARGEST ALLOWABLE ARGUMENT OF 'EXP' FUNCTION

	PARAMETER  ( A1 = 1./3., A2 = -1./8., A3 = 1./60., A4 = -1./5040.,
     $             A5 = 1./272160., A6 = -1./13305600. )
	INTEGER  SMALLV
	REAL     C2, CONC, D(2), EPSIL, EX, MV, P(2), SIGMA, SIGDPI,
     $         V(2), VCUT, VCP(7), VSQ,vtest
c	DOUBLE PRECISION   D1MACH
	SAVE     PI, CONC, VMAX, EPSIL, SIGDPI
	DATA     C2 / 1.438786 /,  SIGMA / 5.67032E-8 /,
     $         VCUT / 1.5 /, VCP / 10.25, 5.7, 3.9, 2.9, 2.3, 1.9, 0.0 /
	DATA     PI / 0.0 /
	F(X) = X**3 / ( EXP(X) - 1 )


ccalculate plkavg for the mid point of the band
        
	IF ( PI.EQ.0.0 )  THEN
	   PI = 2. * ASIN( 1.0 )
	   VMAX = ALOG(R1MACH(2))
	   EPSIL = R1MACH(4)
	   SIGDPI = SIGMA / PI
	   CONC = 15. / PI**4
	END IF

	IF( T.LT.0.0 .OR. WNUMHI.LE.WNUMLO .OR. WNUMLO.LT.0. )
     $    print*, 'PLKAVG--TEMPERATURE OR WAVENUMS. WRONG'

	IF ( T.LT.1.E-4 )  THEN
	   PLKAVG = 0.0
	   RETURN
	ENDIF

	V(1) = C2 * WNUMLO / T
	V(2) = C2 * WNUMHI / T
        
	IF ( V(1).GT.EPSIL .AND. V(2).LT.VMAX .AND.
     $     (WNUMHI-WNUMLO)/WNUMHI .LT. 1.E-2 )  THEN 

C                          ** WAVENUMBERS ARE VERY CLOSE.  GET INTEGRAL
C                          ** BY ITERATING SIMPSON RULE TO CONVERGENCE.
	   HH = V(2) - V(1)
	   OLDVAL = 0.0
	   VAL0 = F( V(1) ) + F( V(2) )

	   DO  2  N = 1, 10
	      DEL = HH / (2*N)
	      VAL = VAL0
	      DO  1  K = 1, 2*N-1
	         VAL = VAL + 2*(1+MOD(K,2)) * F( V(1) + K*DEL )
    1       CONTINUE
	      VAL = DEL/3. * VAL
	      IF ( ABS( (VAL-OLDVAL)/VAL ) .LE. 1.E-6 )  GO TO 3
	      OLDVAL = VAL
    2    CONTINUE
	   print*,'PLKAVG--SIMPSON RULE DIDNT CONVERGE'

    3    PLKAVG = SIGDPI * T**4 * CONC * VAL
	   RETURN
	END IF

	SMALLV = 0
	DO  50  I = 1, 2

	   IF( V(I).LT.VCUT )  THEN
C                                   ** USE POWER SERIES
	      SMALLV = SMALLV + 1
	      VSQ = V(I)**2
	      P(I) =  CONC * VSQ * V(I) * ( A1 + V(I) * ( A2 + V(I) *
     $                ( A3 + VSQ * ( A4 + VSQ * ( A5 + VSQ*A6 ) ) ) ) )
	   ELSE
C                    ** USE EXPONENTIAL SERIES
	      MMAX = 0
C                                ** FIND UPPER LIMIT OF SERIES
   20       MMAX = MMAX + 1
	         IF ( V(I).LT.VCP( MMAX ) )  GO TO 20

	      EX = EXP( - V(I) )
	      EXM = 1.0
	      D(I) = 0.0

	      DO  30  M = 1, MMAX
	         MV = M * V(I)
	         EXM = EX * EXM
	         D(I) = D(I) +
     $                EXM * ( 6. + MV*( 6. + MV*( 3. + MV ) ) ) / M**4
   30       CONTINUE

	      D(I) = CONC * D(I)
	   END IF

   50 CONTINUE

	IF ( SMALLV .EQ. 2 ) THEN
C                                    ** WNUMLO AND WNUMHI BOTH SMALL
	   PLKAVG = P(2) - P(1)

	ELSE IF ( SMALLV .EQ. 1 ) THEN
C                                    ** WNUMLO SMALL, WNUMHI LARGE
	   PLKAVG = 1. - P(1) - D(2)

	ELSE
C                                    ** WNUMLO AND WNUMHI BOTH LARGE
	   PLKAVG = D(1) - D(2)

	END IF

	PLKAVG = SIGDPI * T**4 * PLKAVG
	IF( PLKAVG.EQ.0.0 )
     $    print*,'PLKAVG--RETURNS ZERO; POSSIBLE UNDERFLOW'

	RETURN
	END
