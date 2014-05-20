      PROGRAM COMP_WTAVG_PLANCK
C***************************************************

      DIMENSION FILTER(1000),plk_val(181)
      CHARACTER*1 DUM

      DATA HCSQRD /5.955309E-13/,  HCOK /1.438786/
C     *** Statement function to calculate the Planck function

      BBFN(V,TE) = 2.*HCSQRD*V**3/(EXP(V/(TE/HCOK)) -1.)

C  ************ START OF EXECUTABLE STATEMENTS **********
      
C     Open filter function file and read in values of filter function.
      OPEN(11,FILE='TAPE5.FILTER',FORM='FORMATTED')
      READ(11,*)DUM
      READ(11,*)DUM
      READ(11,9876)V1F,DVF,NP
      READ(11,*)DUM
      DO 233 IFIL = 1, NP
         READ(11,9877)FILTER(IFIL)
 233  CONTINUE
      close(11)

      do 5000 it = 160, 340
         tt = float(it)
         sumpl = 0.
         sumpr = 0.
         sumfil = 0.
         do 4000 ifil = 2, np-1
            vv = v1f + float(ifil-1) * dvf
            pl = bbfn(vv,tt)
            sumpl = sumpl + pl
            sumpr = sumpr + pl * filter(ifil)
            sumfil = sumfil + filter(ifil)
 4000    continue
         write(*,*)sumpl,sumpr,sumfil
         v2f = v1f + dvf * float(np-1)
c         avgpl = (0.5*(bbfn(v1f,tt)+bbfn(v2f,tt))+sumpl)/float(np-1)
         rat = sumpr/sumfil
c         write(*,*)tt,v1f,v2f,avgpl,rat,avgpl/rat
         plk_val(it-159) = rat
 5000 continue

      open(10,file='avplank_01.f')
      k = 1
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
 15         continue
            write (10,104) (plk_val(ii),ii=i1,i2)
            i1=i1+5
            i2=i2+5
         else if (j.eq.4) then
            do 16 i=1,6
               write (10,105) (plk_val(ii),ii=i1,i2)                  
               i1=i1+5
               i2=i2+5            
 16         continue
            write (10,106) plk_val(181)
         endif
 20   continue
      close(10)

 100  format ('      DATA (TOTPLNK(I,',i3,'),I=1,50)/')
 101  format ('      DATA (TOTPLNK(I,',i3,'),I=51,100)/')
 102  format ('      DATA (TOTPLNK(I,',i3,'),I=101,150)/')
 103  format ('      DATA (TOTPLNK(I,',i3,'),I=151,181)/')
 104  format ('     &',4(e11.5,','),e11.5,'/')
 105  format ('     &',5(e11.5,','))
 106  format ('     &',e11.5,'/')

 9876 FORMAT(F10.3,F10.4,I5)
 9877 FORMAT(E15.6)

      STOP
      END
