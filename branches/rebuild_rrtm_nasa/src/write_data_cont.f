      PROGRAM WRITE_DATA_CONT
      
      CHARACTER*2 FNUM(99)
      CHARACTER*50 KG,contfile
      character*20 kgfile1,kgfile2

C     KA is filled with absorption coefficients (x 1.e20) for the
C     lower atmosphere.  It should be dimensioned # of species levels X
C     5 X # of pressure levels X 16, where a 'species level' means a 
C     reference amount of a species (given as amt. species/amt.CO2) at
C     which absorption coefficient data is stored.  KB is the corre-
C     sponding array of absorption coefficients for above 
C     the lower atmosphere.
      parameter (mg = 16)
      parameter (ninc=10)

c      REAL KA_1(5,13,16)
c      REAL KB_1(5,13:59,16)
c      REAL KA_2(9,5,13,16)
c      REAL KB_2(9,5,13:59,16)
      REAL FORREF(4,mg)
      REAL S296(mg),s260(mg)
      REAL SELF_INTERP(mg,ninc)

      DATA TSTART/245.6/
      DATA TINC/7.2/
      DATA FNUM/'01','02','03','04','05','06','07','08','09',
     &     '10','11','12','13','14','15','16','17','18','19',
     &     '20','21','22','23','24','25','26','27','28','29',
     &     '30','31','32','33','34','35','36','37','38','39',
     &     '40','41','42','43','44','45','46','47','48','49',
     &     '50','51','52','53','54','55','56','57','58','59',
     &     '60','61','62','63','64','65','66','67','68','69',
     &     '70','71','72','73','74','75','76','77','78','79',
     &     '80','81','82','83','84','85','86','87','88','89',
     &     '90','91','92','93','94','95','96','97','98','99'/


      OPEN(20,FILE='DATA.CONT',FORM='FORMATTED')

c      write(20,8000)
c      write(20,8001)
c      write(20,8002) 
c      write(20,8003)
c      write(20,8004)
      do 800 ii=1,4
         if (ii .eq. 1) contfile='KG_bbf1'
         if (ii .eq. 2) contfile='KG_bbf2'
         if (ii .eq. 3) contfile='KG_bbf3'
         if (ii .eq. 4) contfile='KG_bbf4'
                  open(14,file=contfile,form='formatted')
                  write(20,8010) ii
                  do 600 iig=1,16
                  read(14,*) forref(ii,iig)
                  forref(ii,iig) = 1.e20*forref(ii,iig)
 600              continue
                  close(14)
                  write(20,8050) forref(ii,1:6)
                  write(20,8050) forref(ii,7:12)
                  write(20,8051) forref(ii,13:16)
 800              continue

       contfile = 'KG_bbs1'
       open(14,file=contfile,form='formatted')
c                  write(20,8030)
                  do 1600 iig=1,16
                  read(14,*) s296(iig)
                  s296(iig) = 1.e20*s296(iig)
 1600              continue
                   close(14)
c                  write(20,8050) s296(1:6)
c                  write(20,8050) s296(7:12)
c                  write(20,8051) s296(13:16)                   

       contfile = 'KG_bbs2'
       open(14,file=contfile,form='formatted')
c                  write(20,8035) 
                  do 1800 iig=1,16
                  read(14,*) s260(iig)
                  s260(iig)=1.e20*s260(iig)
 1800              continue
                   close(14)
c                  write(20,8050) s260(1:6)
c                  write(20,8050) s260(7:12)
c                  write(20,8051) s260(13:16)                   

c Add interpolation
                  do j=1,mg
                    do k=1,ninc
                      tnew=tstart+tinc*dble(k-1)
                      tfac=(tnew-296.)/(260.-296.)
                 self_interp(j,k)=s296(j)*(s260(j)/s296(j))**(tfac)
                    end do
                  end do
	do i=1,mg
	 write(20,8060)
     &     '      DATA (SELFREF(JT,',i,'),JT=1,10)  /'
	 write(20,8061) 
     &      '&',self_interp(i,1),',',   
     &       self_interp(i,2),',',self_interp(i,3),',',
     &      self_interp(i,4),',',
     &	    self_interp(i,5),','
         write(20,8062)
     &      '&',self_interp(i,6),',', 
     &	    self_interp(i,7),',',self_interp(i,8),',',
     &      self_interp(i,9),',',self_interp(i,10),'/'
	end do
	    close(20)

 8000 FORMAT('       PARAMETER (MG=16,NMINOR=7)')
 8001 FORMAT('       REAL FORREF(4,MG)')
c 8002 FORMAT('       REAL S296(MG)')
c 8003 FORMAT('       REAL S260(MG)')
 8002 FORMAT('       REAL SELFREF(10,MG)')
 8004 FORMAT('       COMMON /K_CONT/ FORREF, SELFREF')

 8010  FORMAT('      DATA (FORREF(',i1,',IG),IG=1,16) /')
 8015  FORMAT('      DATA (FORREF(',i1,',IG),IG=1,16) /')
 8020  FORMAT('      DATA (FORREF(',i1,',IG),IG=1,16) /')
 8025  FORMAT('      DATA (FORREF(',i1,',IG),IG=1,16) /')

 8030  FORMAT('      DATA (S296(IG),IG=1,16) /')
 8035  FORMAT('      DATA (S260(IG),IG=1,16) /')

 8050  FORMAT('     &',1P,6(E10.4,','),0P)
 8051  FORMAT('     &',1P,3(E10.4,','),E10.4,0P,'/')

 8060 FORMAT(a23,i2,a13)
 8061 FORMAT(5x,a1,5(1x,1P,e11.5,a1))
 8062 FORMAT(5x,a1,5(1x,1P,e11.5,a1))
      END

