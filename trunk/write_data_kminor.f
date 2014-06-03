       PROGRAM WRITE_DATA_KMINOR
      
      CHARACTER*2 FNUM(99)
      CHARACTER*50 KG
      character*20 kgfile1,kgfile2

C     KA is filled with absorption coefficients (x 1.e20) for the
C     lower atmosphere.  It should be dimensioned # of species levels X
C     5 X # of pressure levels X 16, where a 'species level' means a 
C     reference amount of a species (given as amt. species/amt.CO2) at
C     which absorption coefficient data is stored.  KB is the corre-
C     sponding array of absorption coefficients for above 
C     the lower atmosphere.
c      REAL KA_1(5,13,16)
c      REAL KB_1(5,13:59,16)
c      REAL KA_2(9,5,13,16)
c      REAL KB_2(9,5,13:59,16)
      REAL KA_MINOR_1(7,2,16),KA_MINOR_2(7,9,2,16)
      REAL KB_MINOR_1(7,2,16),KB_MINOR_2(7,5,2,16)

      integer igas_minor_l(7,1),igas_minor_u(7,1)

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

      NAMELIST /PAR/ WAVENUMber1,WAVENUMber2,IGAS1_L,IGAS2_L,IGAS1_U,
     &                   IGAS2_U ,igas_minor_l,igas_minor_u

      read (*,par)

      OPEN(20,FILE='kg_minor.f',FORM='FORMATTED')

      write(20,8000)
      write(20,8001)
      write(20,8002) 
      write(20,8003)

      do 500 ii=1,7
         if (igas_minor_l(ii,1) .ne. 0) then
            if (igas2_l .eq. 0) then 
                  KGfile1 = 'KG-minor'//fnum(ii)//'-T02-n09'
                  open(14,file=KGfile1,form='unformatted')
                  write(20,8010) ii
                  do 600 iig=1,16
                  read(14) ka_minor_1(ii,1,iig)
 600              continue
                  close(14)
                  write(20,8050) ka_minor_1(ii,1,1:6)
                  write(20,8050) ka_minor_1(ii,1,7:12)
                  write(20,8051) ka_minor_1(ii,1,13:16)
                  
                  KGfile2 = 'KG-minor'//fnum(ii)//'-T03-n09'
                  write(20,8015) ii
                  open(15,file=KGfile2,form='unformatted')
                  do 625 iig=1,16
                  read(15) ka_minor_1(ii,2,iig)
 625              continue
                  close(15)
                  write(20,8050) ka_minor_1(ii,2,1:6)
                  write(20,8050) ka_minor_1(ii,2,7:12)
                  write(20,8051) ka_minor_1(ii,2,13:16)
           else
              do 630 in = 1,9
                  KGfile1 = 'KG-minor'//fnum(ii)//'-T02-n'//fnum(in)
                  open(14,file=KGfile1,form='unformatted')
                  write(20,8030) ii,in
                  do 650 iig=1,16
                  read(14) ka_minor_2(ii,in,1,iig)
 650              continue
                  close(14)
                  write(20,8050) ka_minor_2(ii,in,1,1:6)
                  write(20,8050) ka_minor_2(ii,in,1,7:12)
                  write(20,8051) ka_minor_2(ii,in,1,13:16)
                  
                  KGfile2 = 'KG-minor'//fnum(ii)//'-T03-n'//fnum(in)
                  write(20,8035) ii,in
                  open(15,file=KGfile2,form='unformatted')
                  do 675 iig=1,16
                  read(15) ka_minor_2(ii,in,2,iig)
 675              continue
                  close(15)
                  write(20,8050) ka_minor_2(ii,in,2,1:6)
                  write(20,8050) ka_minor_2(ii,in,2,7:12)
                  write(20,8051) ka_minor_2(ii,in,2,13:16)
 630               continue
            endif
        endif
 500   continue

      do 800 ii=1,7
         if (igas_minor_u(ii,1) .ne. 0) then
            if (igas2_u .eq. 0) then
                  KGfile1 = 'KG-minor'//fnum(ii)//'-T07-n09'
                  open(14,file=KGfile1,form='unformatted')
                  write(20,8020) ii
                  do 700 iig=1,16
                  read(14) kb_minor_1(ii,1,iig)
 700              continue
                  close(14)
                  write(20,8050) kb_minor_1(ii,1,1:6)
                  write(20,8050) kb_minor_1(ii,1,7:12)
                  write(20,8051) kb_minor_1(ii,1,13:16)
                  
                  KGfile2 = 'KG-minor'//fnum(ii)//'-T08-n09'
                  write(20,8025) ii
                  open(15,file=KGfile2,form='unformatted')
                  do 725 iig=1,16
                  read(15) kb_minor_1(ii,2,iig)
 725           continue
                  close(15)
                  write(20,8050) kb_minor_1(ii,2,1:6)
                  write(20,8050) kb_minor_1(ii,2,7:12)
                  write(20,8051) kb_minor_1(ii,2,13:16)
            else
               in2 = 1
               do 730 in=1,5
                  KGfile1 = 'KG-minor'//fnum(ii)//'-T07-n'//fnum(in2)
                  open(14,file=KGfile1,form='unformatted')
                  write(20,8040) ii,in
                  do 750 iig=1,16
                  read(14) kb_minor_2(ii,in,1,iig)
 750              continue
                  close(14)
                  write(20,8050) kb_minor_2(ii,in,1,1:6)
                  write(20,8050) kb_minor_2(ii,in,1,7:12)
                  write(20,8051) kb_minor_2(ii,in,1,13:16)
                  
                  KGfile2 = 'KG-minor'//fnum(ii)//'-T08-n'//fnum(in2)
                  write(20,8045) ii,in
                  open(15,file=KGfile2,form='unformatted')
                  do 775 iig=1,16
                  read(15) kb_minor_2(ii,in,2,iig)
 775           continue
                  close(15)
                  write(20,8050) kb_minor_2(ii,in,2,1:6)
                  write(20,8050) kb_minor_2(ii,in,2,7:12)
                  write(20,8051) kb_minor_2(ii,in,2,13:16)
                  in2 = in2 + 2
 730       continue
           endif
        endif
 800   continue

 8000 FORMAT('       PARAMETER (MG=16,NMINOR=7)')
 8001 FORMAT('       REAL KA_MINOR_1(NMINOR,2,MG),'/
     &     '     &      KA_MINOR_2(NMINOR,9,2,MG)')
 8002 FORMAT('       REAL KB_MINOR_1(NMINOR,2,MG),'/
     &     '     &      KB_MINOR_2(NMINOR,5,2,MG)')
 8003 FORMAT('       COMMON /K_MINOR/ KA_MINOR_1,KA_MINOR_2,'/
     &     '     &                  KB_MINOR_1,KB_MINOR_2')

 8010  FORMAT('      DATA (KA_MINOR_1(',i2,',1,IG),IG=1,16) /')
 8015  FORMAT('      DATA (KA_MINOR_1(',i2,',2,IG),IG=1,16) /')
 8020  FORMAT('      DATA (KB_MINOR_1(',i2,',1,IG),IG=1,16) /')
 8025  FORMAT('      DATA (KB_MINOR_1(',i2,',2,IG),IG=1,16) /')

 8030  FORMAT('      DATA (KA_MINOR_2(',i2,',',i2,',1,IG),IG=1,16) /')
 8035  FORMAT('      DATA (KA_MINOR_2(',i2,',',i2,',2,IG),IG=1,16) /')
 8040  FORMAT('      DATA (KB_MINOR_2(',i2,',',i2,',1,IG),IG=1,16) /')
 8045  FORMAT('      DATA (KB_MINOR_2(',i2,',',i2,',2,IG),IG=1,16) /')

 

 8050  FORMAT('     &',1P,6(E10.4,','),0P)
 8051  FORMAT('     &',1P,3(E10.4,','),E10.4,0P,'/')

      END

