      PROGRAM WRITE_DATA_KMINOR
      
      PARAMETER (MXMOL=9)
      CHARACTER*2 FNUM(99)
      CHARACTER*50 KG,contfile

C     KA is filled with absorption coefficients (x 1.e20) for the
C     lower atmosphere.  It should be dimensioned # of species levels X
C     5 X # of pressure levels X 16, where a 'species level' means a 
C     reference amount of a species (given as amt. species/amt.CO2) at
C     which absorption coefficient data is stored.  KB is the corre-
C     sponding array of absorption coefficients for above 
C     the lower atmosphere.
      parameter (mg = 16)
c      REAL KA_1(5,13,16)
c      REAL KB_1(5,13:59,16)
c      REAL KA_2(9,5,13,16)
c      REAL KB_2(9,5,13:59,16)
      REAL FRACREF(MG)

      integer igas_minor_l(mxmol,1),igas_minor_u(mxmol,1)

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
     &                   IGAS2_U ,igas_minor_l,igas_minor_u,nmol

      read (*,par)

      OPEN(20,FILE='kg_planck.f',FORM='FORMATTED')

      write(20,8000)

      if (igas2_l .eq. 0) then
                  contfile='KG_bbp1_n09'
		  write(20,9001)
		  write(20,9002) 
		  write(20,8004)
                  open(14,file=contfile,form='formatted')
                  ii = 9
                  write(20,9010) 
                  do 600 iig=1,16
                  read(14,*) fracref(iig)
 600              continue
                  close(14)
                  write(20,8050) fracref(1:6)
                  write(20,8050) fracref(7:12)
                  write(20,8051) fracref(13:16)

      else
	 write(20,8001)
	 write(20,8002) 
	 write(20,8004)
         do 625 ii=1,9 
            contfile = 'KG_bbp1_n'//fnum(ii)
            open(14,file=contfile,form='formatted')
            write(20,8010) ii
            do 620 iig = 1,16
               read(14,*) fracref(iig)
 620           continue
               close(14)
                  write(20,8050) fracref(1:6)
                  write(20,8050) fracref(7:12)
                  write(20,8051) fracref(13:16)
 625        continue
      endif

      if (igas2_u .eq. 0) then
         contfile='KG_bbp2_n09'
                  open(14,file=contfile,form='formatted')
                  ii = 5
                  write(20,9015) 
                  do 650 iig=1,16
                  read(14,*) fracref(iig)
 650              continue
                  close(14)
                  write(20,8050) fracref(1:6)
                  write(20,8050) fracref(7:12)
                  write(20,8051) fracref(13:16)
      else
         ij = 1
         do 675 ii = 1,9,2
                  contfile='KG_bbp2_n'//fnum(ii)
                  open(14,file=contfile,form='formatted')
                  write(20,8015) ij
                  do 670 iig=1,16
                  read(14,*) fracref(iig)
 670              continue
                  close(14)
                  write(20,8050) fracref(1:6)
                  write(20,8050) fracref(7:12)
                  write(20,8051) fracref(13:16)
                  ij = ij+1
 675        continue
      endif


                  close(20)

 8000 FORMAT('       PARAMETER (MG=16,NMINOR=7)')
 8001 FORMAT('       REAL FRACREFA(MG,9)')
 8002 FORMAT('       REAL FRACREFB(MG,5)')
 9001 FORMAT('       REAL FRACREFA(MG)')
 9002 FORMAT('       REAL FRACREFB(MG)')

 8004 FORMAT('       COMMON /K_PLANCK/ FRACREFA, FRACREFB')

 8010  FORMAT('      DATA (FRACREFA(IG,',i2,'),IG=1,16) /')
 8015  FORMAT('      DATA (FRACREFB(IG,',i2,'),IG=1,16) /')
 9010  FORMAT('      DATA (FRACREFA(IG),IG=1,16) /')
 9015  FORMAT('      DATA (FRACREFB(IG),IG=1,16) /')

 8050  FORMAT('     &',1P,6(E10.4,','),0P)
 8051  FORMAT('     &',1P,3(E10.4,','),E10.4,0P,'/')

      END




