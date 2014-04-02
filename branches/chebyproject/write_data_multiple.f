      PROGRAM WRITE_DATA_KG
      
      CHARACTER*2 FNUM(99)
      CHARACTER*50 KG

C     KA is filled with absorption coefficients (x 1.e20) for the
C     lower atmosphere.  It should be dimensioned # of species levels X
C     5 X # of pressure levels X 16, where a 'species level' means a 
C     reference amount of a species (given as amt. species/amt.CO2) at
C     which absorption coefficient data is stored.  KB is the corre-
C     sponding array of absorption coefficients for above 
C     the lower atmosphere.
      REAL KA_1(5,13,16)
      REAL KB_1(5,13:59,16)
      REAL KA_2(9,5,13,16)
      REAL KB_2(5,5,13:59,16)
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

      OPEN(20,FILE='DATA.KG',FORM='FORMATTED')

      write(20,8000)
      write(20,8001)
      write(20,8002) 
      write(20,8003)
      write(20,8004) 
      write(20,8005)
      write(20,8006)
      write(20,8007)
      write(20,8009)
      write(20,8008)

c      write(20,8009)

      if (igas1_l .eq.0 .and. igas2_l .eq.0) goto 1800

      if (igas2_l .eq. 0) then 
         index2=1
      DO 1400 JT = 1, 5
      INDEX = 1
            KG = 'KG-T'//FNUM(INDEX2)//'-n09'
            OPEN(10,FILE=KG,FORM='UNFORMATTED')

            DO 1200 JP = 1, 13
               DO 1100 IG = 1, 16
                  READ(10) KA_1(JT,JP,IG)
 1100          CONTINUE
 1200       CONTINUE
            CLOSE(10)
         index2=index2+1
 1400 CONTINUE

cSet up the format for the file to be fed into RRTM. 


      DO 1700 IG = 1, 16
         DO 1600 JP = 1, 13
            WRITE(20,8010)JP,IG
            WRITE(20,8011)(KA_1(I,JP,IG),I=1,5)
 1600    CONTINUE
 1700 CONTINUE

      endif

      if (igas2_l .ne. 0) then 
      INDEX2 = 1
      DO 1450 JT = 1, 5
      INDEX = 1
         DO 1350 JS = 1, 9
            KG = 'KG-T'//FNUM(INDEX2)//'-n'//FNUM(INDEX)
            OPEN(10,FILE=KG,FORM='UNFORMATTED')

            DO 1250 JP = 1, 13
               DO 1150 IG = 1, 16
                  READ(10) KA_2(JS,JT,JP,IG)
 1150          CONTINUE
 1250       CONTINUE
            CLOSE(10)
            INDEX = INDEX + 1
 1350    CONTINUE
         index2=index2+1
 1450 CONTINUE
 
      DO 1750 IG = 1, 16
         DO 1650 JP = 1, 13
            WRITE(20,9010)JP,IG
            WRITE(20,9011)(KA_2(I,1,JP,IG),I=1,6)
           WRITE(20,9011)(KA_2(I,1,JP,IG),I=7,9),(KA_2(I,2,JP,IG),I=1,3)
            WRITE(20,9011)(KA_2(I,2,JP,IG),I=4,9)
            WRITE(20,9011)(KA_2(I,3,JP,IG),I=1,6)
           WRITE(20,9011)(KA_2(I,3,JP,IG),I=7,9),(KA_2(I,4,JP,IG),I=1,3)
            WRITE(20,9011)(KA_2(I,4,JP,IG),I=4,9)
            WRITE(20,9011)(KA_2(I,5,JP,IG),I=1,6)
            WRITE(20,9012)(KA_2(I,5,JP,IG),I=7,9)
 1650    CONTINUE
 1750 CONTINUE

      endif


 1800 continue

      if (igas1_u .eq.0 .and. igas2_u .eq. 0) goto 2800

      if (igas2_u .eq. 0) then

         index2 = 6
      DO 2400 JT = 1,5
         KG = 'KG-T'//FNUM(INDEX2)//'-n09'
         OPEN(10,FILE=KG,FORM='UNFORMATTED')
         DO 2200 JP = 13, 59
            DO 2100 IG = 1, 16
               READ(10) KB_1(JT,JP,IG)
 2100                 CONTINUE
 2200                     CONTINUE
         CLOSE(10)
         index2=index2+1
 2400     CONTINUE
 
      DO 2700 IG = 1, 16
         DO 2600 JP = 13, 59
            WRITE(20,8020)JP,IG
            WRITE(20,8011)(KB_1(I,JP,IG),I=1,5)
 2600           CONTINUE
 2700            CONTINUE

      endif 

      if (igas2_u .ne. 0) then
      INDEX2 = 6

      DO 2450 JT = 1,5
      INDEX = 1
         DO 2350 JS = 1,5
            KG = 'KG-T'//FNUM(INDEX2)//'-n'//FNUM(INDEX)
            OPEN(10,FILE=KG,FORM='UNFORMATTED')

            DO 2250 JP = 13,59
               DO 2150 IG = 1, 16
                  READ(10) KB_2(JS,JT,JP,IG)
 2150          CONTINUE

 2250       CONTINUE
            INDEX = INDEX + 2
            CLOSE(10)
 2350    CONTINUE
         index2=index2+1
 2450 CONTINUE
 
      DO 2750 IG = 1, 16
         DO 2650 JP = 13, 59
            WRITE(20,9020)JP,IG
            WRITE(20,9021)(KB_2(I,1,JP,IG),I=1,5)
            WRITE(20,9021)(KB_2(I,2,JP,IG),I=1,5)
            WRITE(20,9021)(KB_2(I,3,JP,IG),I=1,5)
            WRITE(20,9021)(KB_2(I,4,JP,IG),I=1,5)
            WRITE(20,9022)(KB_2(I,5,JP,IG),I=1,5)
 2650    CONTINUE
 2750 CONTINUE

      CLOSE(20)

      endif

 2800 continue

 8000 FORMAT('       PARAMETER (MG=16)')
 8001 FORMAT('       DIMENSION SELFREF(10,MG)')
 8002 FORMAT('       REAL KA_1(5,13,MG)')
 8003 FORMAT('       REAL KA_2(9,5,13,MG)')
 8004 FORMAT('       REAL KB_1(5,13:59,MG)')
 8005 FORMAT('       REAL KB_2(5,5,13:59,MG)')
 8006 FORMAT('       COMMON /HVRSNB/ HVRKG(NBANDS)')
 8007 FORMAT('       COMMON /K1_1/ KA_1 ,KB_1, SELFREF')

 8008 FORMAT('       CHARACTER*8 HVRKG')
 8009 FORMAT('       COMMON /K1_2/ KA_2,KB_2')
 8010  FORMAT('      DATA (KA_1(JT,',I2,',',I2,'),JT=1,5) /')
 8011   FORMAT('     &',1P,4(E10.4,','),E10.4,0P,'/')
 8020    FORMAT('      DATA (KB_1(JT,',I2,',',I2,'),JT=1,5) /')

 9010 FORMAT('      DATA ((KA_2(JS,JT,',I2,',',I2,'),JS=1,9),JT=1,5) /')
 9011 FORMAT('     &',1P,6(E10.4,','),0P)
 9012 FORMAT('     &',1P,2(E10.4,','),E10.4,0P,'/')
 9020 FORMAT('      DATA ((KB_2(JS,JT,',I2,',',I2,'),JS=1,5),JT=1,5) /')
 9021 FORMAT('     &',1P,5(E10.4,','),0P)
 9022 FORMAT('     &',1P,4(E10.4,','),E10.4,0P,'/')

      STOP 
      END

