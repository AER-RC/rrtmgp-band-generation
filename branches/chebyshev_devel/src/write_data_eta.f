      PROGRAM WRITE_DATA_KG
      
      CHARACTER*3 FNUM(101)
      CHARACTER*50 KG

C     KA is filled with absorption coefficients (x 1.e20) for the
C     lower atmosphere.  It should be dimensioned # of species levels X
C     5 X # of pressure levels X 16, where a 'species level' means a 
C     reference amount of a species (given as amt. species/amt.CO2) at
C     which absorption coefficient data is stored.  KB is the corre-
C     sponding array of absorption coefficients for above 
C     the lower atmosphere.
      REAL KA_2(9,5,13,16)
      REAL KETA_2(101,5,13,16)
      REAL KREF_2(101,5,13,16)
      REAL ONEMINUS
      REAL SPECPARM,SPECMULT
      REAL FS,FS1
      REAL ETASTORED(9)
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
     &     '90','91','92','93','94','95','96','97','98','99',
     &     '100','101'/
      DATA ETASTORED/0.0,0.125,0.25,0.375,0.5,0.625,0.75,
     &     0.875,1.0/
      NAMELIST /PAR/ WAVENUMber1,WAVENUMber2,IGAS1_L,IGAS2_L,IGAS1_U,
     &                   IGAS2_U ,igas_minor_l,igas_minor_u


c Set up variables
      ONEMINUS = 1. - 1.E-6

c Read input_param file; create array for writing
      read (*,par)

      INDEX2 = 1
      DO 1450 JT = 1, 5
      	INDEX = 1
        DO 1350 JS = 1, 9
          KG = 'KG-T'//trim(FNUM(INDEX2))//
     &'-n'//trim(FNUM(INDEX))
          OPEN(10,FILE=KG,FORM='UNFORMATTED')
          DO 1250 JP = 1, 13
            DO 1150 IG = 1, 16
              READ(10) KA_2(JS,JT,JP,IG)
 1150       CONTINUE
 1250     CONTINUE
          CLOSE(10)
          INDEX = INDEX + 1
 1350   CONTINUE
        index2=index2+1
 1450 CONTINUE

      INDEX2 = 1
      DO 1425 JT = 1, 5
      	INDEX = 1
        DO 1325 JS = 1, 101
      KG = '../csh_test_gb5/KG-T'//trim(FNUM(INDEX2))//
     &'-n'//trim(FNUM(INDEX))
          OPEN(10,FILE=KG,FORM='UNFORMATTED')
          DO 1225 JP = 1, 13
            DO 1125 IG = 1, 16
              READ(10) KREF_2(JS,JT,JP,IG)
 1125       CONTINUE
 1225     CONTINUE
          CLOSE(10)
          INDEX = INDEX + 1
 1325   CONTINUE
        index2=index2+1
 1425 CONTINUE

c Perform interpolation in eta
      OPEN(15,FILE='COMPARE_ETAS.txt',FORM='FORMATTED')

      DO 1625 IG = 1, 16
        DO 1600 JP = 1, 13 
          DO 1550 JT = 1, 5
            DO 1500 JN = 1, 101
            SPECPARM = (REAL(JN) - 1.0)*0.01
            SPECMULT = 8.*(SPECPARM)
            JS = 1 + INT(SPECMULT)
            FS = AMOD(SPECMULT,1.0)
            WRITE(15,7000),JN,JT,JP,IG
            IF (SPECPARM .GE. ONEMINUS) THEN
              KETA_2(JN,JT,JP,IG) = 
     &          KA_2(JS,JT,JP,IG)
              WRITE(15,7005) ETASTORED(JS),
     &          SPECPARM,SPECPARM,-1.
              WRITE(15,7010) KA_2(JS,JT,JP,IG),
     &          KETA_2(JN,JT,JP,IG),
     &          KREF_2(JN,JT,JP,IG),
     &          -1.
            ELSE
              IF (SPECPARM .LT. 0.125) THEN
                P = FS - 1
                P4 = P**4
                FK0 = P4
                FK1 = 1 - P - 2.0*P4
                FK2 = P + P4
                KETA_2(JN,JT,JP,IG) = 
     &            FK0*KA_2(JS,JT,JP,IG)+
     &            FK1*KA_2(JS+1,JT,JP,IG)+
     &            FK2*KA_2(JS+2,JT,JP,IG)
              ELSE IF (SPECPARM .GT. 0.875) THEN
                P = -FS
                P4 = P**4
                FK0 = P4
                FK1 = 1 - P - 2.0*P4
                FK2 = P + P4
                KETA_2(JN,JT,JP,IG) = 
     &            FK2*KA_2(JS-1,JT,JP,IG)+
     &            FK1*KA_2(JS,JT,JP,IG)+
     &            FK0*KA_2(JS+1,JT,JP,IG)
              ELSE
                FS1 = 1.0 - FS
                KETA_2(JN,JT,JP,IG) = 
     &            FS1*KA_2(JS,JT,JP,IG)+
     &            FS*KA_2(JS+1,JT,JP,IG)
              ENDIF
              WRITE(15,7005) ETASTORED(JS),
     &          SPECPARM,SPECPARM,ETASTORED(JS+1)
              WRITE(15,7010) KA_2(JS,JT,JP,IG),
     &          KETA_2(JN,JT,JP,IG),
     &          KREF_2(JN,JT,JP,IG),
     &          KA_2(JS+1,JT,JP,IG)
            ENDIF
1500        CONTINUE
1550      CONTINUE     
1600    CONTINUE     
1625  CONTINUE     
      CLOSE(15)

      OPEN(UNIT=10,FORM='unformatted',FILE='COMPARE_ETAS.dat')
      WRITE(10) ETASTORED
      WRITE(10) KA_2
      WRITE(10) KETA_2
      WRITE(10) KREF_2
      CLOSE(UNIT=10)

c Write data file
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
 1650   CONTINUE
 1750 CONTINUE

 7000 FORMAT('INDEX (ETA,T,P,IG): ',3(i3,','),i3)
 7005 FORMAT('ETA(JS,INT,REF,JS+1): ',
     & 1P,3(E10.4,','),E10.4)
 7010 FORMAT('ABS(JS,INT,REF,JS+1): ',
     & 1P,3(E10.4,','),E10.4)
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

