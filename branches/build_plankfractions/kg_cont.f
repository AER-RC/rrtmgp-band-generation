       PARAMETER (MG=16,NMINOR=7)
       REAL FORREF(4,MG)
       REAL S296(MG)
       REAL S260(MG)
       COMMON /K_CONT/ FORREF, S296, S260
      DATA (FORREF(1,IG),IG=1,16) /
