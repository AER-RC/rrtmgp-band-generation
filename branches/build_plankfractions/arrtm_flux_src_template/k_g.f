C     path:      /data3/jen/new_model/rrtm2_3/SCCS/s.k_g.f
C     revision:  2.5
C     created:   7/12/96  17:27:31
C     presently: 5/14/98  16:51:04
****************************************************************************

      BLOCK DATA KGB1
      PARAMETER (NBANDS = 16)
      INCLUDE 'k_gB01.f'
      END

      BLOCK DATA KG_MINOR
      INCLUDE 'kg_minor.f'
      END
      
      BLOCK DATA BLOCK_CONT
      INCLUDE 'kg_cont.f'
      END

      BLOCK DATA BLOCK_PLANCK
      INCLUDE 'kg_planck.f'
      END

      
****************************************************************************

