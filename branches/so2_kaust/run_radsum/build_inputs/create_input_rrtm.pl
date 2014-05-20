#!/usr/bin/perl -w

# Build INPUT_RRTM from TAPE

@tape5_files = glob("TAPE5.GAR*b*");

foreach $it (@tape5_files) {
   $id_prof = substr($it,12,1);
   $id_case = substr($it,14);
   $rrtm_in_file = 'INPUT_RRTM.GARAND_'.$id_prof;
   $rrtm_out_file = 'INPUT_RRTM.GARAND'.$id_prof.'.'.$id_case;
   system ("head -3 $rrtm_in_file > $rrtm_out_file");
   system ("tail -n +6 $it >> $rrtm_out_file");
}
