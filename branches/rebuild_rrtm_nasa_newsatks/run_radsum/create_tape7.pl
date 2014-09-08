#!/usr/bin/perl -w

#This driver runs LBLRTM and generates TAPE7s for all sox cases

$code = 'lblrtm_v12.2_linux_pgi_dbl';
$input_dir = '/project/p1905/build_inputs/';

@band_name_arr = ('band1');

@garand_dir =  qw (GARAND1.Toba GARAND1.Pinatubo GARAND5.Toba GARAND5.Pinatubo GARAND6.Toba GARAND6.Pinatubo );
@case_names = @garand_dir;
$ncases = @case_names;

$input_dir = '/project/p1905/build_inputs/';

for ($ic=0;$ic<$ncases;$ic++) {
   $lbl_dir[$ic] = $case_names[$ic];
}
for ($ic=0;$ic<$ncases;$ic++) {
   print $case_names[$ic],",\n";
}


foreach $band_name (@band_name_arr ) {
   for ($ic=0;$ic<$ncases;$ic++) {
   # create TAPE5 for TAPE7 generation and run LBLRTM
      $outdir = $lbl_dir[$ic];
      system ("mkdir -p $outdir");
      $tape5_file = $input_dir.'TAPE5.'.$case_names[$ic];
      system ("head -4 $tape5_file >  TAPE5");
      system ("cat TAPE5.tape7.header >> TAPE5");
      system ("tail -n +6 $tape5_file >> TAPE5");
      system ($code);
      system ("/bin/mv TAPE7 $outdir/TAPE7_$garand_dir[$ic]");
   }
}
   
