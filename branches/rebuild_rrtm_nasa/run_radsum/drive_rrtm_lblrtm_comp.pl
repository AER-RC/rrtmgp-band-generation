#!/usr/bin/perl -w

# This driver runs RRTM for a series of cases and compares with pre-run LBLRMT/RADSUM run

$radsum_home ='/project/p1905/radsum/';

$code = ("/project/p1905/band_build_tst/rrtm_lw/rrtm_v3.3.1_linux_pgf90");
$rrtm_dir = '/project/p1905/rrtm_lw_std/';

#$code = ("/project/p1905/band_build_tst/rrtm_lw_so2_kaust/rrtm_v3.3.1_linux_pgf90");
#$rrtm_dir = '/project/p1905/rrtm_lw_with_so2/';

$band_id = '3';
$band_num = $band_id;
$band_name = 'band'.$band_id;

print $band_num,"\n";

#@case_names =  glob("GARAND*o*");
@case_names =  glob("GARAND[156]");
$ncases = @case_names;

@lbl_dir = ();
for ($ic=0;$ic<$ncases;$ic++) {
   $lbl_dir[$ic] = $case_names[$ic]."/";
}

$idl_prog = "plot_radsum_rrtm";
$idl_run = ".r $idl_prog";
$exit = "exit";

for ($i=0;$i<$ncases;$i++) {
print $i,$case_names[$i],"\n";
}
print $ncases,"\n";
#exit;

for ($i=0;$i<$ncases;$i++) {
#for ($i=11;$i<12;$i++) {
# read in INPUT_RRTM
   chdir($rrtm_dir);
   system ("/bin/rm  OUTPUT_RRTM INPUT_RRTM");
   open (INPUT_RRTM,"INPUT_RRTM.".$case_names[$i]);
   @line_arr = ();
   $kount = 0;
   while (defined ($line = <INPUT_RRTM>)) {
     $line_arr[$kount] = $line;
     $kount++;
   }
   close(INPUT_RRTM);

# modify for this band
   $band_str = sprintf ("%3d",$band_num);
   print $band_str,"\n";
   substr($line_arr[1],87,3)=$band_str;
     
   open (INPUT_RRTM,">INPUT_RRTM");
   for ($ik=0;$ik<$kount;$ik++) {
      print INPUT_RRTM $line_arr[$ik];
   }
   close(INPUT_RRTM);

   system ($code);
   system ("/bin/mv OUTPUT_RRTM OUTPUT_RRTM.$case_names[$i].$band_name");
   
   chdir($radsum_home);
   $radsum_path = $radsum_home.$lbl_dir[$i];
   $radsum_file = "OUTPUT_RADSUM.".$band_name;
   $rrtm_path = $rrtm_dir;
   $rrtm_file = "OUTPUT_RRTM.".$case_names[$i].".".$band_name;
   
   $idl_command = "$idl_prog,'$radsum_path','$radsum_file','$rrtm_path','$rrtm_file','$case_names[$i]'";   
   print $idl_command,"\n";

   system ("idl <<IDL_INPUT\n$idl_run\n$idl_command\n$exit\nIDL_INPUT\n");
   

}
