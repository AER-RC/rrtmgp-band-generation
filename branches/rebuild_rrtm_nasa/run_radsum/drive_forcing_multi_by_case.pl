#!/usr/bin/perl -w

# This driver compares forcings from LBLRTM/RADSUM against RRTM for many cases

@case_names = qw (GARAND1 GARAND5 GARAND6);
$ref_name = "Toba";
$ncases = @case_names;


for ($ic=0;$ic<$ncases;$ic++) {
   $lbl_dir[$ic] = $case_names[$ic]."/";
}

$idl_prog = "plot_forcing_multi_by_case";
$idl_run = ".r $idl_prog";
$exit = "exit";

for ($i=0;$i<$ncases;$i++) {
print $i,$case_names[$i],"\n";
}
print $ncases,"\n";
#exit;

system ("mkdir -p plots_$ref_name");

$rrtm_path1 = "/project/p1905/rrtm_lw_with_so2/";
$rrtm_path2 = "/project/p1905/rrtm_lw_std/";

#@band_name_arr = ('1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16');
@band_name_arr = ('band3');

for ($i=0;$i<$ncases;$i++) {
#for ($i=0;$i<1;$i++) {

   foreach $band_id (@band_name_arr) {
      
      print $band_id,"\n";

      $radsum_path1 = $case_names[$i].".".$ref_name."/";
      $radsum_file = 'OUTPUT_RADSUM.'.$band_id;
      $radsum_path2 = $lbl_dir[$i]."/";
      
      $rrtm_file = 'OUTPUT_RRTM.'.$case_names[$i].'.'.$ref_name.'.'.$band_id;
      $idl_command = "$idl_prog,'$radsum_path1','$radsum_file','$radsum_path2', '$rrtm_path1','$rrtm_file','$rrtm_path2', '$ref_name','$case_names[$i]'";  

      system ("idl <<IDL_INPUT\n$idl_run\n$idl_command\n$exit\nIDL_INPUT\n");
   }   
}

chdir ('plots_'.$ref_name);
@plot_files = glob("*.ps");
$nplots = @plot_files;

system ("~/bin/multips2pdf.pl -pdf rrtm_lbl_case_$ref_name.pdf @plot_files");

sub by_number {
   $plots{$a} <=> $plots{$b}; 
}

