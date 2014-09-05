#!/usr/bin/perl -w

# This driver compares LBLRTM/RADSUM runs for many cases

@band_name_arr = ( 'band1','band2','band3','band4','band5','band6','band7','band8','band9','band10','band11','band12','band13','band14','band15','band16');

@case_names = qw (toba);
@ref_names = qw (us_std);
#@case_names = qw (pinatubo);
#@ref_names = qw (tro);
$ncases = @case_names;


for ($ic=0;$ic<$ncases;$ic++) {
   $lbl_dir[$ic] = $case_names[$ic]."/";
}

$idl_prog = "plot_radsum_multi_by_case";
$idl_run = ".r $idl_prog";
$exit = "exit";

for ($i=0;$i<$ncases;$i++) {
print $i,$case_names[$i],"\n";
}
print $ncases,"\n";
#exit;


@band_name_arr = ('1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16');

for ($i=0;$i<$ncases;$i++) {
#for ($i=0;$i<1;$i++) {
   system ("mkdir -p plots_$case_names[$i]");

   foreach $band_id (@band_name_arr) {
      
      print $band_id,"\n";
      $radsum_path1 = $lbl_dir[$i];
      $radsum_file = 'OUTPUT_RADSUM.band'.$band_id;
      $radsum_path2 = $ref_names[$i]."/";
      print $radsum_path2,"\n";
      
      $idl_command = "$idl_prog,'$radsum_path1','$radsum_file','$radsum_path2','$case_names[$i]'";   

      system ("idl <<IDL_INPUT\n$idl_run\n$idl_command\n$exit\nIDL_INPUT\n");
   }   

   chdir ('plots_'.$case_names[$i]);
   @plot_files = glob("*.ps");
   $nplots = @plot_files;

   @plot_id = ();
   %plots =();
   for ($ip=0;$ip<$nplots;$ip++) {
      $i1 = index($plot_files[$ip],'_')+1;
      $i2 = index($plot_files[$ip],'-');
      $plot_id = substr($plot_files[$ip],$i1,$i2-$i1);
      print $plot_id," ",$plot_files[$ip],"\n";
      $plots{$plot_files[$ip]} = $plot_id;
   }

   @sorted_plots = sort by_number keys(%plots);
   for ($ip=0;$ip<$nplots;$ip++) {
     print $sorted_plots[$ip],"\n";
   }

   system ("~/bin/multips2pdf.pl -pdf rrtm_lbl_case_$case_names[$i].pdf @sorted_plots");
   chdir ("../");
}

sub by_number {
   $plots{$a} <=> $plots{$b}; 
}

