#!/usr/bin/perl -w

#This driver runs LBLRTM and RADSUM for  the GARAND cases with SO2 (Pinatubo and Toba)

$code = 'lblrtm_v12.2_linux_pgi_dbl';
$radsum_home ='/project/p1905/radsum/';
$input_dir = '/project/p1905/build_inputs/';

@band_name_arr =
('band1','band2','band3','band4','band5','band6','band7','band8','band9','band10','band11','band12','band13','band14','band15','band16');

#@garand_dir =  qw (GARAND1.Toba GARAND1.Pinatubo GARAND5.Toba GARAND5.Pinatubo GARAND6.Toba GARAND6.Pinatubo );
@garand_dir =  qw (GARAND1 GARAND5 GARAND6);
@case_names = @garand_dir;
$ncases = @case_names;

for ($ic=0;$ic<$ncases;$ic++) {
   $lbl_dir[$ic] = $case_names[$ic];
}
for ($ic=0;$ic<$ncases;$ic++) {
   print $case_names[$ic],",\n";
}


foreach $band_name (@band_name_arr ) {
   for ($ic=0;$ic<$ncases;$ic++) {
   # create TAPE5 for OD calculation and run LBLRTM
      $outdir = $lbl_dir[$ic];
      system ("mkdir -p $outdir");
      $tape5_file = $input_dir.'TAPE5.'.$case_names[$ic];
      $rrtm_file = $input_dir.'INPUT_RRTM.'.$case_names[$ic];
      system ("head -4 $tape5_file >  TAPE5");
      system ("cat TAPE5.$band_name.header >> TAPE5");
      system ("tail -n +6 $tape5_file >> TAPE5");
      system ($code);
      system ("/bin/mv TAPE5 $outdir/TAPE5.$band_name");

   # create TAPE5 for radiance calculations and run LBLRTM
      build_tape5_rad ($rrtm_file,$case_names[$ic],$band_name);
      system ($code);
      system ("/bin/mv TAPE5 $outdir/TAPE5.$band_name.rad");
      system ("/bin/rm ODdeflt*");
      
   # run RADSUM and store output
      #system ("cp IN_RADSUM.$band_name IN_RADSUM");
      system ("sed s/-1.00/$tsfc/ IN_RADSUM.$band_name > IN_RADSUM");
      system ("radsum_v2.6_linux_pgi_dbl");
      system ("/bin/mv TAPE3[1-3] $outdir");
      system ("/bin/mv TAPE6[1-3] $outdir");
      system ("/bin/mv OUTPUT_RADSUM $outdir/OUTPUT_RADSUM.$band_name");


   }
}
#------------------------------------------------------------------------
sub build_tape5_rad {
   my ($fn,$cn,$bn) = @_ ;
   $emiss = 1.0;
   $t_toa = 0.;
   $od_name = "ODdeflt_";
   $blank = " ";
   $scnmrg_line = "    1    0    0    0.0000                  ";
   $hwhm = 0.25;
   @merge_files = (31,32,33,61,62,63);
   @gauss_ang = (0.91141,0.59053,0.21234,0.91141,0.59053,0.21234);

# get spectral line from TAPE5
  open (TAPE5_header,"TAPE5.".$bn.".header");
  while (defined ($line=<TAPE5_header>)) {
    $spec_line = $line;
    chomp ($spec_line);
  }
  close(TAPE5_header);
  $v1 = substr($spec_line,0,10)+4.75;
  $v2 = substr($spec_line,10,10)-4.75;

# get surface temp and number of levels for radiance calculations
   open (INPUT_RRTM,$fn);
   $kount=0;
   while ($kount < 4) {
      $line=<INPUT_RRTM>;
      $kount++;
      if ($kount == 3) {
	 $_ = $line;
	 @split_line = split;
	 $tsfc = $split_line[0];
      }
      if ($kount == 4) {
	 $_ = $line;
	 @split_line = split;
         $nlev = $split_line[2]-1;
      }
   }
   close(INPUT_RRTM); 
   open (TAPE5,">TAPE5");
   print TAPE5 "         1         2         3         4         5         6         7         8         9 \n";
   print TAPE5 "123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 \n";

   for ($iline=0;$iline<6;$iline++) {
      print TAPE5 "\$",$cn,"\n";
      if ($iline <= 2) {
	print TAPE5 " HI=0 F4=0 CN=1 AE=0 EM=1 SC=0 FI=0 PL=0 TS=0 AM=1 MG35 LA=0 OD=0 XS=0    0    0 \n";
	print TAPE5 $spec_line, "\n";
        printf TAPE5 "%10.4f%10.4f \n",$t_toa,$emiss;
      } else {
	print TAPE5 " HI=0 F4=0 CN=1 AE=0 EM=1 SC=0 FI=0 PL=0 TS=0 AM=1 MG36 LA=0 OD=0 XS=0    0    0 \n";
	print TAPE5 $spec_line, "\n";
        printf TAPE5 "%10.4f%10.4f \n",$tsfc,$emiss;
      }
       
      printf TAPE5 "%7s%48s%4d \n",$od_name,$blank,$nlev;
      printf TAPE5 "%10.3f%10.3f%10.3f%43s%2d \n",$hwhm,$v1,$v2,$scnmrg_line,$merge_files[$iline];
      printf TAPE5 "%10.8f \n",$gauss_ang[$iline];
   }
   print TAPE5 "% \n";
   close(TAPE5);
}
   
