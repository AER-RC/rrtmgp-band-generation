#!/bin/csh -f

# Set up the initial run beforehand with 
# script.gen_2band $1

# To run : script.planck_multilevel band_dir atmos_level

# Stripped down version of script.planck_multilevel.flux 

# Note :input_param must be set so that only key species are included.
# When running lower atmosphere (WHICH SHOULD BE DONE FIRST!!!!!)
# set isortplanck(2) to match level chosen in taumol.f
# When running upper atmosphere set isortplanck(1) to match level 
# determined by running this script on the lower atmosphere and
# then running plot_select_pl.pro


# plot_select_pl.pro then compares the output from the LBLRTM and RRTM flux calculation

# Karen Cady-Pereira   June 2014


cd $1
set atmos_level = $2

# Run  script to get LBL ONLY_KEY validation runs; Name output with distinct ending (_ONLY_KEY)


if ($atmos_level == "UPPER" ) then
   set lower_planck = `nawk 'NR == 9 {print $4}' ../input_param` 
   echo $lower_planck
   \cp kg_planck_LOWER_PL_${lower_planck}.f kg_planck_lower.f 
endif

# Make input_param for other test PL levels and generate Planck function files

cd ..
if ($2 == "LOWER" ) then
   set levels = (1 3 5 7 9 11)
else if ($2 == "UPPER" ) then
   set levels = (1 6 11 16 21 26 31 36 41)
else
   echo "Can only run for UPPER or LOWER as argument 2"
   echo "Will exit"
   exit
endif

foreach  i ( $levels )
  echo PL_$i
  nawk -v level=$i -v atmos=$2 ' \
	  NR!=9 {print}\
          NR==9 && atmos=="LOWER" {print " "$1, $2, $3, level, $5, $6, $7"/"}\
          NR==9 && atmos=="UPPER" {print " "$1, $2, $3, $4, $5, $6, level"/"}  ' input_param > input_param_$2_PL_$i
  \cp input_param_$2_PL_$i $1/input_param
  \mv input_param_$2_PL_$i $1/input_param_$2_PL_$i
  script.gen_planck_only_key.flux $1 PL_$i $2
end

echo Finished
