#!/bin/csh -f

set noglob

# Set up the initial run beforehand with 
# script.gen_2band $1

# To run : script.minors_multilevel band_dir atmos_level minors_case minors_index

# where :

#	band_dir 	working directory for this band
#       atmos_level	UPPER or LOWER
#	minors_case	FIRST or NOT_FIRST 
#       minors_index	index of minor species whose sorting level is being determined

# Script generates LBLRTM and ARRTM flux files to determine best sorting level for
# minor species K's. Lower 

# Two types of runs are possible:
#       first
#       not first

# Script runs ARRTM  with sorting for the minor species specified at six (lower atmosphere) or
# nine (upper atmosphere)  different levels for three different atmospheres.Note that ARRTM
# uses the input_param information to exclude all species not listed as major or minor.

# The FIRST cases determines from isortplanck(2) in input_param which kg_planck.f file
# to use. LBLRTM is run with all major species and the minor species determined by minors_index.
# kg_minor is created, containing k_values for just this species and atmosphere level.
# After running plot_select_minor.pro, the user must edit input_param and run 
# update_kg_minor.

 
# The NOT_FIRST  case is more complicated: 
# Each minor gas in each section of the atmosphere (upper or lower) must be
# added sequentially to the input_param file and then the script must be run for
# this case. Once the script has been run, plot_select_minors is used to determine
# sorting level for this minor gas. This information is stored in input_param,
# and the next minor gas is turned on. Note that the same minor gas in the upper and
# lower atmosphere should be treated as two different gases.
# LBLRTM is run for all the major species and all the minor species whose sorting
# level has already been determined, plus the current minor species.
# kg_minor files are updated to include the k values for the current species.
# These runs will be turned by setting minors_case to NOT_FIRST.
# After running plot_select_minor.pro, the used must edit input_param and 
# run update_kg_minor.

# Note :input_param must be set so that key species and minor species are included.
# isortplanck should have been determined from script.planck_multilevel and
# plot_select_pl.pro


# plot_select_minor.pro then compares the output from the LBLRTM and RRTM flux calculation

# Karen Cady-Pereira   October 2000


echo "Make sure you have updated input_param"
cd $1
set atm = (MLS SAW TRP)

set atmos_level = $2
set minors_case = $3
set minors_index = $4

goto finish

#  Run  script to get LBL_MINORS validation runs; name output with 
#  distinct ending (_MINORS)

      foreach i ($atm)

# build correct input_param and tape5 for _MINORS OD files
# note that other input files for flux calculations are generated by gen_2band

	 \cp ../input_param input_param
	 set lowatm = `echo $i | tr "[A-Z]" "[a-z]"`
	 echo " &atm atmos='"$lowatm"' atmos_level='"$atmos_level"' /" >> input_param
	 echo " &atm atmos='"$lowatm"' atmos_level='"$atmos_level"' /" 

	\cp ../central_exec_scripts/template_${lowatm} .
	 write_tape5_od_minors < input_param
	 \rm template_${lowatm}
         \rm input_param

# run LBLRTM and radsum to calculate  fluxes

	 cd  ../lbl_f90
	 echo "Running OD"
	 \cp ../$1/TAPE5_${i}OD_MINORS TAPE5
	 lblrtm_f90
	 echo "Running RD"
	 \cp ../$1/TAPE5_${i}RD TAPE5
	 lblrtm_f90
	 echo "Running RAD"
	 \cp ../$1/RADINIT_${i} RADINIT
	 radsum.x
	 \mv flxupdn.dat ../$1/OUTPUT_LBLRTM_${i}_MINORS_${atmos_level}_${minors_index}
	 \cp ../$1/TAPE5_${i}OD_MINORS ../$1/TAPE5_${i}OD_MINORS_${atmos_level}_${minors_index}
	 echo Finish ${i}
	 cleanup_lbl
	 cd ../$1
      end

# Make input_param for other test PL levels and generate Planck function files

finish:

if ($minors_case == "FIRST" ) then
   set planck = `nawk 'NR == 9 {print $7}' ../input_param` 
   echo $planck
   if ($planck != 0) then
      \cp kg_planck_UPPER_PL_${planck}.f kg_planck.f 
   else
      set planck = `nawk 'NR == 9 {print $4}' ../input_param` 
      \cp kg_planck_LOWER_PL_${planck}.f kg_planck.f 
    endif
    \cp kg_planck.f src_arrtm_$1/.
   \mv kg_minor.f kg_minor.f.all
   \cp ../input_param input_param_minors
endif

if ($atmos_level == "LOWER" ) then
   set levels = (1 3 5 7 9 11)
   set minor_gas = "igas_minor_l[(]$minors_index,1[)]"
else if ($atmos_level == "UPPER" ) then
   set levels = (1 6 11 16 21 26 31 36 41)
   set minor_gas = "igas_minor_u[(]$minors_index,1[)]"
else
   echo "Can only run for UPPER or LOWER as argument 2"
   echo "Will exit"
   exit
endif

if ($minors_case != "FIRST" ) then
   echo "Have you updated kg_minor.f?"
   \cp kg_minor.f kg_minor_old.f
   echo $minor_gas
   nawk  -v minor_gas=$minor_gas ' \
   NR <= 2 {print} \
   ($1 ~ minor_gas) {if (NF == 6) $6 = 0; if(NF == 9) $9=0; print " "$0} \
   ($4 ~ minor_gas) {$3=0; if (NF == 9) $9 = 0; print " "$0} \
   ($7 ~ minor_gas) {$3=0; $6=0; print " "$0} \
   (NR>2)&&(NR<9)&&($1!~minor_gas)&&($4!~minor_gas)&&($7!~minor_gas){$3=0;if(NF>=6)$6=0;if(NF==9)$9=0;print " "$0}\
   NR == 9 {print} \
    '  ../input_param > input_param_minors

endif

cd ..
foreach  i ( $levels )
  echo PL_$i

  nawk -v level=$i -v minor_gas=$minor_gas ' \
   ($1 !~ minor_gas ) && ($4 !~ minor_gas ) &&  ($7 !~ minor_gas ) {print} \
   $1 ~ minor_gas {print " "$1, $2, level,  $4, $5, $6, $7, $8, $9 } \
   $4 ~ minor_gas {print " "$1, $2, $3,  $4, $5, level, $7, $8, $9 } \
   $7 ~ minor_gas {print " "$1, $2, $3,  $4, $5, $6, $7, $8, level } \
    '  input_param > input_param_${minors_case}_${atmos_level}_$i

  \cp input_param_${minors_case}_${atmos_level}_$i $1/input_param
  \mv input_param_${minors_case}_${atmos_level}_$i $1/.
   script.gen_minors.flux $1 PL_$i $atmos_level $minors_case $minors_index
end

echo Finished
