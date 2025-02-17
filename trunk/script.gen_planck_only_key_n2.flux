#!/bin/csh 

cd $1
echo  Setting up correct input_param file

ln -s ../lbl_f90/TAPE3 .

if ($3 == "LOWER" ) then 
   if ( -e "tape5-T03-n01") then
       @ jl=1
   else
       @ jl=9	
   endif

   echo Generating lower atm kfiles for planck
   while ($jl < 10)
       @ i = 3
       set tape5od = tape5-T0${i}-n0${jl} 
#       set tape5od = tape5-T0${i}-n0${jl}.planck
       echo ${tape5od}
       \cp ${tape5od} TAPE5
       lblrtm_f90
       set kgfile = KG_bbp1_n0${jl}
       \rm tmp
       \rm input_param_planck
       echo " &igaspar igas_on=1/" > tmp
       cat input_param tmp > input_param_planck
       cat input_param_planck
       kdis_2sort_planck < input_param_planck 
       \mv -f CK_ABS_PL.DAT ${kgfile}		
       \rm tmp
       \rm input_param_planck
       @ jl++
   end
endif

if ($3 == "UPPER" ) then 
   if ( -e "tape5-T08-n01") then
       @ ju=1
   else
       @ ju=9
   endif

   echo Generating upper atm kfiles for planck
   while ($ju < 10)
      @ i=8
#      set tape5od = tape5-T0${i}-n0${ju} 
#      set tape5od = tape5-T0${i}-n0${ju}.planck 
#      \cp ${tape5od} TAPE5
#      echo ${tape5od}
#      lblrtm_f90
      set kgfile = KG_bbp2_n0${ju}
      echo " &igaspar igas_on=2/" > tmp
      cat input_param tmp > input_param_planck
      kdis_2sort_planck < input_param_planck 
      \mv -f CK_ABS_PL.DAT ${kgfile}		
      \rm tmp 
      \rm input_param_planck
      @ ju +=2
   end
endif

write_data_planck < input_param

if ($3 == "UPPER" ) then
   \mv kg_planck.f kg_planck_upper.f

   nawk ' NR < 5 {print}  \
       /FRACREFA[(]IG/ , /FRACREFB[(]IG/ ' kg_planck_lower.f > kg_planck.f
   nawk ' /FRACREFB[(]IG/ , /FRACREFA[(]IG/ ' kg_planck_upper.f | tail +2 >> kg_planck.f
#   nawk ' /FRACREFB[(]IG/ , /FRACREFA[(]IG/ ' kg_planck_upper.f | tail +1 >> kg_planck.f
endif
   
\cp kg_planck.f kg_planck_$3_$2.f
\cp kg_planck.f src_arrtm_$1/.

#Generate the necessary Planck information to be used as
#input to ARRTM. This information is compiled into a file
#called block_planck which is then transfered into ARRTM
#program ASETCOEF.F

echo 'Generating Planck functions for band'
planck_int < input_param

\cp avplank_01.f src_arrtm_$1/.

#Remove all excess files from the band directory before
#leaving it.
#\rm OD*
\rm NC*
\rm TAPE3
\rm TAPE9
\rm TAPE10
\rm TAPE11
\rm TAPE12

#Now go to the ARRTM directory and run the code to
#calculate the fluxes in your band.

cd src_arrtm_$1

echo 'Compiling new ARRTM source code'
gmake -f make_arrtm_f90
cd ..
\rm arrtm
ln -s src_arrtm_$1/arrtm .

foreach atmos (MLS SAW TRP)
   echo 'Running ARRTM for '$atmos
   if ($2 == "PL_1") then
      \rm  INPUT_RRTM_${atmos}_ONLY_KEY
      head -7 INPUT_RRTM_$atmos > INPUT_RRTM_${atmos}_ONLY_KEY
      tail +9 ../$1/TAPE5_${atmos}OD_ONLY_KEY>> INPUT_RRTM_${atmos}_ONLY_KEY
   endif
   \cp INPUT_RRTM_${atmos}_ONLY_KEY INPUT_RRTM 
   arrtm < input_param 
   \cp OUTPUT_RRTM OUTPUT_ARRTM_${atmos}_ONLY_KEY_$3_$2
end


