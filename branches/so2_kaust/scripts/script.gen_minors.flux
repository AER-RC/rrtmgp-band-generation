#!/bin/tcsh

cd $1

\rm input_minor

set atmos_level = $3
set minors_case = $4
set minors_index = $5


#Test to see if you are doing case of one or two major gases
#by checking the existence of the tape5 files.

if ($atmos_level == "LOWER") then 

   if ( -e "tape5-T03-n01") then
       @ jl=1
       echo Two Key Species in Lower Atmosphere
   else
       @ jl=9	
       echo One Key Species in Lower Atmosphere
   endif

   while ($jl < 10)
       @ i=3
	echo eta = 0$jl tref = 0$i
	set tape5od = tape5-T0${i}-n0${jl} 
	     if (-e "tape5nc-minor0"${minors_index}"-T03") then
		echo Calculating Minor Gas ${minors_index}
		@ k1 = 2
		while (${k1} < 4) 
		\cp ${tape5od} TAPE5
		lblrtm_f90
		script.mv_files 13 ODint_ ODtmp_

		   set tape5minor = tape5nc-minor0${minors_index}-T0${k1}
		   set kgminor =  KG-minor0${minors_index}-T0${k1}-n0${jl}
		   \cp ${tape5minor} TAPE5
		   lblrtm_f90
		   script.mv_files 13 ODint_ NCint_
		   script.mv_files 13 ODtmp_ ODint_

		   \rm input_minor
		   echo " &igaspar igas_on="${minors_index}" iatmtype=1/" > tmp
		   cat input_param tmp > input_minor
		   \rm tmp
		   echo Sorting Minor Gas ${minors_index}
		   kdis_2sort_minor < input_minor
		   \mv -f CK_ABS_MINOR.DAT ${kgminor}		
		   @ k1++
		end
	    endif

#  Advance counter and run another reference TAPE5 file
    @ jl++
   end

endif
echo "have done lower atmosphere"


if ($atmos_level == "UPPER") then 

   if ( -e "tape5-T08-n01") then
       @ ju=1
       echo Two Key Species in Upper Atmosphere
   else
       @ ju=9
       echo One Key Species in Upper Atmosphere
   endif

   \rm ODint*
   \rm ODtmp*

   while ($ju < 10)
       @ i=8
       set tape5od = tape5-T0${i}-n0${ju} 
       \rm TAPE5
	  if (-e "tape5nc-minor0"${minors_index}"-T08") then
	     echo Calculating Minor Gas ${minors_index}
	     @ k1 = 7
	     while (${k1} < 9) 
	       \cp ${tape5od} TAPE5
	       lblrtm_f90
	       \script.mv_files 47 ODint_ ODtmp_

		set tape5minor = tape5nc-minor0${minors_index}-T0${k1}
		set kgminor =  KG-minor0${minors_index}-T0${k1}-n0${ju}
		\cp ${tape5minor} TAPE5
		lblrtm_f90
		script.mv_files 47 ODint_ NCint_
		script.mv_files 47 ODtmp_ ODint_
		\rm input_minor
		echo " &igaspar igas_on="${minors_index}" iatmtype=2/" > tmp
		cat input_param tmp > input_minor
		\rm tmp
		echo Sorting Minor Gas ${minors_index}
		kdis_2sort_minor < input_minor
		\mv -f CK_ABS_MINOR.DAT ${kgminor}		
		@ k1++
	     end
	  endif
       @ ju+=2
   end

endif
\rm OD*
\rm NC*
\rm TAPE9
\rm TAPE10
\rm TAPE11
\rm TAPE12

echo 'Writing absorption coefficients to file'

#write_data_kminor < input_param_minors
write_data_kminor < input_param
\cp kg_minor.f kg_minor_${atmos_level}_${minors_index}_$2.f

