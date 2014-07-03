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
      set tape5od = tape5-T0${i}-n0${ju} 
#      set tape5od = tape5-T0${i}-n0${ju}.planck 
      \cp ${tape5od} TAPE5
      echo ${tape5od}
      lblrtm_f90
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

   awk ' NR < 5 {print}  \
       /FRACREFA[(]IG/ , /FRACREFB[(]IG/ ' kg_planck_lower.f > kg_planck.f
   awk ' /FRACREFB[(]IG/ , /FRACREFA[(]IG/ ' kg_planck_upper.f | tail +2 >> kg_planck.f
#   nawk ' /FRACREFB[(]IG/ , /FRACREFA[(]IG/ ' kg_planck_upper.f | tail +1 >> kg_planck.f
endif
   
\cp kg_planck.f kg_planck_$3_$2.f


