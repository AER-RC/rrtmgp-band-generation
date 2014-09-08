#!/bin/csh -f


#Generate TAPE5s for self-continuum

head -7 tape5-T03-n09 > tmp
cat tmp ../scripts/template_self > t5_self
set tape5od = t5_self
\rm TAPE5
\cp $tape5od TAPE5
lblrtm_f90

\rm self.coef
\cp ../scripts/self.coef.296 self.coef

set kgfile = KG_bbs1
echo " &lev level=1/" > tmp
cat input_param tmp > input_param_self
\rm tmp
kdis_2sort_self < input_param_self
\rm input_param_self
\mv -f CK_ABS_SELF.DAT ${kgfile}		

\rm self.coef
\cp ../scripts/self.coef.260 self.coef

set kgfile = KG_bbs2
echo " &lev level=2/" > tmp
cat input_param tmp > input_param_self
\rm tmp
kdis_2sort_self < input_param_self
\rm input_param_self
\mv -f CK_ABS_SELF.DAT ${kgfile}		

#Generate TAPE5s for foreign-continuum
head -7 tape5-T03-n09 > tmp
cat tmp ../scripts/template_foreign > t5_foreign
set tape5od = t5_foreign
\rm TAPE5
\cp ${tape5od} TAPE5
lblrtm_f90

\rm for.coef
\cp ../scripts/for.coef.296 for.coef
set kgfile = KG_bbf1
echo " &lev level=1/" > tmp
cat input_param tmp > input_param_for
\rm tmp
kdis_2sort_for < input_param_for
\rm input_param_for
\mv -f CK_ABS_FOR.DAT ${kgfile}		

\rm for.coef
\cp ../scripts/for.coef.260 for.coef
set kgfile = KG_bbf2
echo " &lev level=2/" > tmp
cat input_param tmp > input_param_for
kdis_2sort_for < input_param_for
\rm input_param_for
\rm tmp
\mv -f CK_ABS_FOR.DAT ${kgfile}		

\rm for.coef
\cp ../scripts/for.coef.224 for.coef
set kgfile = KG_bbf3
echo " &lev level=3/" > tmp
cat input_param tmp > input_param_for
kdis_2sort_for < input_param_for
\rm tmp
\rm input_param_for
\mv -f CK_ABS_FOR.DAT ${kgfile}		

\rm for.coef
\cp ../scripts/for.coef.260 for.coef
set kgfile = KG_bbf4
echo " &lev level=4/" > tmp
cat input_param tmp > input_param_for
kdis_2sort_for < input_param_for
\rm tmp
\rm input_param_for
\mv -f CK_ABS_FOR.DAT ${kgfile}		

write_data_cont 

#\cp ../central_exec_scripts/PRESSURE.PROFILE .
#write_tape5 < input_param


if ( -e "tape5-T03-n01") then
    @ jl=1
else
    if (-e "tape5-T03-n09") then
       @ jl=9	
    else
       @ jl=10
    endif
endif

while ($jl < 10)
    @ i = 3
    set tape5od = tape5-T0${i}-n0${jl} 
    \cp ${tape5od} TAPE5
    lblrtm_f90
    set kgfile = KG_bbp1_n0${jl}
    echo " &igaspar igas_on=1/" > tmp
    cat input_param tmp > input_param_planck
    kdis_2sort_planck < input_param_planck 
    \mv -f CK_ABS_PL.DAT ${kgfile}		
    \rm tmp
    \rm input_param_planck
    @ jl++
end


if ( -e "tape5-T08-n01") then
    @ ju=1
else
    if (-e "tape5-T08-n09") then
       @ ju=9
    else
       @ ju=10
    endif
endif

while ($ju < 10)
    @ i=8
    set tape5od = tape5-T0${i}-n0${ju} 
    \cp ${tape5od} TAPE5
    lblrtm_f90
    set kgfile = KG_bbp2_n0${ju}
    echo " &igaspar igas_on=2/" > tmp
    cat input_param tmp > input_param_planck
    kdis_2sort_planck < input_param_planck 
    \mv -f CK_ABS_PL.DAT ${kgfile}		
    \rm tmp 
    \rm input_param_planck
    @ ju+=2
end

write_data_planck < input_param
