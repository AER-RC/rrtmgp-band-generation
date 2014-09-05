; Code drives convert_minor on all kg_xxx.f files created during a 
; a script.minors_multilevel.flux run.

; This is a file containing Ks for a single minor, to be inserted as kg_minor.f into rrtm.
; rrtm accesses it through an include statement in k_g.f

; For SO2 in strat:
nminor = [0,1]
neta = [9,5]
molec_arr_a = ' '
molec_arr_b = 'SO2'
temp_index_a = 0

spawn,'ls kg_minor*PL*.f',minor_files

for im=0,n_elements(minor_files)-1 do begin
   spawn,'cp '+minor_files[im]+' kg_minor.f'
   id_u = strpos(minor_files[im],'_',/reverse_search)
   id_p = strpos(minor_files[im],'.',/reverse_search)
   temp_index_b = fix(strmid(minor_files[im],id_u+1,id_p-id_u))+13

   convert_minor,nminor,neta,temp_index_a,temp_index_b,molec_arr_a,molec_arr_b

   nlen = id_p-9
   tag = strmid(minor_files[im],9,nlen)
   spawn,'cp k_minor.txt k_minor.'+tag+'.txt'
endfor

end
   



