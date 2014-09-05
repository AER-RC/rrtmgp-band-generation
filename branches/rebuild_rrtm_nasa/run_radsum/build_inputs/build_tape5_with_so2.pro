; Code reads in selected  GARAND profiles, then creates two new versions, by inserting Pinatubo and Toba OS2
; profiles

case_id = ['Toba','Pinatubo']
ncases = n_elements(case_id)

prof_id = 'GARAND'+['1','5','6']
nprofs = n_elements(prof_id)

so2_index = 8

; get US std atmosphere SO2
read_lblrtm_profiles,'usa_std_atm','SO2',molec_arr_prof=molec_arr_prof,pres=pres,temp=temp,alt=alt,$
          molec_wgts_i=molec_wgts_i,molec_wgt_air=molec_wgt_air
so2_prof = molec_arr_prof[so2_index,*]

for ic=0,ncases-1 do begin
   case ic of 
   0: begin ; Toba
    p1 = 100.
    p2 = 1
    new_so2 = 4.0 ; (mg/kg)
   end
   1: begin ;  Pinatubo
    p1 = 75.
    p2 = 25
    new_so2 = 0.16 ; (mg/kg)
   end
   endcase

; modify SO2 
   new_so2_vmr = new_so2*(molec_wgt_air/molec_wgts_i) ; (from mmr to ppmv)
   index = where(pres le p1 and pres ge p2)
   so2_prof[index] = new_so2_vmr
   
; insert into Garand profiles
   for ip=0,nprofs-1 do begin
      tape5_insert_no_file,'TAPE5.'+prof_id[ip],i_insert_mol=so2_index+1,pres,so2_prof, $
                            i_rec_3_1=5,tape5_new= 'TAPE5.'+prof_id[ip]+'.'+case_id[ic]
   endfor
endfor
end
