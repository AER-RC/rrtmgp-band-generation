; This code places a standard SO2 atmosphere equal to 4xPinatubo 
; expande to cover the 100 to 1 mbar range into data statements for std_atmos.f
;  Along the way it builds data statements for NO from the US std atmosphere.

; get Pinatubo SO2 and scale
tape5 = '/project/p1905/build_inputs/TAPE5.GARAND6.exp.Pinatubo'
tape5_read,tape5,zm,pm,tm,vmol,i_rec_3_1=6
so2_in = 4.0*vmol[8,*]

; get USstd NO
read_lblrtm_profiles,'usa_std_atm','NO',molec_i_prof=no_in,pres=pres_us

; interpolate both to MLS
read_lblrtm_profiles,'mid_lat_sum','H2O',pres=pres_mls
so2_out = interpol(so2_in,pm,pres_mls)
no_out = interpol(no_in,pres_us,pres_mls)

; set levels above the highest level provided to the value at the highest level
pndex = where(pres_mls lt min(pm,pd))
undex = where(pres_mls lt min(pres_us,ud))
so2_out[pndex] = so2_in[pd]
no_out[undex] = no_in[ud]

; create data statements for each 
data_str = ['       DATA AMOL(8,:) /','       DATA AMOL(9,:) /']
line_hdr = '     *          '

openw,unit1,'build_so2_std.out',/get_lun
for imolec =0,1 do begin
   printf, unit1,data_str[imolec]
   case imolec of
      0: molec_out = no_out
      1: molec_out = so2_out
   endcase
   for ip=0,n_elements(pres_mls)-6,5 do begin
      index = indgen(5)+ip
      printf, unit1,format='(a16,5(e10.4,","))',line_hdr,molec_out[index]
   endfor
   index = indgen(5)+ip
   printf, unit1,format='(a16,4(e10.4,","),e10.4,"/")',line_hdr,molec_out[index]
endfor

free_lun, unit1

end


