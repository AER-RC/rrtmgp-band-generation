pro plot_radsum_multi_by_case, radsum_path1,radsum_file,radsum_path2,case_name
; plot  band fluxes (up and down) and heating rates from RADSUM runs from LBLRTM run with different profiles (SO2 on and
;off; difference plot is in effect a forcing plot)

read_radsum,radsum_path1+radsum_file,radsum_arr1,v1=v1,v2=v2
radsum_arr1 = reform(float(radsum_arr1))

read_radsum,radsum_path2+radsum_file,radsum_arr2,v1=v1,v2=v2
radsum_arr2 = reform(float(radsum_arr2))

nvars = 3
var_names = ['Upward Flux','Downward Flux', 'Heating Rate']
idir = [2,3,5]
ip_col = 1

band_names = ['10-350','350-500','500-630','630-700','700-820','820-980','980-1080','1080-1180','1180-1390', $
              '1390-1480','1480-1800','1800-2080','2080-2250','2250-2380','2380-2600','2600-3250']
band_id = ['band1','band2','band3','band4','band5','band6','band7','band8','band9','band10','band11','band12','band13','band14','band15','band16']

plot_type = 'PS'
if (plot_type eq 'PS') then begin
   set_plot,'PS'
   device,/color,/portrait
   device, xoffset=1, yoffset=1,xsize=20,ysize=26
   device,/Helvetica,/Bold
   !p.font = 0
endif

!p.multi = [0,2,3]
bhohls

!x.thick = 3
!y.thick = 3
!p.thick = 5
!p.charsize = 1.2

max_press = max(radsum_arr1[*,ip_col])
case 1 of
max_press gt 100.: ymax = 100.*fix(max_press/100.)+100.
(max_press gt 10. AND max_press lt 100.) : ymax = 10.*fix(max_press/10.)+10.
else: ymax = 10.
endcase

!y.style=1
!y.range = [ymax,0.1]
!y.title = 'Pressure (mbar) '

parr = fltarr(4,6)
parr[*,0] = [0.07,0.69,0.51,0.95]
parr[*,1] = [0.55,0.69,0.99,0.95]
parr[*,2] = [0.07,0.38,0.51,0.64]
parr[*,3] = [0.55,0.38,0.99,0.64]
parr[*,4] = [0.07,0.05,0.51,0.30]
parr[*,5] = [0.55,0.05,0.99,0.30]

; determine title
newstr = strsplit(radsum_file,'.',/extract)
id = where(band_id eq newstr[1])
device,filename='plots_'+case_name+'/band_'+band_names[id]+'.ps'


for iv=0,nvars-1 do begin
   !p.position = parr[*,2*iv]
   case iv of 
      0: !x.title = ' '
      1: !x.title = 'Flux (W/m!E2!N)'
      2: !x.title = 'Heating Rate (K/day)' 
   endcase
   !y.title = 'Pressure (mbar) '

   minx = min(radsum_arr1[*,idir[iv]]) < min(radsum_arr2[*,idir[iv]])
   maxx = max(radsum_arr1[*,idir[iv]]) > max(radsum_arr2[*,idir[iv]])
   
   !x.range = [minx,maxx]
   ;minx = get_round_val(minx)
   ;maxx = get_round_val(maxx,/up_dn_flag)

   plot,radsum_arr1[*,idir[iv]],radsum_arr1[*,ip_col],/nodata,$
        /ylog,title = var_names[iv]
   oplot,radsum_arr1[*,idir[iv]],radsum_arr1[*,ip_col],color=2
   oplot,radsum_arr2[*,idir[iv]],radsum_arr2[*,ip_col],color=10
   if (iv eq 2) then legend_old,['Volcanic','Baseline'],lines=[0,0],color=[2,10],/bottom,/left,charsize=0.8

   if (iv eq 0) then xyouts,0.5,0.97,case_name+' - Band: '+band_names[id],alignment=0.5,/normal

   !p.position = parr[*,2*iv+1]
   case iv of 
      0: !x.title = 'Volcanic-Baseline (W/m!E2!N)'
      1: !x.title = 'Volcanic-Baseline (W/m!E2!N)'
      2: !x.title = 'Volcanic-Baseline (K/day)' 
   endcase
   !y.title = ' '
    !x.range = 0.
   plot,radsum_arr1[*,idir[iv]]-radsum_arr2[*,idir[iv]],radsum_arr1[*,ip_col],$
        /ylog,title = var_names[iv]
   plots,[0.0,0.0],[!y.crange[0],!y.crange[1]],lines=1

endfor

;date_stamp
if (plot_type eq 'PS') then device,/close_file
return
end
