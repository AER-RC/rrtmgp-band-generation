pro plot_lbl_rrtm_profiles,rtmp,ltmp,case_name,plotdir=plotdir
; plot  band fluxes (up and down) and heating rates from LBLRTM/RADSUM and RRTM and their differences

if (not keyword_set(plotdir)) then plotdir='.' 

rrtm_press = rtmp.pres
nlev_r = n_elements(rrtm_press)

rrtm_arr = fltarr(nlev_r,3)
rrtm_arr[*,0] = reform(rtmp.flux(0,*))
rrtm_arr[*,1] = reform(rtmp.flux(1,*))
rrtm_arr[*,2] = reform(rtmp.flux(3,*))


radsum_press = ltmp.pres
nlev_l = n_elements(ltmp.pres)
if (nlev_l ne nlev_r) then STOP,'PRESSURE LEVELS DO NOT MATCH'
radsum_arr = fltarr(nlev_l,3)
help,radsum_arr
radsum_arr[*,0] = reform(ltmp.flux(0,*))
radsum_arr[*,1] = reform(ltmp.flux(1,*))
radsum_arr[*,2] = reform(ltmp.flux(3,*))

print, 'surface rrtm ',rrtm_arr[0,0],rrtm_press[0]

nvars = 3
var_names = ['Upward Flux','Downward Flux', 'Heating Rate']

band_names = ['10-350','350-500','500-630','630-700','700-820','820-980','980-1080','1080-1180','1180-1330','1330-1390','1390-1480', $
              '1480-1800','1800-2080','2080-2250','2250-2380','2380-2600','2600-3250']
band_id = ['band1','band2','band3','band4','band5','band6','band7','band8','band9','band10','band11','band12','band13','band14','band15','band16','band17']

plot_type = 'PS'
if (plot_type eq 'PS') then begin
   set_plot,'PS'
   device,/color,/portrait
   device, xoffset=1, yoffset=1,xsize=20,ysize=26
   device,/Helvetica,/Bold
   device,filename=plotdir+'/'+case_name+'.ps'
   !p.font = 0
endif

!p.multi = [0,2,3]
bhohls

!x.thick = 3
!y.thick = 3
!p.thick = 5
!p.charsize = 1.2

max_press = max(rrtm_press)
case 1 of
max_press gt 100.: ymax = 100.*fix(max(rrtm_press)/100.)+100.
(max_press gt 10. AND max_press lt 100.) : ymax = 10.*fix(max(rrtm_press)/10.)+10.
else: ymax = 10.
endcase

!y.style=1
!y.range = [ymax,0.01]
!y.title = 'Pressure (mbar) '

parr = fltarr(4,6)
parr[*,0] = [0.07,0.69,0.51,0.95]
parr[*,1] = [0.55,0.69,0.99,0.95]
parr[*,2] = [0.07,0.38,0.51,0.64]
parr[*,3] = [0.55,0.38,0.99,0.64]
parr[*,4] = [0.07,0.05,0.51,0.30]
parr[*,5] = [0.55,0.05,0.99,0.30]

; determine title
newstr = strsplit(case_name,'_',/extract)
id = where(band_id eq 'band'+newstr[2])

for iv=0,nvars-1 do begin
   !p.position = parr[*,2*iv]
   case iv of 
      0: !x.title = ' '
      1: !x.title = 'Flux (W/m!E2!N)'
      2: !x.title = 'Heating Rate (K/day)' 
   endcase
   !y.title = 'Pressure (mbar) '

   minx = min(radsum_arr[*,iv]) < min(rrtm_arr[*,iv])
   maxx = max(radsum_arr[*,iv]) > max(rrtm_arr[*,iv])
   
   !x.range = [minx,maxx]

   plot,radsum_arr[*,iv],radsum_press,/nodata,$
        /ylog,title = var_names[iv]
   oplot,radsum_arr[*,iv],radsum_press,color=2
   oplot,rrtm_arr[*,iv],rrtm_press,color=10
;   if (iv eq 2) then legend_old,['LBLRTM','RRTM'],lines=[0,0],color=[2,10],/bottom,/left,charsize=0.8

   if (iv eq 0) then xyouts,0.5,0.97,case_name+' - Band: '+band_names[id],alignment=0.5,/normal

   !p.position = parr[*,2*iv+1]
   case iv of 
      0: !x.title = ' '
      1: !x.title = 'RRTM-LBLRTM Flux Difference (W/m!E2!N)'
      2: !x.title = 'RRTM-LBLRTM Heating Rate Difference (K/day)' 
   endcase
   !y.title = ' '
    !x.range = 0.
   plot,rrtm_arr[*,iv]-radsum_arr[*,iv],radsum_press,$
        /ylog,title = var_names[iv]
   plots,[0.0,0.0],[!y.crange[0],!y.crange[1]],lines=1

endfor

;date_stamp
if (plot_type eq 'PS') then device,/close_file
return
end
