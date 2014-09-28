pro plot_rrtm_lbl_diff, var_index, datadir, band_name, case_str, ending, nlev, $
                        cores, plot_out, xfactor, minor_flag, delta_flag,  max_diff

; read in and plot RRTM-LBLRTM difference for one variable

band_id = strmid(band_name,4)
band_num = fix(band_id)


plot_names = ['up','down','net','cool']
var_units = [replicate('(W/m!e2!n)',3),'(K/d)']
var_names = ['Upwelling', 'Downwelling', 'Net Flux', 'Cooling Rate']

diff_type = ['RRTM-LBLRTM ', '!4D!3RRTM-!4D!3LBLRTM ']

natm = 6
files_arrtm = strarr(natm,nlev)
files_lbl = strarr(natm)

if (delta_flag eq 1) then begin
   files_arrtm_base = strarr(natm)
   files_lbl_base = strarr(natm)
endif

atmt = ['GARAND1.Pinatubo','GARAND1.Toba','GARAND5.Pinatubo','GARAND5.Toba','GARAND6.Pinatubo','GARAND6.Toba']

for j=0,natm-1 do begin
	for k = 0,n_elements(ending)-1 do begin
		files_arrtm(j,k) = datadir[0]+'/OUTPUT_RRTM.'+atmt(j)+'.'+band_name+case_str+'_'+ending(k)
	endfor

	files_lbl[j] = datadir[1]+atmt[j]+'/OUTPUT_RADSUM.'+band_name

        if (delta_flag eq 1) then begin
	   files_arrtm_base[j] = datadir[2]+'/OUTPUT_RRTM.'+strmid(atmt(j),0,7)+'.'+band_name
	   files_lbl_base[j] = datadir[3]+strmid(atmt[j],0,7)+'/OUTPUT_RADSUM.'+band_name
        endif
endfor


; set up plots 

;if (plot_out eq 'X') then begin
;   set_plot,'X'
;   window,var_index,retain=2 
;endif else begin
;   set_plot,'PS'
;   device,/color,/landscape,filename = plot_names(var_index)+'.ps'
;endelse

; read in and plot data

openw, unit1,'flux_forcing.'+var_names[var_index]+'.out',/get_lun

for j=0,natm-1 do begin
   read_radsum,files_lbl(j),radsum
   radsum_arr = reform(float(radsum))
   pmbl = radsum_arr[*,1]
   uflxl = radsum_arr[*,2]
   dflxl = radsum_arr[*,3]
   nflxl = radsum_arr[*,4]
   hrl = radsum_arr[*,5]

   if (delta_flag eq 1) then begin
      read_radsum,files_lbl_base(j),radsum
      radsum_arr = reform(float(radsum))
      pmbl_nom = radsum_arr[*,1]
      uflxl_nom = radsum_arr[*,2]
      dflxl_nom = radsum_arr[*,3]
      nflxl_nom = radsum_arr[*,4]
      hrl_nom = radsum_arr[*,5]
      strul = string([uflxl[0],uflxl_nom[0]],format='(2f10.3)')
      strdl = string([dflxl[16],dflxl_nom[16]],format='(2f10.3)')
      case var_index of
	 0: uflxl = uflxl - uflxl_nom
	 1: dflxl = dflxl - dflxl_nom
	 2: nflxl = nflxl - nflxl_nom
	 3: hrl = hrl - hrl_nom
      endcase
   endif

   read_rrtm,files_arrtm(j,0),rrtm,nbands=1   ;lev,pmb,uflx,dflx,nflx,hr
   pmb = reverse(rrtm.pres)
   uflx = reverse(rrtm.up)
   dflx = reverse(rrtm.down)
   nflx = reverse(rrtm.net)
   hr = reverse(rrtm.hr)

   if (delta_flag eq 1) then  begin
      read_rrtm,files_arrtm_base(j),rrtm,nbands=1
      pmb_base = reverse(rrtm.pres)
      uflx_base = reverse(rrtm.up)
      dflx_base = reverse(rrtm.down)
      nflx_base = reverse(rrtm.net)
      hr_base = reverse(rrtm.hr)
      strur = string([uflx[0],uflx_base[0]],format='(2f10.3)')
      strdr = string([dflx[16],dflx_base[16]],format='(2f10.3)')
      print ,'pmbl[16]', pmbl[16]
      if (var_index eq 0) then printf, unit1,atmt[j],strul,strur,format='(3a20)'
      if (var_index eq 1) then printf, unit1, atmt[j],strdl,strdr,format='(3a20)'
      case var_index of
	 0: uflx = uflx - uflx_base
	 1: dflx = dflx - dflx_base
	 2: nflx = nflx - nflx_base
	 3: hr = hr - hr_base
      endcase
   endif

; choosing a level over only either the lower or upper atmosphere is currently not
; being implemented

   if (case_str eq '_ONLY_KEY_LOWER') then $
       level_range = where (pmbl gt 100.) $
   else $
       level_range = where (pmbl lt 100.)
 
   level_range = findgen(61)
   case var_index of
      0: arr = uflx-uflxl
      1: arr = dflx-dflxl
      2: arr = nflx-nflxl
      3: arr = -(hr-hrl)
   endcase

   plot_io,arr,pmbl,$
	   yrange=[1013.,.1],ystyle=1,xrange=[-0.25,0.25]*xfactor[var_index],xstyle=1, $
	   /nodata,title=band_name+' - '+var_names(var_index),xtitle=diff_type(delta_flag)+var_units(var_index) ,$
           ytitle = "Pressure (mb)"
   oplot,[0,0],[1013.,.1]

;   plot,arr,pmbl,$
;   yrange=[1013.,.1],ystyle=1,xrange=[-0.25,0.25]*xfactor,xstyle=1, $
;	   /nodata,title=atmt(j),xtitle=diff_type(delta_flag)+var_units(var_index) ,$
;           ytitle = "Pressure (mb)"
;   oplot,[0,0],[1013.,.1]

   max_diff[*] = 0.0
   for i=0,n_elements(ending)-1 do begin
	   read_rrtm,files_arrtm(j,i),rrtm,nbands=1
	    pmb = reverse(rrtm.pres)
	    uflx = reverse(rrtm.up)
	    dflx = reverse(rrtm.down)
	    nflx = reverse(rrtm.net)
	    hr = reverse(rrtm.hr)
           if (delta_flag eq 0) then begin
	       case var_index of
		  0: arr = uflx-uflxl
		  1: arr = dflx-dflxl
		  2: arr = nflx-nflxl
		  3: arr = -(hr-hrl)
	       endcase
           endif
           if (delta_flag eq 1) then begin
	       case var_index of
		  0: arr = uflx-uflx_base-uflxl
		  1: arr = dflx-dflx_base-dflxl
		  2: arr = nflx-nflx_base-nflxl
		  3: arr = hr-hr_base-hrl
	       endcase
           endif
            
	   oplot,arr,pmbl,color = cores[i]

           diff = abs(arr(level_range))
           max_error = max(diff,imax)
           max_diff(i) = max_diff(i) > max_error

           lev_max = pmbl(imax+level_range(0))
           if (lev_max lt .1) then lev_max = .1
           max_error = arr(imax+level_range(0))
           plots,[max_error, max_error],[lev_max, lev_max], psym=2, color=cores[i]
   endfor
   x0 = !x.crange[0]+0.8*(!x.crange[1]-!x.crange[0])
   y0 = !y.crange[0]-.92*(!y.crange[0]-!y.crange[1])
   y0 = 10.0^y0
   ;xyouts, .5, 1.05, band_name+' - '+var_names(var_index)+' Difference', alignment = .5, /normal
   xyouts,x0,y0, atmt(j), alignment = .5,charsize=0.7
   legend_old,[ending],textcolor=cores
endfor


;if (plot_out eq 'PS') then device,/close

free_lun, unit1

end
