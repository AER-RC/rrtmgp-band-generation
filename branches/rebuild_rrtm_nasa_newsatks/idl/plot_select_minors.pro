;MAIN PROGRAM

; Read in upwelling and downward fluxes from LBLRTM runs with and without minors and 
; ARRTM runs with and without minors for each
; level and each atmosphere; plot ARRTM(minors - nominors) -LBLRTM (minors - nominors) difference 
; and  decide which level  does the best job

;11/21
;datadir = '/stormrc3/cadyp/test_gb3'
;band_name = 'Band 3 : 500-630 cm-1 : Key species + LOWER N!D2!NO'
;base_str = '_MINORS_LOWER_4_PL_1'
;case_str = '_MINORS_UPPER_4'
;xfactor = 1.

;11/8
;datadir = '/stormrc3/cadyp/test_gb6'
;band_name = 'Band 6 : 820-980 cm-1 : All species'
;only_key_case = 'LOWER_PL_5'
;case_str = '_NOT_ALL_MINORS_LOWER'
;xfactor = 2.

;11/27
;datadir = '/stormrc3/cadyp/test_gb7_new'
;band_name = 'Band 7 : 980-1080 cm-1 : Key Species + Lower CO!D2!N'
;base_str = '_MINORS_LOWER_2_PL_3'
;case_str = '_MINORS_UPPER_2'
;xfactor = .1

;11/27
;datadir = '/stormrc3/cadyp/test_gb8'
;band_name = 'Band 8 : 1080-1180 cm-1 : Key Species plus CO!D2!N, N!D2!NO and O!D3!N '
;base_str = '_MINORS_UPPER_4_PL_36'
;case_str = '_MINORS_LOWER_3'
;factor = .25

;11/8
;datadir = '/stormrc3/cadyp/test_gb9'
;band_name = 'Band 9 : 1180-1390 cm-1 : All species'
;only_key_case = 'UPPER_PL_41'
;case_str = '_ALL_MINORS_UPPER'
;xfactor = 2.

;12/4
;datadir = '/stormrc3/cadyp/test_gb11'
;band_name = 'Band 11 : 1480-1800 cm-1 : All species'
;only_key_case = 'UPPER_PL_1'
;case_str = '_ALL_MINORS_UPPER'
;xfactor = 0.2

;11/30
;datadir = '/stormrc3/cadyp/test_gb12'
;band_name = 'Band 12 : 1800-2080 cm-1 : All species'
;only_key_case = 'LOWER_PL_9'
;case_str = '_NOT_ALL_MINORS_LOWER'
;xfactor = 2.

;12/22
;datadir = '/stormrc3/jdelamer/flrt_jen/working_model/test_gb13_new'
;band_name = 'Band 13 : 2080-2250 cm-1 : All species'
;base_str = '_ONLY_KEY_UPPER_PL_16'
;case_str = '_MINORS_UPPER_3'
;xfactor = .3

;11/27
;datadir = '/stormrc3/cadyp/test_gb15'
;band_name = 'Band 15 : 2380-2600 cm-1 : All species'
;only_key_case = 'LOWER_PL_1'
;case_str = '_NOT_ALL_MINORS_LOWER'
;xfactor = 2.


;06/20/14
datadir = '//project/p1905'+['/band_build/rrtm_lw_so2_kaust/test_band3','/radsum/','/rrtm_lw_no_so2/','/radsum/']
band_name = 'band3'
case_str = '.UPPER_9'
xfactor = [8.0,8.0,8.0,1.0]
delta_flag = [0,0,0,0]

;06/20/14
datadir = '//project/p1905'+['/band_build/rrtm_lw_so2_kaust/test_band3','/radsum/','/rrtm_lw_no_so2/','/radsum/']
band_name = 'band3'
case_str = '.UPPER_9'
xfactor = [10.0,10.0,10.0,1.0]
delta_flag = [1,1,1,1]



nstr = strlen(case_str)
atmos_level = strmid(case_str,1,5)

set_plot,'PS'
device,filename='compare_plots_minor.ps',/color,/landscape

print, 'atmos_level = ',atmos_level

if (atmos_level eq 'LOWER' ) then begin
   nlev = 6
   ending = ['PL_1','PL_3','PL_5','PL_7','PL_9','PL_11']
   cores = [2, 3, 4, 6, 9, 11]
endif else if (atmos_level eq 'UPPER' ) then begin
   nlev = 9
   ending = ['PL_1','PL_6','PL_11','PL_16','PL_21','PL_26', 'PL_31', $
	      'PL_36','PL_41']
   cores = findgen(9)+2
endif

plot_out = 'PS'

bhohls
!P.multi=[0,3,1]
!p.thick = 3
!p.charsize = 1.25
print,ending
max_diff = fltarr(n_elements(ending))
minor_flag = 1

for iv = 0,3 do  begin
   plot_rrtm_lbl_diff, iv, datadir, band_name, case_str, ending, nlev, $
                        cores, plot_out, xfactor, minor_flag, delta_flag(iv), $
                        max_diff
   if (iv eq 1 ) then begin
      print, max_diff
      print, min(max_diff, imin), imin
      print, "Smallest error due to level selection for Planck sorting occurs for level ", $
	       ending(imin)
   endif
endfor
device,/close
end
