; Plot profiles

restore,filename='garand_profile.dat'

; Pressure Scale
pymin = 1200.0
pymax = 0.1

; Temperature
txmin = 175.0
txmax = 300.0

; WV - PPMV
wvxmin = 1
wvxmax = 1.e5

; O3 
o3xmin = 0.01
o3xmax = 1.e2

set_plot,'ps

@color_table
lcolor = [0,2,3,4,6,7,8]

!P.font = 0
device,filename='garand_stdatm_ppmv.ps',/color,/palatino
device,xsize = 6.5,ysize=9.0,xoffset = 1.0,yoffset=1.0,/inches
!P.multi = [0,1,3]
iprof = 0
!P.thick = 4.0

plot_io,tprofg(iprof).t,tprofg(iprof).p, /nodata,$
	xrange = [txmin,txmax],xstyle=1,xtitle='Temperature [K]', $
	yrange = [pymin,pymax],ystyle=1,ytitle='Pressure [mb]', $
	charsize = 2.0,charthick = 2.0

for iprof = 0,5 do begin
	oplot,tprofg(iprof).t,tprofg(iprof).p,color=lcolor(iprof)
endfor
legend_orig,['TRP','MLS','MLW','SAS','SAW','US'],textcolors =[0,2,3,4,6,7],/top, $
	/right

iprof = 0
plot_oo,tprofg(iprof).wv,tprofg(iprof).p, /nodata,$
	xrange = [wvxmin,wvxmax],xstyle=1,xtitle='Water Vapor Concentration [PPMV]', $
	yrange = [pymin,pymax],ystyle=1,ytitle='Pressure [mb]', $
	charsize = 2.0,charthick = 2.

for iprof = 0,5 do begin
	oplot,tprofg(iprof).wv,tprofg(iprof).p,color=lcolor(iprof)
endfor

iprof = 0
plot_oo,tprofg(iprof).o3,tprofg(iprof).p, /nodata,$
	xrange = [o3xmin,o3xmax],xstyle=1,xtitle='Ozone Concentration [PPMV]', $
	yrange = [pymin,pymax],ystyle=1,ytitle='Pressure [mb]', $
	charsize = 2.0,charthick = 2.0

for iprof = 0,5 do begin
	oplot,tprofg(iprof).o3,tprofg(iprof).p,color=lcolor(iprof)
endfor

device,/close

; Absolute value plot
device,filename='garand_stdatm_numden.ps',/color,/palatino
device,xsize = 6.5,ysize=9.0,xoffset = 1.0,yoffset=1.0,/inches
!P.multi = [0,2,3]
iprof = 0
!P.thick = 4.0

!x.margin = [3.25,3.25]
!y.margin = [2,2]
; WV - PPMV
wvxmin = 1.e10
wvxmax = 1.e18

; O3 
o3xmin = 1.e9
o3xmax = 1.e13

; CH4
ch4xmin = 1.e8
ch4xmax = 1.e14
 
; N2o
n2oxmin = 1.e5
n2oxmax = 1.e13

; co
coxmin = 1.e7
coxmax = 1.e13

plot_io,tprofg(iprof).t,tprofg(iprof).p, /nodata,$
	xrange = [txmin,txmax],xstyle=1,xtitle='Temperature [K]', $
	yrange = [pymin,pymax],ystyle=1,ytitle='Pressure [mb]', $
	charsize = 2.0,charthick = 2.0

for iprof = 0,5 do begin
	oplot,tprofg(iprof).t,tprofg(iprof).p,color=lcolor(iprof)
endfor


iprof = 0
plot_oo,tprofg(iprof).wv,tprofg(iprof).p, /nodata,$
	xrange = [wvxmin,wvxmax],xstyle=1,xtitle='Water Vapor Number Density [cm!u-3!n]', $
	yrange = [pymin,pymax],ystyle=1, $
	charsize = 2.0,charthick = 2.

alosmt = 2.6867775e19
pzero = 1013.25
tzero = 273.15

for iprof = 0,5 do begin
	rhoair = alosmt*(tprofg(iprof).p/pzero)*(tzero/tprofg(iprof).t)
	wv = tprofg(iprof).wv*1.e-6
	wv = (wv/(1.+wv))*rhoair
	oplot,wv,tprofg(iprof).p,color=lcolor(iprof)
endfor
legend_orig,['TRP','MLS','MLW','SAS','SAW','US'],textcolors =[0,2,3,4,6,7],/top, $
	/right

iprof = 0
plot_oo,tprofg(iprof).o3,tprofg(iprof).p, /nodata,$
	xrange = [o3xmin,o3xmax],xstyle=1,xtitle='O!l3!n Number Density [cm!u-3!n]',$
	yrange = [pymin,pymax],ystyle=1,ytitle='Pressure [mb]', $
	charsize = 2.0,charthick = 2.0

for iprof = 0,5 do begin
	rhoair = alosmt*(tprofg(iprof).p/pzero)*(tzero/tprofg(iprof).t)
	wv = tprofg(iprof).wv*1.e-6
	wv = (wv/(1.+wv))*rhoair
	dryair = rhoair - wv
	o3 = tprofg(iprof).o3*dryair*1.e-6
	oplot,o3,tprofg(iprof).p,color=lcolor(iprof)
endfor

iprof = 0
plot_oo,tprofg(iprof).ch4,tprofg(iprof).p, /nodata,$
	xrange = [ch4xmin,ch4xmax],xstyle=1,xtitle='CH!l4!n Number Density [cm!u-3!n]',$
	yrange = [pymin,pymax],ystyle=1, $
	charsize = 2.0,charthick = 2.0

for iprof = 0,5 do begin
	rhoair = alosmt*(tprofg(iprof).p/pzero)*(tzero/tprofg(iprof).t)
	wv = tprofg(iprof).wv*1.e-6
	wv = (wv/(1.+wv))*rhoair
	dryair = rhoair - wv
	ch4 = tprofg(iprof).ch4*dryair*1.e-6
	oplot,ch4,tprofg(iprof).p,color=lcolor(iprof)
endfor

iprof = 0
plot_oo,tprofg(iprof).n2o,tprofg(iprof).p, /nodata,$
	xrange = [n2oxmin,n2oxmax],xstyle=1,xtitle='N!l2!nO Number Density [cm!u-3!n]',$
	yrange = [pymin,pymax],ystyle=1,ytitle='Pressure [mb]', $
	charsize = 2.0,charthick = 2.0

for iprof = 0,5 do begin
	rhoair = alosmt*(tprofg(iprof).p/pzero)*(tzero/tprofg(iprof).t)
	wv = tprofg(iprof).wv*1.e-6
	wv = (wv/(1.+wv))*rhoair
	dryair = rhoair - wv
	n2o = tprofg(iprof).n2o*dryair*1.e-6
	oplot,n2o,tprofg(iprof).p,color=lcolor(iprof)
endfor

iprof = 0
plot_oo,tprofg(iprof).co,tprofg(iprof).p, /nodata,$
	xrange = [coxmin,coxmax],xstyle=1,xtitle='CO Number Density [cm!u-3!n]',$
	yrange = [pymin,pymax],ystyle=1, $
	charsize = 2.0,charthick = 2.0

for iprof = 0,5 do begin
	rhoair = alosmt*(tprofg(iprof).p/pzero)*(tzero/tprofg(iprof).t)
	wv = tprofg(iprof).wv*1.e-6
	wv = (wv/(1.+wv))*rhoair
	dryair = rhoair - wv
	co = tprofg(iprof).co*dryair*1.e-6
	oplot,co,tprofg(iprof).p,color=lcolor(iprof)
endfor

device,/close
end