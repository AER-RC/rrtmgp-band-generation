; Plot profiles
pro plot_profile,fnamein

restore,filename=fnamein

; Constants for conversions
alosmt = 2.6867775e19
pzero = 1013.25
tzero = 273.15

; Pressure Scale
ymin = 1200.0
ymax = 0.1

; Temperature
xmin = 190.0
xmax = 325.0
xtitle='Temperature (K)'
ptitle='Garand Temperature Profiles'
x=tprofg.t
y=tprofg.p
plot_garand_profile,x,y,xtitle,ptitle,ymin=ymin,ymax=ymax,xmin=xmin,xmax=xmax,xlog=0,savefile='Garand_Temperature_Profiles.pdf'

; WV - PPMV
xmin = 1
xmax = 1.e5
xtitle='WV (PPMV)'
ptitle='Garand Water Vapor Profiles'
x=tprofg.wv
y=tprofg.p
plot_garand_profile,x,y,xtitle,ptitle,ymin=ymin,ymax=ymax,xmin=xmin,xmax=xmax,/xlog,savefile='Garand_WV_Profiles.pdf'

; WV - NUMBER DENSITY
rhoair = alosmt*(tprofg.p/pzero)*(tzero/tprofg.t)
wv = tprofg.wv*1.e-6
wv = (wv/(1.+wv))*rhoair
xmin = 1.e10
xmax = 1.e18
xtitle ='WV (cm!u-3!n)'
ptitle='Garand Water Vapor Profiles'
plot_garand_profile,wv,y,xtitle,ptitle,ymin=ymin,ymax=ymax,xmin=xmin,xmax=xmax,/xlog,savefile='Garand_WVNUMDEN_Profiles.pdf'

; CO2 - PPMV
xmin = 100.
xmax = 3000.
xtitle='CO2 (PPMV)'
ptitle='Garand CO2 Profiles'
x=tprofg.co2
y=tprofg.p
plot_garand_profile,x,y,xtitle,ptitle,ymin=ymin,ymax=ymax,xmin=xmin,xmax=xmax,/xlog,savefile='Garand_CO2_Profiles.pdf',appendplot=1,closeplot=0

; CO2 - NUMBER DENSITY
rhoair = alosmt*(tprofg.p/pzero)*(tzero/tprofg.t)
wv = tprofg.wv*1.e-6
wv = (wv/(1.+wv))*rhoair
dryair = rhoair - wv
co2 = tprofg.co2*dryair*1.e-6
xmin = 1.e12
xmax = 1.e17
xtitle ='CO2 (cm!u-3!n)'
ptitle='Garand CO2 Profiles'
plot_garand_profile,co2,y,xtitle,ptitle,ymin=ymin,ymax=ymax,xmin=xmin,xmax=xmax,/xlog,savefile='Garand_CO2_Profiles.pdf',appendplot=1,closeplot=1

; O3 - PPMV
xmin = 0.001
xmax = 1.e2
xtitle='O3 (PPMV)'
ptitle='Garand Ozone Profiles'
x=tprofg.o3
y=tprofg.p
plot_garand_profile,x,y,xtitle,ptitle,ymin=ymin,ymax=ymax,xmin=xmin,xmax=xmax,/xlog,savefile='Garand_O3_Profiles.pdf',appendplot=1,closeplot=0


; O3 - NUMBER DENSITY
rhoair = alosmt*(tprofg.p/pzero)*(tzero/tprofg.t)
wv = tprofg.wv*1.e-6
wv = (wv/(1.+wv))*rhoair
dryair = rhoair - wv
o3 = tprofg.o3*dryair*1.e-6
xmin = 1.e8
xmax = 1.e13
xtitle ='O3 (cm!u-3!n)'
ptitle='Garand Ozone Profiles'
plot_garand_profile,o3,y,xtitle,ptitle,ymin=ymin,ymax=ymax,xmin=xmin,xmax=xmax,/xlog,savefile='Garand_O3_Profiles.pdf',appendplot=1,closeplot=1

; CH4 - PPMV
xmin = 0.1
xmax = 2.
xtitle='CH4 (PPMV)'
ptitle='Garand Methane Profiles'
x=tprofg.ch4
y=tprofg.p
plot_garand_profile,x,y,xtitle,ptitle,ymin=ymin,ymax=ymax,xmin=xmin,xmax=xmax,/xlog,savefile='Garand_CH4_Profiles.pdf',appendplot=1,closeplot=0

; CH4 - NUMBER DENSITY
rhoair = alosmt*(tprofg.p/pzero)*(tzero/tprofg.t)
wv = tprofg.wv*1.e-6
wv = (wv/(1.+wv))*rhoair
dryair = rhoair - wv
ch4 = tprofg.ch4*dryair*1.e-6
xmin = 1.e7
xmax = 1.e14
xtitle ='CH4 (cm!u-3!n)'
ptitle='Garand Methane Profiles'
plot_garand_profile,ch4,y,xtitle,ptitle,ymin=ymin,ymax=ymax,xmin=xmin,xmax=xmax,/xlog,savefile='Garand_CH4_Profiles.pdf',appendplot=1,closeplot=1

; CO - PPMV
xmin = 0.01
xmax = 1.e1
xtitle='CO (PPMV)'
ptitle='Garand CO Profiles'
x=tprofg.co
y=tprofg.p
plot_garand_profile,x,y,xtitle,ptitle,ymin=ymin,ymax=ymax,xmin=xmin,xmax=xmax,/xlog,savefile='Garand_CO_Profiles.pdf',appendplot=1,closeplot=0

; CO - NUMBER DENSITY
rhoair = alosmt*(tprofg.p/pzero)*(tzero/tprofg.t)
wv = tprofg.wv*1.e-6
wv = (wv/(1.+wv))*rhoair
dryair = rhoair - wv
co = tprofg.co*dryair*1.e-6
xmin = 1.e7
xmax = 1.e13
xtitle='CO (cm!u-3!n)'
ptitle='Garand CO Profiles'
plot_garand_profile,co,y,xtitle,ptitle,ymin=ymin,ymax=ymax,xmin=xmin,xmax=xmax,/xlog,savefile='Garand_CO_Profiles.pdf',appendplot=1,closeplot=1

; N2O - PPMV
xmin = 1.e-4
xmax = 1.e1
xtitle='N2O (PPMV)'
ptitle='Garand N2O Profiles'
x=tprofg.n2o
y=tprofg.p
plot_garand_profile,x,y,xtitle,ptitle,ymin=ymin,ymax=ymax,xmin=xmin,xmax=xmax,/xlog,savefile='Garand_N2O_Profiles.pdf',appendplot=1,closeplot=0

; N2O - NUMBER DENSITY
rhoair = alosmt*(tprofg.p/pzero)*(tzero/tprofg.t)
wv = tprofg.wv*1.e-6
wv = (wv/(1.+wv))*rhoair
dryair = rhoair - wv
n2o = tprofg.n2o*dryair*1.e-6
xmin = 1.e5
xmax = 1.e13
xtitle='N2O (cm!u-3!n)'
ptitle='Garand N2O Profiles'
plot_garand_profile,n2o,y,xtitle,ptitle,ymin=ymin,ymax=ymax,xmin=xmin,xmax=xmax,/xlog,savefile='Garand_N2O_Profiles.pdf',appendplot=1,closeplot=1

; O2 - PPMV
xmin = 1.e5
xmax = 1.e6
xtitle='O2 (PPMV)'
ptitle='Garand O2 Profiles'
x=tprofg.o2
y=tprofg.p
plot_garand_profile,x,y,xtitle,ptitle,ymin=ymin,ymax=ymax,xmin=xmin,xmax=xmax,/xlog,savefile='Garand_O2_Profiles.pdf',appendplot=1,closeplot=0

; O2 - NUMBER DENSITY
rhoair = alosmt*(tprofg.p/pzero)*(tzero/tprofg.t)
wv = tprofg.wv*1.e-6
wv = (wv/(1.+wv))*rhoair
dryair = rhoair - wv
o2 = tprofg.n2o*dryair*1.e-6
xmin = 1.e5
xmax = 1.e13
xtitle='O2 (cm!u-3!n)'
ptitle='Garand O2 Profiles'
plot_garand_profile,o2,y,xtitle,ptitle,ymin=ymin,ymax=ymax,xmin=xmin,xmax=xmax,/xlog,savefile='Garand_O2_Profiles.pdf',appendplot=1,closeplot=1

end



