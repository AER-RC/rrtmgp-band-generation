PRO plot_rrtm_lbl_stats,rflux,lflux,pltitle,plinfo,plfilename

nrprofs = n_elements(rflux)
nlprofs = n_elements(lflux)

nrlevs = n_elements(rflux(0).flux(0,*))
nllevs = n_elements(lflux(0).flux(0,*))

; nprofs
nprofs = n_elements(rflux)

; Upwelling flux
ntop = nrlevs-1

print,'HERE FOR CHECK',nrlevs,nllevs
help,rflux
help,lflux

; TOA Upwelling Flux
xvals = lflux.flux(0,ntop)
yvals = rflux.flux(0,ntop)-lflux.flux(0,ntop)
title = 'TOA Upwelling Flux'
ptoa = PLOT(xvals,yvals, TITLE=title, XTITLE = 'LBLRTM [W/m2]', YTITLE = 'RRTM-LBLRTM [W/m2]',/NODATA,layout=[3,2,1],FONT_SIZE=8)
for iprof = 0,nprofs-1 do begin
  t = text(xvals(iprof),yvals(iprof),strcompress(iprof+1,/remove_all),/DATA,target=ptoa)
  t.font_size = 6
endfor
p=ptoa.yrange
p1=p(0)
p2=p(1)
if (p1 lt 0.0) AND (p2 gt 0.0) then zline = plot([ptoa.xrange],[0,0],XRange=ptoa.xrange, YRange=ptoa.yrange, /Current,/Overplot,LINESTYLE=1)

; SFC Downwelling Flux
xvals = lflux.flux(1,0)
yvals = rflux.flux(1,0)-lflux.flux(1,0)
title = 'SFC Downwelling Flux'
psfc = PLOT(xvals,yvals, TITLE=title, XTITLE = 'LBLRTM [W/m2]', YTITLE = 'RRTM-LBLRTM [W/m2]',/NODATA,layout=[3,2,2],/CURRENT,FONT_SIZE=8)
for iprof= 0,nprofs-1 do begin
  t = text(xvals(iprof),yvals(iprof),strcompress(iprof+1,/remove_all),/DATA,target=psfc)
  t.font_size = 6
endfor
p=psfc.yrange
p1=p(0)
p2=p(1)
if (p1 lt 0.0) AND (p2 gt 0.0) then zline = plot([psfc.xrange],[0,0],XRange=psfc.xrange, YRange=psfc.yrange, /Current,/Overplot,LINESTYLE=1)

; Maximum net flux error
xvals=0.0
yvals=0.0
for iprof=0,nprofs-1 do begin
  ytmpd=rflux(iprof).flux(2,*)-lflux(iprof).flux(2,*)
  ytmp=max(abs(ytmpd),maxind)
  yvals=[yvals,abs(ytmpd(maxind))]
  xvals=[xvals,lflux(iprof).flux(2,maxind)]
endfor
xvals=xvals(1:*)
yvals=yvals(1:*)
title = 'Maximum Net Flux Error'
pnet = PLOT(xvals,yvals, TITLE=title, XTITLE = 'LBLRTM [W/m2]', YTITLE = 'RRTM-LBLRTM [W/m2]',/NODATA,layout=[3,2,3],/CURRENT,FONT_SIZE=8)
for iprof = 0,nprofs-1 do begin
  t = text(xvals(iprof),yvals(iprof),strcompress(iprof+1,/remove_all),/DATA,target=pnet)
  t.font_size = 6
endfor
p=pnet.yrange
p1=p(0)
p2=p(1)
if (p1 lt 0.0) AND (p2 gt 0.0) then zline = plot([pnet.xrange],[0,0],XRange=pnet.xrange, YRange=pnet.yrange, /Current,/Overplot,LINESTYLE=1)

; Maximum heating rate in Troposphere error
; Find tropospheric cutoff
itrp = where(rflux(0).pres(*) ge 95.,complement=istrat)
itrp = itrp(n_elements(itrp)-1)
xvals=0.0
yvals=0.0
for iprof=0,nprofs-1 do begin
  ytmpd=rflux(iprof).flux(3,0:itrp)-lflux(iprof).flux(3,0:itrp)
  ytmp=max(abs(ytmpd),maxind)
  yvals=[yvals,abs(ytmpd(maxind))]
  xvals=[xvals,lflux(iprof).flux(3,maxind)]
endfor
xvals=xvals(1:*)
yvals=yvals(1:*)
title = 'Maximum Tropospheric!CHR Error'
phrt = PLOT(xvals,yvals, TITLE=title, XTITLE = 'LBLRTM [K/day]', YTITLE = 'RRTM-LBLRTM [K/day]',/NODATA,layout=[3,2,4],/CURRENT,FONT_SIZE=8)
for iprof = 0,nprofs-1 do begin
  t = text(xvals(iprof),yvals(iprof),strcompress(iprof+1,/remove_all),/DATA,target=phrt)
  t.font_size = 6
endfor
p=phrt.yrange
p1=p(0)
p2=p(1)
if (p1 lt 0.0) AND (p2 gt 0.0) then zline = plot([phrt.xrange],[0,0],XRange=phrt.xrange, YRange=phrt.yrange, /Current,/Overplot,LINESTYLE=1)

; Maximum heating rate in Stratosphere error
; Find tropospheric cutoff
xvals=0.0
yvals=0.0
for iprof=0,nprofs-1 do begin
  ytmpd=rflux(iprof).flux(3,itrp+1:*)-lflux(iprof).flux(3,itrp+1:*)
  xtmpd=lflux(iprof).flux(3,itrp+1:*)
  ytmp=max(abs(ytmpd),maxind)
  yvals=[yvals,abs(ytmpd(maxind))]
  xvals=[xvals,xtmpd(maxind)]
endfor
xvals=xvals(1:*)
yvals=yvals(1:*)
title = 'Maximum Stratospheric!CHR Error'
phrs = PLOT(xvals,yvals, TITLE=title, XTITLE = 'LBLRTM [K/day]', YTITLE = 'RRTM-LBLRTM [K/day]',/NODATA,layout=[3,2,5],/CURRENT,FONT_SIZE=8)
for iprof = 0,nprofs-1 do begin
  t = text(xvals(iprof),yvals(iprof),strcompress(iprof+1,/remove_all),/DATA,target=phrs)
  t.font_size = 6
endfor
p=phrs.yrange
p1=p(0)
p2=p(1)
if (p1 lt 0.0) AND (p2 gt 0.0) then zline = plot([phrs.xrange],[0,0],XRange=phrs.xrange, YRange=phrs.yrange, /Current,/Overplot,LINESTYLE=1)

; Compute statistics and print
yvalup = 0.0
yvaldown = 0.0
yvalnet = 0.0
yvalhrt = 0.0
yvalhrs = 0.0

for iprof = 0,nprofs-1 do begin
  ytmpd = rflux(iprof).flux(0,*)-lflux(iprof).flux(0,*)
  yvalup = [yvalup,reform(ytmpd)]

  ytmpd = rflux(iprof).flux(1,*)-lflux(iprof).flux(1,*)
  yvaldown = [yvaldown,reform(ytmpd)]

  ytmpd = rflux(iprof).flux(2,*)-lflux(iprof).flux(2,*)
  yvalnet = [yvalnet,reform(ytmpd)]

  ytmpd = rflux(iprof).flux(3,0:itrp)-lflux(iprof).flux(3,0:itrp)
  yvalhrt = [yvalhrt,reform(ytmpd)]

  ytmpd = rflux(iprof).flux(3,itrp+1:*)-lflux(iprof).flux(3,itrp+1:*)
  yvalhrs = [yvalhrs,reform(ytmpd)]
endfor


yvalup = strcompress(rms(yvalup(1:*)),/remove_all)
yvaldown = strcompress(rms(yvaldown(1:*)),/remove_all)
yvalnet = strcompress(rms(yvalnet(1:*)),/remove_all)
yvalhrt = strcompress(rms(yvalhrt(1:*)),/remove_all)
yvalhrs = strcompress(rms(yvalhrs(1:*)),/remove_all)

;tmom = moment(yvalup(1:*),sdev=sdev)
;yvalup = strcompress(tmom(0),/remove_all)+','+strcompress(sdev,/remove_all)
;tmom = moment(yvaldown(1:*),sdev=sdev)
;yvaldown = strcompress(tmom(0),/remove_all)+','+strcompress(sdev,/remove_all)
;tmom = moment(yvalnet(1:*),sdev=sdev)
;yvalnet = strcompress(tmom(0),/remove_all)+','+strcompress(sdev,/remove_all)
;tmom = moment(yvalhrt(1:*),sdev=sdev)
;yvalhrt = strcompress(tmom(0),/remove_all)+','+strcompress(sdev,/remove_all)
;tmom = moment(yvalhrs(1:*),sdev=sdev)
;yvalhrs = strcompress(tmom(0),/remove_all)+','+strcompress(sdev,/remove_all)

;t = text(0.65,0.35,"Mean,SDev for all levels",/normal)
t = text(0.65,0.35,"RMS for all levels",/normal)
t.font_size = 10

t = text(0.65,0.30,"Upwelling Flux:!C"+yvalup,/normal)
t.font_size = 10

t = text(0.65,0.23,"Downwelling Flux:!C"+yvaldown,/normal)
t.font_size = 10

t = text(0.65,0.16,"Net Flux:!C"+yvalnet,/normal)
t.font_size = 10

t = text(0.65,0.09,"Tropospheric HR:!C"+yvalhrt,/normal)
t.font_size = 10

t = text(0.65,0.03,"Stratospheric HR:!C"+yvalhrs,/normal)
t.font_size = 10

; Plot title
t = text(0.5,0.975,pltitle,/normal,alignment=0.5)

; LBL/RRTM info
t = text(0.65,0.40,plinfo,/normal)
t.font_size = 10

; Filename 
t.save,plfilename
end



