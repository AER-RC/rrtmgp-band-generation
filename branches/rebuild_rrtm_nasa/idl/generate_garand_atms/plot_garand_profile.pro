PRO plot_garand_profile,x,y,xtitle,ptitle,ymin=ymin,ymax=ymax,xmin=xmin,xmax=xmax,xlog=xlog,$
  savefile=savefile,appendplot=appendplot,closeplot=closeplot

  if (not keyword_set(appendplot)) then appendplot = 0
  if (not keyword_set(closeplot)) then closeplot = 0
; erase graphics screen
;pwin = getwindows() 
;if (n_elements(pwin) ne 0) then foreach window, getwindows() do window.close

w = Window(WINDOW_TITLE=xtitle) 
mycolors=['Dark Blue','Slate Blue','Dodger Blue','Medium Aquamarine','Cyan','Sea Green','Lime Green','Deep Pink','Orchid','Firebrick','Red']

ip=0
for ipanel=0,3 do begin
  graph = plot(x(*,0),y(*,0),/nodata,$
  	xrange = [xmin,xmax],xstyle=1,xtitle=xtitle, $
  	yrange = [ymin,ymax],ystyle=1,ytitle='Pressure [mb]', $
    layout=[2,2,ipanel+1],/CURRENT,$
    /YLOG,xlog=xlog,margin=0.15,$
    ;position=[40,250,275,475],/device,$
    font_size=6,color='black')
  astr=strcompress(indgen(11)+ipanel*11+1,/remove_all)
  iprof=ip
  ograph0 = plot(x(*,iprof),y(*,iprof),/overplot,/current,/YLOG,color=mycolors(0),thick=2,name=astr(0))
  iprof=iprof+1
  ograph1 = plot(x(*,iprof),y(*,iprof),/overplot,/current,/YLOG,color=mycolors(1),thick=2,name=astr(1))
  iprof=iprof+1
  ograph2 = plot(x(*,iprof),y(*,iprof),/overplot,/current,/YLOG,color=mycolors(2),thick=2,name=astr(2))
  iprof=iprof+1
  ograph3 = plot(x(*,iprof),y(*,iprof),/overplot,/current,/YLOG,color=mycolors(3),thick=2,name=astr(3))
  iprof=iprof+1
  ograph4 = plot(x(*,iprof),y(*,iprof),/overplot,/current,/YLOG,color=mycolors(4),thick=2,name=astr(4))
  iprof=iprof+1
  ograph5 = plot(x(*,iprof),y(*,iprof),/overplot,/current,/YLOG,color=mycolors(5),thick=2,name=astr(5))
  iprof=iprof+1
  ograph6 = plot(x(*,iprof),y(*,iprof),/overplot,/current,/YLOG,color=mycolors(6),thick=2,name=astr(6))
  iprof=iprof+1
  ograph7 = plot(x(*,iprof),y(*,iprof),/overplot,/current,/YLOG,color=mycolors(7),thick=2,name=astr(7))
  iprof=iprof+1
  ograph8 = plot(x(*,iprof),y(*,iprof),/overplot,/current,/YLOG,color=mycolors(8),thick=2,name=astr(8))

  if (ipanel ne 3) then begin
    iprof=iprof+1
    ograph9 = plot(x(*,iprof),y(*,iprof),/overplot,/current,/YLOG,color=mycolors(9),thick=2,name=astr(9))
    iprof=iprof+1
    ograph10 = plot(x(*,iprof),y(*,iprof),/overplot,/current,/YLOG,color=mycolors(9),thick=2,name=astr(10))
  endif
  ip = ip+11
  case ipanel of
    0: begin
      legpos=[300,460]
      targets=[ograph0,ograph1,ograph2,ograph3,ograph4,ograph5,ograph6,ograph7,ograph8,ograph9,ograph10]
    end
    1: begin
      legpos=[625,460]
      targets=[ograph0,ograph1,ograph2,ograph3,ograph4,ograph5,ograph6,ograph7,ograph8,ograph9,ograph10]      
    end
    2: begin
        legpos=[300,205]
        targets=[ograph0,ograph1,ograph2,ograph3,ograph4,ograph5,ograph6,ograph7,ograph8,ograph9,ograph10]        
      end
    3: begin
        legpos=[625,205]
        targets=[ograph0,ograph1,ograph2,ograph3,ograph4,ograph5,ograph6,ograph7,ograph8]        
      end
  endcase

  leg = LEGEND(target=targets,font_size=6,$
      position=legpos,/device,$
      sample_width=0.05,shadow=0,vertical_spacing=0.01)  


endfor
t1 = text([0.5],[0.95],ptitle,/normal,alignment=0.5)
if (n_elements(savefile) eq 1) then graph.save,savefile,append=appendplot,close=closeplot
end