ib='5'
restore,'Band'+ib+'_Stats.dat'
file_mkdir,'lbl_gb'+ib+'_transplots'
nlay = n_elements(pave)

for ig=0,15 do begin
  plmax=max([transdiffone(*,ig,*),transdifftwo(*,ig,*)])
  aplot = plot([0.],[0.],/nodata,$
  ytitle='pressure',yrange=[1050.,0.1],/ylog,$
  title='IG='+strcompress(ig+1,/remove_all),$
  xtitle='t-t_ref',xrange=[0.,plmax])
  
  oaplot = plot([transdiffone(*,ig,0)],[pave],NAME='tau(ave),sf=0.3',color='red',symbol='circle',sym_size=1.0,/overplot)
  obplot = plot([transdifftwo(*,ig,0)],[pave],NAME='trans(ave),sf=0.3',color='blue',symbol='circle',sym_size=1.0,/overplot)

  ocplot = plot([transdiffone(*,ig,1)],[pave],NAME='tau(ave),sf=1.0',color='red',symbol='triangle',sym_size=1.0,/overplot)
  odplot = plot([transdifftwo(*,ig,1)],[pave],NAME='trans(ave),sf=1.0',color='blue',symbol='triangle',sym_size=1.0,/overplot)

  oeplot = plot([transdiffone(*,ig,2)],[pave],NAME='tau(ave),sf=3.0',color='red',symbol='plus',sym_size=1.0,/overplot)
  ofplot = plot([transdifftwo(*,ig,2)],[pave],NAME='trans(ave),sf=3.0',color='blue',symbol='plus',sym_size=1.0,/overplot)
  leg=legend(target=[oaplot,obplot,ocplot,odplot,oeplot,ofplot],position=[0.9,0.85],/normal)

  aplot.save,'lbl_gb'+ib+'_transplots'+'/Band'+ib+'_IG_'+strcompress(ig+1,/remove_all)+'.pdf'

endfor

end