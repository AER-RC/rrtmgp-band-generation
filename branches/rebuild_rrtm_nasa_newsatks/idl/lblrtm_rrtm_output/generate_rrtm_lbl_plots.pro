PRO generate_rrtm_lbl_plots,rrtm_dir,lbl_dir,bandnum,plid

  plfilename='STATS_'+plid+'_'+strcompress(bandnum,/remove_all)+'.pdf'
  get_lbl_rrtm_data,rrtm_dir,lbl_dir,rrtm,lbl,BANDNUM=bandnum
  
  pltitle = 'Garand Atmospheres, '+rrtm(0).band
  plot_rrtm_lbl_stats,rrtm,lbl,pltitle,plid,plfilename

; loop through profiles
  pdir=rrtm_dir+'/plots'
  print,n_elements(rrtm)
  for iprof=0,n_elements(rrtm)-1 do begin
    plot_lbl_rrtm_profiles,rrtm(iprof),lbl(iprof),rrtm(iprof).filename,plotdir=pdir
  endfor
  plfilestats = 'PROFS_'+plid+'_'+strcompress(bandnum,/remove_all)+'.pdf'
  spawn,'~/bin/multips2pdf.pl -pdf '+plfilestats+' '+pdir+'/*ps'

end


