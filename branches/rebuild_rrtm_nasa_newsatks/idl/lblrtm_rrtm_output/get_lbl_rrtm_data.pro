PRO get_lbl_rrtm_data,rrtm_dir,lbl_dir,rrtm,lbl,BANDNUMBER = bandnum

  delvar, rrtm
  delvar,lbl
  
  ; Set up input
  if (not keyword_set(bandnum)) then bandnum = 17
  
  ; Get list of RRTM directories
  rfiles = file_search(rrtm_dir,'OUTPUT_RRTM.GARAND*')
  ; Sort the files
  nfiles = n_elements(rfiles)
  rtmp = 0
  
  for ifile=0,nfiles-1 do begin
    a = strsplit(rfiles(ifile),'_',COUNT=ncount)
    ra = strmid(rfiles(ifile),a(ncount-2))
    raa = strsplit(ra,'_')
    rtmp = [rtmp,fix(strmid(ra,0,raa(1)-1))]
  endfor
  rtmp = rtmp(1:*)
  rfiles = rfiles(sort(rtmp))
  
  ; Now extract....
  rbase = file_basename(rfiles)
  rext = strmid(rbase,12)
  
  if (bandnum eq 17) then begin
    for ifile = 0,nfiles-1 do begin
      read_rrtm,rfiles(ifile),rtmp,nbands=1
    endfor
  endif else begin
    lbl_dir = lbl_dir+'lbl_gb'+strcompress(bandnum,/remove_all)+'/'
    imark = 0
  
    for ifile = 0,nfiles-1 do begin
      lbl_file = lbl_dir+'OUTPUT_LBLRTM.'+rext(ifile) 
  
      if (file_test(lbl_file)) then begin
        print,'INTEST',rfiles(ifile)
        read_rrtm,rfiles(ifile),rtmp,nbands=1
        read_radsum,lbl_file,ltmp
        if (imark eq 0) then begin
          rrtm = rtmp
          lbl = ltmp
          imark = 1
        endif else begin
          rrtm = [rrtm,rtmp]
          lbl = [lbl,ltmp]
        endelse
      endif
    endfor
  end
  
  help,rrtm

end


