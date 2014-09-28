PRO drive_lbl_rrtm, BANDNUMBER = bandnum

; Set up input
if (not keyword_set(bandnum)) then bandnum = 17

; Choose rrtm directory
rrtm_dir = '/project/p1770/jdelamer/garand_validation_atmospheres/rrtm_v3.0'
plots_dir = rrtm_dir + 'plots'

lbl_dir = '/project/p1770/jdelamer/garand_validation_atmospheres/lbl_v5.21_aer_jpl_hitran_96_v2.0/'

; Get list of RRTM directories
rfiles = file_search(rrtm_dir,'OUTPUT_RRTM.GARAND*')
rbase = file_basename(rfiles)
rext = strmid(rbase,12)
nfiles = n_elements(rext)

if (bandnum eq 17) then begin
  for ifile = 0,nfiles-1 do begin
    read_rrtm,rfiles(ifile),rtmp,nbands=1
  endfor
endif else begin
  lbl_dir = lbl_dir+'lbl_gb'+strcompress(bandnum,/remove_all)+'/'
  imark = 0
  for ifile = 0,nfiles-1 do begin
    lbl_file = lbl_dir+'OUTPUT_LBLRTM.'+rext(ifile) 
    print,'COMPARING FILES: ',rfiles(ifile),lbl_file
    if (file_test(lbl_file)) then begin
      read_rrtm,rfiles(ifile),rtmp,nbands=1
      read_lbl_radsum,lbl_file,ltmp
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

end


