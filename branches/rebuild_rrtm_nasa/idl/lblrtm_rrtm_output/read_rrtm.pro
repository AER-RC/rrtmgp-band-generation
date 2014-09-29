
;-------------------------------------------------------
PRO read_rrtm,filename,rrtm,nbands=nbands
;-------------------------------------------------------

; Include common file with rrtm band numbers
@common_rrtm_bands

; Set keyword to read in all bands or just one band
if (not keyword_set(nbands)) then nbands = 17

; Count number of levels in output file
get_lun,unit_lw
openr,unit_lw,filename

a='' & wvnline = ''
readf,unit_lw,wvnline
readf,unit_lw,a
readf,unit_lw,a
readf,unit_lw,a
nlevels = fix(strmid(a,0,5))+1 

; Create structure
rrtm = create_struct('wvn1',strarr(nbands),'wvn2',strarr(nbands),$
  'nband',strarr(nbands),$
  'band',strarr(nbands),$
	'pres',dblarr(nlevels),$
	'nlevs',nlevels,$
  'flux',dblarr(4,nlevels,nbands),$
  'fluxstr',['up','down','net','hr'],$
	'filename','')

point_lun,unit_lw,0

rrtm.filename=strmid(file_basename(filename),12)

for i=0,nbands-1 do begin
	a=''  &  wvnline = ''

	readf,unit_lw,wvnline
	if (strmid(wvnline,2,3) eq 'Mod') then begin
		free_lun,unit_lw
		return
	endif

	rrtm.wvn1(i) = strcompress(strmid(wvnline,14,6),/remove_all)
	rrtm.wvn2(i) = strcompress(strmid(wvnline,23,6),/remove_all)
  rrtm.band(i) = rrtm.wvn1(i) + '-' + rrtm.wvn2(i)
  
; Assign band number
  icount = where(rrtm.band(i) eq wvn_rrtm_lw,nc)
  if (nc eq 1) then rrtm.nband(i) = strcompress(icount,/remove_all)
	
	readf,unit_lw,a
	readf,unit_lw,a
	readrad = dblarr(6,nlevels)

	readf,unit_lw,readrad

	rrtm.pres = reverse(reform(readrad(1,*)))
	rrtm.flux(0,*,i) = reverse(reform(readrad(2,*)))
	rrtm.flux(1,*,i) = reverse(reform(readrad(3,*)))
	rrtm.flux(2,*,i) = reverse(reform(readrad(4,*)))
	rrtm.flux(3,*,i) = reverse(reform(readrad(5,*)))

	readf,unit_lw,a
endfor

free_lun,unit_lw
return
end
