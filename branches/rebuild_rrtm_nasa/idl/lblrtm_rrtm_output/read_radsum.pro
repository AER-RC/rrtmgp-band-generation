;-------------------------------------------------------
PRO read_radsum,filename,radsum
;-------------------------------------------------------
; Include common file with rrtm band numbers
@common_rrtm_bands

; Read RADSUM style output; assumes only one band in file.
a='' & b = 1000 & nlevels = 0

; There are five lines in the header of a RADSUM file
nlevels = file_lines(filename) - 5
print,'NLEVELS',nlevels
radsum = create_struct('wvn1','','wvn2','',$
  'nband','',$
  'band','',$
  'pres',dblarr(nlevels),$
  'nlevs',nlevels,$
  'flux',dblarr(4,nlevels),$
  'fluxstr',['up','down','net','hr'],$
  'filename','')

get_lun,unit_radsum
openr,unit_radsum,filename

a='' & b=1 & c=0.0 & d=0.0 & e=0.0 & f=0.0 & g=0.0
readf,unit_radsum,a
readf,unit_radsum,a
wave_info_str = STRSPLIT(a, ' ', /EXTRACT)
radsum.wvn1 = strcompress(wave_info_str(2),/remove_all)
radsum.wvn2 = strcompress(wave_info_str(4),/remove_all)
radsum.band = radsum.wvn1 + '-' + radsum.wvn2

; Assign band number
icount = where(radsum.band eq wvn_rrtm_lw,nc)
if (nc eq 1) then radsum.nband = strcompress(icount,/remove_all)

readf,unit_radsum,a
readf,unit_radsum,a
readf,unit_radsum,a

while(b ge 1) do begin
	readf,unit_radsum,b,c,d,e,f,g
	radsum.pres(b) = c
	radsum.flux(0,b) = d
	radsum.flux(1,b) = e
	radsum.flux(2,b) = f
	radsum.flux(3,b) = g
endwhile
free_lun,unit_radsum

end
