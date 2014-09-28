
ig=indgen(16)+1

scalefac = [0.3,1.0,3.0]
nscalefac = n_elements(scalefac)
;meantau = dblarr(nscalefac) & meantranstau = meantau

; Iband
ib = '3'
fsave = 'Band3_stats.dat'
;goto, jump
nlay=42
ng=16
nsf=3
na=42
transmean=dblarr(nlay,ng,nsf,na)
transzero = transmean & transone = transmean & transtwo = transmean
pave = fltarr(nlay)
bstr=strcompress(ib,/remove_all)
for ia=0,na-1 do begin
  astr = strcompress(ia+1,/remove_all) 
  fnameres='/Volumes/project/p1770/jdelamer/build_planckfractions/tst_newtrans/lbl_gb'+bstr+'_scalefac/TAUSTATS_'+astr+'_'+bstr+'_L.DAT'
  print,'Reading...',fnameres
  data = read_ascii(fnameres,data_start=1)
  ptmp = uniq(data.field1(1,*))
  pave(0:25)=data.field1(1,ptmp)
  npts=0
  for il=0,25 do begin
    for ig=0,ng-1 do begin
      for isf=0,nsf-1 do begin
        transmean(il,ig,isf,ia)=data.field1(4,npts)
        transzero(il,ig,isf,ia)=data.field1(5,npts)
        transone(il,ig,isf,ia)=data.field1(6,npts)
        transtwo(il,ig,isf,ia)=data.field1(7,npts)        
        npts = npts+1
      endfor
    endfor
  endfor
endfor
ptmp = uniq(data.field1(1,*))
pave(0:25)=data.field1(1,ptmp)

for ia=0,na-1 do begin
  astr = strcompress(ia+1,/remove_all) 
  fnameres='/Volumes/project/p1770/jdelamer/build_planckfractions/tst_newtrans/lbl_gb'+bstr+'_scalefac/TAUSTATS_'+astr+'_'+bstr+'_U.DAT'
  print,'Reading...',fnameres
  data = read_ascii(fnameres,data_start=1)
  npts=0
  for il=26,nlay-1 do begin
    for ig=0,ng-1 do begin
      for isf=0,nsf-1 do begin
        transmean(il,ig,isf,ia)=data.field1(4,npts)
        transzero(il,ig,isf,ia)=data.field1(5,npts)
        transone(il,ig,isf,ia)=data.field1(6,npts)
        transtwo(il,ig,isf,ia)=data.field1(7,npts) 
        npts = npts+1
      endfor
    endfor
  endfor
endfor
ptmp = uniq(data.field1(1,*))
pave(26:*)=data.field1(1,ptmp)
save,pave,transmean,transzero,transone,transtwo,filename=fsave

jump: print,'Restoring file'
restore,fsave
; Collect statisticts
transdiffone = fltarr(nlay,ng,nsf)
transdifftwo = fltarr(nlay,ng,nsf)
for ig=0,ng-1 do begin
  transdiffone_sf1 = 0.0
  transdiffone_sf2 = 0.0
  transdiffone_sf3 = 0.0
  transdifftwo_sf1 = 0.0
  transdifftwo_sf2 = 0.0
  transdifftwo_sf3 = 0.0
  for il=0,nlay-1 do begin
    for ia=0,na-1 do begin
      if (transmean(il,ig,0,ia) gt 1.e-4) then begin
        transdiffone_sf1 = [transdiffone_sf1,abs(transone(il,ig,0,ia)-transzero(il,ig,0,ia))]
        transdiffone_sf2 = [transdiffone_sf2,abs(transone(il,ig,1,ia)-transzero(il,ig,1,ia))]
        transdiffone_sf3 = [transdiffone_sf3,abs(transone(il,ig,2,ia)-transzero(il,ig,2,ia))]
        transdifftwo_sf1 = [transdifftwo_sf1,abs(transtwo(il,ig,0,ia)-transzero(il,ig,0,ia))]
        transdifftwo_sf2 = [transdifftwo_sf2,abs(transtwo(il,ig,1,ia)-transzero(il,ig,1,ia))]
        transdifftwo_sf3 = [transdifftwo_sf3,abs(transtwo(il,ig,2,ia)-transzero(il,ig,2,ia))]   
      endif
    endfor
  transdiffone(il,ig,0) = mean(transdiffone_sf1)
  transdifftwo(il,ig,0) = mean(transdifftwo_sf1)
  transdiffone(il,ig,1) = mean(transdiffone_sf2)
  transdifftwo(il,ig,1) = mean(transdifftwo_sf2)
  transdiffone(il,ig,2) = mean(transdiffone_sf3)
  transdifftwo(il,ig,2) = mean(transdifftwo_sf3)
  endfor  
endfor

save,pave,transmean,transzero,transone,transtwo,transdiffone,transdifftwo,filename=fsave
end
