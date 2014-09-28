ib=3
ig=indgen(16)+1

scalefac = [0.3,1.0,3.0]
nscalefac = n_elements(scalefac)
;meantau = dblarr(nscalefac) & meantranstau = meantau

; Iband
ib = '3'

bstr=strcompress(ib,/remove_all)
for ia=0,42 do begin
  astr = strcompress(ia+1,/remove_all)
  for il=0,26 do begin
    lstr = strcompress(il+1,/remove_all)  
    for ig=0,15 do begin
      gstr = strcompress(ig+1,/remove_all)
      fnameres='/Volumes/project/p1770/jdelamer/build_planckfractions/tst_newtrans/junk/'+'IG_'+gstr+'_LAY_'+lstr+'_'+astr+'_'+bstr+'_L.DAT'
      data = read_ascii(fnameres)
      tauv = double(data.field1(*))
      meantauv = mean(tauv)
      transv = exp(-tauv)
      meantransv = mean(transv)
      meantranstauv = -1.*alog(meantransv)
      
      for i=0,n_elements(scalefac)-1 do begin
        tau = tauv*scalefac(i)
        ; Compute mean tau with scale factor
        meantau = mean(tau)
      
        ; Compute mean tau via transmittance with scale factor
        trans = exp(-tau)
        meantrans = mean(trans)
        meantranstau = -1.*alog(meantrans)
      
      ; Comparison with normal method
        meantausf = exp(-scalefac(i)*meantauv)
        meantranstausf = exp(-scalefac(i)*meantranstauv)
        print,scalefac(i),exp(-meantau),meantrans,meantausf,meantranstausf
      endfor
      stop
      plot,tau
      oplot,[0,10000000000.],[meantau,meantau],linestyle=1
      oplot,[0,10000000000.],[meantranstau,meantranstau],linestyle=2
      stop
    endfor
  endfor
endfor

end
