PRO get_garand_profs,tprofg,scalefacs=scalefacs

  close,2
  ; Set up GARAND profile arrays
  nprof = 42
  nlevels = 43

  tprofg = {tprofg, id:0, alt:fltarr(nlevels), $
  	p:fltarr(nlevels), $
  	t:fltarr(nlevels),wv:fltarr(nlevels),co2:fltarr(nlevels), $
  	o3:fltarr(nlevels),n2o:fltarr(nlevels),co:fltarr(nlevels), $
  	ch4:fltarr(nlevels),o2:fltarr(nlevels),n2:fltarr(nlevels)}

  tprofg = replicate(tprofg,nprof)

  ; Read GARAND profiles from Canadian web-site
  atxt=''
  a=0
  b=0

  ; Find file with Garand profile information; assume it is in the IDL directory that contains the code)
  pathdir = expand_path(!PATH,/ARRAY)
  atmfile = file_search(pathdir+'/42_newprofiles.txt')
  natmfile = n_elements(atmfile)
  case 1 of
    (natmfile eq 0): begin
      print,'NO 42_newprofiles.txt file found in IDL !PATH'
      stop
    end
    (natmfile gt 1): begin
      print,'Multiple 42_newprofiles.txt file found in IDL !PATH'
      stop
    end
    (natmfile eq 1): begin
      print,'Reading '+atmfile+' for Garand profiles'
      file_copy,atmfile,'.',/OVERWRITE      
    end
  endcase
  openr,2,atmfile

  for inp = 0,nprof-1 do begin
  	readf,2,atxt
  	readf,2,atxt
  	tprofg(inp).id = inp+1
  	for inl = 0,nlevels-1 do begin

  		readf,2,b,c,d,e,f,g,h,p,q,r,s,t
  		tprofg(inp).alt(inl) = c
  		tprofg(inp).p(inl) = d
  		tprofg(inp).t(inl) = e
  		tprofg(inp).wv(inl) = f*1.e6
  		tprofg(inp).co2(inl) = g*1.e6
  		tprofg(inp).o3(inl) = h*1.e6
  		tprofg(inp).n2o(inl) = p*1.e6
  		tprofg(inp).co(inl) = q*1.e6
  		tprofg(inp).ch4(inl) = r*1.e6
  		tprofg(inp).o2(inl) = s*1.e6
  		tprofg(inp).n2(inl) = t*1.e6
  	endfor
  	tprofg(inp).alt = reverse(tprofg(inp).alt)
  	tprofg(inp).p = reverse(tprofg(inp).p)
  	tprofg(inp).t = reverse(tprofg(inp).t)
  	tprofg(inp).wv = reverse(tprofg(inp).wv)
  	tprofg(inp).co2 = reverse(tprofg(inp).co2)
  	tprofg(inp).o3 = reverse(tprofg(inp).o3)
  	tprofg(inp).n2o = reverse(tprofg(inp).n2o)
  	tprofg(inp).co = reverse(tprofg(inp).co)
  	tprofg(inp).ch4 = reverse(tprofg(inp).ch4)
  	tprofg(inp).o2 = reverse(tprofg(inp).o2)
  	tprofg(inp).n2 = reverse(tprofg(inp).n2)
  endfor
  close,2
  
  if (n_elements(scalefacs) eq 7) then begin
    tprofg.wv=scalefacs(0)*tprofg.wv
    tprofg.co2=scalefacs(1)*tprofg.co2
    tprofg.o3=scalefacs(2)*tprofg.o3
    tprofg.n2o=scalefacs(3)*tprofg.n2o
    tprofg.co=scalefacs(4)*tprofg.co
    tprofg.ch4=scalefacs(5)*tprofg.ch4
    tprofg.o2=scalefacs(6)*tprofg.o2
;    tprofg.n2=scalefacs(6)*tprofg.n2
  endif
  
end