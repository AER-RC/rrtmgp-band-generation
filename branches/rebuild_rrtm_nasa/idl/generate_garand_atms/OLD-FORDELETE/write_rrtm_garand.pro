pro write_t5_rrtm,profile,dennum,ATM
pm = profile(0).p
tm = profile(0).t
re = 6371.23
zmdl = profile(0).alt
immax = n_elements(profile(0).p)

zbnd = zmdl



; Create input parameters for RECORD 1.1
cxid1 = '$GARAND Profile (Canadian Study): '+strcompress(profile(0).id,/remove_all)

; RECORD 3.1
MODEL = 0
ITYPE = 2
IBMAX = n_elements(zbnd)
NOZERO = 1
NOPRNT = 0
NMOL = 7
IPUNCH = 1
IFXTYP = 0
MUNITS = 0
RE = 6356.91
HSPACE = 0
VBAR = 0

; RECORD 1.4
tbnd_rd = [0.0,tm(0)]
sremis_rd = [0.,1.]

; RECORD 3.4
IMMAX = n_elements(pm)

H1 = zbnd(0)
H2 = zbnd(n_elements(zbnd)-1)

; RECORD 3.5
JCHARP = 'A'
JCHART = 'A'
JLONG = 0


JCHAR = ['A','A','A','A','A','A','A']

; RRTM RECORDS
IATM = ' AM=1'
NUMANGS = 3
ICLD = 0
upang=0.0
angle = upang
IXSECT =' XS=0'

wvn1 = [10.,10., 350., 500., 630., 700., 820., 980., 1080., 1180., 1390., 1480., $
	1800., 2080., 2250., 2380., 2600.,10.]

wvn2 = [3250.,350., 500., 630., 700., 820., 980., 1080., 1180., 1390., 1480., $
	1800., 2080., 2250., 2380., 2600., 3250.,3250.]
  
for iwvn = 0,n_elements(wvn1)-1 do begin
  if (iwvn eq n_elements(wvn1)-1) then IOUT = '99' else IOUT = strcompress(iwvn,/remove_all)

  rrtmdir = 'rrtm_gb'+IOUT
  rrtmtst = file_test(rrtmdir,/DIRECTORY)
  if (rrtmtst eq 0) then file_mkdir,rrtmdir
  file_t5_rr = rrtmdir+'/INPUT_RRTM.GARAND_'+strcompress(profile(0).id,/remove_all)+'_'+IOUT

  close,1
  openw,1,file_t5_rr
  printf,1,strcompress(cxid1,/remove_all)
  printf,1,format='(45x,a5,15x,a5,13x,i2,2x,i3,4x,i1,f10.6)',iatm,ixsect,numangs,iout,icld,upang
  printf,1,format='(2f10.3)',tbnd_rd(1),sremis_rd(1)
  printf,1,format='(7i5,i2,1x,i2,3f10.3)',model,itype,ibmax, $
  	nozero,noprnt,nmol,ipunch,ifxtyp,munits,re,hspace,vbar
  printf,1,format='(3f10.4,20x,5x,5x,10x)',h1,h2,angle
  printf,1,format='(13(8f10.3,:,/))',zbnd
  printf,1,format='(i5)',immax
  
  for j=0,n_elements(zmdl)-1 do begin
  	printf,1,format='(3(f10.3),5x,a1,a1,1x,1x,1x,7a1)',zmdl(j),pm(j), tm(j),$
  	jcharp,jchart,jchar
  	printf,1,format='(7e10.4)',profile(0).wv(j),profile(0).co2(j),profile(0).o3(j), $
  		profile(0).n2o(j),profile(0).co(j),profile(0).ch4(j),profile(0).o2(j)
  endfor
  
  printf,1,'%'
  
  close,1

endfor

; Create input parameters for RECORD 1.2 OD file
IHIRAC = ' HI=1'
ILBLF4 = ' F4=1'
ICNTNM = ' CN=1'
IAERSL = ' AE=0'
IEMIT = ' EM=0'
ISCAN = ' SC=0'
IFILTR = ' FI=0'
IPLOT = ' PL=0'
ITEST =	' TS=0'
IATM = ' AM=1'
IMRG =' MG=1'
ILAS = ' LA=0'
IOD = ' OD=0'
IXSECT =' XS=0'
MPTS = '   00'
NPTS = '   00'

wvn1 = [10., 350., 500., 630., 700., 820., 980., 1080., 1180., 1390., 1480., $
	1800., 2080., 2250., 2380., 2600.]

wvn2 = [350., 500., 630., 700., 820., 980., 1080., 1180., 1390., 1480., $
	1800., 2080., 2250., 2380., 2600., 3250.]
  
for iwvn = 0,n_elements(wvn1)-1 do begin

  lbldir = 'lbl_gb'+strcompress(iwvn+1,/remove_all)
  lbltst = file_test(lbldir,/DIRECTORY)
  if (lbltst eq 0) then file_mkdir,lbldir
  
  file_t5_lbl = lbldir+'/TAPE5.GARANDOD_'+strcompress(profile(0).id,/remove_all)+'_'+strcompress(iwvn+1,/remove_all)
  
  openw,1,file_t5_lbl
  printf,1,strcompress(cxid1,/remove_all)
  printf,1,format='(14(5a),a5,a5)',$
  	ihirac, ilblf4,icntnm,iaersl,iemit,iscan,ifiltr, $
  	iplot,itest,iatm,imrg,ilas,iod,ixsect,mpts,npts
  printf,1,format='(2f10.3)',wvn1(iwvn)-5.0,wvn2(iwvn)+5.0
  ;printf,1,format='(2f10.3)',tbnd_rd(1),sremis_rd(1)
  printf,1,format='(7i5,i2,1x,i2,3f10.3)',model,itype,ibmax, $
  	nozero,noprnt,nmol,ipunch,ifxtyp,munits,re,hspace,vbar
  printf,1,format='(3f10.4,20x,5x,5x,10x)',h1,h2,0.0
  printf,1,format='(13(8f10.3,:,/))',zbnd
  printf,1,format='(i5)',immax
  for j=0,n_elements(zmdl)-1 do begin
  	printf,1,format='(3(f10.3),5x,a1,a1,1x,1x,1x,7a1)',zmdl(j),pm(j), tm(j),$
  	jcharp,jchart,jchar
  	printf,1,format='(7e10.4)',profile(0).wv(j),profile(0).co2(j),profile(0).o3(j), $
  		profile(0).n2o(j),profile(0).co(j),profile(0).ch4(j),profile(0).o2(j)
  endfor
  
  
  printf,1,'%'
  close,1
endfor

; write TAPE5 RD file
IHIRAC = ' HI=0'
ILBLF4 = ' F4=0'
ICNTNM = ' CN=1'
IAERSL = ' AE=0'
IEMIT = ' EM=0'
ISCAN = ' SC=0'
IFILTR = ' FI=0'
IPLOT = ' PL=0'
ITEST =	' TS=0'
IATM = ' AM=1'
IMRG =' MG35'
ILAS = ' LA=0'
IOD = ' OD=0'
IXSECT =' XS=0'
MPTS = '   00'
NPTS = '   00'  

for iwvn = 0,n_elements(wvn1)-1 do begin

  lbldir = 'lbl_gb'+strcompress(iwvn+1,/remove_all)
  lbltst = file_test(lbldir,/DIRECTORY)
  if (lbltst eq 0) then file_mkdir,lbldir

  file_t5_lbl = lbldir+'/TAPE5.GARANDRD_'+strcompress(profile(0).id,/remove_all)+'_'+strcompress(iwvn+1,/remove_all)  
  openw,1,file_t5_lbl
	cxid1 = '$ TAPE5.GARANDRD_'+strcompress(profile(0).id,/remove_all)+'_'+strcompress(iwvn+1,/remove_all)

	IHIRAC = ' HI=0'
	ILBLF4 = ' F4=0'
	ICNTNM = ' CN=1'
	IAERSL = ' AE=0'
	IEMIT = ' EM=1'
	ISCAN = ' SC=0'	
	IFILTR = ' FI=0'
	IPLOT = ' PL=0'
	ITEST =	' TS=0'
	IATM = ' AM=1'
	IMRG =' MG35'
	ILAS = ' LA=0'
	IOD = ' OD=0'
	IXSECT =' XS=0'
	MPTS = '   00'
	NPTS = '   00'

	hwhm = 0.25
	jemit = 1
	jfn = 0
	jvar = 0
	sampl = 0.0
	nnfile = [31,32,33]
	nnpts = 0

	dircos = [0.911412,0.590533,0.212341]

  for iangle = 0,2 do begin
    printf,1,strcompress(cxid1,/remove_all)
    printf,1,format='(14(5a),a5,a5)',$
    	ihirac, ilblf4,icntnm,iaersl,iemit,iscan,ifiltr, $
    	iplot,itest,iatm,imrg,ilas,iod,ixsect,mpts,npts
    printf,1,format='(2f10.3)',wvn1(iwvn)-5.0,wvn2(iwvn)+5.0
    printf,1,format='(2f10.3)',0.0,0.0
    printf,1,format='(a8,47x,1x,i4)','ODdeflt_',ibmax-1
    printf,1,format='(3f10.3,3x,i2,3x,i2,3x,i2,f10.4,15x,i5,i5)', $
    	hwhm,wvn1(iwvn)-0.25,wvn2(iwvn)+0.25,jemit,jfn,jvar,sampl, $
    	nnfile(iangle),nnpts
    printf,1,format='(f10.5)',dircos(iangle)
  endfor

	IMRG =' MG36'
	nnfile = [61,62,63]
  for iangle = 0,2 do begin
  	printf,1,strcompress(cxid1,/remove_all)
  	printf,1,format='(14(5a),a5,a5)',$
  		ihirac, ilblf4,icntnm,iaersl,iemit,iscan,ifiltr, $
  		iplot,itest,iatm,imrg,ilas,iod,ixsect,mpts,npts
  	printf,1,format='(2f10.3)',wvn1(iwvn)-5.0,wvn2(iwvn)+5.0
  	printf,1,format='(2f10.3)',tbnd_rd(1),sremis_rd(1)
  	printf,1,format='(a8,47x,1x,i4)','ODdeflt_',ibmax-1
  	printf,1,format='(3f10.3,3x,i2,3x,i2,3x,i2,f10.4,15x,i5,i5)', $
  		hwhm,wvn1(iwvn)-0.25,wvn2(iwvn)+0.25,jemit,jfn,jvar,sampl, $
  		nnfile(iangle),nnpts
  	printf,1,format='(f10.5)',dircos(iangle)
  endfor
  printf,1,'%'
  close,1


  openw,1,lbldir+'/RADINIT.GARAND_'+strcompress(profile(0).id,/remove_all)+'_'+strcompress(iwvn+1,/remove_all)
  printf,1,format='(2f10.2,2i5,f8.3,i5,a1)',wvn1(iwvn),wvn2(iwvn),3,ibmax,tbnd_rd(1),0,'<'
  printf,1,'-1.'
  close,1

endfor


end
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
openr,2,'42_newprofiles.txt'

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

	write_t5_rrtm,tprofg(inp)
endfor
close,2
tprofgg = tprofg
save,filename='garand_profile.dat',tprofg

end
