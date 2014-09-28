pro write_t5garand_profs,tprofg,filehdr=filehdr,rrtmpdir=rrtmpdir,lblpdir=lblpdir,splitprof=splitprof
  nprofs = n_elements(tprofg)
  if (n_elements(filehdr) eq 0) then filehdr='$GARAND Profile (Canadian Study):'  

  for iprof = 0,nprofs-1 do begin
    profile = tprofg(iprof)

    ; Manage keywords
    if (not keyword_set(rrtmpdir)) then rrtmpdir='.'
    if (not keyword_set(lblpdir)) then lblpdir='.'  
    
    ; Set up RRTM wavelengths for writing profile
    @common_rrtm_wavelengths
    
    ; Atmospheric Pressure
    pm = profile.p
    ; Atmospheric Temperature
    tm = profile.t
    ; Atmospheric Altitudes (Set level boundaries to be that of input profile)
    zmdl = profile.alt
    zbnd = zmdl
    nzmdl = n_elements(zmdl)
    nend = 1
    if (keyword_set(splitprof)) then begin
      @common_keyspecies
      tropind = where(alog(pm) ge 4.56,ntrop,complement=stratind)
      itrop = ntrop-1
      nend = -1
    endif  
    ; Create input parameters for RECORD 1.1
    cxid1 = filehdr + ': Garand Profile ' + strcompress(profile.id,/remove_all)
    
    ; RECORD 3.1
    model = 0
    itype = 2
    ibmax = n_elements(zbnd)
    nozero = 1
    noprnt = 0
    nmol = 7
    ipunch = 1
    ifxtyp = 0
    munits = 0
    re = 6356.910
    hspace = 0
    vbar = 0
    
    ; RECORD 1.4
    tbnd_rd = [0.0,tm(0)]
    sremis_rd = [0.,1.]
    
    ; RECORD 3.4
    immax = n_elements(pm)
    
    h1 = zbnd(0)
    h2 = zbnd(n_elements(zbnd)-1)
    
    ; RECORD 3.5
    jcharp = 'A'
    jchart = 'A'
    jlong = 0
    
    jchar = ['A','A','A','A','A','A','A']
    
    ; RRTM RECORDS
    iatm = ' am=1'
    numangs = 3
    icld = 0
    upang=0.0
    angle = upang
    ixsect =' xs=0'

    ; Very important note here!
    ; We will write out two additional RRTM files, RRTM total for all bands, RRTM for all bands in one file
    ; These are written after the initial files
    nwvn = n_elements(wvn1)
    
    for iwvn = 0,nwvn+nend do begin
      case 1 of
        (iwvn eq nwvn): iout='0'
        (iwvn eq nwvn+1): iout='99
        else: iout = strcompress(iwvn+1,/remove_all)
      endcase    
      rrtmdir = rrtmpdir+'/rrtm_gb'+iout
      rrtmtst = file_test(rrtmdir,/DIRECTORY)
      if (rrtmtst eq 0) then file_mkdir,rrtmdir
      file_t5_rr = rrtmdir+'/INPUT_RRTM.GARAND_'+strcompress(profile.id,/remove_all)+'_'+iout
    
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
      
      if (keyword_set(splitprof)) then begin  
        scale_l = reform(key_l(iwvn,0:6))
        scale_u = reform(key_u(iwvn,0:6))
        for j=0,ntrop-1 do begin
    	    printf,1,format='(3(f10.3),5x,a1,a1,1x,1x,1x,7a1)',zmdl(j),pm(j), tm(j),$
    	      jcharp,jchart,jchar
    	    printf,1,format='(7e10.4)',scale_l(0)*profile.wv(j),scale_l(1)*profile.co2(j),$
            scale_l(2)*profile.o3(j),scale_l(3)*profile.n2o(j),$
            scale_l(4)*profile.co(j),scale_l(5)*profile.ch4(j),scale_l(6)*profile.o2(j)
        endfor
        for j=ntrop,nzmdl-1 do begin
    	    printf,1,format='(3(f10.3),5x,a1,a1,1x,1x,1x,7a1)',zmdl(j),pm(j), tm(j),$
    	      jcharp,jchart,jchar
          printf,1,format='(7e10.4)',scale_u(0)*profile.wv(j),scale_u(1)*profile.co2(j),$
            scale_u(2)*profile.o3(j),scale_u(3)*profile.n2o(j),$
            scale_u(4)*profile.co(j),scale_u(5)*profile.ch4(j),scale_u(6)*profile.o2(j)
        endfor           
      endif else begin
        for j=0,nzmdl-1 do begin
        	printf,1,format='(3(f10.3),5x,a1,a1,1x,1x,1x,7a1)',zmdl(j),pm(j), tm(j),$
        	jcharp,jchart,jchar
          printf,1,format='(7e10.4)',profile.wv(j),profile.co2(j),profile.o3(j), $
        		profile.n2o(j),profile.co(j),profile.ch4(j),profile.o2(j)
        endfor
      endelse 
      
      printf,1,'%'
      
      close,1
    
    endfor
    
    ; Create input parameters for RECORD 1.2 OD file
    ihirac = ' HI=1'
    ilblf4 = ' F4=1'
    icntnm = ' CN=1'
    iaersl = ' AE=0'
    iemit =  ' EM=0'
    iscan =  ' SC=0'
    ifiltr = ' FI=0'
    iplot =  ' PL=0'
    itest =	 ' TS=0'
    iatm =   ' AM=1'
    imrg =   ' MG=1'
    ilas =   ' LA=0'
    iod =    ' OD=0'
    ixsect = ' XS=0'
    mpts =   '   00'
    npts =   '   00'
    
    for iwvn = 0,n_elements(wvn1)-1 do begin
    
      lbldir = lblpdir+'/lbl_gb'+strcompress(iwvn+1,/remove_all)
      lbltst = file_test(lbldir,/DIRECTORY)
      if (lbltst eq 0) then file_mkdir,lbldir
      
      file_t5_lbl = lbldir+'/TAPE5.GARANDOD_'+strcompress(profile.id,/remove_all)+'_'+strcompress(iwvn+1,/remove_all)
      
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
     
      if (keyword_set(splitprof)) then begin  
        scale_l = reform(key_l(iwvn,0:6))
        scale_u = reform(key_u(iwvn,0:6))
        for j=0,ntrop-1 do begin
    	    printf,1,format='(3(f10.3),5x,a1,a1,1x,1x,1x,7a1)',zmdl(j),pm(j), tm(j),$
    	      jcharp,jchart,jchar
    	    printf,1,format='(7e10.4)',scale_l(0)*profile.wv(j),scale_l(1)*profile.co2(j),$
            scale_l(2)*profile.o3(j),scale_l(3)*profile.n2o(j),$
            scale_l(4)*profile.co(j),scale_l(5)*profile.ch4(j),scale_l(6)*profile.o2(j)
        endfor
        for j=ntrop,nzmdl-1 do begin
    	    printf,1,format='(3(f10.3),5x,a1,a1,1x,1x,1x,7a1)',zmdl(j),pm(j), tm(j),$
    	      jcharp,jchart,jchar
          printf,1,format='(7e10.4)',scale_u(0)*profile.wv(j),scale_u(1)*profile.co2(j),$
            scale_u(2)*profile.o3(j),scale_u(3)*profile.n2o(j),$
            scale_u(4)*profile.co(j),scale_u(5)*profile.ch4(j),scale_u(6)*profile.o2(j)
        endfor           
      endif else begin
        for j=0,nzmdl-1 do begin
        	printf,1,format='(3(f10.3),5x,a1,a1,1x,1x,1x,7a1)',zmdl(j),pm(j), tm(j),$
        	jcharp,jchart,jchar
          printf,1,format='(7e10.4)',profile.wv(j),profile.co2(j),profile.o3(j), $
        		profile.n2o(j),profile.co(j),profile.ch4(j),profile.o2(j)
        endfor
      endelse 
      
      printf,1,'%'
      close,1
    endfor
    
    ; write TAPE5 RD file
    ihirac = ' HI=0'
    ilblf4 = ' F4=0'
    icntnm = ' CN=1'
    iaersl = ' AE=0'
    iemit =  ' EM=1'
    iscan =  ' SC=0'
    ifiltr = ' FI=0'
    iplot =  ' PL=0'
    itest =	 ' TS=0'
    iatm =   ' AM=1'
    ilas =   ' LA=0'
    iod =    ' OD=0'
    ixsect = ' XS=0'
    mpts =   '   00'
    npts =   '   00' 
     
    hwhm = 0.25
    jemit = 1
    jfn = 0
    jvar = 0
    sampl = 0.0
    nnpts = 0
    dircos = [0.911412,0.590533,0.212341]
    
    for iwvn = 0,n_elements(wvn1)-1 do begin
      lbldir = lblpdir+'/lbl_gb'+strcompress(iwvn+1,/remove_all)
      lbltst = file_test(lbldir,/DIRECTORY)
      if (lbltst eq 0) then file_mkdir,lbldir
      
      file_t5_lbl = lbldir+'/TAPE5.GARANDRD_'+strcompress(profile.id,/remove_all)+'_'+strcompress(iwvn+1,/remove_all)  
      openw,1,file_t5_lbl
    
    	IMRG =' MG35'
      nnfile = [31,32,33]
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
    
      openw,1,lbldir+'/RADINIT.GARAND_'+strcompress(profile.id,/remove_all)+'_'+strcompress(iwvn+1,/remove_all)
      printf,1,format='(2f10.2,2i5,f8.3,i5,a1)',wvn1(iwvn),wvn2(iwvn),3,ibmax,tbnd_rd(1),0,'<'
      printf,1,'-1.'
      close,1
    
    endfor
  endfor  
end