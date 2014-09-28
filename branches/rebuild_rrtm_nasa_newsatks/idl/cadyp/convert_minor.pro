pro convert_minor,nminor,neta,temp_index_a,temp_index_b,molec_arr_a,molec_arr_b
close,1
close,2

; Set up arrays for input
mo3_a_1 = fltarr(neta[0],16)
mo3_a_2 = fltarr(neta[0],16)
mo3_b_1 = fltarr(neta[1],16)
mo3_b_2 = fltarr(neta[1],16)

; Set temperature profile used in calculations (MLS)
arr = read_file_str ('/project/p1905/band_build/TAPE6.tropo.short',1)
temp_a = float(arr[*,7])

; Last trop level and first strat level are the same; throw out first strat level
arr = read_file_str ('/project/p1905/band_build/TAPE6.strat.short',2)
temp_b = float(arr[*,7])
tlev_arr = [temp_a,temp_b]


; Set up temperature array for interpolation
tstart = 188.0
nt=19
tnew = [tstart+findgen(nt)*7.2]

; Read in files

openr,1,'kg_minor.f'
openw,2,'k_minor.txt'

a=''
for i=0,6 do readf,1,a


; Read in tropospheric values first (tagged as l or a)
for im=0,nminor[0]-1 do begin

; Set chosen temperatures for minor species
   tlev_a = tlev_arr[temp_index_a[im]-1]

   ; Read minor kabs 

; Get values for first temperature
   for i=0,neta[0]-1 do begin
   readf,1,a
   readf,1,format='(5x,a1,6(e10.4,a1))',a,b,a,c,a,d,a,e,a,f,a,g,a
   mo3_a_1(i,0:5) = [b,c,d,e,f,g]
   readf,1,format='(5x,a1,6(e10.4,a1))',a,b,a,c,a,d,a,e,a,f,a,g,a
   mo3_a_1(i,6:11) = [b,c,d,e,f,g]
   readf,1,format='(5x,a1,4(e10.4,a1))',a,b,a,c,a,d,a,e,a
   mo3_a_1(i,12:15) = [b,c,d,e]

; Get values for second temperature
   readf,1,a
   readf,1,format='(5x,a1,6(e10.4,a1))',a,b,a,c,a,d,a,e,a,f,a,g,a
   mo3_a_2(i,0:5) = [b,c,d,e,f,g]
   readf,1,format='(5x,a1,6(e10.4,a1))',a,b,a,c,a,d,a,e,a,f,a,g,a
   mo3_a_2(i,6:11) = [b,c,d,e,f,g]
   readf,1,format='(5x,a1,4(e10.4,a1))',a,b,a,c,a,d,a,e,a
   mo3_a_2(i,12:15) = [b,c,d,e]
   endfor

   ; Use exponential interpolation to obtain values at 7.2 degree increment,
   tfac_a = (tnew-tlev_a-15.)/15.0
   mo3_a = fltarr(neta[0],16,nt)

   for j=0,15 do begin
       for k=0,nt-1 do begin
	   for m=0,neta[0]-1 do begin	
	   mo3_a(m,j,k) =  mo3_a_1(m,j)*(mo3_a_2(m,j)/mo3_a_1(m,j))^tfac_a(k)
	   endfor
       endfor
   endfor

   ; Write out new file to be inserted into k_gB??.f file

   for i=0,15 do begin
       for j=0,neta[0]-1 do begin
	   printf,2,format='(a17,a3,a1,i2,a4,i2,a13)','      DATA (KA_M',molec_arr_a[im],'(',j+1,',JT,',i+1,'),JT=1,19)  /'
	   printf,2,format='(5x,a1,5(1x,e11.5,a1))','&',mo3_a(j,i,0),',',   $
		   mo3_a(j,i,1),',',mo3_a(j,i,2),',',mo3_a(j,i,3),',', $
		   mo3_a(j,i,4),','
	   printf,2,format='(5x,a1,5(1x,e11.5,a1))','&',mo3_a(j,i,5),',',   $
		   mo3_a(j,i,6),',',mo3_a(j,i,7),',',mo3_a(j,i,8),',', $
		   mo3_a(j,i,9),','
	   printf,2,format='(5x,a1,5(1x,e11.5,a1))','&',mo3_a(j,i,10),',',   $
		   mo3_a(j,i,11),',',mo3_a(j,i,12),',',mo3_a(j,i,13),',', $
		   mo3_a(j,i,14),','
	   printf,2,format='(5x,a1,4(1x,e11.5,a1))','&',mo3_a(j,i,15),',',   $
		   mo3_a(j,i,16),',',mo3_a(j,i,17),',',mo3_a(j,i,18),'/'
       endfor
   endfor

endfor ; end of lower minor loop

; Read in stratospheric values first (tagged as u or b)
for im=0,nminor[1]-1 do begin

; Set chosen temperatures for minor species
   tlev_b = tlev_arr[temp_index_b[im]-1]

   ; Read minor kabs 

; Get values for first temperature
   for i=0,neta[1]-1 do begin
      readf,1,a
      readf,1,format='(5x,a1,6(e10.4,a1))',a,b,a,c,a,d,a,e,a,f,a,g,a
      mo3_b_1(i,0:5) = [b,c,d,e,f,g]
      readf,1,format='(5x,a1,6(e10.4,a1))',a,b,a,c,a,d,a,e,a,f,a,g,a
      mo3_b_1(i,6:11) = [b,c,d,e,f,g]
      readf,1,format='(5x,a1,4(e10.4,a1))',a,b,a,c,a,d,a,e,a
      mo3_b_1(i,12:15) = [b,c,d,e]

; Get values for second temperature
      readf,1,a
      readf,1,format='(5x,a1,6(e10.4,a1))',a,b,a,c,a,d,a,e,a,f,a,g,a
      mo3_b_2(i,0:5) = [b,c,d,e,f,g]
      readf,1,format='(5x,a1,6(e10.4,a1))',a,b,a,c,a,d,a,e,a,f,a,g,a
      mo3_b_2(i,6:11) = [b,c,d,e,f,g]
      readf,1,format='(5x,a1,4(e10.4,a1))',a,b,a,c,a,d,a,e,a
      mo3_b_2(i,12:15) = [b,c,d,e]
   endfor

   ; Use exponential interpolation to obtain values at 7.2 degree increment,
   tfac_b = (tnew-tlev_b-15.)/15.0
   mo3_b = fltarr(neta[1],16,nt)

   for j=0,15 do begin
       for k=0,nt-1 do begin
	   for m=0,neta[1]-1 do begin	
	   mo3_b(m,j,k) =  mo3_b_1(m,j)*(mo3_b_2(m,j)/mo3_b_1(m,j))^tfac_b(k)
	   endfor
       endfor
   endfor

   ; Write out new file to be inserted into k_gB??.f file

   for i=0,15 do begin
       for j=0,neta[1]-1 do begin
	   printf,2,format='(a17,a3,a1,i2,a4,i2,a13)','      DATA (KB_M',molec_arr_b[im],'(',j+1,',JT,',i+1,'),JT=1,19)  /'
	   printf,2,format='(5x,a1,5(1x,e11.5,a1))','&',mo3_b(j,i,0),',',   $
		   mo3_b(j,i,1),',',mo3_b(j,i,2),',',mo3_b(j,i,3),',', $
		   mo3_b(j,i,4),','
	   printf,2,format='(5x,a1,5(1x,e11.5,a1))','&',mo3_b(j,i,5),',',   $
		   mo3_b(j,i,6),',',mo3_b(j,i,7),',',mo3_b(j,i,8),',', $
		   mo3_b(j,i,9),','
	   printf,2,format='(5x,a1,5(1x,e11.5,a1))','&',mo3_b(j,i,10),',',   $
		   mo3_b(j,i,11),',',mo3_b(j,i,12),',',mo3_b(j,i,13),',', $
		   mo3_b(j,i,14),','
	   printf,2,format='(5x,a1,4(1x,e11.5,a1))','&',mo3_b(j,i,15),',',   $
		   mo3_b(j,i,16),',',mo3_b(j,i,17),',',mo3_b(j,i,18),'/'
       endfor
   endfor

endfor ; end of upper minor loop

close,1
close,2
return

end



