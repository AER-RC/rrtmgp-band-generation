close,1
close,2
; Fill in starting information
; Level 3
mn2o_a_1 = fltarr(9,16)
mn2o_a_2 = fltarr(9,16)
tlev = 278.94
tn2o_a_1 = tlev-15
tn2o_a_2 = tlev

; Level 1
mn2o_b_1 = fltarr(5,16)
mn2o_b_2 = fltarr(5,16)
tlev = 215.7
tn2o_b_1 = tlev-15
tn2o_b_2 = tlev

; Read in files

openr,1,'kg_minor.f'
openw,2,'k_minor.txt'

a=''
readf,1,a
readf,1,a
readf,1,a
readf,1,a
readf,1,a
readf,1,a
readf,1,a

; Read lower n2o

for i=0,8 do begin
readf,1,a
readf,1,format='(5x,a1,6(e10.4,a1))',a,b,a,c,a,d,a,e,a,f,a,g,a
mn2o_a_1(i,0:5) = [b,c,d,e,f,g]
readf,1,format='(5x,a1,6(e10.4,a1))',a,b,a,c,a,d,a,e,a,f,a,g,a
mn2o_a_1(i,6:11) = [b,c,d,e,f,g]
readf,1,format='(5x,a1,4(e10.4,a1))',a,b,a,c,a,d,a,e,a
mn2o_a_1(i,12:15) = [b,c,d,e]

readf,1,a
readf,1,format='(5x,a1,6(e10.4,a1))',a,b,a,c,a,d,a,e,a,f,a,g,a
mn2o_a_2(i,0:5) = [b,c,d,e,f,g]
readf,1,format='(5x,a1,6(e10.4,a1))',a,b,a,c,a,d,a,e,a,f,a,g,a
mn2o_a_2(i,6:11) = [b,c,d,e,f,g]
readf,1,format='(5x,a1,4(e10.4,a1))',a,b,a,c,a,d,a,e,a
mn2o_a_2(i,12:15) = [b,c,d,e]
endfor

; Read upper n2o

for i=0,4 do begin
readf,1,a
readf,1,format='(5x,a1,6(e10.4,a1))',a,b,a,c,a,d,a,e,a,f,a,g,a
mn2o_b_1(i,0:5) = [b,c,d,e,f,g]
readf,1,format='(5x,a1,6(e10.4,a1))',a,b,a,c,a,d,a,e,a,f,a,g,a
mn2o_b_1(i,6:11) = [b,c,d,e,f,g]
readf,1,format='(5x,a1,4(e10.4,a1))',a,b,a,c,a,d,a,e,a
mn2o_b_1(i,12:15) = [b,c,d,e]

readf,1,a
readf,1,format='(5x,a1,6(e10.4,a1))',a,b,a,c,a,d,a,e,a,f,a,g,a
mn2o_b_2(i,0:5) = [b,c,d,e,f,g]
readf,1,format='(5x,a1,6(e10.4,a1))',a,b,a,c,a,d,a,e,a,f,a,g,a
mn2o_b_2(i,6:11) = [b,c,d,e,f,g]
readf,1,format='(5x,a1,4(e10.4,a1))',a,b,a,c,a,d,a,e,a
mn2o_b_2(i,12:15) = [b,c,d,e]
endfor

close,1
; Use exponential interpolation to obtain values at 7.2 degree increment,
; starting at 245.6

tstart = 188.0
nt=19

tnew = [tstart+findgen(nt)*7.2]

tfac_n2o_a = (tnew-tn2o_a_1)/(tn2o_a_2-tn2o_a_1)
mn2o_a = fltarr(9,16,nt)

tfac_n2o_b = (tnew-tn2o_b_1)/(tn2o_b_2-tn2o_b_1)
mn2o_b = fltarr(5,16,nt)


for j=0,15 do begin
    for k=0,nt-1 do begin
	for m=0,8 do begin	
	mn2o_a(m,j,k) =  mn2o_a_1(m,j)*(mn2o_a_2(m,j)/mn2o_a_1(m,j))^tfac_n2o_a(k)
	endfor
    endfor
endfor

for j=0,15 do begin
    for k=0,nt-1 do begin
	for m=0,4 do begin	
	mn2o_b(m,j,k) =  mn2o_b_1(m,j)*(mn2o_b_2(m,j)/mn2o_b_1(m,j))^tfac_n2o_b(k)
	endfor
    endfor
endfor

; Write out new file to be inserted into k_gB??.f file

for i=0,15 do begin
    for j=0,8 do begin
	printf,2,format='(a20,i2,a4,i2,a13)','      DATA (KA_MN2O(',j+1,',JT,',i+1,'),JT=1,19)  /'
	printf,2,format='(5x,a1,5(1x,e11.5,a1))','&',mn2o_a(j,i,0),',',   $
		mn2o_a(j,i,1),',',mn2o_a(j,i,2),',',mn2o_a(j,i,3),',', $
		mn2o_a(j,i,4),','
	printf,2,format='(5x,a1,5(1x,e11.5,a1))','&',mn2o_a(j,i,5),',',   $
		mn2o_a(j,i,6),',',mn2o_a(j,i,7),',',mn2o_a(j,i,8),',', $
		mn2o_a(j,i,9),','
	printf,2,format='(5x,a1,5(1x,e11.5,a1))','&',mn2o_a(j,i,10),',',   $
		mn2o_a(j,i,11),',',mn2o_a(j,i,12),',',mn2o_a(j,i,13),',', $
		mn2o_a(j,i,14),','
	printf,2,format='(5x,a1,4(1x,e11.5,a1))','&',mn2o_a(j,i,15),',',   $
		mn2o_a(j,i,16),',',mn2o_a(j,i,17),',',mn2o_a(j,i,18),'/'
    endfor
endfor

for i=0,15 do begin
    for j=0,4 do begin
	printf,2,format='(a20,i2,a4,i2,a13)','      DATA (KB_MN2O(',j+1,',JT,',i+1,'),JT=1,19)  /'
	printf,2,format='(5x,a1,5(1x,e11.5,a1))','&',mn2o_b(j,i,0),',',   $
		mn2o_b(j,i,1),',',mn2o_b(j,i,2),',',mn2o_b(j,i,3),',', $
		mn2o_b(j,i,4),','
	printf,2,format='(5x,a1,5(1x,e11.5,a1))','&',mn2o_b(j,i,5),',',   $
		mn2o_b(j,i,6),',',mn2o_b(j,i,7),',',mn2o_b(j,i,8),',', $
		mn2o_b(j,i,9),','
	printf,2,format='(5x,a1,5(1x,e11.5,a1))','&',mn2o_b(j,i,10),',',   $
		mn2o_b(j,i,11),',',mn2o_b(j,i,12),',',mn2o_b(j,i,13),',', $
		mn2o_b(j,i,14),','
	printf,2,format='(5x,a1,4(1x,e11.5,a1))','&',mn2o_b(j,i,15),',',   $
		mn2o_b(j,i,16),',',mn2o_b(j,i,17),',',mn2o_b(j,i,18),'/'
    endfor
endfor

close,2

end


