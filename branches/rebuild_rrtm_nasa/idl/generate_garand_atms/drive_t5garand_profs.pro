; Add gas factors before writing the profiles
result = file_test('input_scalefacs')
if (result eq 1) then begin
  @input_scalefacs
  file_mkdir,rrtmpdir
  file_mkdir,lblpdir
  file_copy,'input_scalefacs',rrtmpdir,/REQUIRE_DIRECTORY,/OVERWRITE
  file_copy,'input_scalefacs',lblpdir,/REQUIRE_DIRECTORY,/OVERWRITE
endif else begin
  print,'input_scalefacs file required; does not exist in current directory'
endelse
get_garand_profs,tprofg,scalefacs=scalefacs
file_copy,'42_newprofiles.txt',rrtmpdir,/OVERWRITE
file_copy,'42_newprofiles.txt',lblpdir,/OVERWRITE
fnamein = rrtmpdir+'/'+atmfile
save,tprofg,filename=fnamein

nprofs=n_elements(tprofg)
write_t5garand_profs,tprofg,filehdr=filehdr,rrtmpdir=rrtmpdir,lblpdir=lblpdir,splitprof=splitprof

;cd,rrtmpdir
;plot_profile,atmfile

end