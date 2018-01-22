function fits_init,filename
;
; initialize fits file 
;
; paramters : fits_file -- fits file name
;             fits_ext -- fits extension
;
; eg result=fits_init(fits_file,fits_ext,unit)
;
common global
common fits,str_buf,unit

print,'try to open fits file ',filename

ier=0
openw,unit,filename,2880,error=ier,/get_lun

if (ier eq 0) then begin
   res=fits_buff('',/init)
   return,1
endif else begin
  print,!err_string
  return,0
endelse
end





