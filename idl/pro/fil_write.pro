function fil_write ,file,lines
;
; Writes an ascii file from string array.
; result = -1 (error) , 1 (ok)
; eg. : result=fil_write(file,lines)
;
openw,unit,file,/get_lun,error=err
if err ne 0 then begin
  print,!err_string
  return,-1
endif
i=0L
while i lt n_elements(lines) do begin
 printf,unit,lines(i)
 i=i+1L
endwhile
close,unit & free_lun,unit
return,1
end
