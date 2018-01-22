function fil_app_str ,file,lines,nlines
;
; Reads an ascii file into string array.
; result = -1 (couldn't open) 1 (ok)
; appends results onto an existing string array
; if file can't be opened - leaves str array as is
; eg. : result=fil_app_str(file,lines,nlines)
;
openr,unit,file,/get_lun,error=err
if err ne 0 then begin
  print,!err_string
  return,-1
endif
line=''
while not eof(unit) do begin
 readf,unit,line
 lines=[lines,line]
endwhile
nlines=n_elements(lines)
close,unit & free_lun,unit
return,1
end
