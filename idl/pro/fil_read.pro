function fil_read ,file,lines,nlines
;
; Reads an ascii file into string array.
; result = -1 (couldn't open) 1 (ok)
; eg. : result=fil_read(file,lines,nlines)
;
openr,unit,file,/get_lun,error=err
if err ne 0 then begin
  print,!err_string
  return,-1
endif
lines=''
line=''
while not eof(unit) do begin
 readf,unit,line
 lines=[lines,line]
endwhile
nlines=n_elements(lines)-1
if nlines ne 0 then begin
  lines=lines[1:*]
  nlines=nlines-1
endif
close,unit & free_lun,unit
return,1
end
