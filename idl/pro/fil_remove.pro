function fil_remove ,file
;
; Removes file from host file system.
; result = -1 (error) , 1 (ok)
; eg. : result=fil_remove(file)
;
count=0 & result=findfile(file,count=count)
if count ne 0 then begin
  cmd=strarr(2) & cmd(0)='rm' & cmd(1)=file & spawn,cmd,/noshell
endif
return,1
end
