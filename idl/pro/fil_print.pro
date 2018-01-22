function fil_print ,file,remove=remove
;
; Prints file from host file system and removes if /remove
; result = -1 (error) , 1 (ok)
; eg. : result=fil_print(file)
;
common global
cmd=strarr(4) & cmd(0)='lpr' & cmd(1)='-P' & cmd(2)=e.printer & cmd(3)=file & spawn,cmd,/noshell
if keyword_set(remove) then result=fil_remove(file)
return,1
end
