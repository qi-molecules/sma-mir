function fits_buff,str,init=init,flush=flush
;
; add str to buffer str_buf and write out when 2880 bytes
;
common fits,str_buf,unit
if keyword_set(init) then begin
  str_buf=''
  return,1
endif
print,str
;print,'new size of buffer ',strlen(str_buf+str)
if keyword_set(flush) then begin
  while strlen(str_buf+str) lt 2880 do begin
    str=str+string('',format='(a,T81)')
  endwhile
endif
str_buf=str_buf+str
if strlen(str_buf) eq 2880 then begin
;print,'Now writing out the 2880 byte str_buf'
  writeu,unit,str_buf
  str_buf=''
endif
return,1
end





