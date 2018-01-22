function fits_wend
;
; write fits end
;
common fits,str_buf,unit
print,'fits end: str_buf = ',strlen(str_buf)
res=fits_buff(string('END',format='(a,T81)'),/flush)
return,1
end






