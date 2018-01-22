function fits_wcmt,name,comment
;
; write fits comment
;
common fits,str_buf,unit
res=fits_buff(string(name,comment,format='(a,T9,a,T81)'))
return,1
end






