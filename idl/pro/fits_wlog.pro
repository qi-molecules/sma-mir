function fits_wlog,name,value,comment
;
; write fits log
;
common fits,str_buf,unit
res=fits_buff(string(name,value,comment,format='(a,T9,"=",T30,a,T32,"/",T34,a,T81)'))
return,1
end




