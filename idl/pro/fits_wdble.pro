function fits_wdble,name,value,comment
;
; write fits real
;
common fits,str_buf,unit
res=fits_buff(string(name,value,comment,format='(a,T9,"=",T11,e20.13,T32,"/",T35,a,T81)'))
return,1
end




