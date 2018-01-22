function fits_wint,name,value,comment
;
; write fits integer
;
common fits,str_buf,unit
res=fits_buff(string(name,value,comment,format='(a,T9,"=",T11,i20,T32,"/",T35,a,T81)'))
return,1
end


