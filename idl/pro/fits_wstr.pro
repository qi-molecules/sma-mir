function fits_wstr,name,value,comment
;
; write fits string
;
common fits,str_buf,unit
apost="'"
res=fits_buff(string(name,apost,value,apost,comment,format='(a,T9,"=",T11,a,a,a,T32,"/",T35,a,T81)'))
return,1
end





