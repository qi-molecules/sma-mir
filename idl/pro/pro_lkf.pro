pro pro_lkf ,stri
;
; Look for .pro files with stri in first non-bank comment.
; eg. : pro_lkf ,'read'  to list all files w/ 'read'
; or  : pro_lkf ,''  to list all files
;
common global
cmd=strarr(1) & cmd(0)='\ls -1 '+e.idl_pro+'*.pro' & spawn,cmd,/sh,pro_list
list=pro_list
uti_stri_replace ,list,'.pro',''
uti_stri_replace ,list,e.idl_pro,''
line=''
for i=0,n_elements(pro_list)-1 do begin
istart=0
openr,unit,pro_list(i),/get_lun
while not eof(unit) do begin
 readf,unit,line
 if strpos(line,';',0) ne -1 and strupcase(line) ne strlowcase(line) then begin
   if strpos(strupcase(line),strupcase(stri),0) ne -1 then begin
    reads,line,line,format='(1x,a)'     
    print,strupcase(strtrim(list(i),2L)),' :',strlowcase(line)
   endif
   goto, jump
 endif 
endwhile
jump: 
close,unit & free_lun,unit

endfor
end
