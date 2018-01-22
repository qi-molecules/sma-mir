pro uti_stri_replace ,lines,ori,new
;
; Replaces all occurances of ori w/ new in string array.
; eg. : uti_stri_replace ,lines,ori,new
;
i=0L
len=strlen(ori)
while i lt n_elements(lines) do begin
 pos=strpos(lines(i),ori)
 if pos ne -1L then begin
 cpos=0L
 line=''
 while pos ne -1 do begin
   line=line + strmid(lines(i),cpos,(pos-cpos)) + new & cpos=pos+len
   pos=strpos(lines(i),ori,cpos)
 endwhile
 line=line+strmid(lines(i),cpos,strlen(lines(i))-cpos)
 lines(i)=line
 endif
 i=i+1L
endwhile
end

