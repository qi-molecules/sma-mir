pro pro_hlp ,str_par,variable=variable,structure=structure,program=program
;
; Gets comment/help lines for a .pro file, variable or struct.
; Converts string to lowercase and adds wildcards to 
; beginning and end of file string. Looks only in rdx/idl/pro.
; keyword : variable -- get comments on variable str_par
;                       or variable=variable
; keyword : structure -- get description of structure
;                        if simply /structure, then print all
; keyword : program -- programming help
; 
; eg. : pro_hlp,'chan_read'
; eg. : pro_hlp,'*'  ; to do all files
; eg. : pro_hlp,'rea'  ; to do all files w/ an rea in name
; eg. : pro_hlp,variable='inhid' ; to get information on variable inhid   
; eg. : pro_hlp,/structure ; to list all structures 
; eg. : pro_hlp,structure='in' ; to list just the 'in' structure   
; eg. : pro_hlp,/program  ; to get help on idl mir programming  
;
common global
if n_params() eq 0 then begin
  if not keyword_set(variable) and not keyword_set(structure) and $
     not keyword_set(program) then begin
    pro_hlp,'pro_hlp'
    return
  endif
  goto, keys
endif
file=str_par
cmd=strarr(1) & cmd(0)='\ls -1 '+e.idl_pro+'*'+strlowcase(file)+'*.pro' & spawn,cmd,/sh,pro_list
if pro_list(0) eq '' then begin
  variable=str_par
  goto, keys
endif
list=pro_list
uti_stri_replace ,list,'.pro',''
uti_stri_replace ,list,e.idl_pro,''

for i=0,n_elements(pro_list)-1 do begin
  openr,unit,pro_list(i),/get_lun,error=err
  if err ne 0 then begin
    goto, en
  endif
  close,unit & free_lun,unit
  result=fil_read(pro_list(i),lines,nlines)
  istart=-1
  istop=-1
  for l=0,nlines-1 do begin
        if (istart eq -1 and strpos(lines(l),';',0) eq 0) then istart=l 
        if (istart ne -1 and strpos(lines(l),';',0) eq 0) then istop=l 
        if (istart ne -1 and istop ne -1 and $
            strpos(lines(l),';',0) ne 0) then   goto, pr 
  endfor
  pr: line=''
  for l=istart,istop do begin
    reads,lines(l),line,format='(1x,a)'
    if line eq ';' then line = ' '
    print,strupcase(strtrim(list(i),2L)),' : ',line
  endfor
  en:
endfor

keys:
if keyword_set(variable) then begin
  j=where(strpos(ms.stag,variable,0) ne -1)
  if max(j) ne -1 then begin
    for i=0,n_elements(j)-1 do begin
      k=j(i)
      print,format='(a," ",a,"  ,tagno: ",i5,'+ $
         '"  ,label: ",a,"  ,format: ",a,"  ,type: ",a)', $
          ms.stag(k),ms.description(k),ms.tagno(k), $
          ms.label(k),ms.format(k),ms.type(k)          
    endfor
  endif
 endif
if keyword_set(structure) then begin
;
; if keyword structure is set but not set to something, do all
;
  structures=structure
  if strtrim(string(structure),2) eq '1' then structures=ms.ss
  print,structures
  for l=0,n_elements(structures)-1 do begin
  structure=structures(l)
  j=where(strpos(ms.ss,structure,0) ne -1)
  if max(j) ne -1 then begin
    for i=0,n_elements(j)-1 do begin
      k=j(i)
      print,format='(:/,"structure: ",a," *** ",a," ***  ,# tags: ",i5)',+ $
          ms.ss(k),ms.ss_label(k),ms.ntags(k)          
    endfor
  endif
  j=where(strpos(ms.s,structure,0) ne -1)
  if max(j) ne -1 then begin
    for i=0,n_elements(j)-1 do begin
      k=j(i)
      print,format='(T1,a,T13," ",T14,a,T38,"  tag #: ",i2,'+ $
         'T50,"  label: ",a,T68,"  ",a,T75," ",a)', $
          ms.stag(k),ms.description(k),ms.tagno(k), $
          ms.label(k),ms.format(k),ms.type(k)          
    endfor
  endif
  endfor
 
endif
if keyword_set(program) then pro_hlp,'pro_idl'
end
