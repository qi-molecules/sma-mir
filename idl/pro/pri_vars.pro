function pri_vars ,var_list,do_dup=do_dup,do_format_long=do_format_long, $
                    do_file=do_file,do_editor=do_editor
;
; Prints list of var's in var_list.
;
; parameters : var_list -- list of variable names in mir structures
; keyword : do_format_long -- print w/ longest formats possible
; keyword : do_file -- put print output in a file
; keyword : do_editor -- bring up editor to scan file
; result = -1 (error), 1(ok)
; eg. : var_list=['int','inhid','blhid','sphid']
; eg. : result=pri_vars(var_list)
;
common global
common data_set

;
; find printing information on all variables
;
nvar_pos=3*n_elements(var_list)-1
ss=make_array(nvar_pos,/string)
stags=make_array(nvar_pos,/string)
tags=make_array(nvar_pos,/string)
tagnos=make_array(nvar_pos,/int)
labels=make_array(nvar_pos,/string)
formats=make_array(nvar_pos,/string)
justifys=make_array(nvar_pos,/int)
max_lengths=make_array(nvar_pos,/int)
nvar=0L
for i=0,n_elements(var_list)-1 do begin
  if not dat_var_info(var_list(i),s,stag,tag,tagno,pointer_str,'f',label,format, $
         justify,type,itype,description,format_long, $
         max_length,condition,c_code,i_code,/all_info) then return,-1

  n_new=1
  if keyword_set(do_dup) then n_new=n_elements(stag)
  for j=0L,n_new-1L do begin
    ss(nvar+j)=s(j) & stags(nvar+j)=stag(j) 
    tags(nvar+j)=tag(j) & tagnos(nvar+j)=tagno(j) & formats(nvar+j)=format(j)
    labels(nvar+j)=label(j) & justifys(nvar+j)=justify(j) 
    max_lengths(nvar+j)=max_length(j)
    if keyword_set(do_format_long) then formats(nvar+j)=format_long(j)
  endfor
  nvar=nvar+n_new
endfor
ss=ss(0:nvar-1)
stags=stags(0:nvar-1)
tags=tags(0:nvar-1)
tagnos=tagnos(0:nvar-1)
labels=labels(0:nvar-1)
formats=formats(0:nvar-1)
justifys=justifys(0:nvar-1)
max_lengths=max_lengths(0:nvar-1)
ts=make_array(nvar+1,/int)
tsl=make_array(nvar+1,/int)
;
; assemble complete format
;
var_str=''
lab_str=''
format='('
format_lab='(:/,'
label=''
ts(0)=1
tsl(0)=1
for i=0,nvar-1L do begin
  if i ne 0 then format=format+','
  if i ne 0 then format_lab=format_lab+','
  dec_pos=strpos(formats(i),'.',0)
  if dec_pos eq -1 then dec_pos=strlen(formats(i))
  c_pos=strpos(formats(i),'a',0)
  if c_pos eq -1 then c_pos=strpos(formats(i),'g',0)
  if c_pos eq -1 then c_pos=strpos(formats(i),'f',0)
  if c_pos eq -1 then c_pos=strpos(formats(i),'i',0)
  if c_pos eq -1 then c_pos=0
  field_len=strmid(formats(i),c_pos+1,dec_pos-c_pos-1)
  if strpos(formats(i),'a',0) ne -1 then field_len=max_lengths(i)
  ts(i+1)=ts(i)+field_len+1
  tsl(i+1)=tsl(i)+field_len+1
  if justifys(i) eq 1 then tsl(i)=tsl(i)+field_len-strlen(labels(i))
  format=format+'T'+strtrim(string(ts(i)),1)+','+formats(i)
  format_lab=format_lab+'T'+strtrim(string(tsl(i)),1)+',a'
  if i ne 0 then lab_str=lab_str+','
  lab_str=lab_str+'"'+strtrim(labels(i),2)+'"'
  if i ne 0 then var_str=var_str+','
  pstr='.'
  case 1 of 
    (ss(i) eq 'in') : var_str=var_str+'in(pif(i)).'+tags(i)
    (ss(i) eq 'bl') : var_str=var_str+'bl(pbf(i)).'+tags(i)
    (ss(i) eq 'sp') : var_str=var_str+'sp(psf(i)).'+tags(i)
    (ss(i) eq 're') : var_str=var_str+'re(prf(i)).'+tags(i)
    (ss(i) eq 'ch') : var_str=var_str+'ch(pcf(i)).'+tags(i)
    (ss(i) eq 'c')  : begin
      if c.icode_s(tagnos(i)) eq 'in' then pstr='(pif(i)).'
      if c.icode_s(tagnos(i)) eq 'bl' then pstr='(pbf(i)).'
      if c.icode_s(tagnos(i)) eq 'sp' then pstr='(psf(i)).'
      var_str=var_str+ss(i)+'.'+tags(i)+'('+ $
          c.icode_s(tagnos(i))+pstr+c.icode_tag(tagnos(i))+')'      
    end
  else : print,'unrecognized structure :',ss(i)
  endcase
endfor
format=format+')'
format_lab=format_lab+')'
print_str='print,format="'
if  keyword_set(do_file) or  keyword_set(do_editor) then begin
  file='print_output'
  openw,unit,file,/get_lun,error=err
  print_str='printf,'+strtrim(string(unit),2)+',format="'
endif
cmd_lab=print_str+format_lab+'",'+lab_str
cmd=print_str+format+'",'+var_str
print,cmd_lab
print,cmd
for i=0,n_elements(psf)-1 do begin
  if i mod 20 eq 0 then begin
    result=execute(cmd_lab)
  endif
  result=execute(cmd)
endfor
;
; close file, bring up editor, remove file if only editor
;
if keyword_set(do_file) or  keyword_set(do_editor) then begin
  close,unit & free_lun,unit
endif
if  keyword_set(do_editor) then begin
  spawn,e.editor+' '+file
endif
if keyword_set(do_editor) and not keyword_set(do_file) then begin
  result=fil_remove(file)
endif

   
return,1
end
