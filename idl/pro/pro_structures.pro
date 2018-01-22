function pro_structures ,struct_list,struct_labels, $
                     derive=derive,ascii=ascii, $
                     save=save,merge=merge
;
; Generates descriptor arrays for all mir structures
; including the ms structure (created here) to 
; describe the others.
; keyword : derive => let idl derive the ms structure
; keyword : save   => save the ms structure in a restore file
;                     (this file is read on startup)
; keyword : ascii  => save the ms default structure and formats 
;                    in ascii file (normally done after derive)
; keyword : merge  => merge the variable info from the ascii file
;                     and the database MIR_VARS table 
; Outputs : 
; eg. : result=pro_structures(['e','in','bl','sp','c','re','ca','pl', $
; eg. :  's_page'],['environment','integration headers', $
; eg. :  'baseline headers','spectral headers','codes','records', $
; eg. :   'calibration','plot','plot cont ...'],/derive,/save, $
; eg. :    /ascii,/merge)
;
common global
common data_set
common plo
common wlm
ms={ss:'1',ss_label:'1',ntags:1L,s:'1',stag:'1',tag:'1',tagno:1L,label:'1', $
    format:'1',justify:-1,type:'1',itype:1L,description:'1', $
    format_long:'1',max_length:1L}
ss=[struct_list,'ms']
ss_label=[struct_labels,'mir structures']
ntags=make_array(300,/long)
s=make_array(300,/string)
stag=make_array(300,/string)
tag=make_array(300,/string)
tagno=make_array(300,/long)
label=make_array(300,/string)
form=make_array(300,/string)
justify=make_array(300,/int)
type=make_array(300,/string)
itype=make_array(300,/long)
description=make_array(300,/string)
form_long=make_array(300,/string)
max_length=make_array(300,/long)
types=['undef','byte','int','long','float','double','complex', $
       'string','structure','dp complex','pointer','object']

print,'made it this far: defined arrays'

is=0L
for i=0,n_elements(ss)-1 do begin
  cmd='vars=tag_names('+ss(i)+')'
  result=execute(cmd)
  nvar=n_elements(vars)
  sizes=make_array(nvar,/long)
  for j=0,nvar-1 do begin
    cmd='size_temp=size('+ss(i)+'.'+vars(j)+')'
    result=execute(cmd)
    sizes(j)=size_temp(n_elements(size_temp)-2) ; 2'nd from last
  endfor
  ntags(i)=nvar
  s(is:is+nvar-1)=ss(i)
  stag(is:is+nvar-1)=ss(i)+'.'+vars
  tag(is:is+nvar-1)=vars
  tagno(is:is+nvar-1)=lindgen(nvar)
  label(is:is+nvar-1)=''
  form(is:is+nvar-1)=''
  justify(is:is+nvar-1)=-1
  type(is:is+nvar-1)=types(sizes)
  itype(is:is+nvar-1)=sizes
  description(is:is+nvar-1)='-'
  form_long(is:is+nvar-1)=''
  max_length(is:is+nvar-1)=26
  is=is+nvar
endfor
nvar=is

print,'made it this far: defined more arrays'

; add in non-structure arrays : ch,pi,pb,ps,pif,pbf,psf;
;                               prf,pcf,pil,pbl,psl
;
s(nvar)='' & stag(nvar)='ch' & tag(nvar)='ch'
tagno(nvar)=0 & label(nvar)='data' & type(nvar)='complex'
itype(nvar)=6 & description(nvar)='records & channels'
nvar=nvar+1
s(nvar)='' & stag(nvar)='pi' & tag(nvar)='pi'
tagno(nvar)=0 & label(nvar)='p(in)' & type(nvar)='int'
itype(nvar)=2 & description(nvar)='pointer to in'
nvar=nvar+1
s(nvar)='' & stag(nvar)='pb' & tag(nvar)='pb'
tagno(nvar)=0 & label(nvar)='p(bl)' & type(nvar)='int'
itype(nvar)=2 & description(nvar)='pointer to bl'
nvar=nvar+1
s(nvar)='' & stag(nvar)='ps' & tag(nvar)='ps'
tagno(nvar)=0 & label(nvar)='p(sp)' & type(nvar)='int'
itype(nvar)=2 & description(nvar)='pointer to sp'
nvar=nvar+1
s(nvar)='' & stag(nvar)='pr' & tag(nvar)='pr'
tagno(nvar)=0 & label(nvar)='p(re)' & type(nvar)='int'
itype(nvar)=2 & description(nvar)='pointer to re'
nvar=nvar+1
s(nvar)='' & stag(nvar)='pc' & tag(nvar)='pc'
tagno(nvar)=0 & label(nvar)='p(ch)' & type(nvar)='int'
itype(nvar)=2 & description(nvar)='pointer to ch'
nvar=nvar+1
s(nvar)='' & stag(nvar)='pif' & tag(nvar)='pif'
tagno(nvar)=0 & label(nvar)='pf(in)' & type(nvar)='int'
itype(nvar)=2 & description(nvar)='filter pointer in'
nvar=nvar+1
s(nvar)='' & stag(nvar)='pbf' & tag(nvar)='pbf'
tagno(nvar)=0 & label(nvar)='pf(bl)' & type(nvar)='int'
itype(nvar)=2 & description(nvar)='filter pointer bl'
nvar=nvar+1
s(nvar)='' & stag(nvar)='psf' & tag(nvar)='psf'
tagno(nvar)=0 & label(nvar)='pf(sp)' & type(nvar)='int'
itype(nvar)=2 & description(nvar)='filter pointer sp'
nvar=nvar+1
s(nvar)='' & stag(nvar)='pcf' & tag(nvar)='pcf'
tagno(nvar)=0 & label(nvar)='pf(ch)' & type(nvar)='int'
itype(nvar)=2 & description(nvar)='filter pointer ch'
nvar=nvar+1
s(nvar)='' & stag(nvar)='prf' & tag(nvar)='prf'
tagno(nvar)=0 & label(nvar)='pf(re)' & type(nvar)='int'
itype(nvar)=2 & description(nvar)='filter pointer re'
nvar=nvar+1
s(nvar)='' & stag(nvar)='pil' & tag(nvar)='pil'
tagno(nvar)=0 & label(nvar)='pl(in)' & type(nvar)='int'
itype(nvar)=2 & description(nvar)='list pointer in'
nvar=nvar+1
s(nvar)='' & stag(nvar)='pbl' & tag(nvar)='pbl'
tagno(nvar)=0 & label(nvar)='pl(bl)' & type(nvar)='int'
itype(nvar)=2 & description(nvar)='list pointer bl'
nvar=nvar+1
s(nvar)='' & stag(nvar)='psl' & tag(nvar)='psl'
tagno(nvar)=0 & label(nvar)='pl(sp)' & type(nvar)='int'
itype(nvar)=2 & description(nvar)='list pointer sp'
nvar=nvar+1
s(nvar)='' & stag(nvar)='pcl' & tag(nvar)='pcl'
tagno(nvar)=0 & label(nvar)='pl(ch)' & type(nvar)='int'
itype(nvar)=2 & description(nvar)='list pointer ch'
nvar=nvar+1
s(nvar)='' & stag(nvar)='prl' & tag(nvar)='prl'
tagno(nvar)=0 & label(nvar)='pl(re)' & type(nvar)='int'
itype(nvar)=2 & description(nvar)='list pointer re'
nvar=nvar+1

print,'made it this far: filled in some arrays'

; truncate arrays, set up default formats, and put in structure

s=strlowcase(s(0:nvar-1))
stag=strlowcase(stag(0:nvar-1))
tag=strlowcase(tag(0:nvar-1))
tagno=tagno(0:nvar-1)
label=strlowcase(label(0:nvar-1))
form=form(0:nvar-1)
justify=justify(0:nvar-1)
type=type(0:nvar-1)
itype=itype(0:nvar-1)
description=description(0:nvar-1)
form_long=form_long(0:nvar-1)
max_length=max_length(0:nvar-1)
label=tag
j=where(type(*) eq 'string') 
if max(j) ne -1 then begin
  form(j)='a'
  form_long(j)='a'
endif
j=where(type(*) eq 'long') 
if  max(j) ne -1 then begin
  form(j)='i10'
  form_long(j)='i10'
  max_length(j)=10
  justify(j)=1
endif
j=where(type(*) eq 'int') 
if  max(j) ne -1 then begin
  form(j)='i5'
  form_long(j)='i5'
  max_length(j)=6
  justify(j)=1
endif
j=where(type(*) eq 'float') 
if  max(j) ne -1 then begin
  form(j)='g12.6'
  form_long(j)='g12.6'
  justify(j)=1
endif
j=where(type(*) eq 'double') 
if  max(j) ne -1 then begin
  form(j)='g12.6'
  form_long(j)='g20.14'
  justify(j)=1
endif
j=where(type(*) eq 'complex') 
if  max(j) ne -1 then begin
  form(j)='g12.6'
  form_long(j)='g12.6'
  justify(j)=1
endif
ms={ss:ss,ss_label:ss_label,ntags:ntags,s:s,stag:stag,tag:tag, $
    tagno:tagno,label:label,format:form,justify:justify,type:type, $
    itype:itype,description:description,format_long:form_long, $
    max_length:max_length}
;
; write out variable info
;
if keyword_set(ascii) then begin
openw,unit,e.idl_sav+'IDL_VARS',/get_lun,error=err
i=0L
while i lt n_elements(ms.tag) do begin
 printf,unit,format='(a5,a20,a15,i5,a15,a15,i3,a10,i3,a25,a10,i3)', $
          ms.s(i),ms.stag(i),ms.tag(i),ms.tagno(i), $
          ms.label(i),ms.format(i),ms.justify(i),ms.type(i), $
          ms.itype(i),ms.description(i),ms.format_long(i), $
          ms.max_length(i)
 i=i+1L
endwhile
close,unit & free_lun,unit
print,'*** ascii outut in file '+e.idl_sav+'IDL_VARS'
endif
;
; merge the db variable info with that derived by idl
; and rewrite the ascii file
;
if keyword_set(merge) then begin
;
; generate sql query to get info from MIR_VARS table
;
if e.dbprocess NE 0 then begin
sql_str=' select convert(char(5),structure),' + $
        'convert(char(20),structure_tag),' + $
        'convert(char(15),tag),' + $
        'right(str(tagno),5),' + $
        'convert(char(15),label),' + $
        'convert(char(15),format),' + $
        'right(str(justify),3),' + $
        'convert(char(10),type),' + $
        'right(str(itype),3),' + $
        'convert(char(25),description),' + $
        'convert(char(10),format_long),'+ $
        'right(str(max_length),3) from MIR_VARS '+ $
        'where ltrim(rtrim(structure_tag)) in ('
for i=0,n_elements(ms.tag)-2 do begin
  sql_str=sql_str +'"'+ms.stag(i)+'",'
endfor
sql_str=sql_str+'"'+ms.stag(n_elements(ms.stag)-1)+'") group by structure_tag '
endif
result=dbi_sql_submit(sql_str,output_file='db_vars',/no_notify)

  result=fil_read('db_vars',lines,nlines)
  j_in_db=make_array(n_elements(ms.stag),/int)
  for i=2,nlines-3 do begin
    svar=strtrim(strmid(lines(i),7,20),2L)
    j=where(ms.stag eq svar)
    if max(j) eq -1 then begin
      print,svar,' is in db table MIR_VARS but not in structures,', $
                 ' delete it from MIR_VARS !'
    endif else begin
;
; for the entries which exist in both places, use the db version
;
     
     j_in_db(j)=1
     ms.s(j)=strtrim(strmid(lines(i),0,5),2L)
     ms.stag(j)=strtrim(strmid(lines(i),7,20),2L)
     ms.tag(j)=strtrim(strmid(lines(i),28,15),2L)
     ms.tagno(j)=strtrim(strmid(lines(i),44,5),2L)
     ms.label(j)=strtrim(strmid(lines(i),50,15),2L)
     ms.format(j)=strtrim(strmid(lines(i),66,15),2L)
     ms.justify(j)=strtrim(strmid(lines(i),82,3),2L)
     ms.type(j)=strtrim(strmid(lines(i),86,10),2L)
     ms.itype(j)=strtrim(strmid(lines(i),97,3),2L)
     ms.description(j)=strtrim(strmid(lines(i),101,25),2L)
     ms.format_long(j)=strtrim(strmid(lines(i),127,10),2L)
     ms.max_length(j)=strtrim(strmid(lines(i),138,3),2L)
    endelse
  endfor
  not_in_db=where(j_in_db eq 0)
  if max(not_in_db eq -1) then begin
    print,'all existing structure variables are listed in db table MIR_VARS'
  endif else begin
    openw,unit,e.idl_sav+'MIR_VARS_NEW',/get_lun,error=err
    for j=0,n_elements(not_in_db)-1 do begin
      i=not_in_db(j)
      print,'*** ',ms.stag(i),' is a new variable !'
      printf,unit,format= $
          '(T1,a,T6,a,T26,a,T41,a,T46,a,T61,a,T76,a,T79,' + $
         'a,T89,a,T92,a,T117,a,T127,a)', $
          strtrim(string(ms.s(i)),2),strtrim(string(ms.stag(i)),2), $
          strtrim(string(ms.tag(i)),2),strtrim(string(ms.tagno(i)),2), $
          strtrim(string(ms.label(i)),2),strtrim(string(ms.format(i)),2), $
          strtrim(string(ms.justify(i)),2),strtrim(string(ms.type(i)),2), $
          strtrim(string(ms.itype(i)),2),strtrim(string(ms.description(i)),2), $
          strtrim(string(ms.format_long(i)),2), $
          strtrim(string(ms.max_length(i)),2)
    endfor
    close,unit & free_lun,unit
 print,'*** new rows to add to MIR_VARS are in file '+e.idl_sav+'MIR_VARS_NEW'
 print,'*** edit these rows to follow column spacing in '+e.idl_sav+'MIR_VARS'
 print,'*** (do not use tabs)'
 print,'*** merge them into file '+e.idl_sav+'MIR_VARS, '
 print,'*** delete all rows in the db table table MIR_VARS, then'
 print,'*** copy the file into the database w/ the command : '
 print,'*** bcp MIR_VARS in '+e.idl_sav+'MIR_VARS -f '+e.idl_bcp+'fmt_mir_vars -U rdx -P rdxrdx'
 print,'*** then copy out file w/ the command : '
 print,'*** bcp MIR_VARS out '+e.idl_sav+'MIR_VARS -f '+e.idl_bcp+'fmt_mir_vars -U rdx -P rdxrdx'
 print,'*** finally, run pro_structures once more w/ save option'
  endelse
result=fil_remove('db_vars')
endif
;
; save the {ms} in restore file
; (to restore : restore,filename='ms.save')
;
if keyword_set(save) then begin
  save,ms,filename=e.idl_sav+'ms.save'
endif 

return,1
end
