function dat_var_in ,var_list=var_list,do_file=do_file,do_labels=do_labels,struc=struc
;
; Prints list of var's in in structure in var_list.
;
; keyword : var_list -- list of variables to be printed
; keyword : do_file -- put output in a file list_in
; keyword : do_labels -- generate column header every 20 lines
; keyword : struc -- put results in struct and return in result
; result = -1 (error), 1(ok)
; eg. : var_list=['conid','cocd','traid','inhid','int','tq',
;                 'az','el','ha','ut','ref_time','dhrs',vc','vctype',
;                 'sx','sy','sz','rinteg','proid','souid','source','pos',
;                 'offx','offy','offtype','ra','dec','rar','decr',
;                 'epoch','sflux','size']
; eg. : result=dat_var_in(var_list=var_list,/do_file)
; eg. : result=dat_var_in(/do_file)
; eg. : result=dat_var_in(/struc)
;
common global
common data_set

if  keyword_set(do_file) then begin
  file='in_list'
  openw,unit,file,/get_lun,error=err
endif
;
; find printing information on all variables
;
explicit=0
if (keyword_set(var_list) or (not keyword_set(do_file))) and (not keyword_set(struc)) then begin
explicit=1
nvar_pos=3*n_elements(var_list)-1
ss=make_array(nvar_pos,/string)
stags=make_array(nvar_pos,/string)
tags=make_array(nvar_pos,/string)
tagnos=make_array(nvar_pos,/int)
labels=make_array(nvar_pos,/string)
formats=make_array(nvar_pos,/string)
justifys=make_array(nvar_pos,/int)
max_lengths=make_array(nvar_pos,/int)
nvar=-1L
for i=0,n_elements(var_list)-1 do begin
  if not dat_var_info(var_list(i),s,stag,tag,tagno,pointer_str,'f',label,format, $
         justify,type,itype,description,format_long, $
         max_length,condition,c_code,i_code,/all_info) then return,-1

  for j=0L,n_elements(stag)-1L do begin
    if s[j] eq 'in' or s[j] eq 'c' then begin
     ss(nvar+1)=s(j) & stags(nvar+1)=stag(j) 
     tags(nvar+1)=tag(j) & tagnos(nvar+1)=tagno(j) & formats(nvar+1)=format(j)
     labels(nvar+1)=label(j) & justifys(nvar+1)=justify(j) 
     max_lengths(nvar+1)=max_length(j)
     if keyword_set(do_format_long) then formats(nvar+1)=format_long(j)
    endif
  endfor
  nvar=nvar+1
endfor

ts=make_array(nvar+2,/int)
tsl=make_array(nvar+2,/int)
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
for i=0,nvar do begin
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
    (ss(i) eq 'in') : var_str=var_str+'in(piuniq(i)).'+tags(i)
    (ss(i) eq 'c')  : begin
      if c.icode_s(tagnos(i)) eq 'in' then pstr='(piuniq(i)).'
      var_str=var_str+ss(i)+'.'+tags(i)+'('+ $
          c.icode_s(tagnos(i))+pstr+c.icode_tag(tagnos(i))+')'      
    end
  else : print,'unrecognized structure :',ss(i)
  endcase
endfor
format=format+')'
format_lab=format_lab+')'
print_str='print,format="'
if  keyword_set(do_file) then print_str='printf,'+strtrim(string(unit),2)+',format="'
cmd_lab=print_str+format_lab+'",'+lab_str
cmd=print_str+format+'",'+var_str
endif
piuniq=uti_distinct(pil,ndistinct,/many_repeat)
if keyword_set(struc) then begin
   inl={conid:in(piuniq(*)).conid,cocd:c.cocd(in(piuniq(*)).icocd), $
traid:in(piuniq(*)).traid,inhid:in(piuniq(*)).inhid, $
int:in(piuniq(*)).int,tq:c.tq(in(piuniq(*)).itq), $
az:in(piuniq(*)).az,el:in(piuniq(*)).el,ha:in(piuniq(*)).ha, $
ut:c.ut(in(piuniq(*)).iut),ref_time:c.ref_time(in(piuniq(*)).iref_time), $
dhrs:in(piuniq(*)).dhrs,vc:in(piuniq(*)).vc, $
vctype:c.vctype(in(piuniq(*)).ivctype),sx:in(piuniq(*)).sx, $
sy:in(piuniq(*)).sy,sz:in(piuniq(*)).sz,rinteg:in(piuniq(*)).rinteg, $
proid:in(piuniq(*)).proid,souid:in(piuniq(*)).souid, $
source:c.source(in(piuniq(*)).isource),pos:c.pos(in(piuniq(*)).ipos), $
offx:in(piuniq(*)).offx,offy:in(piuniq(*)).offy, $
offtype:c.offtype(in(piuniq(*)).iofftype),ra:c.ra(in(piuniq(*)).ira), $
dec:c.dec(in(piuniq(*)).idec),rar:in(piuniq(*)).rar, $
decr:in(piuniq(*)).decr,epoch:in(piuniq(*)).epoch, $
sflux:in(piuniq(*)).sflux,sixe:in(piuniq(*)).size}

   return,inl
endif
for i=0,n_elements(piuniq)-1 do begin
  if i mod 20 eq 0 and keyword_set(do_labels) then begin
   if not explicit then printf,unit,format="(:/,T6,a,T12,a,T18,a,T29,a,T37,a,T43,a,T57,a,T70,a,T83,a,T88,a,T88,a,T106,a,T121,a,T122,a,T140,a,T153,a,T166,a,T175,a,T182,a,T188,a,T192,a,T201,a,T214,a,T227,a,T229,a,T240,a,T245,a,T259,a,T271,a,T283,a,T296,a,T310,a)","conid","icocd","traid","inhid","int","itq","az","el","ha","iut","iref_time","dhrs","vc","ivctype","sx","sy","sz","rinteg","proid","souid","isource","ipos","offx","offy","iofftype","ira","idec","rar","decr","epoch","sflux","size"
   if explicit then     result=execute(cmd_lab)
  endif
if not explicit then printf,unit,format="(T1,i10,T12,i5,T18,i5,T24,i10,T35,i5,T41,i5,T47,g12.6,T60,g12.6,T73,g12.6,T86,i5,T92,i5,T98,g12.6,T111,g12.6,T124,i5,T130,g12.6,T143,g12.6,T156,g12.6,T169,g12.6,T182,i5,T188,i5,T194,i5,T200,i5,T206,g12.6,T219,g12.6,T232,i5,T238,i5,T244,i5,T250,g12.6,T263,g12.6,T276,g12.6,T289,g12.6,T302,g12.6)",in(piuniq(i)).conid,in(piuniq(i)).icocd,in(piuniq(i)).traid,in(piuniq(i)).inhid,in(piuniq(i)).int,in(piuniq(i)).itq,in(piuniq(i)).az,in(piuniq(i)).el,in(piuniq(i)).ha,in(piuniq(i)).iut,in(piuniq(i)).iref_time,in(piuniq(i)).dhrs,in(piuniq(i)).vc,in(piuniq(i)).ivctype,in(piuniq(i)).sx,in(piuniq(i)).sy,in(piuniq(i)).sz,in(piuniq(i)).rinteg,in(piuniq(i)).proid,in(piuniq(i)).souid,in(piuniq(i)).isource,in(piuniq(i)).ipos,in(piuniq(i)).offx,in(piuniq(i)).offy,in(piuniq(i)).iofftype,in(piuniq(i)).ira,in(piuniq(i)).idec,in(piuniq(i)).rar,in(piuniq(i)).decr,in(piuniq(i)).epoch,in(piuniq(i)).sflux,in(piuniq(i)).size
if explicit then result=execute(cmd)
endfor
;
; close file, bring up editor, remove file if only editor
;
if keyword_set(do_file) then begin
  close,unit & free_lun,unit
endif

   
return,1
end
