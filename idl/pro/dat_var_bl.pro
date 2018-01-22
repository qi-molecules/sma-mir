function dat_var_bl ,var_list=var_list,do_file=do_file,do_labels=do_labels,struc=struc
;
; Prints list of var's in in structure in var_list.
;
; keyword : var_list -- list of variables to be printed
; keyword : do_file -- put output in a file list_bl
; keyword : do_labels -- generate column header every 20 lines
; keyword : struc -- put results in struct and return in result
; result = -1 (error), 1(ok)
; eg. : var_list=['blhid','inhid','sb','pol','pa','aq','bq','cq','oq','rec',
;                 'ifc','u','v','w','prbl','angres','vis','coh','sigcoh',
;                 'csnr','vflux','cnoise','avedhrs','ampave','phaave',
;                 'tpvar','blsid','tel1','tel2','blcd','ble','bln','blu','soid']
; eg. : result=dat_var_bl(var_list=var_list,/do_file)
; eg. : result=dat_var_bl(/struc)
; eg. : result=dat_var_bl(/do_file)
;
common global
common data_set

if  keyword_set(do_file) then begin
  file='bl_list'
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
    if s[j] eq 'bl' or s[j] eq 'c' then begin
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
    (ss(i) eq 'bl') : var_str=var_str+'bl(pbuniq(i)).'+tags(i)
    (ss(i) eq 'c')  : begin
      if c.icode_s(tagnos(i)) eq 'bl' then pstr='(pbuniq(i)).'
      var_str=var_str+ss(i)+'.'+tags(i)+'('+ $
          c.icode_s(tagnos(i))+pstr+c.icode_tag(tagnos(i))+')'      
    end
  else : print,'unrecognized structure :',i,ss(i)
  endcase
endfor
format=format+')'
format_lab=format_lab+')'
print_str='print,format="'
if  keyword_set(do_file) then print_str='printf,'+strtrim(string(unit),2)+',format="'
cmd_lab=print_str+format_lab+'",'+lab_str
cmd=print_str+format+'",'+var_str
endif

pbuniq=uti_distinct(pbl,ndistinct,/many_repeat)
if keyword_set(struc) then begin
   bll={blhid:bl(pbuniq(*)).blhid,inhid:bl(pbuniq(*)).inhid, $
sb:c.sb(bl(pbuniq(*)).isb),pol:c.pol(bl(pbuniq(*)).ipol),pa:bl(pbuniq(*)).pa, $
aq:c.aq(bl(pbuniq(*)).iaq),bq:c.bq(bl(pbuniq(*)).ibq),cq:c.cq(bl(pbuniq(*)).icq), $
oq:c.oq(bl(pbuniq(*)).ioq),rec:c.rec(bl(pbuniq(*)).irec), $
ifc:c.ifc(bl(pbuniq(*)).iifc),u:bl(pbuniq(*)).u,v:bl(pbuniq(*)).v, $
w:bl(pbuniq(*)).w,prbl:bl(pbuniq(*)).prbl,angres:bl(pbuniq(*)).angres, $
vis:bl(pbuniq(*)).vis,coh:bl(pbuniq(*)).coh,sigcoh:bl(pbuniq(*)).sigcoh, $
csnr:bl(pbuniq(*)).csnr,vflux:bl(pbuniq(*)).vflux, $
cnoise:bl(pbuniq(*)).cnoise,avedhrs:bl(pbuniq(*)).avedhrs, $
ampave:bl(pbuniq(*)).ampave,phaave:bl(pbuniq(*)).phaave, $
tpvar:bl(pbuniq(*)).tpvar,blsid:bl(pbuniq(*)).blsid, $
tel1:c.tel1(bl(pbuniq(*)).itel1),tel2:c.tel2(bl(pbuniq(*)).itel2), $
blcd:c.blcd(bl(pbuniq(*)).iblcd),ble:bl(pbuniq(*)).ble, $
bln:bl(pbuniq(*)).bln,blu:bl(pbuniq(*)).blu,soid:bl(pbuniq(*)).soid}
   return,bll
endif
for i=0,n_elements(pbuniq)-1 do begin
  if i mod 20 eq 0 and keyword_set(do_labels) then begin
   if not explicit then printf,unit,format="(:/,T6,a,T17,a,T23,a,T25,a,T38,a,T41,a,T44,a,T46,a,T48,a,T50,a,T54,a,T69,a,T82,a,T95,a,T105,a,T116,a,T132,a,T145,a,T155,a,T170,a,T182,a,T194,a,T206,a,T220,a,T233,a,T247,a,T258,a,T264,a,T269,a,T274,a,T292,a,T305,a,T318,a,T328,a)","blhid","inhid","sb","pol","pa","aq","bq","cq","oq","rec","ifc","u","v","w","prbl","angres","vis","coh","sigcoh","csnr","vflux","cnoise","avedhrs","ampave","phaave","tpvar","blsid","tel1","tel2","blcd","ble","bln","blu","soid"
   if explicit then result=execute(cmd_lab)
  endif
if not explicit then printf,unit,format="(T1,i10,T12,i10,T23,a,T25,a,T28,g12.6,T41,a,T44,a,T46,a,T48,a,T50,a,T54,a,T58,g12.6,T71,g12.6,T84,g12.6,T97,g12.6,T110,g12.6,T123,g12.6,T136,g12.6,T149,g12.6,T162,g12.6,T175,g12.6,T188,g12.6,T201,g12.6,T214,g12.6,T227,g12.6,T240,g12.6,T253,i10,T264,a,T269,a,T274,a,T283,g12.6,T296,g12.6,T309,g12.6,T322,i10)",bl(pbuniq(i)).blhid,bl(pbuniq(i)).inhid,c.sb(bl(pbuniq(i)).isb),c.pol(bl(pbuniq(i)).ipol),bl(pbuniq(i)).pa,c.aq(bl(pbuniq(i)).iaq),c.bq(bl(pbuniq(i)).ibq),c.cq(bl(pbuniq(i)).icq),c.oq(bl(pbuniq(i)).ioq),c.rec(bl(pbuniq(i)).irec),c.ifc(bl(pbuniq(i)).iifc),bl(pbuniq(i)).u,bl(pbuniq(i)).v,bl(pbuniq(i)).w,bl(pbuniq(i)).prbl,bl(pbuniq(i)).angres,bl(pbuniq(i)).vis,bl(pbuniq(i)).coh,bl(pbuniq(i)).sigcoh,bl(pbuniq(i)).csnr,bl(pbuniq(i)).vflux,bl(pbuniq(i)).cnoise,bl(pbuniq(i)).avedhrs,bl(pbuniq(i)).ampave,bl(pbuniq(i)).phaave,bl(pbuniq(i)).tpvar,bl(pbuniq(i)).blsid,c.tel1(bl(pbuniq(i)).itel1),c.tel2(bl(pbuniq(i)).itel2),c.blcd(bl(pbuniq(i)).iblcd),bl(pbuniq(i)).ble,bl(pbuniq(i)).bln,bl(pbuniq(i)).blu,bl(pbuniq(i)).soid

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
