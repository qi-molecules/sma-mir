function dat_var_sp ,var_list=var_list,do_file=do_file,do_labels=do_labels,struc=struc
;
; Prints list of var's in in structure in var_list.
;
; keyword : var_list -- list of variables to be printed
; keyword : do_file -- put output in a file list_sp
; keyword : struc -- put results in struct and return in result
; keyword : do_labels -- generate column header every 20 lines
; result = -1 (error), 1(ok)
; eg. : var_list=[]
; eg. : result=dat_var_sp(var_list=var_list,/do_file)
; eg. : result=dat_var_sp(/struc)
; eg. : result=dat_var_sp(/do_file)
;
common global
common data_set

if  keyword_set(do_file) then begin
  file='sp_list'
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
    if s[j] eq 'sp' or s[j] eq 'c' then begin
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
    (ss(i) eq 'sp') : var_str=var_str+'sp(psuniq(i)).'+tags(i)
    (ss(i) eq 'c')  : begin
      if c.icode_s(tagnos(i)) eq 'sp' then pstr='(psuniq(i)).'
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

psuniq=psl
if keyword_set(struc) then begin
   spl={sphid:sp(psuniq(*)).sphid,blhid:sp(psuniq(*)).blhid,inhid:sp(psuniq(*)).inhid, $
gq:c.gq(sp(psuniq(*)).igq),pq:c.pq(sp(psuniq(*)).ipq),band:c.band(sp(psuniq(*)).iband), $
pstate:c.pstate(sp(psuniq(*)).ipstate),tau0:sp(psuniq(*)).tau0,vel:sp(psuniq(*)).vel, $
vres:sp(psuniq(*)).vres,vtype:c.vtype(sp(psuniq(*)).ivtype),fsky:sp(psuniq(*)).fsky, $
fres:sp(psuniq(*)).fres,tssb:sp(psuniq(*)).tssb,integ:sp(psuniq(*)).integ, $
wt:sp(psuniq(*)).wt,taper:c.taper(sp(psuniq(*)).itaper), $
snoise:sp(psuniq(*)).snoise,nch:sp(psuniq(*)).nch,nrec:sp(psuniq(*)).nrec, $
dataoff:sp(psuniq(*)).dataoff,linid:sp(psuniq(*)).linid, $
trans:c.trans(sp(psuniq(*)).itrans),rfreq:sp(psuniq(*)).rfreq, $
pasid:sp(psuniq(*)).pasid,gaiidamp:sp(psuniq(*)).gaiidamp, $
gaiidpha:sp(psuniq(*)).gaiidpha,flcid:sp(psuniq(*)).flcid,atmid:sp(psuniq(*)).atmid}
   return,spl
endif
for i=0,n_elements(psuniq)-1 do begin
  if i mod 20 eq 0 and keyword_set(do_labels) then begin
if not explicit then printf,unit,format="(:/,T6,a,T17,a,T28,a,T34,a,T36,a,T40,a,T43,a,T54,a,T68,a,T80,a,T85,a,T102,a,T115,a,T128,a,T140,a,T156,a,T159,a,T167,a,T176,a,T181,a,T189,a,T197,a,T203,a,T231,a,T237,a,T240,a,T246,a,T255,a,T261,a)","sphid","blhid","inhid","gq","pq","band","pstate","tau0","vel","vres","vtype","fsky","fres","tssb","integ","wt","taper","snoise","nch","nrec","dataoff","linid","trans","rfreq","pasid","gaiidamp","gaiidpha","flcid","atmid"
    if  explicit then    result=execute(cmd_lab)
  endif
if not explicit then printf,108,format="(T1,i10,T12,i10,T23,i10,T34,a,T36,a,T40,a,T43,a,T46,g12.6,T59,g12.6,T72,g12.6,T85,a,T94,g12.6,T107,g12.6,T120,g12.6,T133,g12.6,T146,g12.6,T159,a,T161,g12.6,T174,i5,T180,i5,T186,i10,T197,i5,T203,a,T224,g12.6,T237,i5,T243,i5,T249,i5,T255,i5,T261,i5)",sp(psuniq(i)).sphid,sp(psuniq(i)).blhid,sp(psuniq(i)).inhid,c.gq(sp(psuniq(i)).igq),c.pq(sp(psuniq(i)).ipq),c.band(sp(psuniq(i)).iband),c.pstate(sp(psuniq(i)).ipstate),sp(psuniq(i)).tau0,sp(psuniq(i)).vel,sp(psuniq(i)).vres,c.vtype(sp(psuniq(i)).ivtype),sp(psuniq(i)).fsky,sp(psuniq(i)).fres,sp(psuniq(i)).tssb,sp(psuniq(i)).integ,sp(psuniq(i)).wt,c.taper(sp(psuniq(i)).itaper),sp(psuniq(i)).snoise,sp(psuniq(i)).nch,sp(psuniq(i)).nrec,sp(psuniq(i)).dataoff,sp(psuniq(i)).linid,c.trans(sp(psuniq(i)).itrans),sp(psuniq(i)).rfreq,sp(psuniq(i)).pasid,sp(psuniq(i)).gaiidamp,sp(psuniq(i)).gaiidpha,sp(psuniq(i)).flcid,sp(psuniq(i)).atmid
if explicit then  result=execute(cmd)
endfor
;
; close file, bring up editor, remove file if only editor
;
if keyword_set(do_file) then begin
  close,unit & free_lun,unit
endif

   
return,1
end
