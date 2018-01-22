function dbi_head_write
; 
; Write header data to bulk copy file in flat table format 
; result=-1 (error), 1(ok)
;
; parameters : none
;
; eg. : result=dbi_head_write()
;

common global
common data_set

openw,unit,e.dataset_dir+'/tmp/in_write',/get_lun,error=err
if err ne 0 then begin
  print,!err_string
  return,-1
endif
writeu,unit,in
close,unit & free_lun,unit
openw,unit,e.dataset_dir+'/tmp/bl_write',/get_lun,error=err
if err ne 0 then begin
  print,!err_string
  return,-1
endif
writeu,unit,bl
close,unit & free_lun,unit
openw,unit,e.dataset_dir+'/tmp/sp_write',/get_lun,error=err
if err ne 0 then begin
  print,!err_string
  return,-1
endif
writeu,unit,sp
close,unit & free_lun,unit
openw,unit,e.dataset_dir+'/tmp/codes_write',/get_lun,error=err
if err ne 0 then begin
  print,!err_string
  return,-1
endif
;
; what a mess ! , can probably do this better
;
for i=0L,n_elements(c.cocd)-1L do printf,unit,format='(i4,a30,a30)',i,'cocd',c.cocd(i)
for i=0L,n_elements(c.ut)-1L do printf,unit,format='(i4,a30,a30)',i,'ut',c.ut(i)
for i=0L,n_elements(c.ref_time)-1L do printf,unit,format='(i4,a30,a30)',i,'ref_time',c.ref_time(i)
for i=0L,n_elements(c.tq)-1L do printf,unit,format='(i4,a30,a30)',i,'tq',c.tq(i)
for i=0L,n_elements(c.vctype)-1L do printf,unit,format='(i4,a30,a30)',i,'vctype',c.vctype(i)
for i=0L,n_elements(c.sb)-1L do printf,unit,format='(i4,a30,a30)',i,'sb',c.sb(i)
for i=0L,n_elements(c.pol)-1L do printf,unit,format='(i4,a30,a30)',i,'pol',c.pol(i)
for i=0L,n_elements(c.aq)-1L do printf,unit,format='(i4,a30,a30)',i,'aq',c.aq(i)
for i=0L,n_elements(c.bq)-1L do printf,unit,format='(i4,a30,a30)',i,'bq',c.bq(i)
for i=0L,n_elements(c.cq)-1L do printf,unit,format='(i4,a30,a30)',i,'cq',c.cq(i)
for i=0L,n_elements(c.oq)-1L do printf,unit,format='(i4,a30,a30)',i,'oq',c.oq(i)
for i=0L,n_elements(c.rec)-1L do printf,unit,format='(i4,a30,a30)',i,'rec',c.rec(i)
for i=0L,n_elements(c.ifc)-1L do printf,unit,format='(i4,a30,a30)',i,'ifc',c.ifc(i)
for i=0L,n_elements(c.tel1)-1L do printf,unit,format='(i4,a30,a30)',i,'tel1',c.tel1(i)
for i=0L,n_elements(c.tel2)-1L do printf,unit,format='(i4,a30,a30)',i,'tel2',c.tel2(i)
for i=0L,n_elements(c.blcd)-1L do printf,unit,format='(i4,a30,a30)',i,'blcd',c.blcd(i)
for i=0L,n_elements(c.gq)-1L do printf,unit,format='(i4,a30,a30)',i,'gq',c.gq(i)
for i=0L,n_elements(c.pq)-1L do printf,unit,format='(i4,a30,a30)',i,'pq',c.pq(i)
for i=0L,n_elements(c.band)-1L do printf,unit,format='(i4,a30,a30)',i,'band',c.band(i)
for i=0L,n_elements(c.pstate)-1L do printf,unit,format='(i4,a30,a30)',i,'pstate',c.pstate(i)
for i=0L,n_elements(c.vtype)-1L do printf,unit,format='(i4,a30,a30)',i,'vtype',c.vtype(i)
for i=0L,n_elements(c.taper)-1L do printf,unit,format='(i4,a30,a30)',i,'taper',c.taper(i)
for i=0L,n_elements(c.trans)-1L do printf,unit,format='(i4,a30,a30)',i,'trans',c.trans(i)
for i=0L,n_elements(c.source)-1L do printf,unit,format='(i4,a30,a30)',i,'source',c.source(i)
for i=0L,n_elements(c.pos)-1L do printf,unit,format='(i4,a30,a30)',i,'pos',c.pos(i)
for i=0L,n_elements(c.offtype)-1L do printf,unit,format='(i4,a30,a30)',i,'offtype',c.offtype(i)
for i=0L,n_elements(c.ra)-1L do printf,unit,format='(i4,a30,a30)',i,'ra',c.ra(i)
for i=0L,n_elements(c.dec)-1L do printf,unit,format='(i4,a30,a30)',i,'dec',c.dec(i)
close,unit & free_lun,unit
return,0
end

