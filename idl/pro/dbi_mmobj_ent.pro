function dbi_mmobj_ent ,h_name,fh_name,c_name,fc_name
;
; Enter a new user table in db directory table MMOBJ.
; This routine checks that both the table and its SCH 
; counterpart actually exist.
;
; parameters : h_name,fh_name,c_name,fc_name   -- table names
;              for the header and channel data (f=> w/ user name)
;
; eg. : result=dbi_mmobj_ent('t_tr3_1','dbo.t_tr3_1','SCH_tr3_1','dbo.SCH_tr3_1')
;
;
common global
if e.debug then print,'... enter table ',h_name,' in MMOBJ'
if (dbi_tab_exist(h_name) ne 0 and dbi_tab_exist(c_name) ne 0) then begin
  sql_statement='insert MMOBJ values ("' + $
                strtrim(h_name,2)+'","'+strtrim(fh_name,2) + $
                '","U","copy","file","htable","' + $
                strtrim(c_name,2)+'","'+strtrim(fc_name,2)+'","old")'
  result_str=dbi_sql_submit(sql_statement)
  if e.debug then print,sql_statement
  if e.debug then print,result_str
  result=1
endif else begin
  print,'tables :',h_name,c_name,' not found in db'
  return,0
endelse
return,result
end
