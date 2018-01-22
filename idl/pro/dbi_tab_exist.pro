function dbi_tab_exist ,table_name
;
; Checks to see if a table exists in db.
;
; parameters : table_name   -- table name (if not in form uid.table_name,
;                              the table must be for this user)                           ; result = 0 (does not exist), 1(exists already)
; 
; eg. : result=dbi_tab_exist('t_tr3500_1')
; eg. : result=dbi_tab_exist('nzs.t_tr3500_1')
;
common global
owner=strmid(table_name,0,strpos(table_name,'.',0))
if owner eq '' then begin
  sql_statement='select number_of_tables=count(name) from sysobjects '+ $
              'where name = "'+ strtrim(table_name,2)+ $
              '" and type = "U" and user_id() = uid'
endif else begin
  table=strmid(table_name,strpos(table_name,'.',0)+1,40)
  sql_statement='select number_of_tables=count(name) from sysobjects '+ $
              'where name = "'+ strtrim(table,2)+ $
              '" and type = "U" and uid =user_id("'+strtrim(owner,2L)+'")'
endelse
result_str=dbi_sql_submit(sql_statement)
if e.debug then print,result_str
result=long(result_str(2))
return,result
end
