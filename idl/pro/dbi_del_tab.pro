function dbi_del_tab ,table_name
;
; Deletes a user table in db.
; Will only delete tables w/ names like t_ or SCH_ (ie. user tables)
; and will only delete tables this user owns. Removes entry from
; MMOBJ table which is the directory of all mir user tables.
;
; parameters : table_name   -- table name (must be for this user also)
;
; eg. : result=dbi_del_tab('t_tr3500_1')
;
; usually called in combinationatio w/ dbi_tab_exist 
; eg. : if (dbi_tab_exist('t_tr3500_1') ne 0) then $
;                 result=dbi_del_tab('t_tr3500_1')
;
common global
if e.debug then print,'... deleting table ',table_name
sql_statement='if exists (select * from sysobjects where name = "'+ $
              strtrim(table_name,2)+'" and type = "U" and '+ $
             '(name like "[t][_]%" or name like "[S][C][H][_]%") '+ $
             'and user_id() = uid) '+ $
             'begin '+ $
             ' drop table '+ strtrim(table_name,2) + $
             ' delete MMOBJ where name = "' + strtrim(table_name,2) +'" '+ $
             ' and type ="U" '+ $
             'end '
result_str=dbi_sql_submit(sql_statement)
if e.debug then print,sql_statement
if e.debug then print,result_str
result=1
return,result
end
