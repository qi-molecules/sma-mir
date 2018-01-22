function dbi_sou_exist ,source_name
;, ra, dec
;
; Checks to see if a source name exists in db.
;
; parameters : source_name   -- source name 
; result = 0 (does not exist), 1(exists already)
; 
; eg. : result=dbi_sou_exist('3c279')
;
common global

source_name=strtrim(source_name,2L)
;ra=strtrim(ra,2L)
;dec=strtrim(dec,2L)
if source_name eq 'mars' or source_name eq 'uranus' or source_name eq 'neptune' or source_name eq 'jupiter' or source_name eq 'venus' or source_name eq 'sun'  or source_name eq 'moon' or source_name eq 'mercury' or source_name eq 'titan' or source_name eq 'pluto' then begin
;print,'solar system objects'
result=1
return,result
endif else begin
;sql_statement='select number_of_sources=count(source) from SOU ' + $
;           'where source="'+source_name+'"'+ $
;           ' and ra="'+ra+'" and dec= "'+dec+'"'
sql_statement='select distinct pri# from SOU_ALI ' + $
            'where sec# in (select sou# from SOU ' + $
            'where source="'+source_name+'")'
;           'where source="'+source_name+'"'+ $
;           ' and ra="'+ra+'" and dec= "'+dec+'")'
result_str=dbi_sql_submit(sql_statement)
if e.debug then print,result_str
;result=long(result_str(2))
return, fix(result_str(2))
endelse

end
