function dbi_utable_read ,table_owner,table_name,filter
;
; Transfer data from DB user table or view to idl.
; bcp's the tables to unix files, then reads the tables into 
; idl structures.
; parameters : table_owner = db owner of table (eg. 'dbo' or 'kbm')
;            : table_name  = table name (eg. 'last' or 'tr3500_1')
;            : filter = logic in sql where clause ('1=1' => all) 
; result=0 (error), 1(ok)
;
; eg. : result=dbi_utable_read('dbo','last','1=1')
; eg. : result=dbi_utable_read('dbo','tr3500_1','1=1')
;
common global
common data_set
if e.dbprocess EQ 0 then begin
 print,'**** ERROR : must start server before you can get data'
 return,0
endif

if not fil_read(e.idl_sql+'db_read_utable',lines,nlines) then return,0
;
; edit in the full table name and SCH string
;
case 1 of
  (table_name eq 'last'): begin
                            t_table='dbo.v_last'
                            SCH_table='mm.dbo.SCH'
                          end 
  (table_name eq 'present'): begin
                            t_table='dbo.v_present'
                            SCH_table='mm.dbo.SCH'
                          end
  (table_name eq 'previous'): begin
                            t_table='dbo.v_previous'
                            SCH_table='mm.dbo.SCH'
                          end
else: begin
  t_table=table_owner+'.t_'+table_name
  SCH_table=table_owner+'.SCH_'+table_name 
  endelse
endcase
if dbi_tab_exist(t_table) eq 0 or dbi_tab_exist(SCH_table) eq 0 then begin
  print,t_table,' or ',SCH_table,' not found in db !'
  return,-1
endif
str_ori='xxxx_table' & uti_stri_replace ,lines,str_ori,t_table
str_ori='xxxx_filter' & uti_stri_replace ,lines,str_ori,filter
str_ori='idl_bcp' & uti_stri_replace ,lines,str_ori,e.idl_bcp
str_ori='xxxx_SCH' & uti_stri_replace ,lines,str_ori,SCH_table
str_ori='login_str' & uti_stri_replace ,lines,str_ori,e.login_str
if not fil_write('read_utable_temp',lines) then return,0
if e.debug then print,format='(a)',lines
;
; execute the isql script to create the db tables for this track
;
spawn,'isql '+e.login_str+'< read_utable_temp',result
if e.debug then print,result
;
; bcp out the individual tables
;
nlines=max(where(strpos(lines,'bcp lines follow',0) ne -1))
if e.debug then print,'start ',lines(nlines+1l)
spawn,lines(nlines+1l),res
if (strpos(res(3),'0 rows',0) eq 0) then begin
  print,'no data found for table :',table_name
  print,result
  return,0
endif
if e.debug then print,'start ',lines(nlines+2l)
spawn,lines(nlines+2l),res
if (strpos(res(3),'0 rows',0) eq 0) then begin
  print,'no data found for table :',table_name
  print,result
  return,0
endif
if e.debug then print,'start ',lines(nlines+3l)
spawn,lines(nlines+3l),res
if (strpos(res(3),'0 rows',0) eq 0) then begin
  print,'no data found for table :',table_name
  print,result
  return,0
endif
if e.debug then print,'start ',lines(nlines+4l)
spawn,lines(nlines+4l),res
if (strpos(res(3),'0 rows',0) eq 0) then begin
  print,'no data found for table :',table_name
  print,result
  return,0
endif
if e.debug then print,'start ',lines(nlines+5l)
spawn,lines(nlines+5l),res
if (strpos(res(3),'0 rows',0) eq 0) then begin
  print,'no data found for table :',table_name
  print,result
  return,0
endif
;
; read then into the idl structures
;
if dbi_head_read() eq -1 then return,0
if dbi_chan_read() eq -1 then return,0
;if dat_chan_fix()  eq -1 then return,0
;
; delete the bcp files
;
result=fil_remove('in_read')
result=fil_remove('bl_read')
result=fil_remove('sp_read')
result=fil_remove('codes_read')
result=fil_remove('sch_read')
result=fil_remove('read_utable_temp')
spawn,'isql '+e.login_str+'< '+e.idl_sql+'db_track_drop',result
return,1
end
