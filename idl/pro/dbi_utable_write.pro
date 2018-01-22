function dbi_utable_write ,table_name
;
; Transfer data from idl to DB user table.
; For the header data :
; Writes out the in. nl, sp, and c structures to host files
; bcp's these unix files into temporary db tables, then runs
; an isql scripts to create the standard mma flat header table.
; For the channel and record data :
; Converts the ch strucutre with floating data back into the
; scaled int*2 values, writes out this to a host file, then 
; bcp's this file into a standard mma channel table format.
; If a user table already exists in the db with the table_name,
; this table is deleted and replaced with the idl data.
;
; result=0 (error), 1(ok)
;
; eg. : result=dbi_utable_write('tr3500_1')
;
common global
common data_set
if e.dbprocess EQ 0 then begin
 print,'**** ERROR : must start server before you can get data'
 return,0
endif
;
; construct db table names (convert to l.c. to distinguish
; from archive tables)
;
table=strtrim(strlowcase(table_name),2)
h_name='t_'+table
c_name='SCH_'+table
db_user=e.user_name
if e.user_name eq 'rdx' then db_user='dbo'
fh_name=db_user+'.'+h_name 
fc_name=db_user+'.'+c_name 
fn = uti_filename(e.user_name,suffix='.uta'); 
;
; write out the idl structures to host files
;
if dbi_head_write() eq -1 then return,0
if dbi_chan_write() eq -1 then return,0
;
;
; check that output table is not an archive table and
; if there is a similarly named user table, delete it.
;
if (dbi_tab_exist(h_name) ne 0) then result=dbi_del_tab(h_name)
if (dbi_tab_exist(c_name) ne 0) then result=dbi_del_tab(c_name)
;
if not fil_read(e.idl_sql+'db_write_utable',lines,nlines) then return,0
str_ori='xxxx_table' & uti_stri_replace ,lines,str_ori,h_name
str_ori='xxxx_SCH' & uti_stri_replace ,lines,str_ori,c_name
str_ori='idl_bcp' & uti_stri_replace ,lines,str_ori,e.idl_bcp
str_ori='login_str' & uti_stri_replace ,lines,str_ori,e.login_str
str_ori='dataset_dir' & uti_stri_replace ,lines,str_ori,e.dataset_dir
;
; create blank tables
;
f_lines=max(where(strpos(lines,'begin table create',0) ne -1))
e_lines=max(where(strpos(lines,'end table create',0) ne -1))
if not fil_write(e.dataset_dir+'/'+fn,lines(f_lines:e_lines)) then return,0
if e.debug then print,format='(a)',lines(f_lines:e_lines)
spawn,'isql '+e.login_str+'< '+e.dataset_dir+'/'+fn,result
;
; bcp in the individual tables
;
flines=max(where(strpos(lines,'bcp lines follow',0) ne -1))
spawn,lines(flines+1l),res & spawn,lines(flines+2l),res
spawn,lines(flines+3l),res & spawn,lines(flines+4l),res
spawn,lines(flines+5l),res
;
; execute the isql script to create the flat db table
; grants select permissions and creates indexes 
;
f_lines=max(where(strpos(lines,'begin join tables',0) ne -1))
e_lines=max(where(strpos(lines,'end join tables',0) ne -1))
if not fil_write(e.dataset_dir+'/'+fn,lines(f_lines:e_lines)) then return,0
if e.debug then print,format='(a)',lines(f_lines:e_lines)
spawn,'isql '+e.login_str+'< '+e.dataset_dir+'/'+fn,result
if e.debug then print,result
result=dbi_mmobj_ent(h_name,fh_name,c_name,fc_name)
result=dbi_sql_submit('update '+fc_name+ $
      ' set nbyt=datalength(packdata)',/no_notify)
;
; delete the bcp files
;
result=fil_remove(e.dataset_dir+'/tmp/in_write')
result=fil_remove(e.dataset_dir+'/tmp/bl_write')
result=fil_remove(e.dataset_dir+'/tmp/sp_write')
result=fil_remove(e.dataset_dir+'/tmp/codes_write')
result=fil_remove(e.dataset_dir+'/tmp/sch_write')
result=fil_remove(e.dataset_dir+'/'+fn)
spawn,'isql '+e.login_str+'< '+e.idl_sql+'db_utable_drop',result
return,1
end
