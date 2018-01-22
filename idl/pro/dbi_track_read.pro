function dbi_track_read ,traid,save_file=save_file,start_int=start_int, $
   end_int=end_int
;
; Transfer track data from DB to idl.
; Executes db script file to create tables for specified track,
; bcp's the tables to unix files, then reads the tables into 
; idl structures.fil_
;
; paramters : traid : track number
;             save_file = file name for save file
;             start_int = starting integration number (not implimented yet)
;             end_int = ending integration number (not implimented yet)
;            
; result=0 (error), 1(ok)
;
; eg. : result=dbi_track_read(3500)
;
common global
common data_set
if e.dbprocess EQ 0 then begin
 print,'**** ERROR : must start server before you can get track data'
 return,0
endif
sp_str=''
if e.java then sp_str='source '+e.rdx_dir+'/.cshrc; '
spawn,sp_str+'isql '+e.login_str+'< '+e.idl_sql+'db_track_drop',result
if not fil_read(e.idl_sql+'db_read_track',lines,nlines) then return,0
if not fil_app_str(e.idl_sql+'db_bcp_track',lines,nlines) then return,0
str_traid=strtrim(string(traid),2L) & str_ori='xxxx' & uti_stri_replace ,lines,str_ori,str_traid
;str_traid='1=1' & str_ori='XXXX_FILTER' & uti_stri_replace ;,lines,str_ori,str_traid
str_traid='1=1'
if not keyword_set(start_int) then start_int=0
if not keyword_set(end_int) then end_int=0
if start_int ne 0 then str_traid=str_traid+ ' and int >= '+string(start_int)
if end_int ne 0 then str_traid=str_traid+ ' and int <= '+string(end_int)
str_ori='XXXX_FILTER' & uti_stri_replace ,lines,str_ori,str_traid
;
; edit in the correct mm database string
;
case 1 of
  (traid le 460L): str_mm='mm9192'
  (traid gt 460L) and (traid le 896L): str_mm='mm9293'
  (traid gt 896L) and (traid le 1644L): str_mm='mm9394'
  (traid gt 1644L) and (traid le 2262L): str_mm='mm9495'
  (traid gt 2262L) and (traid le 2920L): str_mm='mm9596'
  (traid gt 2920L) and (traid le 3682L): str_mm='mm9697'
  (traid gt 3683L) and (traid le 4457L): str_mm='mm9798'
  (traid gt 4457L) and (traid le 5171L): str_mm='mm9899'
  (traid gt 5171L) and (traid le 5956L): str_mm='mm9900'
  (traid gt 5956L) and (traid le 6911L): str_mm='mm0001'
  (traid gt 6911L) and (traid le 7865L): str_mm='mm0102'
else: str_mm='mm'
endcase
;
; check that needed databases exist
;
sql_statement='select ct=count(*) from master..sysdatabases'+ $
   ' where status2 < 16 and name in ("mmdb","'+str_mm+'")'
result=dbi_sql_submit(sql_statement)
if strpos(result[2],'2') eq -1 then begin
  print,'required databases (mmdb , '+str_mm+') not available'
  return,0
endif
str_ori='mmyryr' & uti_stri_replace ,lines,str_ori,str_mm
str_ori='idl_bcp' & uti_stri_replace ,lines,str_ori,e.idl_bcp
str_ori='login_str' & uti_stri_replace ,lines,str_ori,e.login_str
if not fil_write('read_track_temp',lines) then return,0
if e.debug then print,format='(a)',lines
;
; execute the isql script to create the db tables for this track
;
spawn,sp_str+'isql '+e.login_str+'< read_track_temp',result
if e.debug then print,result
;return,0
;
; bcp out the individual tables
;
nlines=max(where(strpos(lines,'bcp lines follow',0) ne -1))
spawn,sp_str+lines(nlines+1l),res
if (strpos(res(3),'0 rows',0) eq 0) then begin
  print,'no data found for tra# :',traid
  print,res
  return,0
endif
spawn,sp_str+lines(nlines+2l),res
if (strpos(res(3),'0 rows',0) eq 0) then begin
  print,'no data found for tra# :',traid
  print,res
  return,0
endif
spawn,sp_str+lines(nlines+3l),res
if (strpos(res(3),'0 rows',0) eq 0) then begin
  print,'no data found for tra# :',traid
  print,res
  return,0
endif
spawn,sp_str+lines(nlines+4l),res
if (strpos(res(3),'0 rows',0) eq 0) then begin
  print,'no data found for tra# :',traid
  print,res
  return,0
endif
spawn,sp_str+lines(nlines+5l),res
if (strpos(res(3),'0 rows',0) eq 0) then begin
  print,'no data found for tra# :',traid
  print,res
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
result=fil_remove('read_track_temp')
spawn,sp_str+'isql '+e.login_str+'< '+e.idl_sql+'db_track_drop',result
if keyword_set(save_file) then begin
  e.save_fn=save_file
  result = pro_mir_save()
endif
return,1
end
