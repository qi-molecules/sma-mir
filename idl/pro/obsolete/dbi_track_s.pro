function dbi_track_s ,traid, fn, si, ei
;
; Transfer track data from DB to idl.
; Executes db script file to create tables for specified track,
; reads the tables to idl structures using dblib.
; result=0 (error), 1(ok)
;
; INPUT PARAMETERS
;	traid - track id  of the dataset you wish to create
;	fn - name to save the dataset under once it's created
;	si - start integration - start from this integration number
;	ei - end with this integration number
;
; eg. : result=dbi_track_s(4188,'savefile',1,45)
;
common global
common data_set
common wlm,wlm_ref_time,wlm_ntel,wlm_itel,       $
           wlm_times,wlm_id,wlm_raw,wlm_wts,     $
           wlm_start,wlm_stop,wlm_cal,wlm_gains, $
           wlm_egains,wlm_xfactor,wlm_filter,wlm_icoherence

;Make sure there is a database connection before continuing
if e.dbprocess EQ 0 then begin
   print,'**** ERROR : must start server before you can get track data'
   return,0
endif

;Set the e.save_fn to be the filename passed in
e.save_fn=fn

sqlfn =  e.idl_sql+'db_track_drop'
test = CALL_EXTERNAL('db.so', 'db_execute_sql_file', e.dbprocess, traid, sqlfn,si,ei)
if(test EQ -1) then begin
    print, "Table deletion failed"
    return, -1
endif

;Create the tables in the database sql commands
;  send in the following:
; 	'db_execute_sql_file' - function name
;	e.dbprocess - connection to the sybase server
;	traid - track id for the dataset creation
;	sqlfn - name of file that contains sql
sqlfn =  e.idl_sql+'db_read_track'
test = CALL_EXTERNAL('db.so', 'db_execute_sql_file', e.dbprocess, traid, sqlfn,si,ei)
print,'back from first external call'
if(test EQ -1) then begin
    print, "Table creation failed"
    return, -1
endif

;
; read then into the idl structures
;
if dbi_head_sread() eq -1 then return,0
if dbi_chan_sread() eq -1 then return,0
;if dat_chan_fix()  eq -1 then return,0

;Remove the tables created for the dataset.
;  send in the following:
; 	'db_execute_sql_file' - function name
;	e.dbprocess - connection to the sybase server
;	traid - track id for the dataset creation
;	sqlfn - name of file that contains sql
;sqlfn =  e.idl_sql+'db_track_drop'
;test = CALL_EXTERNAL('db.so', 'db_execute_sql_file', e.dbprocess, traid, sqlfn,si,ei)
;if(test EQ -1) then begin
;    print, "Table deletion failed"
;    return, -1
;endif

result = pro_mir_save()
if(result EQ -1) then begin
    print, "Unable to save dataset"     ;ERROR CONDITION - what happens if we can't save
    return,-1
endif
spawn,'isql '+e.login_str+'< '+e.idl_sql+'db_track_drop',result
return,1
end
