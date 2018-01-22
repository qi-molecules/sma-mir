; *************************************************************************
; FUNCTION 
;      wlm_track_read
;
; WRITTEN
;      August 30, 2001 by JMC
;
; PURPOSE
;      Reads a track or existing table from the database
;
; INPUTS
;      track       : Track number to read and process in IDL
;      table_name  : Name of table in database (eg. 'last' or 'tr3500_1')
;      table_owner : Owner of table (default: current IDL user)
;      noread      : If set, just use the track from memory
;
; OUTPUTS
;      tname       : Name of table that was read in
;      track_num   : Track number
;      outroot     : Output root name for plots
;      -1  -> Error reading track
;      +1  -> No errors reading track
;
; EXAMPLES
;      result = wlm_track_read(6528)
;      result = wlm_track_read(table_name='t6528_1')
;
; *************************************************************************
function wlm_track_read,track,noread=noread,$
                        table_name=table_name,table_owner=table_owner,$
                        tname=tname,track_num=track_num,outroot=outroot
   ; Common blocks
     common global
     common data_set

   ; BOOLEANS
     iread  = 1 - keyword_set(noread)

   ; Read track
     if (keyword_set(track) or not keyword_set(table_name)) then begin
        ; Set track number
          if (not keyword_set(track)) then begin
             print,format='($,"Enter track number ")'
             tname = " "
             read,tname
             track_num = long(tname)
          endif else $
             track_num = long(track)
          tname = string(track_num)

        ; Do we need to read the track
          if not iread then begin
             if (in(0).traid ne track_num) then begin
                print,'Error reading data'
                print,'Track number entered: ',track_num
                print,'Track number stored : ',in(0).traid
                return,-1
             endif
             print," "
             print," "
             print,"--- WARNING: READING TRACK FROM MEMORY"
             if dat_list(s_l,/reset,/no_notify) le 0 then begin
                print,'No data after reloading track'
                return,-1
             endif
          endif

        ; Read the track
          if (iread) then begin
             print," "
             print," "
             print,strcompress("--- Reading track " + tname + " ...")
             time = systime(1)
             result = dbi_track_read(track_num)
             if (result ne 1) then begin
                print,strcompress("Error reading track " + tname)
                return,-1
             endif
             s = wlm_settime(time)
             print,strcompress("--- Read track successfully ... time = " + s)
          endif

        ; Set output root name
          outroot = 't' + tname
     endif

   ; Read exisiting table name from the database, if needed
     if not keyword_set(track) and keyword_set(table_name) then begin
        ; Set table owner
          if (not keyword_set(table_owner)) then table_owner = e.user_name
          tname = strcompress(string(table_name),/remove_all)

        ; Read the table
          table_name = strcompress(table_owner + ":" + tname,/remove_all)
          print," "
          print," "
          print,strcompress("--- Attempting to read table "+table_name+" ...")
          time = systime(1)
          result = dbi_utable_read(table_owner,tname,'1=1')
          if (result ne 1) then begin
             print,strcompress("Error reading table " + table_name)
             return,-1
          endif
          s = wlm_settime(time)
          print,strcompress("--- Finished reading table " + tname + $
                            " ... time = " + s)

        ; Set output root name
          track_num = in(0).traid
          outroot = strcompress('t' + string(track_num),/remove)
     endif

   ; Done
     return,1
end
