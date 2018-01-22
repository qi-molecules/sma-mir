function reduce_quality,track,track_quality,track_status,$
                  unit=unit,savedb_ok=savedb_ok

   ; Get track quality
     irepeat = 1
     quality = ['good', 'usable', 'bad']
     while (irepeat) do begin
        print,""
        print,"Enter track quality:"
        for i=0, n_elements(quality)-1 do begin
           print,format='(%"   %d : %s")',i+1,quality[i]
        endfor
        input = ''
        read,prompt='Choice? ',input
        ierr = 0
        input = strtrim(input,2)
        if not valid_num(input) then $
           ierr = 1 $
        else begin
           x = 0.0
           reads,input,x
           iquality = fix(x)
           if 1.0*iquality ne x or iquality lt 1 or iquality gt n_elements(quality) then $
             ierr = 1 $
           else begin
             irepeat = 0
             track_quality = quality[iquality-1]
           endelse
        endelse
        if (ierr) then begin
           print,""
           print,"   *** Error entering track quality ***"
           print,""
        endif
     endwhile

   ; Get track status
     irepeat = 1
     status = ['done', 'partial', 'repeat']
     while (irepeat) do begin
        print,""
        print,"Enter track status:"
        for i=0, n_elements(status)-1 do begin
           print,format='(%"   %d : %s")',i+1,status[i]
        endfor
        input = ''
        read,prompt='Choice? ',input
        ierr = 0
        input = strtrim(input,2)
        if not valid_num(input) then $
           ierr = 1 $
        else begin
           x = 0.0
           reads,input,x
           istatus = fix(x)
           if 1.0*istatus ne x or istatus lt 1 or istatus gt n_elements(status) then $
             ierr = 1 $
           else begin
             irepeat = 0
             track_status = status[istatus-1]
           endelse
        endelse
        if (ierr) then begin
           print,""
           print,"   *** Error entering track status ***"
           print,""
        endif
     endwhile

   ; Enter information into log
     printlog,"Track quality : ",track_quality,unit=unit
     printlog,"Track status  : ",track_status,unit=unit

   ; We are finished if we are not on inyo
     if not keyword_set(savedb_ok) then return,-1
     if not savedb_ok then return,-1

   ; Add track quality/status to database if on inyo
     print,""
     print,"Saving track quality/status to database"
     print,""
     com = string(format='(%"update TRA set quality=\"%s\", status=\"%s\" where tra#=%d")',track_quality,track_status,track)
     result = dbi_sql_submit(com,user='rdx',pass='rdxrdx',/no)
     if (result[0] ne "(1 row affected)") then begin
        print,"*** ERROR SAVING QUALITY/STATUS TO DATABASE"
        return,-1
     endif

   ; Done
     return,1
end
