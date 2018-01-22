function reduce_email,track,project,title,obsdate,track_quality,track_status,$
                      dir_logs=dir_logs,unit=unit
   ; Common blocks
     common global
     common data_set

   ; Must enter track numner
     if not keyword_set(track) or not keyword_set(project) then begin
        print,"Must enter track and project number in reduce_email"
        return,-1
     endif

   ; Make a list of the 3 log files that need to be sent
   ; directory : path name to the logs
   ; files     : root name for the logs
   ; filebrief : short name for the logs
   ; filetype  : description of the logs
   ;
     dir = keyword_set(dir_logs) ? dir_logs : $
          ( (e.campuslogin eq 'ovro') ? '/mm/logs/' : './')
     subdir = (e.campuslogin eq 'ovro') ? ['obslog/','redlog/','tralog/'] : $
                  replicate('./',3)
     directory = dir + subdir
     files = ["ovro_obslog_"+string(track),$
              "ovro_redlog_"+string(track),$
              "ovro_tralog_"+string(track)]
     files = strcompress(files,/remove)
     filebrief = ["observing log", "reduction log", "track log"]
     filetype = ["observing log", $
                 "results from OVRO data reduction script", $
                 "detailed list of integrations"]

   ; See which files exist. 0->no, 1->yes
     use = file_test(directory + files)
     good_files = where(use eq 1,ngood_files)
     if (ngood_files eq 0) then begin
         printlog,"",unit=unit
         printlog,format='(%"No logsheets available for track %d")',track,unit=unit
         return,-1
     endif

   ; Ask which logsheets to send
     print,""
     print,"Available log sheets"
     print,"ID  Log sheet          Description"
     print,"--  ----------------   -----------"
     for i=0,ngood_files-1 do print,format='(%"%2d  %s   %s")',$
         i+1,files[good_files(i)],filetype[good_files(i)]

   ; Let user select IDS
     com = "Select which logs to send to investigators"
     ids_default = findgen(ngood_files) + 1
     result = reduce_ids(ngood_files,ids_default,ids_use,defaults=defaults,com=com)
     if (ids_use[0] eq -1) then begin
        printlog,"",unit=unit
        printlog,"No logs sent by email",unit=unit
        printlog,"",unit=unit
        if keyword_set(unit) then begin
           close,unit
           free_lun,unit
        endif
        return,-1
     endif else begin
        ids_use = ids_use - 1
        good_files = good_files[ids_use]
        ngood_files = n_elements(good_files)
     endelse

   ; Get email addresses for this project
     com = 'select fname,lname,email from INV,PR_IN where PR_IN.inv#=INV.inv# and PR_IN.pro#=' + string(project)
     dbi_sql_tab,com,ct,email
     nemail = (ct gt 0) ? n_elements(email[0,*]) : 0

   ; Print possible email address
     if (nemail eq 0) then begin
        printlog,"",unit=unit
        printlog,"No investigators listed for this track",unit=unit
        printlog,"",unit=unit
        ids_use = -1
     endif else begin
        ; Print table of email addreses
          print,""
          print,"Investigators on this project"
          max_name  = max(strlen(email[0,*])) + max(strlen(email[1,*])) + 1
          max_email = max(strlen(email[2,*]))
          s1 = '' & for i=1,max_name-strlen("name")  do s1 = s1 + ' '
          print,format='(%"ID  Name%s   Email")',s1
          s1 = '' & for i=1,max_name  do s1 = s1 + '-'
          s2 = '' & for i=1,max_email do s2 = s2 + '-'
          print,"--  ",s1,"  ",s2
          for i=0,nemail-1 do print,format='(%"%2d  %s %s  %s")',$
              i+1,email[0,i],email[1,i],email[2,i]

        ; Let user select IDS
          com = "Select investigators who will receive the logs"
          ids_default = where(email[2,*] ne '') + 1
          result = reduce_ids(nemail,ids_default,good_email,defaults=defaults,com=com)
          if (good_email[0] eq -1) then begin
             printlog,"Did not send logs reports to investigators",unit=unit
          endif else begin
             good_email = good_email - 1
             printlog,"",unit=unit
             printlog,format='(%"%s",$)',"Sending ",unit=unit
             for i=0,ngood_files-1 do begin
                 if (i gt 0 and ngood_files gt 2) then printlog,format='(%"%s",$)',", ",unit=unit
                 if (i gt 0 and ngood_files eq 2) then printlog,format='(%"%s",$)'," and ",unit=unit
                 printlog,format='(%"%s",$)',filebrief[good_files(i)],unit=unit
             endfor
             printlog," to:",unit=unit
             printlog,email[*,good_email],unit=unit
          endelse
     endelse

   ; Close log file
     if keyword_set(unit) then begin
        close,unit
        free_lun,unit
     endif

   ; Send email
     j = where(good_email ge 0, nj)
     if nj gt 0 then begin
        ; Reset good_email
          good_email = good_email[j]

        ; Create text file to print email message
          txtfile = '/tmp/' + uti_filename()
          unit_out = 0
          openw,unit_out,txtfile,/get_lun

        ; Print to text file
          s = keyword_set(obsdate) ? ' on ' + obsdate + ':' : ':'
          printf,unit_out,"The following track was observed with the OVRO interferometer",s
          printf,unit_out,format='(%"    Project      : %d")',project
          if keyword_set(title) then $
             printf,unit_out,format='(%"    Title        : %s")',title
          printf,unit_out,format='(%"    Track Number : %d")',track
          if keyword_set(track_quality) then $
             printf,unit_out,format='(%"    Quality      : %s")',track_quality
          if keyword_set(track_status) then $
             printf,unit_out,format='(%"    Status       : %s")',track_status
          printf,unit_out,""
          printf,unit_out,""
          attachments = ''
          if ngood_files gt 0 then begin
             printf,unit_out,"Attachments:"
             for i=0, ngood_files-1 do begin
                printf,unit_out,"     ",files[good_files(i)],' : ',filetype[good_files(i)]
                attachments = attachments + ' -a ' + directory[good_files(i)] + files[good_files(i)]
             endfor
          endif
            
        ; Closing message
;         printf,unit_out,""
;         printf,unit_out,""
;         printf,unit_out,"If you have any questions concerning this track, contact"
;         printf,unit_out,"contact John Carpenter at jmc@astro.caltech.edu (phone:626-395-4024)."

        ; Close file
          close,unit_out
          free_lun,unit_out

        ; Construct email
          subject = string(format='(%" -s \"OVRO track %d\"")',track)
          command = "mutt " + attachments + subject + " -i " + txtfile
          for i=0,n_elements(good_email)-1 do begin
             com = command + " " + email[2,good_email[i]]  + " < /dev/null"
             spawn,com,result,err_result
             if err_result[0] ne '' then $
                printlog,"*** ERROR SENDING LOG FILES TO ",email[2,good_email[i]],unit=unit
          endfor

        ; Remove text file
          file_delete,txtfile,/quiet
     endif
end
