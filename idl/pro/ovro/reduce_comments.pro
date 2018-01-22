function reduce_comments,unit,section,$
                         enter=enter,savedb_ok=savedb_ok,track=track
   ; Common blocks
     common global

   ; Exit if a file is not open
     if not keyword_set(unit) then return,1

   ; Print instructions for user
     print,' '
     print,' '
     print,'************************************************************ '
     print,'Enter comments for ',section,' at end of file. '
     print,'After entering comments, exit editor to continue IDL script'
     if keyword_set(enter) and keyword_set(savedb_ok) and keyword_set(track) then begin
        print,""
        print,"While the comments can be of any length, the first 255 "
        print,"characters only will be saved to the database."
     endif
     print,'************************************************************ '
     print,' '
     print,' '

   ; Open temporary file for comments
     tmpfile = '/tmp/' + uti_filename()
     openw,unittmp,tmpfile,/get_lun
     
   ; Set string label for comments and add them to the temporary file
     label_string = string(format='(%"Observers comments for %s:")',section)
     printlog,label_string,unit=unittmp,/onlylog
     printlog,"",unit=unittmp

   ; Close tmp file
     close,unittmp
     free_lun,unittmp

   ; Get number of lines in the tmp logfile so that the cursor can be
   ; positions at the end of the file
     com = 'wc -l ' + tmpfile
     spawn,com,result,err
     nlines = 0
     reads,result,nlines

   ; Start editor - what a kludge
     editor = getenv('EDITOR')
     if editor eq 'vi' or editor eq '' then begin
        command = string(format='(%"xterm -e vi -c %d %s")',nlines,tmpfile)
     endif else if editor eq 'emacs' then begin
        command = string(format='(%"emacs +%d %s")',nlines,tmpfile)
     endif else begin 
        command = string(format='(%"%s %s")',editor,tmpfile)
     endelse
     spawn,command

   ; Get number of characters in the tmp logfile 
     com = 'wc -lm ' + tmpfile
     spawn,com,result,err
     nchar = 0
     nlines = 0
     reads,result,nlines,nchar
     if nchar eq 0 then return,-1

   ; Now read the tempfile and store the comments in a string array
     comments = strarr(nlines)
     openr,unittmp,tmpfile,/get_lun,/delete
     readf,unittmp,comments
     close,unittmp
     free_lun,unittmp

   ; Print comments to logfile
     printlog,"",unit=unit,/onlylog
     printlog,label_string,unit=unit,/onlylog
     j = where(comments ne label_string,ncomments)
     if (ncomments gt 0) then printlog,transpose(comments(j)),unit=unit,/onlylog
     printlog,"",unit=unit,/onlylog

   ; If we are not saving the comments to the database, then exit now
     if ncomments eq 0 or not keyword_set(track) or $
        not keyword_set(enter) or not keyword_set(savedb_ok) $
        then return,1

   ; Get the comments only
     comments = comments(j)

   ; Join the comments into a single character array
     sjoin = strjoin(comments,' ',/single)

   ; Split string based on '"' character
     result = strsplit(sjoin,'"',/extract,/preserve)

   ; Merge results, but replacing " with ''
     comments = strjoin(result,"''")

   ; Write first 255 characters into database
     com = string(format='(%"update TRA set comments=\"%s\" where tra#=%d")',$
           strmid(comments,0,MIN([255,strlen(comments)])),track)
     result = dbi_sql_submit(com,user='rdx',pass='rdxrdx',/no)
     if (result[0] ne "(1 row affected)") then begin
        print,"*** ERROR SAVING COMMENTS TO DATABASE"
        return,-1
     endif

   ; Done
     return,1
end
