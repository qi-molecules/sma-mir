function reduce_readtrack,strack,noread=noread
   ; Common blocks
     common global
     common data_set
     common plo

   ; Read track string if not entered on command line
     if (not keyword_set(strack)) then begin
        strack = ' '
        read,strack,prompt="Enter track number : "
     endif

   ; Make sure track string is a number
     status = valid_num(strack)
     if (status eq 0) then begin
        printf,-1,"Error entering track number"
        return,-1
     endif

   ; Finally, read the track number. Make sure it is an integer
     track = 0.0
     reads,strack,track
     if (1.0 * fix(track) ne track) then begin
        printf,-1,"Track number must be an integer"
        return,-1
     endif
     if (track lt 1) then begin
        printf,-1,"Error reading track number"
        printf,-1,strack
        return,-1
     endif

   ; Check track number if correct if not reading it from the database
     if keyword_set(noread) then begin
        if not keyword_set(in) then begin
           printf,-1,"Error reading track from IDL memory."
           printf,-1,'You may need to run "reduce" without the noread option'
           return,-1
        endif
        if (track ne in(0).traid) then begin
           printf,-1,format='("Current track number is ",I5," not ",I5)',$
               in(0).traid,track
           return,-1
        endif
        result = dat_filter(s_f,/reset)
     endif

   ; Done
     return,fix(track)
end
