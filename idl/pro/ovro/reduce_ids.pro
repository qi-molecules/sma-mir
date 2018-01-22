function reduce_ids,nids,ids_default,ids_use,defaults=defaults,com=com
   ; Get ID numbers to use
     if keyword_set(defaults) then $
       ids_use = ids_default $
     else begin
       delim = " ,"
       input = " "
       irepeat = 1
       while (irepeat) do begin
         ; Print instructions
           print," "
           if keyword_set(com) then begin
              for i=0,n_elements(com)-1 do print,com[i]
           endif
           print,format='("    default  :",$)'
           for i=0,n_elements(ids_default)-1 do $
               print,format='(%" %d",$)',ids_default[i]
           print,""
           print,"          0 -> none"
           print,format='($,a)',"Choice? "

         ; Read ids into strings
           read,input
           input = strtrim(input,2)
           sids = uti_distinct(strtok(input,delim,/extract))

         ; Convert to numbers. Verify they are valid integers
           irepeat = 0
           if (input eq '') then begin
              ids_use = ids_default
           endif else if (input eq '0') then begin
              ids_use = -1
           endif else if (irepeat eq 0) then begin
              ; Make sure they are valid numbers
                for i=0,n_elements(sids)-1 do begin
                   if valid_num(sids[i]) eq 0 then irepeat = 1
                endfor

              ; Make sure they are integers and within range
                if (irepeat eq 0) then begin
                   x = fltarr(n_elements(sids))
                   ids_use = intarr(n_elements(sids))
                   reads,sids,x
                   ids_use = fix(x)
                   j = where(ids_use ne x or ids_use lt 1 or ids_use gt nids,nj)
                   if nj gt 0 then irepeat = 1
                endif
           endif
           if (irepeat) then begin
              print," "
              print,"                *** INVALID ENTTRY ***"
              print," "
            endif
       endwhile
     endelse
end
