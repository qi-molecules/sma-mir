function map_driver,pixel_size=pixel_size,npixels=npixels,$
                reset=reset,nogrid=nogrid,notaper=notaper,$
                uvtaper=uvtaper,uniform=uniform

   ; Common blocks
     common global
     common data_set

   ; Reset the filter as needed
     if keyword_set(reset) then begin
        result = dat_list(s_l,/reset,/no_notify)
        if (result le 0) then begin
           print,"Error resetting filter in MAP_DRIVER"
           return,1
        endif
     endif

   ; Set all possible source/receiver combinations
     names = c.source(in(pil).isource)
     j = where(names ne "noise",count_j)
     if (count_j eq 0) then return,-1
     frames = c.source[in(pil[j]).isource] + ' ' + c.rec[bl(pbl[j]).irec]
     distinct_frames = uti_distinct(frames,nframes,/many_repeat)
     names = strarr(nframes)
     rec   = strarr(nframes)
     irec  = strarr(nframes)
     for i=0,nframes-1 do begin
        result = strsplit(distinct_frames[i],/extract)
        names[i] = result[0]
        rec[i]   = result[1]
        irec[i]  = result[1]
     endfor

   ; Change rec code to 1mm or 3mm
     j = where(rec eq '1',nj)
     if (nj gt 0) then rec[j] = '3mm'
     j = where(rec eq '2',nj)
     if (nj gt 0) then rec[j] = '1mm'

   ; Append spaces to names
     nmax = max([strlen(names)])
     for i=0,nframes-1 do begin
         nspaces = nmax+1 - strlen(names[i])
         names[i] = names[i] + strjoin(replicate(' ',nspaces))
     endfor

   ; Start loop
     irepeat = 1
     nprint_per_line = 1
     while (irepeat) do begin
         ; Print sources
           print," "
           print,"Choose continuum source to map"
           for i = 0L, nframes-1L, nprint_per_line do begin
              for j=i, MIN([nframes-1L,i+nprint_per_line-1L]) do $
                 print,format='(%"(%2d) %s : %s  ",$)',j+1,names[i],rec[i]
              print," "
           endfor

         ; Get choice
           print,format='($,"Choice (0 to exit)? ")'
           input = " "
           read,input
           imap = 0
           ierr = 0
           if not valid_num(input,imap,/int) then begin
              ierr = 1
           endif else if (imap lt 0 or imap gt nframes) then begin
              ierr = 1
           endif else begin
              imap = imap - 1
           endelse
 
         ; Map the source
           if (ierr) then begin
              print," "
              print,"                *** INVALID ENTTRY ***"
              print," "
           endif else if (imap eq -1) then begin
              irepeat = 0
           endif else if (imap ge 0) then begin
              ; Set filter command
                com = '"band" not like "s"' + $
                      ' and "source" eq ' + '"' + names(imap) + '"' + $
                      ' and "rec" eq ' + '"' + irec(imap) + '"'
                com = strcompress(com)

              ; Filter the data
                if (dat_list(s_l,com,/reset,/no_notify) le 0) then begin
                   print,"Could not read in any data"
                   print,filter
                   return,-1
                endif

              ; Map the source
                result = map(pixel_size=pixel_size,npixels=npixels,$
                  nogrid=nogrid,notaper=notaper,uvtaper=uvtaper,$
                  uniform=uniform,image=image,beam=beam,udata=udata,vdata=vdata)

              ; Clean the source
                input = ' '
                print,""
                print,format='("Clean image (Y/N)? ",$)
                read,input
                input = strlowcase(input)
                if (input eq 'y' or input eq '' or input eq 'yes') then begin
                  result = map_clean_driver(0,image,beam,udata,vdata,pixel_size,clean=clean)
                endif

              ; Reset filter
                result = dat_select(s_s,/reset)
           endif
     endwhile
     return,1
end
