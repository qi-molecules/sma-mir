function reduce_specplot,ntrim_max=ntrim_max
   ; Common blocks
     common global
     common data_set
     common plo

   ; Set number of channels to trim spectra by
     ntrim_max = keyword_set(ntrim_max) ? ntrim_max : 2

   ; Set filter
     command_select = '"wt" gt "0.0" and "band" like "s"'
     if (dat_select(s_s,command_select,/reset,/no) eq 0) then return,-1

   ; Make a list of availabe sources to plot
     distinct_sources = uti_distinct(c.source[in(pil).isource],nsources,$
                                     /many,elements=c.source)
     nlength = max([strlen("Source"),strlen(distinct_sources)])

   ; Add spaces to source name
     sources = distinct_sources
     for i=0,nsources-1 do begin
        if strlen(sources[i]) lt nlength then $
           sources[i] = sources[i] + strjoin(replicate(" ",nlength-strlen(sources[i])))
     endfor

   ; Enter a loop where the user can select which sources to plot
     irepeat = 1
     while (irepeat) do begin
        ; Make a list of available sources
          print,""
          print,"Select a source to plot"
          print,format='(%"     ID   %s")',"Source"
          print,format='(%"     --   %s")',strjoin(replicate("-",nlength))
          for i=0,nsources-1 do $
             print,format='(%"   %4d   %s")',i+1,sources[i]
          print,""
          print,format='(%"   %4d   %s")',0,"Exit"
          print,format='("Choice ? ",$)'
          input = ''
          read,input
          input = strtrim(input,2)

        ; Decipher option
          ichoice = 0
          x = 0.0
          ierr = 0
          if valid_num(input,x) eq 0 then begin
             ierr = 1
          endif else begin
             ichoice = fix(x)
             if (1.0*ichoice ne x) then ierr = 1
             if (ichoice eq 0) then return,1
             if (ichoice lt 1 or ichoice gt nsources) then ierr = 1
          endelse
          if (ierr) then begin
             print,""
             print,"*** ERROR SELECTING SOURCE ***"
             print,""
          endif

        ; Plot spectra
          if (not ierr) then begin
             com = command_select + ' and "source" eq "' + distinct_sources[ichoice-1] + '"'
             result = dat_select(s_s,com,/reset,/no)
             result = plo_spec('channel','amp,pha','blcd','band','sb',4,$
                               ntrim_max=ntrim_max)
          endif
     endwhile
end
