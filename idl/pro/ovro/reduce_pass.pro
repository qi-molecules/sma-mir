function reduce_pass,rec,ntrim_max=ntrim_max,redo=redo,$
           defaults=defaults,unit=unit,doplots=doplots
   ; Common blocks
     common global
     common data_set
     common plo

   ; Initialize 
     redo = 0

   ; Set number of channels to trim spectra by
     ntrim_max = keyword_set(ntrim_max) ? ntrim_max : 2

   ; Set filter
     command_select = '"wt" gt "0.0" and "band" like "s" and "rec" like "' + rec + '"'
     if (dat_select(s_s,command_select,/reset,/no) eq 0) then begin
        printlog,"",unit=unit
        printlog,format='(%"No spectral line data for Rx %s")',rec,unit=unit
        printlog,"",unit=unit
        return,-1
     endif

   ; Make a list of all sources, their average amplitudes, and expected fluxes
     result = pas_ini(use,all_souids,all_sources,all_amps,nint=nint)

   ; Get the velocity resolution of the data in km/s. If it less than 10 km/s, 
   ; then the default should include the noise. Otherwise, it should not.
     vres = mean(abs(sp(psl).fres/sp(psl).fsky)) / 1e6 * !CVEL
     use_noise = (vres lt 10.0)

   ; Set default ids. 
   ; Remove noise from list of default ids if velocity resolution is too low
     ids_default = where(use eq 1,ndefault) + 1
     if (use_noise eq 0 and n_elements(ids_default) gt 1) then begin
        j = where(all_sources eq 'noise' and use eq 1,nj)
        if (nj gt 0) then begin
           ids_default[j] = 0
           use[ids_default[j]-1] = 0
        endif
     endif
     j = where(ids_default gt 0,nj)
     ids_default = ids_default[j]

   ; Print table of fluxes
     printlog,""
     printlog,format='(%"Observed passband calibrators for Rx %s")',rec
     printlog,' ID Use?  Source      Nint  <Amp>'
     for i=0,n_elements(all_souids)-1 do begin
        name = all_sources[i]
        n = strlen(all_sources[i])
        for j=n+1,12 do name = name + ' '
        suse = (use[i]) ? 'yes' : 'no'
        printlog,format='(%"%3d  %3s  %12s %3d %6.3f")',$
           i+1,suse,name,nint[i],all_amps[i]
     endfor

   ; Get ID numbers for sources
     com = 'Enter ID numbers of *ALL* passband calibrators you want to use'
     result = reduce_ids(n_elements(use),ids_default,ids_use,$
                   defaults=defaults,com=com)
     if (ids_use[0] eq -1) then begin
        printlog,format='(%"Skipping passband calibration for Receiver %s")',$
           rec,unit=unit
        printlog,""
        return,-1
     endif
     ids_use = ids_use - 1
     cal = all_sources[ids_use]

   ; Print table of adopted passband calibrators
     printlog,""
     printlog,format='(%"Sources/amplitudes used for Rx %s passband calibration")',rec,unit=unit
     printlog,' ID  Source         Amp',unit=unit
     printlog,'---  ----------   ------',unit=unit
     for j=0,n_elements(ids_use)-1 do begin
        i = ids_use[j]
        name = all_sources[i]
        n = strlen(all_sources[i])
        for k=n+1,12 do name = name + ' '
        printlog,format='(%"%3d  %12s %6.3f")',i+1,name,all_amps[i],unit=unit
     endfor
     printlog,""

   ; Initialize variables for passband plots
     frames_per_page = 4
     x_var = 'channel'
     cal_type = 'amp,pha'
     frame_vars = 'blcd,sb,band'
     plid = 1

   ; Loop over the passband calibration twice. On the first pass, derive/apply
   ; passband calibration from the noise source. On the second pass, apply
   ; passband from any remaining sources.
   ; SMOOTHING:
   ;     If there was a noise source and it was applied, then do not smooth
   ;     when applying quasar passband. Otherwise, smooth over 4 channels
     used_noise = 0
     for i=1,2 do begin
        ; Select which sources to plot
          if (i eq 1) then begin
            j = where(all_sources[ids_use] eq 'noise',nj)
            smoothing = 0
            used_noise = 1
            if (nj gt 0) then begin
               print,""
               print,"***************** DERIVING NOISE  PASSBAND *****************"
               print,""
            endif
          endif else begin
            j = where(all_sources[ids_use] ne 'noise',nj)
            smoothing = used_noise ? 10000 : 4
            if nj gt 0 and not keyword_set(defaults) then begin
               print,""
               print,"***************** DERIVING QUASAR PASSBAND *****************"
               print,""
               irepeat = 1
               s = strcompress(string(format='(%"Average velocity resolution = %6.2f km/s")',vres))
               s = [s,'Enter smoothing width in channels for quasar passband']
               while (irepeat) do begin
                  print,s[0]
                  print,format='(%"%s",$)',strcompress(s[1])
                  input = ''
                  read,input
                  ierr = 0
                  if valid_num(input) eq 0 then $
                     ierr = 1 $
                  else begin
                     smoothing = 0L
                     reads,input,smoothing
                     if (1.0*fix(smoothing) ne smoothing or smoothing lt 0) then $
                        ierr = 1 $
                     else $
                        irepeat = 0
                  endelse
                  if (ierr) then begin
                     print," "
                     print,"     *** Error entering channel smoothing width ***"
                     print," "
                  endif
               endwhile
            endif
          endelse

        ; Apply passband
          if nj gt 0 then begin
             result=pass(all_souids[ids_use(j)],x_var,cal_type,smoothing,$
                   frame_vars,frames_per_page,plid,ntrim_max=ntrim_max) 
             if (result ge 0) then begin
                irepeat = 1
                while (irepeat) do begin
                   input = keyword_set(defaults) ? "y" : "n"
                   if not keyword_set(defaults) then begin
                     print,format='("Apply passband solution [Yes/No/Redo]? ",$)'
                     read,input
                   endif
                   input = strlowcase(input)
                   if (input eq 'r' or input eq 'redo') then begin
                      irepeat = 0
                      redo = 1
                      return,1
                   endif else if (input eq 'y' or input eq '' or input eq 'yes') then begin
                      irepeat = 0
                      result=cal_apply(passband='amp,pha')
                      printlog,format='(%"Applied passband solution to Rx %s")',rec
                   endif else if (input eq 'n' or input eq 'no') then begin
                      irepeat = 0
                      printlog,format='(%"WARNING: Did not apply passband solutions to Rx %s")',rec
                   endif else begin
                      print,""
                      print,"*** Error entering input ***"
                      print,""
                   endelse
                endwhile
                printlog,""
             endif
          endif
     endfor

   ; Plot spectra
     com = '("pq" eq "p" or "gq" eq "g") and "wt" gt "0.0"'
     result = dat_select(s_s,com,/reset,/no)
     if keyword_set(doplots) then begin
        print,""
        print,"*** PLOTTING SPECTRA FOR ALL CALIBRATORS IN TRACK. ***"
        print,"*** VERIFY SPECTRA ARE FLAT IN AMPLTIUDE AND PHASE ***"
        print,""
        result = plo_spec('channel','amp,pha','blcd','band','sb',4,$
                          ntrim_max=ntrim_max)
     endif

   ; Done
     return,1
end
