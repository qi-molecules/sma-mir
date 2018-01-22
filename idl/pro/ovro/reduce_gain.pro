function reduce_gain,rec,smoothing=smoothing,cal=cal,$
                     defaults=defaults,redo=redo,unit=unit
   ; Common blocks
     common global
     common data_set
     common plo

   ; Initialize
     redo = 0
     cal  = ""

   ; Set filter
     command = '"rec" eq "' + rec + '" and "source" ne "noise" and "wt" gt "0.0"'
     if (dat_select(s_s,command,/reset,/no) eq 0) then begin
        printlog,"",unit=unit
        printlog,format='("No data for Receiver %s")',rec,unit=unit
        printlog,"",unit=unit
        return,-1
     endif

   ; Make a list of all sources, their average amplitudes, and expected fluxes
     result=gai_ini(30.,use,all_souids,all_sources,all_amps, $
                    numbs_3mm,numbs_1mm,fluxes_3mm,fluxes_1mm)
     if rec eq '2' then begin
        fluxes = fluxes_1mm
        nfluxes = numbs_1mm
        fluxband = '1mm'
     endif else begin
        fluxes = fluxes_3mm
        nfluxes = numbs_3mm
        fluxband = '3mm'
     endelse

   ; Initialize
     plid = 1

   ; Print table of fluxes
     printlog,format='(%"Observed gain/phase calibrators for Rx %s")',rec
     printlog,' ID Use?  Source      Nint  <Amp>  Flux  Nfluxes'
     printlog,'                                    (Jy)'
     printlog,'--- ----  ---------- ------ -----  ----- -------'
     nint = intarr(n_elements(use))
     for i=0,n_elements(all_souids)-1 do begin
        name = all_sources[i]
        n = strlen(all_sources[i])
        for j=n+1,12 do name = name + ' '
        sflux = (nfluxes[i] eq 0) ? '------' : $
                 strcompress(string(format='(%"%10.3f")',fluxes[i]),/remove)
        j = where(in(pis).souid eq all_souids[i])
        iscans = uti_distinct(in(pis(j)).int,nscans)
        nint[i] = nscans
        use[i] = (nint[i] lt 2) ? 0 : use[i]
        suse = (use[i]) ? 'yes' : 'no'
        printlog,format='(%"%3d  %3s  %12s %3d %6.3f  %6s    %3d")',$
           i+1,suse,name,nint[i],all_amps[i],sflux,fix(nfluxes[i])
     endfor

   ; Get ID numbers for sources
     com = 'Enter ID numbers for desired gain calibrators '
     ids_default = where(use eq 1,ndefault) + 1
     result = reduce_ids(n_elements(use),ids_default,ids_use,$
                   defaults=defaults,com=com)
     if (ids_use[0] eq -1) then begin
        printlog,format='(%"Skipping gain calibration for Rx %s")',rec,unit=unit
        return,-1
     endif
     ids_use = ids_use - 1

   ; Make sure gain calibrators have more than one measurement
     j = where(nint[ids_use] lt 2,nj)
     if (nj gt 0) then begin
        printlog,""
        printlog,"Cannot use the following calibrators since they were observed only once:"
        for i=0,nj-1 do printlog,"   ",all_sources[ids_use(j)]
        j = where(nint[ids_use] ge 2,nj)
        if (nj eq 0) then begin
           printlog,format='(%"Skipping gain calibration for Rx %s")',rec,unit=unit
           return,-1
        endif
        ids_use = ids_use[j]
     endif

   ; Allow the user to update the fluxes
     if not keyword_set(defaults) then begin
        print,""
        print,"To update flux for gain calibrator, enter <ID new_flux>"
        print,"Enter blank line to continue"
        irepeat = 1
        while (irepeat) do begin
           print,format='("Choice? ",$)'
           input = ' '
           read,input
           input = strtrim(input,2)
           if input eq '' then $
             irepeat = 0 $
           else begin
             ; Should only be two arguments
               result = strtok(input,' ,',/extract)
               ierr = (n_elements(result) ne 2)
               if (not ierr) then begin
                  ierr = 1
                  x = 0.0
                  xid = 0.0
                  if valid_num(result[0],xid) eq 1 and $
                     valid_num(result[1],x) eq 1 then begin
                     ; Read id - make sure it is an integer
                       id = fix(xid)
                       ierr = (1.0*id ne xid)
                       id = id - 1

                     ; Read flux - must be non-negative
                       if (x lt 0.0) then ierr = 1
                  endif
               endif

             ; If no error, make sure flux is valid
               if (not ierr) then begin
                  j = where(ids_use eq id,nj)
                  if (nj eq 0) then begin
                     print,"ERROR: ID number was not selected as a gain calibrator"
                  endif else begin
                     fluxes[ids_use[j(0)]] = x
                  endelse
               endif

             ; Print error message if needed
               if (ierr) then print,"Error entering fluxes"
           endelse
        endwhile
     endif

   ; If a flux is zero, then change it to the average amplitude
     j = where(fluxes[ids_use] eq 0.0,nj)
     if (nj gt 0) then fluxes[ids_use(j)] = all_amps[ids_use(j)]

   ; Print table of adopted fluxes
     printlog,""
     printlog,format='(%"Sources/fluxes used for Rx %s gain/phase calibration")',$
        rec,unit=unit
     printlog,'Source         Amp    Flux',unit=unit
     printlog,'                      (Jy)',unit=unit
     printlog,'-----------  ------  -------',unit=unit
     for j=0,n_elements(ids_use)-1 do begin
        i = ids_use[j]
        name = all_sources[i]
        n = strlen(all_sources[i])
        for k=n+1,12 do name = name + ' '
        sflux = strcompress(string(format='(%"%10.3f")',fluxes[i]),/remove)
        printlog,format='(%"%12s %6.3f  %6s")',name,all_amps[i],sflux,unit=unit
     endfor
     printlog,"",unit=unit

   ; OK - we now have the calibrators/fluxes for this receiver. Loop
   ; over the IFs and apply gain calibration.
     frames_per_page = 1
     x_var = 'hours'
     cal_type = 'amp,pha'
     smoothing = 60.0
     if not keyword_set(defaults) then begin
        s = string(format='(%"Enter smoothing time for gains/phases in minutes (default=%10.1f minutes): ")',smoothing)
        s = strcompress(s)
        input = ''
        print,""
        read,prompt=s,input
        ierr = 0
        x = 0.0
        if not valid_num(input,x) then begin
           ierr = 1
        endif else begin
           if (x le 0) then ierr = 1
           if (not ierr) then smoothing = x
        endelse
        if (ierr) then begin
           print,""
           print,"Error entering smoothing time for gains/phases."
           s = string(format='(%"Using the default value of %10.1f minutes")',smoothing)
           print,strcompress(s)
           print,""
        endif
     endif
     ; Changes smoothing to hours
     smoothing = smoothing / 60.0
     ifc = uti_distinct(bl(pbl).iifc,nifc)
     for i=0,nifc-1 do begin
        ; Print message 
          printlog,""
          printlog,format='(%"*** GAIN CALIBRATION FOR IF %s")',c.ifc[ifc[i]]
        ; Select only data for this IF
          command = '"ifc" eq "' + c.ifc[ifc[i]] + '" and "rec" eq "' + rec + '" and "wt" gt "0.0"'
          if (dat_select(s_s,command,/reset,/no) gt 0) then begin
             result=gain(all_souids[ids_use],fluxes(ids_use),fluxes(ids_use), $
           'baseline',x_var,cal_type,frames_per_page,dt_smooth=smoothing,plid)
             if (result ge 0) then begin
                irepeat = 1
                while (irepeat) do begin
                   input = keyword_set(defaults) ? "y" : "n"
                   if not keyword_set(defaults) then begin
                     print,format='("Apply gain solution [Yes/No/Redo]? ",$)'
                     read,input
                   endif
                   input = strlowcase(input)
                   if (input eq 'r' or input eq 'redo') then begin
                      irepeat = 0
                      redo = 1
                      return,1
                   endif else if (input eq 'y' or input eq 'yes' or input eq '') then begin
                      irepeat = 0
                      cal = all_sources[ids_use]
                      result=cal_apply(gain='amp,pha',x_var=x_var)
                      printlog,format='(%"Applied gain/phase solutions to IF %s, Rx %s")',$
                         ifc[i],rec
                   endif else if (input eq 'n' or input eq 'no') then begin
                      irepeat = 0
                      printlog,format='(%"WARNING: Did NOT apply gain solutions to IF %s, Rx %s")',$
                       ifc[i],rec
                   endif else begin
                      print,""
                      print,"*** Error entering input ***"
                      print,""
                   endelse
                endwhile
             endif
          endif
     endfor

   ; Done
     return,1
end
