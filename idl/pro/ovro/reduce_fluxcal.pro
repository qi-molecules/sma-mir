function reduce_fluxcal,rec,coh_min=coh_min,div_coh=div_coh,$
            frames=frames,defaults=defaults,apply=apply,$
            cal_sources=cal_sources,cal_primary=cal_primary,$
            method=method,unit=unit
   ; Common blocks
     common global
     common data_set
     common plo

   ; Initialize
     cal_primary = 0

   ; Minimum coherence. Normalize it between 0 and 1
     mincoh = keyword_set(coh_min) ? 1.0 * coh_min : 0.90
     if (mincoh gt 1.0) then mincoh = mincoh / 100.0

   ; Select data for only this receiver
     com = '"rec" eq "' + rec + '"'
     if (dat_select(s_s,com,/reset,/no) eq 0) then return,-1

   ; Set available baselines/sidebands/continuum combinations
     bsl = c.blcd(uti_distinct(bl(pbs).iblcd,nbsl))
     sbs = c.sb(uti_distinct(bl(pbs).isb,nsbs))
     bands = c.band(uti_distinct(sp(pss).iband,nbands))
     jcont = where(strmatch(bands,"s*") ne 1,ncont)
     if (ncont eq 0) then begin
        printlog,"No continuum channels for flux calibration",unit=unit
        printlog,"",unit=unit
        return,-1
     endif
     cbands = bands[jcont]

   ; Get list of gain calibrators and their fluxes. This actually computes 
   ; the flux at the average frequency in the track.
     result = flux_cal_ini(sources=sources,flags=flags,nint=nint,$
           amp=amp,flux=flux,icalib=icalib,freq=freq,/noprint)
     if (result eq -1) then begin
        printlog,"No flux calibrators in track",unit=unit
        printlog,"",unit=unit
        return,-1
     endif

   ; Select calibrators only
     nsources = n_elements(sources)
     if (nsources eq 0) then begin
       printlog,"",unit=unit
       printlog,"No flux calibrators in track",unit=unit
       printlog,"",unit=unit
       return,-1
     endif

   ; Print list of primary and secondary calibrators
     print,format='(%"List of primary and secondary calibrators for Rx %s")',$
        rec
     print,"  ID  Source      Flags <Amp>    Flux    Freq"
     print,"                                 (Jy)   (GHz)"
     for i=0,nsources-1 do begin
        name = sources[i]
        if (strlen(name) lt 12) then $
           name = name + strjoin(replicate(' ',12-strlen(name)))
        print,format='(%"%4d  %12s %4s %5.2f   %5.2f   %7.3f")',$
            i+1, name, flags[i], amp[i], flux[i], freq[i]
     endfor

   ; Prompt user to enter id numbers of valid calibrators
     ids_default = where(icalib eq 1,nj) + 1
     if (nj eq 0) then ids_default = where(icalib eq 2) + 1
     com = 'Enter ID numbers for flux calibrators you want to use'
     result = reduce_ids(nsources,ids_default,ids_use,$
                         defaults=defaults,com=com)
     if ids_use[0] eq -1 then begin
        printlog,'No flux calibrators chosen',unit=unit
        printlog,'',unit=unit
        return,-1
     endif
     ids_use = ids_use - 1

   ; Ask user to update flux for secondary calibrator
     if not keyword_set(defaults) then begin
        j = where(icalib[ids_use] ne 1, nj)
        ichange = 0
        printlog," "
        for k=0,nj-1 do begin
           l = ids_use[j(k)]
           printlog,format='(%"Enter flux (Jy) for %10s (default=%5.2f) ",$)',$
               sources[l],flux[l]
           x = 0.0
           input = ' '
           read,input
           if valid_num(input,x) then begin 
              ichange = 1
              flux[l] = x
           endif
        endfor
     endif

   ; If the flux is zero, then do not use that calibrator
     j = where(flux(ids_use) ne 0, nj)
     if (nj eq 0) then begin
        printlog,"",unit=unit
        printlog,"All flux calibrators have zero adopted flux --- skipping flux calibration"
        printlog,"",unit=unit
        return,-1
     endif
     ids_use = ids_use(j)

   ; Summarize calibrators
     cal_sources = sources[ids_use]
     printlog,format='(%"Flux calibrating Rx %s using the following sources")',$
        rec,unit=unit
     printlog,"Source       <Amp>    Flux",unit=unit
     printlog,"                      (Jy)",unit=unit
     printlog,"---------    -----    ----",unit=unit
     for i=0,n_elements(ids_use)-1 do begin
        name = sources[ids_use[i]]
        if (strlen(name) lt 12) then $
           name = name + strjoin(replicate(' ',12-strlen(name)))
        printlog,format='(%"%12s %5.2f   %5.2f")',$
            name, amp[ids_use[i]], flux[ids_use[i]],unit=unit
     endfor

   ; "cal_primary" is a flag that indicates whether or not the data are 
   ; being calibrated with primary calibrators only.
     j = where(icalib(ids_use) ne 1,nj)
     cal_primary = (nj eq 0) ? 1 : 0

   ; Allocate memory to store flux calibration factors
     scale_factor_sig = fltarr(ncont,nsbs,nbsl)
     scale_factor_bsl = fltarr(ncont,nsbs,nbsl)
     scale_factor_all = fltarr(ncont)
     scale_factor_sb  = fltarr(ncont,nsbs)
     frames = ""

   ; Loop over available continuum channels and flux calibrate the data
     for i = 0, ncont-1 do begin
        ; Reset filter
          result = dat_select(s_s,/reset,/no)

        ; Derive scale factors for this continuum band
          result = flux_sf_derive(sources[ids_use],flux[ids_use],cbands(i),$
                coh_min=mincoh,div_coh=div_coh,$
                distinct_bsl=distinct_bsl,distinct_sb=distinct_sb,$
                good_frames=good_frames,$
                jct_bsl=jct_bsl,jct_sb=jct_sb,jct_all=jct_all,jct_sig=jct_sig,$
                /noprint)
          if (result eq -1) then begin
             printlog,"Error deriving flux calibration for channel ",cbands[i],$
                      unit=unit
             printlog,"Data may not exceed the minimum coherence criteria",unit=unit
             method=''
          endif else begin
             ; Store scale factors in cal structure
               method='bsl'
               result = flux_sf_store(distinct_bsl,distinct_sb,$
                     jct_bsl,jct_sb,jct_all,method=method)

             ; Save average scale factor for this channel
               scale_factor_all[i] = jct_all

             ; Save results
               if (result ne -1) then begin
                  frames = (frames[0] eq "") ? good_frames : [frames,good_frames]
                  result=dat_comb_sep(good_frames,['band','blcd','sb'],$
                             codes, icodes,n_components)
                  for j=0,n_elements(good_frames)-1 do begin
                     ; Set indices in jct_bsl
                       isb  = where(distinct_sb  eq codes[2,j])
                       ibsl = where(distinct_bsl eq codes[1,j])

                     ; Set indices in c structure
                       jsb  = where(codes[2,j] eq c.sb)
                       jbsl = where(codes[1,j] eq c.blcd)

                     ; Store scale factors
                       scale_factor_bsl(i,jsb,jbsl) = jct_bsl[isb,ibsl]
                       scale_factor_sig(i,jsb,jbsl) = jct_sig[isb,ibsl]
                       scale_factor_sb(i,jsb) = jct_sb[isb]
                  endfor
               endif

             ; Apply flux calibrations by selecting appropriate IF band
               if keyword_set(apply) then begin
                  ; Make sure there is only one IF
                    command = '"band" eq "' + c.band[i] + '"'
                    result = dat_select(s_s,command,/reset,/no)
                    ifs = uti_distinct(bl(pbl).iifc,nifs)
                    if (nifs ne 1) then begin
                       printlog,'Error applying flux calibration -  multiple IFS for continuum channel',unit=unit
                       return,-1
                    endif

                  ; Select appropriate IF
                    command = '"ifc" eq "' + c.ifc[ifs[0]] + '"'
                    result = dat_select(s_s,command,/reset,/no)

                  ; Flux calibrate data
                    result = cal_apply(gain='amp')
                    if (result eq -1) then begin
                       printlog,'Error applying flux calibration for channel ',$
                          c.ifc[ifs[0]],unit=unit
                       return,-1
                    endif
               endif
          endelse
     endfor

   ; Print comments
     printlog," ",unit=unit
     printlog,format='(%"Rx %s scale factors using coh >= %4.2f",$)',$
          rec,mincoh,unit=unit
     if keyword_set(div_coh) then $
        printlog," (with coherence correction)",unit=unit $
     else $
        printlog," (without coherence correction)",unit=unit

   ; Print table header
     printlog,format='(%" %s",$)',"BSL",unit=unit
     for i=0,ncont-1 do begin
        if (nsbs eq 1) then $
           printlog,format='(7x,A2,"    ",$)',cbands[i],unit=unit $
        else $
           printlog,format='(13x,A2,"           ",$)',cbands[i],unit=unit
     endfor
     printlog,"",unit=unit
     printlog,format='(%"%s",$)'," --- ",unit=unit
     for i=0,ncont-1 do begin
        if (nsbs eq 1) then $
           printlog,format='(%" %s ",$)',"-----------",unit=unit $
        else $
           printlog,format='(%" %s ",$)',"------------------------",unit=unit
     endfor
     printlog,"",unit=unit
     printlog,format='(%"%s",$)',"    ",unit=unit
     for i=0,ncont-1 do begin
         for j=0,nsbs-1 do begin
            sideband = (sbs[j] eq 'l' ? "LSB" : "USB")
            printlog,format='("      ",A,"    ",$)',sideband,unit=unit
         endfor
      endfor
     printlog,"",unit=unit

   ; Print table of scale factors
     for i=0,nbsl-1L do begin
       printlog,format='(1x,A3,1x,$)',c.blcd[i],unit=unit
       for j=0,ncont-1 do begin
       for k=0,nsbs-1 do begin
          if (scale_factor_bsl[j,k,i] gt 0.0) then begin
             printlog,format='(1x,F5.2,"+/-",$)',scale_factor_bsl[j,k,i],unit=unit
             if (scale_factor_sig[j,k,i] gt 0.005) then $
                printlog,format='(F4.2,$)',scale_factor_sig[j,k,i],unit=unit $
             else $
                printlog,format='(%" %s",$)',"N/A",unit=unit
          endif else $
             printlog,format='(1x,%" %s",$)',"-----------",unit=unit
       endfor
       endfor
       printlog,"",unit=unit
     endfor
     printlog,"",unit=unit

   ; Average over baselines
     printlog,format='(1x,A3,1x,$)','Ave',unit=unit
     for j=0,ncont-1 do begin
     for k=0,nsbs-1 do begin
        i = where(scale_factor_bsl[j,k,*] gt 0.0,ni)
        if (ni gt 0) then begin
           ave = total(scale_factor_bsl[j,k,i]) / ni
           printlog,format='(1x,F5.2,"       ",$)',ave,unit=unit
        endif else $
           printlog,format='(1x,%" %s",$)',"-----------",unit=unit
     endfor
     endfor
     printlog,"",unit=unit

   ; Averages over bands
     for j=0,ncont-1 do begin
        x = scale_factor_bsl[j,*,*]
        i = where(x gt 0.0,ni)
        if (ni gt 0) then begin
          ave = total(x) / ni
          printlog,format='(%" Average in band %s : %5.2f")',cbands[j],ave,unit=unit
        endif
     endfor
     printlog,"",unit=unit

   ; Indicate which method is being used for flux measurements
     case method of
       'bsl' : method = ' baseline-based scale factors'
       'sb'  : method = ' sideband-averaged scale factors'
       else  : method = ' band-averaged scale factors'
     endcase

   ; Return value indicates if any good base where found
     result = (frames[0] eq '') ? -1 : 1
     return,result
end
