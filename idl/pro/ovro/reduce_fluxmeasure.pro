function reduce_fluxmeasure,sources=sources,frames=frames,coh_min=coh_min,$
            div_coh=div_coh,flux=flux,snr=snr,cal_primary=cal_primary,$
            cal_sources=cal_sources,$
            notprimary=notprimary,$all_sources=all_sources,$
            individual=individual,add=add,$
            noprint=noprint,weight=weight,snr_limit=snr_slimit,$
            method=method,defaults=defaults,unit=unit
                       
;
; Flux measurement
; 
; Measure the flux for sources within a track.
; INPUTS:
;    sources: if set, fluxes only for these sources will be measured.
;             default is to measuure fluxes for all sources
;    frames : String array that indicates which combination of 
;             channel/sideband/baseline should be used to measure 
;             the fluxes
;    coh_min: Minimum coherence to measure fluxes. Default = 90
;    div_coh: If set, will divide by coherence when measuring the fluxes
; OUTPUTS:
;    flux   : Measured fluxes
;    snr    : Theoretical signal to noise ratio
;    
; Examples:
;      result = flux_measure(coh_min=80,/div_coh)
;      result = flux_measure(sources=["3c273","3c345"],frames=["c2 2-4 u"]
;                result=cal_apply(gain='amp')

   ; Command blocks
     common global
     common data_set

   ; Get list of sources
     if not keyword_set(sources) then begin
        command = '"source" ne "noise" and "wt" gt "0.0"'
        if not keyword_set(all_sources) then $
           command = command + ' and ("gq" like "g" or "cq" like "c")'
        if keyword_set(notprimary) or $
           (not keyword_set(primary) and not keyword_set(all_sources)) then $
           command = command + ' and "aq" ne "1"'
        if dat_select(s_s,command,/reset,/no_notify) eq 0 then return,-1
        sources = c.source[uti_distinct(in(pis).isource)]
     endif

   ; Defaults
     coh_min_default   = 0.9
     snr_limit_default = 3.0
     div_coh_default   = 0

   ; Minimum coherence, normalize between 0 and 1
     mincoh = keyword_set(coh_min) ? 1.0 * coh_min : coh_min_default
     if (mincoh gt 1.0) then mincoh = mincoh / 100.0

   ; Set snr limit
     if not keyword_set(snr_limit) then snr_limit = snr_limit_default

   ; Divide by coherence?
     idivide = keyword_set(div_coh) ? 1 : div_coh_default

   ; Indicate which method is being used for flux measurements
     if keyword_set(method) then $
        printlog,"Measuring fluxes using",method,unit=unit
     printlog,format='(%"Measuring fluxes using coh >= %4.2f")',mincoh,unit=unit
     if (idivide) then $
        printlog,"Dividing by coherence before measuring fluxes",unit=unit $
     else $
        printlog,"Not dividing by coherence before measuring fluxes",unit=unit
     printlog,"",unit=unit

   ; Indicate if any of the defaults have changed
     using_defaults = (mincoh eq coh_min_default) and $
                      (snr_limit eq snr_limit_default) and $
                      (idivide eq div_coh_default) and $
                      keyword_set(frames)

   ; Reset filter
     result = dat_select(s_s,/reset)

   ; The filter command will consist of the source name,
   ; coherence, and allowable sideband,baseline,continuum-channels.
   ; coherence and sideband/baseline/continuum-channels critera are
   ; applicable to all calls, so contruct that clause here
   ;
   ; Initialize command
     command = strcompress('"coh" ge "' + string(mincoh) + '"')

   ; Add constraints if frames is set
     if keyword_set(frames) then begin
        ; Remove duplicates
          frames = uti_distinct(frames,nframes)
          jband = where(strmatch(c.band,"s*") ne 1,nband)

        ; Put commands in parenthesis
          command = command + ' and ('

        ; Loop over frame combinations
          for i = 0, n_elements(frames)-1 do begin
              ; Separate string into band/sideband/baseline
                result = strtok(frames[i],/extract)
                if (n_elements(result) ne 3) then begin
                   printlog,"Error reading band/sideband/baseline combination in flux_measure()",unit=unit
                  printlog,frames[i],unit=unit
                  return,-1
                endif

              ; Add quotes to strings
                channel  = strcompress('"' + result[0] + '"',/remove)
                baseline = strcompress('"' + result[1] + '"',/remove)
                sideband = strcompress('"' + result[2] + '"',/remove)

              ; Add to query
                if (i gt 0) then command = command + ' or '
                command = command + ' ('
                command = command + '"blcd" eq ' + baseline
                command = command + ' and "sb" eq ' + sideband
                command = command + ' and "band" eq ' + channel
                command = command + ')'

        ; Loop over frame combinations
          endfor

        ; End command
          command = command + ')'
     endif

   ; Now, loop over the sources and measure the fluxes
     flux = [0.0]
     flux_sig = [0.0]
     flux_rms = [0.0]
     flux_n = [0]
     iaq  = [0]
     freq = [0.0]
     el   = [0.0]
     coh  = [0.0]
     iut  = [0]
     souname = [""]
     souid = [0L]
     xsize = [0.0]
     alpha = [0.0]
     for i=0,n_elements(sources)-1 do begin
        ; Construct command
          com = '"band" like "c" and "source" eq "' + sources[i] + '" and ' + command

        ; Filter the data
          nrows = dat_select(s_s,com,/reset,/no)

        ; If keyword INDIVIDUAL is set, then compute flux for each 
        ; sideband/channel combination. First, set distinct sidebands and 
        ; distinct channels depending on whether keyword INDIVIDUAL is set.
        ; This is cumbersome, but then I only need a single loop to compute
        ; the fluxes rather than repeat the code.
          if nrows eq 0 then begin
             nsb   = 0
             nband = 0;
          endif else if keyword_set(individual) then begin
             nsb = n_elements(c.sb)
             jband = where(strmatch(c.band,"s*") ne 1,nband)
          endif else begin
             nsb = 1
             nband = 1;
          endelse
          distinct_pos = uti_distinct(in(pis).ipos,npos)

        ; Loop over combinations
          for q=0,n_elements(c.rec)-1 do begin
          for l=0,npos-1 do begin
          for k=0,nsb-1 do begin
          for j=0,nband-1 do begin
             ; Get amplitudes/weights
               s = '"rec" eq "' + c.rec[q] + '" and "pos" eq "' + c.pos[l] + '"'
               if keyword_set(individual) then $
                  s = s + ' and "wt" gt "0" and "band" eq "' + c.band[jband[j]] + $
                               '" and "sb" eq "' + c.sb[k] + '"'
               nrows = dat_list(s_l,s,/reset,/no)

             ; Continue if nrows is positive
               if (nrows gt 0) then begin
                  ; Set amps/weights
                    ampave = bl(pbl).ampave
                    wts = keyword_set(weight) ? $
                          sp(psl).wt : replicate(1.0,n_elements(ampave))

                  ; Divide by coherence
                    if idivide then begin
                       ampave = ampave / bl(pbl).coh
                       wts = wts * bl(pbl).coh^2
                    endif
                    result = uti_meanvar(ampave,weights=wts,iset=iset)
                    rms = iset[2] eq 1 ? result[2] : 1.0

                  ; The average flux is computed as the weighted mean of the
                  ; amplitude averages. The uncertainty is evaluated by one
                  ; of the following means, in order of preference:
                  ;    (1) If more than 5 scans, then the standard deviation
                  ;        of the 5 scans.
                  ;    (2) If more than 5 measurements (over all scans/bls),
                  ;        then the standard deviation of the observations.
                  ;    (3) If less than 5 observations, as the continuum SNR.

                  ; Compute average flux. This is simply the weighted mean,
                  ; and eflux is the standard deviation of all observations.
                    result = uti_meanvar(ampave,weights=wts)
                    xflux = result[0]
                    eflux = result[2]

                  ; Set mean fsky, elevation, and coherence
                    result = uti_meanvar(sp[psl].fsky)
                    nu = result[0]

                    result = uti_meanvar(in(pil).el,weights=wts)
                    xel = result[0]

                    result = uti_meanvar(bl(pbl).coh,weights=wts)
                    xcoh = result[0]

                  ; Adjust rms if necessary.
                  ; If more than 5 scans, then estimate uncertainty as standard
                  ; deviation of the individual scans. 
                  ; If less than 5 scans and less than 5 measurements, 
                  ; estimate uncertainty as continuum signal to noise. I include
                  ; nscans=1 since it is possible to flux cal on 1 scan, and
                  ; then the RMS will be 0.0 by definition.
                  ; If none of the above are satisfied, then eflux is the
                  ; the standard deviation of the measurements.
                    distinct_scans = uti_distinct(in(pil).int,nscans)
                    if (nscans eq 1) or (nscans le 5 and n_elements(pbl) le 5) then begin
                       noise = ampave / bl(pbl).csnr
                       result = uti_meanvar(ampave/bl(pbl).csnr,weights=wts)
                       eflux  = result[0]
                    endif else if nscans gt 5 then begin
                       f  = fltarr(nscans)
                       ef = fltarr(nscans)
                       for m=0,nscans-1 do begin
                          ; Compute observed mean/sigma
                            n = where(in(pil).int eq distinct_scans[m],nn)
                            x = ampave(n)
                            s = wts(n)
                            result = uti_meanvar(x,weights=s,iset=iset)

                          ; Reset rms if too few measurements
                            if (nn le 5) then result[2] = rms

                          ; If RMS less than SNR, then re-set rms
                            result[2] = result[2] > mean(ampave(n)/bl(pbl(n)).csnr)
                          ; Save results
                            f[m] = result[0]
                            ef[m] = result[2]
                       endfor
                       results = uti_meanvar(f,sig=ef)
                       eflux = results[2]
                    endif

                  ; Save results.
                    flux = [flux,xflux]
                    flux_n = [flux_n,nscans]
                    flux_sig = [flux_sig,eflux]
                    el = [el,xel]
                    coh = [coh,xcoh]
                    freq = [freq,nu]
                    souname = [souname,sources[i]]
                    souid = [souid,in(pil(0)).souid]
                    iut = [iut,in[pil(0)].iut]
                    iaq = [iaq,bl(pbl(0)).iaq]
                    xsize = [xsize,in(pil(0)).size]
                    alpha = [alpha,in(pil(0)).sflux]
               endif
          endfor
          endfor
          endfor
          endfor
     endfor

   ; Make sure some fluxes were measured
     n = n_elements(flux)
     if n eq 1 then return,-1

   ; Set which fluxes to save to database based on SNR
   ; cal_sources contains the list of sources used to calibrate the data.
   ; This are marked as "no"
     snr = fltarr(n)
     j = where(flux_sig gt 0,nj)
     if (nj gt 0) then snr[j] = flux[j] / flux_sig[j]
     ;use = (snr ge snr_limit and c.aq[iaq] ne '1')
     use = (snr ge snr_limit)
     if keyword_set(cal_sources) then begin
        for i=0,n_elements(cal_sources)-1 do begin
            j = where(cal_sources[i] eq souname,nj)
            if (nj gt 0) then use[j] = 0
        endfor
     endif
     s = ['no','yes']
     suse = s[use]

   ; Print results. Element 0 is a dummy element
     if not keyword_set(noprint) then begin
        ; printlog," ",unit=unit
        printlog," ID Save? Source        Frequency   Flux   E(Flux)  SNR  Nscans  Coh  El",unit=unit
        printlog,"                          (GHz)     (Jy)    (Jy)                     (deg)",unit=unit
        printlog,"--- ----- ---------     ---------   ----   -------  ---  ------  ---  --",unit=unit
        for i=1,n_elements(flux)-1 do begin
           name = souname[i]
           for j=0,12-strlen(name) do name = name + ' '
           printlog,format='(%"%3d %4s  %12s  %8.4f  %6.3f   %6.3f  %5.1f  %4d    %2d  %2d")',$
             i,suse[i],name,freq[i],$
             flux[i],flux_sig[i],snr[i],flux_n[i],fix(100.0*coh[i]),fix(el[i]),unit=unit
        endfor
     endif
     if not keyword_set(add) then begin
        printlog,"",unit=unit
        return,1
     endif

   ; Get ID numbers to save
     com = 'Enter ID numbers for fluxes to save to database'
     ids_default = where(use eq 1,ndefault)
     result = reduce_ids(n_elements(flux)-1,ids_default,ids_use,$
                   defaults=defaults,com=com)
     if (ids_use[0] eq -1) then begin
        printlog,"No fluxes entered into database",unit=unit
        printlog,"",unit=unit
        return,-1
     endif

   ; Finally, add fluxes
     if (using_defaults) then $
        reid = keyword_set(cal_primary) ? 'cal' : 'obs' $
     else $
        reid = e.user_name
     for i=0, n_elements(ids_use)-1 do begin
        ; Set id number
          j = ids_use[i]

        ; Make sure this source was not already entered into database
          com = 'select count(*) from FLU where reid="' + reid + '"'
          com = com + ' and uto = "' + c.ut[iut[j]] + '"'
          com = com + ' and abs(flux - ' + strcompress(string(flux[j])) + ') < 0.001'
          com = com + ' and abs(snr - ' + strcompress(string(snr[j])) + ') < 0.001'
          result = dbi_sql_submit(com)
          k = where (result eq ' ----------- ')
          dup = 0
          reads,result[k+1],dup
          if (dup ne 0) then begin
              printlog,format='(%"ID = %d has already been entered into the database")',ids_use[i],unit=unit
          endif else begin
            ; Get flux ID
              com = 'declare @min int, @max int, @flu# id, @pri# int '
              com = com + 'declare @source varchar(12), @pos varchar(12) '
              com = com + 'begin transaction '
              com = com + 'select @min=min_allowed,@max=max_allowed '
              com = com + 'from UPD where table_name="FLU" and '
              com = com + 'var_name="flu#" and ser_name="'
              com = com + e.server + '" '
              com = com + 'select @flu#=max(flu#)+1 from FLU where '
              com = com + 'flu# >= @min and flu# <= @max '

            ; Get primary source id
              com = com + 'select @pri#=0 '
              com = com + strcompress('select @pri#=pri# from SOU_ALI where sec#=' + string(souid[j]))
              com = com + ' if (@pri# is not null and @pri# != 0) begin '

            ; Construct flux command
              com = com + $
                    'select @source=source,@pos=pos from SOU where sou#=@pri# ' + $
                    ' insert FLU values (@flu#,"' + $
                    reid + '",getdate(),@pri#,@source,@pos,"' + $
                    c.ut[iut[j]] + '",' + $
                    string(flux[j]) + $
                    ',' + string(snr[j]) + $
                    ',' + string(freq[j]) + $
                    ',' + string(alpha[j]) + $
                    ',' + string(xsize[j])
               for k=0,9 do com = com + ',"n"'
               com = com + ',"good",NULL)'
               com = com + ' end'
               com = strcompress(com)

             ; Gulp - hope this is right!
               com = com + ' commit transaction'
               result = dbi_sql_submit(com)
               k = where (result ne '(1 row affected)',nk)
               if nk gt 0 then $
                  printlog,format='(%"WARNING: Error entering flux for id=%d into database")',ids_use[i],unit=unit $
               else $
                  printlog,format='(%"Saving flux for id=%2d as reid=%s")',$
                      ids_use[i],reid,unit=unit
          endelse
     endfor
     printlog,"",unit=unit
end
