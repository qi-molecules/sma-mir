function reduce_stats_gain,rec,sources=sources,smoothing=smoothing,$
             sigamp=sigamp,sigpha=sigpha,unit=unit,defaults=defaults
   ; Common blocks
     common global
     common data_set

   ; Initialize
     sigamp = 0.0
     sigpha = 0.0

   ; Set sources
     sourcelist = ""
     if keyword_set(sources) then begin
        sourcelist = "["
        for i=0,n_elements(sources)-1 do begin
           if (i gt 0) then sourcelist = sourcelist + ","
           sourcelist = sourcelist + '"' + sources[i] + '"'
        endfor
        sourcelist = sourcelist + "]"
     endif

   ; Select gain calibrators
     command = '"gq" eq "g" and "band" like "c" and "rec" eq "' + rec + '" and "wt" gt "0"'
     if keyword_set(sources) then $
        command = command + ' and "source" in ' + sourcelist

     if dat_select(s_s,command,/reset,/no) le 0 then begin
        printlog,"No gain calibrators to computer statistics",unit=unit
        return,-1
     endif

   ; Find sources and band/sideband combinations
     sources = uti_distinct(in(pis).isource,nsources)
     comb = c.band[sp(pss).iband] + ',' + c.sb[bl(pbs).isb]
     comb = uti_distinct(comb,ncomb)

   ; Loop over sources
     amp_frac = fltarr(nsources,ncomb)
     pha_frac = fltarr(nsources,ncomb)
     for i=0,nsources-1 do begin
        ; Loop over combinations
          source = c.source[sources[i]]
          for j=0,ncomb-1 do begin
             ; Get band,sideband
               result = strtok(comb[j],',',/extract)
               band = result[0]
               sb = result[1]

             ; Filter data
               command = '"source" eq "' + source + $
                    '" and "band" eq "' + band + '" and "sb" eq "' + sb + '"'
               nrows = dat_list(s_l,command,/reset,/no)

             ; Compute average amplitude and dispersion.
               freqave= mean(sp(psl).fsky)
               ampave = mean(bl(pbl).ampave)
               phaave = mean(bl(pbl).phaave)
               amprms = (nrows gt 1) ? stddev(bl(pbl).ampave) : 0.0
               pharms = (nrows gt 1) ? stddev(bl(pbl).phaave) : 0.0
               amp_frac[i,j] = amprms/ampave
               pha_frac[i,j] = pharms/360.0*!CVEL/(freqave*1e6)
          endfor
     endfor

   ; Compute averag amp/pha disersions
     j = where(amp_frac gt 0 and pha_frac gt 0,nj)
     sigamp = (nj gt 0) ? total(amp_frac(j)) / nj : 0.0
     sigpha = (nj gt 0) ? total(pha_frac(j)) / nj : 0.0

   ; Print results. Make two passes through the data, first printing 
   ; amplitudes, and then printing phases
     sdt = keyword_set(smoothing) ? $
       string(format='(%"(smoothing = %8.1f minutes)")',smoothing*60.0) : $
       "smoothing = ??? minutes"
     sdt = strcompress(sdt)
     for i=0,1 do begin
        ; Print header
          label = (i eq 0) ? $
            "Gain  calibration for Rx " + rec + ": rms / mean amplitude" : $
            "Phase calibration for Rx " + rec + ": rms in mm"
          printlog,label + ' ' + sdt,unit=unit
          s = 'Source        '
          ss = '------------- '
          for k=0,ncomb-1 do begin
             s  = s  + '    ' + strupcase(comb[k])
             ss = ss + '    '
             for j=1,strlen(comb[k]) do ss = ss + '-'
          endfor
          printlog,s,unit=unit
          printlog,ss,unit=unit

        ; Loop over sources
          for j=0, nsources-1 do begin
             ; Print source name
               name = c.source[sources[j]]
               for k=strlen(c.source[sources[j]])+1,12 do name = name + ' '
               printlog,format='(%"%12s",$)',name,unit=unit

             ; Loop over combinations
               x = (i eq 0) ? amp_frac[j,*] : pha_frac[j,*]

             ; Print fractional results
               printlog,format='("  ",4F8.2)',x,unit=unit
          endfor
          printlog,"",unit=unit
     endfor
end
