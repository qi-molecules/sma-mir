; Produces a table of average coherences for each band/sideband combination

function reduce_coh,noprint=noprint,unit=unit
   ; Common blocks
     common global
     common data_set

   ; Select continuum gain observations 
     command = '"gq" eq "g" and "band" like "c" and "source" ne "noise" and "wt" gt "0.0"'
     if (dat_select(s_s,command,/reset,/no) eq 0) then begin
        printlog,"No gain calibrators in track",unit=unit
        return,-1
     endif

   ; Compute average elevation
     elevation = mean(in(pis).el)

   ; Continuum Bands
     iband = uti_distinct(sp(pss).iband,nbands)
     if nbands eq 0 then return,-1
   
   ; Sidebands
     isb = uti_distinct(bl(pbs).isb,nsb)
     
   ; Number of possible baselines
     nbls_max = !NTEL * (!NTEL-1)/2

   ; Initialize - I do it this way since I want to sort by frequency
     coh = make_array(nsb*nbands,nbls_max,value=-1.0)
     bls = strarr(nbls_max)
     freq = dblarr(nsb*nbands)
     band = strarr(nsb*nbands)
     sb   = strarr(nsb*nbands)

   ; Loop over sidebands/bands
     nn = 0
     for j=0,nbands-1 do begin
     for i=0,nsb-1 do begin
         ; Get frequency
           command = '"band" eq "' + c.band[iband(j)] + $
                     '" and "sb" eq "' + c.sb[isb(i)] + '"' + $
                     ' and "gq" eq "g" and "source" ne "noise" and "wt" gt "0.0"'
           if dat_select(s_l,command,/reset,/no) gt 0 then begin
              ; Set frequency - used to sort the data later on
                freq[nn] = mean(sp(psl).fsky)
                band[nn] = c.band[iband(j)]
                sb[nn]   = strupcase(c.sb[isb(i)])

              ; Loop over baselines
                n = 0
                for i1=1,!NTEL-1 do begin
                for i2=i1+1,!NTEL do begin
                   ; Set filter
                     bls[n] = strcompress(string(i1) + '-' + string(i2),/remove)
                     com = command + ' and "blcd" eq "' + bls[n] + '"'

                   ; Compute average coherence
                     if dat_list(s_l,com,/reset,/no) gt 0 then $
                        coh[nn,n] = total(bl(pbl).coh) / n_elements(pbl)
                     n = n + 1
                endfor
                endfor

              ; Increment counter
                nn = nn + 1
           endif
     endfor
     endfor

   ; Print results
     if not keyword_set(noprint) then begin
        ; Print header
          printlog,"Average coherences on gain calibrators",unit=unit
          printlog,format='(%"%s ",$)',"Band",unit=unit
          for i=0,nbls_max -1 do printlog,format='(%"%4s ",$)',bls[i],unit=unit
          printlog,"",unit=unit
          printlog,format='(%"%s ",$)',"----",unit=unit
          for i=0,nbls_max -1 do printlog,format='(%"%4s ",$)','----',unit=unit
          printlog,"",unit=unit

        ; Print results
          for i=0,nn-1 do begin
             printlog,format='(%"%s %s",$)',band[i],sb[i],unit=unit
             for k=0, nbls_max-1 do begin
                if coh[i,k] ge 0.0 then $
                   printlog,format='(%" %4.2f",$)',coh[i,k],unit=unit $
                else $
                   printlog,format='(%" %s",$)',"    ",unit=unit
             endfor
             printlog,"",unit=unit
          endfor
     endif
     printlog," ",unit=unit
end
