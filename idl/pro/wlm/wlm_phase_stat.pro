function print_phase_stat,rms_before,rms_after,iload,box_noise,xfactor,lambda
   ; Common blocks
     common global
     common data_set
     common wlm

   ; Initialize
     med1 = dblarr(2)
     med2 = dblarr(2)
     npts = lonarr(2)

   ; Print header
     print,format='("# Track ",I4,", nscans = ",I3)',in(0).traid,n_elements(in)
     print,'#        Expected              Cold Load                                Sky
     print,'#      --------------   ------------------------    ---------------------------------------------'
     print,'#  BSL   WLM  WLM+src   RMS_bef  RMS_aft   %Diff    RMS_bef  RMS_aft   %Diff   Coh_bef   Coh_aft'
     print,"#  (1)   (2)     (3)      (4)      (5)      (6)       (7)      (8)      (9)      (10)     (11)"

   ; Loop over baselines
     for i = 0L,n_elements(c.blcd)-1L do begin
        ; Read the antennas
          reads,c.blcd[i],j1,j2,format='(I1,1x,I1)'

        ; Compute expected noise
          rms_exp = xfactor * (box_noise[j1-1]^2 + box_noise[j2-1]^2)^0.5

        ; Were WLM box online for both antennas?
          j = where(j1 eq wlm_itel or j2 eq wlm_itel,nj)
          ionline = (nj eq 2)

        ; Loop over sky/cold loads
          for j = 1, 2 do begin
             ; Find data
               k = where(rms_before[*,i] ge 0.0 and rms_after[*,i] and iload eq j,nk)

             ; Get median value
               med1[j-1] = median(rms_before[k,i],/even)
               med2[j-1] = median(rms_after[k,i],/even)
               npts[j-1] = nk
          endfor
          coh_before = exp(-0.5*(med1/lambda*2.0*!DPI))
          coh_after  = exp(-0.5*(med2/lambda*2.0*!DPI))

        ; Print baseline
          print,format='($,3x,A3)',c.blcd[i]

        ; Print WLM noise for this baseline
          if (ionline) then begin
            rms_exp2 = (rms_exp^2 + med1[0]^2)^0.5
            print,format='($,2(2x,F5.3))',rms_exp,rms_exp2
          endif else begin
            print,format='($,A)',"   ...     ..."
          endelse

        ; Loop over hot/cold loads
          for j=1,0,-1 do begin
             if (npts[j] gt 0) then begin
                ; Print beginning phase rms
                  print,format='($,3x,F7.3)',med1[j]

                ; Print change in rms
                  if (ionline) then begin
                     xchange = (med1[j] - med2[j]) / med1[j] * 100.0
                     print,format='($,2x,F7.3,1x,F8.2)',med2[j],xchange
                  endif else begin
                    print,format='($,A)',"      ...      ..."
                  endelse

                ; Print coherences if one sky
                  if (j eq 0) then begin
                     print,format='($,2x,F7.2)',coh_before[j]
                     if (ionline) then begin
                        print,format='($,2x,F7.2)',coh_after[j]
                     endif else begin
                        print,format='($,2x,A)',"    ..."
                     endelse
                  endif
             endif else begin
                print,format='($,A)',"     ...     ...   ..."
             endelse
             print,format='($,8x)'
          endfor
          print,""
     endfor
     print,""
     print,""
end

function wlm_phase_stat,rms,coh,rms2,coh2,iload,box_noise,xfactor,$
                        linear=linear,print=print
   ; Common blocks
     common global
     common data_set

   ; Initialize
     nscans = n_elements(in)
     nbaselines = n_elements(c.blcd)
     coh  = make_array(nscans,nbaselines,/double)
     rms  = make_array(nscans,nbaselines,/double,value=!BAD_VALUE)
     lambda = dblarr(nscans)

   ; Set conversion factors from degrees to millimeters
     lambda = !CVEL / sp(psl).fsky / 1e6
     deg_to_mm = lambda / 360.0

   ; Loop over data
     for i = 0L, n_elements(pil)-1L do begin
        ; Set index
          iblcd = bl(pbl[i]).iblcd
          iscan = in(pil[i]).int
          j = where(in.int eq iscan,nj)
          iscan = j[0]

        ; Get phase data for this integration
          js = [i]
          dat_get_rows,cmp,amp,pha,t,wts,pt_first,pt_npts,"hours",js,/amp_pha

        ; Unwrap phases
          result = wlm_unwrap(t,pha,linear=linear)

        ; Convert to millimeters
          pha = pha * deg_to_mm[i]

        ; Compute rms
          rms[iscan,iblcd] = stddev(pha)

        ; Set coherence
          coh[iscan,iblcd] = bl[pbl[i]].coh
     endfor

   ; Print results
     if (keyword_set(print)) then $
       result = print_phase_stat(rms2,rms,iload,box_noise,xfactor,lambda[0])

   ; Done
     return,1
end
