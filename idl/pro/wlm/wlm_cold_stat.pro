function print_stat,xave,xave_lin,xrms,xrms_lin,itel,itype,icold,$
                    box_noise=box_noise
   ; Common blocks
     common global
     common data_set

   ; Initialize
     use  = intarr(2,!NTEL)
     ave  = dblarr(2,!NTEL)
     rms  = dblarr(2,!NTEL)
     ave_lin = dblarr(2,!NTEL)
     rms_lin = dblarr(2,!NTEL)
     rms_mean = dblarr(2,!NTEL)

   ; Loop over and load types and telescopes
     for j=1,2 do begin
     for i=0L,!NTEL-1L do begin
        ; Compute average
          k = where(itel eq i+1 and itype eq j, nk)
          if (nk gt 0) then begin
            use[j-1,i] = 1
            ave[j-1,i] = median(xave[k],/even)
            rms[j-1,i] = median(xrms[k],/even)
            ave_lin[j-1,i] = median(xave_lin[k],/even)
            rms_lin[j-1,i] = median(xrms_lin[k],/even)
            rms_mean[j-1,i] = stddev(xave[k])
          endif
     endfor
     endfor

   ; Save observed RMS noise on cold load
     box_noise = rms_lin[1,*]

   ; Print results
     track  = in(0).traid
     nscans = n_elements(in)
     if keyword_set(track) then $
         print,format='("# Track ",I4,", nscans = ",I3)',track,nscans
     print,'#                    Sky                             Cold Load'
     print,'#       -------------------------------  ---------------------------------'
     print,'#  Ant    Mean  RMS_mean   RMS  RMS_lin    Mean  RMS_mean   RMS  RMS_lin'
     for i = 0, !NTEL-1L do begin
        ; Antenna
          print,format='($,5x,I1)',i+1

        ; Loop over type
          for j=0,1 do begin
             if (use[j,i]) then begin
                print,format='($,1x,4(1x,F7.4))',$
                 ave[j,i],rms_mean[j,i],rms[j,i],rms_lin[j,i]
             endif else begin
                print,format='($,A)',"     ...     ...     ...     ... "
             endelse
             print,format='($,1x)'
          endfor
          print,""
     endfor
     print,""
     print,""
end

function wlm_cold_stat,dt_wlm,wts,iload=iload,box_noise=box_noise
   ; Common blocks
     common global
     common data_set
     common wlm
     common plo

   ; Get the minute mark on which the integrations where started
     ut_min = fix((in.dhrs - fix(in.dhrs)) * 60.0)

   ; Integrations started near 0,10,20,30,40,50,60 minutes are cold load
   ; Integrations started near 5,15,25,35,45,55    minutes are sky
     isky  = 1
     icold = 2
     iload = (ut_min mod 10) / 5 + 1

   ; Compute start/stop times for each astronomy integration
     tastro_start = in.dhrs
     tastro_stop  = tastro_start + (in.rinteg / 3600.) - 30.0/3600.0

   ; Set start/stop times for each WLM record (WLM integration time = dt_wlm)
     twlm_start = wlm_times - (dt_wlm / 2.0)
     twlm_stop  = twlm_start + dt_wlm

   ; Initialize
     nscans   = n_elements(in)
     itype    = [-1]
     itel     = [-1]
     mean_wlm = [0.0]
     rms_wlm  = [0.0]
     mean_wlm_lin = [0.0]
     rms_wlm_lin  = [0.0]

   ; Initialize weights
     jbad = where(wlm_wts lt 0,nbad)
     wts  = intarr(n_elements(wlm_wts))

   ; Create output files
     for i = 0, 1 do begin
        type = 'sky'
        if (i eq 1) then type = 'cold'
        for j = 1, !NTEL do begin
           output = strcompress('wlm_'+type+'_' + string(j) + '.dat',/remove)
           openw,unit,output,/get_lun
           close,unit
           free_lun,unit
        endfor
     endfor

   ; Loop over astronomy integrations
     for i = 0L, nscans-1L do begin 
        ; Find valid wlm wlm time stamps for this integration
          j = where(twlm_start le tastro_stop[i] and $
                    twlm_stop  ge tastro_start[i],nj)

        ; Prepare output file
          type = 'wlm_sky_'
          if (iload[i] eq 2) then type = 'wlm_cold_'

        ; Loop over WLM boxes
          for k=0L,wlm_ntel-1L do begin
             ; Find IDS for this box
               l = where(wlm_id eq wlm_itel[k],nl)

             ; Find good WLM values
               good = where(wlm_wts(l(j)) gt 0,ngood)

             ; Compute stats
               if (ngood gt 2) then begin
                 ; Store data
                   x = wlm_cal(l(j(good)))
                   x0 = x

                 ; Save itype/telescope parameters
                   itype   = [itype,iload[i]]
                   itel    = [itel,wlm_itel[k]]
                   wts[l(j(good))] = iload[i]

                 ; Loop over data twice. One second pass, remove linear term
                   for m=0,2 do begin
                     ; Remove constant or linear term
                       if (m eq 0) then begin
                         x1 = x - mean(x)
                       endif else if (m eq 1) then begin
                         uti_fit_lin,lindgen(ngood),x0,a,siga,b,sigb
                         x  = x0 - (a[0] + b[0]*lindgen(ngood)) + mean(x0)
                         x2 = x - mean(x0)
                       endif else begin
                         coef = [-1, -1, -1]
                         z = lindgen(n_elements(x))
                         result = poly_fit(z,x0,2)
                         x  = x0 - poly(z,result) + mean(x0)
                         x3 = x - mean(x0)
                       endelse

                     ; Compute stats results
                       ave = mean(x)
                       rms = stddev(x)

                     ; Save results
                       if (m eq 0) then begin
                          mean_wlm = [mean_wlm,ave]
                          rms_wlm  = [rms_wlm,rms]
                       endif else if (m eq 1) then begin
                          mean_wlm_lin = [mean_wlm_lin,ave]
                          rms_wlm_lin  = [rms_wlm_lin,rms]
                       endif

                     ; Plot data
                       if (0 and k eq 1 and iload[i] eq isky and m eq 0) then begin
                          print,mean(x),stddev(x)
                          plot,x,yrange=[min(x),max(x)]
                          print,'Hit any key to continue'
                          result = get_kbrd(1)
                       end
                   endfor

                 ; Open output file
                   output = strcompress(type + string(wlm_itel[k])+'.dat',/rem)
                   openw,unit,output,/append,/get_lun
                   for m=0L, n_elements(x0)-1L do begin
                      printf,unit,format='(I5,4(3x,F9.2))',$
                             in(i).int,x0[m]*1e3,x1[m]*1e3,x2[m]*1e3,x3[m]*1e3
                   endfor
                   close,unit
                   free_lun,unit
               endif
          end
     endfor

   ; Set any weights
     if (nbad gt 0) then wts(jbad) = 0

   ; Print statistics
     result = print_stat(mean_wlm,mean_wlm_lin,rms_wlm,rms_wlm_lin,$
                         itel,itype,icold,box_noise=box_noise)
end
