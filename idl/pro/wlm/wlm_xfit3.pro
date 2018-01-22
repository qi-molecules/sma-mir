function wlm_xfit3,wlm_k,phases_deg,wavelength,time,$
                   xfactor,labels,pderived=pderived,$
                   rms_before=rms_before,rms_after=rms_after,$
                   derive=derive,linear=linear,sig=sig,plot=plot,track=track

    ; Booleans
      iplot   = keyword_set(plot)
      ilinear = keyword_set(linear)

    ; Unwrap phases
      pha = phases_deg
      result = wlm_unwrap(time,pha,labels=labels,linear=linear,frames=frames)

    ; Constant to convert from degrees to wavelength
      c = wavelength / 360.0

    ; Compute stddev of phases before wlm corrections are applied
      pbefore = pha * c
      rms_before = stddev(pbefore)

    ; Derive scale factor
      if keyword_set(derive) then begin
         uti_fit_lin,wlm_k,pbefore,a,siga,b,sigb
         xfactor = b[2]
         sig     = sigb[2]
         scale = xfactor
      endif else if not keyword_set(xfactor) then begin
         scale = 0.0
      endif else begin
         scale = xfactor[0]
      endelse

    ; Compute residuals
      wlm_mm = wlm_k*scale
      residuals = pha - (wlm_mm / c)
      result = wlm_unwrap(time,residuals,labels=labels,linear=linear)
      residuals = residuals * c
      rms_after = stddev(residuals)

    ; Compute "raw" phases
      pderived = residuals + wlm_mm

    ; Plot data 
      if iplot then begin
         ; Loop over unique frames
           for i= 0L, n_elements(frames)-1L do begin
              ; Find data
                j = where(labels eq frames[i],nj)
                if (nj gt 1) then begin
                  ;  Set data
                     npts = nj
                     wts  = make_array(npts,/int,value=1)
                     ids  = make_array(npts,/string,value=frames[i])
                     xs   = (time(j) - time(0)) * 60.0
                     y    = fltarr(2,npts)
                     yfit = fltarr(2,npts)
                     ; y[0,*] = pderived
                     y[0,*] = pbefore(j)
                     y[1,*] = residuals(j)
                     yfit[0,*] = wlm_mm(j)

                   ; Compute rms before/after
                     sig_before = stddev(y[0,*])
                     sig_after  = stddev(yfit[0,*])
                     change = (sig_before-sig_after)/sig_before*100.0

                   ; Set blabel
                     blabel = ""
                     if keyword_set(track) then $
                        blabel = 'Track  ' + string(track) + "; "
                     blabel = blabel + $
                         'X = ' + string(scale,format='(F5.2)') + " mm/K; " +$
                         'RMS (before) = ' + string(sig_before,format='(F8.2)') + " mm; "  +$
                         'RMS (after)  = ' + string(sig_after,format='(F8.2)') + " mm; "   +$
                         'Change = ' + string(change,format='(F8.1)') + "%"
                     blabel = strcompress(blabel)

                   ; Plot data
                     result = wlm_plot_data(time(j),y,wts,ids,$
                                "Time [hours]",["Phase","Residuals"],$
                                yfit=yfit,psym=[0,0],$
                                plot=plot,m_options="cspne",blabel=blabel,/same)
                endif
           endfor
      endif
    ; Return residuals
      return,1
end
