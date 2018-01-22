function wlm_xfit2,wlm_k,phases_deg,wavelength,time,xfactor,sig_xfactor,$
                   pbefore=pbefore,pderived=pderived,$
                   rms_before=rms_before,rms_after=rms_after,$
                   plot=plot,track=track,sb=sb,band=band,$
                   bsl=bsl,rec=rec,source=source,iscan=iscan,$
                   derive=derive

    ; Booleans
      iplot   = keyword_set(plot)
      ilinear = keyword_set(linear)

    ; Constant to convert from degrees to wavelength
      c = wavelength / 360.0

    ; Compute stddev of phases before wlm corrections are applied
      pbefore = uti_pha_180(phases_deg)
      result  = uti_pha_unwrap(pbefore)
      pbefore = pbefore * c
      if (ilinear) then begin
         uti_fit_lin,time,pbefore,a,siga,b,sigb
         pbefore = pbefore - (b(0)*time+ a(0))
      endif else begin
         pbefore = pbefore - mean(pbefore)
      endelse
      rms_before = stddev(pbefore)

    ; Derive scale factor
      if keyword_set(derive) then begin
         uti_fit_lin,wlm_k,pbefore,a,siga,b,sigb
         xfactor     = b[2]
         sig_xfactor = sigb[2]
      endif

    ; Compute residuals
      wlm_mm = wlm_k*xfactor[0]
      residuals = phases_deg - (wlm_mm / c)
      residuals = uti_pha_180(residuals)
      result = uti_pha_unwrap(residuals)
      residuals = residuals * c

    ; Remove any drift from phase residuals
      if (ilinear) then begin
         uti_fit_lin,time,residuals,a,siga,b,sigb
         residuals = residuals - (b(0)*time+ a(0))
      endif else begin
         residuals = residuals - mean(residuals)
      endelse
      rms_after = stddev(residuals)

    ; Compute "raw" phases
      pderived = residuals + wlm_mm

    ; Plot data 
      if iplot then begin
        if (sb eq 'l') then sb = 'LSB'
        if (sb eq 'u') then sb = 'USB'
        str = source
        str = str + ", Scan " + string(iscan)
        str = str + ", BSL " + bsl
        str = str + ", " + sb
        str = str + ", Rx " + string(rec)
        str = strcompress(str)
        change = (rms_before-rms_after)/rms_before*100.0
        blabel = $
            'Track  ' + string(track) + "; " + $
            'X = ' + string(xfactor[0],format='(F5.2)') + " mm/K; " +$
            'RMS (before) = ' + string(rms_before,format='(F8.2)') + " mm; "  +$
            'RMS (after)  = ' + string(rms_after,format='(F8.2)') + " mm; "   +$
            'Change = ' + string(change,format='(F8.1)') + "%"
        blabel = strcompress(blabel)
        npts = n_elements(pderived)
        wts  = make_array(npts,/int,value=1)
        ids  = make_array(npts,/string,value=str)
        xs   = (time - time(0)) * 60.0
        y    = fltarr(2,npts)
        yfit = fltarr(2,npts)
        ; y[0,*] = pderived
        y[0,*] = pbefore
        y[1,*] = residuals
        yfit[0,*] = wlm_mm
        result = wlm_plot_data(time,y,wts,ids,$
                  "Time [hours]",["Phase","Residuals"],$
                  yfit=yfit,psym=[0,0],$
                  plot=plot,m_options="cspne",blabel=blabel,/same)
      endif

    ; Return residuals
      return,1
end
