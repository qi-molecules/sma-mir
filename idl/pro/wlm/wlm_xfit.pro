; *************************************************************************
; FUNCTION
;      wlm_xfit
;
; WRITTEN
;      November 28, 1999 by JMC
;
; PURPOSE
;      Applies WLM corrections to astronomy data and unwraps the phases
;
; INPUTS
;      xfactor   : Scale factor from Kelvin to millimeters of delay
;      pbefore   : Raw astronomy phases (mm) before WLM corrections
;      pderived  : Raw astronomy phases (mm), but now correctly phase 
;                  unwrapped using WLM data
;      rms_before: Phase rms (mm) in pbefore
;      rms_after : Phase rms (mm) after applying WLM corrections
;      plot      : If set, plot WLM and astronomy data
;      track     : Track number   - used when plotting data
;      sb        : Sideband       - used when plotting data
;      band      : Continuum band - used when plotting data
;      bsl       : Baseline       - used when plotting data
;      rec       : Receiver       - used when plotting data
;      Source    : Source name    - used when plotting data
;      Scan      : Scan number    - used when plotting data
;
; OUTPUT
;      -1   Failed.
;       1   Successful
;
; EXAMPLES
;       sb     = c.sb[bl[pbl(i)].isb]
;       band   = c.band[sp[psl(i)].iband]
;       bsl    = c.blcd[bl[pbl(i)].iblcd]
;       rec    = c.rec[bl[pbl[i]].irec]
;       iscan  = in[pil(i)].int
;       source = c.source[in(pil[i]).isource]
;       track  = in(pil[i]).traid
;       result = wlm_xfit(xfactor,pderived=pderived,$
;                         pbefore=pbefore,sb=sb,band=band,bsl=bsl,$
;                         rms_before=rms_before,rms_after=rms_after,$
;                         plot=plot,track=track,rec=rec,source=source,$
;                         iscan=iscan)
;
; *************************************************************************

function wlm_xfit,xfactor,$
                  pbefore=pbefore,pderived=pderived,$
                  rms_before=rms_before,rms_after=rms_after,$
                  plot=plot,track=track,sb=sb,band=band,$
                  bsl=bsl,rec=rec,source=source,iscan=iscan

    ; Common blocks
      common wlm_scale_factor

    ; Booleans
      iplot = keyword_set(plot)

    ; Constant to convert from degrees to wavelength
      c = wavelength / 360.0

    ; Compute stddev of phases before wlm corrections are applied
      pbefore = uti_pha_180(phases_deg)
      result  = uti_pha_unwrap(pbefore)
      pbefore = pbefore * c
      ;uti_fit_lin,time,pbefore,a,siga,b,sigb
      ;pbefore = pbefore - (b(0)*time+ a(0))
      pbefore = pbefore - mean(pbefore)
      rms_before = stddev(pbefore)

    ; Apply WLM correction
      wlm_mm = wlm_k*xfactor[0]
      ; rms_wlm_deg = stddev(wlm_mm) / c
      residuals = phases_deg - (wlm_mm / c)
      residuals = uti_pha_180(residuals)
      result = uti_pha_unwrap(residuals)
      residuals = residuals * c

    ; Remove any linear drift from phase residuals
      ;uti_fit_lin,time,residuals,a,siga,b,sigb
      ;residuals = residuals - (b(0)*time+ a(0))
      residuals = residuals - mean(residuals)
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
      return,residuals/wlm_err
      ; return,residuals/rms_wlm_deg
end
