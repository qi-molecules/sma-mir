function wlm_scale_derive,wlm_diff,phases,lambda,itel1,itel2,iscan,times,$
                          ave_time=ave_time,factor=factor,plot=plot,$
                          linear=linear,sigma_before=sigma_before,$
                          sigma_after=sigma_after
  ; Common blocks
    common global
    common data_set

  ; Booleans
    iplot = keyword_set(plot)

  ; Find unique scan numbers
    scans = uti_distinct(iscan,nscans,/many)
    j = where(scans gt 0,nscans)
    if (nscans eq 0) then begin
       printf,unit,"No scans to derive scale factor"
       stop
    end
    scans = scans(j)

  ; Allocate memory to store derived factors
    ave_time = make_array(nscans,/float,val=!BAD_VALUE)
    factor   = make_array(nscans,/float,val=!BAD_VALUE)
    sigma_before = make_array(nscans,/float,val=!BAD_VALUE)
    sigma_after  = make_array(nscans,/float,val=!BAD_VALUE)
    bsl = string(itel1) + "-" + string(itel2)

  ; Loop over the scans
    for i = 0,nscans-1 do begin
       ; Find all data for this scan
         j = where(iscan eq scans[i],nj)
         k = where(wlm_diff(j) ne !BAD_VALUE and phases(j) ne !BAD_VALUE,nk)
         if (nk gt 0) then begin
            ; Set subset of WLM and phase data
              w = wlm_diff[j(k)]
              p = phases[j(k)]
              wavelength = lambda[j(k)]
              t = times[j(k)]
              ave_time(i) = total(t) / nj

            ; Find points where both wlm and phases are OK
              result = wlm_xfit3(w,p,wavelength,t,xfactor,bsl[j(k)],$
                                 /derive,sig=sig_xfactor,linear=linear,$
                                 rms_before=rms_before,rms_after=rms_after)
              factor[i] = xfactor
              sigma_before[i] = rms_before
              sigma_after[i]  = rms_after
         endif

      ; Plot data
        if (iplot and nk gt 0) then begin
           y    = fltarr(1,nk)
           y[0,*] = phases[j(k)] / 360.0 * lambda[j(k)]
           wts = make_array(nk,/int,value=1)
           str = "Scan = " + string(scans[i])
           str = str + "  X = " + string(xfactor,format='(F8.3)')
           str = strcompress(str)
           str = " "
           ids = make_array(nk,/string,value=str)
           blabel = "X " + string(xfactor,format='(F8.2)') + " +/- " + $
                    string(sig_xfactor,format='(F8.2)') + " mm/K " + $
                    " Track " + string(in(pil(0)).traid) + $
                    " Scan " + string(scans[i])
           blabel = strcompress(blabel)
           a = findgen(16) * (!DPI*2/16.)
           usersym,cos(a),sin(a),/fill
           result = wlm_plot_data(factor[i]*w,y,wts,ids,$
                  "WLM [mm]",["Phases [mm]"],yfit=yfit,psym=[8],$
                  plot=iplot,m_options="cspne",blabel=blabel)
        endif
    endfor
end
