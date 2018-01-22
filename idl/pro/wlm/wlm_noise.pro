; wlm_noise will read in a WLM test track that takes data for
; 5 minutes on a cold load, and 5 minutes on sky, and analyze
; the noise statistics.

pro wlm_noise,iload=iload,defaults=defaults,noplots=noplots,$
              postscript=postscript,outroot=outroot,box_noise=box_noise
   ; Common blocks
     common global
     common data_set
     common wlm
     common plo

   ; BOOLEANS --- plotting and output
     ilog        = keyword_set(mklog)
     iplot       = 1 - keyword_set(noplots)
     ipostscript = keyword_set(postscript) and iplot

   ; BOOLEANS --- read/apply/write
     idefaults = keyword_set(defaults)

   ; Set start/stop times for each WLM record (integration time = 2 seconds)
     dt_wlm = 2.0D / 3600.0D

   ; Determine WLM statistics when on cold load, and when on sky
   ; This also sets the weights such that
   ;    wts<= 0 --> Bad WLM data
   ;    wts = 1 --> WLM taken during sky integration
   ;    wts = 2 --> WLM taken during cold load integration
     result = wlm_cold_stat(dt_wlm,wts,iload=iload,box_noise=box_noise)

   ; Subtract mean for each wlm box
     for i = 0L, wlm_ntel-1L do begin
        j = where(wlm_id eq wlm_itel[i] and wlm_wts gt 0,nj)
        if (nj gt 0) then begin
           ave = total(wlm_cal(j)) / nj
           wlm_cal(j) = wlm_cal(j) - ave
           k = where(abs(wlm_cal(j)) gt 40.0,nk)
           if (nk gt 0) then begin
              wlm_cal(j(k)) = !BAD_VALUE
              wlm_wts(j(k)) = !BAD_VALUE
           endif
        endif
     endfor

   ; Make a plot of the WLM data on sky
     if iplot then begin
        if ipostscript then wlm_open,strcompress(outroot + '_sky.ps')
        ydata = reform(transpose(wlm_cal),1,n_elements(wlm_cal))
        blabel = strcompress ('WLM on sky [K] vs Time ' + $
             ' [Track ' + string(in(pil[0]).traid) + ']')
        j = where(wts ne 1,nj)
        wts_tmp = wts
        if (nj gt 0) then wts_tmp(j) = 0
        if (nj gt 0) then ydata[0,j] = !BAD_VALUE
        result = wlm_plot_data(wlm_times,ydata,wts_tmp,$
                     wlm_id,"Time (hours)", ["WLM"],plot=iplot,$
                     m_options="cspne",blabel=blabel,/expand,plid)
        if ipostscript then wlm_close
     endif

   ; Make a plot of the WLM data on cold load
     if iplot then begin
        if ipostscript then wlm_open,strcompress(outroot + '_cold.ps')
        ydata = reform(transpose(wlm_cal),1,n_elements(wlm_cal))
        blabel = strcompress ('WLM on cold load [K] vs Time ' + $
             ' [Track ' + string(in(pil[0]).traid) + ']')
        j = where(wts ne 2,nj)
        wts_tmp = wts
        if (nj gt 0) then wts_tmp(j) = 0
        if (nj gt 0) then ydata[0,j] = !BAD_VALUE
        result = wlm_plot_data(wlm_times,ydata,wts_tmp,$
                     wlm_id,"Time (hours)", ["WLM"],plot=iplot,$
                     m_options="cspne",blabel=blabel,/expand,plid)
        if ipostscript then wlm_close
     endif

   ; Done!
     return
end
