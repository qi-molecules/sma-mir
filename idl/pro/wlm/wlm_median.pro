; *************************************************************************
; FUNCTION
;      wlm_median
;
; WRITTEN
;      June 13, 2000 by JMC
;
; PURPOSE
;      Computes median scale factor
;
; INPUTS
;      None
;
; OUTPUT
;      1 if successful
;     -1 if not successful
;
; EXAMPLES
;       result = wlm_median()
;
; *************************************************************************
function wlm_median,plid,noplot=noplot,postscript=postscript,outroot=outroot
  ; Common blocks
    common global
    common data_set
    common wlm

  ; BOOLEANS
    iplot = 1 - keyword_set(noplot)
    ipostscript = (iplot and keyword_set(postscript) and keyword_set(outroot))

  ; Derive the scale factor for each baseline/integration/sideband
    scale_init = 12.0
    result = wlm_correlate(wlm_diff,scale_init,ave_time=ave_time,$
                           factor=factor,/derive,/get_wlm)

  ; Set median scale factor
    xadopt  = 0.0
    xmedian = 0.0
    jgood = where(factor gt !BAD_VALUE,ngood)
    jbad  = where(factor le !BAD_VALUE,nbad)
    if (ngood gt 0) then xmedian = median(factor(jgood),/even)
    wlm_xfactor = xmedian
      
  ; Plot conversion factors
    if (iplot and ngood gt 0) then begin
       npts = n_elements(pil)
       x = ave_time - ave_time[jgood(0)]
       if (nbad gt 0) then x[jbad] = !BAD_VALUE
       y    = fltarr(1,npts)
       yfit = fltarr(1,npts)
       y[0,*] = factor
       yfit[0,*] = xmedian
       wts = make_array(npts,/int,value=1)
       str = strcompress("Median = " + string(xmedian,format='(F5.2)'))
       ids = make_array(npts,/string,value=str)
       blabel = "Scale factor [mm/K] vs Time for Track " + string(in(pil(0)).traid)
       blabel = strcompress(blabel)
       a = findgen(16) * (!DPI*2/16.)
       usersym,cos(a),sin(a),/fill
      result = wlm_plot_data(x,y,wts,ids,$
                 "Time [hours]",["X"],plid,yfit=yfit,psym=[8],$
                 plot=iplot,m_options="cspne",blabel=blabel)
    endif

  ; Done - return number of plot pages
    return,1
end
