; *************************************************************************
; FUNCTION
;      wlm_residuals
;
; WRITTEN 
;      June 22, 2000 by JMC
;
; PURPOSE
;      Compute final residuals from WLM corrections
;
; INPUTS 
;      xfactor : Adopted scale factor
;
; OUTPUT
;      -1   Calibration failed.
;       1   Calibrated WLM data stored successfully
;
; EXAMPLES
;      result = wlm_apply_ini(baseline,rms_before,rms_after,perdiff)
; *************************************************************************
function wlm_residuals,plid,baseline,rms_before,rms_after,perdiff,$
                    noplot=noplot,noapply=noapply,write=write,mklog=mklog
  ; Common blocks
    common global
    common data_set
    common wlm

  ; BOOLEANS
    iplot  = 1 - keyword_set(noplot)

  ; Filter the data
    if (wlm_filter eq '0') then begin
       baseline = ['---']
       rms_before = ['---']
       rms_after = ['---']
       perdiff   = ['---']
       return,0
    endif else if (dat_list(s_l,wlm_filter,/reset,/no_notify) le 0) then begin
       print,"Never here: WLM"
       print,wlm_filter
       return,-1
    end

  ; Recompute the phase residuals based on the single scale factor
    result = wlm_correlate(wlm_diff,wlm_xfactor,/get_wlm,$
                   phases_before=phases_before,phases_after=phases_after,$
                   frames=frames)

  ; Set good and bad WLM values
    jbad  = where(wlm_diff le !BAD_VALUE,nbad)
    jgood = where(wlm_diff gt !BAD_VALUE,ngood)
    if (ngood eq 0) then begin
       printf,-1,'Cannot derive scale factor'
       printf,-1,'No WLM data present for the calibrator source'
       wlm_scale_use = 1.0
       return,-1
    endif

  ; Scale the wlm data by the conversion factor
    wlm_diff = wlm_diff * wlm_xfactor
    if (nbad gt 0) then begin
       wlm_diff(jbad) = !BAD_VALUE
       frames(jbad) = ""
    endif

  ; Set frame ids to plot by baseline
    nrec_tot = TOTAL(sp[psl].nrec)
    xs = fltarr(nrec_tot)
    distinct_frames = uti_distinct(frames,nframes,/many)
    j = where(distinct_frames ne "",nframes)
    distinct_frames = distinct_frames[j]

  ; Compute RMS before and after water line corrections
    nrms       = lonarr(nframes+1)
    rms_before = fltarr(nframes+1)
    rms_after  = fltarr(nframes+1)
    rms_change = fltarr(nframes+1)
    lambda = (!CVEL / 1e6) / TOTAL(sp(psl).fsky) * n_elements(psl)
    for i = 0, nframes do begin
       if (i lt nframes) then $
          j = where(frames eq distinct_frames[i],nj) $
       else begin
          nj = n_elements(wlm_diff)
          j  = lindgen(nj)
       endelse
       if (nj gt 2) then begin
          k = where(phases_before(j) gt !BAD_VALUE and $
                    wlm_diff(j) gt !BAD_VALUE,nk)
          if (i ne nframes) then xs[j(k)]=lindgen(nk)
          if nk gt 0 then begin
            rms_before[i] = stddev(phases_before(j(k)))
            rms_after[i]  = stddev(phases_after(j(k))-wlm_diff(j(k)))
            nrms[i] = n_elements(k)
            if (rms_before[i] ne 0.0) then $
              rms_change[i] = (rms_before[i]-rms_after[i]) / rms_before[i]*100.0
          endif
       endif
    endfor

  ; Plot Phase vs. WLM
    ys   = fltarr(1,nrec_tot,/nozero)
    wts  = make_array(nrec_tot,/long,value=1L)
    ys[0,*] = phases_after
    if (nbad gt 0) then wts[jbad] = 0L
    blabel = 'Phase (mm) vs. WLM (mm) for Track ' + string(in(pil[0]).traid)
    blabel = strcompress(blabel)
    a = findgen(16) * (!DPI*2/16.)
    usersym,0.75*cos(a),0.75*sin(a),/fill
;   result = wlm_plot_data(wlm_diff,ys,wts,frames,$
;                "WLM",["Phases"],plid,psym=[8],plot=iplot,$
;                m_options="cspne",blabel=blabel,/same,nframes_max=15)

  ; Create arrays for plotting WLM, phases, and residuals
    ys      = fltarr(3,nrec_tot,/nozero)
    wts     = make_array(nrec_tot,/long,value=1L)
    ys[0,*] = wlm_diff
    ys[1,*] = phases_before
    ys[2,*] = reform(phases_after - ys[0,*])

  ; Flag bad data
    if (nbad gt 0) then begin
       wts(jbad) = 0L
       xs(jbad)  = !BAD_VALUE
       for i = 0, 2 do ys(i,jbad) = !BAD_VALUE
    endif

  ; Plot individual  baselines
    a = findgen(16) * (!DPI*2/16.)
    usersym,0.5*cos(a),0.5*sin(a),/fill
    j = where(phases_before gt !BAD_VALUE and wlm_diff gt !BAD_VALUE,nj)
    w = intarr(n_elements(wlm_diff))
    w(j) = 1
    blabel = 'Track  ' + string(in(pil(0)).traid) + "; " + $
             'X = ' + string(wlm_xfactor,format='(F5.2)') + " mm/K"
    blabel = strcompress(blabel)
    result = wlm_plot_data(xs[j],ys[*,j],w(j),frames(j),$
               "Sample number",["WLM", "Phases", "Residuals"],plid,$
               psym=[8,8,8],plot=iplot,$
               m_options="cspne",nframes_max=1,blabel=blabel,/same)

  ; Print header to indicate changes in the phase rms
    com1 = strcompress('RESULTS FOR TRACK ' + string(in(pil(0)).traid))
    com2 = strcompress('SCALE FACTOR = ' + string(wlm_xfactor,format='(F6.3)'))
    print,format='("# LAMBDA ",F8.4," mm")',lambda
    print,format='("# FREQ   ",F8.4," GHz")',!CVEL/lambda/1e6
    print,"# CONFIG ",c.cocd
    print,format='("#        ********* ",a23," *********")',com1
    print,format='("#        ********* ",a23," *********")',com2
    print,"#"
    print,"# BASELINE   RMS_BEFORE  RMS_AFTER   % Change    NPTS"
    print,"#               [mm]       [mm]"
    print,"# --------   ----------  ---------   --------   ------"
    baseline = ' '
    for i = 0L, nframes do begin
       label  = "All"
       if (i lt nframes) then label = strmid(distinct_frames[i],4,3)
       baseline = [baseline,label]
       if (nrms[i] gt 0L) then $
          print,format='("   ",a7,2("     ",F6.3),"  ",F10.2,"    ",I6)',$
                 label,rms_before[i],rms_after[i],rms_change[i],nrms[i]
    endfor
    baseline = baseline[1:n_elements(baseline)-1L]
    perdiff = fltarr(n_elements(rms_after))
    j = where(rms_before ne 0.0,nj)
    if (nj gt 0) then $
       perdiff[j] = -(rms_after[j] - rms_before[j]) / rms_before[j] * 100.0

  ; Finished
    return,nframes
end
