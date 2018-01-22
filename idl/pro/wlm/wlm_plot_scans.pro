; *************************************************************************
; PROCEDURE
;      wlm_plot_scans
;
; WRITTEN
;      September 6, 2001 by JMC
;
; PURPOSE
;      Plots wlm and phase data on scan-by-scan basis
;
; INPUTS
;      xfactor  : Scale factor from Kelvin to mm of delay
;      /linear  : If set, subtract linear fit from WLM data
;      /noise   : If set, indicates this is a noise track
;      /track   : If set, load this track
;      /noread  : Use track stored in memory
;
; OUTPUT
;      1 -> Successful
;     -1 -> Not successful
;
; EXAMPLES
;      result = wlm_plot_scans(10.0,/linear)
;
; *************************************************************************

function wlm_plot_scans,xfactor,linear=linear,noise=noise,track=track,$
                        postscript=postscript
  ; Common blocks
    common global
    common data_set
    common wlm

  ; Color lookup table
    loadct,39,/silent

  ; Booleans
    ilinear = keyword_set(linear)
    inoise  = keyword_set(noise)
    ipostscript = keyword_set(postscript)

  ; Read data
    if keyword_set(track) then begin
      if (not keyword_set(noread)) then begin
         result = dbi_track_read(track)
         if result eq -1 then begin
            printf,-1,'Cannot load track ' + string(track)
            return,-1
         endif
      endif
      if (inoise) then begin
         if (dat_list(s_l,'"band" like "c1" and "sb" like "l"',/reset,/no_notify) eq 0) then begin
            printf,-1,"No continuum data to plot"
            return,-1
         endif
      endif
    endif

  ; Read the WLM data
    result = wlm_db_read(tel_online=tel_online,dt_smooth=0.0,$
                         ut_cal=ut_cal,itel_cal=itel_cal,nodrop=nodrop,$
                         mklog=mklog,outroot=outroot,tname=tname)
    if (result ne 1) then begin
       printf,-1,'No WLM data for this track'
       return,-1
    endif

  ; Set which WLM boxes are OK to use
    result = wlm_choose_box(use_boxes=use_boxes,/defaults)

  ; Compute gains for the WLM boxes and store calibrated WLM data in wlm_cal.
    result = wlm_calibrate()
    if (result eq -1) then begin
       printf,-1,'Error calibrating WLM boxes'
    endif

  ; Select continuum data only
    if (not inoise) then begin
       if (dat_list(s_l,'"band" like "c"',/reset) eq 0) then begin
          printf,-1,"No continuum data to plot"
          return,-1
       endif
    endif

  ; Regrid WLM data onto astronomy time stamps
    dt_wlm = 2.0D/3600.0D
    result = wlm_regrid(wlm_diff,lambda,wlm_cal,wlm_times,dt_wlm,wlm_id,$
                        times=times,phases=phases,linear=linear,$
                        bsl=bsl,isource=isource,/coherence,/store)
    if (result eq -1) then begin
       printf,-1,"No good phase/wlm data to plot"
       return,-1
    endif

  ; Open postscript file
    output = strcompress('t' + string(in(0).traid) + '.ps',/remove)
    if ipostscript then wlm_open,strcompress(output)

  ; Loop over scans/baselines combinations
    for iscan = 0L, n_elements(in)-1L do begin
    for itel1 = 0L, n_elements(c.tel1)-1L do begin
    for itel2 = 0L, n_elements(c.tel2)-1L do begin
        ; Set baseline/scan id
          scan = in(iscan).int
          baseline = strcompress(c.tel1(itel1) + '-' + c.tel2(itel2),/rem)
          id = strcompress(string(scan) + ":" + baseline,/remove)

        ; Find data
          j = where(bsl eq id,nj)
          if (nj gt 2) then begin
            ; Save data
              t = times(j)
              p = phases(j)
              w = wlm_diff(j) * xfactor

            ; Unwrap phases
              result = wlm_unwrap(t,p,linear=linear)

            ; Convert phases from degrees to millimeters
              deg_to_mm = lambda(j[0]) / 360.0
              p = p * deg_to_mm

            ; Compute rms before
              rms_before = stddev(p)

            ; Subtract wlm data, unwrap, and recompute rms
              psub = (p - w) / deg_to_mm
              result = wlm_unwrap(t,psub,linear=linear)
              psub = psub * deg_to_mm
              rms_after = stddev(psub)

            ; Set arrays for wlm_plot_data
              ydata = transpose([[p],[psub]])
              yfit  = transpose([[w],[replicate(0.0,nj)]])

            ; Set rms label
              srmsb = 'RMS_before=' + string(rms_before,format='(F100.2)')
              srmsa = 'RMS_after=' + string(rms_after,format='(F100.2)')
              srmsb = strcompress(srmsb,/remove)
              srmsa = strcompress(srmsa,/remove)

            ; Percent change
              change = -(rms_after - rms_before) / rms_before * 100.0
              schange = 'Change=' + string(change,format='(F100.2)') + '%'
              schange = strcompress(schange,/remove)

            ; Set plot label
              str = 'Track ' + string(in(0).traid) 
              str = str + ', ' + c.source(isource(j[0]))
              str = str + ", Scan " + string(scan)
              str = str + ", BSL " + baseline
              str = str + ", " + srmsb + ", " + srmsa + ", " + schange
              blabel = strcompress(str)

            ; Set label if noise track
              if (inoise) then begin
                ; Get the minute mark on which the integrations where started
                  ut_min = fix((t[0] - fix(t[0])) * 60.0)

                ; Integrations started near 0,10,20,30,40,50,60 min = cold load
                ; Integrations started near 5,15,25,35,45,55    min = sky
                  iload = (ut_min mod 10) / 5 + 1
                  if (iload eq 1) then begin
                    ids = replicate("sky",nj)
                  endif else begin
                    ids = replicate("cold load",nj)
                  endelse
              endif else begin
                 ids = replicate("",nj)
              endelse

            ; Plot data
              result = wlm_plot_data(times(j),ydata,replicate(1,nj),ids,$
                       yfit=yfit,"Time (hours)",["Phase","Resid"],/plot,$
                       psym=[10,10],m_options="cspne",blabel=blabel)
          endif
    endfor
    endfor
    endfor

  ; Close postscript file
    if ipostscript then wlm_close

  ; Done
    return,1
end
