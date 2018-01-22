; *************************************************************************
; FUNCTION
;      wlm_grid_ini
;
; WRITTEN
;      June 13, 2000 by JMC
;
; PURPOSE
;      Computes residuals for a grid of scale factors
;
; INPUTS
;      None
;
; OUTPUT
;      xfactor   : Grid of scale factors
;      rms       : RMS after applying WLM correction
;      perdiff   : Percent improvement
;      1 if successful
;     -1 if not successful
;
; EXAMPLES
;       result = wlm_grid_ini(xfactor,rms,perdiff)
;
; *************************************************************************
function wlm_grid_ini,xfactor,rms,perdiff,noplot=noplot
  ; Common blocks
    common global
    common data_set
    common wlm

  ; BOOLEANS
    iplot = 1 - keyword_set(noplot)

  ; Return if not plotting data
    if (wlm_filter eq '0') then begin
       xfactor = [0.0]
       rms = [0.0]
       perdiff = [0.0]
       return,1
    endif

  ; Filter the data
    if (dat_list(s_l,wlm_filter,/reset,/no_notify) le 0) then begin
       print,"Never here: WLM"
       print,wlm_filter
       return,-1
    end

  ; Derive the scale factor for each baseline/integration/sideband
    scale_init = 12.0
    result = wlm_correlate(wlm_diff,scale_init,ave_time=ave_time,$
                           phases_before=phases_before,$
                           sigma_before=sigma_before,sigma_after=sigma_after,$
                           factor=factor,/get_wlm,frames=frames)

  ; Set good and bad WLM values
    jbad  = where(wlm_diff le !BAD_VALUE,nbad)
    jgood = where(wlm_diff gt !BAD_VALUE,ngood)
    if (ngood eq 0) then begin
       printf,-1,'Cannot derive scale factor'
       printf,-1,'No WLM data present for the calibrator source'
       wlm_scale_use = 1.0
       return,-1
    endif

  ; Initialize variables to generate grid of scale factors/phase residuals
    nstart = 2
    nstop  = 22
    nstep  = 2
    ngrid  = (nstop-nstart)/nstep + 1
    phases = phases_before(jgood)
    rms    = stddev(phases)
    ids    = replicate(0,ngood)
    xsample = lindgen(ngood)+1
    xfactor = 0.0

  ; Generate and plot of phase residuals with various scale factors
    for i = nstart, nstop, nstep do begin
       ; Compute residuals for this scale factor
         result = wlm_correlate(wlm_diff,float(i),$
                       phases_before=phases_before,phases_after=phases_after)

       ; Compute residuals
         residuals = phases_after(jgood)-wlm_diff(jgood)*i

       ; Compute residuals rms
         rms = [rms,stddev(residuals)]

       ; Store results
         phases  = [phases,residuals]
         ids     = [ids,replicate(i,ngood)]
         xsample = [xsample,lindgen(ngood)+1]
         xfactor = [xfactor,1.0*i]
    endfor

  ; Print table of before/after residuals
    print," "
    print," "
    print,strcompress("#       RESULTS FOR TRACK " + string(in(pil(0)).traid))
    print,"#   SCALE        RMS       Percent"
    print,"#  FACTOR       [mm]       Decrease"
    print,"# --------   ----------  -----------"
    for i = 0L, n_elements(xfactor)-1L do begin
        if (xfactor[i] eq 0.0) then begin
           print,format='("  Raw Data   ",F8.3,"         ****")',rms[i]
        endif else begin
           print,format='(3x,F5.2,6x,F7.3,6x,F7.1)',xfactor[i],rms[i],$
                    -(rms[i]-rms[0])/rms[0]*100.0
        endelse
    endfor
    print,"# NOTE: PERCENT DECREASE SHOULD BE POSITIVE IF"
    print,"#       THE WLM CORRECTIONS IMPROVED THE DATA"
    print," "
    print," "

  ; Compute precent difference
    perdiff = -(rms - rms[0]) / rms[0] * 100.0
    perdiff[0] = 0.0;

  ; Plot grid
    blabel = $
        'Track  ' + string(in(pil(0)).traid)
    blabel = strcompress(blabel)
    a = findgen(16) * (!DPI*2/16.)
    usersym,0.25*cos(a),0.25*sin(a),/fill
    ys = fltarr(1,n_elements(phases))
    ys[0,*] = phases
    if (e.java eq 0) then begin
       result = wlm_plot_data(xsample,ys,xsample,ids,$
                    "Sample number",["Residuals"],$
                    psym=[8,8,8],plot=iplot,$
                    m_options="cspne",blabel=blabel,nframes_max=ngrid+1)
    endif

  ; Print table of before/after residuals
    print," "
    print," "
    print,strcompress("#       RESULTS FOR TRACK " + string(in(pil(0)).traid))
    print,"#   SCALE        RMS       Percent"
    print,"#  FACTOR       [mm]       Decrease"
    print,"# --------   ----------  -----------"
    for i = 0L, n_elements(xfactor)-1L do begin
        if (xfactor[i] eq 0.0) then begin
           print,format='("  Raw Data   ",F8.3,"         ****")',rms[i]
        endif else begin
           print,format='(3x,F5.2,6x,F7.3,6x,F7.1)',xfactor[i],rms[i],$
                    perdiff[i]
        endelse
    endfor
    print,"# NOTE: PERCENT DECREASE SHOULD BE POSITIVE IF"
    print,"#       THE WLM CORRECTIONS IMPROVED THE DATA"
    print," "
    print," "

  ; Done - return number of plot pages
    return,1
end
