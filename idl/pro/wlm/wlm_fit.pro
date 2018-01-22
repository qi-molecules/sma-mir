; *************************************************************************
; FUNCTION
;      wlm_fit
;
; WRITTEN
;      November 28, 1999 by JMC
;
; PURPOSE
;      Fits a smooth curve through WLM calibration variables
;
; INPUTS
;      time          : Time stamps (x axis in hours)
;      yorig         : Raw calibration data for each telescope
;      itel          : Vector containing telescope nummbers
;      id            : Vector containing telescope ID number of each element 
;                      in time and yorig.
;      dt_smooth     : Time interval [hours] to smooth over.
;                      Default: 0.0 -> just connect the points.
;                      If < 0, then take an average of all the data points.
;      plot          : If set, then plot the curve through the data points
;      boxcar        : If set, then use a boxcar smoothing function instead
;                      of hanning smoothing.
;
; OUTPUT
;     Gains stored in wlm_gains
;      1  -> successful
;     -1  -> not successful
;
; EXAMPLES
;       result = wlm_fit(ut_cal,y,ysmooth,wlm_itel,s.telcal,smooth,$
;                        ylabel,/plot,/boxcar)
; *************************************************************************

function wlm_fit,time,yorig,ysmooth,itel,id,dt_smooth,yvar,$
                 plot=plot,boxcar=boxcar
   common global
   common data_set

   ; Set smoothing time
     smooth = 0.0
     if keyword_set(dt_smooth) then smooth = dt_smooth

   ; Transform data
     npts = n_elements(yorig)
     ndim = 1 + (smooth ne 0.0)
     ndim = 1
     ydata = fltarr(ndim,npts)
     ydata[0,*] = yorig

   ; Allocate memory for model fit and residuals
     yfs = fltarr(ndim,npts)
     id_plot = lonarr(npts)

   ; Loop over each telescope and compute model fit and residuals
     for i = 0L, n_elements(itel)-1L do begin
        ; Set telescope number
          n = itel(i)

        ; Find indices to this telescope
          j = where(n eq id,nj)

        ; Compute model fit and residuals
          if (nj gt 0) then begin
            ; Smoothed funtion
              if (smooth lt 0.0 or nj eq 1) then $
                yfs[0,j] = total(ydata[0,j]) / nj $
              else if (smooth eq 0.0) then $
                yfs[0,j] = ydata[0,j] $
              else $
                yfs[0,j] = wlm_smooth(ydata[0,j],time[j],smooth,boxcar=boxcar)

            ; Residuals
              if (ndim gt 1) then begin
                 ydata[1,j] = ydata[0,j] - yfs[0,j]
                 yfs[1,j]   = 0.0
              endif
              id_plot[j] = n
          endif
     endfor

   ; Store smoothed data
     ysmooth = reform(yfs[0,*])

   ; Plot data
     blabel = strcompress(yvar + ' vs time' + $
               ' [Track ' + string(in(pil[0]).traid) + ']')
     a = findgen(16) * (!DPI*2/16.)
     usersym,cos(a),sin(a),/fill
     result = wlm_plot_data(time,ydata,id_plot,id_plot,$
                yfit=yfs,"Time (hours)",[yvar,"Res"],plot=plot,$
                psym=[8,10],m_options="cspne",blabel=blabel)
end
