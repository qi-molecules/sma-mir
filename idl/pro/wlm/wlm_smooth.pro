; *************************************************************************
; FUNCTION
;      wlm_smooth
;
; WRITTEN 
;      June 30, 1998 by JMC
;
; PURPOSE
;      Smooths data to remove DC drifts
;
; INPUTS 
;      data   : vector containing data to be smoothed
;      times  : Times (in hours) that the data were sampled
;      hwidth : Full width of the smoothing kernel (minutes)
;      boxcar : If set, use boxcar smoothing. Otherse, using Hanning
;
; OUTPUT
;      A vector containing the smoothed data
;
; CALLED BY
;      wlm_box_gain
;      wlm_fit
;
; EXAMPLES
;      result = wlm_smooth(data,times,60.0)
;
; *************************************************************************
function wlm_smooth,data,times,hwidth,boxcar=boxcar
   ; Create copy of input vector
     cdata = data

   ; Must be at least two data points
     if (n_elements(data) lt 2) then begin
        printf,-1,'Not enough data points for smoothing'
        stop
     endif

   ; Set smoothing half width in hours
     dh = abs(hwidth)/2.0/60.0

   ; Loop over entire vector and compute smoothed function
     for j = 0L, n_elements(cdata)-1L do begin
         ; Compute time offsets (in minutes) from this data point
           toff = times - times(j)

         ; Find where time stamps fall within smoothing window
           jstart = where(toff ge -dh)
           jstart = jstart(0)
           jend   = where(toff le dh,countj)
           jend   = jend(countj-1L)

         ; Compute smoothing kernel
           if keyword_set(boxcar) then $
             kernel = make_array(jend-jstart+1L,/double,value=1.0) $
           else begin
             if (toff(jend) eq toff(jstart)) then $
                dt = dblarr(jend-jstart+1L) $
             else $
                dt = !TWOPI * toff(jstart:jend) / (2.0 * dh)
             kernel = 0.5 + 0.5*cos(dt)
           endelse
           cdata[j] = TOTAL(data[jstart:jend] * kernel) / TOTAL(kernel)
     endfor
   ; Return smoothed data
     return,cdata
end
