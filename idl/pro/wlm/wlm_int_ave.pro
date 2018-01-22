; *************************************************************************
; FUNCTION
;      wlm_int_ave
;
; WRITTEN
;      November 4, 1998 by JMC
;
; PURPOSE
;      Determine the integration average value of up to two water line boxes
;
; INPUTS
;
; OUTPUT
;      -1   Failed.
;       1   Successful
;
; CALLED BY
;       wlm_box_gain
;
; EXAMPLES
;       result = wlm_int_ave(itel1,tave1,itel2,tave2)
;       result = wlm_int_ave(itel1,tave1,/single)
;
; *************************************************************************
function wlm_int_ave,itel1,data1,itel2,data2,avetime,n,single=single
  ; Common blocks
    common wlm
    common global
    common data_set

  ; Boolean
    iboth = not keyword_set(single)

  ; Allocate memory for integration averages
    nscans  = n_elements(in.dhrs)
    data1   = make_array(nscans,/double,value=!BAD_VALUE)
    if (iboth) then begin
      data2   = make_array(nscans,/double,value=!BAD_VALUE)
      avetime = make_array(nscans,/double,value=!BAD_VALUE)
    endif

  ; Define astronomy stop times for this integration based on integration 
  ; time stamps. Start times are given by in.dhrs
    astro_start = in.dhrs
    astro_stop  = astro_start + (in.rinteg / 3600.)

  ; Compute integration averages for each scan. Make sure that data exists 
  ; on both telescopes for each appropriate time sample
    for i = 0L,nscans-1L do begin
       ; Find WLM times within this integration for telescope.
         itimes = where(wlm_times ge astro_start[i] and $
                        wlm_times le astro_stop[i], ntimes)
         if (ntimes gt 0) then begin
            ; Compute
            ;    (1) integration-averaged WLM curve for the two WLM boxes
            ;    (2) the average reference UT time in each WLM box per integration;
              i1 = where(wlm_id eq itel1)
              if (iboth) then begin
                 i2    = where(wlm_id eq itel2)
                 igood = where(wlm_wts(i1(itimes)) gt 0 and $
                               wlm_wts(i2(itimes)) gt 0,ngood)
              endif else begin
                 igood = where(wlm_wts(i1(itimes)) gt 0,ngood)
              endelse

            ; Compute data and time averages
              if (ngood gt 0L) then begin
                 data1[i]   = total(wlm_raw[i1(itimes(igood))]) / ngood
                 if (iboth) then begin
                   data2[i]   = total(wlm_raw[i2(itimes(igood))]) / ngood
                   avetime[i] = total(wlm_times[itimes(igood)])   / ngood
                 endif
              endif
         endif
    endfor

  ; Finished
    return,1
end
