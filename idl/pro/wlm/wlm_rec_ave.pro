; *************************************************************************
; FUNCTION
;      wlm_rec_ave
;
; WRITTEN 
;      November 4, 1998 by JMC
;
; PURPOSE
;      Determine the average WLM value within a given time range.
;      (Usually a astronomy record time interval.)
;
; INPUTS 
;      t1            : Vector contain the WLM start times for a given antenna 
;      t2            : Vector contain the WLM stop  times for a given antenna 
;      wc            : Vector contain the WLM values for a given antenna
;      tstart        : Astronomy start time
;      tstop         : Astronomy start time
;
; NOTES
;      (1) sum_aw and sum_w should be initialized to zero before calling
;          this routine. See wlm_derive() and wlm_apply().
;      (2) Time stamps should have the same units, normally in hours.
;      (3) This program weights each WLM point by the integration time.
;          Fractional overlap with tstart and tstop is taken into account.
;    
; OUTPUT
;      If WLM is present in the appropriate time range, then the average
;      WLM value is returned. If not, then !BAD_VALUE is returned.
;
; CALLED BY
;      wlm_apply
;      wlm_derive
;
; EXAMPLES
;      for k = nrec_start, nrec_end do begin
;         w1 = wlm_rec_ave(wts1,wte1,wc1,tstart,tstop,sum_aw,sum_w,0)
;         w2 = wlm_rec_ave(wts2,wte2,wc2,tstart,tstop,sum_aw,sum_w,1)
;      endfor
; *************************************************************************

function wlm_rec_ave,wlm_tstart,wlm_tstop,wlmdata,$
                     astro_tstart,astro_tstop
   ; Initialize
     x = !BAD_VALUE

   ; Find range of WLM indices in astronomy record
     j = where(wlm_tstart lt astro_tstop and wlm_tstop gt astro_tstart,nj)
     if nj eq 0 then return,x

   ; Compute average
     wtstart = (wlm_tstart(j) > astro_tstart)
     wtstop  = (wlm_tstop(j)  < astro_tstop)
     dt_wlm  = wtstop - wtstart
     tot_dt_wlm = TOTAL(dt_wlm)
     if tot_dt_wlm gt 0.05*(astro_tstop-astro_tstart) then $
        x = TOTAL(wlmdata(j) * dt_wlm) / tot_dt_wlm

   ; Done
     return,x
end
