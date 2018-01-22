; *************************************************************************
; FUNCTION
;      wlm_mask
;
; WRITTEN
;      November 28, 1999 by JMC
;
; PURPOSE
;      Masks out WLM data taken while telescopes were slewing.
;
; INPUTS
;      dt_wlm : Integration time in hours for WLM data.
;
; OUTPUT
;      -1   Failed.
;       1   Successful
;
; EXAMPLES
;       result = wlm_mask(2.0)
;
; *************************************************************************

function wlm_mask,dt_wlm
   ; Common blocks
     common global
     common data_set
     common wlm
     common plo

   ; Define astronomy stop times based on integration time stamps.
     tastro_start = in.dhrs
     tastro_stop  = tastro_start + (in.rinteg / 3600.)

   ; Set start/stop times for each WLM record (WLM integration time = dt_wlm)
     twlm_start = wlm_times - (dt_wlm / 2.0)
     twlm_stop  = twlm_start + dt_wlm

   ; Set astronomy "weights" to negative numbers
     jbad = where(wlm_wts le 0, nbad)
     wlm_wts = -abs(wlm_wts)

   ; Loop over each antenna. Find which WLM time stamps overlap with the 
   ; astronomy integrations and set their weights to a positive value.
     for i = 0L, wlm_ntel-1L do begin
        ; Find data for this antenna
          j = where(wlm_id eq wlm_itel[i],nj)
          if (nj gt 0) then begin
             ; Loop over integrations
               for k = 0L, n_elements(in)-1L do begin
                  ; Find WLM time stamps within this integration
                    l = where(twlm_start le tastro_stop[k] and $
                              twlm_stop  ge tastro_start[k],nl)

                  ; Mark these integrations as good
                    if (nl gt 0) then wlm_wts(j(l)) = abs(wlm_wts(j(l)))
               endfor
          endif
     endfor

   ; Mark initial bad data as bad again
     if (nbad gt 0) then wlm_wts(jbad) = -abs(wlm_wts(jbad))

   ; Done
     return,1
end
