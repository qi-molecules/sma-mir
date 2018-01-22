; *************************************************************************
; FUNCTION
;      wlm_gains_ini
;
; WRITTEN
;      June 13, 2000 by JMC
;
; PURPOSE
;      Set gains/egains for ION
;
; INPUTS
;      gains   : array containing gains
;      egains  : array containing errors in gains
;      Each array has dimensions NTEL. If the WLM box is not present,
;      then the gain error is set to -1.0.
;
; OUTPUT
;      1 if successful
;     -1 if not successful
;
; EXAMPLES
;       result = wlm_gains_ini(gains,egains)
;
; *************************************************************************
function wlm_gains_ini,gains,egains
   ; Common blocks
     common wlm

   ; Initialize arrays
     gains  = fltarr(!NTEL)
     egains = make_array(!NTEL,/float,val=-1.0)

   ; Compute gains
     result = wlm_calibrate()

   ; Loop over WLM boxes
     gains[wlm_itel-1L]  = wlm_gains
     egains[wlm_itel-1L] = wlm_egains

   ; Done
     return,1
end
