; *************************************************************************
; FUNCTION
;      wlm_residuals_set
;
; WRITTEN 
;      June 14, 2000 by JMC
;
; PURPOSE
;      Compute residuals and calibrate WLM data
;
; INPUTS 
;      xfactor : Adopted scale factor
;
; OUTPUT
;      -1   Calibration failed.
;       1   Calibrated WLM data stored successfully
;
; EXAMPLES
;      result = wlm_residuals_set(xfactor,do_phase_corrections)
; *************************************************************************
function wlm_residuals_set,xfactor,do_phase_corrections
  ; Common blocks
    common wlm

  ; Set parameters
    wlm_xfactor = xfactor
    wlm_icoherence = 1 - keyword_set(do_phase_corrections)

  ; Done
    return,1
end
