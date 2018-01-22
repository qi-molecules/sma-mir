; *************************************************************************
; FUNCTION
;      wlm_scale_ini
;
; WRITTEN
;      June 13, 2000 by JMC
;
; PURPOSE
;      Send scale factor to ION
;
; INPUTS
;      scale   : scale factor in mm/K
;
; OUTPUT
;      1 if successful
;     -1 if not successful
;
; EXAMPLES
;       result = wlm_scale_ini(scale)
;
; *************************************************************************
function wlm_scale_ini,scale
   ; Common blocks
     common wlm

   ; Initialize arrays
     scale = wlm_xfactor

   ; Done
     return,1
end
