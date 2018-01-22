; *************************************************************************
; FUNCTION
;      wlm_scale_set
;
; WRITTEN
;      June 13, 2000 by JMC
;
; PURPOSE
;      Record scale factor entered by user
;
; INPUTS
;      scale   : scale factor in mm/K
;
; OUTPUT
;      1 if successful
;     -1 if not successful
;
; EXAMPLES
;       result = wlm_scale_set(scale)
;
; *************************************************************************
function wlm_scale_set,scale,noplot=noplot,mkplot=mklog
   ; Common blocks
     common wlm

   ; Initialize arrays
     wlm_xfactor = scale
     wlm_filter = '0'

   ; Done
     return,1
end
