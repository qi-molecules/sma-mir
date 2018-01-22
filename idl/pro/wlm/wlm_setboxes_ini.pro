; *************************************************************************
; FUNCTION
;      wlm_setboxes_ini
;
; WRITTEN
;      June 13, 2000 by JMC
;
; PURPOSE
;      Set which boxes will be used for WLM processing
;
; INPUTS
;      use   : array obtained from Java interface
;
; OUTPUT
;      n     : number of WLM boxes used
;
; EXAMPLES
;       result = wlm_setboxes_ini(use)
;
; *************************************************************************
function wlm_setboxes_ini,use
   ; Common blocks
     common wlm

   ; WLM_ITEL contains ID numbers (1 -> NTEL) off valid WLM boxes
     wlm_itel = use
     wlm_ntel = n_elements(wlm_itel)

   ; Done
     return,wlm_ntel
end
