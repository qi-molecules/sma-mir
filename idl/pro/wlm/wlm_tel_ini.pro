; *************************************************************************
; FUNCTION
;      wlm_tel_ini
;
; WRITTEN
;      June 15, 2000 by JMC
;
; PURPOSE
;      Set which boxes are available
;
; INPUTS
;      use   : integer array set to Java interface
;              !NTEL elements long. 1-> WLM box online, 0->not
;
; OUTPUT
;      n     : number of WLM boxes used
;
; EXAMPLES
;       result = wlm_tel_ini(use)
;
; *************************************************************************
function wlm_tel_ini,use
   ; Common blocks
     common wlm

   ; WLM_ITEL contains ID numbers (1 -> NTEL) have valid WLM boxes
     use = intarr(!NTEL)
     use[wlm_itel-1L] = 1

   ; Done
     return,wlm_ntel
end
