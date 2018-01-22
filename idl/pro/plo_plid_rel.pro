; *************************************************************************
; FUNCTION
;      plo_plid_rel.pro
;
; WRITTEN
;      Aug 10, 2000 by syl
;
; PURPOSE
;      After the plot goes away - this sets the plid structure
;      used to non active
; INPUTS
;      plid -- pl id for de-activating
;
; OUTPUT
;      none
;
; EXAMPLES
;      plo_plid_rel,plid
;
; *************************************************************************
;
pro plo_plid_rel,plid

common global
common plo

pl[plid].active = 0

return
end
