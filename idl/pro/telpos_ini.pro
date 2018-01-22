; *************************************************************************
; FUNCTION
;      telpos_ini.pro
;
; WRITTEN
;      July 19, 2000 by JMC
;
; PURPOSE
;      Given an input SOID, this returns the telescope positions
;
; INPUTS
;      soid : input soid number
;
; OUTPUT
;    positions : matrix containing the telescope positions
;
; EXAMPLES
;       result = telpos_ini(743,positions)
;
; *************************************************************************
function telpos_ini,soid,positions
   ; Common blocks
     common global
     common data_set

   ; Retrieve soid information
     result = cal_bas_soid(soid,soid_info)

   ; Initialize array
     positions = fltarr(!NTEL,4)

   ; Store the results in a matrix
     positions[0:!NTEL-1,0] = soid_info.dte
     positions[0:!NTEL-1,1] = soid_info.dtn
     positions[0:!NTEL-1,2] = soid_info.dtu
     positions[0:!NTEL-1,3] = soid_info.dta

   ; Transpose matrix
     positions = transpose(positions)
     positions = reform(positions,6,4)

   ; Done
     return,1
end
