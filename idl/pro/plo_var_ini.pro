; *************************************************************************
; FUNCTION
;      plo_var_ini
;
; WRITTEN
;      January 26, 2001 by JMC
;
; PURPOSE
;      Returns list of possible plot variable names to MIR
;
; INPUTS
;      none
;
; OUTPUT
;      returns variable names in varnames
;      1 if successful
;     -1 if not successful
;
; EXAMPLES
;       result = plo_var_ini(var_num,var_str)
;
; *************************************************************************
function plo_var_ini,var_num,var_str
   ; Common blocks
     common global
     common data_set
     common plo

   ; Get structure contain variables names
     vars = dat_var()

   ; Put out the names only
     varnames = vars.tags
     types = vars.types
     js=where(types eq 'string')
     jn=where(types ne 'string')
     var_num=varnames[jn]
     var_str=varnames[js]   

   ; Done
     return,1
end
