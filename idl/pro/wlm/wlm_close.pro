; *************************************************************************
; PROCEDURE
;      wlm_close
;
; WRITTEN
;      November 28, 1999 by JMC
;
; PURPOSE
;      Closes postscript file and returns to X graphics window
;
; INPUTS
;      None
;
; OUTPUT
;      None
;
; EXAMPLES
;      wlm_close
;
; *************************************************************************
pro wlm_close

if (!D.name eq 'PS') then device,/close
set_plot,'X'

end
