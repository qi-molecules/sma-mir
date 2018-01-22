; *************************************************************************
; FUNCTION
;      wlm_settime
;
; WRITTEN
;      November 28, 1999 by JMC
;
; PURPOSE
;      Sets string indicating how long in time it was between two numbers
;
; INPUTS
;      time  : number from systime(1) indicating the starting time.
;
; OUTPUT
;      s     : String printing the time in seconds or minutes since "time"
;
; EXAMPLES
;       time = systime(1)
;       s = wlm_settime(time)
;
; *************************************************************************
function wlm_settime,time
     time = systime(1) - time
     if (time lt 60.0) then $
        s = string(format='(f4.1)',time) + " seconds" $
     else $
        s = string(format='(f4.1)',time/60) + " minutes"
     return,s
end
