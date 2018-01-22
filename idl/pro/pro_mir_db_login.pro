pro pro_mir_db_login
;
; Logs into the db and sets the e.dbprocess variable
;

common global

;
; Make a connection to the database - preserve the connection for later use
;
e.dbprocess = 1
if (e.dbprocess EQ 0) then begin
    print, "DB LOGIN FAILED"
    return
endif
if e.debug then print,'   DBPROCESS ',e.dbprocess

end
