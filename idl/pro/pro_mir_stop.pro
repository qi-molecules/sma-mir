pro pro_mir_stop,saveenv
;
; Cleanly shuts down MIR including logging out of the db
;

common global

if(saveenv eq 1) then begin
   result=pro_mir_save()
   if(result EQ -1) then begin
       print, "Unable to save dataset"	;ERROR CONDITION - what happens if we can't save
       return
   endif
endif
end
