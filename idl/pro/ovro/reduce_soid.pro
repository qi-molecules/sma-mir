function reduce_soid,defaults=defaults,unit=unit
   ; Common blocks
     common global
     common data_set

   ; Reset filter
     if (dat_list(s_l,/reset,/no) le 0) then return,-1

   ; Get other soids for this configuration
     result = baseline_ini(soids,comments,ibest,ncurrent)
     if n_elements(soids) eq 1 then begin
        printlog,"",unit=unit
        printlog,format='(%"WARNING: Only one soid (%d) is available for this track so far")',soids[0],unit=unit
        printlog,"",unit=unit
        return,1
     endif

   ; Get current soids
     soi = uti_distinct(bl(pbl).soid,nsoid_applied)

   ; Print current soids.
     printlog,"Current soids applied to track",unit=unit
     printlog,"SOID   Comments",unit=unit
     printlog,"----   --------",unit=unit
     for i=0,nsoid_applied-1 do begin
        j = where(soi[i] eq soids)
        printlog,format='(%"%4d  %s")',soids[j],comments[j],unit=unit
     endfor
     if nsoid_applied gt 1 then printlog,"WARNING: Multiply SOIDS applied to data",unit=unit

   ; Print alternate soids.
     printlog,"",unit=unit
     j = (nsoid_applied eq 1) ? where(soi ne soids,nj) : lindgen(n_elements(soids))
     printlog,"Alternate soids available",unit=unit
     printlog,"SOID   Comments",unit=unit
     printlog,"----   --------",unit=unit
     for i=0,nj-1 do $
        printlog,format='(%"%4d  %s")',soids[j[i]],comments[j[i]],unit=unit
     printlog,"",unit=unit

   ; Get new soid
     soid_default = max([soids])
     soid_new = 0L
     if keyword_set(defaults) then begin
        soid_new = soid_default
     endif else begin
        s = string(format='(%"Enter new soid number if needed (default: %d) ")',soid_default)
        irepeat = 1
        while (irepeat) do begin
           ; Get new soid from use
             print,format='(%"%s",$)',s
             input = ''
             read,input
             input = strtrim(input,2)

           ; Initialize
             ierr = 0

           ; Get new soid
             if (input eq '') then begin
                soid_new = soid_default
                irepeat = 0
             endif else if valid_num(input) eq 0 then begin
                ierr = 1
             endif else begin
                reads,input,soid_new
                if (1.0*fix(soid_new) ne soid_new) then $
                   ierr = 1 $
                else begin
                   j = where(soid_new eq soids,nj)
                   ierr = (nj eq 0)
                   irepeat = ierr
                endelse
             endelse

           ; Print error message and repeat if necessar
             if (ierr) then begin
                print," "
                print,"     *** Error entering new soid number ***"
                print," "
             endif
        endwhile
     endelse

   ; We have the new soid number. Now apply it to the data. If the "new"
   ; soid equals the current soid and only one soid has been applied to
   ; the track, then just return now
     if (n_elements(soi) eq 1 and soid_new eq soi) then begin
        printlog,format='(%"Using current soid %d")',soi,unit=unit
     endif else if cal_bas(soid_new) eq -1 then begin
        printlog,format='(%"Error applying soid %d")',soid_new,unit=unit
        return,-1
     endif else begin
        printlog,format='(%"Applied soid %d")',soid_new,unit=unit
     endelse
     printlog,"",unit=unit

   ; Done
     return,1
end
