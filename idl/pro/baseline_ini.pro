; *************************************************************************
; FUNCTION
;      baseline_ini.pro
;
; WRITTEN
;      July 19, 2000 by JMC
;
; PURPOSE
;      Set vector contain soids and soid comments for MIR
;
; INPUTS
;      none
;
; OUTPUT
;       soids : integer vector containg SOID numbers
;    comments : character vector containing soid comments
;       ibest : If non-zero, then indicates current soid is the best one
;
; EXAMPLES
;       result = baseline_ini(soids,comments,ibest,ncurrent)
;
; *************************************************************************
function baseline_ini,soids,comments,ibest,ncurrent
   ; Common blocks
     common global
     common data_set

   ; Initialize
     soids = [0]
     comments = ['bad soid']

   ; Find SOID number in track
     soid_current = uti_distinct(bl.soid,ncurrent,/many_repeat)
     soid_current = fix(soid_current[0])

   ; Find current configuration number
     conid = uti_distinct(in.conid,nconid,/many_repeat)
     conid = fix(conid)

   ; Set isql command
     command = 'select distinct altsoids=b.soid,altcomment=t.comments ' + $
               'from BLS b,TPO t ' + $
               'where b.con#=' + string(conid) + $
               ' and b.soid=t.soid order by b.soid desc'
     result = dbi_sql_submit(command)

   ; Must have at least 1 soid
     nsoids = n_elements(result) - 4
     if (nsoids eq 0) then return,-1

   ; Allocate memory for temporary and final storage
     soids         = intarr(nsoids)
     soids_temp    = intarr(nsoids)
     comments      = strarr(nsoids)
     comments_temp = strarr(nsoids)

   ; Read soid numbers and comments
     j = 0
     s = ' '
     for i = 0,nsoids-1 do begin
         reads,result(2+i),j,s
         soids_temp[i]    = j
         comments_temp[i] = strcompress(s)
     endfor

   ; Find location of current soid
     icurrent = where(soids_temp eq soid_current)
     if (ncurrent eq 0) then return,-1

   ; If icurrent is not zero, then there may be a better soid
     ibest = 1 * (icurrent[0] eq 0)

   ; Put current soid in entry 0
     soids[0]    = soids_temp[icurrent]
     comments[0] = comments_temp[icurrent]

   ; Now add the other soids
     j = where(soids_temp ne soid_current,nother_soids)
     if (nother_soids gt 0) then begin
        soids[1:nother_soids]    = soids_temp[j]
        comments[1:nother_soids] = comments_temp[j]
     endif

   ; Done
     return,1
end
