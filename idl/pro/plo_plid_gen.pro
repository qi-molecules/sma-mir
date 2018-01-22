; *************************************************************************
; FUNCTION
;      plo_plid_gen.pro
;
; WRITTEN
;              xxxx Kim
;      Sep 18, 2000 header doc added (syl)
;
; PURPOSE
;      This routine generates a plid - an id into the pl structure for use by
;      plo routines. It checks the pl structure and looks for the first index
;      not being used.  If everything is being used - it adds more structures 
;      to the array.
; INPUTS
;      none
;
; OUTPUT
;      return id number
; EXAMPLES
;       result = plo_plid_gen()
;
; *************************************************************************
;
function plo_plid_gen

common global
common plo

num_pls = n_elements(pl)
i = 0
index = -1

while ((index lt 0) and (i lt num_pls)) do begin
   if pl[i].active eq 0 then begin
      index = i
      pl[index].active = 1
   endif
   i = i + 1
endwhile

;If there's not enough room in the pl array - add a new element
;I'm doing it 1 at a time because this shouldn't happen very
;often and I don't want to dramatically increase the array
if index lt 0 then begin
   pl = [pl,pl_par]
   index = n_elements(pl) - 1
   pl[index].active = 1
endif

return,index

end
