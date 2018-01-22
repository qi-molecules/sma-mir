function uti_distinct ,inp_array,many_distinct=many_distinct, $
                   sample=sample,ndistinct
;
; Generates sorted list of all distinct values of the input array.
; many_distinct = 0 (most the same) , 1 (most distinct)
; sample = 0 (do entire input array) , >0 (sample w/ n_el/sample pts)
; eg. : distinct_array=uti_distinct(inp_array,many_distinct,ndistinct)
;
; first method : for cases with most values different
;
n_el=n_elements(inp_array)
if n_el lt 300L then sample=0L
if keyword_set(sample) then begin
  j=long((n_el-1)*randomu(s,n_el/sample))
  reduced_array=inp_array(j(uniq(inp_array(j))))
endif
if keyword_set(many_distinct) then begin
   if keyword_set(sample) then  begin $
       distinct_array=reduced_array(uniq(reduced_array,sort( reduced_array )))
   endif 
   if not keyword_set(sample) then  begin $
     distinct_array=inp_array(uniq(inp_array,sort( inp_array  )))
   endif
endif else begin 
;
; second method : faster for cases with most values the same
;
if not  keyword_set(sample) then reduced_array=inp_array(uniq(inp_array))
distinct_array=reduced_array(uniq(reduced_array,sort( reduced_array  ))) 
endelse

ndistinct=n_elements(distinct_array)
return, distinct_array
end



