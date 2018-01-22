function uti_distinct, inarray, ndistinct, $
                     elements=elements, many_repeat=many_repeat
;
; Returns unique members of the inarray.
;
; A new function that could replace uti_distinct.pro.
; K. Sakamoto, Jun. 10, 2002
;
; input    : inarray   -- input array (no need to sort it in advance)
; output   : ndistinct -- number of unique elements
; keywords : elements  -- elements to search for in inarray
;            many_repeat -- set this keyword if a small number of members
;                           appear in the inarray repeatedly.
; 
;
; Usage:
; e.g., unique_array=uti_unique(inarray, ndistinct)
;
; If the size of the inarray is large, the above usage takes a long time.
; There are two ways to speed it up if a small number of elements
; appear in the inarray many times.
;
; (1) If all the elements that may be found in inarray is known, then use
;    unique_array=uti_unique(inarray, elements=elements)
; where elements is the array that contains all possible elements
; of the inarray.
;
; (2) If many elements of the inarray are identical but are unknown, then
;    unique_array=unique(inarray, /many_repeat)
; fasten the search.
;
; In many cases, (1) is faster than (2).
;
; Difference between uti_distinct and uti_unique:
; Unlike uti_distinct.pro, this function does not use sampling of
; inarray to speed up the search. The sampling makes the output
; stochastic, and sometimes errorneous. The stochastic error can
; be very hard to find. So I (KS) strongly suggest to use
; this routine instead of uti_distinct. The `elements' keyword
; helps to speed up the otherwise time-consuming search in almost
; all situations encountered in MIR where uti_distinct(sample) is used.
;
; For example, one can find the names of the telescopes which are 
; used as telescope1 with
;   u_tel1 = c.tel1[uti_unique(bl.itel1,elements=indgen(n_elements(c.tel1)),$
;                   ntel1)]
; rather than
;   u_tel1 = c.tel1[uti_distinct(bl.itel1,sample=7,ntel1)]
; which is stochastic. 
; Another way,
;   u_tel1 = c.tel1[uti_unique(bl.itel1,/many_repeat,ntel1)],
; is slower than using the elements.
;

if n_elements(elements) gt 0 then begin
  candidates=uti_distinct(elements,ncan)
  exists=intarr(ncan)
  for i=0L, ncan-1L do begin
    index=where(inarray eq candidates[i],count)
    exists[i]=(count ge 1)
  endfor
  outarray=candidates[where(exists)]
endif else begin
  if keyword_set(many_repeat) then array=inarray[uniq(inarray)] $
                              else array=inarray

  outarray=array[uniq(array,sort(array))]
endelse

ndistinct=n_elements(outarray)
return, [outarray]

end
