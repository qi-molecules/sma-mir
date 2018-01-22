pro  uti_range_even,min_value,max_value,incr,min_rnd,max_rnd
;
; Calculates the range which is rounded larger the data values   
; 
;
; parameters : min_value  -- minimum data value 
;              max_value  -- maximum data value
;              incr       -- round to next lower/higher value of incr
;              min_rnd,max_rnd  -- rounded min and max
;
; eg. : uti_range_even,min_value,max_value,round,min_rnd,max_rnd  
;
common global
common data_set

incr=abs(incr)
if incr eq 0. then incr=1.
if min_value eq max_value then begin
  max_value=min_value+0.00001*incr
endif
  min_rnd=incr*floor((min_value)/incr)
  max_rnd=incr*ceil((max_value)/incr)
end

