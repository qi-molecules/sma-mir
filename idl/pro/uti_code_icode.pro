pro uti_code_icode, input_strs,output_strs,ioutputs
;
; Converts a string array to unique set of integer codes.
; eg. : uti_code_icode, input_strs,output_strs,ioutputs
;
output_strs=uti_distinct(input_strs,n_outputs,/many_repeat)
ioutputs=make_array(n_elements(input_strs),/int)

for i=0L,(n_outputs-1L) do begin 
j = where(input_strs eq output_strs(i),count)
ioutputs(j) = i 
endfor
output_strs=strtrim(output_strs,2L)
;
;  to get a list of the rows in the original array with
;  char code eq '????', 
;  print,where(ioutputs eq max(where(output_strs eq '????')))

end

