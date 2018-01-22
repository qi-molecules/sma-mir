; e.g. result = uti_filename()
;      result = uti_filename('jmc',track=200,suffix='.dat')
;
function uti_filename,root,track_number=track_number,suffix=suffix

   ; Construct string
     if (not keyword_set(root)) then begin
        output_name = 'tmp'
     endif else begin
        output_name = root
     endelse
     output_name = output_name + "_"

   ; Add current date to generate file name
     result = systime(0)
     sdate  = strmid(result,20,4) + "_" + strmid(result,4,3) + "_" + $
              strmid(result,8,2)  + "_" + strmid(result,11,8)
     output_name = output_name + sdate

   ; Add track number, if set
     if (keyword_set(track_number)) then begin
        output_name = output_name + "_" + string(track_number)
     endif

   ; Add suffix
     if (keyword_set(suffix)) then output_name = output_name + suffix

   ; Remove all spaces
     output_name = strcompress(output_name,/remove)

   ; Done
     return,output_name
end
