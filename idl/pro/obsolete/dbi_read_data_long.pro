function dbi_read_data_long, int_read=int_read, skip=skip

common global
common data_set
common wlm

if (keyword_set(skip) or keyword_set(int_read)) then result=dbi_headint_read() $
        else result=dbi_head_read()
in_skip=(keyword_set(skip)) ? skip:0
if (keyword_set(int_read)) then begin
    in_skip=int_read[0]
    in_rows=int_read[1]+1
endif else begin
    in_rows=n_elements(in)
endelse
bl_skip=in_skip*2*n_elements(c.blcd)
sp_skip=bl_skip*n_elements(c.band)

bl_rows=in_rows*2*n_elements(c.blcd)
sp_rows=bl_rows*n_elements(c.band)

result=dbi_headint_read(in_rows=in_rows, bl_rows=bl_rows,sp_rows=sp_rows,$
                        in_skip=in_skip, bl_skip=bl_skip,sp_skip=sp_skip)

result=dbi_chanint_read(in_skip=in_skip,in_rows=in_rows)

return,1

end
