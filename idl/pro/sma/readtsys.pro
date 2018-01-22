pro readtsys, dir=dir, tsys=tsys

common global
common data_set



if not keyword_set(dir) then dir='.'
print,'******** OS TYPE ***********'
print,'Platform =',!VERSION.ARCH
;
;  set up structures for header table
;
tsys_temp={inhid:0L,dhrs:0d,ipol:0,itel:0,tssb:fltarr(48,2)}
;
openr,unit,dir+'/tsys_read',/get_lun,error=err
if err ne 0 then begin
  print,!err_string
  return
endif

status = fstat(unit) & tsysrows = status.size/400
if e.debug then print,tsysrows,' tsys rows'

tsys = replicate(tsys_temp,tsysrows)
point_lun,unit,0L
readu,unit,tsys
;if (strpos(!VERSION.ARCH,'86') ge 0) then $
;  tsys = swap_endian(tsys)
if tsys[0].itel gt 20 then tsys = swap_endian(tsys)
close,unit & free_lun,unit

print,'finished reading TSYS structure'

return
end

