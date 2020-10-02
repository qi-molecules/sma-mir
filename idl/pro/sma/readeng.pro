pro readeng, dir=dir, eng=eng

common global
common data_set



if not keyword_set(dir) then dir='.'
print,'******** OS TYPE ***********'
print,'Platform =',!VERSION.ARCH
;
;  set up structures for header table
;
engh_temp={ant:0L,pad:0L,antstat:0L,trkstat:0L,comstat:0L,inhid:0L,$
           int:0L,dhrs:0d,ha:0d,lst:0d,pmdaz:0d,pmdel:0d,tiltx:0d,tilty:0d,$
           actaz:0d,actel:0d,azoff:0d,eloff:0d,aztrkerr:0d,eltrkerr:0d,$
           N:0d,chopx:0d,chopy:0d,chopz:0d,chopang:0d,tsysrxa:0d,tsysrxb:0d,$
           ambtemp:0d}

openr,unit,dir+'/eng_read',/get_lun,error=err
if err ne 0 then begin
  print,!err_string
  return
endif

status = fstat(unit) & engrows = status.size/196
if e.debug then print,engrows,' engh rows'

eng = replicate(engh_temp,engrows)
point_lun,unit,0L
readu,unit,eng
;if (strpos(!VERSION.ARCH,'86') ge 0) then $
;  we = swap_endian(we)
;if endianFlag eq 1 then we = swap_endian(we)
close,unit & free_lun,unit

print,'finished reading ENG structure'

return
end

