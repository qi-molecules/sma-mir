pro readwe, dir=dir, we=we

common global
common data_set



if not keyword_set(dir) then dir='.'
print,'******** OS TYPE ***********'
print,'Platform =',!VERSION.ARCH
;
;  set up structures for header table
;
weh_temp={inhid:0L,flags:lonarr(11),refrac:fltarr(11),$
          tamb:fltarr(11), pressure:fltarr(11),humid:fltarr(11),$
  windspd:fltarr(11),winddir:fltarr(11),water:fltarr(11)}
;

inh_temp={traid:0L,inhid:0L,$
         int:0L,az:0e,el:0e,ha:0e,$
         iut:0,$
         iref_time:0,$
         dhrs:0d,vc:0e,sx:0d,sy:0d,sz:0d,rinteg:0e,proid:0L,$
         souid:0L,isource:0,ivrad:0,offx:0e,offy:0e,$
         ira:0,$
         idec:0,rar:0d,decr:0d,epoch:0e,size:0e,$
         inhint1:0L,inhint2:0L,inhint3:0L,$
         inhint4:0L,inhint5:0L,inhint6:0L,sflux:0d,$
         inhdbl2:0d,inhdbl3:0d,inhdbl4:0d,inhdbl5:0d,inhdbl6:0d}
endianFlag=0
openr,unit,dir+'/in_read',/get_lun,error=err
point_lun,unit,0L
readu,unit,inh_temp
if inh_temp.epoch ne 2000 then endianFlag=1
close,unit & free_lun,unit

openr,unit,dir+'/we_read',/get_lun,error=err
if err ne 0 then begin
  print,!err_string
  return
endif

status = fstat(unit) & werows = status.size/356
if e.debug then print,werows,' weh rows'

we = replicate(weh_temp,werows)
point_lun,unit,0L
readu,unit,we
;if (strpos(!VERSION.ARCH,'86') ge 0) then $
;  we = swap_endian(we)
if endianFlag eq 1 then we = swap_endian(we)
close,unit & free_lun,unit

print,'finished reading WE structure'

return
end

