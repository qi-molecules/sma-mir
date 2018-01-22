pro uti_xdelay, smoothing=smoothing, reference=reference, defaults=defaults, swarm=swarm, asic=asic, _extra=extra_keywords

common global
common data_set

if tag_exist(bl,'iaq') ne 0 then newformat=0 else newformat=1

if keyword_set(swarm) then select,/p,/re,/swarm
if keyword_set(asic) then select,/p,/re,/asic
if (not keyword_set(swarm)) and (not keyword_set(asic)) then select,/p,/re
result=dat_filter(s_s,/save)


if not keyword_set(smoothing) then begin
  if not keyword_set(poly) then begin
    smoothing = -1
    funct = 'poly'
    poly=1
  endif else begin
    smoothing = -1
    funct = 'poly'
  endelse
endif else begin
  funct = 'boxcar'
endelse


if not keyword_set(reference) then begin
   print, 'Please use REFERENCE keyword to select the source '
   print, 'Quit !'
   return
endif

if n_elements(reference) gt 1 then begin
   print, 'Only one reference source can be selected !'
   print, 'Quit !'
   return
endif

result=dat_filter(s_f,'"source" eq "'+reference+'"',/no_notify,/reset)
temp_souid=in[pif[0]].souid
;print,temp_souid

print, '***********************************************'
print, 'Fixing CROSS RX DELAY for rx400 - rx345:'
print, '...'

;result=dat_filter(s_f,'(("ipol" eq "0" and "iaq" eq "1" and "ibq" eq
;"0") or ("ipol" eq "1" and "iaq" eq "1" and "ibq" eq "1") or ("ipol"
;eq "2" and "iaq" eq "0" and "ibq" eq "0") or ("ipol" eq "3" and "iaq"
;eq "0" and "ibq" eq "1")) and "wt" gt "0"',/no_notify,/reset)
result=dat_filter(s_s,/restore)
if newformat eq 0 then result=dat_filter(s_f,'("iaq" eq "1" and "ibq" eq "0" and "wt" gt "0")',/no_notify) else result=dat_filter(s_f,'("ant1rx" eq "1" and "ant2rx" eq "0" and "wt" gt "0")',/no_notify)
if result le 0 then begin
   print,'No cross rx data between rx400 - rx345. '
   goto, switch_wvp 
endif
if not keyword_set(defaults) then begin
   result=pass_xdelay(temp_souid,'channel','pha',smoothing,'blcd,rec,sb,band', $
     4,1,funct=funct,npoly=poly,_extra=extra_keywords)
   aa = ''
   read,aa,prompt='Apply delay ? [NO <YES>]:  '
   if (aa eq 'YES' or aa eq 'yes' or aa eq 'Yes' or aa eq 'Y' or aa eq 'y') then begin
      print,'Applying delay'
      result=cal_apply2(passband='pha',x_var='channel')
   endif else begin
      print,'NO: Nothing done'
   endelse
endif else begin
   result=pass_xdelay(temp_souid,'channel','pha',smoothing,'blcd,rec,sb,band', $
     4,1,funct=funct,npoly=poly,/noplot,_extra=extra_keywords)
   result=cal_apply2(passband='pha',x_var='channel')
endelse

print, 'Done!'
print, ''

switch_wvp:
;goto, end_wvp
;print, '************************************************'
print, 'Fixing CROSS RX DELAY for rx345 - rx400:'
print, '...'

;result=dat_filter(s_f,'(("ipol" eq "0" and "iaq" eq "0" and "ibq" eq "1") or ("ipol" eq "1" and "iaq" eq "0" and "ibq" eq "0") or ("ipol" eq "2" and "iaq" eq "1" and "ibq" eq "1") or ("ipol" eq "3" and "iaq" eq "1" and "ibq" eq "0")) and "wt" gt "0"',/no_notify,/reset)
result=dat_filter(s_s,/restore)
if newformat eq 0 then result=dat_filter(s_f,'("iaq" eq "0" and "ibq" eq "1" and "wt" gt "0")',/no_notify) else result=dat_filter(s_f,'("ant1rx" eq "0" and "ant2rx" eq "1" and "wt" gt "0")',/no_notify)
if result le 0 then begin
   print,'No cross rx data between rx345 - rx400. '
   goto, end_wvp 
endif

if not keyword_set(defaults) then begin
   result=pass_xdelay(temp_souid,'channel','pha',smoothing,'blcd,rec,sb,band', $
     4,1,funct=funct,npoly=poly,_extra=extra_keywords)
   aa = ''
   read,aa,prompt='Apply delay ? [NO <YES>]:  '
   if (aa eq 'YES' or aa eq 'yes' or aa eq 'Yes' or aa eq 'Y' or aa eq 'y') then begin
      print,'Applying delay'
      result=cal_apply2(passband='pha',x_var='channel')
   endif else begin
      print,'NO: Nothing done'
   endelse
endif else begin
   result=pass_xdelay(temp_souid,'channel','pha',smoothing,'blcd,rec,sb,band', $
     4,1,funct=funct,npoly=poly,/noplot,_extra=extra_keywords)
   result=cal_apply2(passband='pha',x_var='channel')
endelse

end_wvp:
print, 'Done!'
print, '************************************************'

select,/p,/re

end
