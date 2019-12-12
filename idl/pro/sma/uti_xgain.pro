pro uti_xgain, x_var=x_var,cal_type=cal_type,smoothing=smoothing, $
  frames_per_page=frames_per_page, tel_bsl=tel_bsl, $
  loose=loose, poly=poly, connect=connect, preavg=preavg, $
  no_unwrap=no_unwrap, non_point=non_point, asic=asic, swarm=swarm, $
  defaults=defaults,_extra=extra_keywords

common global
common data_set

if tag_exist(bl,'iaq') ne 0 then newformat=0 else newformat=1

if keyword_set(swarm) then select,/p,/re,/swarm
if keyword_set(asic) then select,/p,/re,/asic
if (not keyword_set(swarm)) and (not keyword_set(asic)) then select,/p,/re
; obtain the corresponding c1 for xgain
uti_avgband
result=dat_filter(s_s,/save)

if (keyword_set(frames_per_page) eq 0) then frames_per_page = 1
if (keyword_set(x_var) eq 0) then x_var = 'int'
if (keyword_set(cal_type) eq 0) then cal_type = 'pha'
if (n_elements(smoothing) eq 0) and (n_elements(poly) eq 0) then begin
  if x_var eq 'int' then smoothing = 20.
  if x_var eq 'hours' then smoothing = 2.
  if x_var eq 'el' then poly=2
endif
plid = 1

tel_bsl='baseline'

if (not keyword_set(connect)) then connect = 0


result=gain_ini(30.,use,all_souids,all_sources,all_amps, $
   numbs_3mm,numbs_1mm,fluxes_3mm,fluxes_1mm,defaults=defaults)
if result eq 0 then begin
    print, 'Found no sources in dataset to be selected as gain calibrators'
    print, 'Quit !'
    return
endif

j = where(use eq 1,count)
if count eq 0 then begin
    print, 'Found no sources in dataset to be selected as gain calibrators'
    print, 'Quit !'
    return
endif

ncals = n_elements(j)
print,'number of gain cals ',ncals
print,'gain cal source(s): ',all_sources[j]
print,'gain cal 3mm fluxes:',fluxes_3mm[j]
print,'gain cal 1mm fluxes:',fluxes_1mm[j]


gai_souids = make_array(ncals,/float)
gai_fluxes_3mm = make_array(ncals,/float)
gai_fluxes_1mm = make_array(ncals,/float)

for i = 0, ncals-1 do begin
  gai_souids[i] = all_souids[j[i]]
  gai_fluxes_3mm[i] = fluxes_3mm[j[i]]
  gai_fluxes_1mm[i] = fluxes_1mm[j[i]]
  if gai_fluxes_3mm[i] eq 0 or gai_fluxes_1mm[i] eq 0 then begin
      print, 'Calibrator fluxes should be nonezero !'
      return
  endif
endfor

souid_str="("
k=0
for i=0,n_elements(gai_souids)-1 do begin
     k=k+1
     if k ne 1 then souid_str=souid_str+' or '
     souid_str=souid_str+'"souid" eq "'+strtrim(string(gai_souids[i]),2)+'"'
endfor
souid_str=souid_str+")"

print, '***********************************************'
print, 'gain solutions for rx345-rx400:'
print, '...'
result=dat_filter(s_s,/restore)
if newformat eq 0 then result=dat_filter(s_f,'"iaq" eq "0" and "ibq" eq "1" and "wt" gt "0"',/no_notify) else result=dat_filter(s_f,'"ant1rx" eq "0" and "ant2rx" eq "1" and "wt" gt "0"',/no_notify)
if result le 0 then begin
   print,'No cross rx data between rx345 - rx400. '
   goto, switch_wvp 
endif
if (keyword_set(connect)) then $
  result=gain_xpol(gai_souids,gai_fluxes_3mm,gai_fluxes_1mm, $
  tel_bsl,x_var,cal_type,frames_per_page, $
  dt_smooth=smoothing,plid,refant=refant,loose=loose, $
  connect=connect, preavg=preavg, no_unwrap=no_unwrap, $
  non_point=non_point, _extra=extra_keywords) $
else $
  if (keyword_set(smoothing) gt 0) then $
  result=gain_xpol(gai_souids,gai_fluxes_3mm,gai_fluxes_1mm, $
  tel_bsl,x_var,cal_type,frames_per_page, $
  dt_smooth=smoothing,plid,refant=refant,loose=loose, $
  preavg=preavg, no_unwrap=no_unwrap, non_point=non_point, $
  _extra=extra_keywords) $
else $
  result=gain_xpol(gai_souids,gai_fluxes_3mm,gai_fluxes_1mm, $
  tel_bsl,x_var,cal_type,frames_per_page, $
  dt_smooth=-poly,plid, refant=refant,loose=loose, $
  preavg=preavg, no_unwrap=no_unwrap, non_point=non_point, $
  _extra=extra_keywords)

if not keyword_set(defaults) then begin
   aa = ''
   read,aa,prompt='Apply gain solution? [NO <YES>]:  '
   if (aa eq 'YES' or aa eq 'yes' or aa eq 'Yes' or aa eq 'Y' or aa eq 'y') then begin
      print,'YES: apply gains'
      print,'Applying Gains'
      result=cal_apply(gain=cal_type,x_var=x_var,tel_bsl=tel_bsl,refant=refant)
   endif else begin
      print,'NO: nothing done'
   endelse   
endif else begin
   print,'Applying Gains'
   result=cal_apply(gain=cal_type,x_var=x_var,tel_bsl=tel_bsl,refant=refant)   
endelse
print, ''

switch_wvp:
print, '***********************************************'
print, 'gain solutions for rx400-rx345:'
print, '...'

result=dat_filter(s_s,/restore)
if newformat eq 0 then result=dat_filter(s_f,'"iaq" eq "1" and "ibq" eq "0" and "wt" gt "0"',/no_notify) else result=dat_filter(s_f,'"ant1rx" eq "1" and "ant2rx" eq "0" and "wt" gt "0"',/no_notify)

if (keyword_set(connect)) then $
  result=gain_xpol(gai_souids,gai_fluxes_3mm,gai_fluxes_1mm, $
  tel_bsl,x_var,cal_type,frames_per_page, $
  dt_smooth=smoothing,plid,refant=refant,loose=loose, $
  connect=connect, preavg=preavg, no_unwrap=no_unwrap, $
  non_point=non_point, _extra=extra_keywords) $
else $
  if (keyword_set(smoothing) gt 0) then $
  result=gain_xpol(gai_souids,gai_fluxes_3mm,gai_fluxes_1mm, $
  tel_bsl,x_var,cal_type,frames_per_page, $
  dt_smooth=smoothing,plid,refant=refant,loose=loose, $
  preavg=preavg, no_unwrap=no_unwrap, non_point=non_point, $
  _extra=extra_keywords) $
else $
  result=gain_xpol(gai_souids,gai_fluxes_3mm,gai_fluxes_1mm, $
  tel_bsl,x_var,cal_type,frames_per_page, $
  dt_smooth=-poly,plid, refant=refant,loose=loose, $
  preavg=preavg, no_unwrap=no_unwrap, non_point=non_point, $
  _extra=extra_keywords)


if not keyword_set(defaults) then begin
   aa = ''
   read,aa,prompt='Apply gain solution? [NO <YES>]:  '
   if (aa eq 'YES' or aa eq 'yes' or aa eq 'Yes' or aa eq 'Y' or aa eq 'y') then begin
      print,'YES: apply gains'
      print,'Applying Gains'
      result=cal_apply(gain=cal_type,x_var=x_var,tel_bsl=tel_bsl,refant=refant)
   endif else begin
      print,'NO: nothing done'
   endelse   

endif else begin
   print,'Applying Gains'
   result=cal_apply(gain=cal_type,x_var=x_var,tel_bsl=tel_bsl,refant=refant)   
endelse

print, 'Done!'
print, '************************************************'

select,/p,/re

end

