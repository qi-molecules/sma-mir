pro gain_cal_pol,x_var=x_var,cal_type=cal_type,smoothing=smoothing, $
  frames_per_page=frames_per_page, tel_bsl=tel_bsl, refant=refant, $
  loose=loose, poly=poly, connect=connect, preavg=preavg, $
  no_unwrap=no_unwrap, non_point=non_point, $
  defaults=defaults,_extra=extra_keywords

common global
common data_set
common plo

if tag_exist(bl,'iaq') ne 0 then newformat=0 else newformat=1

select,/p,/re
if (keyword_set(frames_per_page) eq 0) then frames_per_page = 1
if (keyword_set(x_var) eq 0) then x_var = 'int'
if (keyword_set(cal_type) eq 0) then cal_type = 'amp,pha'
if (n_elements(smoothing) eq 0) and (n_elements(poly) eq 0) then begin
  if x_var eq 'int' then smoothing = 20.
  if x_var eq 'hours' then smoothing = 2.
  if x_var eq 'el' then poly=2
endif
plid = 1

tel_bsl='telescope'

if (not keyword_set(connect)) then connect = 0

print, '***********************************************'
print, 'gain solutions for rx345:'
print, '...'

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

if newformat eq 0 then result=dat_filter(s_f,'"iaq" eq "0" and "ibq" eq "0" and "wt" gt "0"',/reset,/no_notify) else result=dat_filter(s_f,'"ant1rx" eq "0" and "ant2rx" eq "0" and "wt" gt "0"',/reset,/no_notify)

if (keyword_set(connect)) then $
  result=gain(gai_souids,gai_fluxes_3mm,gai_fluxes_1mm, $
  tel_bsl,x_var,cal_type,frames_per_page, $
  dt_smooth=smoothing,plid,refant=refant,loose=loose, $
  connect=connect, preavg=preavg, no_unwrap=no_unwrap, $
  non_point=non_point, polrx=-1,_extra=extra_keywords) $
else $
  if (keyword_set(smoothing) gt 0) then $
  result=gain(gai_souids,gai_fluxes_3mm,gai_fluxes_1mm, $
  tel_bsl,x_var,cal_type,frames_per_page, $
  dt_smooth=smoothing,plid,refant=refant,loose=loose, $
  preavg=preavg, no_unwrap=no_unwrap, non_point=non_point, $
  polrx=-1,_extra=extra_keywords) $
else $
  result=gain(gai_souids,gai_fluxes_3mm,gai_fluxes_1mm, $
  tel_bsl,x_var,cal_type,frames_per_page, $
  dt_smooth=-poly,plid, refant=refant,loose=loose, $
  preavg=preavg, no_unwrap=no_unwrap, non_point=non_point, $
  polrx=-1,_extra=extra_keywords)

if not keyword_set(defaults) then begin
   aa = ''
   read,aa,prompt='Apply gain solution? [NO <YES>]:  '
   if (aa eq 'YES' or aa eq 'yes' or aa eq 'Yes' or aa eq 'Y' or aa eq 'y') then begin
      print,'YES: apply gains'
      print,'Applying Gains'
      result=cal_apply(gain=cal_type,x_var=x_var,tel_bsl=tel_bsl,refant=refant,poldualrx=[-1,-1])
   endif else begin
      print,'NO: nothing done'
   endelse   
endif else begin
   result=cal_apply(gain=cal_type,x_var=x_var,tel_bsl=tel_bsl,refant=refant,poldualrx=[-1,-1])
endelse


print, '***********************************************'
print, 'gain solutions for rx400:'
print, '...'

;result=gain_ini(30.,use,all_souids,all_sources,all_amps, $
;   numbs_3mm,numbs_1mm,fluxes_3mm,fluxes_1mm,defaults=defaults)
;if result eq 0 then begin
;    print, 'Found no sources in dataset to be selected as gain calibrators'
;    print, 'Quit !'
;    return
;endif

;j = where(use eq 1,count)
;if count eq 0 then begin
;    print, 'Found no sources in dataset to be selected as gain calibrators'
;    print, 'Quit !'
;    return
;endif

;ncals = n_elements(j)
;print,'number of gain cals ',ncals
;print,'gain cal source(s): ',all_sources[j]
;print,'gain cal 3mm fluxes:',fluxes_3mm[j]
;print,'gain cal 1mm fluxes:',fluxes_1mm[j]


;gai_souids = make_array(ncals,/float)
;gai_fluxes_3mm = make_array(ncals,/float)
;gai_fluxes_1mm = make_array(ncals,/float)

;for i = 0, ncals-1 do begin
;  gai_souids[i] = all_souids[j[i]]
;  gai_fluxes_3mm[i] = fluxes_3mm[j[i]]
;  gai_fluxes_1mm[i] = fluxes_1mm[j[i]]
;  if gai_fluxes_3mm[i] eq 0 or gai_fluxes_1mm[i] eq 0 then begin
;      print, 'Calibrator fluxes should be nonezero !'
;      return
;  endif
;endfor

if newformat eq 0 then result=dat_filter(s_f,'"iaq" eq "1" and "ibq" eq "1" and "wt" gt "0"',/reset,/no_notify) else result=dat_filter(s_f,'"ant1rx" eq "1" and "ant2rx" eq "1" and "wt" gt "0"',/reset,/no_notify)

if (keyword_set(connect)) then $
  result=gain(gai_souids,gai_fluxes_3mm,gai_fluxes_1mm, $
  tel_bsl,x_var,cal_type,frames_per_page, $
  dt_smooth=smoothing,plid,refant=refant,loose=loose, $
  connect=connect, preavg=preavg, no_unwrap=no_unwrap, $
  non_point=non_point, polrx=-2,_extra=extra_keywords) $
else $
  if (keyword_set(smoothing) gt 0) then $
  result=gain(gai_souids,gai_fluxes_3mm,gai_fluxes_1mm, $
  tel_bsl,x_var,cal_type,frames_per_page, $
  dt_smooth=smoothing,plid,refant=refant,loose=loose, $
  preavg=preavg, no_unwrap=no_unwrap, non_point=non_point, $
  polrx=-2,_extra=extra_keywords) $
else $
  result=gain(gai_souids,gai_fluxes_3mm,gai_fluxes_1mm, $
  tel_bsl,x_var,cal_type,frames_per_page, $
  dt_smooth=-poly,plid, refant=refant,loose=loose, $
  preavg=preavg, no_unwrap=no_unwrap, non_point=non_point, $
  polrx=-2,_extra=extra_keywords)

if not keyword_set(defaults) then begin
   aa = ''
   read,aa,prompt='Apply gain solution? [NO <YES>]:  '
   if (aa eq 'YES' or aa eq 'yes' or aa eq 'Yes' or aa eq 'Y' or aa eq 'y') then begin
      print,'YES: apply gains'
      print,'Applying Gains'
      result=cal_apply(gain=cal_type,x_var=x_var,tel_bsl=tel_bsl,refant=refant,poldualrx=[-2,-2])
   endif else begin
      print,'NO: nothing done'
   endelse   
endif else begin
   result=cal_apply(gain=cal_type,x_var=x_var,tel_bsl=tel_bsl,refant=refant,poldualrx=[-2,-2])
endelse

if not keyword_set(defaults) then begin
   aa = ''
   print,''
   print,'***'
   read,aa,prompt='Apply gain solutions of rx345 and rx400 on cross rx polarization data? [NO <YES>]:  '
   if (aa eq 'YES' or aa eq 'yes' or aa eq 'Yes' or aa eq 'Y' or aa eq 'y') then begin
      print,'YES: apply gains'
      print,'Applying Gains for rx345 - rx400'
      if newformat eq 0 then result=dat_filter(s_f,'"iaq" eq "0" and  "ibq" eq "1" and "wt" gt "0"',/reset,/no_notify) else result=dat_filter(s_f,'"ant1rx" eq "0" and  "ant2rx" eq "1" and "wt" gt "0"',/reset,/no_notify)
      result=cal_apply(gain=cal_type,tel_bsl=tel_bsl,refant=refant,x_var=x_var,poldualrx=[-1,-2])   

      print,''
      print,'Applying Gains for rx400-rx345'
      if newformat eq 0 then result=dat_filter(s_f,'"iaq" eq "1" and  "ibq" eq "0" and "wt" gt "0"',/reset,/no_notify) else result=dat_filter(s_f,'"ant1rx" eq "1" and  "ant2rx" eq "0" and "wt" gt "0"',/reset,/no_notify)
      result=cal_apply(gain=cal_type,tel_bsl=tel_bsl,refant=refant,x_var=x_var,poldualrx=[-2,-1])   
      
   endif else begin
      print,'NO: nothing done'
   endelse   
endif else begin
   print,''
   print,'Applying Gains for rx345 - rx400'
      if newformat eq 0 then result=dat_filter(s_f,'"iaq" eq "0" and  "ibq" eq "1" and "wt" gt "0"',/reset,/no_notify) else result=dat_filter(s_f,'"ant1rx" eq "0" and  "ant2rx" eq "1" and "wt" gt "0"',/reset,/no_notify)
      result=cal_apply(gain=cal_type,tel_bsl=tel_bsl,refant=refant,x_var=x_var,poldualrx=[-1,-2])   

      print,''
      print,'Applying Gains for rx400-rx345'
      if newformat eq 0 then result=dat_filter(s_f,'"iaq" eq "1" and  "ibq" eq "0" and "wt" gt "0"',/reset,/no_notify) else result=dat_filter(s_f,'"ant1rx" eq "1" and  "ant2rx" eq "0" and "wt" gt "0"',/reset,/no_notify) 
      result=cal_apply(gain=cal_type,tel_bsl=tel_bsl,refant=refant,x_var=x_var,poldualrx=[-2,-1])   

endelse

;result=dat_filter(s_f,/reset)
select,/p,/re

end
