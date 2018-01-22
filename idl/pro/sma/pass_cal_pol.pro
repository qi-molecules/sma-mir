pro pass_cal_pol,x_var=x_var,cal_type=cal_type,frame_vars=frame_vars, $
          frames_per_page=frames_per_page,smoothing=smoothing, $
          ntrim_max=ntrim_max, no_unwrap=no_unwrap, poly=poly, $
          tel_bsl=tel_bsl, refant=refant, swarm=swarm, asic=asic, $
          preavg=preavg, defaults=defaults,_extra=extra_keywords 
common global
common data_set
common plo

if tag_exist(bl,'iaq') ne 0 then newformat=0 else newformat=1

if keyword_set(swarm) then select,/p,/re,/swarm
if keyword_set(asic) then select,/p,/re,/asic
if (not keyword_set(swarm)) and (not keyword_set(asic)) then select,/p,/re
result=dat_filter(s_s,/save)

result =  pass_ini(use,all_souids,all_sources,all_amps,defaults=defaults)
if result eq 0 then begin
    print, 'Found no sources in dataset to be selected as passband calibrators'
    print, 'Quit !'
    return
endif

print,'use ',use
print,'all_souids ',all_souids
print,'all_sources ',all_sources
print,'all_amps ', all_amps

j = where(use eq 1, count)
if count eq 0 then begin
    print, 'Found no sources in dataset to be selected as passband calibrators'
    print, 'Quit !'
    return
endif

ncals = n_elements(j)
print,'number of pass cals ',ncals
pas_souids = make_array(ncals,/float)

for i = 0,ncals-1 do begin
  pas_souids[i] = all_souids[j[i]]
endfor

print,'pas_souids ',pas_souids

if not keyword_set(frames_per_page) then frames_per_page = 4
if not keyword_set(x_var) then x_var = 'channel'
if not keyword_set(frame_vars) then frame_vars = 'blcd,rec,sb,band'
if not keyword_set(cal_type) then cal_type = 'amp,pha'
if not keyword_set(smoothing) then begin
  if n_elements(poly) ne 1 then begin
    smoothing = 20
    funct = 'boxcar'
  endif else begin
    smoothing = -1
    funct = 'poly'
  endelse
endif else begin
  funct = 'boxcar'
endelse

if not keyword_set(preavg) then preavg=1L else preavg=long(preavg)

tel_bsl='telescope'

plid = 1

aa=''

print, '***********************************************'
print, 'Passband for rx345:'
print, '...'

result=dat_filter(s_s,/restore)
if newformat eq 0 then result=dat_filter(s_f,'"iaq" eq "0" and "ibq" eq "0" and "wt" gt "0"',/no_notify) else result=dat_filter(s_f,'"ant1rx" eq "0" and "ant2rx" eq "0" and "wt" gt "0"',/no_notify)

result=pass(pas_souids,x_var,cal_type,smoothing,frame_vars, $
  frames_per_page,plid,ntrim_max=ntrim_max, $
  tel_bsl=tel_bsl, refant=refant, no_unwrap=no_unwrap, $
  funct=funct, npoly=poly, delay=delay, preavg=preavg, $
  polrx=-1,_extra=extra_keywords)

if not keyword_set(defaults) then begin
   aa = ''

   read,aa,prompt='Apply passband solution for rx345? [NO <YES>]:  '
   if (aa eq 'YES' or aa eq 'yes' or aa eq 'Yes' or aa eq 'Y' or aa eq 'y') then begin
      print,'YES: apply passband cal'
      print,'Applying Passbands'
      result=cal_apply2(passband=cal_type,tel_bsl=tel_bsl,refant=refant,x_var=x_var,poldualrx=[-1,-1])   

   endif else begin
      print,'NO: nothing done'
   endelse   
endif else begin
   print,'Applying Passbands'
   result=cal_apply2(passband=cal_type,tel_bsl=tel_bsl,refant=refant,x_var=x_var,poldualrx=[-1,-1])   
endelse

print, ''
print, '***'
print, 'Passband for rx400:'
print, '...'

result=dat_filter(s_s,/restore)
if newformat eq 0 then result=dat_filter(s_f,'"iaq" eq "1" and "ibq" eq "1" and "wt" gt "0"',/no_notify) else result=dat_filter(s_f,'"ant1rx" eq "1" and "ant2rx" eq "1" and "wt" gt "0"',/no_notify)

result=pass(pas_souids,x_var,cal_type,smoothing,frame_vars, $
  frames_per_page,plid,ntrim_max=ntrim_max, $
  tel_bsl=tel_bsl, refant=refant, no_unwrap=no_unwrap, $
  funct=funct, npoly=poly, delay=delay, preavg=preavg, $
  polrx=-2,_extra=extra_keywords)

if not keyword_set(defaults) then begin
   aa = ''

   read,aa,prompt='Apply passband solution for rx400? [NO <YES>]:  '
   if (aa eq 'YES' or aa eq 'yes' or aa eq 'Yes' or aa eq 'Y' or aa eq 'y') then begin
      print,'YES: apply passband cal'
      print,'Applying Passbands'
      result=cal_apply2(passband=cal_type,tel_bsl=tel_bsl,refant=refant,x_var=x_var,poldualrx=[-2,-2])   

   endif else begin
      print,'NO: nothing done'
   endelse   
endif else begin
   print,'Applying Passbands'
   result=cal_apply2(passband=cal_type,tel_bsl=tel_bsl,refant=refant,x_var=x_var,poldualrx=[-2,-2])   
   print,'Done!'
endelse


if not keyword_set(defaults) then begin
   aa = ''
   print,''
   print,'***'
   read,aa,prompt='Apply passband solutions of rx345 and rx400 on cross rx polarization data? [NO <YES>]:  '
   if (aa eq 'YES' or aa eq 'yes' or aa eq 'Yes' or aa eq 'Y' or aa eq 'y') then begin
      print,'YES: apply passband cal'
      print,'Applying Passbands for rx345-rx400'
      result=dat_filter(s_s,/restore)
      if newformat eq 0 then result=dat_filter(s_f,'"iaq" eq "0" and  "ibq" eq "1" and "wt" gt "0"',/no_notify) else result=dat_filter(s_f,'"ant1rx" eq "0" and  "ant2rx" eq "1" and "wt" gt "0"',/no_notify)
      result=cal_apply2(passband=cal_type,tel_bsl=tel_bsl,refant=refant,x_var=x_var,poldualrx=[-1,-2])   

      print,'Applying Passbands for rx400-rx345'
      result=dat_filter(s_s,/restore)
      if newformat eq 0 then result=dat_filter(s_f,'"iaq" eq "1" and  "ibq" eq "0" and "wt" gt "0"',/no_notify) else result=dat_filter(s_f,'"ant1rx" eq "1" and  "ant2rx" eq "0" and "wt" gt "0"',/no_notify)
      result=cal_apply2(passband=cal_type,tel_bsl=tel_bsl,refant=refant,x_var=x_var,poldualrx=[-2,-1])   

   endif else begin
      print,'NO: nothing done'
   endelse   
endif else begin
   print,''
   print,'Applying Passbands for rx345-rx400'
   result=dat_filter(s_s,/restore)
   if newformat eq 0 then result=dat_filter(s_f,'"iaq" eq "0" and  "ibq" eq "1" and "wt" gt "0"',/no_notify) else result=dat_filter(s_f,'"ant1rx" eq "0" and  "ant2rx" eq "1" and "wt" gt "0"',/no_notify)
   result=cal_apply2(passband=cal_type,tel_bsl=tel_bsl,refant=refant,x_var=x_var,poldualrx=[-1,-2])   

   print,''
   print,'Applying Passbands for rx400-rx345'
   result=dat_filter(s_s,/restore)
   if newformat eq 0 then result=dat_filter(s_f,'"iaq" eq "1" and  "ibq" eq "0" and "wt" gt "0"',/no_notify) else result=dat_filter(s_f,'"ant1rx" eq "1" and  "ant2rx" eq "0" and "wt" gt "0"',/no_notify)
   result=cal_apply2(passband=cal_type,tel_bsl=tel_bsl,refant=refant,x_var=x_var,poldualrx=[-2,-1])   

endelse

;result=dat_filter(s_f,/reset)
select,/p,/re

end
