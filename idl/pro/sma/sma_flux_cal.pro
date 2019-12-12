pro sma_flux_cal,channel=channel,day_range=day_range,_extra=extra_keywords
;yes
;=Task:SMA_FLUX_CAL --- To excute flux calibration (SMA wrapper)
;#Type: calib 
;+Use:
;      SMA_FLUX_CAL determines the scale factors for flux correction
;      and apply, if needed, the scalings to the visibility data.
;      Both primary and secondary type of calibrators can be used.
;      When using primary flux calibrators (planets, planetary
;      satellites), make sure the sizes of them (diameter in arcsecs)
;      are correct. Their expected fluxes are calculated by theoretical 
;      models. When using secondary flux calibrators, specify your
;      trusted fluxes by entering the source name and the flux
;      separated by a space. Scale factors are derived on each sideband
;      - sideband combination using only data satisfy a minimum
;      visibility and coherence requirement. Scale factors on
;      baseline-bases, sideband-bases, and an overall average will be
;      shown separately, which can be used for scale the data.
;@channel:   
;      continuum channel to be used for deriving scale factors.
;      'c1' is used by default
;@day_range: 
;      number of days to search back in database for (secondary) 
;      calibrator fluxes. Not implemented currently as SMA MIR/IDL
;      is not linked to a database. Fluxes for secondary calibrators
;      need to be specified.
;
;&history:
;--------------------------------------------------------------------
;       syliu 09mar04 compiling the header
;---------------------------------------------------------------------

common global
common data_set
common plo

;result=dat_filter(s_f,/reset)

if keyword_set(channel) eq 0 then channel='c1'
if keyword_set(day_range) eq 0 then day_range=30
result =  sma_flux_cal_ini(channel,day_range,names=names,flags=flags,amp=amp,flux=flux,nscans=nscans, _extra=extra_keywords)

sngc=''
print, 'Enter flux calibrator source, and if needed, flux in Jy, eg: 3c279 18.1'
read,sngc
parts = strtrim(strsplit(sngc,' ',/extract),2)
calsource = parts[0]
if (n_elements(parts) gt 1) then begin
  calflux=float(parts[1])
  result=flux_scale(calsource,channel,flux_inp=calflux,_extra=extra_keywords)
endif else begin
;  i=where(calsource eq names)
;  calflux=flux(i)
  result=flux_scale(calsource,channel,_extra=extra_keywords)
  if result eq -1 then begin
     print, '***** The flux calibrator has been completely resolved out'
     print, '***** Please use another flux source. Quit !!!'
     return
  endif
endelse



aa = ''
read,aa,prompt='Apply Flux Calibration? [NO <YES>]:  '
if (aa eq 'YES' or aa eq 'yes' or aa eq 'Yes' or aa eq 'Y' or aa eq 'y') then begin
   print,'YES: apply flux cal'
   result=cal_apply(gain='amp')
endif else begin
   print,'NO: nothing done'
endelse


end

