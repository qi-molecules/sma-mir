pro pass_cal,x_var=x_var,cal_type=cal_type,frame_vars=frame_vars, $
          frames_per_page=frames_per_page,smoothing=smoothing, $
          ntrim_max=ntrim_max, no_unwrap=no_unwrap, poly=poly, $
          tel_bsl=tel_bsl, refant=refant, sideband=sideband, $
          delay=delay, apply_all=apply_all, old=old,$
          preavg=preavg, unity_fill=unity_fill,defaults=defaults, $
          wideband=wideband, _extra=extra_keywords 
;yes
;=Task:PASS_CAL --- To execute bandpass calibration (SMA wrapper)
;#type: calib
;+Use:
;     PASS_CAL derives bandpass solution and if needed apply 
;     the calibration to the UV data. The first thing to 
;     do in PASS_CAL is to choose a calibrator as in 
;     GAIN_CAL, however the absolute fluxes of the bandpass
;     calibrators do not have to be entered in the passband 
;     selection because the total amplitude across each spectrum
;     of the calibrator is normalized to unity before appllying
;     the passband corrections.
;@x_var: 
;     header variable for x-coor:
;        'channel' : channel --- the default
;        'fsky'    : sky frequency
;        'velocity': velocity 
;@cal_type:
;     passband solution to be derived:
;        'amp'    : amplitude 
;        'pha'    : phase
;        'amp,pha': amplitude and phase --- the default
;@smoothing:
;     number of channels to smooth the spectra.
;     default : 20 channels
;@frame_vars:
;     header variable used to separate data between plot pages.
;        'blcd'  : baseline
;        'rec'   : receiver
;        'sb'    : sideband
;        'band'  : band
;     the combibation of 'blcd, rec, sb, band' can be used.
;@frames_per_page:
;     maximum number of frames per page
;     default : 4
;@ntrim:
;      To ingore "ntrim" number of channel on the chunck edges.
;@no_unwrap:
;      by default phases will be unwrapped over the channel space.
;      Set this flag to deactivate phase unwrapping.
;@tel_bsl:
;      'baseline' :baseline-based solution and
;      'telescope':antenna-based solution
;      'baseline' will be the default, if not set
;@refant:
;      reference antenna needed in the ant-based
;      solution derivation, NO default
;@sideband:
;      To drive passband solution using data from one sideband
;      and use on the opposite sideband. This is useful when
;      calibration data from one sideband is comtaminated by
;      spectral line features.
;@delay:
;      Set this flag to use the passband (phase) data to derive
;      the instrumental dealy. (ONLY baseline-based solution is
;      available at this moment).
;@_extra:
;@apply_call:
;      to apply the passband solution to the full dataset,
;      ignoring the filter set-up. By detault, the solution is
;      only applied to filtered data. This option is useful
;      when one use partial baselines to obtain antenna-based
;      passband solutions but would like to apply the solution
;      to the full dataset.
;&history:
;--------------------------------------------------------------------
;      cykuo 19dec03 adapting the form of the header
;---------------------------------------------------------------------     

common global
common data_set
common plo

if (keyword_set(apply_all)) then begin
     result=dat_filter(s_s,/save)
     result=dat_filter(s_f,'"wt" gt "0"',/reset,/no_notify)
endif
result =  pass_ini(use,all_souids,all_sources,all_amps,defaults=defaults)
if result eq 0 then begin
    print, 'Found no sources in dataset to be selected as passband calibrators'
    print, 'Quit !'
    return
endif

if (keyword_set(apply_all)) then result=dat_filter(s_s,/restore)

if (keyword_set(sideband)) then begin
   print, 'WARNING WARNING WARNING'
   print, 'Please make sure the phase conjugate is consistent '
   print, '       between upper and lower sidebands'
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
if keyword_set(delay) then begin
   poly=1
   x_var='fsky'
   cal_type='pha'
   no_unwrap=1
endif
if not keyword_set(preavg) then preavg=1L else preavg=long(preavg)
;if not keyword_set(unity_fill) then unity_fill=0
if not keyword_set(tel_bsl) then tel_bsl='baseline'
if (tel_bsl ne 'baseline' and tel_bsl ne 'telescope') then tel_bsl='baseline'

plid = 1

aa=''

if keyword_set(sideband) then begin
   if x_var ne 'channel' then begin
      print,"x_var has to be set as 'channel'"
      print,'quit!'
   endif
  yfs=0.
  result=pass_sideband(pas_souids,x_var,cal_type,smoothing,frame_vars, $
		frames_per_page,plid,ntrim_max=ntrim_max, $
                tel_bsl=tel_bsl, refant=refant, $
                no_unwrap=no_unwrap, funct=funct, npoly=poly, delay=delay, preavg=preavg, $
                sideband=sideband, yfs=yfs, _extra=extra_keywords)

  sideband=(sideband eq 'u') ? 'l' : 'u'

  result=pass_sideband(pas_souids,x_var,cal_type,smoothing,frame_vars, $
		frames_per_page,plid,ntrim_max=ntrim_max, $
                tel_bsl=tel_bsl, refant=refant, $
                no_unwrap=no_unwrap, funct=funct, npoly=poly, delay=delay, preavg=preavg, $
                sideband=sideband, yfs=yfs, _extra=extra_keywords)

endif else begin
   
   if keyword_set(wideband) then begin
      result=pass_wide(pas_souids,x_var,cal_type,smoothing,frame_vars, $
        frames_per_page,plid,ntrim_max=ntrim_max, $
        no_unwrap=no_unwrap, funct=funct, npoly=poly, $
        preavg=preavg, _extra=extra_keywords)

   endif else begin

      result=pass(pas_souids,x_var,cal_type,smoothing,frame_vars, $
        frames_per_page,plid,ntrim_max=ntrim_max, $
        tel_bsl=tel_bsl, refant=refant, no_unwrap=no_unwrap, $
        funct=funct, npoly=poly, delay=delay, preavg=preavg, $
        _extra=extra_keywords)

   endelse
   
endelse

if not keyword_set(defaults) then begin
aa = ''

read,aa,prompt='Apply passband solution? [NO <YES>]:  '
if (aa eq 'YES' or aa eq 'yes' or aa eq 'Yes' or aa eq 'Y' or aa eq 'y') then begin
   print,'YES: apply passband cal'

   if (keyword_set(apply_all)) then begin
     print, ' --- Flag apply_all is set. Set filter temperarily to the full dataset ---'
     result=dat_filter(s_s,/save)
     result=dat_filter(s_f,'"wt" gt "0"',/reset,/no_notify)
   endif

   print,'Applying Passbands'
   if keyword_set(old) then result=cal_apply(passband=cal_type,tel_bsl=tel_bsl,refant=refant,x_var=x_var) else result=cal_apply2(passband=cal_type,tel_bsl=tel_bsl,refant=refant,x_var=x_var)

    if (keyword_set(apply_all)) then begin
      print,' --- Restoring filter ---'
      result=dat_filter(s_s,/restore)
    endif

endif else begin
   print,'NO: nothing done'
endelse

endif else begin
   print,'Applying Passbands'
   if keyword_set(old) then result=cal_apply(passband=cal_type,tel_bsl=tel_bsl,refant=refant,x_var=x_var) else result=cal_apply2(passband=cal_type,tel_bsl=tel_bsl,refant=refant,x_var=x_var)

endelse

end
