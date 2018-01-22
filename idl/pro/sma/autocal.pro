pro autocal, file=file, refant=refant

common global
common data_set
common plo

pl[*].plot_device='null'
if not keyword_set(refant) then refant=4
if not keyword_set(file) then read,file,prompt='Enter the calibration source table file (or return for calsources.txt as default):'
if file eq '' then file='./calsources.txt'


; 0) Loading calibrator information
count=0 & result=findfile(file,count=count)
IF count eq 0 THEN BEGIN
   print,'Could not find file: ',file
   print,'Failed. No auto calibration done !'
   RETURN
ENDIF
openr,/get_lun, unit, file
print,'Loading calibration sources table:', file
line=''
flux_inp=0
fluxflag=0
gainsource=''
passsource=''
while not EOF(unit) do begin
   readf,unit,line
   parts=strtrim(strsplit(line,',',/extract),2)
   sn=parts[1]
   qc=parts[0]
   fx=0
   if n_elements(parts) eq 3 then fx=float(parts[2])
   list = '"source" eq "'+strtrim(sn,2)+'"'
   result=dat_filter(s_f,list,/reset,/no_notify)
   if result gt 0 then begin
      case qc of
         'pass' : begin
            sp[psf].ipq=1
            passsource=[passsource,strtrim(sn,2)]
         end
         'gain' : begin
            sp[psf].igq=1
            if fx then in[pif].sflux=fx else in[pif].sflux=1
            gainsource=[gainsource,strtrim(sn,2)]
         end
         'flux' : begin
            fluxflag=1
            fluxsource=strtrim(sn,2)
            if fx then flux_inp=fx
         end
         else: begin
            print, 'Invalid input in calibration source table !' & return
         endelse
      endcase
;      if qc eq 'gain' then in[pil].sflux=fx
   endif
endwhile

if not fluxflag then begin
   if fx eq 0 then begin
      print,'Put gain calibrator flux into calibration table file or add flux calibrator.'
      return
   endif
endif

; 1) Flag pointing data
print,''
print,'***** 1) Search and flag pointing (scan time < 10s) data *****'
result=dat_filter(s_f,'"integ" lt "10"',/no_notify,/reset)
if result gt 0 then begin
   flag,/flag
   print,'Pointing data flagged!'
endif else print,'No pointing data!'

; 2) Search and fix spikes
print,''
print,'***** 2) Search and fix spikes ******'
for i=1, n_elements(passsource)-1L do begin
   result=dat_filter(s_f,'"wt" gt "0"',/no_notify,/reset)
   uti_checkspike,source=passsource[i],/fix
endfor

; 3) Regenerate continuum
print,''
print, '***** 3) Regenerate continuum *****'
result=dat_filter(s_f,'"wt" gt "0"',/no_notify,/reset)
uti_avgband

; 4) Apply tsys correction
print,''
print, '***** 4) Tsys correction *****'
apply_tsys

; 5) Bandpass calibration
nch=uti_distinct(sp[psf].nch,nnch,/many)
nch=nch[where(nch ne 1)]

for i=0, n_elements(nch)-1 do begin
   print,''
   print,'***** 5) Bandpass calibration - phase only              *****'
   print,'*****    Will vector-average 10 MHz channels by default  *****'
   command='"nch" eq "'+strcompress(string(nch[i]),/remove)+'" and "wt" gt "0"'
   print,command
   result=dat_filter(s_f,command,/no_notify,/reset)
   chavg=floor(10./abs(sp[psf[0]].fres))
   print,'** Vector-averaging:',chavg,' ntrim:',floor(nch[i]/100.)
   pass_cal,cal_type='pha',preavg=chavg ,tel_bsl='telescope',refant=refant, ntrim=floor(nch[i]/100.),/defaults,/noplot
endfor

print,''
print, '***** Regenerate continuum *****'
result=dat_filter(s_f,'"wt" gt "0"',/no_notify,/reset)
uti_avgband

for i=0, n_elements(nch)-1 do begin
   print,''
   print,'***** 5) Bandpass calibration - amplitude only *****'
   command='"nch" eq "'+strcompress(string(nch[i]),/remove)+'" and "wt" gt "0"'
   print,command
   result=dat_filter(s_f,command,/no_notify,/reset)
   print,'** Smoothing:',floor(nch[i]/10.),' ntrim:',floor(nch[i]/100.)
   pass_cal,cal_type='amp',smoothing=floor(nch[i]/10.),ntrim=floor(nch[i]/100.),/defaults,/noplot
endfor

; 6) Phase gain calibration
; 6.1) if there is flux calibrator, do flux cal and measure the fluxes of 
;    gain calibrators

if fluxflag then begin
   result=dat_filter(s_f,'"source" ne "'+fluxsource+'" and "wt" gt "0"',/no_notify,/reset)
   print,''
   print,'***** 6) Gain Calibration - phase only *****'
   gain_cal,cal_type='pha',x='hours',smoothing=0.3,/preavg,tel_bsl='telescope',refant=refant,/defaults,/noplot

   result=dat_filter(s_f,'"source" eq "'+fluxsource+'" and "wt" gt "0"',/no_notify,/reset)
   sp[psf].igq=1
   if flux_inp then in[pif].sflux=flux_inp else in[pif].sflux=1
   gain_cal,cal_type='pha',/connect,tel_bsl='telescope',refant=refant,/non_point,/defaults,/noplot


; flux measurement
   result=dat_filter(s_f,'"band" eq "c1" and "wt" gt "0"',/no_notify,/reset)
   result=flux_scale(fluxsource,'c1',flux_inp=flux_inp,/noprint,/scale_bsl)
   result=cal_apply(gain='amp')
   result=sma_flux_measure('c1',names=names,flux=flux,/defaults)

   result=dat_filter(s_f,'"source" eq "'+fluxsource+'" and "wt" gt "0"',/no_notify,/reset)
   sp[psf].igq=0
   minel=strcompress(string(floor(min(in[pif].el))),/remove)
   maxel=strcompress(string(ceil(max(in[pif].el))),/remove)

   result=dat_filter(s_f,'"wt" gt "0" and "band" eq "c1" and "el" gt "'+minel+'" and "el" lt "'+maxel+'"',/no_notify,/reset)
   result=sma_flux_measure('c1',names=names2,flux=flux2,snr=snr2,/defaults)

   for i=1, n_elements(gainsource)-1L do begin
      iflux=where(names eq gainsource[i])
      gainflux=flux[iflux]
      list = '"source" eq "'+gainsource[i]+'"'
      result=dat_filter(s_f,list,/reset,/no_notify)
      in[pif].sflux=gainflux[0]
      print,'The flux for Gain calibrator ',gainsource[i],' is derived as ',gainflux[0],' Jy'
      
      iflux=where(names2 eq gainsource[i],count)
      if count gt 0 then begin
         gainflux=flux2[iflux]
         snr=snr2[iflux]
         if snr gt 20 then begin
            in[pif].sflux=gainflux[0]
            print,'The flux for gain calibrator ',gainsource[i],' is derived as ',gainflux[0],' Jy between elevation ', minel, ' and ',maxel
         endif
      endif
   endfor
endif else begin
   result=dat_filter(s_f,'"wt" gt "0"',/no_notify,/reset)
   print,''
   print,'***** 6) Gain Calibration - phase only *****'
   gain_cal,cal_type='pha',x='hours',smoothing=0.3,/preavg,tel_bsl='telescope',refant=refant,/defaults,/noplot   
endelse

; 7) Amplitude gain calibration

result=dat_filter(s_f,'"wt" gt "0"',/no_notify,/reset)
print,''
print,'***** 7) Gain Calibration - amplitude *****'
gain_cal,cal_type='amp',x='hours',smoothing=0.3,/preavg,tel_bsl='telescope',refant=refant,/defaults,/noplot

result=dat_filter(s_f,'"wt" gt "0"',/no_notify,/reset)
mir_save,'calibrated.mir',/nowait
print,'***** 8) Calibration done ("calibrated.mir" saved) !'
pl[*].plot_device='x'
end


