pro tsys_bsl

common global
common data_set

; tsys_bsl.pro
;
; This procedure converts SMA antenna-based Tsys into baseline-based Tsys.
;      Tsys for ant 1 (itel1) stored in : bl[pbf].csnr
;      Tsys for ant 2 (itel2) stored in : bl[pbf].cnoise
;      Tsys for the baseline stored in sp[psf].tssb
; 
; Intended to be used after tsys_ant has been used to calculate antenna-based
; Tsys and tsys_replace has been used to replace Tsys for antennas gone bad.
; 
; mhughes Dec06
; edited for multi-frequency data sets 03Jul07

if tag_exist(bl,'csnr') ne 0 then newformat=0 else newformat=1

result=dat_filter(s_f,/reset,/no_notify)

; In case of dual-frequency data sets
allfreq=bl[pbf].irec
freqs=allfreq(uniq(allfreq, sort(allfreq)))
nfreq=n_elements(freqs)
if nfreq gt 1 then begin
  print,"***************************************************************"
  print,"Multi-frequency data set.  "
  print,"Please choose a receiver: "
  strfreq=string(freqs)
  strfreq(n_elements(freqs)-1)=strfreq(n_elements(freqs)-1)+'        (highest freq)'
  print,'(lowest freq)',strfreq
  read,freq,prompt='receiver: '
  freqstr='"irec" eq "'+string(freq)+'"'
  result=dat_filter(s_f,freqstr,/no_notify)
endif

if newformat eq 0 then begin
   if (bl[pbf[0]].csnr le 40.) or (bl[pbf[0]].cnoise le 40.) then begin
      print,'You need run tsys_ant first before using this program.'
      print,'Quit !'
      return
   endif
endif else begin
   if (bl[pbf[0]].blhdbl5 le 40.) or (bl[pbf[0]].blhdbl4 le 40.) then begin
      print,'You need run tsys_ant first before using this program.'
      print,'Quit !'
      return
   endif
endelse   

; check which antennas are available
subset1=bl[pbf].itel1
subset2=bl[pbf].itel2
subset=[subset1,subset2]
ants=subset(uniq(subset, sort(subset)))
nants=n_elements(ants)

print,"Calculating baseline-based Tsys..."
; Loop through all baselines
for i=0,nants-2 do begin
  for j=i+1,nants-1 do begin
    loc=where(bl[pbf].itel1 eq ants[i] and bl[pbf].itel2 eq ants[j]) 
    if newformat eq 0 then sp[psf[loc]].tssb = sqrt( bl[pbf[loc]].csnr * bl[pbf[loc]].cnoise ) else sp[psf[loc]].tssb = sqrt( bl[pbf[loc]].blhdbl5 * bl[pbf[loc]].blhdbl4 )
  endfor
endfor

print,'FINISHED'

return
end
