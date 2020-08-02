pro tsys_replace, badant=badant, goodant=goodant, factor=factor

common global
common data_set

; tsys_replace.pro
;
; This procedure replaces the Tsys values for one antenna by the Tsys 
; values for another. 
; 
; May only be used AFTER the procedure tsys_ant has been used to calculate
; antenna-based Tsys (stored in bl[pbf].csnr and bl[pbf].cnoise)
;
; After running this procedure, you can run tsys_bsl to replace the 
; baseline-based values with the corrected Tsys. 
;
; mhughes 27Jan07 
; edited for multi-frequency data sets 03Jul07

if tag_exist(bl,'csnr') ne 0 then newformat=0 else newformat=1

print,'*************************************************************'
print,'Tsys_replace.pro'
print,'*************************************************************'
print,'Replace Tsys values for antenna x with values from antenna y:'
;badant=0
;goodant=0
if not keyword_set(badant) then read,badant,prompt='x = '
if not keyword_set(goodant) then read,goodant,prompt='y = '
if not keyword_set(factor) then factor=1.

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


; setting up: finding out which antennas are in the array
subset1=bl[pbf].itel1
subset2=bl[pbf].itel2
subset=[subset1,subset2]
ants=subset(uniq(subset, sort(subset)))
nants=n_elements(ants)

; using baseline goodant-badant to get Tsys for goodant for all ints
if goodant gt badant then begin
  loc=where(bl[pbf].itel1 eq badant and bl[pbf].itel2 eq goodant,count) 
  if count eq 0 then begin
     print,'Check antenna numbers !'
     print,'Failed!'
     return
  endif
  if newformat eq 0 then tgood=bl[pbf[loc]].cnoise else tgood=bl[pbf[loc]].blhdbl4
endif else begin
  loc=where(bl[pbf].itel1 eq goodant and bl[pbf].itel2 eq badant,count) 
  if count eq 0 then begin
     print,'Check antenna numbers !'
     print,'Failed!'
     return
  endif
  if newformat eq 0 then tgood=bl[pbf[loc]].csnr else tgood=bl[pbf[loc]].blhdbl5
endelse

; looping through baselines and replacing badant values with goodant values
dimens=n_elements(loc)
for i=0,nants-2 do begin
  for j=i+1,nants-1 do begin
    loc=where(bl[pbf].itel1 eq ants(i) and bl[pbf].itel2 eq ants(j),count) 
    if count ne dimens then begin
      print,'Number of data points not consistent between baselines.'
      print,'Check data structure. '
      print,'Exiting...'
      return
    endif
    if ants(i) eq badant then begin 
       if newformat eq 0 then bl[pbf[loc]].csnr=tgood*factor else bl[pbf[loc]].blhdbl5=tgood*factor
    endif
    if ants(j) eq badant then begin
       if newformat eq 0 then bl[pbf[loc]].cnoise=tgood*factor else bl[pbf[loc]].blhdbl4=tgood*factor
    endif
  endfor
endfor

print,'FINISHED!'
return
end
