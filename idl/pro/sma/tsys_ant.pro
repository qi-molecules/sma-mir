pro tsys_ant

common global
common data_set

; tsys_ant.pro
; 
; This procedure converts SMA baseline-based Tsys into antenna-based Tsys.
;      Tsys for ant 1 stored in : bl[pbf].csnr
;      Tsys for ant 2 stored in : bl[pbf].cnoise
; *** IMPORTANT *** Do not modify Tsys values AT ALL before using this 
;      procedure!
; 
; For use with the procedures tsys_replace (to replace one antenna's 
; Tsys by another's) and tsys_bsl (to convert the antenna-based Tsys
; back to a baseline-based quantity for use with other routines).
;
;  mhughes 28Jan07
;  revised for multi-frequency data sets 03Jul07

if tag_exist(bl,'csnr') ne 0 then newformat=0 else newformat=1

result=dat_filter(s_f,/reset,/no_notify)

; check which antennas are available
subset1=bl[pbf].itel1
subset2=bl[pbf].itel2
subset=[subset1,subset2]
ants=subset(uniq(subset, sort(subset)))
nants=n_elements(ants)
if (n_elements(ants) lt 3) then begin
  print,"***************************************************************"
  print,"Not enough antennas in the array.  Revise selection criteria."
  print,"Cannot extract antenna-based values with fewer than 3 antennas."
  print,"***************************************************************"
  return
endif

; In case of dual-frequency data sets
allfreq=bl[pbf].irec
freqs=allfreq(uniq(allfreq, sort(allfreq)))
nfreq=n_elements(freqs)
if nfreq gt 1 then begin
  print,"****************************************************************"
  print,"Multi-frequency data set.  "
  print,"Please choose a receiver: "
  strfreq=string(freqs)
  strfreq(n_elements(freqs)-1)=strfreq(n_elements(freqs)-1)+'        (highest freq)'
  print,'(lowest freq)',strfreq
  read,freq,prompt='receiver: '
  freqstr='"irec" eq "'+string(freq)+'"'
  result=dat_filter(s_f,freqstr,/no_notify)
  if freq gt 1 then begin
    print,'Please enter three antennas which have Rxs at this frequency:'
    read,oneant,prompt='1st: '
    read,twoant,prompt='2nd: '
    read,threeant,prompt='3rd: '
    loc1=where(ants eq oneant) & loc2=where(ants eq twoant) & loc3=where(ants eq threeant)
    if ((loc1 eq -1) or (loc2 eq -1) or (loc3 eq -1)) then begin
      print,'Invalid antenna numbers!  Exiting...'
      return
    endif
    if ((loc1 eq loc2) or (loc1 eq loc3) or (loc2 eq loc3)) then begin
      print,'Three different antennas are needed.  Exiting...'
      return
    endif
    ; switching so that ants(0), ants(1), and ants(3) contain the good antennas:
    ants(loc1)=ants(0) & ants(loc2)=ants(1) & ants(loc3)=ants(2)
    ants(0)=oneant & ants(1)=twoant & ants(2)=threeant
  endif
endif

print,"****************************************************************"
print,"Recovering antenna-based Tsys..."
; Making a new array of integration number and Tsys
loc=where(bl[pbf].itel1 eq ants(0) and bl[pbf].itel2 eq ants(1)) 
dimens=n_elements(loc)
; Tsys array will store Tsys values for each antenna for each integration, 
; spectral channel, sideband, polarization, etc.
tsysarray=fltarr(dimens,nants)
; temparray will store baseline-based Tsys values
temparray=fltarr(dimens,nants,nants)
for i=0,nants-2 do begin
  for j=i+1,nants-1 do begin
    if ants(i) lt ants(j) then loc=where(bl[pbf].itel1 eq ants(i) and bl[pbf].itel2 eq ants(j),count) else loc=where(bl[pbf].itel1 eq ants(j) and bl[pbf].itel2 eq ants(i),count)
    if count ne dimens then begin
       print,'Number of data points not consistent between baselines.'
       print,'Check data structure. '
       print,'(Suggestion: Are there integrations which appear at a different'
       print,'frequency for one antenna than for the others?  If so, try running'
       print,'this procedure with an extra dat_filter to filter out those integrations'
       print,'after the first use of dat_filter at the beginning of the procedure.)'
       print,'Exiting...'
       return
    endif
    temparray(*,i,j)=sp[psf[loc]].tssb
    temparray(*,j,i)=sp[psf[loc]].tssb
  endfor
endfor

; calculating antenna-based Tsys
t12=temparray(*,0,1)
t13=temparray(*,0,2)
t23=temparray(*,1,2)
tsysarray(*,0)=t12*t13/t23
tsysarray(*,1)=t12*t23/t13
tsysarray(*,2)=t13*t23/t12
if nants gt 3 then begin
  for i=3,nants-1 do begin
    t1n=temparray(*,0,i)
    t2n=temparray(*,1,i)
    tsysarray(*,i)=t1n*t2n/t12
  endfor
endif

; putting antenna-based Tsys into variables csnr and cnoise
for i=0,nants-2 do begin
  for j=i+1,nants-1 do begin
        if ants(i) lt ants(j) then begin
          loc=where(bl[pbf].itel1 eq ants(i) and bl[pbf].itel2 eq ants(j))
          if newformat eq 0 then begin
             bl[pbf[loc]].csnr=tsysarray(*,i) 
             bl[pbf[loc]].cnoise=tsysarray(*,j) 
          endif else begin
             bl[pbf[loc]].blhdbl5=tsysarray(*,i) 
             bl[pbf[loc]].blhdbl4=tsysarray(*,j) 
          endelse
        endif else begin
          loc=where(bl[pbf].itel2 eq ants(i) and bl[pbf].itel1 eq ants(j))
          if newformat eq 0 then begin
             bl[pbf[loc]].csnr=tsysarray(*,i) 
             bl[pbf[loc]].cnoise=tsysarray(*,j) 
          endif else begin
             bl[pbf[loc]].blhdbl5=tsysarray(*,i) 
             bl[pbf[loc]].blhdbl4=tsysarray(*,j) 
          endelse
        endelse
  endfor
endfor

if newformat eq 0 then begin
print,"****************************************************************"
print,'FINISHED'
print,'antenna (itel1) Tsys stored in bl[pbf].csnr'
print,'antenna (itel2) Tsys stored in bl[pbf].cnoise'
print,"****************************************************************"
endif else begin
print,"****************************************************************"
print,'FINISHED'
print,'antenna (itel1) Tsys stored in bl[pbf].blhdbl5'
print,'antenna (itel2) Tsys stored in bl[pbf].blhdbl4'
print,"****************************************************************"  
endelse

return
end
