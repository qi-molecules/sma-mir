pro uti_vel_fix, refband=refband, band=band, regenerate=regenerate, reffreq=reffreq, refsb=refsb, refvel=refvel

common global
common data_set

if not keyword_set(regenerate) then begin

if (keyword_set(refband) eq 0 or (keyword_set(band) eq 0)) then begin
   print, 'Please set the reference band and the target band '
   print, '      with keywords refband and band.' 
   print, 'Quit!'
   return
endif


if ((strpos(refband,'s') eq -1 or strpos(band,'s') eq -1)) then begin
   print, 'Please check the input band name again !'
   print, 'No velocity change made, Quit!'
   return
endif

result=dat_filter(s_s,/save,/no_notify)
command='"band" eq "'+refband+'"'
result=dat_filter(s_f,command,/no_notify,/reset)

f0=sp[psf].fsky
v0=sp[psf].vel
fs=f0/sqrt((1-v0*1000.d/!cvel)/(1+v0*1000.d/!cvel))

command='"band" eq "'+band+'"'
result1=dat_filter(s_f,command,/no_notify,/reset)
if result ne result1 then begin
   print, 'Inconsistent scan numbers for both bands, Quit!'
   return
endif
   
f1=sp[psf].fsky
alpha=(f1/fs)^2.
sp[psf].vel=-(alpha-1.)*!cvel/(1000.d*(alpha+1.))
;sp[psf].vel=v1

result=dat_filter(s_s,/restore,/no_notify)
print, 'Velocity header fixed on band '+band+' using band '+refband 

endif else begin

if not keyword_set(reffreq) then begin
   reffreq=0.d
   read,reffreq,prompt='Input the dopplerTracking frequency in GHz (double-precesion): '
endif
reffreq=double(reffreq)

if not keyword_set(refvel) then begin
   refvel=0.d
   read,refvel,prompt='Input the dopplerTracking velocity in km/s: '
endif
refvel=double(refvel)

if not keyword_set(refsb) then begin
   refsb=''
   read,refsb,prompt='Input the dopplerTracking sideband: '
endif

if not keyword_set(refband) then begin
   refband=''
   read,refband,prompt='Input the dopplerTracking chunk: '
endif

command='"band" eq "'+refband+'" and "sb" eq "'+refsb+'"'
result=dat_filter(s_f,command,/no_notify,/reset)
print, '*************************'
print, 'Regenerating velocity header based on input information from ',refsb+refband+'...'

;print,command

velo=in[0].vc*double(1000.)+sp[0].vradcat
beta=velo/!cvel
delta=(reffreq-sqrt((1+beta)/(1-beta))*sp[psf[0]].fsky)/reffreq*!cvel/1000.
sp[psf].vel=double(refvel+delta)
;velheader=double(catv+delta)
;print,velheader

f0=sp[psf].fsky
v0=sp[psf].vel
fs=f0/sqrt((1-v0*1000.d/!cvel)/(1+v0*1000.d/!cvel))

command='"band" eq "'+refband+'" and "sb" ne "'+refsb+'"'
;print,command
result=dat_filter(s_f,command,/no_notify,/reset)
f1=sp[psf].fsky
alpha=(f1/fs)^2.
sp[psf].vel=-(alpha-1.)*!cvel/(1000.d*(alpha+1.))

command='"band" eq "'+refband+'"'
result=dat_filter(s_f,command,/no_notify,/reset)
f0=sp[psf].fsky
v0=sp[psf].vel
fs=f0/sqrt((1-v0*1000.d/!cvel)/(1+v0*1000.d/!cvel))

allbands=uti_distinct(sp.iband,nbands,/many_repeat)
for i=0,nbands-1 do begin
   command='"iband" eq"'+strcompress(string(allbands[i]),/remove)+'"'
   result=dat_filter(s_f,command,/no_notify,/reset)
   f1=sp[psf].fsky
   alpha=(f1/fs)^2.
   sp[psf].vel=-(alpha-1.)*!cvel/(1000.d*(alpha+1.))
endfor

print,'Done!'
print, '*************************'

endelse

end
