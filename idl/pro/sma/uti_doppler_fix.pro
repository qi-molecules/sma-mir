pro uti_doppler_fix,reference=reference, source=source, raref=raref, decref=decref, diurnal=diurnal

common global
common data_set

if not (keyword_set(reference) or keyword_set(raref) or keyword_set(decref) $
  or keyword_set(diurnal)) then begin
   print, 'Please use REFERENCE keyword to select the source '
   print, '       on which original dopplerTrack was issued, e.g.'
   print, "       IDL> uti_doppler_fix, reference='HH212'  "
   print, '    or use RAREF and DECREF keyword to set the '
   print, '       apparent RA and Dec (in radians) of the ' 
   print, '       dopplerTracked source, e.g. '
   print, '       IDL> uti_doppler_fix,raref=3.27054, decref=0.03498
   print, '       which is tracking 3c273 on Feb 12, 2008.
   print, 'Quit !'
   return
endif

if n_elements(reference) gt 1 then begin
   print, 'Reference source number must be one !'
   print, 'Quit !'
   return
endif

; reset vres 
sp.vres=-1.*sp.fres/sp.fsky*!cvel/1e6

; Parameters
SECONDS_PER_DAY=24.*60.*60.
globalRefLong=155.477522d*!dpi/180.d
globalRefLat=19.82420526391d*!dpi/180.d
globalRefRadius=6382.248*1000.d
M_PER_KM=1000.d
longitude=-155.477522d

; mJD
datobs=c.ref_time[in[pi[0]].iref_time]
months = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
mon=strmid(datobs,0,3)
j=where(mon eq months,count)
num_day_obs=[fix(strtrim(strmid(datobs,8,4),2)),fix(strtrim(string(j[0]+1),2)),(strtrim(strmid(datobs,4,2),2))]
day=strtrim(string(num_day_obs[2]),2)
yr =strtrim(string(num_day_obs[0]),2)
mo =strtrim(string(num_day_obs[1]),2)
mJD=uti_date2mjd(yr,mo,day)
;print,'mjd: ', mjd
mjd2000=uti_date2mjd(2000,1,1)
newepoch=2000.+(mJD-mjd2000)/365.

; refunitVector

if not keyword_set(diurnal) then begin
if keyword_set(raref) then begin
   if not keyword_set(decref) then begin
      print, 'Please set DECREF keyword !'
      print, 'Quit !'
      return
   endif
   refRA=raref
   refDec=decref
endif else begin
   result=dat_list(s_l,'"source" eq "'+reference+'"',/no_notify,/reset)
   if result gt 0 then begin 
      refRA=double(in[pil[0]].rar)
      refDec=double(in[pil[0]].decr)
      uti_precess,refRA,refDec,2000,newepoch,/radian
   endif else begin
      print, 'Reference source ',reference, ' not found !'
      print, 'Quit !'
      return
   endelse
endelse

sladcs2c,refRA,refDec,refunitVector

endif 

; Source selection
list=''
if not keyword_set(source) or n_elements(source) gt 1 then begin
   print, 'Please provide ONE source name with keyword SOURCE !'
   print, 'Quit !'
   return
endif else begin
   list='"source" eq "'+source+'"'
endelse

result=dat_list(s_l,list,/reset,/no_notify)
if result le 0 then begin
   print, 'No source found !'
   print, 'Quit !'
   return
endif

; Fix starts...
print, ''
print, 'Fix starts ...'
print, ''

sbs=strupcase(c.sb(bl[pbl].isb))
bands=strupcase(c.band(sp[psl].iband))
recs=c.rec(bl(pbl).irec)
bls=c.blcd(bl(pbl).iblcd)
combinations=bls+' '+recs+' '+sbs+' '+bands
distinct_combinations=uti_distinct(combinations,ncombinations,/many_repeat)

ints = in[pil].int
pcls = pcl
pils = pil
psls = psl

ii=uti_distinct(in[pil].int,nint,/many_repeat)

i=0
tmp_idx = where(ints eq ii[i], ncombo)
print,'Fixing integration ',ii[i], " which has ", ncombo, " combinations"
ut=in[pils[[tmp_idx[0]]]].dhrs
ra=in[pils[[tmp_idx[0]]]].rar
dec=in[pils[[tmp_idx[0]]]].decr
uti_precess,ra,dec,2000,newepoch,/radian
sladcs2c,ra,dec,unitVector
if e.debug then print,ra,dec,' unitVector: ',unitVector

if e.debug then print,'ut= ',ut
uti_ut2lst,yr,mo,day,ut,lst,longitude=longitude
if e.debug then print,'lst= ',lst

dayFraction=ut/24.
;;   print,'dayFraction: ',dayFraction
;ha=lst-refRA*180.d/!dpi/15.d
; calculate diurnal correction for source
ha=lst-ra*180.d/!dpi/15.d
haRad=ha*15.d*!dpi/180.d
sidereal_rate=double(86400.)/double(86164.)
dayFraction0=dayFraction-haRad/(sidereal_rate*2.d*!dpi)

tTMinusUTC = slaDtt(mJD+dayFraction)/SECONDS_PER_DAY
tTMinusUTC0 = slaDtt(mJD+dayFraction0)/SECONDS_PER_DAY
;;   print,'tTMinusUTC = ',tTMinusUTC
tT = mJD+dayFraction + tTMinusUTC
tT0 = mJD+dayFraction0 + tTMinusUTC0
;;   print,'tT: ',tT
tDBMinusTT = slaRcc(mJD, dayFraction, globalRefLong, cos(globalRefLat)*globalRefRadius/M_PER_KM,sin(globalRefLat)*globalRefRadius/M_PER_KM)/SECONDS_PER_DAY
tDBMinusTT0 = slaRcc(mJD, dayFraction0, globalRefLong, cos(globalRefLat)*globalRefRadius/M_PER_KM,sin(globalRefLat)*globalRefRadius/M_PER_KM)/SECONDS_PER_DAY

;;   print,'tDBMinusTT: ',tDBMinusTT
tDB = tT + tDBMinusTT 
tDB0 = tT0 + tDBMinusTT0
;;   print,'tDB: ',tDB

; radial velocity in ref direction
souvRad=uti_vrad(ra,dec,lst,tDB)
if e.debug then print,'source radial velocity = ',souvRad
;souvRad=souvRad*slaDvdv(unitVector,refunitVector)
;if e.debug then print,'source radial velocity in reference source direction: ',souvRad
if keyword_set(diurnal) then refvRad=uti_vrad(ra,dec,ra*180.d/(15.d*!dpi),tDB0) else refvRad=uti_vrad(refRA,refDec,lst,tDB)
if e.debug then print,'ref source radial velocity: ',refvRad
fixVel=-1.*(souvRad-refvRad)/double(1000.)
;   print,'Velocity shifted by ',fixVel, ' km/s'
v0=fixVel
sp[psl].vel=sp[psl].vel+v0

for i=1L,nint-1L do begin
   
   tmp_idx = where(ints eq ii[i], ncombo)
   print,'Fixing integration ',ii[i], " which has ", ncombo, " combinations"

   ut=in[pils[[tmp_idx[0]]]].dhrs
   ra=in[pils[[tmp_idx[0]]]].rar
   dec=in[pils[[tmp_idx[0]]]].decr
   uti_precess,ra,dec,2000,newepoch,/radian
   sladcs2c,ra,dec,unitVector
   if e.debug then print,ra,dec,' unitVector: ',unitVector

   if e.debug then print,'ut= ',ut
   uti_ut2lst,yr,mo,day,ut,lst,longitude=longitude
   if e.debug then print,'lst= ',lst

   dayFraction=ut/24.
;;   print,'dayFraction: ',dayFraction
   tTMinusUTC = slaDtt(mJD+dayFraction)/SECONDS_PER_DAY
;;   print,'tTMinusUTC = ',tTMinusUTC
   tT = mJD+dayFraction + tTMinusUTC
;;   print,'tT: ',tT
   tDBMinusTT = slaRcc(mJD, dayFraction, globalRefLong, cos(globalRefLat)*globalRefRadius/M_PER_KM,sin(globalRefLat)*globalRefRadius/M_PER_KM)/SECONDS_PER_DAY
;;   print,'tDBMinusTT: ',tDBMinusTT
   tDB = tT + tDBMinusTT 
;;   print,'tDB: ',tDB

; radial velocity in ref direction
   souvRad=uti_vrad(ra,dec,lst,tDB)
   if e.debug then print,'source radial velocity = ',souvRad
;   souvRad=souvRad*slaDvdv(unitVector,refunitVector)
;   if e.debug then print,'source radial velocity in reference source direction: ',souvRad
   if not keyword_set(diurnal) then refvRad=uti_vrad(refRA,refDec,lst,tDB)
   if e.debug then print,'ref source radial velocity: ',refvRad
   fixVel=-1.*(souvRad-refvRad)/double(1000.)
   print,'Velocity shifted by ',fixVel, ' km/s'
   dv=fixvel-v0
   for j=0L,ncombo-1L do begin 
      nc=sp[psls[[tmp_idx[j]]]].nch
      vsh=dv/sp[psls[[tmp_idx[j]]]].vres
      data_ch=ch[pcls[[tmp_idx[j]]]:pcls[[tmp_idx[j]]]+nc-1]
      data_out=interpolate(data_ch,findgen(nc)-vsh, cubic=-0.5)
;      data_out=shift_spectrum(data_ch,nc,vsh)
      ch[pcls[[tmp_idx[j]]]:pcls[[tmp_idx[j]]]+nc-1]=data_out
   endfor
endfor


result=dat_list(s_l,/reset)
end   


    


