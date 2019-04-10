pro uti_hayshft_fix, reference=reference, raref=raref, decref=decref, flipsign=flipsign

common global
common data_set

sign=-1.
if keyword_set(flipsign) then sign=1.

; Parameters
SECONDS_PER_DAY=24.*60.*60.
globalRefLong=155.477522d*!dpi/180.d
globalRefLat=19.82420526391d*!dpi/180.d
globalRefRadius=6382.248*1000.d
M_PER_KM=1000.d
longitude=-155.477522d
lat_hay=42.623d ; haystack latitude

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

datatime=mJD+in[pil[0]].dhrs/24.
; load reference ra and dec from engineering table hayshft_lookup.txt
; only available between 2011 April 4 to 2019 April 10

file=e.IDL_PRO+'sma/hayshft_lookup.txt'
nlines=file_lines(file)
temp=dblarr(4,nlines)
openr, unit, file,/get_lun
readf,unit,temp
close, unit & free_lun,unit
time1=reform(temp[0,*])
time2=reform(temp[1,*])
ratab=reform(temp[2,*])
dectab=reform(temp[3,*])
i=where(datatime ge time1 and datatime le time2, count)
if count ge 1 then begin
   reflag=1
   refRA=ratab[i[0]]
   refDec=dectab[i[0]]
endif else reflag=0

if not reflag then begin
   if not (keyword_set(reference) or keyword_set(raref) or keyword_set(decref)) then begin 
      print, 'Please use REFERENCE keyword to select the source '
      print, '       on which original dopplerTrack was issued, e.g.'
      print, "       IDL> uti_doppler_fix, reference='HH212'  "
      print, '    or use RAREF and DECREF keyword to set the '
      print, '       apparent RA and Dec (in radians) of the '
      print, '       dopplerTracked source, e.g. '
      print, '       IDL> uti_doppler_fix,raref=3.27054, decref=0.03498'
      print, '       which is tracking 3c273 on Feb 12, 2008.'
      print, 'Quit !'
      return
   endif

   if n_elements(reference) gt 1 then begin
      print, 'Reference source number must be one !'
      print, 'Quit !'
      return
   endif

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
endif

; Fix starts...
print, ''
print, 'Fix starts ...'
print, ''

result=dat_list(s_l,/reset,/no_notify)

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

for i=0L,nint-1L do begin
   
   tmp_idx = where(ints eq ii[i], ncombo)
   print,'Fixing integration ',ii[i], " which has ", ncombo, " combinations"

   ut=in[pils[[tmp_idx[0]]]].dhrs
   ra=in[pils[[tmp_idx[0]]]].rar
   dec=in[pils[[tmp_idx[0]]]].decr
   uti_precess,ra,dec,2000,newepoch,/radian
;   sladcs2c,ra,dec,unitVector
;   if e.debug then print,ra,dec,' unitVector: ',unitVector

   if e.debug then print,'ut= ',ut
   uti_ut2lst,yr,mo,day,ut,lst,longitude=longitude
   if e.debug then print,'lst= ',lst


   ha=lst-refRA*180.d/!dpi/15.d
;   print,'ha_sma is ',ha
   haRad=ha*15.d*!dpi/180.d

; assume ha here is the same as ha_sma
   ha_hay=ha+5.+25./60.
;   print,'ha_hay is ',ha_hay

   sh_hay=globalRefRadius*2.*!DPI*cos(!DPI*lat_hay/180.)*cos(refDec)*sin(!DPI*ha_hay/12.)/(24.*3600.)

   sh_sma=globalRefRadius*2.*!DPI*cos(globalRefLat)*cos(refDec)*sin(haRad)/(24.*3600.)

   dv=sign*(sh_sma-sh_hay)/double(1000.)
   print, 'Velocity shifted by ',dv, ' km/s'
   ; will convert to frequency shifted based on restfreq
   for j=0L,ncombo-1L do begin 
      nc=sp[psls[[tmp_idx[j]]]].nch
;      vsh=dv/sp[psls[[tmp_idx[j]]]].vres
      fsh=-1.*dv*sp[psls[[tmp_idx[j]]]].rfreq*1e6/!cvel/sp[psls[[tmp_idx[j]]]].fres
      data_ch=ch[pcls[[tmp_idx[j]]]:pcls[[tmp_idx[j]]]+nc-1]
      data_out=interpolate(data_ch,findgen(nc)-fsh, cubic=-0.5)
;      data_out=interpolate(data_ch,findgen(nc)-vsh, cubic=-0.5)
;      data_out=shift_spectrum(data_ch,nc,vsh)
      ch[pcls[[tmp_idx[j]]]:pcls[[tmp_idx[j]]]+nc-1]=data_out
   endfor
endfor


result=dat_list(s_l,/reset)
end   


    


