pro uti_erot_fix,sign=sign,force=force,source=source
common global
common data_set

juldat_stop=uti_jul_day(4,29,2005)+0.d0
juldat_start=uti_jul_day(12,10,2004)+0.d0

months = ['Jan','Feb','Mar','Apr','May','Jun','Jul', $
          'Aug','Sep','Oct','Nov','Dec']
datobs=c.ref_time[in[pil[0]].iref_time]
mon=strmid(datobs,0,3)
j=where(mon eq months,count)
num_jdref=[fix(strtrim(strmid(datobs,8,4),2)), $
     fix(strtrim(string(j[0]+1),2)),(strtrim(strmid(datobs,4,2),2))]
juldat_day0 =  uti_jul_day(num_jdref[1],num_jdref[2],num_jdref[0]) + 0.d0
if (not keyword_set(force)) and ((juldat_day0 gt juldat_stop) $
  or (juldat_day0 lt juldat_start)) then begin
   print,' The data was taken on ',c.ref_time
   print,' There is no need to correct Earth rotation for data'
   print,'    taken before Dec 10, 2004 or after April 29, 2005.'
   print,' No correction done yet !'
   print,' Otherwise, use uti_erot_fix ,/force to force the correction.'
   return
endif

if keyword_set(sign) then sign=1. else sign=-1.

distinct_source=uti_distinct(c.source[in[pil].isource],nsources,/many_repeat)

if not keyword_set(source) and nsources gt 1 then begin
   print, ' Multiple sources filtered.'
   print, ' Please use SOURCE keyword to select the main source '
   print, '     on which dopplerTrack was issued, e.g.'
   print, "        IDL> uti_erot_fix, source='HH212_1'"
   print, ' No action done !'
   return
endif
   

day=86164.d
radius=6.382248e6
lat=double(19.82420526391d*!pi/180.d)
Vel0=2*!pi*radius*cos(lat)/day/1000.d
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

if not keyword_set(source) then source=distinct_source[0]
if nsources gt 1 then begin
   result=dat_list(s_l,'"source" eq "'+source+'"',/no_notify,/reset)
   source_time=in[pil].dhrs
   source_ha=in[pil].ha
   coeffs=linfit(source_time,source_ha,/double)
   source_dec=in[pil].decr
endif

for i=0L,nint-1L do begin

   tmp_idx = where(ints eq ii[i], ncombo)

   print,'Fixing integration ',ii[i], " which has ", ncombo, " combinations"

   for j=0L,ncombo-1L do begin 

      if (nsources eq 1) then begin
        h=in[pils[[tmp_idx[j]]]].ha*15.d*!pi/180.d
        dec=in[pil[[tmp_idx[j]]]].decr
      endif else begin
        t=in[pil[[tmp_idx[j]]]].dhrs
        h=t*coeffs[1]+coeffs[0]
        h=h*15.d*!pi/180.d 
        dec=source_dec[0]
;        dec=interpol(source_dec,source_time,t)
      endelse
      fixVel=Vel0*sin(h)*cos(dec)
      nc=sp[psls[[tmp_idx[j]]]].nch
      sh=fixVel/sp[psls[[tmp_idx[j]]]].vres
      vsh=sign*sh
      data_ch=ch[pcls[[tmp_idx[j]]]:pcls[[tmp_idx[j]]]+nc-1]
      data_out=shift_spectrum(data_ch,nc,vsh)
      ch[pcls[[tmp_idx[j]]]:pcls[[tmp_idx[j]]]+nc-1]=data_out
   endfor
endfor

result=dat_list(s_l,/reset)
end   
