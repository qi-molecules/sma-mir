pro uti_ha_fix, uvwfix=uvwfix
common global
common data_set

long=-155.4775 ; sma longitude
months = ['Jan','Feb','Mar','Apr','May','Jun','Jul', $
          'Aug','Sep','Oct','Nov','Dec']
datobs=c.ref_time[in[pil[0]].iref_time]
mon=strmid(datobs,0,3)
j=where(mon eq months,count)
if count le 0 then begin
  print,"couldn't decode UT date in data (",c.ref_time[in[pil[0]].iref_time],") !"
  return
endif
num_day_obs=[fix(strtrim(strmid(datobs,8,4),2)), $
     fix(strtrim(string(j[0]+1),2)),(strtrim(strmid(datobs,4,2),2))]
day=strtrim(string(num_day_obs[2]),2) 
year =strtrim(string(num_day_obs[0]),2)
month =strtrim(string(num_day_obs[1]),2)

da=367.*year-fix(7.*(year+fix((month+9.)/12.))/4.)+fix(275.*month/9.)+day-730530.
n=fix( (98.9818  +  0.985647352d * (da+1.) + 24.*15. + long)/360. )-1
lst=(in[pil].dhrs*(0.985647352d/24. + 15.)+long+0.985647352d*da+98.9818-360.*n)/15.
ii=where(lst ge 24.,count)
if count gt 0 then lst[ii]=lst[ii]-24.

in[pil].ha=lst-in[pil].rar*12.0/!PI

if keyword_set(uvwfix) then begin 
   uti_uvw_fix 
   print, 'Done! HA header fixed !'
   print, 'UVW header also fixed !'
endif else begin
   print,'Done! HA header fixed !'
   print,'You might also want to fix uvw with UTI_UVW_FIX !'
endelse

end
