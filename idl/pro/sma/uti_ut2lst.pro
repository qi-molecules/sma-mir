pro uti_ut2lst, year, month, day, ut, lst, longitude=longitude
if not keyword_set(longitude) then longitude=-155.477522d ; sma longitude
;day=day-1
da=367.*year-fix(7.*(year+fix((month+9.)/12.))/4.)+fix(275.*month/9.)+day-730530.
n=fix( (98.9818  +  0.985647352d * (da+1.) + 24.*15. + longitude)/360. )-1
lst=(ut*(0.985647352d/24. + 15.)+longitude+0.985647352d*da+98.9818-360.*n)/15.
;print,'LST =',sixty(lst-24)
if lst ge 24. then lst=lst-24.
end

