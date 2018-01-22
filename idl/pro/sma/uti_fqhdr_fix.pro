pro uti_fqhdr_fix, force=force
common global
common data_set

juldat_stop=uti_jul_day(11,27,2007)+0.d0
months = ['Jan','Feb','Mar','Apr','May','Jun','Jul', $
          'Aug','Sep','Oct','Nov','Dec']
datobs=c.ref_time[in[pil[0]].iref_time]
mon=strmid(datobs,0,3)
j=where(mon eq months,count)
num_jdref=[fix(strtrim(strmid(datobs,8,4),2)), $
     fix(strtrim(string(j[0]+1),2)),(strtrim(strmid(datobs,4,2),2))]
juldat_day0 =  uti_jul_day(num_jdref[1],num_jdref[2],num_jdref[0]) + 0.d0

if (not keyword_set(force)) and (juldat_day0 gt juldat_stop) then begin
   print,' The data was taken on ',c.ref_time
   print,' There is no need to correct frequency header for data '
   print,' taken after November 27, 2007.'
   print,' No action taken !'
   print,' Otherwise, use uti_fqhdr_fix,/force to force the fix.'
   return
endif

result=dat_filter(s_f,'"band" eq "s01" or  "band" eq "s02" or  "band" eq "s05" or  "band" eq "s06" or  "band" eq "s09" or  "band" eq "s10" or  "band" eq "s13" or  "band" eq "s14" or  "band" eq "s17" or  "band" eq "s18" or  "band" eq "s21" or  "band" eq "s22"',/no_notify,/reset)
;select,/p,/re,band=['s01','s02','s05','s06','s09','s10','s13','s14','s17','s18','s21','s22']
if result gt 0 then begin
   sp[psf].fsky = sp[psf].fsky + 5.e-4*sp[psf].fres
   sp[psf].vel = sp[psf].vel + 5.e-1*sp[psf].vres
endif

result=dat_filter(s_f,'"band" eq "s03" or  "band" eq "s04" or  "band" eq "s07" or  "band" eq "s08" or  "band" eq "s11" or  "band" eq "s12" or  "band" eq "s15" or  "band" eq "s16" or  "band" eq "s19" or  "band" eq "s20" or  "band" eq "s23" or  "band" eq "s24"',/no_notify,/reset)
;select,/p,/re,band=['s03','s04','s07','s08','s11','s12','s15','s16','s19','s20','s23','s24']
if result gt 0 then begin
   sp[psf].fsky = sp[psf].fsky - 5.e-4*sp[psf].fres
   sp[psf].vel = sp[psf].vel - 5.e-1*sp[psf].vres
endif

print,'Half-channel frequency header corrected !'

result=dat_filter(s_f,/reset)
end


