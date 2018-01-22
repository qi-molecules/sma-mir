pro uti_if2vel_fix, ref=ref, force=force, fvalues=fvalues

common global
common data_set

juldat_stop=uti_jul_day(11,27,2009)+0.d0

months = ['Jan','Feb','Mar','Apr','May','Jun','Jul', $
          'Aug','Sep','Oct','Nov','Dec']
datobs=c.ref_time[in[pil[0]].iref_time]
mon=strmid(datobs,0,3)
j=where(mon eq months,count)
num_jdref=[fix(strtrim(strmid(datobs,8,4),2)), $
     fix(strtrim(string(j[0]+1),2)),(strtrim(strmid(datobs,4,2),2))]
juldat_day0 =  uti_jul_day(num_jdref[1],num_jdref[2],num_jdref[0]) + 0.d0
if ((not keyword_set(force)) and (juldat_day0 gt juldat_stop)) then begin
   print,' The data was taken on ',c.ref_time
   print,' There is no need to fix S25-S48 velocity header for data'
   print,'    taken after November 28, 2009.'
   print,' No fix done yet !'
   print," Otherwise, use 'uti_if2vel_fix ,/force' to force the correction."
   return
endif

if not keyword_set(fvalues) then begin

if not keyword_set(ref) then ref='s01'

fu0=0
fl0=0

command='"sb" eq "u" and "band" eq "'+ref+'"'
result=dat_list(s_l,command,/no_notify,/reset)
if result gt 0 then begin
   fu0=sp[psl[0]].fsky
   vu0=sp[psl[0]].vel
   fus=fu0/sqrt((double(1.)-vu0*1000.d/!cvel)/(double(1.)+vu0*1000.d/!cvel))
endif
command='"sb" eq "l" and "band" eq "'+ref+'"'
result=dat_list(s_l,command,/no_notify,/reset)
if result gt 0 then begin
   fl0=sp[psl[0]].fsky
   vl0=sp[psl[0]].vel
   fls=fl0/sqrt((double(1.)-vl0*1000.d/!cvel)/(double(1.)+vl0*1000.d/!cvel))
endif

endif else begin

if n_elements(fvalues) ne 4 then return
fu0=fvalues[0]
vu0=fvalues[1]
fl0=fvalues[2]
vl0=fvalues[3]
fus=fu0/sqrt((double(1.)-vu0*1000.d/!cvel)/(double(1.)+vu0*1000.d/!cvel))
fls=fl0/sqrt((double(1.)-vl0*1000.d/!cvel)/(double(1.)+vl0*1000.d/!cvel))
endelse


for i=25,48 do begin
   band='s'+strcompress(string(i),/remove_all)
   command='"sb" eq "u" and "band" eq "'+band+'"'
   result=dat_list(s_l,command,/no_notify,/reset)
;   if result gt 0 and (keyword_set(force) or $
;     finite(sp[psl[0]].vel) eq 0) then begin
   if result gt 0 then begin
      if fu0 eq 0 then begin
         print, 'Inappropriate reference band !'
         print, 'Quit !'
         return
      endif
      fu1=sp[psl[0]].fsky
      alpha=(fu1/fus)^2.
      vu1=-(alpha-double(1.))*!cvel/(1000.d*(alpha+double(1.)))
      sp[psl].vel=vu1
   endif

   command='"sb" eq "l" and "band" eq "'+band+'"'
   result=dat_list(s_l,command,/no_notify,/reset)
;   if result gt 0 and (keyword_set(force) or $
;     finite(sp[psl[0]].vel) eq 0) then begin
   if result gt 0 then begin
      if fl0 eq 0 then begin
         print, 'Inappropriate reference band !'
         print, 'Quit !'
         return
      endif
      fl1=sp[psl[0]].fsky
      alpha=(fl1/fls)^2.
      vl1=-(alpha-double(1.))*!cvel/(1000.d*(alpha+double(1.)))
      sp[psl].vel=vl1
   endif

endfor

result=dat_list(s_l,/reset)
if not keyword_set(fvalues) then print, 'IF2 bands velocity headers fixed using band '+ref+' !' else print, 'Done !'

end
