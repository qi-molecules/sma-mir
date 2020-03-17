pro uti_uvw_fix
common global
common data_set

; check newepoch for apparent dec
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

;lat=double(19.82420526391d/57.29577951)
lat=double(19.82420526391d*!dpi/180.d)
m1=[[-sin(lat),0.d,cos(lat)],[0.d,1.d,0.d],[cos(lat),0.d,sin(lat)]]
sbs=strupcase(c.sb(bl(pbl).isb))
bls=c.blcd(bl(pbl).iblcd)
recs=c.rec(bl[pbl].irec)
combinations=bls+' '+sbs+' '+recs
distinct_combinations=uti_distinct(combinations,ncombinations,/many_repeat)
ii=uti_distinct(in[pil].int,nint,/many_repeat)
for i=0L,nint-1L do begin
   s_int=strcompress(string(ii[i]),/remove_all)
   if (i eq (i/100)*100) then print,'Fixing uvw coord in integration # from '+s_int+' to '+strcompress(string( ii[(((i/100+1)*100)<nint-1)] ),/remove_all)
   result=dat_list(s_l,'"int" eq "'+s_int+'"',/no_notify,/reset)
   result=dat_list(s_l,/save,/no_notify)
;   print,'Fixing integration #:',s_int
   for j=0L,ncombinations-1L do begin
      result=dat_list(s_l,/restore,/no_notify)
      result=dat_comb_sep(distinct_combinations[j],['blcd','sb','rec'], $
        codes,icodes,n_components)
      result=dat_list(s_l,'"blcd" eq "'+codes(0)+ $
        '" and "rec" eq "'+codes[2] + $
        '" and "sb" eq "'+codes(1)+ '"',/no_notify)
      if n_elements(pcl) ge 1 and result gt 0 then begin
         h=in[pil[0]].ha*15.d*!dpi/180.d
         dec=in[pil[0]].decr
         ra=in[pil[0]].rar
         uti_precess,ra,dec,2000,newepoch,/radian
         if tag_exist(c,'filever') then begin
            if min([fix(c.filever)]) ge 3 then begin
               ;print,'calculated adec is ',dec
               dec=in[pil[0]].adec
               ;print,'data adec is ',dec
            endif
         endif               

         m2=[[sin(h),cos(h),0],[-sin(dec)*cos(h),sin(dec)*sin(h),cos(dec)],$
           [cos(dec)*cos(h),-cos(dec)*sin(h),sin(dec)]]
         neu=[bl[pbl[0]].bln,bl[pbl[0]].ble,bl[pbl[0]].blu]
         neu=transpose(neu)
;         uvw=reform(m2##m1##neu)/(1000.d*0.299792458d/sp[psl[0]].fsky)
;         print,uvw
         klam=!cvel/sp[psl[0]].fsky/1e6
         uvw=reform(m2##m1##neu)/klam
         bl[pbl].u=uvw[0]
         bl[pbl].v=uvw[1]
         bl[pbl].w=uvw[2]
         bl[pbl].prbl=sqrt(bl[pbl].u*bl[pbl].u+bl[pbl].v*bl[pbl].v)
      endif
   endfor
endfor
result=dat_list(s_l,/reset)
end

