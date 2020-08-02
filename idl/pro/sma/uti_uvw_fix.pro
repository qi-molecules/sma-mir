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
a0=pil & a1=pbl & a2=psl
for i=0, nbls-1 do begin
   blcd=distinct_bls[i]
   ibl=min(where(c.blcd eq blcd))
;   print,'baseline:',blcd
   for j=0, nrecs-1 do begin
      rec=distinct_recs[j]
      irx=min(where(c.rec eq rec))
;      print,'receiver:',rec
      for k=0, nsbs-1 do begin
         sb=distinct_sbs[k]
         isbd=min(where(c.sb eq strlowcase(sb)))
;         print,'sideband:',sb
         n=where( (bl[a1].irec eq irx) and (bl[a1].isb eq isbd) and (bl[a1].iblcd eq ibl), count)
         if count gt 0 then begin
            b0=a0[n] & b1=a1[n] & b2=a2[n]
            h=in[b0[0]].ha*15.d*!dpi/180.d
            dec=in[b0[0]].decr
            ra=in[b0[0]].rar
            uti_precess,ra,dec,2000,newepoch,/radian
            if min([fix(c.filever)]) ge 3 then dec=in[b0[0]].adec ;adec
            m2=[[sin(h),cos(h),0],[-sin(dec)*cos(h),sin(dec)*sin(h),cos(dec)],$
                [cos(dec)*cos(h),-cos(dec)*sin(h),sin(dec)]]
            neu=[bl[b1[0]].bln,bl[b1[0]].ble,bl[b1[0]].blu]
            neu=transpose(neu)
;            klam=!cvel/sp[b2[0]].fsky/1e6
;            uvw=reform(m2##m1##neu)/klam

            for m =0L, nint-1L do begin
               s_int=strcompress(string(ii[m]),/remove_all)
               if (m eq (m/100)*100) then print,'Fixing uvw coord in integration # from '+s_int+' to '+strcompress(string( ii[(((m/100+1)*100)<nint-1)] ),/remove_all)

               n=where( in[b0].int eq ii[m], count)
               if count eq 0 then goto, jump2
               c0=b0[n] & c1=b1[n] & c2=b2[n]
               klam=!cvel/sp[c2[0]].fsky/1e6
               h=in[c0[0]].ha*15.d*!dpi/180.d
               dec=in[c0[0]].adec ; adec
               m2=[[sin(h),cos(h),0],[-sin(dec)*cos(h),sin(dec)*sin(h),cos(dec)],$
                      [cos(dec)*cos(h),-cos(dec)*sin(h),sin(dec)]]
               neu=[bl[c1[0]].bln,bl[c1[0]].ble,bl[c1[0]].blu]
               neu=transpose(neu)
               uvw=reform(m2##m1##neu)/klam
               bl[c1].u=uvw[0]
               bl[c1].v=uvw[1]
               bl[c1].w=uvw[2]
               bl[c1].prbl=sqrt(bl[c1].u*bl[c1].u+bl[c1].v*bl[c1].v)  

               jump2:                        
            endfor              ; integration
         endif                  ; count
      endfor                    ; sbs
   endfor                       ; recs
endfor                          ; baselines 

result=dat_list(s_l,/reset)
end

