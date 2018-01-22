function bas_sol,sn_limit=sn_limit,dt_limit=dt_limit,fitbox=fitbox, $
         axint=axint,reftel=reftel,search_param=search_param

;
; Solves for telescope baseline solution
;
;
; eg. result = bas_sol()
;
; 
;
common global
common data_set
common plo
common baseline,tenlat,ntel,ref_tel,tel1,tel2,dist_tel,dist_tel1, $
         dist_tel2,nprimary,sec_bl,sec_sign,dist_blcd,dist_iblcd,pri_bl, $
         dist_blcd_tel1,dist_blcd_tel2,avthbl,dthdelaybl,utdts, $
         nsecondary,dthlobl,dthdelaytel,dthlotel,nbase,nobs,npair, $
         i1p,i2p,php,blcdp,sbp,cspa,cspb,csxpa,csxpb,csypa,csypb, $
         cszpa,cszpb,celpa,celpb,sigp,wtp,phaave,blcd,sb,band,sx,sy,sz, $
         cels,e0,neast,de,n0,nnorth,dn,u0,nup,du,a0,naxis,da,nmin, $
         chisq,bsol,ixp,slat,clat,cont,tha


tenlat = 37.23405556d0/!radeg
clat = cos(tenlat)
slat = sin(tenlat)
npar_tel=5
tol=1.d-03
threshold=1.3
if not keyword_set(sn_limit) then sn_limit=2.
if not keyword_set(dt_limit) then dt_limit=15.
if not keyword_set(fitbox) then fitbox=50.
if not keyword_set(axint) then axint=0
if not keyword_set(reftel) then reftel=1
ref_tel=reftel
if not keyword_set(search_param) then begin
  tel=['1','2','3','4','5','6']
  epa=[0,0,0,0,0,0]
  tde=[20,20,20,20,20,20]
  npa=[0,0,0,0,0,0]
  tdn=[20,20,20,20,20,20]
  upa=[0,0,0
pbl].itel2]
dist_tel1=uti_disti,0,0,0]
  tdu=[5,5,5,5,5,5]
  apa=[0,0,0,0,0,0]
  tda=[0,0,0,0,0,0]
  param={tel:tel,e:epa,tde:tde,n:npa,tdn:tdn,u:upa,tdu:tdu,a:apa,tda:tda}
endif
if not keyword_set(axint) then param.tda=0.d0

res=dat_list(s_l,'"bq" eq "b" and "csnr" ge "'+ $
        strtrim(string(sn_limit),2)+'" and "wt" gt "0."',/reset,/no_notify)
dist_band=uti_distinct(c.band[sp[psl].iband],nband,/many_repeat)

for iband=0,nband-1 do begin
band=dist_band[iband]
res=dat_list(s_l,'"bq" eq "b" and "csnr" ge "'+ $
        strtrim(string(sn_limit),2)+'" and "wt" gt "0." and '+ $
        '"band" eq "'+band+'"',/reset,/no_notify)
sort=sort(strtrim(string(bl[pbl].iblcd),2)+strtrim(string(bl[pbl].isb),2)+ $
          strtrim(string(in[pil].inhid),2))
pil=pil[sort]
pbl=pbl[sort]
psl=psl[sort]
blcd=c.blcd[bl[pbl].iblcd]
sb=c.sb[bl[pbl].isb]
dist_sb=uti_distinct(c.sb[bl[pbl].isb],nsb,/many_repeat)
dist_inhid=uti_distinct(in[pil].inhid,ninhid,/many_repeat)
iblcds=bl[pbl].iblcd
dist_iblcd=uti_distinct(iblcds,nbase,/many_repeat)
dist_blcd=c.blcd[dist_iblcd]
tel1=c.tel1[bl[pbl].itel1]
tel2=c.tel2[bl[pbl].itel2]
dist_tel1=uti_distinct(tel1,ntel1,/many_repeat)
dist_tel2=uti_distinct(tel2,ntel2,/many_repeat)
;dist_tel2=uti_distinct(tel2, $
;          many_distincmpleneast,=1,nte
;l2)
dinobs*(lindgencmpleneaste/nid)+make_array(neast,/int,value=1)##idd, $
          manydistinct=0,ample=1,ntel)
nprimary=ntel-1 
sec_bl=make_array(nprimary,5,3,/int)
sec_sign=make_array(nprimary,5,3,/int)
dist_blcd_tel1=make_array(nbase,/string)
dist_blcd_tel2=make_array(nbase,/string)
pri_bl=make_array(ntel-1,/int)
nsecondary=make_array(ntel-1,/int)
avthbl=make_array(3,nbase,/double) 
dthdelaybl=make_array(2,nbase,/double)
dthlobl=make_array(2,nbase,/double)
dthdelaytel=make_array(2,ntel,/double)
dthlotel=make_array(2,ntel,/double)

for i=0,nbase-1 do begin
  j=where(iblcds eq dist_iblcd[i])
  dist_blcd_tel1[i]=tel1[j[0]]
  dist_blcd_tel2[i]=tel2[j[0]]
endfor

maxtraid=max(in[pil].traid)
nobs=n_elements(pil)

nbsol=make_array(3,nbase,/long)
good=make_array(3,nbase,/long)
good_data=make_array(3,nbase,/long)
sig=make_array(3,nbase,5,/double)
bsol=make_array(3,nbase,25,5,/double)
tsol=make_array(3,6,6,/double)
ddcos=cos(dindgen(360)/!radeg)
ddsin=sin(dindgen(360)/!radeg)

print,maxtraid,band,nobs,nsb,nbase, $
    format='(//1h ," solution for tra# : ",i5," band ",a,/1h ,i5,'+ $
         '" observations :",i2," sidebands, ",i2," baselines")'

utdts=bl[pbl].avedhrs*60.
utdiff=bl[pbl].avedhrs- $
    uti_date_dif(c.ut[in[pil].iut],c.ref_time[in[pil].iref_time], $
            units='hr',out='date_diff')
fsky=sp[psl].fsky
phaave=bl[pbl].phaave
csnr=bl[pbl].csnr
ble=bl[pbl].ble
bln=bl[pbl].bln
blu=bl[pbl].blu
sb=c.sb[bl[pbl].isb]
band=c.band[sp[psl].iband]



;
; Correct pointing for time difference between start and median time scan
;
 osx = in[pil].sx
 osy = in[pil].sy
 dha = utdiff * !pi / 12.d0 

 sx = osx * cos(dha) + osy * sin(dha)
 sy = osy * cos(dha) - osx * sin(dha)
 sz = in[pil].sz
 j=where(phaave lt 0.d0,count_neg)
 if count_neg gt 0 then  phaave[j] = phaave[j] + 360.d0
     
 freqmax = max(fsky)
 flo = fsky - !if_freq
 j=where(sb eq 'l',count_l)
 if count_l gt 0 then flo[j]=fsky[j] + !if_freq
 wlmm = 299.79250D0 / fsky
 wl = wlmm / 1000.d0
 cs=360.d0/wl

 sx=sx*cs
 sy=sy*cs
 sz=sz*cs

 phaave=((phaave+3600.d0-180.d0) mod 360.d0)-180.d0 
 cels = cos(in[pil].el/!radeg)*cs
 print,sx[0:10]
 csnr= (csnr > 1.d0)
 sigtha= (80.d0/!radeg)/csnr
 wts=(csnr < 20.d0)
 wts=sp[psl].wt*wts
 conts=make_array(nobs,/int,value=1)
 j=where((abs(utdts(1+indgen(nobs-1))-utdts(indgen(nobs-1))) gt dt_limit) $
     or (blcd(1+indgen(nobs-1)) ne blcd(indgen(nobs-1))) $
     or (sb(1+indgen(nobs-1)) ne sb(indgen(nobs-1))),count)
 if count gt 0 then conts[j]=0
;
; initailize closure triangles
;
 if bas_sol_clo_ini(iobs,1,0) eq 0 then return,0
;
;  group pairs of observations within dt < dtlimit
;
 if bas_sol_group(dt_limit) eq 0 then return,0
;
;    normalize wt's to values 10 => 100
;
 wts = 100.d0*((wts/max(wts)) > 0.1)
 sigp = sqrt(1.d0/(wts*wts))

 i
----------
X-Sun-Data-Type: default
X-Sun-Data-Description: default
X-Sun-Data-Name: bas_sol_clo_ini.pro
X-Sun-Content-Lines: 169
X-Sun-Charset: us-ascii

function bas_sol_clo_ini,iobs,icall,ist

;
;  initializes the arrays specifying secondary routes to each of the 
;  non-reference telescopes, via one other telescope
;  and calculates sb and tel based phase offsets
;
; eg   res=bas_sol_clo_ini(iobs,icall,ist)
;

common global
common data_set
common plo
common baseline,tenlat,ntel,ref_tel,tel1,tel2,dist_tel,dist_tel1, $
         dist_tel2,nprimary,sec_bl,sec_sign,dist_blcd,dist_iblcd,pri_bl, $
         dist_blcd_tel1,dist_blcd_tel2,avthbl,dthdelaybl,utdts, $
         nsecondary,dthlobl,dthdelaytel,dthlotel,nbase,nobs,npair, $
         i1p,i2p,php,blcdp,sbp,cspa,cspb,csxpa,csxpb,csypa,csypb, $
         cszpa,cszpb,celpa,celpb,sigp,wtp,phaave,blcd,sb,band,sx,sy,sz, $
         cels,e0,neast,de,n0,nnorth,dn,u0,nup,du,a0,naxis,da,nmin, $
         chisq,bsol,ixp,slat,clat,cont,tha

;
;  secbl : list of basline id's contributing to each secondary path
;  secsi : sign with which to include baseline in tel. offset sum
;
if icall eq 1 then begin
   nprimary = 0   
   if ntel lt 2 then return,0
   j=where((dist_blcd_tel2 ne ref_tel and dist_blcd_tel1 eq ref_tel) $
        or (dist_blcd_tel2 eq ref_tel and dist_blcd_tel1 ne ref_tel),nprimary)
   pri_bl=dist_iblcd[j]
   for i=0,nprimary-1 do begin
     print,pri_bl[i]+1,dist_blcd[pri_bl[i]], $
           format='("primary baseline ",i2,":   ",a)'
   endfor
   bas_tels=make_array(nbase,2,/int)
;
; bas_tels[j,*] lists the tel ids for baseline j
; sec_bl lists all possible secondary baselines
;
   for j=0,nbase-1 do bas_tels[j,*]=[dist_blcd_tel1[j],dist_blcd_tel2[j]]
   for i=0,nprimary-1 do begin
     pt1=bas_tels[pri_bl[i],0] & pt2=bas_tels[pri_bl[i],1]
;
; list of possible sec tels in dist_tel[st]
;
     st=where(dist_tel ne pt1 and dist_tel ne pt2,cst)
     if cst gt 0 then begin
       for j=0,cst-1 do begin
         pt3=dist_tel[st[j]]
         j13=where(bas_tels[*,0] eq  pt1 and bas_tels[*,1] eq pt3,c13)
         j31=where(bas_tels[*,0] eq  pt3 and bas_tels[*,1] eq pt1,c31)
         j23=where(bas_tels[*,0] eq  pt2 and bas_tels[*,1] eq pt3,c23)
         j32=where(bas_tels[*,0] eq  pt3 and bas_tels[*,1] eq pt2,c32)
         if c13 eq 1 and c23 eq 1 then begin
           nsecondary[i]=nsecondary[i]+1
           sec_bl(i,nsecondary[i]-1,0)=dist_iblcd[j13]
           sec_sign(i,nsecondary[i]-1,0)=1.d0
           sec_bl(i,nsecondary[i]-1,1)=dist_iblcd[j23]
           sec_sign(i,nsecondary[i]-1,1)=-1.d0
         endif
         if c13 eq 1 and c32 eq 1 then begin
           nsecondary[i]=nsecondary[i]+1
           sec_bl(i,nsecondary[i]-1,0)=dist_iblcd[j13]
           sec_sign(i,nsecondary[i]-1,0)=1.d0
           sec_bl(i,nsecondary[i]-1,1)=dist_iblcd[j32]
           sec_sign(i,nsecondary[i]-1,1)=1.d0
         endif
         if c31 eq 1 and c23 eq 1 then begin
           nsecondary[i]=nsecondary[i]+1
           sec_bl(i,nsecondary[i]-1,0)=dist_iblcd[j31]
           sec_sign(i,nsecondary[i]-1,0)=1.d0
           sec_bl(i,nsecondary[i]-1,1)=dist_iblcd[j23]
           sec_sign(i,nsecondary[i]-1,1)=-1.d0
         endif
         if c31 eq 1 and c32 eq 1 then begin
           nsecondary[i]=nsecondary[i]+1
           sec_bl(i,nsecondary[i]-1,0)=dist_iblcd[j31]
           sec_sign(i,nsecondary[i]-1,0)=-1.d0
           sec_bl(i,nsecondary[i]-1,1)=dist_iblcd[j32]
           sec_sign(i,nsecondary[i]-1,1)=1.d0
         endif
       endfor
     endif
   endfor

print,ref_tel,format='(/1h ,"available secondary baseline routes to each '+$
                 'non-reference telescope (ref=",i2,")",/1h )'
for i1=0,nprimary-1 do begin 
 print,dist_blcd_tel1(pri_bl(i1)),dist_blcd_tel2(pri_bl(i1)), $
       format='(1h ,"baseline (",i1,"-",i1,")")'
 for i2=0,nsecondary(i1)-1 do begin 
    c1=' ' & c2='+'
    if sec_sign(i1,i2,0) lt 0 then c1='-'
    if sec_sign(i1,i2,1) lt 0 then c2='-'
    print,i2,c1,dist_blcd[sec_bl[i1,i2,0]],c2,dist_blcd[sec_bl[i1,i2,1]], $
      format='(1h ,"   sec. route #",i2,": ",a1,"(",a,")",1x,a1,1x,"(",a,")")'
  endfor
endfor
endif

if icall eq 2 then begin
  dthdelaybl(ist,*)=0.d0
  dthlobl(ist,*)=0.d0
  dthdelaytel(ist,*)=0.d0
  dthlotel(ist,*)=0.d0
if ntel lt 2 or nsb eq 1 then return,1

;
;  compute telescope-based usb-lsb phase errors due to delays and lo
;
       i1=0
       pi=3.14159265
       degra=1.d0/!radeg
       r2d=180./pi
       ic=complex(0.0,1.0)

for i1=0,nprimary-1 do begin
  it2=dist_blcd_itel2(pribl(i1))
         i=0
  csum1=complex(0.0,0.0)
  csum2l=complex(0.0,0.0)
  csum2u=complex(0.0,0.0)
  csum1=csum1+1.*exp( ic*complex(degra* $
             (avthbl(1,pribl(i1))-avthbl(0,pribl(i1)))))
  csum2l=csum2l+1.*exp( ic*complex(degra* $
             (avthbl(0,pribl(i1)))))
  csum2u=csum2u+1.*exp( ic*complex(degra* $
             (avthbl(1,pribl(i1)))))
  for i=0,nsecondary[i1]-1 do begin 
    dth1=secsi(i1,i,0)*(avthbl(1,secbl(i1,i,0))-avthbl(0,secbl(i1,i,0)))+ $
         secsi(i1,i,1)*(avthbl(1,secbl(i1,i,1))-avthbl(0,secbl(i1,i,1)))
    dth2l=secsi(i1,i,0)*(avthbl(0,secbl(i1,i,0)))+ $
               secsi(i1,i,1)*(avthbl(0,secbl(i1,i,1)))
    dth2u=secsi(i1,i,0)*(avthbl(1,secbl(i1,i,0)))+ $
               secsi(i1,i,1)*(avthbl(1,secbl(i1,i,1)))
    csum1=csum1+1.*exp(ic*complex(degra*dth1))
    csum2l=csum2l+1.*exp(ic*complex(degra*dth2l))
    csum2u=csum2u+1.*exp(ic*complex(degra*dth2u))
  endfor
  csum1=csum1/double(1+nsec(i1))
  ipart=imaginary(csum1) & rpart=float(csum1)
  dthdelaytel(ist,it2)=-r2d*atan(ipart,rpart)/2.d0
  lobe=abs(dthdelaytel(ist,it2))/360.
  if dthdelaytel(ist,it2) lt 0. then lobe=-lobe-1
  dthdelaytel(ist,it2)=dthdelaytel(ist,it2)-lobe*360.
  if dthdelaytel(ist,it2) gt 180. then dthdelaytel(ist,it2)= $
                        dthdelaytel(ist,it2)-360.
  csum2l=csum2l/double(1+nsec(i1))
  ipart=imaginary(csum2l) & rpart=float(csum2l)
  dthlotel(ist,it2)=(-r2d*atan(ipart,rpart)-dthdelaytel(ist,it2))/2.d0
  csum2u=csum2u/double(1+nsec(i1))
  ipart=imaginary(csum2u) & rpart=float(csum2u)
  dthlotel(ist,it2)=(-r2d*atan(ipart,rpart)+dthdelaytel(ist,it2))/2.d0
  if dthlotel(ist,it2) gt 180.d0 then dthlotel(ist,it2)= $
                  dthlotel(ist,it2)-360.d0
  if dthlotel(ist,it2) lt -180.d0 then dthlotel(ist,it2)= $
                  dthlotel(ist,it2)+360.d0
endfor
for ibl=0,nbase-1 do begin
  dthdelaybl(ist,ibl)=dthdelaytel(ist,dist_blcd_itel2(ibl))- $
             dthdelaytel(ist,dist_blcd_itel1(ibl))
  dthlobl(ist,ibl)=dthlotel(ist,dist_blcd_itel2(ibl))- $
             dthlotel(ist,dist_blcd_itel1(ibl))
endfor
endif
return,1
end
----------
X-Sun-Data-Type: default
X-Sun-Data-Description: default
X-Sun-Data-Name: bas_sol_group.pro
X-Sun-Content-Lines: 112
X-Sun-Charset: us-ascii

function bas_sol_group,dt_limit
;
;   group pairs of observations with time seperation < dtlimit
;
; eg res=bas_sol_group(dt_limit)
;
common global
common data_set
common plo
common baseline,tenlat,ntel,ref_tel,tel1,tel2,dist_tel,dist_tel1, $
         dist_tel2,nprimary,sec_bl,sec_sign,dist_blcd,dist_iblcd,pri_bl, $
         dist_blcd_tel1,dist_blcd_tel2,avthbl,dthdelaybl,utdts, $
         nsecondary,dthlobl,dthdelaytel,dthlotel,nbase,nobs,npair, $
         i1p,i2p,php,blcdp,sbp,cspa,cspb,csxpa,csxpb,csypa,csypb, $
         cszpa,cszpb,celpa,celpb,sigp,wtp,phaave,blcd,sb,band,sx,sy,sz, $
         cels,e0,neast,de,n0,nnorth,dn,u0,nup,du,a0,naxis,da,nmin, $
         chisq,bsol,ixp,slat,clat,cont,tha

max_pair=15000
i1p=make_array(max_pair,/int)
i2p=make_array(max_pair,/int)
php=make_array(max_pair,/double)
blcdp=make_array(max_pair,/string)
sbp=make_array(max_pair,/string)
cspa=make_array(max_pair,/double)
cspb=make_array(max_pair,/double)
csxpa=make_array(max_pair,/double)
csxpb=make_array(max_pair,/double)
csypa=make_array(max_pair,/double)
csypb=make_array(max_pair,/double) 
cszpa=make_array(max_pair,/double) 
cszpb=make_array(max_pair,/double)
celpa=make_array(max_pair,/double)
celpb=make_array(max_pair,/double)
sigp=make_array(max_pair,/double)
wtp=make_array(max_pair,/double)

npair=0 
 
for i=0,nobs-2 do begin
  nmax=min([nobs-i-1,20])
  index=(i+1)+indgen(nmax)
  jp=where((abs(utdts(index+1)-utdts(i)) le dt_limit) and $
     (bl[pbl[index+1]].iblcd eq bl[pbl[i]].iblcd) and $
     (bl[pbl[index+1]].isb eq bl[pbl[i]].isb),count)
  if count gt 0 then begin
    i1=npair+2*indgen(count)
    i2=index[jp] 
    i1p[i1]=tel1[i]
    i2p[i1]=tel2[i]
    php(i1)=cos(phaave(i2+1))-cos(phaave(i))
    blcdp(i1)=blcd(i)
    sbp(i1)=sb(i)
    cspa(i1)=100.d0*360.d0/(((!cvel/1.d6)/!if_freq)/1000.d0)
    cspb(i1)=100.d0*360.d0/(((!cvel/1.d6)/!if_freq)/1000.d0)
    csxpa(i1)=sx(i2) 
    csxpb(i1)=sx(i) 
    csypa(i1)=sy(i2) 
    csypb(i1)=sy(i) 
    cszpa(i1)=sz(i2) 
    cszpb(i1)=sz(i) 
    celpa(i1)=cels(i2)
    celpb(i1)=cels(i)
    sigp(i1)=0.05d0
    wtp(i1) =1.d0
    npair=npair+count
    i1=i1+1
    i1p[i1]=tel1[i]
    i2p[i1]=tel2[i]
    php(i1)=cos(phaave(i2+1))-cos(phaave(i))
    blcdp(i1)=blcd(i)
    sbp(i1)=sb(i)
    cspa(i1)=100.d0*360.d0/(((!cvel/1.d6)/!if_freq)/1000.d0)
    cspb(i1)=100.d0*360.d0/(((!cvel/1.d6)/!if_freq)/1000.d0)
    csxpa(i1)=sx(i2) 
    csxpb(i1)=sx(i) 
    csypa(i1)=sy(i2) 
    csypb(i1)=sy(i) 
    cszpa(i1)=sz(i2) 
    cszpb(i1)=sz(i) 
    celpa(i1)=cels(i2)
    celpb(i1)=cels(i)
    sigp(i1)=0.05d0
    wtp(i1) =1.d0
    npair=npair+count
  endif
  if npair ge max_pair-4 then goto,jump
endfor
jump:
npair=npair-1
i1p=i1p[0:npair-1]
i2p=i2p[0:npair-1]
php=php[0:npair-1]
blcdp=blcdp[0:npair-1]
sbp=sbp[0:npair-1]
cspa=cspa[0:npair-1]
cspb=cspb[0:npair-1]
csxpa=csxpa[0:npair-1]
csxpb=csxpb[0:npair-1]
csypa=csypa[0:npair-1]
csypb=csypb[0:npair-1]
cszpa=cszpa[0:npair-1]
cszpb=cszpb[0:npair-1]
celpa=celpa[0:npair-1]
celpb=celpb[0:npair-1]
sigp=sigp[0:npair-1]
wtp=wtp[0:npair-1]
print,npair,dt_limit, $
   format='("found ",i5," pairs of observations w/i ",f5.2," min")'

return,1
end
----------
X-Sun-Data-Type: default
X-Sun-Data-Description: default
X-Sun-Data-Name: bas_sol_lsgrid.pro
X-Sun-Content-Lines: 158
X-Sun-Charset: us-ascii

function bas_sol_lsgrid,ibl,isb
;
;   finds nmin lowest minimums in the sum of the squared phase differences
;
; res=bas_sol_lsgrid(ibl,isb)

;
;   ib = baseline #
;   e0 = center east offset (m) for the 2nd tel rel. to first
;   ne = number of grid points in east coord.
;   de = grid spacing in east coord.
;   similarly for n,u, and a(axis intersection parameter)
;   nobs = number of observations
;   nmin = desired number of minima
;

common global
common data_set
common plo
common baseline,tenlat,ntel,ref_tel,tel1,tel2,dist_tel,dist_tel1, $
         dist_tel2,nprimary,sec_bl,sec_sign,dist_blcd,dist_iblcd,pri_bl, $
         dist_blcd_tel1,dist_blcd_tel2,avthbl,dthdelaybl,utdts, $
         nsecondary,dthlobl,dthdelaytel,dthlotel,nbase,nobs,npair, $
         i1p,i2p,php,blcdp,sbp,cspa,cspb,csxpa,csxpb,csypa,csypb, $
         cszpa,cszpb,celpa,celpb,sigp,wtp,phaave,blcd,sb,band,sx,sy,sz, $
         cels,e0,neast,de,n0,nnorth,dn,u0,nup,du,a0,naxis,da,nmin, $
         chisq,bsol,ixp,slat,clat,cont,tha


;
;  initialize theta displacement arrays for grid of parameters and observations
;
 e00 = e0 - float(neast-1)*de/2.
 n00 = n0 - float(nnorth-1)*dn/2.
 u00 = u0 - float(nup-1)*du/2.
 a00 = a0 - float(naxis-1)*da/2.
 je=indgen(neast)
 jn=indgen(nnorth)
 ju=indgen(nup)
 ja=indgen(naxis)
 jo=indgen(nobs)
 ea=e00+je*de
 no=n00+jn*dn
 up=u00+ju*du
 ai=a00+ja*da
 ix=ixp[jo]
 dtha_e=make_array(neast,nobs,/int)
 dtha_n=make_array(nnorth,nobs,/int)
 dtha_u=make_array(nup,nobs,/int)
 dtha_a=make_array(naxis,nobs,/int)
 dtha_e[*]=ea(je)#sy[ix]
 dtha_n[*]=-slat*no[jn]#sx[ix]+clat*no[jn]#sz[ix]
 dtha_u[*]=clat*up[ju]#sx[ix]+slat*up[ju]#sz[ix]
 dtha_a[*]=ai[ja]#cels[ix]
;
;  set up array w/ square of phase differences
;
 i=lindgen(181)
 sqs=make_array(721,/long)
 sqs[i]=((i-1L)^2L)/10L
 sqs[361-i]=sqs[i]
 sqs[359+i]=sqs[i]
 sqs[721-i]=sqs[i]
 if isb eq 0 then begin
   print,1000.*ea(0),1000.*ea(neast-1),1000.*no(0),1000.*no(nnorth-1), $
         1000.*up(0),1000.*up(nup-1),1000.*ai(0),1000.*ai(naxis-1), $
         format='(1h ,"search box for baseline displacements",'+ $
             '/1h ,"  east     : ",f7.2," =>",f7.2," mm",'+ $
             '/1h ,"  north    : ",f7.2," =>",f7.2," mm",'+ $
             '/1h ,"  up       : ",f7.2," =>",f7.2," mm",'+ $
             '/1h ,"  axis int : ",f7.2," =>",f7.2," mm")'
 endif
;
;   search for set of lowest min sum of least squares 
;
ncont=n_elements(cont)
id=indgen(ncont-1)
if ncont gt 0 then begin
  itha=make_array(neast,ncont,/long)
  in=ncont*(lindgen(ncont,neast)/ncont)+make_array(neast,/int,value=1)##id
print,n_elements(in)
print,in
stop
  for jnn=0,nnorth-1 do begin
    for juu=0,nup-1 do begin
      for jaa=0,naxis-1 do begin
        itha[in]=-dtha_e(in)
        itha[*,id]=itha[*,id]+tha(id)-dtha_n(jnn,id)-dtha_u(juu,id)- $
             dtha_a(jaa,id) 
        itha[je,idd[1:n_elements(idd)-1]]=((itha[je,idd[1:n_elements(idd)-1]]- $
            itha[je,idd[0:n_elements(idd)-2]]+18000) mod 360)+360
        ichisq=total(sqs[itha[je,idd[1:n_elements(idd)-1]]],2)
print,ichisq
if juu eq 2 then stop
        chis=real(ichisq(sort(ichisq)))/1000.
        jl=where(chis lt chisq(isb,ibl,nmin-1),count_low)
        if count_low gt 0 then begin
          save_bsol=bsol & save_chisq=chisq
          jlow=sort([chis[jl],chisq[isb,ibl,0:nmin-1]])
          ncount=min([nmin,n_elements(jlow)])
          for i=0,ncount-1 do begin
            j=jlow[i]
            if j lt count_low-1 then begin
;
; add new minimum in
; 
              bsol(isb,ibl,i,1)=ea(je[jl])
              bsol(isb,ibl,i,2)=no(jnn)
              bsol(isb,ibl,i,3)=up(juu)
              bsol(isb,ibl,i,4)=ai(jaa)
              chisq(isb,ibl,i)=chis[jl]              
            endif else begin
;
; add old minimum in
; 
              bsol(isb,ibl,i,*)=save_bsol(isb,ibl,j-count_low-1,*)
              chisq(isb,ibl,i)=save_chisq[j-count_low-1]
            endelse
          endfor
;
; remove any minima which are close (w/i 2 grid cells) together
;
          i=0
          while i le ncount-2 do begin
            in=i+1+indgen(ncount-1-i-1)
            j=where( $
            dabs(bsol(isb,ibl,in,1)-bsol(isb,ibl,i,1))/de le 2 and $ 
            dabs(bsol(isb,ibl,in,2)-bsol(isb,ibl,i,2))/dn le 2.and $ 
            dabs(bsol(isb,ibl,in,3)-bsol(isb,ibl,i,3))/du le 2.and $
            dabs(bsol(isb,ibl,in,4)-bsol(isb,ibl,i,4))/da le 2.,ct)
            if ct gt 0 then begin
              print,'dropping a degenerate solution'
              ncount=ncount-1
              bsol(isb,ibl,in[j]:ncount-1,*)=bsol(isb,ibl,in[j]+1:ncount,*)
              chisq[isb,ibl,in[j]:ncount-1]=chisq[isb,ibl,in[j]+1:ncount]
              bsol[isb,ibl,ncount,*]=1.
              chisq[isb,ibl,ncount]=1.d+10
            endif
            i=i+1
          endwhile
        endif
      endfor
    endfor
  endfor
endif
if e.debug then begin
  for i=0,nmin do begin
    print,i,chisq(isb,ib,i),1000.d0*bsol(isb,ibl,i,1), $
                     1000.d0*bsol(isb,ibl,i,2),1000.d0*bsol(isb,ibl,i,3), $
                     1000.d0*bsol(isb,ibl,i,4), $
              format='(1h ,i2," l-s min =",f8.3," at: ",'+ $
                     ',"east=",f7.2," mm north=",f7.2," mm up=",' + $
                     'f7.2," mm axis=",f7.2," mm")'
  endfor
endif
return,1
end
 
----------
X-Sun-Data-Type: default
X-Sun-Data-Description: default
X-Sun-Data-Name: baseline_ini.pro
X-Sun-Charset: us-ascii
X-Sun-Content-Lines: 86

; *************************************************************************
; FUNCTION
;      baseline_ini.pro
;
; WRITTEN
;      July 19, 2000 by JMC
;
; PURPOSE
;      Set vector contain soids and soid comments for MIR
;
; INPUTS
;      none
;
; OUTPUT
;       soids : integer vector containg SOID numbers
;    comments : character vector containing soid comments
;       ibest : If non-zero, then indicates current soid is the best one
;
; EXAMPLES
;       result = baseline_ini(soids,comments,ibest,ncurrent)
;
; *************************************************************************
function baseline_ini,soids,comments,ibest,ncurrent
   ; Common blocks
     common global
     common data_set

   ; Initialize
     soids = [0]
     comments = ['bad soid']

   ; Find SOID number in track
     soid_current = uti_distinct(bl.soid,ncurrent)
     soid_current = fix(soid_current[0])

   ; Find current configuration number
     conid = uti_distinct(in.conid,nconid)
     conid = fix(conid)

   ; Set isql command
     command = 'select distinct altsoids=b.soid,altcomment=t.comments ' + $
               'from BLS b,TPO t ' + $
               'where b.con#=' + string(conid) + $
               ' and b.soid=t.soid order by b.soid desc'
     result = dbi_sql_submit(command)

   ; Must have at least 1 soid
     nsoids = n_elements(result) - 4
     if (nsoids eq 0) then return,-1

   ; Allocate memory for temporary and final storage
     soids         = intarr(nsoids)
     soids_temp    = intarr(nsoids)
     comments      = strarr(nsoids)
     comments_temp = strarr(nsoids)

   ; Read soid numbers and comments
     j = 0
     s = ' '
     for i = 0,nsoids-1 do begin
         reads,result(2+i),j,s
         soids_temp[i]    = j
         comments_temp[i] = strcompress(s)
     endfor

   ; Find location of current soid
     icurrent = where(soids_temp eq soid_current)
     if (ncurrent eq 0) then return,-1

   ; If icurrent is not zero, then there may be a better soid
     ibest = 1 * (icurrent[0] eq 0)

   ; Put current soid in entry 0
     soids[0]    = soids_temp[icurrent]
     comments[0] = comments_temp[icurrent]

   ; Now add the other soids
     j = where(soids_temp ne soid_current,nother_soids)
     if (nother_soids gt 0) then begin
        soids[1:nother_soids]    = soids_temp[j]
        comments[1:nother_soids] = comments_temp[j]
     endif

   ; Done
     return,1
end
