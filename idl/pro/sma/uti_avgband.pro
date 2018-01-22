pro uti_avgband, all=all, band=band, old=old, swmch1=swmch1, swmch2=swmch2, zeroflag=zeroflag
;yes
;=Task:UTI_AVGBAND --- To create new continuum visibilities by averaging channel data.
;#Type: utility
;+Use:
;      The commnad UTI_AVGBAND can be used to average the uv data in
;      one or more chunks and make a new continuum channel c1. The
;      following are some examples:
;      >uti_avgband           --- Averaging all bands 
;      >uti_avgband,band='s3' --- Averaging chunk3 data
;      >uti_avgband,band=['s1','s2','s3','s4'] --- Averaging data from chunk1
;                                                  to chunk4.   
;      >uti_avgband,band='s1',chstart=32,chend=64 --- Averaging chunk1 data,
;                                                     but only from ch32 to ch64;%history:
;&history:
;------------------------------------------------------------------------
;      cykuo 19feb04 Adapting the header
;------------------------------------------------------------------------   

common global
common data_set

if tag_exist(sp,'sphint1') then newformat=1 else newformat=0

if keyword_set(band) then begin
   print,' BAND SELECTION IS OBSOLETE HERE'
   print,' PLEASE USE SELECT OR DAT_FILTER TO SELECT BAND FIRST'
   print,' BE SURE TO INCLUDE C1 FOR CONTINUUM GENERATION.'
   return
endif

;if not keyword_set(band) or keyword_set(all) then begin
if not keyword_set(old) then begin

lat=double(19.82420526391d*!dpi/180.d)
m1=[[-sin(lat),0.d,cos(lat)],[0.d,1.d,0.d],[cos(lat),0.d,sin(lat)]]

print, 'Averaging all filtered spectral bands to regenerate continuum ...'
nch=sp[psl].nch
j=where(nch eq 1, count)
if (count le 0) then begin
   print,' *** NO CONTINUUM BAND IN FILTER ! ***'
   print,' *** PLEASE RESET FILTER TO INCLUDE CONTINUUM BANDS ***'
   return
endif

j=where(nch gt 1,count)
if (count le 0) then begin
   print,' *** NO SPECTRUM BAND IN FILTER ! ***'
   print,' ***   PLEASE RESET THE FILTER !   ***'
   return
endif

;minch=min(nch[j])
if keyword_set(zeroflag) then begin
   ibad=where(abs(ch[pcl[j]+nch[j]/2]) eq 0., count)
endif else begin
   ibad=where((nch[j] lt 2000) and (abs(ch[pcl[j]+nch[j]/2]) eq 0.), count)
endelse 

;print,j[ibad]
;print,uti_distinct(sp[psl[j[ibad]]].iband,nband_bad,/many)
if (count gt 0.) then begin
   int_bad=uti_distinct(in[pil[j[ibad]]].int,nint_bad,/many_repeat)
   print, 'Automatic flagging flat-spectrum points in integrations:' 
   print,int_bad
   print, ''
   sp[psl[j[ibad]]].wt=-abs(sp[psl[j[ibad]]].wt)
   re.wts[prl[j[ibad]]]=-abs(re.wts[prl[j[ibad]]])
   result=dat_list(s_l,'"wt" gt "0"',/reset,/no_notify)
endif

distinct_iband=uti_distinct(sp[psl].iband,nband,/many_repeat)
nch=sp[psl].nch

icont=where(nch eq 1, count)
if (count le 0) then begin
   print,' *** NO CONTINUUM BAND IN FILTER ! ***'
   print,' ***   PLEASE RESET THE FILTER !   ***'
   return
endif

acont=icont[0L:count-2L]
bcont=icont[1L:count-1L]
ccont=bcont-acont
i=where((ccont le 1) or (ccont gt nband),temp)
if (temp gt 0) then begin
   int_bad=uti_distinct(in[pil[icont[i]]].int,nint_bad,/many_repeat)
   print, ' No CORRESPONDING SPECTRA BAND in integrations:'
   print, int_bad
   print, ' ***   CHECK/FLAG THE DATA AND RESET FILTER !      ***'
   return
endif


for i=0L, count-1L do begin
   if i eq count-1L then nb=n_elements(pcl)-icont[count-1L]-1 else nb=icont[i+1]-icont[i]-1
   npts=nch[icont[i]+1:icont[i]+nb]
   avgwt=dblarr(nb)
   
   skip1 = fix(  (1 - 82./104.)*npts/2.  )
   skip2 = fix(  (1 - 82./104.)*npts/2.  )
   first = pcl[icont[i]+1:icont[i]+nb]
   
;   k=where( npts eq 16384, nswp)
   if newformat then k=where((sp[psl[icont[i]+1:icont[i]+nb]].sphint1 eq 1) or (sp[psl[icont[i]+1:icont[i]+nb]].iband ge 49),nswp) else k=where(sp[psl[icont[i]+1:icont[i]+nb]].iband ge 49,nswp)
   bw=0.
   if nswp gt 0 then begin
      if keyword_set(swmch1) and keyword_set(swmch2) then begin
         skip1[k]=swmch1-1
         skip2[k]=npts[k]-swmch2
      endif
      swmfres=abs(sp[psl[k[0]+1]].fres)
      bw=total([npts[k]-skip1[k]-skip2[k]])*swmfres + (nb-nswp)*82.
      avgwt[*] = 82./bw
      avgwt[k] = (npts[k]-skip1[k]-skip2[k])*swmfres/bw
   endif else begin
      bw=n_elements(npts)*82.
      avgwt[*] = 82./bw
   endelse

; with chstart and chend 
;        if (  keyword_set(chstart)) then begin
;           if (n_elements(chstart) gt 1) then begin
;              if (n_elements(chstart) ne n_elements(npts)) or (n_elements(chend) ne n_elements(npts)) then begin
;                 print, 'Wrong number of chstart/chend !'
;                 print, 'Please reset chstart/chend !'
;                 return
;              endif else begin
;                 skip1=chstart - 1
;                 skip2=npts - chend
;              endelse
;           endif else begin
;              skip1 = make_array(n_elements(npts),/long,value=chstart)-1
;              skip2 =  make_array(n_elements(npts),/long)
;              skip2 =  npts - chend 
;           endelse
;        endif

        cmp=complex(0,0)
        for j=0,nb-1 do begin
           cmp=cmp+avgwt[j]*total(ch[first[j]+skip1[j]:first[j]+npts[j]-skip2[j]-1])/(npts[j]-skip2[j]-skip1[j])
        endfor
        ch[pcl[icont[i]]]=cmp
        oldfres=sp[psl[icont[i]]].fres
        sp[psl[icont[i]]].fres=sp[psl[icont[i]]].fres/abs(sp[psl[icont[i]]].fres)*bw
        sp[psl[icont[i]]].wt=sp[psl[icont[i]]].wt*abs(sp[psl[icont[i]]].fres/oldfres)
        re.wts[prl[icont[i]]]=re.wts[prl[icont[i]]]*abs(sp[psl[icont[i]]].fres/oldfres)
        sp[psl[icont[i]]].fsky=total(avgwt*sp[psl[icont[i]+1:icont[i]+nb]].fsky)
        sp[psl[icont[i]]].vres=-1.*sp[psl[icont[i]]].fres/sp[psl[icont[i]]].fsky*!cvel/1e6
        uti_conv_apc,cmp,amp,pha,/amp_pha
        bl[pbl[icont[i]]].ampave=amp
        bl[pbl[icont[i]]].phaave=pha

        ; recalculate u,v,w
        h=in[pil[icont[i[0]]]].ha*15.d*!dpi/180.d
        dec=in[pil[icont[i[0]]]].decr
         m2=[[sin(h),cos(h),0],[-sin(dec)*cos(h),sin(dec)*sin(h),cos(dec)],$
           [cos(dec)*cos(h),-cos(dec)*sin(h),sin(dec)]]
         neu=[bl[pbl[icont[i[0]]]].bln,bl[pbl[icont[i[0]]]].ble,bl[pbl[icont[i[0]]]].blu]
         neu=transpose(neu)
         uvw=reform(m2##m1##neu)/(1000.d*0.299792458d/sp[psl[icont[i]]].fsky)
;         print,uvw
         bl[pbl[icont[i]]].u=uvw[0]
         bl[pbl[icont[i]]].v=uvw[1]
         bl[pbl[icont[i]]].w=uvw[2]
         bl[pbl[icont[i]]].prbl=sqrt(uvw[0]^2+uvw[1]^2)
endfor

print, 'Done!'

endif else begin

band=uti_distinct(c.band[sp[psl].iband],nband,/many_repeat)
band=band[where(band ne 'c1')]
distinct_sb=uti_distinct(c.sb[bl[pbl].isb],nsbs,/many_repeat)
distinct_blcd=uti_distinct(c.blcd[bl[pbl].iblcd],nblcds,/many_repeat)
distinct_rec=uti_distinct(c.rec[bl[pbl].irec],nrecs,/many_repeat)

for ir = 0,nrecs-1 do begin

for is = 0,nsbs-1 do begin
  freq=0.
  nfreq=0.
  for ib = 0,nblcds-1 do begin
     result=dat_list(s_l,'"wt" gt "0" and "band" eq "c1" and "sb"   eq "' $
        +distinct_sb[is] + '" and "blcd" eq "' +distinct_blcd[ib]+ $
        '" and "rec" eq "'+distinct_rec[ir]+'"',/reset,/no_notify)
     if sp[psl[0]].fres ne n_elements(band)*82. then sp[psl].fres=(sp[psl].fres/abs(sp[psl].fres)) * n_elements(band)*82.
     nc=n_elements(pil)
     nint=in[pil].int
     nb=intarr(nc)
     for k = 0L,n_elements(band)-1 do begin
        recheck: result=dat_list(s_l,'"wt" gt "0" and "band" eq "' + band[k]+ '" and "sb"   eq "' $
            +distinct_sb[is]+ '" and "blcd" eq "' +distinct_blcd[ib]+ $
            '" and "rec" eq "'+distinct_rec[ir]+'"',/reset,/no_notify)
        if result eq 0 then begin
          print, 'No data in '+band[k]
          print, 'Please flag out this whole band first. Quit !!!'
          return
       endif
       n_ch=sp[psl].nch+1
       ibad=where(abs(ch[pcl+n_ch/2]) eq 0., count)
       if (count gt 0.) then begin
          int_bad=strcompress(string(uti_distinct(in[pil[ibad]].int,nint_bad,/many_repeat)),/remove_all)          
          print, 'Automatic flagging bad points of integrations in baseline '+ $
                 distinct_blcd[ib]+ ' band '+band[k]+':'  
          print,int_bad
          sp[psl[ibad]].wt=-abs(sp[psl[ibad]].wt)
          re.wts[prl[ibad]]=-abs(re.wts[prl[ibad]])
          goto, recheck
       endif

;        kk=where((in[pil[1:*]].int-in[pil].int) ge 2, count)
;        if count gt 0 then deficit=in[pil[kk]].int+1 else deficit=-1
        freq=freq+total(sp[psl].fsky)
        nfreq=nfreq+n_elements(psl)
        dat_get_rows,cmp,amp,pha,x,wts,first,npts,'channel',js,/list,/amp_pha
;        print,'The number of points is ',n_elements(first)
        skip1 = fix(  (1 - 82./104.)*npts/2.  )
        skip2 = fix(  (1 - 82./104.)*npts/2.  )
        if (  keyword_set(chstart)) then begin
           if (n_elements(chstart) gt 1) then begin
              print, 'chstart/chend have to be set as a single number here.'
              print, 'Please reset chstart/chend !'
              return
           endif else begin
              skip1 = make_array(n_elements(npts),/long,value=chstart)
              skip2 =  make_array(n_elements(npts),/long)
              skip2 =  npts - chend 
           endelse
        endif

;print,'start end ',skip1[0],npts[0] - skip2[0] 


;print,'skip channels ',skip

        if (k eq 0) then begin
           avgreal =  fltarr(nc)
           avgimag =  fltarr(nc)
        endif
        for j = 0L, nc-1 do begin
          i=where(nint(j) eq in[pil].int, count)
          if count ne 0 then begin
  avgreal[j] = avgreal[j] + total(  float (cmp[   first[i]+skip1[i] : first[i]+npts[i]-skip2[i]-1 ])  )/(npts[i]-skip2[i]-skip1[i] )
  avgimag[j] = avgimag[j] + total(  imaginary(cmp[   first[i]+skip1[i] : first[i]+npts[i]-skip2[i]-1 ])  )/(npts[i]-skip2[i]-skip1[i] )
              nb[j]=nb[j]+1
          endif
        endfor             

     endfor
     n_nb=where(nb eq 0,count)
     if count gt 0 then begin
        int_nb=strcompress(string(nint(where(nb eq 0))),/remove_all)
        command='"rec" eq "'+distinct_rec[ir]+'" and "sb" eq "'+distinct_sb[is]+ '" and "blcd" eq "' $
            +distinct_blcd[ib]+ '" and ("int" eq "'+int_nb[0]+'"'
        if count gt 1 then for i=1,count-1 do command=command+' or "int" eq "'+ int_nb[i]+'"'
        command=command+')'
;        print,command
        result=dat_list(s_l,command,/reset,/no_notify)
        sp[psl].wt = -abs(sp[psl].wt)
        re.wts[prl] = -abs(re.wts[prl] )        
     endif
     avgreal = avgreal(where(nb ne 0))/nb(where(nb ne 0))
     avgimag = avgimag(where(nb ne 0))/nb(where(nb ne 0))
;     print,'The number of average points is ',n_elements(avgreal)
     result=dat_list(s_l,'"wt" gt "0" and "band" eq "c1" and "sb"   eq "' +distinct_sb[is] $
        + '" and "blcd" eq "' +distinct_blcd[ib]+'" and "rec" eq "'+distinct_rec[ir]+'"',/reset,/no_notify)
;     print,'The number of continuum points is ',n_elements(pcl),n_elements(pbl)
     ch[pcl]=complex(avgreal,avgimag)

     phase2 = atan( avgimag,avgreal ) * 180./!pi
     amp2 = sqrt(avgimag*avgimag + avgreal*avgreal) 
     bl(pbl).ampave = amp2
     bl(pbl).phaave = phase2
     print,'Bands averaging done for baseline '+distinct_blcd[ib]+' '+distinct_sb[is]+ 'sb'+ ' '+'rx '+distinct_rec[ir]
     print,''
  endfor
  freq=freq/nfreq
  result=dat_list(s_l,'"band" eq "c1" and "sb" eq "' +distinct_sb[is]+ '" and "rec" eq "'+distinct_rec[ir]+'"',/reset,/no_notify)
  sp[psl].fsky=freq
endfor ; for sb
endfor ; for rec

print,'Finished bands averaging ',band

endelse

result=dat_list(s_l,/reset)

end


