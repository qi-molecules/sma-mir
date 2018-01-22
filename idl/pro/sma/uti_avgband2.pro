pro uti_avgband2, chstart=chstart, chend=chend, band=band, old=old
;yes
;=Task:UTI_AVGBAND2 --- To create new continuum visibilities by averaging channel data.
;#Type: utility
;+Use:
;      The commnad UTI_AVGBAND can be used to average the uv data in
;      one or more chunks and make a new continuum channel c1. The
;      following are some examples:
;      >uti_avgband           --- Averaging all bands 
;      >uti_avgband,chstart=32,chend=64 --- Averaging a given chunk data,
;                                          but only from ch32 to ch64
;%history:
;&history:
;------------------------------------------------------------------------
;      cykuo 19feb04 Adapting the header
;------------------------------------------------------------------------   

common global
common data_set

if keyword_set(band) then begin
   print,' BAND SELECTION IS OBSOLETE HERE'
   print,' PLEASE USE SELECT OR DAT_FILTER TO SELECT BAND FIRST'
   print,' BE SURE TO INCLUDE C1 FOR CONTINUUM GENERATION.'
   return
endif

;if not keyword_set(band) or keyword_set(all) then begin
if not keyword_set(old) then begin

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

ibad=where(abs(ch[pcl[j]+nch[j]/2]) eq 0., count)
;print,j[ibad]
if (count gt 0.) then begin
   int_bad=uti_distinct(in[pil[j[ibad]]].int,nint_bad,/many_repeat)
   print, 'Automatic flagging flat-spectrum points in integrations:' 
   print,int_bad
   print, ''
   sp[psl[j[ibad]]].wt=-abs(sp[psl[j[ibad]]].wt)
   re.wts[prl[j[ibad]]]=-abs(re.wts[prl[j[ibad]]])
   result=dat_list(s_l,'"wt" gt "0"',/reset,/no_notify)
endif

distinct_band=uti_distinct(c.band[sp[psl].iband],nband,/many_repeat)
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
        skip1 = fix(  (1 - 82./104.)*npts/2.  )
        skip2 = fix(  (1 - 82./104.)*npts/2.  )
        first = pcl[icont[i]+1:icont[i]+nb]
        if (  keyword_set(chstart)) then begin
           if (n_elements(chstart) gt 1) then begin
              if (n_elements(chstart) ne n_elements(npts)) or (n_elements(chend) ne n_elements(npts)) then begin
                 print, 'Wrong number of chstart/chend !'
                 print, 'Please reset chstart/chend !'
                 return
              endif else begin
                 skip1=chstart - 1
                 skip2=npts - chend
              endelse
           endif else begin
              skip1 = make_array(n_elements(npts),/long,value=chstart)-1
              skip2 =  make_array(n_elements(npts),/long)
              skip2 =  npts - chend 
           endelse
        endif
        cmp=complex(0,0)
        for j=0,nb-1 do begin
           cmp=cmp+total(ch[first[j]+skip1[j]:first[j]+npts[j]-skip2[j]-1])/(npts[j]-skip2[j]-skip1[j])
        endfor
        ch[pcl[icont[i]]]=cmp/nb
        oldfres=sp[psl[icont[i]]].fres
        sp[psl[icont[i]]].fres=sp[psl[icont[i]]].fres/abs(sp[psl[icont[i]]].fres)*82.*nb
        sp[psl[icont[i]]].wt=sp[psl[icont[i]]].wt*abs(sp[psl[icont[i]]].fres/oldfres)
        re.wts[prl[icont[i]]]=re.wts[prl[icont[i]]]*abs(sp[psl[icont[i]]].fres/oldfres)
        sp[psl[icont[i]]].fsky=mean(sp[psl[icont[i]+1:icont[i]+nb]].fsky)
        uti_conv_apc,cmp/nb,amp,pha,/amp_pha
        bl[pbl[icont[i]]].ampave=amp
        bl[pbl[icont[i]]].phaave=pha
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


