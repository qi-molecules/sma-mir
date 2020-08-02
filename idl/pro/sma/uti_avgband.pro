pro uti_avgband, all=all, band=band, old=old, swmch1=swmch1, swmch2=swmch2, zeroflag=zeroflag, excludefreq=excludefreq, c1_order=c1_order
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


if tag_exist(in,'conid') then newformat=0 else newformat=1

if keyword_set(band) then begin
   print,' BAND SELECTION IS OBSOLETE HERE'
   print,' PLEASE USE SELECT OR DAT_FILTER TO SELECT BAND FIRST'
   print,' BE SURE TO INCLUDE C1 FOR CONTINUUM GENERATION.'
   return
endif

;if not keyword_set(band) or keyword_set(all) then begin
if not keyword_set(old) then begin

   print, 'Averaging all filtered spectral bands to regenerate continuum ...'

   if keyword_set(excludefreq) then begin
;   if sp[psl[0]].sphint1 ne 1 then begin
;      print,'Stop ! Not a SWARM-only track.'
;      return
;   endif

;   temp=uti_distinct(bl[pbl].irec,nrecs,/many)
;   temp=uti_distinct(bl[pbl].isb,nsbs,/many)
;   if (nrecs gt 1) or (nsbs gt 1) then begin
;      print,'Stop ! Not working for multi-rx, multi-sb data.'
;      return
;   endif
   
      ntmp=n_elements(excludefreq)
      if ntmp/2*2 ne ntmp then begin
         print,'*** Wrong EXCLUDEFREQ range !***'
         print,'***         Quit !           ***'
         return
      endif
      
      if (size(excludefreq))[0] eq 2 then excludefreq=reform(excludefreq,ntmp)
      
      nlines=ntmp/2
      print,'Excluding frequency ranges:'
      for i=0,nlines-1 do begin
         print,'   ',excludefreq[2*i], ' to ',excludefreq[2*i+1],' GHz ...'
         if (abs(excludefreq[2*i+1]-excludefreq[2*i]) gt 1.806) then begin
            print,'*** EXCLUDEFREQ range too wide !***'
            print,'***          Quit !             ***'
            return
         endif
      endfor
   endif

      
      
   nch=sp[psl].nch
   j=where(nch lt 10, count)
   if (count le 0) then begin
      print,' *** NO CONTINUUM BAND IN FILTER ! ***'
      print,' *** PLEASE RESET FILTER TO INCLUDE CONTINUUM BANDS ***'
      return
   endif
      
   j=where(nch gt 10,count)
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

   icont=where(nch lt 10, count)
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
      
      if keyword_set(excludefreq) then begin
         chunkfreq=fltarr(nb*2)
         chunkfres=fltarr(nb)
         excludechan=intarr(nb,2*nlines)
         excludebw=fltarr(nb)
         iexclude=0
         if sp[psl[icont[i]]].fsky lt sp[psl[icont[i]+1]].fsky then sign=-1. else sign=1.
        
         for j=0,nb-1 do begin
            chunkfres[j]=sp[psl[icont[i]+j+1]].fres*1e-3
            chunkfreq[2*j]=sp[psl[icont[i]+j+1]].fsky-sign*(npts[j]/2-skip1[j])*abs(chunkfres[j])
            chunkfreq[2*j+1]=sp[psl[icont[i]+j+1]].fsky+sign*(npts[j]/2-skip2[j])*abs(chunkfres[j])
         endfor
         ;print,chunkfreq
         loc = VALUE_LOCATE(chunkfreq,excludefreq)
         ;print,loc

         for index=0,nlines-1 do begin
            m=index*2
            ;if loc[m] ge 0 then begin
            if abs(loc[m+1]-loc[m]) ge 2 then begin
               print,'*** EXCLUDEFREQ '+string(excludefreq[loc[m]])+' and '+string(excludefreq[loc[m+1]])+ ' set too wide !***'
               print,'***   Quit !      ***'
               return
            endif               
               inb=loc[m]/2
               inb2=loc[m+1]/2
               
               ; start line in even loc but not same chunk (save)
               ;if (inb*2 eq loc[m]) and (loc[m+1] ne loc[m]) then begin
               ;   if chunkfres[inb] gt 0 then skip2[inb]=skip2[inb]+(chunkfreq[loc[m]+1]-excludefreq[m])/chunkfres[inb] else skip1[inb]=skip1[inb]-(chunkfreq[loc[m]+1]-excludefreq[m])/chunkfres[inb]	
               ;endif
      
               ; end line in even loc 
               if inb2*2 eq loc[m+1] then begin
                  if loc[m+1] eq loc[m] then begin
                     ; same chunk
;                  excludechan[inb,iexclude]=128+fix((excludefreq[m]-sp[psl[icont[i]+inb+1]].fsky)/chunkfres[inb])
;                  excludechan[inb,iexclude+1]=128+fix((excludefreq[m+1]-sp[psl[icont[i]+inb+1]].fsky)/chunkfres[inb])  
                     tmp1=sp[psl[icont[i]+inb+1]].nch/2+fix((excludefreq[m]-sp[psl[icont[i]+inb+1]].fsky)/chunkfres[inb])
                     tmp2=sp[psl[icont[i]+inb+1]].nch/2+fix((excludefreq[m+1]-sp[psl[icont[i]+inb+1]].fsky)/chunkfres[inb])
                     excludechan[inb,iexclude]=tmp1<tmp2
                     excludechan[inb,iexclude+1]=tmp1>tmp2
                     ;print,excludechan[inb,iexclude]
                     ;print,excludechan[inb,iexclude+1]
                     iexclude=iexclude+2
                     excludebw[inb]=abs(excludefreq[m+1]-excludefreq[m])*1e3 ; mhz
                  endif else begin
                  ;if (loc[m+1]-loc[m]) ge 2 then begin (save)	
                  ;   if (loc[m+1]-loc[m]) eq 2 then begin
                  ;      if chunkfres[inb2] gt 0 then skip1[inb2]=skip1[inb2]+(excludefreq[m+1]-chunkfreq[loc[m+1]])/chunkfres[inb2] else skip2[inb2]=skip2[inb2]-(excludefreq[m+1]-chunkfreq[loc[m+1]])/chunkfres[inb2]
                  ;   endif else begin
                  ;      print,'Freq range wider over one chunk width. Check freq range!'
                  ;      return
                  ;   endelse
                  ;endif else begin
                     
                  ; start line in odd loc
                     tmp1=sp[psl[icont[i]+inb2+1]].nch/2+sign*fix((chunkfreq[loc[m+1]]-sp[psl[icont[i]+inb2+1]].fsky)/chunkfres[inb2])
                     tmp2=sp[psl[icont[i]+inb2+1]].nch/2+fix((excludefreq[m+1]-sp[psl[icont[i]+inb2+1]].fsky)/chunkfres[inb2])
                     ;stop
                     excludechan[inb2,iexclude]=tmp1<tmp2
                     excludechan[inb2,iexclude+1]=tmp1>tmp2
                     ;print,excludechan[inb2,iexclude]
                     ;print,excludechan[inb2,iexclude+1]
                     excludebw[inb2]=abs((excludechan[inb2,iexclude+1]-excludechan[inb2,iexclude])*chunkfres[inb2])*1e3;mhz
                     iexclude=iexclude+2
                     ;excludebw[inb2]=abs(excludefreq[m+1]-chunkfreq[loc[m+1]])*1e3 ; mhz
                  endelse                                                   ; end line in even loc
               endif else begin 
                  ; end line in odd loc
                  if loc[m+1] eq loc[m] then begin
                     ; same chunk
                     if (loc[m] lt 0) or (loc[m] eq nb*2-1) then begin
                        ;print,'excludeFrequency range not used.' 
                        goto, sbFlag
                     endif else begin
                        print,'*** EXCLUDEFREQ '+string(excludefreq[m])+' and '+string(excludefreq[m+1])+' set incorrectly !***'
                        print,'***   Quit !!      ***'
                        return
                     endelse
                  endif else begin
                     ; start line in even loc
                     tmp1=sp[psl[icont[i]+inb+1]].nch/2+fix((excludefreq[m]-sp[psl[icont[i]+inb+1]].fsky)/chunkfres[inb])
                     tmp2=sp[psl[icont[i]+inb+1]].nch/2+sign*fix((-1.*chunkfreq[loc[m]]+sp[psl[icont[i]+inb+1]].fsky)/chunkfres[inb])
                     excludechan[inb,iexclude]=tmp1<tmp2
                     excludechan[inb,iexclude+1]=tmp1>tmp2
                     ;print,excludechan[inb,iexclude]
                     ;print,excludechan[inb,iexclude+1]
                     excludebw[inb]=abs((excludechan[inb,iexclude+1]-excludechan[inb,iexclude])*chunkfres[inb])*1e3;mhz
                     iexclude=iexclude+2
                     ;excludebw[inb]=abs(excludefreq[m]-chunkfreq[loc[m]])*1e3 ; mhz
                  endelse                            
               endelse
            ;endif
               sbFlag:
         endfor           
      endif

      if e.debug then begin
         print,'skip1:'
         print,skip1
         print,'skip2:'
         print,skip2
         print,'excludechan:'
         print,excludechan
         print,'excludebw:'
         print,excludebw
         read,iii
      endif

;   k=where( npts eq 16384, nswp)
      if newformat then k=where((sp[psl[icont[i]+1:icont[i]+nb]].sphint1 eq 1) or (sp[psl[icont[i]+1:icont[i]+nb]].iband ge 49),nswp) else k=where(sp[psl[icont[i]+1:icont[i]+nb]].iband ge 49,nswp)
      bw=0.
      if nswp gt 0 then begin
         if keyword_set(swmch1) and keyword_set(swmch2) then begin
            skip1[k]=swmch1-1
            skip2[k]=npts[k]-swmch2
         endif else begin
            skip1[k]= fix( 1031.*npts[k]/16384. )
            skip2[k]= fix( 1031.*npts[k]/16384. )
         endelse
         swmfres=abs(sp[psl[k[0]+1]].fres)
         bw=total([npts[k]-skip1[k]-skip2[k]])*swmfres + (nb-nswp)*82.
         avgwt[*] = 82./bw
         avgwt[k] = (npts[k]-skip1[k]-skip2[k])*swmfres/bw
         if keyword_set(excludefreq) then begin
            bw=bw-total(excludebw)         
            avgwt[k]=((npts[k]-skip1[k]-skip2[k])*swmfres-excludebw[k])/bw
         endif      
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
         if keyword_set(excludefreq) then begin
            jtmp=where(excludechan[j,*] gt 0, jcount)
            tmpcmp=dcomplex(0,0)
            if jcount gt 0 then begin
               nexclude=0
               for n=0,jcount/2-1 do begin
                  tmpcmp=total(ch[first[j]+excludechan[j,jtmp[n*2]]:first[j]+excludechan[j,jtmp[n*2+1]]])
                  nexclude=nexclude+abs(excludechan[j,jtmp[n*2+1]]-excludechan[j,jtmp[n*2]])
               endfor
               cmp=cmp+avgwt[j]*(total(ch[first[j]+skip1[j]:first[j]+npts[j]-skip2[j]-1])-tmpcmp)/(npts[j]-skip2[j]-skip1[j]-nexclude)
            endif else begin
               cmp=cmp+avgwt[j]*total(ch[first[j]+skip1[j]:first[j]+npts[j]-skip2[j]-1])/(npts[j]-skip2[j]-skip1[j])
            endelse              
         endif else begin
            cmp=cmp+avgwt[j]*total(ch[first[j]+skip1[j]:first[j]+npts[j]-skip2[j]-1])/(npts[j]-skip2[j]-skip1[j])
         endelse
      endfor
      uti_conv_apc,cmp,amp,pha,/amp_pha
      bl[pbl[icont[i]]].ampave=amp
      bl[pbl[icont[i]]].phaave=pha
      if sp[psl[icont[i]]].nch gt 1 then begin
         bl[pbl[icont[i]]].fave=total(avgwt*sp[psl[icont[i]+1:icont[i]+nb]].fsky)
         bl[pbl[icont[i]]].bwave=bw
         bl[pbl[icont[i]]].wtave=sp[psl[icont[i]]].wt*bw/abs(sp[psl[icont[i]]].fres)
         if keyword_set(c1_order) then ch[pcl[icont[i]]+c1_order-1]=cmp
      endif else begin
         ch[pcl[icont[i]]]=cmp
         oldfres=sp[psl[icont[i]]].fres
         sp[psl[icont[i]]].fres=sp[psl[icont[i]]].fres/abs(sp[psl[icont[i]]].fres)*bw
         sp[psl[icont[i]]].wt=sp[psl[icont[i]]].wt*abs(sp[psl[icont[i]]].fres/oldfres)
         re.wts[prl[icont[i]]]=re.wts[prl[icont[i]]]*abs(sp[psl[icont[i]]].fres/oldfres)
         sp[psl[icont[i]]].fsky=total(avgwt*sp[psl[icont[i]+1:icont[i]+nb]].fsky)
         sp[psl[icont[i]]].vres=-1.*sp[psl[icont[i]]].fres/sp[psl[icont[i]]].fsky*!cvel/1e6
      endelse
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


