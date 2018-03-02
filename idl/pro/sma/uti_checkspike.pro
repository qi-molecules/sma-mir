pro uti_checkspike, source=source, ntrim_max=ntrim_max, threshold=threshold, baseline=baseline, fix=fix, sample=sample

common global
common data_set

if not keyword_set(threshold) then threshold=10.
if not keyword_set(sample) then sample=10

if not keyword_set(source) then begin
   print,'No source provided. Quit!'
   return
endif

x_var='channel'
result=dat_list(s_l,'"band" like "s" and "source" like "'+source+'"',/reset,/no)
if result le 0 then begin
   print,'No source ',source,' found. Quit !'
   return
endif


distinct_irecs=uti_distinct(bl[pbl].irec,nrecs,/many_repeat)
distinct_isbs=uti_distinct(bl[pbl].isb,nsbs,/many_repeat)
bands=strupcase(c.band(sp(psl).iband))
distinct_bands=uti_distinct(strlowcase(bands),nbands,/many_repeat)
distinct_ibands=make_array(nbands,/int,/nozero)

blave=1
chanlist=[0,0,0,0]
if keyword_set(baseline) then begin
   distinct_ibls=uti_distinct(bl[pbl].iblcd,nbls,/many_repeat) 
   blave=0
   chanlist=[0,0,0,0,0]
endif
nchs=make_array(nbands,/long) & first_chs=make_array(nbands,/long) 
for i=0,nbands-1 do begin
   distinct_ibands(i)=min(where(c.band eq distinct_bands(i)))
   first_chs(i)=total(nchs)
   nchs(i)=sp(psl(min(where(sp(psl).iband eq distinct_ibands(i))))).nch
endfor

a0=pil & a1=pbl & a2=psl & a3=pcl & a4=prl & index=-1L
for i=0, nbands-1 do begin
   band=distinct_bands[i]
   ib=max(where(distinct_bands eq band))
   for j=0, nsbs-1 do begin
      sb=strupcase(c.sb[distinct_isbs[j]])
      for k=0, nrecs-1 do begin
         rec=c.rec[distinct_irecs[k]]
         n=where( (bl[a1].irec eq distinct_irecs[k]) and (bl[a1].isb eq distinct_isbs[j]) and (sp[a2].iband eq distinct_ibands[i]), count)
         if count eq 0 then goto,jump1
         if blave then begin
            pil=a0[n] & pbl=a1[n] & psl=a2[n] & pcl=a3[n] & prl=a4[n]
            dat_get_rows,cmp,amp,pha,x,wts,first,npts,x_var,0,/list, $
                         average='all',/normalize,/amp_pha
            index=index+1
            if npts[0] eq 0 then begin
               print,'could not average ', rec, ' ',sb, ' ',band
               goto, jump1
            endif         
;            x=x+first_chs(ib)
            ntrim = keyword_set(ntrim_max)? (npts[0] gt 2*ntrim_max ? ntrim_max : 0) : 0
            x=x[ntrim:npts[0]-ntrim-1]
;         cmp=cmp[ntrim:npts[0]-ntrim-1]
            amp=amp[ntrim:npts[0]-ntrim-1]
            npts=npts-2*ntrim
            amprms=stddev(amp)
            ampsmooth=smooth([amp<0.001],npts/100,/edge_mirror)
            i_spike=where(abs(amp-ampsmooth) gt threshold*amprms,count)
            if count gt 0 then begin
               for is=0, count-1 do begin
                  print,'Spike found in RX ',c.rec[distinct_irecs[k]],', Sideband ',strupcase(c.sb[distinct_isbs[j]]), ', Band ', distinct_bands[i], ', Channel ',strcompress(string(i_spike[is]+ntrim+1),/remove)
                  chanlist=[[chanlist],[k,j,i,i_spike[is]+ntrim+1]]
               endfor
            endif
         endif else begin
            b0=a0[n] & b1=a1[n] & b2=a2[n] & b3=a3[n] & b4=a4[n]
            for m=0, nbls-1 do begin
               blcd=c.blcd[distinct_ibls[m]]
               n=where( bl[b1].iblcd eq distinct_ibls[m], count)
               if count eq 0 then goto, jump2
               pil=b0[n] & pbl=b1[n] & psl=b2[n] & pcl=b3[n] & prl=b4[n]
               dat_get_rows,cmp,amp,pha,x,wts,first,npts,x_var,0,/list, $
                            average='all',/normalize,/amp_pha
               index=index+1
               if npts[0] eq 0 then begin
                  print,'could not average ', rec, ' ',sb, ' ',band
                  goto, jump1
               endif         
;               x=x+first_chs(ib)               
               ntrim = keyword_set(ntrim_max)? (npts[0] gt 2*ntrim_max ? ntrim_max : 0) : 0
               x=x[ntrim:npts[0]-ntrim-1]
;         cmp=cmp[ntrim:npts[0]-ntrim-1]
               amp=amp[ntrim:npts[0]-ntrim-1]
               npts=npts-2*ntrim
               amprms=stddev(amp)
               ampsmooth=smooth([amp<0.001],npts/100,/edge_mirror)
               i_spike=where(abs(amp-ampsmooth) gt threshold*amprms,count)
               if count gt 0 then begin
                  for is=0, count-1 do begin
                     print,'Spike found in RX ',c.rec[distinct_irecs[k]],', Sideband ',strupcase(c.sb[distinct_isbs[j]]), ', Band ', distinct_bands[i], ', Baseline ',blcd, ',Channel ',strcompress(string(i_spike[is]+ntrim+1),/remove)
                     chanlist=[[chanlist],[k,j,i,m,i_spike[is]+ntrim+1]]
                  endfor
               endif              
               jump2:
            endfor ; baselines
         endelse ; blave
         jump1:
      endfor ;recs
   endfor ; sbs
endfor ;bands

s=size(chanlist)
if keyword_set(fix) and s[0] gt 1 then begin
   print,'Channel-fixing will be applied to all sources in the track ...'
   result=dat_filter(s_s,/save)
   for i=1,s[2]-1 do begin
      if s[1] eq 4 then begin
         command='"rec" eq "'+c.rec[distinct_irecs[chanlist[0,i]]]+'" and "sb" eq "'+c.sb[distinct_isbs[chanlist[1,i]]]+'" and "band" eq "'+distinct_bands[chanlist[2,i]]+'"'
         chanfix=chanlist[3,i]
      endif else begin
         command='"rec" eq "'+c.rec[distinct_irecs[chanlist[0,i]]]+'" and "sb" eq "'+c.sb[distinct_isbs[chanlist[1,i]]]+'" and "band" eq "'+distinct_bands[chanlist[2,i]]+'" and "blcd" eq "'+ c.blcd[distinct_ibls[chanlist[3,i]]]+'"'
         chanfix=chanlist[4,i]
      endelse
      result=dat_filter(s_f,command,/no_notify,/reset)
;      print,command,chanfix
      uti_chanfix,channel=chanfix,sample=sample
   endfor
   print,'Channel-fix done with sample = '+strcompress(string(sample),/remove)+' !'
   result=dat_filter(s_s,/restore)
endif else print,'No more spike found !' 

end
