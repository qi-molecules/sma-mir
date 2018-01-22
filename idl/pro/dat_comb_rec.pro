function dat_comb_rec
;
; Used to combine individual records w/i each integration
;
;
; eg . result=dat_comb_rec()    
;

;
;Define common blocks
;
common wlm
common data_set
common global
common plo
; 
; select out all data with > 1 record
;
res=dat_list(s_l,'"nrec" ne "1"',/reset,/no_notify)
nsp=n_elements(psl)
ndata=long(total(sp[psl].nch))
pil_save=pil & pbl_save=pbl & psl_save=psl
;
; keep track of data which is not having records combined so 
; it can all be merged in end (if we can not easily determining
; the excluded rows, we will delete the excluded data -- in most
; cases this is not necessary)
;
delete_other_data=1
if max(ps) ne n_elements(ps)-1 then begin
   print,'combining records will delete all data no passing through filter'
endif else begin
  ps_exclude=make_array(n_elements(ps),/long)
  ps_exclude(psl)=1L
  j=where(ps_exclude eq 0,count_ps_exclude)
  if count_ps_exclude gt 0 then begin
    ps_exclude=ps_exclude[j]
    pi_exclude=make_array(n_elements(pi),/long)
    pi_exclude(pil)=1L
    j=where(pi_exclude eq 0,count_pi_exclude)
    if count_pi_exclude gt 0 then pi_exclude=pi_exclude[j]
    pb_exclude=make_array(n_elements(pb),/long)
    pb_exclude(pbl)=1L
    j=where(pb_exclude eq 0,count_pb_exclude)
    if count_pb_exclude gt 0 then pb_exclude=pb_exclude[j]
    if e.debug then print,'excluded rows in in,bl,and sp :',count_pi_exclude,$
          count_pb_exclude,count_ps_exclude
    nsp=n_elements(ps)-1
    ndata=long(total(sp[psl].nch))+ $
        long(total(sp[ps_exclude].nrec*sp[ps_exclude].nch))
    delete_other_data=0
  endif
endelse
;
; set up arrays to hold new data
;
integs=fltarr(nsp,nozero=1)
toffs=fltarr(nsp,nozero=1)
noises=fltarr(nsp,nozero=1)
scales=fltarr(nsp,nozero=1)
wts_temp=fltarr(nsp,nozero=1)
ch_temp=complexarr(ndata,nozero=1)
pr_temp=make_array(nsp,/long,value=0L)
pc_temp=make_array(nsp,/long,value=0L)
;
; now find all the distinct combinations of nrec and nch since
; each must be processed separately in dat_get_rows
;
nrec_nch=10000L*sp[psl].nch+sp[psl].nrec
distinct_nrec_nch=uti_distinct(nrec_nch,ndistinct,/many_repeat)
nchs=distinct_nrec_nch/10000L & nrecs=distinct_nrec_nch-10000L*nchs
ch_ptr=0L & rec_ptr=0L
for i=0,n_elements(distinct_nrec_nch)-1 do begin
   res=dat_list(s_l,'"nch" eq "'+strtrim(string(nchs[i]),2)+ $
             '" and "nrec" eq "'+strtrim(string(nrecs[i]),2)+'"', $
             /reset,/no_notify)
   dat_get_rows,cmp,amp,pha,x,wts,first,npts,'channel',0,/list, $
             order='chan',average='int'
   for ii=0,n_elements(psl)-1 do begin
     index=pr[psl[ii]]+lindgen(sp[psl[ii]].nrec)
     integs[rec_ptr]=total(re.integs[index])
     toffs[rec_ptr]=total(re.toffs[index])/nrecs[i]
     noises[rec_ptr]=sp[psl[ii]].wt
     scales[rec_ptr]=total(re.scales[index])/nrecs[i]  
     wts_temp[rec_ptr]=total(re.wts[index])
     ch_temp[ch_ptr:ch_ptr+npts[ii]-1L]=cmp[first[ii]:first[ii]+npts[ii]-1L]
     pc_temp[psl[ii]]=ch_ptr 
     pr_temp[psl[ii]]=rec_ptr
     rec_ptr=rec_ptr+1L & ch_ptr=ch_ptr+npts[ii]
   endfor
   sp[psl].nrec=1
endfor
if delete_other_data then begin
  in=in[pil_save] & bl=bl[pbl_save] & sp=sp[psl_save]
  pc_temp=pc_temp[psl_save] 
  pr_temp=pr_temp[psl_save]
endif else begin
  for ii=0,n_elements(ps_exclude)-1 do begin
    npts=sp[ps_exclude[ii]].nch*sp[ps_exclude[ii]].nrec
    index=pr[ps_exclude[ii]]+lindgen(sp[ps_exclude[ii]].nrec)
    re_index=rec_ptr+lindgen(sp[ps_exclude[ii]].nrec)
    ch_temp[ch_ptr:ch_ptr+npts-1L]= $
          ch[pc[ps_exclude[ii]]:pc[ps_exclude[ii]]+npts-1L]
    integs[re_index]=re.integs[index]
    toffs[re_index]=re.toffs[index]
    noises[re_index]=re.noises[index]
    scales[re_index]=re.scales[index]
    wt_temp[re_index]=re.wts[index]
    pc_temp[ps_exclude[ii]]=ch_ptr
    pr_temp[ps_exclude[ii]]=rec_ptr
    rec_ptr=rec_ptr+sp[ps_exclude[ii]].nrec & ch_ptr=ch_ptr+npts[ii]
  endfor
endelse
ch=ch_temp
pc=pc_temp
pr=pr_temp
;
; reset all the filter and select pointers
;
pcf=pc
prf=pr
pcs=pc
prs=pr
pcl=pc
prl=pr
;
;  combine record headers into structure re
;
re={integs:integs,toffs:toffs,noises:noises,scales:scales,wts:wts_temp}
;
return,1

end
