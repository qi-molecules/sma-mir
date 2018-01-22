function dat_chan_fix
; 
; Fix errors in the channel data.
; 1) removes spectral records with scale factors indicating a
;    'band d' problem.
;
; parameters : none
; result = -1 (error), 1(ok)
; 
; eg. : result=dat_chan_fix()
; 
; 
;
common global
common data_set
  
result=dat_list(s_l,/reset)
sp_prev=-1L
sp_index=0L
sp_max=n_elements(sp)-1L
for i=0L,(n_elements(psl)-1L) do begin
   j=psl[i]
   if sp[j].nch gt 1 then begin
     scalemin=min([re.scales[prl[i]:prl[i]+sp[j].nrec-1L]])
     scalemax=scalemin*12.
     js=where(re.scales[prl[i]:prl[i]+sp[j].nrec-1L] gt scalemax,count)
     if count gt 0 then begin
       print,'remove inh#,rec ',sp[j].inhid,js
       js=[js]+prl[i]
       old_sum_wt=total(re.wts[prl[i]:prl[i]+sp[j].nrec-1L])
       re.scales[js]=0.
       jss=[where(re.scales[prl[i]:prl[i]+sp[j].nrec-1L] gt 0.,count)]+prl[i]
       re.wts[js]=0.
       new_sum_wt=total(re.wts[jss])
       sp[j].wt=sp[j].wt*new_sum_wt/old_sum_wt
       j=sp[j].nrec*lindgen(sp[j].nch)+pcl(i)
       for ii=0,n_elements(js)-1L do begin
         jss=j+js(ii)-prl[i]
         ch(jss)=0.
       endfor
     endif
   endif
endfor
return,0
end


