function pro_mir_save,filename=filename
;
;Used to save the dataset whenever a disconnect from the ION server happens
;
; eg . result=pro_mir_save()
;
;Define common blocks
common wlm
common data_set
common global
common plo

if (keyword_set(filename) eq 0) then filename=e.dataset_dir+'/'+e.save_fn
if (e.campuslogin ne 'sma' and e.campuslogin ne 'cfa') then begin
  ; filename=e.dataset_dir+'/dataset/'+e.save_fn
  count=0 & result=findfile(e.dataset_dir+'/dataset/',count=count)
endif else begin
  newfilename='a'
  print,'All data and idl environment will be save to file: ',filename
  read,newfilename,prompt='Return for OK, or enter another file name: '
  if newfilename ne '' then filename = newfilename
  count=1
endelse

if count ne 0 then begin
    save,in,bl,sp,c,ch,re,ca,pi,pb,ps,pc,pr,pif,pbf, $
     	psf,pcf,prf,pis,pbs,pss,pcs,prs,pil,pbl,psl,pcl,prl, $
        wlm_ref_time,wlm_ntel,wlm_itel,       $
        wlm_times,wlm_id,wlm_raw,wlm_wts,     $
        wlm_start,wlm_stop,wlm_cal,           $
        wlm_gains,wlm_egains,wlm_xfactor,     $
        wlm_filter,wlm_icoherence,            $
        pl,s_page,gai,pas,con,spe,wlm,map,var, $
    	filename=filename
  return,1
endif
return,0

end
