pro mir_save,fn,filename=filename, nowait=nowait, new=new
;yes
;=Task:MIR_SAVE --- To save the dataset in IDL format.
;#Type: i/o
;+Use:
;      After some repairs and calibrations on the data, the data set 
;      and all the environment variables of MIR can be saved to disk
;      with the MIR_SAVE commnad in IDL format. MIR by default saves
;      data to the current directory, assuming mysaveset is a filename
;      unless the filename begins with a slash (/). In this case MIR
;      assumes that the address is  the full pathname. If no filename
;      is specified, MIR will make up a filename based on the date
;      and time. This is convenient for unstable systems since the
;      files are  named sequentially. The saved data can be restored
;      by the command MIR_RESORE
;@filename:
;      The filename of the data to be saved. For example, to save the
;      file 'mysaveset', you can try
;      >mir_save,'mysaveset'   or
;      >mir_save,'mysaveset'   or
;      >mir_save,filename='mysaveset'
;&history:
;--------------------------------------------------------------------
;      cykuo 25dec03 adapting the header
;--------------------------------------------------------------------

;Define common blocks
common wlm
common data_set
common global
common plo

if (e.campuslogin ne 'sma' and e.campuslogin ne 'cfa') then begin

; this section for caltech/ovro

  filename=e.dataset_dir+'/dataset/'+e.save_fn
  test = CALL_EXTERNAL('db.so', 'db_check_dataset_dir', e.dataset_dir)
  if(test EQ -1) then begin
      print, "Unable to create dataset directory"
      return
  endif

endif  else begin        

;this section for cfa/sma 

;if there was no filename on the command line then make up a name
  if (keyword_set(filename) eq 0 and n_params() eq 0) then begin
;    date_now = systime(0)
;    parts=strtrim(strsplit(date_now,' ',/extract),2L)
;    parts=parts(where (parts ne ''))
;    parts(3)=strmid(parts(3),0,strpos(parts(3),':',/reverse_search))
;    e.save_fn='saveset'
;    for i=0L,n_elements(parts)-2 do begin
;       e.save_fn=e.save_fn+'_'+parts(i)
;    endfor
     parts=strsplit(systime(0),/extract)
     e.save_fn=strjoin(['saveset',parts[[4,1,2,3]]],'_')

  endif

if (n_params() ne 0 ) then e.save_fn = fn
if (keyword_set(filename) eq 1) then e.save_fn = filename
if (strmid(e.save_fn,0,1) eq '/') then begin
  filename=e.save_fn
endif else begin
  filename=e.dataset_dir+e.save_fn
endelse


  print,'All data and the idl environment will be saved to file: '
  print,filename
  newfilename='a'
  if keyword_set(nowait) then newfilename=filename else read,newfilename,prompt='Return for OK, or enter another file name: '
  if newfilename ne '' then begin
    length = strlen(newfilename)
    if (strmid(newfilename,0,1) eq "'") then  $
      newfilename = strmid(newfilename,1,length-1)
    if (strmid(newfilename,length-2,1) eq "'") then  $
      newfilename = strmid(newfilename,0,length-2)

    filename = newfilename
    e.save_fn = filename
  endif

endelse

if keyword_set(new) then begin
   temp_pc=long64(total(sp[psl].nch,/cum,/double))
   ptr=make_array(total(sp(psl).nch,/double),/l64,/nozero)
   iptr_end=long64(-1) & nch_prev=-1L
   for j=0L,n_elements(psl)-1L do begin 
     iptr=iptr_end+1L & iptr_end=iptr+sp[psl[j]].nch-1L
     if sp[psl[j]].nch ne nch_prev then lin_nch=lindgen(sp[psl[j]].nch)
     nch_prev=sp[psl[j]].nch 
     ptr[iptr:iptr_end]=pcl[j]+lin_nch 
   endfor
   
   ch=ch[ptr]
   pc=[0,temp_pc[0:n_elements(psl)-2]]
   in=in[0:max(pil)]
   bl=bl[0:max(pbl)]
   sp=sp[psl]
   re.integs=re.integs[prl]
   re.toffs=re.toffs[prl]
   re.noises=re.noises[prl]
   re.scales=re.scales[prl]
   re.wts=re.wts[prl]

   pi = pil
   pb = pbl
   ps = lindgen(n_elements(sp))
   pif=pi
   pbf=pb
   psf=ps
   prf=ps
   pis=pi
   pbs=pb
   pss=ps
   prs=ps
   pil=pi
   pbl=pb
   psl=ps
   prl=ps
   pr=ps
   pcf=pc
   pcs=pc
   pcl=pc
endif

save,in,bl,sp,c,ch,re,ca,pi,pb,ps,pc,pr,pif,pbf, $
     	psf,pcf,prf,pis,pbs,pss,pcs,prs,pil,pbl,psl,pcl,prl, $
        wlm_ref_time,wlm_ntel,wlm_itel,       $
        wlm_times,wlm_id,wlm_raw,wlm_wts,     $
        wlm_start,wlm_stop,wlm_cal,           $
        wlm_gains,wlm_egains,wlm_xfactor,     $
        wlm_filter,wlm_icoherence,            $
        pl,s_page,gai,pas,con,spe,wlm,map,var, $
    	filename=filename,/verbose

print,'If there were no error messages, the data was saved to: '
print,filename
return

end
