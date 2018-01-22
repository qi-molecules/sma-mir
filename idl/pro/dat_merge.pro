function dat_merge,merge_filenames,save_filename
;
; Used to merge the datasets listed in merge_filenames array
; into a final savefile called save_filename 
;
;
; eg . result=dat_merge(['saveset1','saveset2'],'mergedsaveset')
; The files have to be in the e.data_set_dir/dataset/ directory
;
;Define common blocks
common wlm
common data_set
common global
common plo
dbprocess=e.dbprocess


filenames=reverse(merge_filenames)
count=0 & result=findfile(e.dataset_dir+'/dataset/',count=count)
if count eq 0 then begin
  print,'could not find directory :', $
                e.dataset_dir+'/dataset/'
  return,-1
endif


count_fn=0
for i=0,n_elements(filenames)-1 do begin
  result=findfile(e.dataset_dir+'/dataset/'+merge_filenames[i],count=count)
  count_fn=count_fn+count
  if count eq 0 then begin
    print,'could not find file :',e.dataset_dir+'/dataset/'+merge_filenames[i]
  endif else begin
    print,'found file :',e.dataset_dir+'/dataset/'+merge_filenames[i]
  endelse
endfor
if count_fn lt n_elements(filenames) then return,-1


; first save existing environment
    save,in,bl,sp,c,ch,re,ca,pi,pb,ps,pc,pr,pif,pbf, $
     	psf,pcf,prf,pis,pbs,pss,pcs,prs,pil,pbl,psl,pcl,prl, $
        wlm_ref_time,wlm_ntel,wlm_itel,       $
        wlm_times,wlm_id,wlm_raw,wlm_wts,     $
        wlm_start,wlm_stop,wlm_cal,           $
        wlm_gains,wlm_egains,wlm_xfactor,     $
        wlm_filter,wlm_icoherence,            $
        pl,s_page,gai,pas,con,spe,wlm,map,var, $
    	filename=e.dataset_dir+'/dataset/merge_initial'


ifn = 0
print,'Restoring dataset ',e.dataset_dir+'/dataset/'+filenames[ifn]
restore,filename=e.dataset_dir+'/dataset/'+filenames[ifn]

; put data from first set in _temp structures and arrays
; skip the ca structure since it is redefined at the end
; of this code as an empty structure

; arrays of structures
    in_temp=in  & bl_temp=bl  & sp_temp=sp  

; structure of arrays
; The re structure is different because it is a structure of
; arrays, not an array of structures. The re_temp array will 
; be an array of structures same as the other ones. Fix it
; at the end.
   re_temp = {integs:0.0,toffs:0.0,noises:0.0,scales:0.0,wts:0.0}
   re_template = re_temp
   for i = 0,n_tags(re)-1 do begin
     re_temp.(i) = re.(i)[0]
   endfor
   for j = 1,n_elements(re.integs)-1 do begin
     re_temp = [re_temp,re_template]
     for i = 0,n_tags(re)-1 do begin
       re_temp[n_elements(re_temp)-1].(i) = re.(i)[j]
     endfor
   endfor

;print,'n_elements(re.integs)',n_elements(re.integs)
;print,'n_elements(re_temp)  ',n_elements(re_temp) 
;print,'re_temp[0] ',re_temp[0] 
;for i = 0,n_tags(re)-1 do begin
;print,'re.(i)[0] ',re.(i)[0]
;endfor

; The c structure is a structure of arrays of different
; lengths. Have to copy each of the arrays separately. 
; First stuff them all in one big string array.

; Count the codes in each data set and each tag of the c structure
ncodes = intarr(n_elements(filenames),n_tags(c))

for i = 0,n_tags(c)-1 do begin
   ncodes[0,i] = n_elements(c.(i))
endfor
ncsets = total(ncodes,2)
;print,'totals ',total(ncodes,2)

str_temp = strarr(ncsets[0])

k = 0
for i = 0,n_tags(c)-1 do begin
  for j = 0,n_elements(c.(i))-1 do begin
   str_temp[k] = c.(i)[j]
   k = k+1
  endfor
endfor

; simple arrays
    ch_temp=ch    & pi_temp=pi    & pb_temp=pb  
    ps_temp=ps    & pc_temp=pc    & pr_temp=pr  
    pif_temp=pif  & pbf_temp=pbf  & psf_temp=psf  
    pcf_temp=pcf  & prf_temp=prf  & pis_temp=pis 
    pbs_temp=pbs  & pss_temp=pss  & pcs_temp=pcs  
    prs_temp=prs  & pil_temp=pil  & pbl_temp=pbl  
    psl_temp=psl  & pcl_temp=pcl  & prl_temp=prl    

; for each of the structures, make a template for concatenation
    in_template = in[0]
    bl_template = bl[0]
    sp_template = sp[0]

;print,'structure in_temp',in_temp[0]
;print,'n_elements(in_temp) ',n_elements(in_temp) 

; This will be the reference time for the merged data set
ref_time0 = c.ref_time[0]


; Starting from second file read in the files and append to _temp 

for ifn=1,n_elements(filenames)-1 do begin

  print,'Restoring dataset ',e.dataset_dir+'/dataset/'+filenames[ifn]
  restore,filename=e.dataset_dir+'/dataset/'+filenames[ifn]

; Make any changes to data before appending
; increase integer codes to ascii strings

  in.icocd=in.icocd+max([in_temp.icocd]) +1
  in.iut=in.iut+max([in_temp.iut]) +1
  in.iref_time=in.iref_time+max([in_temp.iref_time])  +1
  in.itq=in.itq+max([in_temp.itq]) +1
  in.ivctype=in.ivctype+max([in_temp.ivctype])+1
  bl.isb=bl.isb+max([bl_temp.isb]) +1
  bl.ipol=bl.ipol+max([bl_temp.ipol]) +1
  bl.iaq=bl.iaq+max([bl_temp.iaq]) +1
  bl.ibq=bl.ibq+max([bl_temp.ibq]) +1
  bl.icq=bl.icq+max([bl_temp.icq]) +1
  bl.ioq=bl.ioq+max([bl_temp.ioq]) +1
  bl.irec=bl.irec+max([bl_temp.irec]) +1
  bl.iifc=bl.iifc+max([bl_temp.iifc])  +1
  bl.itel1=bl.itel1+max([bl_temp.itel1]) +1
  bl.itel2=bl.itel2+max([bl_temp.itel2]) +1
  bl.iblcd=bl.iblcd+max([bl_temp.iblcd]) +1
  sp.igq=sp.igq+max([sp_temp.igq]) +1
  sp.ipq=sp.ipq+max([sp_temp.ipq]) +1
  sp.iband=sp.iband+max([sp_temp.iband]) +1
  sp.ipstate=sp.ipstate+max([sp_temp.ipstate]) +1
  sp.ivtype=sp.ivtype+max([sp_temp.ivtype]) +1
  sp.itaper=sp.itaper+max([sp_temp.itaper]) +1
  sp.itrans=sp.itrans+max([sp_temp.itrans])  +1
  in.isource=in.isource+max([in_temp.isource]) +1
  in.ipos=in.ipos+max([in_temp.ipos]) +1
  in.iofftype=in.iofftype+max([in_temp.iofftype])   +1
  in.ira=in.ira+max([in_temp.ira]) +1
  in.idec=in.idec+max([in_temp.idec])+1
  pc=pc+n_elements(ch_temp)+1
  pr=pr+n_elements(re_temp.toffs)+1

; did not worry about c.icode_s and c.icode_tag, may need to

; update bl.avedhrs, c.ref_time, in.iref_time, in.dhrs

   num_date = uti_sd_nd(ref_time0)
   ref_date_jul_temp = uti_jul_day(num_date[1],num_date[2],num_date[0]) -0.5d0
   num_date = uti_sd_nd(c.ref_time[0])
   ref_date_jul = uti_jul_day(num_date[1],num_date[2],num_date[0]) -0.5d0
   hr_diff=24.d0*(ref_date_jul-ref_date_jul_temp)
   c.ref_time=ref_time0
   bl.avedhrs=bl.avedhrs+hr_diff
   in.dhrs=in.dhrs+hr_diff
   in.iref_time[*]=in_temp[0].iref_time

; Finished all changes to the data, now start appending.
; The structures are more complicated because appeding to
; structures does not seem well supported in idl. Even if the
; structures are identical, the concatenation only works
; if the two structures to be concatenated are derived 
; from the same structure. There seems to be some sort of overly
; strict structure consistency check in idl that prevents
; easy concatenation or use of vector processing. But it
; can be circumvented by the following slow scalar method.

;print,'structure in_temp',in_temp[0]
;print,'n_elements(in_temp) ',n_elements(in_temp) 
;print,'n_elements(in) ',n_elements(in) 
;print,'n_tags(in) ',n_tags(in) 
;print,'n_tags(in_temp) ',n_tags(in_temp) 

   for j = 0,n_elements(in)-1 do begin
     in_temp = [in_temp,in_template]
     for i = 0,n_tags(in)-1 do begin
       in_temp[n_elements(in_temp)-1].(i) = in[j].(i)
     endfor
   endfor

;print,'structure in',in[n_elements(in)-1]
;print,'n_elements(in) ',n_elements(in) 
;print,'structure in_temp',in_temp[n_elements(in_temp)-1]
;print,'n_elements(in_temp) ',n_elements(in_temp) 

   for j = 0,n_elements(bl)-1 do begin
     bl_temp = [bl_temp,bl_template]
     for i = 0,n_tags(bl)-1 do begin
       bl_temp[n_elements(bl_temp)-1].(i) = bl[j].(i)
     endfor
   endfor

   for j = 0,n_elements(sp)-1 do begin
     sp_temp = [sp_temp,sp_template]
     for i = 0,n_tags(sp)-1 do begin
       sp_temp[n_elements(sp_temp)-1].(i) = sp[j].(i)
     endfor
   endfor

   for j = 1,n_elements(re.integs)-1 do begin
     re_temp = [re_temp,re_template]
     for i = 0,n_tags(re)-1 do begin
       re_temp[n_elements(re_temp)-1].(i) = re.(i)[j]
     endfor
   endfor

for i = 0,n_tags(c)-1 do begin
   ncodes[ifn,i] = n_elements(c.(i))
endfor
ncsets = total(ncodes,2)
print,'totals ',total(ncodes,2)

str_temp = [str_temp,strarr(ncsets[ifn])]

k = ncsets[ifn-1]
for i = 0,n_tags(c)-1 do begin
  for j = 0,n_elements(c.(i))-1 do begin
   str_temp[k] = c.(i)[j]
   k = k+1
  endfor
endfor


   ch_temp=[ch_temp,ch]  & pi_temp=[pi_temp,pi] & pb_temp=[pb_temp,pb]   
   ps_temp=[ps_temp,ps]  & pc_temp=[pc_temp,pc]  & pr_temp=[pr_temp,pr]  
   pif_temp=[pif_temp,pif]  & pbf_temp=[pbf_temp,pbf] 
   psf_temp=[psf_temp,psf] & pcf_temp=[pcf_temp,pcf]   
   prf_temp=[prf_temp,prf]  & pis_temp=[pis_temp,pis] 
   pbs_temp=[pbs_temp,pbs]  & pss_temp=[pss_temp,pss]  
   pcs_temp=[pcs_temp,pcs] & prs_temp=[prs_temp,prs]  
   pil_temp=[pil_temp,pil]  & pbl_temp=[pbl_temp,pbl]  
   psl_temp=[psl_temp,psl]  & pcl_temp=[pcl_temp,pcl]  
   prl_temp=[prl_temp,prl] 
endfor

;global plot parameters

pl_par={active:0,plot_device:'x',plot_interact:1,plot_file:'a',plot_date:'a', $
	plot_zoom:0, mouse_x:0.0,mouse_y:0.0,mouse_button:'l', $
	plot_copy:0,plot_panel:-1,plot_key:'',plot_done:0, $
	num_pages:0,plot_type:''}
pl=replicate(pl_par,15)

gai_par={initial:0} & pas_par={initial:0} & con_par={initial:0}
spe_par={initial:0} & wlm_par={initial:0} & var_par={initial:0}
con=0 & gai=0 & pas=0 & spe=0 & spe=0 & wlm=0 & var=0	
s_page=0 & map=0
wlm_ref_time=0 & wlm_ntel=0 & wlm_itel=0 & wlm_times=0 & wlm_id=0 & wlm_raw=0 
wlm_wts=0 & wlm_start=0 & wlm_stop=0 & wlm_cal=0 & wlm_gains=0 & wlm_egains=0
wlm_xfactor=0 & wlm_filter = '' & wlm_icoherence=0
;
; put in empty ca (cal) structure
;
  ca={cal_type:strarr(1),x_var:strarr(1),y_var:strarr(1), $
    tel_bsl:strarr(1),inhid_beg:lonarr(1), $
    inhid_end:lonarr(1),blcd_tel:strarr(1), $
    iblcd_itel:intarr(1),rec:strarr(1),irec:intarr(1), $
    isb:intarr(1),band:strarr(1),iband:intarr(1), $
    sb:strarr(1),pc_xbeg:lonarr(1),pc_xend:lonarr(1), $
    pc_ybeg:lonarr(1),pc_yend:lonarr(1),cal_parm:fltarr(1), $
    x:make_array(1,/float,value=!values.f_nan), $
    y:make_array(1,/float,value=!values.f_nan), $
    cal_exist:0}
;
;  sort the structures
;
;j=sort(in_temp.inhid)
;in=in_temp(j)
;j=sort(bl_temp.blhid)
;bl=bl_temp(j)
;j=sort(sp_temp.sphid)
;sp=sp_temp(j)
in=in_temp
bl=bl_temp
sp=sp_temp
ch=ch_temp

; Define a new re structure as a structure of arrays
; and copy the re_temp array of structures containing the
; data into it.
re={integs:fltarr(n_elements(re_temp)),$
     toffs:fltarr(n_elements(re_temp)),$
    noises:fltarr(n_elements(re_temp)),$
    scales:fltarr(n_elements(re_temp)),$
       wts:fltarr(n_elements(re_temp)) }
re.integs=re_temp.integs
re.toffs =re_temp.toffs
re.noises=re_temp.noises
re.scales=re_temp.scales
re.wts   =re_temp.wts   

; Redefine the c structure with the necessary dimensions
nctags = total(ncodes,1)
ncsets = total(ncodes,2)
print,'nctags ',nctags
c = { $
     cocd:strarr(nctags[0]),ut:strarr(nctags[1]), $
     ref_time:strarr(nctags[2]), $
     tq:strarr(nctags[3]),vctype:strarr(nctags[4]),$
     sb:strarr(nctags[5]),pol:strarr(nctags[6]),$
     aq:strarr(nctags[7]),bq:strarr(nctags[8]),$
     cq:strarr(nctags[9]),oq:strarr(nctags[10]),$
     rec:strarr(nctags[11]),$
     ifc:strarr(nctags[12]),tel1:strarr(nctags[13]),$
     tel2:strarr(nctags[14]),blcd:strarr(nctags[15]),$
     gq:strarr(nctags[16]),pq:strarr(nctags[17]),$
     band:strarr(nctags[18]),pstate:strarr(nctags[19]),$
     vtype:strarr(nctags[20]),taper:strarr(nctags[21]),$
     trans:strarr(nctags[22]),$
     source:strarr(nctags[23]),pos:strarr(nctags[24]),$
     offtype:strarr(nctags[25]),$
     ra:strarr(nctags[26]),dec:strarr(nctags[27]),$
     icode_s:strarr(nctags[28]),icode_tag:strarr(nctags[29])}

k = 0
m = intarr(n_tags(c))
for n = 0,n_elements(filenames)-1 do begin
for i = 0,n_tags(c)-1 do begin
  for j = 0,ncodes[n,i]-1 do begin
   aa = str_temp[k]
   c.(i)[ j + m[i] ] = aa
   k = k+1
  endfor
  m[i] = m[i] + ncodes[n,i]
endfor
endfor

;
; set up pointer arrays for correspondence from sp to in and bl
; set up a similar one for sp even though it is not needed
;
pi = make_array(n_elements(sp),/int,value=0)
pb = make_array(n_elements(sp),/int,value=0)
ps = make_array(n_elements(sp),/int,value=0)
in_prev=-1L
bl_prev=-1L
in_index=0L
bl_index=0L
in_max=n_elements(in)-1L
bl_max=n_elements(bl)-1L
for i=0L,(n_elements(sp)-1L) do begin
  if (sp(i).inhid ne in_prev) then begin
    in_upper= min([in_index+2L,in_max])
    in_index = in_index + $
           max([where(in(in_index:in_upper).inhid eq sp(i).inhid,count)])
    if (count le 0) then in_index = max([where(in.inhid eq sp(i).inhid)])
    in_prev = sp(i).inhid
  endif
  pi(i) = in_index
  if (sp(i).blhid ne bl_prev) then begin
    bl_index= max([0L,bl_index-45L])
    bl_upper= min([bl_index+90L,bl_max])
    bl_index = bl_index + $
           max([where(bl(bl_index:bl_upper).blhid eq sp(i).blhid,count)])
    if (count le 0) then bl_index = max([where(bl.blhid eq sp(i).blhid)])
    bl_prev = sp(i).blhid
  endif
  pb(i) = bl_index
  ps(i) = i
endfor
  pif=pi
  pbf=pb
  psf=ps
  pis=pi
  pbs=pb
  pss=ps
  pil=pi
  pbl=pb
  psl=ps
  pcf=pc
  prf=pr
  pcs=pc
  prs=pr
  pcl=pc
  prl=pr

; save this merged file
;
if e.debug then print,'saving merged file ', $
        e.dataset_dir+'/dataset/'+save_filename
    save,in,bl,sp,c,ch,re,ca,pi,pb,ps,pc,pr,pif,pbf, $
     	psf,pcf,prf,pis,pbs,pss,pcs,prs,pil,pbl,psl,pcl,prl, $
        wlm_ref_time,wlm_ntel,wlm_itel,       $
        wlm_times,wlm_id,wlm_raw,wlm_wts,     $
        wlm_start,wlm_stop,wlm_cal,           $
        wlm_gains,wlm_egains,wlm_xfactor,     $
        wlm_filter,wlm_icoherence,            $
        pl,s_page,gai,pas,con,spe,wlm,map,var, $
    	filename=e.dataset_dir+'/dataset/'+save_filename

;
; lastly, restore initial environment and remove file
;
restore,filename=e.dataset_dir+'/dataset/merge_initial'
result=fil_remove(e.dataset_dir +'/dataset/merge_initial')
e.dbprocess=dbprocess
return,1

end
