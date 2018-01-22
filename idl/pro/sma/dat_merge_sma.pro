function dat_merge_sma,merge_filenames,save_filename,newformat=newformat, sametrack=sametrack
;
; Used to merge the datasets listed in merge_filenames array
; into a final savefile called save_filename 
;
;
; eg . result=dat_merge_sma(['saveset1','saveset2'],'mergedsaveset')
; The files have to be in the e.data_set_dir directory
;
;Define common blocks
common wlm
common data_set
common global
common plo
dbprocess=e.dbprocess

if keyword_set(newformat) then msfile=e.idl_sav+'ms_newformat.save' else msfile=e.idl_sav+'ms.save'
restore,msfile

;filenames=reverse(merge_filenames)
filenames=merge_filenames
count=0 & result=findfile(e.dataset_dir,count=count)
if count eq 0 then begin
  print,'could not find directory :', $
                e.dataset_dir
  return,-1
endif


count_fn=0
for i=0,n_elements(filenames)-1 do begin
  result=findfile(e.dataset_dir+merge_filenames[i],count=count)
  count_fn=count_fn+count
  if count eq 0 then begin
    print,'could not find file :',e.dataset_dir+merge_filenames[i]
  endif else begin
    print,'found file :',e.dataset_dir+merge_filenames[i]
  endelse
endfor
if count_fn lt n_elements(filenames) then return,-1

ifn = 0
print,'Restoring dataset ',e.dataset_dir+filenames[ifn]
restore,filename=e.dataset_dir+filenames[ifn]

if n_elements(ch) ne (max(pc)+sp(max(ps)).nch) then begin
   print, 'DATA INCONSISTENCY, READ '+filenames[ifn]+' WITH READDATA_LONG'
   return,-1
endif

; put data from first set in _temp structures and arrays
; skip the ca structure since it is redefined at the end
; of this code as an empty structure

; arrays of structures
    in_temp=in  & bl_temp=bl  & sp_temp=sp  

; structure of arrays
; The re structure is different because it is a structure of
; arrays, not an array of structures. 

re_temp=re

; The c structure is a structure of arrays of different
; lengths. Have to copy each of the arrays separately. 
; First stuff them all in one big string array.

; Count the codes in each data set and each tag of the c structure
ncodes = intarr(n_elements(filenames),n_tags(c))

for i = 0,n_tags(c)-1 do begin
   ncodes[0,i] = n_elements(c.(i))
endfor
ncsets = total(ncodes,2)
nrsets = total(ncodes,1)
;print,'totals ',total(ncodes,2)

str_temp = strarr(ncsets[0])

k = 0
for i = 0,n_tags(c)-1 do begin
  for j = 0,n_elements(c.(i))-1 do begin
   str_temp[k] = c.(i)[j]
   k = k+1L
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

  print,'Restoring dataset ',e.dataset_dir+filenames[ifn]
  restore,filename=e.dataset_dir+filenames[ifn]

  if (ifn ne n_elements(filenames)-1) and (n_elements(ch) ne (max(pc)+sp(max(ps)).nch)) then begin
     print, 'DATA INCONSISTENCY, READ '+filenames[ifn]+' WITH READDATA_LONG'
     return,-1
  endif

; Make any changes to data before appending
; increase integer codes to ascii strings

if not keyword_set(sametrack) then begin

if not keyword_set(newformat) then begin
;  in.icocd=in.icocd+nrsets[0]
  in.iut=in.iut+nrsets[1]
  in.iref_time=in.iref_time+nrsets[2]
;  in.itq=in.itq+nrsets[3]
;  in.ivctype=in.ivctype+nrsets[4] 
;  bl.isb=bl.isb+nrsets[5]
;  bl.ipol=bl.ipol+nrsets[6]
;  bl.iaq=bl.iaq+nrsets[7]
;  bl.ibq=bl.ibq+nrsets[8]
;  bl.icq=bl.icq+nrsets[9]
;  bl.ioq=bl.ioq+nrsets[10]
;  bl.irec=bl.irec+nrsets[11]
;  bl.iifc=bl.iifc+nrsets[12]
  bl.itel1=bl.itel1+nrsets[13]
  bl.itel2=bl.itel2+nrsets[14]
  bl.iblcd=bl.iblcd+nrsets[15]
;  sp.igq=sp.igq+nrsets[16]
;  sp.ipq=sp.ipq+nrsets[17]
;  sp.iband=sp.iband+nrsets[18]
;  sp.ipstate=sp.ipstate+nrsets[19]
;  sp.ivtype=sp.ivtype+nrsets[20]
;  sp.itaper=sp.itaper+nrsets[21]
;  sp.itrans=sp.itrans+nrsets[22]
  in.isource=in.isource+nrsets[23]
;  in.ipos=in.ipos+nrsets[24]
;  in.iofftype=in.iofftype+nrsets[25]
  in.ira=in.ira+nrsets[26]
  in.idec=in.idec+nrsets[27]
endif else begin
  in.iut=in.iut+nrsets[0]
  in.iref_time=in.iref_time+nrsets[1]
;  bl.isb=in.itq+nrsets[2]
;  bl.ipol=bl.ipol+nrsets[3]
;  bl.irec=bl.irec+nrsets[4]
  bl.itel1=bl.itel1+nrsets[5]
  bl.itel2=bl.itel2+nrsets[6]
  bl.iblcd=bl.iblcd+nrsets[7]
;  sp.igq=sp.igq+nrsets[8]
;  sp.ipq=sp.ipq+nrsets[9]
;  sp.iband=sp.iband+nrsets[10]
;  sp.ipstate=sp.ipstate+nrsets[11]
  in.isource=in.isource+nrsets[12]
;  in.ivrad=in.iofftype+nrsets[13]
  in.ira=in.ira+nrsets[14]
  in.idec=in.idec+nrsets[15]
endelse

endif

  in.int = in.int+max([in_temp.int]) + 1L
  in.inhid = in.inhid+max([in_temp.inhid])+1L
  bl.blhid = bl.blhid+max([bl_temp.blhid])+1L
  bl.inhid = bl.inhid+max([bl_temp.inhid])+1L
  sp.sphid = sp.sphid+max([sp_temp.sphid])+1L
  sp.blhid = sp.blhid+max([sp_temp.blhid])+1L
  sp.inhid = sp.inhid+max([sp_temp.inhid])+1L
;  pc=pc+n_elements(ch_temp)
;  pr=pr+n_elements(re_temp.toffs)
;  pc=pc+max([pc_temp])+sp_temp[max([ps_temp])].nch
  pc=pc+n_elements(ch_temp)
  pr=pr+max([pr_temp])+1L
  pb=pb+max([pb_temp])+1L
  ps=ps+max([ps_temp])+1L
  pi=pi+max([pi_temp])+1L

; print,'n_elements(re_temp.toffs)= ',n_elements(re_temp.toffs)

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
; can be circumvented by the slow scalar method.
; idl5.4 has already relaxed the rule. so no need of scalar method. 

in_temp=[in_temp,in]

bl_temp=[bl_temp,bl]

sp_temp=[sp_temp,sp]

re_temp={integs:[re_temp.integs,re.integs], $
         toffs:[re_temp.toffs,re.toffs], $
         noises:[re_temp.noises,re.noises], $
         scales:[re_temp.scales,re.scales], $
         wts:[re_temp.wts,re.wts]}

for i = 0,n_tags(c)-1 do begin
   ncodes[ifn,i] = n_elements(c.(i))
endfor
ncsets = total(ncodes,2)
nrsets = total(ncodes,1)
;print,'***** ncodes ****:'
;print,ncodes

str_temp = [str_temp,strarr(ncsets[ifn])]

;k = ncsets[ifn-1]
for i = 0,n_tags(c)-1 do begin
   for j = 0,n_elements(c.(i))-1 do begin
;      if ((i eq 1) or (i eq 2) or (i eq 8) or (i eq 13) or (i eq 14) or (i eq 15) or (i eq 23) or (i eq 26) or (i eq 27)) then begin
         str_temp[k] = c.(i)[j]
;   if i eq 23 then print,'source is: ', str_temp[k]
         k = k+1L
;      endif
  endfor
endfor

;print, '*****str_temp*****:'
;print, str_temp

   ch_temp=[ch_temp,ch]  & pi_temp=[pi_temp,pi] & pb_temp=[pb_temp,pb]   
   ps_temp=[ps_temp,ps]  & pc_temp=[pc_temp,pc]  & pr_temp=[pr_temp,pr]  
;   pif_temp=[pif_temp,pif]  & pbf_temp=[pbf_temp,pbf] 
;   psf_temp=[psf_temp,psf] & pcf_temp=[pcf_temp,pcf]   
;   prf_temp=[prf_temp,prf]  & pis_temp=[pis_temp,pis] 
;   pbs_temp=[pbs_temp,pbs]  & pss_temp=[pss_temp,pss]  
;   pcs_temp=[pcs_temp,pcs] & prs_temp=[prs_temp,prs]  
;   pil_temp=[pil_temp,pil]  & pbl_temp=[pbl_temp,pbl]  
;   psl_temp=[psl_temp,psl]  & pcl_temp=[pcl_temp,pcl]  
;   prl_temp=[prl_temp,prl] 
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
pc=pc_temp
pr=pr_temp
ps=ps_temp
pi=pi_temp
pb=pb_temp

re=re_temp

if not keyword_set(sametrack) then begin
print,'Redefine the c structure...'

; Redefine the c structure with the necessary dimensions
nctags = total(ncodes,1)
ncsets = total(ncodes,2)
if not keyword_set(newformat) then begin
c = { $
     cocd:strarr(ncodes[0,0]),ut:strarr(nctags[1]), $
     ref_time:strarr(nctags[2]), $
     tq:strarr(ncodes[0,3]),vctype:strarr(ncodes[0,4]),$
     sb:strarr(ncodes[0,5]),pol:strarr(ncodes[0,6]),$
     aq:strarr(ncodes[0,7]),bq:strarr(ncodes[0,8]),$
     cq:strarr(ncodes[0,9]),oq:strarr(ncodes[0,10]),$
     rec:strarr(ncodes[0,11]),$
     ifc:strarr(ncodes[0,12]),tel1:strarr(nctags[13]),$
     tel2:strarr(nctags[14]),blcd:strarr(nctags[15]),$
     gq:strarr(ncodes[0,16]),pq:strarr(ncodes[0,17]),$
     band:strarr(ncodes[0,18]),pstate:strarr(ncodes[0,19]),$
     vtype:strarr(ncodes[0,20]),taper:strarr(ncodes[0,21]),$
     trans:strarr(ncodes[0,22]),$
     source:strarr(nctags[23]),pos:strarr(ncodes[0,24]),$
     offtype:strarr(ncodes[0,25]),$
     ra:strarr(nctags[26]),dec:strarr(nctags[27]),$
     icode_s:strarr(ncodes[0,28]),icode_tag:strarr(ncodes[0,29])}

     ichange=[1,2,13,14,15,23,26,27]

endif else begin

c = { $
     ut:strarr(nctags[0]), $
     ref_time:strarr(nctags[1]), $
     sb:strarr(ncodes[0,2]),pol:strarr(ncodes[0,3]),$
     rec:strarr(ncodes[0,4]),tel1:strarr(nctags[5]),$
     tel2:strarr(nctags[6]),blcd:strarr(nctags[7]),$
     gq:strarr(ncodes[0,8]),pq:strarr(ncodes[0,9]),$
     band:strarr(ncodes[0,10]),pstate:strarr(ncodes[0,11]),$
     source:strarr(nctags[12]),vrad:strarr(ncodes[0,13]),$
     ra:strarr(nctags[14]),dec:strarr(nctags[15]),$
     icode_s:strarr(ncodes[0,16]),icode_tag:strarr(ncodes[0,17])}
    
     ichange=[0,1,5,6,7,12,14,15]

endelse

k = 0
m = intarr(n_tags(c))
for i = 0,n_tags(c)-1 do begin
   for j = 0,ncodes[0,i]-1 do begin
      aa = str_temp[k]
      c.(i)[ j + m[i] ] = aa
      k = k+1L
   endfor
   m[i] = m[i] + ncodes[0,i]
endfor


for n = 1,n_elements(filenames)-1 do begin
   for i = 0,n_tags(c)-1 do begin
      for j = 0,ncodes[n,i]-1 do begin
         aa = str_temp[k]
         k=k+1L
;         if ((i eq 1) or (i eq 2) or (i eq 13) or (i eq 14) or (i eq
;         15) or (i eq 23) or (i eq 26) or (i eq 27)) then begin 
         temp=where(ichange eq i, count)
         if count gt 0 then begin
            c.(i)[ j + m[i] ] = aa
         endif
      endfor
;   if i eq 23 then print,c.(i)
;      if ((i eq 1) or (i eq 2) or (i eq 13) or (i eq 14) or (i eq 15) or (i eq 23) or (i eq 26) or (i eq 27)) then begin       
      temp=where(ichange eq i, count)
      if count gt 0 then begin
         m[i] = m[i] + ncodes[n,i]
      endif
   endfor
 ;print,'***** m = ******'
 ;print,m
endfor

endif

pif=pi
pbf=pb
psf=ps
pis=pi
pbs=pb
pss=ps
pcf=pc
prf=pr
pcs=pc
prs=pr
pil=pi
pbl=pb
psl=ps
pcl=pc
prl=pr

distinct_source=uti_distinct(c.source[in[pil].isource],nsources,/many_repeat)
for is=0L, nsources -1L do begin
  result=dat_list(s_l,'"source" eq "'+distinct_source[is]+'"',/reset,/no_notify)
  in[pil].isource=min(in[pil].isource)
  in[pil].souid=in[pil].isource
endfor
distinct_blcd=uti_distinct(c.blcd[bl[pbl].iblcd],nblcds,/many_repeat)
for is=0L, nblcds -1L do begin
  result=dat_list(s_l,'"blcd" eq "'+distinct_blcd[is]+'"',/reset,/no_notify)
  bl[pbl].itel1=min(bl[pbl].itel1)
  bl[pbl].itel2=min(bl[pbl].itel2)
  bl[pbl].iblcd=min(bl[pbl].iblcd)
  bl[pbl].blsid=min(bl[pbl].iblcd)
endfor

pil=pi
pbl=pb
psl=ps
pcl=pc
prl=pr

; save this merged file
;
if e.debug then print,'saving merged file ', $
        e.dataset_dir+save_filename
    save,in,bl,sp,c,ch,re,ca,pi,pb,ps,pc,pr,pif,pbf, $
     	psf,pcf,prf,pis,pbs,pss,pcs,prs,pil,pbl,psl,pcl,prl, $
        wlm_ref_time,wlm_ntel,wlm_itel,       $
        wlm_times,wlm_id,wlm_raw,wlm_wts,     $
        wlm_start,wlm_stop,wlm_cal,           $
        wlm_gains,wlm_egains,wlm_xfactor,     $
        wlm_filter,wlm_icoherence,            $
        pl,s_page,gai,pas,con,spe,wlm,map,var, $
    	filename=e.dataset_dir+save_filename

;
; lastly, restore initial environment and remove file
;
;restore,filename=e.dataset_dir+'/dataset/merge_initial'
;result=fil_remove(e.dataset_dir +'/dataset/merge_initial')
e.dbprocess=dbprocess
print,'Data merging done !'
return,1

end
