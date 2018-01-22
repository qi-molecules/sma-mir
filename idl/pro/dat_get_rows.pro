pro dat_get_rows,cmp,amp,pha,x,wt,pt_first,pt_npts,xtype, $
             js,sphid=sphid,list=list,order=order,amp_pha=amp_pha, $
             normalize=normalize,average=average,x2=x2,y_lab=y_lab 
;
; Gets rows of record/channel complex and amp/pha data and averages.
; Operates in two modes : list set => get all rows in list pointers
;                         list not set => get psl(js)
; For cases of averageing, the routine assumes that all rows have the
; same number of channels (ie. the calling routine has done pre-sorting).  
; 
; parameters : cmp      -- complex array (output) 
;              amp      -- amp array (output if amp_pha set) 
;              pha      -- pha array (output if amp_pha set) 
;              x        -- x-coord of each pt (output)
;              wt       -- weight of each point (one values for each
;                          and every data value
;              pt_first -- first point in amp, pha, ... arrays for ouput row j
;              pt_npts  -- number of points for each output row
;              xtype    -- 'int' => x-coord is int+record/nrec
;                          'channel' => x-coord is ch# (for spectrometer)
;                          'hours' => x-ccord is dec. hrs since ref_time
;                          'fsky' => x-ccord is fsky (for spectrometer)
;                          'velocity' => x-ccord is velocity (for spectrometer)
;                          none of above => don't bother computing x's
;              js       -- keyword with indexes for those rows in psl
;                          [ie. psl(js)] which one wants.
;              sphid    -- keyword : get the spectrum corresponding to sphid 
;              list     -- keyword : if set, get entire list pointer set
;                          all rows in psl are returned
;              order    -- return output in 'rec' order or 'chan' order
;              normalize-- normalize to continuum vis in each integration
;              average  -- keyword to average over 'int', or 'all'
;                          if not set, return individual records
;              amp_pha  -- keyword to compute amp and phase in addition 
;                          to complex vis
;              x2       -- keyword to return record number in x2
;              y_lab    -- keyword for array to y labels for rows
;
; the output is arranged : ch1_rec1_int1 , ch1_rec2_int1 , ...
;                          ch2_rec1_int1 , ch2_rec2_int1 , ...
;                          ...
;                          ch1_rec1_int2 , ch1_rec2_int2 , ...
;
; except if average='all', all int's and rec's are averaged and 
;        if average='int', all rec's w/i each int are averaged.
;        if order='chan' , the record and channel order are swapped
;  
;       get the all complex cont. rec. in list pointers :
; eg. : dat_get_rows,cmp,amp,pha,x,wt,pt_first,pt_npts,'int',0,/list 
;       to also get amp/pha in the list pointers,
; eg. : dat_get_rows,cmp,amp,pha,x,wt,pt_first,pt_npts,'int',0,/amp_pha,/list
;       to get rows with index js,
;       js=0  
; eg. : dat_get_rows,cmp,amp,pha,x,wt,pt_first,pt_npts,'',js,/amp_pha
;       to average the records in each integration,
; eg. : dat_get_rows,cmp,amp,pha,x,wt,pt_first,pt_npts,'int',js,average='int', $
;               /amp_pha
;
common global
common data_set
if not keyword_set(average) then average=0
if not keyword_set(order) then order=0
ct=0
if keyword_set(sphid) then js=where(sp[psl].sphid eq sphid,ct)
if keyword_set(list) and ct eq 0 then js=lindgen(n_elements(psl))
;
; set up number of channels, records and total number of points
;
  nrows=n_elements(js)
  nch=sp(psl(js)).nch
  nrec=sp(psl(js)).nrec
  npts=long(nrec*nch)
  if not keyword_set(average) then begin
    npts_out=total(npts)
  endif
  if keyword_set(average) then begin
    nch=min([nch]) & npts=max([nch])
    if nch ne npts then goto, channel_change
;    if string(average) eq 'int' then begin
;      inhids_distinct=uti_distinct(in(pil(js)).inhid,sample=4,nints)
;      npts_out=nints*nch
;    endif else npts_out=nrows*npts
    npts_out=nrows*npts
  endif
  cmp=make_array(npts_out,/complex,/nozero)
  x=make_array(npts_out,/double,/nozero)
  if keyword_set(x2) then x2=make_array(npts_out,/double,value=1.)
  row=-1
  wt=make_array(npts_out,/float,/nozero)
  pt_first=make_array(n_elements(js),/long,/nozero)
  pt_npts=make_array(n_elements(js),/long,/nozero)
  tot_wt=0.
  for j=0L,nrows-1L do begin
    tot_wt=tot_wt+total(re.wts(prl(js[j]):prl(js[j])+nrec[j]-1L))
  endfor
  if tot_wt le 0. then goto, no_data
  ipt_end=-1L
  y_lab=0.
  if keyword_set(average) then y_lab=0
  for j=0L,nrows-1L do begin
  ipt_beg=ipt_end+1L
  pt_first[j]=ipt_beg
    if keyword_set(average) and nrec[j] gt 1L then begin
      pt_npts[j]=npts  
;
; average over records
; the statement below avoids a slow for loop over channels
;
      ipt_end=ipt_beg+npts-1L
      wt[ipt_beg:ipt_end]=total(re.wts(prl(js[j]):prl(js[j])+nrec[j]-1L))
      cmp[ipt_beg:ipt_end]=total(reform( $
          ch(pcl(js[j]):pcl(js[j])+nch*nrec[j]-1L)* $
          re.wts(prl(js[j])+transpose(lindgen(nch,nrec[j])/nch)),nrec[j],nch), $
          1)/ total(re.wts(prl(js[j]):prl(js[j])+nrec[j]-1L))
      
      case xtype of
        'channel'  : x[ipt_beg:ipt_end]=findgen(nch)+1. ;
        'int'      : x[ipt_beg:ipt_end]=in[pil[js[j]]].int     ;
        'hours'    : x[ipt_beg:ipt_end]=bl[pbl[js[j]]].avedhrs
        'fsky'     : x[ipt_beg:ipt_end]=sp[psl[js[j]]].fsky+0.001* $
                      (findgen(nch)+(1.-float(nch))/2.)*sp[psl[js[j]]].fres
        'velocity' : x[ipt_beg:ipt_end]=sp[psl[js[j]]].vel+ $
                      (findgen(nch)+(1.-float(nch))/2.)*sp[psl[js[j]]].vres
        else :
      endcase
      if keyword_set(x2) then begin
        x2[ipt_beg:ipt_end]=in[pil[js[j]]].int
        y_lab=[y_lab,in(pil(js[j])).int]
      endif
    endif else begin
      if keyword_set(average) then npts_t=npts else npts_t=npts[j] 
      if keyword_set(average) then nch_t=nch else nch_t=nch[j] 
      pt_npts[j]=npts_t  
      ipt_end=ipt_beg+npts_t-1L
      cmp[ipt_beg:ipt_end]=ch(pcl(js[j]):(pcl(js[j])+npts_t-1L))
      if keyword_set(normalize) then wt[ipt_beg:ipt_end]=(bl[pbl[js[j]]].ampave)^2*re.wts( prl(js[j])+ $
            transpose(lindgen(nch_t,nrec[j])/nch_t)) else $
      wt[ipt_beg:ipt_end]=re.wts( prl(js[j])+ $
            transpose(lindgen(nch_t,nrec[j])/nch_t))
      case xtype of
        'channel'  : x[ipt_beg:ipt_end]= $
                           float(lindgen(nch_t,nrec[j])/nrec[j])+1.
        'int'      : x[ipt_beg:ipt_end]=in(pil(js[j])).int+ $
                           findgen(nch_t,nrec[j])/nrec[j]
        'hours'    : x[ipt_beg:ipt_end]=in[pil[js[j]]].dhrs + $
                           (re.toffs[ prl[js[j]] + $
                            transpose(lindgen(nch_t,nrec[j])/nch_t) ] - $
                           0.5*re.integs[prl[js[j]] + $
                            transpose(lindgen(nch_t,nrec[j])/nch_t)])/3600.0
;       'hours'    : x[ipt_beg:ipt_end]=bl[pbl[js[j]]].avedhrs+ $
;                          re.toffs[ prl[js[j]]+ $
;                          transpose( lindgen(nch_t,nrec[j])/nch_t) ]/3600. - $
;                          0.5*sp[psl[js[j]]].integ/3600.
        'fsky'     : x[ipt_beg:ipt_end]=sp(psl(js[j])).fsky+ $
                           0.001*((float(lindgen(nch_t,nrec[j])/nrec[j]))+ $
                           ((1.-float(nch_t))/2.))*sp(psl(js[j])).fres
        'velocity' : x[ipt_beg:ipt_end]=sp[psl[js[j]]].vel+ $
                           ((float(lindgen(nch_t,nrec[j])/nrec[j]))+ $
                           ((1.-float(nch_t))/2.))*sp(psl(js[j])).vres
          else :
        endcase
        if keyword_set(x2) then begin
             x2[ipt_beg:ipt_end]= row+1+ $
                           indgen(nch_t,nrec[j])/nch_t
             y_lab=[y_lab,[in(pil(js[j])).int+ $
                           findgen(nrec[j])/nrec[j]]]
        endif
; $
;                           findgen(nch_t,nrec[j])
    endelse
    if keyword_set(x2) then row=max(x2)
  endfor
  if (n_elements(y_lab) gt 1) then y_lab=y_lab[1:*]
  if keyword_set(average) then nrec=make_array(n_elements(nrec),/int,value=1) 
;
; reverse channel and record order
;
  if string(order) eq 'channel' and not keyword_set(average) then begin
    individual=1
    if min([nch]) eq max([nch]) and min([nrec]) eq max([nrec]) then begin
      individual=0
      is=transpose(lindgen(nrec[0],nch[0]))
    endif
    ipt_end=-1L
    for j=0L,nrows-1L do begin
      ipt_beg=ipt_end+1L  
      ipt_end=ipt_beg+npts[j]-1L
      if individual then is=transpose(lindgen(nrec[j],nch[j]))
      cmp[ipt_beg:ipt_end]=cmp[is]
      x[ipt_beg:ipt_end]=x[is]
;      x2[ipt_beg:ipt_end]=x2[is]
      wt[ipt_beg:ipt_end]=wt[is]
    endfor    
  endif
; 
; normalize to continuum visibility (used for passband)
;
  if keyword_set(normalize) then begin
    uti_conv_apc,cmp_cont,bl[pbl[js]].ampave,bl[pbl[js]].phaave,/complex
    ipt_end=-1L
    for j=0L,nrows-1L do begin
      if keyword_set(average) then npts_t=npts else npts_t=npts[j] 
      ipt_beg=ipt_end+1L  
      ipt_end=ipt_beg+npts_t-1L
      cmp[ipt_beg:ipt_end]=cmp[ipt_beg:ipt_end]/cmp_cont[j]
    endfor    
  endif
; 
; average all integrations if average='all'
;
  if string(average) eq 'all' then begin
    nch=min([nch])
    cmp=cmp*wt
    wt[0:nch-1L]=total(reform(wt,nch,nrows),2) & wt=wt[0:nch-1L]
    cmp[0:nch-1L]=total(reform(cmp,nch,nrows),2)/wt & cmp=cmp[0:nch-1L] 
    x[0:nch-1L]=total(reform(x,nch,nrows),2) & x=x[0:nch-1L]/nrows
    pt_first=0
    pt_npts=nch
  endif

amp_pha:
if keyword_set(amp_pha) then begin
  amp=abs(cmp) & pha=!radeg*atan(imaginary(cmp),float(cmp))
endif
return

channel_change:
  print,'number of channels changed during averaging !'
  print,'(probably trying to average different spectrometers)'
  npts=0
  return

no_data:
  ; print,'No valid data found by dat_get_rows'
  cmp=make_array(npts_out,/complex)  ; ret 0's if wt=0
  return

end

