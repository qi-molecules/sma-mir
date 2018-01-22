function pass,pas_souids,x_var,y_vars,dch_smooth,frame_vars, $
		frames_per_page,plid,ntrim_max=ntrim_max, $
                no_unwrap=no_unwrap, funct=funct, npoly=npoly, $
                tel_bsl=tel_bsl, refant=refant, delay=delay, $
                preavg=preavg, unity_fill=unity_fill, polar=polar, $
                iflag=iflag, noplot=noplot, ramp=ramp, polrx=polrx, $
                _extra=extra_keywords
;
; Passband derivation for amp and phase with interactive fitting.
;
; The spectra on the passband calibrators will be plotted with multi-frames
; (one for each baseline, rec, spectral band,a dn sb). Each panel will have 
; subpanels to plot amp and pha seperately.
;
; Note : The desired plot device should be set in pl[plid].plot_device
;        before calling plo_spec, eg. pl[plid].plot_device='x' 
;    
; parameters : pas_souids -- souids for sources to be used for passband
;              x_var      -- variable for x-ccord :'channel', 'fsky' ,'velocity'
;              y_vars     -- variables plotted as y-coord : 
;                            'amp', 'pha', and 'amp,pha'
;              dch_smooth -- number of channels to smooth the spectra
;              frame_vars -- header variable used to separate data between
;                            frames
;              frames_per_page -- max number of frames per page
;              ntrim_max       -- max number of channels to trim spectra by
;                                 when plotting and smoothing
;		plid -- plot id - generated in IDL ONLY routines
;              funct -- default='boxcar', can be 'poly' for polynomial fit (KS)
;              npoly -- order of polynomial
;
;
; result = -1 (error), 0 (no rows) >0 (number of rows)
;
; To do a passband fit for both amp and pha with a maximum of 4 plots 
; per page :  
;  
; eg. : result=dat_filter(s_f,/reset) &  pl[plid].plot_device='x'
; eg. : result=pas_ini(use,pas_souids,all_sources,all_amps)
; eg. : result=pass(pas_souids,'channel','amp,pha',2,'blcd,rec,sb,band',4)

;
; temporary item. (used till npoly ne 0 is allowed. ks)
; 

if funct eq 'poly' then begin
  if n_elements(npoly) eq 1 then begin
;    if npoly ne 0 then begin
;      print,'Error! npoly ne 0 not yet implemented.'
;      return, -1
;    endif
  endif else begin
    print,'Error! n_elements(npoly) ne 1'
    return, -1
  endelse
endif

common global
common data_set
common plo

;
; checking input parameters
;
if x_var ne 'channel' and x_var ne 'velocity' and x_var ne 'fsky' then begin 
       print,'*** ',x_var,' not recognized x-coord !'
       return,-1
endif
if not keyword_set(tel_bsl) then tel_bsl='baseline'
if ((tel_bsl eq 'telescope') and  (not keyword_set(refant))) then begin
      print, 'NO reference antenna assigned for deriving antenna-based solution!!!'
      return, -1
endif


if (keyword_set(unity_fill)) then begin

  result=dat_filter(s_s,/save)
  result=dat_filter(s_f,/reset,/no_notify)

  tmp=bl(pbl).itel1
  list_itel1=tmp[uniq(tmp)]
  tmp=bl(pbl).itel2
  list_itel2=tmp[uniq(tmp)]

  result=dat_filter(s_s,/restore)

endif


;
; interactive device setup
;
if not e.java then begin
plid = plo_plid_gen()
endif
;
; first set up arrays w/ the desired amp, pha for spectra 
; for the data passing through the filter
;
if keyword_set(polrx) then begin
   inhid_beg=min([in(pil).inhid]) & inhid_end=max([in(pil).inhid])
   result=dat_list(s_l,'(("ipol" eq "1") or ("ipol" eq "4")) and "band" like "s" and "wt" gt "0"',/reset,/no_notify)
endif else begin
   result=dat_list(s_l,'"band" like "s" and "wt" gt "0"',/reset,/no_notify)
   inhid_beg=min([in(pil).inhid]) & inhid_end=max([in(pil).inhid])
endelse
;inhid_beg=min([in(pil).inhid]) & inhid_end=max([in(pil).inhid])

distinct_pas_souids=uti_distinct(pas_souids,nsources,/many_repeat)
distinct_sources=c.source(distinct_pas_souids)
;
; segment the data by blcd, receiver, band, and sb
;
bls=c.blcd(bl(pbl).iblcd)
recs=c.rec(bl(pbl).irec)
sbs=strupcase(c.sb(bl(pbl).isb))
bands=strupcase(c.band(sp(psl).iband))
combinations=bls+' '+recs+' '+sbs+' '+bands
distinct_combinations=uti_distinct(combinations,ncombinations,/many_repeat)
distinct_bls=uti_distinct(bls,nbls,/many_repeat)
distinct_recs=uti_distinct(recs,nrecs,/many_repeat)
distinct_sbs=uti_distinct(sbs,nsbs,/many_repeat)
distinct_bands=uti_distinct(bands,nbands,/many_repeat)
distinct_ibands=make_array(nbands,/int)
distinct_ibls=make_array(nbls,/int)
distinct_irecs=make_array(nrecs,/int)
distinct_isbs=make_array(nsbs,/int)
;
; setup channel ranges for each band
; 
nchs=make_array(ncombinations,/long) & first_chs=make_array(ncombinations,/long) 
pt_first=make_array(ncombinations,/long,value=-1L)
pt_npts=make_array(ncombinations,/long,value=-1L)
wt_tot=make_array(ncombinations,/float,value=0.)
amp_wt=make_array(ncombinations,/float,value=0.)
pha_wt=make_array(ncombinations,/float,value=0.)
rinteg_tot=make_array(ncombinations,/float,value=0.)

if (tel_bsl eq 'telescope') then begin
  ;
  ; setup antenna-based cal_frames (rx/sb/bands) tag for each combination
  ;
  cal_frames= make_array(ncombinations,/string)
  cal_sb = make_array(ncombinations,/string)
  cal_iblcd = make_array(ncombinations,/int)
  cal_itel1 = make_array(ncombinations,/int)
  cal_itel2 = make_array(ncombinations,/int)
  cal_nchs  = make_array(ncombinations,/long)
endif

;
; loop through all sources which can be seen in filter or
; whatever source was specified in call statement
;
  npt=0L
  cmps=make_array(1,/complex,/nozero) 
  amps=make_array(1,/float,/nozero) 
  phas=make_array(1,/float,/nozero) 
  xs=make_array(1,/float,/nozero)
  frames=make_array(1,/string,value='')
  colors=make_array(1,/string,value='')
  symbols=make_array(1,/string,value='')

for js=0,n_elements(distinct_pas_souids)-1 do begin
  result=dat_list(s_l,'"souid" eq "'+ $
                 strtrim(string(distinct_pas_souids[js]),2)+'"',/reset,/no_notify)
  result=dat_list(s_l,'"band" like "s" and "wt" gt "0"',/no_notify)
  if  keyword_set(polar) then begin
     print, 'HERE IS POLARIZATION FILTER'
     command='"ipol" eq "'+strtrim(polar[0],2)+'"'
     npolar=n_elements(polar)
     if npolar gt 1 then for i=1,npolar-1 do command=command+' or "ipol" eq "'+strtrim(polar[i],2)+'"'
     print,command
     result=dat_list(s_l,command)     
  endif
  if result le 0 then begin
    print,'No data for spectrum of ',distinct_sources(js)
    goto, end_sources
  endif
;;  result=dat_list(s_l,/save,/no_notify)

  ;
  ; get the average spectra for each of the combinations
  ;
  ;
  ; in this loop we find and average all spectra in each combination
  ; of blcd, receiver, sb, and band. to do this, we :
  ; 1) find the values of each separate parameter going into the combination.
  ; 2) set the lists to select only those data which satisfy these parameters
  ; 3) use dat_get_rows to bulk average all spectra in the selected list
  ; 4) sum the new spectrum into the plotting arrays
  ; 5) record in the arrays :frames,colors, and symbols the values for each band
  ; 6) divide data by sum of weights 
  ;

  a0=pil & a1=pbl & a2=psl & a3=pcl & a4=prl & index=-1L
  for i=0,nbls-1 do begin
     blcd=distinct_bls[i]
     distinct_ibls[i]=min(where(c.blcd eq blcd))
     for j=0, nrecs-1 do begin
        rec=distinct_recs[j]
        distinct_irecs[j]=min(where(c.rec eq rec))
        for k=0, nsbs-1 do begin
           sb=distinct_sbs[k]
           distinct_isbs[k]=min(where(c.sb eq strlowcase(sb)))
           n=where( (bl[a1].irec eq distinct_irecs[j]) and (bl[a1].isb eq distinct_isbs[k]) and (bl[a1].iblcd eq distinct_ibls[i]), count)
           if count eq 0 then goto,jump1
           b0=a0[n] & b1=a1[n] & b2=a2[n] & b3=a3[n] & b4=a4[n]
           for m=0, nbands-1 do begin
              band=distinct_bands[m]
              distinct_ibands[m]=min(where(c.band eq strlowcase(band)))
              ib=max(where(distinct_bands eq band))
;              print,blcd,' ',rec,' ',sb,' ',band
              n=where( sp[b2].iband eq distinct_ibands[m], count)
              if count eq 0 then goto, jump2
              pil=b0[n] & pbl=b1[n] & psl=b2[n] & pcl=b3[n] & prl=b4[n]
              frame=blcd+' '+strupcase(sb)
              frames=[frames,frame]
              index=index+1
;              print,index
;              read,iii
              if (tel_bsl eq 'telescope') then begin
                 tmp=bl(pbl).itel1
                 cal_itel1[index]=tmp[uniq(tmp)]
                 tmp=bl(pbl).itel2
                 cal_itel2[index]=tmp[uniq(tmp)]
              endif

    ;
    ; set the weight for this spectrum to ampave*ampave*wt
    ; for this combination
    ;
              wt_source=total(bl(pbl).ampave*bl[pbl].ampave*sp(psl).wt)
    ;
    ; use dat_get_rows to bulk average all spectra in the selected list
    ;
              if keyword_set(delay) then begin
                 dat_get_rows,cmp,amp,pha,x,wts,first,npts,x_var,0,/list, $
                              average='all'
              endif else begin        
                 dat_get_rows,cmp,amp,pha,x,wts,first,npts,x_var,0,/list, $
                              /normalize,average='all'
              endelse
              if npts[0] eq 0 then begin
                 goto, jump2
              endif
              if x_var eq 'channel' then begin
                 x=x+first_chs(ib)
                 ntrim = keyword_set(ntrim_max)? (npts[0] gt 2*ntrim_max ? ntrim_max : 0) : 0
              endif else begin
                 ntrim = keyword_set(ntrim_max)? (npts[0] gt 2*floor(ntrim_max/abs(sp[psl[0]].fres)) ? floor(ntrim_max/abs(sp[psl[0]].fres)):0) :0
              endelse
              x=x[ntrim:npts[0]-ntrim-1]
              cmp=cmp[ntrim:npts[0]-ntrim-1]
              npts=npts-2*ntrim
              if keyword_set(preavg) then begin
                 if x_var eq 'fsky' then begin
                    chavg=ceil(preavg/abs(sp[psl[0]].fres))
                 endif else begin
                    chavg=long(preavg)
                 endelse
                 new_npts=floor(npts/float(chavg))
                 newx=make_array(new_npts,/float)
                 newcmp=make_array(new_npts,/dcomplex)
                 chstart=(npts-long(new_npts*chavg))/2
                 chend=0
                 for chcount=0,new_npts-1 do begin
                    chend=chstart+chavg-1
                    newcmp[chcount]=total(cmp[chstart:chend])/float(chavg)
                    newx[chcount]=total(x[chstart:chend])/float(chavg)
                    chstart=chend+1
                 endfor
                 cmp=newcmp
                 x=newx
                 npts=new_npts                  
              endif

    ;
    ; add the spectral band into the plotting arrays
    ;
              if pt_first[index] eq -1L then begin
                 pt_first[index]=1+total(pt_npts > 0)
                 npt=npt+npts[0]
                 xs=[xs,x]
                 cmps=[cmps,cmp*wt_source]
              endif else begin
                 cmps[pt_first[index]:pt_first[index]+pt_npts[index]-1L]= $
                   cmps[pt_first[index]:pt_first[index]+pt_npts[index]-1L]+cmp*wt_source
              endelse
              wt_tot[index]=wt_tot[index]+wt_source
              if pt_npts[index] eq -1L then pt_npts[index]=npts[0]
              if pt_npts[index] ne npts[0] then begin
                 print,'number of channels changed in ',distinct_combinations[index]
                 return,-1
              endif
;    if xs[pt_first[i]] ne x[0] and xs[pt_first[i]+1] ne x[1] then begin
;      print,'channel coordinate changed in ',distinct_combinations[i]
;      return,-1
;    endif
              jump2:
           endfor ; bands
           jump1:
        endfor ; sbs
     endfor ; recs
  endfor ; baselines
  end_sources:
endfor
;
; remove any combinations which had no good data
;
j_good=where(pt_npts ne -1L and wt_tot gt 0.,count)
if count ne ncombinations then begin
  distinct_combinations=distinct_combinations(j_good)
  ncombinations=n_elements(distinct_combinations)
  pt_first=pt_first[j_good]
  pt_npts=pt_npts[j_good]
  xs=xs[j_good]
  wt_tot=wt_tot[j_good]
endif
;
; now remove the first dummy element of the plotting arrays
;
xs=xs[1:npt]
cmps=cmps[1:npt]
pt_first=pt_first-1L
for i=0,ncombinations-1L do begin
  cmps[pt_first[i]:pt_first[i]+pt_npts[i]-1L]= $
         cmps[pt_first[i]:pt_first[i]+pt_npts[i]-1L]/wt_tot[i]
endfor
;***************************************************************************
; Baseline-Based Solution
;***************************************************************************
if (tel_bsl eq 'baseline') then begin
  ; 
  ; cmps (ratio of spectra data to continuum data) is the passband solution
  ;
   uti_conv_apc,cmps,amps,phas,/amp_pha
endif

if keyword_set(delay) then begin
   frames=frames[1:n_elements(frames)-1]
   distinct_frames=uti_distinct(frames,nframes,/many_repeat)
endif else begin
   frames=distinct_combinations
   distinct_frames=distinct_combinations & nframes=ncombinations
endelse

;***************************************************************************
; End of Baseline-Based Solution
;***************************************************************************


;***************************************************************************
; Antenna-Based Solution
;***************************************************************************
if (tel_bsl eq 'telescope') then begin
;  if not keyword_set(preavg) then preavg=1L else preavg=long(preavg)
  for i=0, ncombinations-1 do begin
    result=dat_comb_sep(distinct_combinations[i],['blcd','rec','sb','band'], $
                      codes,icodes,n_components)

    cal_frames[i]=codes[1]+' '+codes[2]+' '+codes[3]
    cal_sb[i] = codes[2]
    cal_iblcd[i]=icodes[0]

  endfor

  distinct_cal_frames=uti_distinct(cal_frames,ncal_frames,/many_repeat)
  distinct_cal_nchs = make_array(ncal_frames,/long)
  distinct_cal_grps = make_array(ncal_frames,/int)

  ;
  ; get the combination counts for making the ant-based gain solution array
  ;
  ; first, number of existings antennas
  ;
  if (not keyword_set(unity_fill)) then begin

    totaltels = [cal_itel1,cal_itel2]
    tellist = totaltels(  uniq(totaltels,sort(totaltels) ))
    noftel    = n_elements(tellist)

  endif else begin

    totaltels = [list_itel1,list_itel2]
    tellist = totaltels(  uniq(totaltels,sort(totaltels) ))
    noftel    = n_elements(tellist)

  endelse

  for cfcount=0,(ncal_frames -1) do begin
  ;
  ; going through all segments (rec, sb, bands) first to get the number of channels
  ;
    j=where(cal_frames eq distinct_cal_frames[cfcount],count)
    distinct_cal_nchs[j]= pt_npts[j[0]]
;    distinct_cal_grps[j]= ceil(pt_npts[j[0]]/float(preavg))
    distinct_cal_grps[j]= pt_npts[j[0]]
  endfor

  total_nchs = total(distinct_cal_grps)
  ;
  ; creating the ploting/solution array
  ;   
  pl_npt= noftel * total_nchs
  pl_xs=make_array(pl_npt,/float)
  pl_cmps=make_array(pl_npt,/complex)

  pl_frames=make_array(noftel*ncal_frames,/string)
  pl_delay_frames=make_array(noftel*ncal_frames,/string)
  pl_pt_first=make_array(noftel*ncal_frames,/long)
  pl_pt_npts =make_array(noftel*ncal_frames,/long)

  ;
  ; some more minor initialization
  ;
  niter_max = 100
  epsi1=1.0e-8
  epsi2=1.0e-4
  factors=[0.5,0.75,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.5]

  good_sol = 0

  ;
  ; start looping all segments
  ;

  for cfcount=0,(ncal_frames -1) do begin
    ;
    ; going through dataset by rec, sb, bands
    ;
    j=where(cal_frames eq distinct_cal_frames[cfcount],count)

    if (e.debug) then print, 'Solving: ',distinct_cal_frames[cfcount]
    ;
    ; quick check on the data integity
    ;

    weight = 1.

    allchan = pt_npts[j]
    nchans =  allchan(  uniq(allchan, sort(allchan) ))

;    ngroup = ceil(nchans/float(preavg))
    ngroup = nchans

    alltels = [cal_itel1[j],cal_itel2[j]]
    listtel = alltels(  uniq(alltels,sort(alltels) ))
    ntel    = n_elements(listtel)
    if (e.debug) then print, distinct_cal_frames[cfcount]+' has total ', ntel,' antennas'

    allbsls = cal_iblcd[j]
    listbsl = allbsls(  uniq(allbsls,sort(allbsls) ))
    nbsl   = n_elements(listbsl)
    if (e.debug) then print, distinct_cal_frames[cfcount]+ ' has total ', nbsl,' baselines'

    nmiss=0
    if (keyword_set(unity_fill)) then begin
      if (ntel lt noftel) then begin
        nmiss=noftel-ntel
        mlist = make_array(nmiss,/int)
        mi=0
        for mj=0,(noftel-1) do begin
          mflag=0
          for mk=0,(ntel-1) do begin
            if (listtel(mk) eq tellist(mj)) then mflag=1
          endfor
          if (mflag eq 0) then begin
            mlist(mi) = tellist(mj)
            mi=mi+1
          endif
        endfor
        print, "!!! NOTE: WILL FILL UP MISSING ANT ",mlist, " WITH UNITY GAINS !!!"
      endif
    endif

    ;
    ; solution matrix indices
    ;
    if (cfcount eq 0) then baseindex = 0
    if (cfcount ne 0) then baseindex = total(distinct_cal_grps[0:(cfcount-1)])

    if (n_elements(nchans) gt 1.0) then begin
       print, 'WARNING: Un-matching channel numbers between different baseline/receiver/sideband'+ $
            '           in the same band  selection'
       print, '         Passband calibration will abort !!!'
       return, -1
    endif

    if (nbsl ne ((ntel)*(ntel-1)/2)) then begin
       print, 'WARNING: mismatch between number of ANTENNAS and number of BASELINES at '+ $
                        distinct_cal_frames[cfcount]+'.'
       print, '         PASS.PRO will continue to derive the passband solution.'
       print, '         Phase closure, however, may not be satisfied by the data in this segment.'
    endif

    if (where(listtel eq refant) eq -1) then begin
       print, 'WARNING: missing data from the reference antenna at ' + distinct_cal_frames[cfcount]+'.'
       print, '         Please select a different reference antenna !!!'
       return, -1
    endif

    modelamps = make_array(nbsl,/float,value=1.0)
    modelphas = make_array(nbsl,/float,value=0.0)

    ;
    ; Now going through all channels in this segment
    ;
    igroup = 0
    for chcount = 0, nchans-1 do begin

      complexdata=make_array(nbsl,/dcomplex)

      for ibsl = 0, (n_elements(j) -1) do begin
         if ((chcount+1L) le nchans) then begin
         complexdata[ibsl] = total(cmps[pt_first[j[ibsl]]+chcount:pt_first[j[ibsl]]+chcount])
         ichan=chcount
         endif else begin
         complexdata[ibsl] = total(cmps[pt_first[j[ibsl]]+chcount:pt_first[j[ibsl]]+nchans-1])/float(nchans-chcount)
         ichan=(chcount + nchans - 1)/2.0
         endelse
      endfor

      complexmodel=make_array(nbsl,/dcomplex)
      uti_conv_apc,complexmodel,modelamps,modelphas,/complex

      complexr=make_array(nbsl,/dcomplex)
      complexr=complexdata/complexmodel

      complexcr=make_array(nbsl,/dcomplex)
      conplexcr=conj(complexdata)/complexmodel

      wt = weight
      vm = conj(complexmodel) *       complexdata * wt
      vv =  abs(complexmodel) * abs(complexmodel) * wt
      mm =   abs(complexdata) *  abs(complexdata) * wt

      ;
      ; chi-square minimization for each antenna using info for all relevant baselines
      ;
 
      ;
      ; first, do a "phase-only" minimization 
      ;

      telg = make_array(ntel,/dcomplex,value=1.0)
      sum = make_array(ntel,/dcomplex,value=0.0)

      converge = -1
      niter = 0
      if (ntel le 6) then factor = 0.5 else factor = 0.8
      
      while (converge ne 1 and niter lt niter_max) do begin

        niter = niter + 1
        ;
        ;  consolidating baseline-based information into antenna-based format
        ;  for antenna-based gain derivation
        ;

        for ibsl = 0, (n_elements(j) -1) do begin

           ptel1 = where(listtel eq cal_itel1[j[ibsl]])
           ptel2 = where(listtel eq cal_itel2[j[ibsl]])

           sum(ptel1) = sum(ptel1) + telg(ptel2) * vm(ibsl)
           sum(ptel2) = sum(ptel2) + telg(ptel1) * conj(vm(ibsl))

        endfor

        incremt = 0
        ;
        ;  real interations for antenna-based gain derivation
        ;
        for itel = 0, (ntel-1) do begin

          temp = (sum(itel)/abs(sum(itel)))
          temp = telg(itel) + factor * (temp - telg(itel))
          temp = temp/abs(temp)
          incremt = incremt + abs(telg(itel) - temp)^2
          telg(itel) = temp
          sum(itel) = 0.0

        endfor

        if (incremt/ntel lt epsi1) then converge = 1
      endwhile

      ; if (e.debug) then print,'Total iteration : ',niter

      if (incremt/ntel lt epsi2) then converge = 1 else converge = -1

      ;
      ; do another "amp/phase-combo" minimization if necessary
      ; with the earlier phase solution used as a initial guess
      ;

      if (converge eq 1) then begin 

        ;
        ; a bit more calculations for the inital guess 
        ;
        sumrvm = 0
        sumrvv = 0

        for ibsl = 0, (n_elements(j) -1) do begin

          ptel1 = where(listtel eq cal_itel1[j[ibsl]])
          ptel2 = where(listtel eq cal_itel2[j[ibsl]])

          sumrvm = sumrvm + conj(telg(ptel1)) * telg(ptel2) * vm[ibsl]
          sumrvv = sumrvv + vv[ibsl]

        endfor

        factor = sqrt(abs(sumrvm/sumrvv))

        telg =  telg * factor[0]

        sum = make_array(ntel,/dcomplex,value=0.0)
        sum2 = make_array(ntel,/dcomplex,value=0.0)

        ;
        ; ready to start iteration
        ;
        converge = -1
        niter = 0
      
        while (converge ne 1 and niter lt niter_max) do begin

          niter = niter + 1

          if (ntel le 6) then factor = 0.5 else factor = factors(min([10,niter]))

          ;
          ;  again, consolidating baseline-based information into antenna-based format
          ;  for antenna-based gain derivation
          ;

          for ibsl = 0, (n_elements(j) -1) do begin
  
            ptel1 = where(listtel eq cal_itel1[j[ibsl]])
            ptel2 = where(listtel eq cal_itel2[j[ibsl]])

            sum(ptel1) = sum(ptel1) + telg(ptel2) * vm[ibsl]
            sum(ptel2) = sum(ptel2) + telg(ptel1) * conj(vm[ibsl])

            sum2(ptel1) = sum2(ptel1) + abs(telg(ptel2))^2 * vv[ibsl]
            sum2(ptel2) = sum2(ptel2) + abs(telg(ptel1))^2 * vv[ibsl]

          endfor

          incremt = 0
          sumwt = 0
          ;
          ;  real interations for antenna-based gain derivation
          ;
          for itel = 0, (ntel-1) do begin
             
            temp = sum(itel)/sum2(itel) - telg(itel)
            telg(itel) = telg(itel) + factor * temp
            incremt = incremt + abs(temp)^2
            sumwt = sumwt + abs(telg(itel))^2
            sum(itel) = 0.0
            sum2(itel) = 0.0
          endfor

          if (incremt/sumwt lt epsi1) then converge = 1
        endwhile
        if (incremt/sumwt lt epsi2) then converge = 1 else converge = -1

      endif

      if (converge eq 1) then begin
        good_sol = good_sol + 1
      endif else begin
        print, 'No convergance at '+distinct_cal_frames[cfcount]+' channel ',ichan
        print, 'All gains are set to 1.0 (no corrections!)'
        ; No convergance, set gain solution to 1.0 and weight to NEGATIVE(-1.0)
        ; not sure if this is the best way to reset an array values,
        ; telg=1.0 certainly won't work.
        telg = make_array(ntel,/dcomplex,value=1.0)
        weight = -1.
      endelse

      ;
      ; normalizing antenna-based gains to the reference antenna
      ;

      ptelref = where(listtel eq refant)
      if (ptelref ne -1) then begin
        temp =conj(telg(ptelref))/abs(telg(ptelref))
        for itel = 0, (ntel-1) do begin
          telg(itel) = telg(itel) * temp
        endfor
      endif

      ;
      ; putting solution into the plotting array
      ;

      for itel = 0, (ntel-1) do begin
        pl_xs[total_nchs * itel + baseindex + igroup] = $
                      xs[pt_first[j[0]]+long(ichan)]+(ichan-long(ichan))
        pl_cmps[total_nchs * itel + baseindex + igroup] = telg[itel]
      endfor

      if (nmiss gt 0) then begin
        for itel = ntel, (ntel+nmiss-1) do begin
          pl_xs[total_nchs * itel + baseindex + igroup] = $
                        xs[pt_first[j[0]]+long(ichan)]+(ichan-long(ichan))
          pl_cmps[total_nchs * itel + baseindex + igroup] = complex(1.0)
        endfor
      endif

      igroup = igroup + 1

    endfor

    for itel = 0, (ntel-1) do begin
      pl_frames[noftel * cfcount + itel] = $
                   string(listtel[itel])+' '+distinct_cal_frames[cfcount]
      pl_delay_frames[noftel * cfcount + itel] = $
                   string(listtel[itel])+' '+ cal_sb[j[0]]
      pl_pt_first[noftel * cfcount + itel] = total_nchs * itel + baseindex
      pl_pt_npts[noftel * cfcount + itel] = ngroup
    endfor

    if (nmiss gt 0) then begin
      for itel = ntel, (ntel+nmiss-1) do begin
        pl_frames[noftel * cfcount + itel] = $
                     string(mlist[itel-ntel])+' '+distinct_cal_frames[cfcount]
        pl_delay_frames[noftel * cfcount + itel] = $
                     string(listtel[itel-ntel])+' '+ cal_sb[j[0]]
        pl_pt_first[noftel * cfcount + itel] = total_nchs * itel + baseindex
        pl_pt_npts[noftel * cfcount + itel] = ngroup
      endfor
    endif

  endfor

  xs = pl_xs
  amps=make_array(pl_npt,/float,/nozero)
  phas=make_array(pl_npt,/float,/nozero)
  uti_conv_apc,pl_cmps,amps,phas,/amp_pha
  npt=pl_npt

  pt_first=pl_pt_first
  pt_npts=pl_pt_npts

if keyword_set(delay) then begin

  frames=pl_delay_frames
  distinct_frames = uti_distinct(pl_delay_frames,nframes,/many_repeat)
  distinct_combinations = uti_distinct(pl_frames,ncombinations,/many_repeat)

endif else begin

  frames=pl_frames
  distinct_frames = uti_distinct(pl_frames,nframes,/many_repeat)

endelse

endif
;***************************************************************************
; End of Antenna-Based Solution
;***************************************************************************
;
; initializing the plotting device
;
pindex = 0

pl[plid].plot_type='pas'
if not e.java then pl[plid].plot_interact=plo_init(plid,pindex)
color_vars=''
symbol_vars=''
;
; the rest of plotting setups
;

multi_pt=1
distinct_colors=colors[0] & ncolors=1 
distinct_symbols=symbols[0] & nsymbols=1
color_index=0.6*bytscl((indgen(max([n_elements(distinct_colors),1])) $
            mod 16)+1)+64.

;
; use statement below for grey scale displays
; 
;color_index=make_array(n_elements(distinct_colors),/int,value=223)

symbol_pt_index=2 & symbol_line_index=0
;
; find data ranges, plot sizes and number of col's and rows
; 
bottom_label='track : '+strtrim(string(in(pil(0)).traid),2)
npts=n_elements(xs)
xmin=min(xs) & xmax=max(xs)
dx=xmax-xmin & xmin=xmin-0.05*dx & xmax=xmax+0.05*dx
amp_min=min(amps) & amp_max=max(amps) & dy=amp_max-amp_min 
amp_max=amp_max+0.05*dy & amp_min=amp_min-0.05*dy
nor_xmin=0.1 & nor_xmax=0.95 & nor_ymin=0.1 & nor_ymax=0.94
nor_dx=nor_xmax-nor_xmin & nor_dy=nor_ymax-nor_ymin
frames_per_page=min([nframes,frames_per_page])
ncol=max([long(sqrt(frames_per_page)),1]) 
nrow=frames_per_page/ncol 
frames_per_page = nrow * ncol 
if e.debug then print,nframes,' frames ',frames_per_page,' frames_per_page ',ncol,' cols ',nrow,' rows'
if e.debug then print,n_elements(distinct_frames),' frames ',n_elements(distinct_colors), $
        ' colors ',n_elements(distinct_symbols),' symbols '
;
; generate a page and frame location for each frames
;
page=indgen(nframes)/frames_per_page
frame_on_page=indgen(nframes)-page*frames_per_page
row=frame_on_page/ncol & col=frame_on_page-row*ncol
pan_dx=nor_dx/ncol
pan_dy=nor_dy/nrow
pan_xmin=nor_xmin+col*pan_dx & pan_xmax=nor_xmin+(col+1)*pan_dx
pan_ymax=nor_ymax-row*pan_dy & pan_ymin=nor_ymax-(row+1)*pan_dy
;
; setup sub-panels : sub_fxmin => fraction of panel dx to offset from min
;                    sub_fxmax => fraction of panel dx to offset from min
; note : o'th subpanel at top
;
; put amp and pha in ys array
;
nsub=0
case y_vars of
     'amp'        : begin
                      nsub=1 
                      y_var=['amp'] & psym=10
                      sub_fymin=0. & sub_fymax=1.
                      ymin=make_array(nframes,nsub,/float,value=amp_min)
                      ymax=make_array(nframes,nsub,/float,value=amp_max)
                      ys=make_array(nsub,npt,/float,/nozero) 
                      ys(0,*)=amps
                    end
     'amp,pha'    : begin
                      nsub=2 
                      y_var=['amp','pha'] & psym=[10,1]
                      sub_fymin=make_array(nsub,/float,value=0.) 
                      sub_fymax=make_array(nsub,/float,value=1.)
                      sub_fymax(1)=0.3 & sub_fymin(0)=0.3  
                      ymin=make_array(nframes,nsub,/float,value=amp_min)
                      ymax=make_array(nframes,nsub,/float,value=amp_max)
                      ymin(*,1)=-190. & ymax(*,1)=190.
                      ys=make_array(nsub,npt,/float,/nozero) 
                      ys(0,*)=amps & ys(1,*)=phas 
                    end
     'pha'    : begin
                      nsub=1 
                      y_var=['pha'] & psym=[1]
                      sub_fymin=0. & sub_fymax=1.
                      ymin=make_array(nframes,nsub,/float,value=-190.)
                      ymax=make_array(nframes,nsub,/float,value=190.)
                      ys=make_array(nsub,npt,/float,/nozero) 
                      ys(0,*)=phas 
                    end
     'complex'    : begin
                      nsub=2 
                      y_var=['amp','pha'] & psym=[10,1]
                      sub_fymin=make_array(nsub,/float,value=0.) 
                      sub_fymax=make_array(nsub,/float,value=1.)
                      sub_fymax(1)=0.3 & sub_fymin(0)=0.3  
                      ymin=make_array(nframes,nsub,/float,value=amp_min)
                      ymax=make_array(nframes,nsub,/float,value=amp_max)
                      ymin(*,1)=-190. & ymax(*,1)=190.
                      ys=make_array(nsub,npt,/float,/nozero) 
                      ys(0,*)=amps & ys(1,*)=phas 
                    end
     else: begin 
       print,'*** plotting of ',y_vars,' not supported !'
       return,-1
     endelse
endcase
;
; if plotting phases, unwrap them for each frame
;
if y_vars eq 'amp,pha' or y_vars eq 'pha' then begin 
  fir_ind=0 & if y_vars eq 'amp,pha' then fir_ind=1
  for i=0,n_elements(distinct_frames)-1 do begin
    js=max(where(frames eq distinct_frames(i)))
    yt=reform(ys(fir_ind,pt_first[js]:pt_first[js]+pt_npts[js]-1L),pt_npts[js])
; used to be     result=uti_pha_unwrap(yt,smooth=pt_npts[js]/16) 
    if not keyword_set(no_unwrap) then result=uti_pha_unwrap(yt,smooth=pt_npts[js]/16,ramp=ramp)
    ys(fir_ind,pt_first[js]:pt_first[js]+pt_npts[js]-1L)=yt
    uti_range_even,min([yt]),max([yt]),10.,ymi,yma
    ymin(i,fir_ind)=ymi & ymax(i,fir_ind)=yma
  endfor
endif
  sub_fxmin=make_array(nsub,/float,value=0.) 
  sub_fxmax=make_array(nsub,/float,value=1.)
  xmin=make_array(nsub,/float,value=xmin)
  xmax=make_array(nsub,/float,value=xmax)

;
;  do baseline-based or telescope-based passband fit
;

icursor=0
if keyword_set(delay) then begin
   if delay eq 2 then begin
      icursor=2
      dtel1=bl[pbf].itel1
      dtel2=bl[pbf].itel2
      dtel=[dtel1,dtel2]
      distinct_dtel=uti_distinct(dtel,ndtel,/many_repeat)
      distinct_dtel=distinct_dtel(sort(distinct_dtel))
      print,distinct_dtel
      delay=dblarr(2,ndtel)
;;      if keyword_set(iflag) then begin
;;         icursor=2
      for i_dtel=0L, ndtel-1 do begin
         tmp=0.d
         read,tmp,prompt='Enter delay coefficient for Antenna '+string(distinct_dtel[i_dtel])+': '
         delay[0,i_dtel]=distinct_dtel[i_dtel]
         delay[1,i_dtel]=tmp
;         delay[2,i_dtel]=tmp[1]
      endfor
;;   endif
   endif
endif

yfs=make_array(nsub,npt,/float,/nozero)

start:
result=pass_fit(tel_bsl,funct,dch_smooth,x_var,y_vars,frames, $
          distinct_frames,xs,ys,wts,pt_first,pt_npts,yfs, $
          npoly=npoly, delay=delay, icursor=icursor,_extra=extra_keywords)
if y_vars eq 'complex' then y_vars='amp,pha'

;
; save the original parameters
;
npts = n_elements(xs)
saved_par={xmin:pan_xmin,xmax:pan_xmax,ymin:pan_ymin,ymax:pan_ymax, $
   pan_dx:pan_dx,pan_dy:pan_dy,row:row,col:col,data_xmin:xmin, $
   data_xmax:xmax,data_ymin:ymin,data_ymax:ymax, $
   nor_xmin:nor_xmin,nor_xmax:nor_xmax,nor_ymin:nor_ymin, $
   nor_ymax:nor_ymax,nframes:nframes,frames_per_page:frames_per_page, $
   nrow:nrow,ncol:ncol}  

pas_par={saved_par:saved_par,nor_xmax:nor_xmax,nor_ymax:nor_ymax, $
     nor_dx:nor_dx,nor_dy:nor_dy,frames:frames, $
     distinct_frames:distinct_frames,colors:colors, $
     distinct_colors:distinct_colors,symbols:symbols, $
     distinct_symbols:distinct_symbols,pan_xmin:pan_xmin, $
     pan_xmax:pan_xmax,pan_ymin:pan_ymin,pan_ymax:pan_ymax,pan_dx:pan_dx, $
     pan_dy:pan_dy,sub_fxmin:sub_fxmin,sub_fxmax:sub_fxmax, $
     sub_fymin:sub_fymin,sub_fymax:sub_fymax,xmin:xmin,xmax:xmax,ymin:ymin,$
     ymax:ymax,psym:psym,row:row,col:col,plot_scale:make_array(frames_per_page,nsub,2,2,/float), $
     iframe:0,j_first:0,j_last:0, $
     frames_per_page:frames_per_page,nframes:nframes, $        
     color_index:color_index,symbol_pt_index:symbol_pt_index, $
     symbol_line_index:symbol_line_index,nsub:nsub,xs:xs, $
     x_var:x_var,y_var:y_var,ys:ys,wts:wts,nrow:nrow,ncol:ncol, $
     bottom_label:bottom_label,pt_first:pt_first,pt_npts:pt_npts, $
     multi_pt:multi_pt,initial:0, $
     m_options:'cspne',control:'',$
     j_select:intarr(npts),i_select:intarr(npts),$
     x_select:dblarr(npts),y_select:dblarr(npts),$
     m_button_select:intarr(npts),n_select:0,yfs:yfs,$
     inhid_beg:inhid_beg,inhid_end:inhid_end,tel_bsl:tel_bsl}


pas=replicate(pas_par,1)
pl[plid].num_pages = ceil(float(pas[pindex].nframes) / float(pas[pindex].frames_per_page))
iframe=0
if (pl[plid].plot_device ne 'null') then loadct,39,/silent
j_first=iframe
j_last=min([iframe+frames_per_page-1,nframes-1])
pas[pindex].iframe=iframe & pas[pindex].j_first=j_first & pas[pindex].j_last=j_last
pas[pindex].frames_per_page=frames_per_page & pas[pindex].nframes=nframes
pas[pindex].nrow=nrow & pas[pindex].ncol=ncol
pas[pindex].nsub=2
if not keyword_set(noplot) then result=plo_page(plid,pindex)
pas[pindex].color_index=color_index+150.
if not keyword_set(noplot) then result=plo_over_page(plid,pindex)
if not e.java then begin
   if not keyword_set(noplot) then pl[plid].plot_interact=plo_control(plid,pindex)

  if keyword_set(delay) then begin
    if icursor eq 0 then begin
       icursor=1
       pha_x=0. & pha_y=0.
       delay=fltarr(5)
       print,''
       print,'***1. Click the first phase-frequency point'
       cursor,pha_x,pha_y,/down,/data
       print,pha_x,pha_y
       delay[0:1]=[pha_x,pha_y]
       print,''
       print,'***2. Click the second phase-frequency point'
       cursor,pha_x,pha_y,/down,/data
       print,pha_x,pha_y   
       delay[2:3]=[pha_x,pha_y]
       print,''
       read,pha_x,prompt='***3. Enter the wrap number: '
       delay[4]=pha_x
       goto, start
    endif
    saved_par={xmin:pan_xmin,xmax:pan_xmax,ymin:pan_ymin,ymax:pan_ymax, $
               pan_dx:pan_dx,pan_dy:pan_dy,row:row,col:col,data_xmin:xmin, $
               data_xmax:xmax,data_ymin:ymin,data_ymax:ymax, $
               nor_xmin:nor_xmin,nor_xmax:nor_xmax,nor_ymin:nor_ymin, $
               nor_ymax:nor_ymax,nframes:ncombinations,frames_per_page:frames_per_page, $
               nrow:nrow,ncol:ncol}  

    pas_par={saved_par:saved_par,nor_xmax:nor_xmax,nor_ymax:nor_ymax, $
             nor_dx:nor_dx,nor_dy:nor_dy,frames:distinct_combinations, $
             distinct_frames:distinct_combinations,colors:colors, $
             distinct_colors:distinct_colors,symbols:symbols, $
             distinct_symbols:distinct_symbols,pan_xmin:pan_xmin, $
             pan_xmax:pan_xmax,pan_ymin:pan_ymin,pan_ymax:pan_ymax,pan_dx:pan_dx, $
             pan_dy:pan_dy,sub_fxmin:sub_fxmin,sub_fxmax:sub_fxmax, $
             sub_fymin:sub_fymin,sub_fymax:sub_fymax,xmin:xmin,xmax:xmax,ymin:ymin,$
             ymax:ymax,psym:psym,row:row,col:col,plot_scale:make_array(frames_per_page,nsub,2,2,/float), $
             iframe:0,j_first:0,j_last:0, $
             frames_per_page:frames_per_page,nframes:ncombinations, $        
             color_index:color_index,symbol_pt_index:symbol_pt_index, $
             symbol_line_index:symbol_line_index,nsub:nsub,xs:xs, $
             x_var:x_var,y_var:y_var,ys:ys,wts:wts,nrow:nrow,ncol:ncol, $
             bottom_label:bottom_label,pt_first:pt_first,pt_npts:pt_npts, $
             multi_pt:multi_pt,initial:0, $
             m_options:'cspne',control:'',$
             j_select:intarr(npts),i_select:intarr(npts),$
             x_select:dblarr(npts),y_select:dblarr(npts),$
             m_button_select:intarr(npts),n_select:0,yfs:yfs,$
             inhid_beg:inhid_beg,inhid_end:inhid_end,tel_bsl:tel_bsl}
    pas=replicate(pas_par,1)
    pas[pindex].frames=distinct_combinations
    pas[pindex].distinct_frames=distinct_combinations
    pas[pindex].nframes=ncombinations
    pas[pindex].saved_par.nframes=ncombinations
  endif ; end delay setting

  if keyword_set(polrx) then result=pass_store(x_var,y_vars,dch_smooth,frame_vars,distinct_sources,plid,pindex, polrx=polrx) else result=pass_store(x_var,y_vars,dch_smooth,frame_vars,distinct_sources,plid,pindex)
  plo_plid_rel,plid
endif

return,pl[plid].num_pages
end
