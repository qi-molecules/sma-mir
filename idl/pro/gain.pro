function gain,gai_souids,gai_fluxes_3mm,gai_fluxes_1mm,tel_bsl, $
         x_var, y_vars,frames_per_page,dt_smooth=dt_smooth,plid, $
         refant=refant, loose=loose, connect=connect, preavg=preavg, $
         no_unwrap=no_unwrap,non_point=non_point, multicolor=multicolor, $
         polar=polar, rec=rec, line=line, $
         calsb=calsb,calband=calband, calbchan=calbchan, calechan=calechan,$
         noplot=noplot, wsmooth=wsmooth, ramp=ramp, polrx=polrx, difif=difif,$
         orig_flux=orig_flux
;
; Gain derivation for amp and phase with interactive fitting.
;
; The data on the gain calibrators will be plotted with multi-frames
; (one for each baseline, rec, continuum band) and multi-colors
; for different sb. Each panel will have subpanels to plot amp, pha 
; and coh seperately but only the amp and pha are fit.
;
; Note : The desired plot device should be set in pl[plid].plot_device
;        before calling gai, eg. pl[plid].plot_device='x' 
;
; parameters : gai_souids -- souids for source to be used for gain cal
;              gai_fluxes_3mm -- 3 mm source fluxes from db archive or user
;              gai_fluxes_1mm -- 1 mm source fluxes from db archive or user
;              tel_bsl    -- 'telescope' or 'baseline' -based fit
;              x_var      -- header variable for x-ccord :'int','hours','el','ha'
;              y_vars     -- variables plotted and fit as y-coord : 
;                            'amp','pha','amp,pha'
;                            For the telescope(antenna)-based calibration, 
;                            cal_type='amp' implies cal_type='amp,pha'
;              frames_per_page -- max number of frames per page
;              dt_smooth -- if dt_smooth > 0, boxcar smoothing is used
;                           and this is the smoothing interval size.
;                           if dt_smooth <=0, polynomial fitting is used,
;                           and this is the order of the polynomial
;              plid      --   plot id number - in IDL only it is generated
;              refant    -- reference antenna used for ant-based calibration
;              loose     -- the program by default requires that at each
;                           solution interval, the number of antennas matches
;                           the number of baselines, that is
;                           n_baselines = (n_antenna)*(n_antenna-1)/2
;                           When there are many antenna/baselines, sometimes
;                           it's OK if a few baselines data are missing.
;                           set loose will ignore this ristriction. Be aware
;                           that when the number of antennas/baselines are
;                           small, a few missing baselines will break the
;                           phase-closure relationship, hence the solution 
;                           may not be reliable. "loose" has no effect in
;                           the tel_bsl='baseline' case.
;              preavg    -- CAUTION: this is done in the "integration" domian.
;                           Not valid if data acquisition was stopped
;                           but restarted with consecutive integration numbers.
;
; result = -1 (error), 0 (no rows) >0 (number of rows)
;
; To do a gain fit for both amp and pha with a maximum of 4 plots per page :  
; eg. : result=dat_filter(s_f,/reset) &  plid=0 & pl[plid].plot_device='x'
; eg. : result=gai_ini(30.,use,gai_souids,gai_sources,all_amps,numbs_3mm,numbs_1mm,gai_fluxes_3mm,gai_fluxes_1mm)
; eg. : result=gain(gai_souids,gai_fluxes_3mm,gai_fluxes_1mm, $
;                  'baseline','int','amp,pha',4,1)
; eg. : result=gain(gai_souids,gai_fluxes_3mm,gai_fluxes_1mm, $
;                   'baseline','int','pha',4,1)
;
common global
common data_set
common plo

;
; setting up constants
;
factors=[0.5,0.75,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.5]
;

; mJD
datobs=c.ref_time[in[pi[0]].iref_time]
months = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
mon=strmid(datobs,0,3)
j=where(mon eq months,count)
num_day_obs=[fix(strtrim(strmid(datobs,8,4),2)),fix(strtrim(string(j[0]+1),2)),(strtrim(strmid(datobs,4,2),2))]
day=strtrim(string(num_day_obs[2]),2)
yr =strtrim(string(num_day_obs[0]),2)
mo =strtrim(string(num_day_obs[1]),2)
mJD=uti_date2mjd(yr,mo,day)

; a quick check on input parameters

if (x_var eq 'el') and y_vars ne 'amp' then begin
     print, 'cal_type must be set to amp only'
     return, -1
endif

if ((tel_bsl eq 'telescope') and  (not keyword_set(refant))) then begin
      print, 'NO reference antenna assigned!!!'
      return, -1
endif

if (not keyword_set(loose)) then loose=0
if (not keyword_set(non_point)) then non_point=0
if (not keyword_set(connect)) then connect=0

;
; debugging messages
;
if e.debug then begin
  print,'gai_soiuids = ',gai_souids
  print,'gai_fluxes_3mm = ',gai_fluxes_3mm
  print,'gai_fluxes_1mm = ',gai_fluxes_1mm
  print,'tel_bsl = ',tel_bsl
  print,x_var
  print,y_vars
  print,frames_per_page
  print,dt_smooth
  print,plid
  if (keyword_set(refant)) then print,refant
  if (keyword_set(loose)) then print,loose
endif
;
; interactive device setup
;
if not e.java then begin
plid = plo_plid_gen()
endif

;
; setting up plot label 
;
   case x_var of
     'hours' : bottom_label='reference time :'+c.ref_time(in(pil(0)).iref_time)
     'int'   : bottom_label='track : '+strtrim(string(in(pil(0)).traid),2)
     'ha'    : bottom_label='track : '+strtrim(string(in(pil(0)).traid),2)
     'el'    : bottom_label='track : '+strtrim(string(in(pil(0)).traid),2)
     else    : begin 
               print,'*** ',x_var,' not recognized x-coord !'
               return,-1
               endelse
   endcase
;
; in case user has not called gai_ini
;
if n_elements(gai_souids) eq 0 then begin
  result=gai_ini(30.,gai_souids,all_sources,all_amps,all_numbs, $
                gai_fluxes_3mm,gai_fluxes_1mm,use)
  j=where(use ne 0,count)
  if count gt 0 then begin
    gai_souids=gai_souids[j] 
    gai_fluxes_3mm=gai_fluxes_3mm[j] & gai_fluxes_1mm=gai_fluxes_1mm[j]
  endif
endif
;
; setting up integration header range
;
  result=dat_list(s_l, '"band" like "c" and "wt" gt "0"', /no_notify,/reset)
  inhid_beg=min([in(pil).inhid]) & inhid_end=max([in(pil).inhid])
  if (e.debug) then print, 'integration header limits ',inhid_beg, inhid_end
;
; check if there is any cal data at all
;
souid_str="("
k=0
for i=0,n_elements(gai_souids)-1 do begin
     k=k+1
     if k ne 1 then souid_str=souid_str+' or '
     souid_str=souid_str+'"souid" eq "'+strtrim(string(gai_souids[i]),2)+'"'
endfor
souid_str=souid_str+")"

if keyword_set(polrx) then begin
   result=dat_list(s_l,souid_str+' and "band" like "c" and (("pol" eq "RR") or ("pol" eq "LL")) and "wt" gt "0"', /no_notify, /reset)
endif else begin
   result=dat_list(s_l,souid_str+' and "band" like "c" and "wt" gt "0"', /no_notify, /reset)
endelse

if  keyword_set(polar) then begin
   print, 'HERE IS POLARIZATION FILTER'
   command='"ipol" eq "'+strtrim(polar[0],2)+'"'
   npolar=n_elements(polar)
   if npolar gt 1 then for i=1,npolar-1 do command=command+' or "ipol" eq "'+strtrim(polar[i],2)+'"'
   print,command
   result=dat_list(s_l,command)     
endif

if result le 0 then begin
   result=dat_list(s_l,/reset)
   print,'No gain derivation due to lack of data (check filter).'
   return, -1
endif

;
; a quick check for the antenna-based case for mininum baselines & reference antennas
;
if (tel_bsl eq 'telescope') then begin

  alltels = [bl[pbl].itel1,bl[pbl].itel2]
  distinct_tels=alltels(  uniq(alltels,sort(alltels) ))
  ndistinct_tels = n_elements(distinct_tels)
  ref_is = where(distinct_tels eq refant)

  if (ref_is eq -1) then begin
      print, 'Reference antenna does not exist in the calibrator data at all!'
      return, -1
  endif

  if (y_vars eq 'pha') then begin
    if (ndistinct_tels lt 3) then begin
      print, 'A mininum of 3 antennas is needed for phase cal'
      return, -1
      ; if passed, at least there are overall more than 3 antennas
      ; not gauranteed there are > 3 ants at any given interval
    endif 
  endif else begin
    if (ndistinct_tels lt 4) then begin
      print, 'A mininum of 4 antennas is needed for amp cal'
      return, -1
      ; if passed, at least there are overall more than 4 antennas
      ; not gauranteed there are > 4 ants at any given interval
    endif  
    if (y_vars eq 'amp') then begin 
      print, '**** NO antenna-based amp-only calibration ****'
      print, '**** Will use amp/phase combo calibration ****'
      y_vars = 'amp,pha'
    endif
  endelse

endif

;
; prepare OBSERVED uv data (first select uvdata and get integration limits)
;

  if (e.debug) then print,"PREPARING OBSERVED UV DATA"

  if (not keyword_set(line)) then begin

    if (not keyword_set(rec)) then begin
       if keyword_set(polrx) then result=dat_list(s_l,souid_str+' and "band" like "c" and (("pol" eq "RR") or ("pol" eq "LL")) and "wt" gt "0"', /no_notify, /reset) else result=dat_list(s_l,souid_str+' and "band" like "c" and "wt" gt "0"',/no_notify,/reset)
    endif else begin
      result=dat_list(s_l,souid_str+' and "rec" like "'+strtrim(rec,2)+'" and "band" like "c" and "wt" gt "0"', /no_notify,/reset)
    endelse

    if keyword_set(difif) then begin
       print,'Setting up DIFIF term ...'
       if tag_exist(bl,'tpvar') then begin
          amps=bl[pbl].sigcoh
          phas=uti_pha_180(bl[pbl].tpvar)
       endif else begin
          amps=bl[pbl].blhdbl6
          phas=bl[pbl].blhdbl5
       endelse
    endif else begin
       amps=bl(pbl).ampave
       phas=uti_pha_180 (bl(pbl).phaave )
    endelse
    cohs=bl(pbl).coh
    wts=sp(psl).wt

  endif else begin

    if (not keyword_set(rec)) then begin
      result=dat_list(s_l,souid_str+' and "sb" like "'+strtrim(calsb,2)+'" and "band" like "'+strtrim(calband,2)+'" and "wt" gt "0"', /no_notify,/reset)
    endif else begin
      result=dat_list(s_l,souid_str+' and "rec" like "'+strtrim(rec,2)+'" and "sb" like "'+strtrim(calsb,2)+'" and "band" like "'+strtrim(calband,2)+'" and "wt" gt "0"', /no_notify,/reset)
    endelse

    linedata = make_array( n_elements(pcl), /complex)

    for pclcount = 0L,(n_elements(pcl) -1) do begin
      linedata[pclcount] = total(ch[pcl(pclcount)+calbchan:pcl(pclcount)+calechan])/(calechan-calbchan+1)
    endfor

    uti_conv_apc, linedata, amps, phas, /amp_pha
    cohs=bl(pbl).coh
    wts=sp(psl).wt

  endelse

  if (keyword_set(preavg)) then begin

    print,"[PREAVG SELECTED} Calculating visibility averages"

    pil_bak=pil & pbl_bak=pbl & psl_bak=psl & pcl_bak=pcl & prl_bak=prl

;    totalints = in(pil).int
;    intlist = totalints(  uniq(totalints,sort(totalints) ))
;    nofint = n_elements(intlist)
 
    tmpblcd=strcompress(string(bl[pbl[0]].iblcd),/remove)
    tmprec=strcompress(string(bl[pbl[0]].irec),/remove)
    tmpsb=strcompress(string(bl[pbl[0]].isb),/remove)
    command=' "iblcd" eq "'+tmpblcd+'" and "iband" eq "0" and "irec" eq "'+tmprec+'" and "isb" eq "'+tmpsb+'"'
    result=dat_list(s_l,command,/reset,/no_notify)
    intlist=in[pil].int 
    nofint=n_elements(intlist)
    templist=in[pil].isource 
    tempjump=where( (templist-shift(templist,1) ne 0) or (intlist - shift(intlist,1) ne 1))
    gindex_from=tempjump
    ngroup=n_elements(tempjump)
    gindex_to=gindex_from[1:ngroup-1]-1L
    gindex_to=[gindex_to,nofint-1]
    ;stop
    ;print, intlist(gindex_from)
    ;print, intlist(gindex_to)
    
;    gindex_from=where((intlist - shift(intlist,1)) ne 1)
;    gindex_to=where((intlist - shift(intlist,-1)) ne -1)
    gindex_mid= (gindex_from+gindex_to)/2
;    ngroup = n_elements(gindex_mid)

    pil=pil_bak & pbl=pbl_bak & psl=psl_bak & pcl=pcl_bak & prl=prl_bak

    print,'VISILITIES WILL BE GROUP AND AVERAGED'
    print,'OVER INTEGRATIONS IN THE FOLLOWING WAY:'
    print,'      GROUP                 INTEGRATION'
    for ib = 0, ngroup -1 do begin
       print, ib+1, '   int ',intlist(gindex_from(ib)), ' to ',intlist(gindex_to(ib))
    endfor
    
    marker = make_array(n_elements(pil),/int)
    grptag = make_array(n_elements(pil),/int)

;    for i = 0, ngroup -1 do begin
;      tmp_idx = where(in(pil).int eq intlist(gindex_mid(i)))
;      marker(tmp_idx) = 1
;    endfor
;    marker_idx = where (marker ne 0)

    bls=c.blcd(bl(pbl).iblcd)
    recs=c.rec(bl(pbl).irec)
    ;
    ; change from rec=1,2 to 3mm,1mm
    ;
    j=where(recs eq '1')
    if max(j) ne -1 then recs(j)= '3mm' 
    j=where(recs eq '2')
    if max(j) ne -1 then recs(j)= '1mm'
    ;
    ; Upper Case sb and band, then find all distinct combinations 
    ; of blcd, receiver, sb, and band
    ;
    sbs=strupcase(c.sb(bl(pbl).isb))
    bands=strupcase(c.band(sp(psl).iband))
    ;
    ; setup the variables which will discriminate frames
    ;
    frames=bls+' '+recs+' '+sbs+' '+bands
    distinct_frames=uti_distinct(frames,nframes,/many_repeat)

    for ia=0,nframes-1 do begin
      for ib = 0, ngroup -1 do begin

        js=where(frames eq distinct_frames(ia) and wts gt 0. and $
                 in(pil).int ge intlist(gindex_from(ib)) and $
                 in(pil).int le intlist(gindex_to(ib)) ,count)
        if (count le 0) then begin
          if e.debug then print, "missing data for group ", ib+1, " in ", distinct_frames(ia)
        endif else begin

;          to_mark = 0
;          ic=0
;          while (to_mark eq 0 and ic lt ngroup) do begin
;             tmp_idx = where(in(pil(js)).int eq intlist(gindex_mid(ic)),tmp_count)
;             stop,ia,ib,to_mark,ic
;             if (tmp_count ne 0) then to_mark = 1
;             ic=ic+1
;          endwhile

          distinct_source=uti_distinct(in[pil[js]].isource,ns,/many_repeat)
          if ns gt 1 then begin
             print,'Source has been changed between integration #',intlist(gindex_from(ib)), ' to ',intlist(gindex_to(ib))
             print,'Please flag out the integration during antenna slewing!'
             print,'Quit !'
             return,0
          endif

          tmp_idx = count/2

          if (e.debug) then begin
            print, 'JS ', js
            print, 'INTs ', frames(js),in(pil(js)).int
            print,'tmp_idx is ', tmp_idx
          endif

          tmp_data=make_array(n_elements(js),/dcomplex)
          uti_conv_apc, tmp_data, amps(js), phas(js), /complex

          tmp_avg = total(tmp_data)/float(n_elements(js))
          uti_conv_apc, tmp_avg, tmp_ampave, tmp_phaave, /amp_pha

          amps(js(tmp_idx))=tmp_ampave
          phas(js(tmp_idx))=tmp_phaave

          marker(js(tmp_idx)) = 1
          grptag(js(tmp_idx)) = ib+1

          if (e.debug) then begin
            print, 'DATA ', tmp_data,tmp_avg
            print,js(tmp_idx),tmp_ampave,tmp_phaave
          endif

        endelse

      endfor
    endfor

    marker_idx = where (marker ne 0)

    amps = amps(marker_idx)
    phas = phas(marker_idx)
    cohs = cohs(marker_idx)

    grptag = grptag(marker_idx)

    pil = pil(marker_idx)
    pbl = pbl(marker_idx)
    psl = psl(marker_idx)
    pcl = pcl(marker_idx)
    prl = prl(marker_idx)

  endif

;
; set up MODEL uv data
;
  if (e.debug) then print,"SETTING UP MODEL UV DATA"
;
; POINT SOURCES (quasars, setting up fluxes first)
;
for i=0,n_elements(gai_souids)-1 do begin
  ;
  ; if missing 1 or 3mm flux use the other, also cut out source with neither
  ; for different freqs use -0.5 spectral index
  ;
  if gai_fluxes_1mm[i] eq 0. then gai_fluxes_1mm[i] = gai_fluxes_3mm[i] / 0.66
  if gai_fluxes_3mm[i] eq 0. then gai_fluxes_3mm[i] = gai_fluxes_1mm[i] * 0.66
endfor

; modelamp (simply the input amplitude)
; modelpha (simply zero)
modelamps=make_array(n_elements(pbl),/float)
modelphas=make_array(n_elements(pbl),/float)

for j=0,n_elements(gai_souids)-1 do begin
    k=where(in[pil].souid eq gai_souids[j] and bl[pbl].irec ge 1,count)
    if count gt 0 then modelamps[k]=gai_fluxes_1mm[j]
    kk=where(in[pil].souid eq gai_souids[j] and bl[pbl].irec eq 0,count)
    if count gt 0 then modelamps[kk]=gai_fluxes_3mm[j]
;    print,j,gai_souids[j],gai_fluxes_1mm[j],gai_fluxes_3mm[j]
;    if (c.source[gai_souids[j]] eq 'uranus' or
;    c.source[gai_souids[j]] eq 'neptune' or c.source[gai_souids[j]]
;    eq 'callisto')  then begin
    kkk=where(in[pil].souid eq gai_souids[j])
; 1830-210 is turned out to be a gravitationally lensed 'double'
; source with raoff 0.35 and decoff 0.394 arcsecs. 
    if (c.source[gai_souids[j]] eq '1833-210' or c.source[gai_souids[j]] eq '1830-210') then begin
    print, 'Correction for 1833-210 or 1830-210 applied !'
       dph=bl[pbl].u*1000.*(0.35/3600.)*(!pi/180.)+bl[pbl].v*1000.*(0.394/3600.)*(!pi/180.)
       modelamps[kkk]=modelamps[kkk]*abs(cos(dph[kkk]*!TWOPI))
       jj=where(cos(dph[kkk]*!TWOPI) lt 0., count)
       if count gt 0. then modelphas[kkk[jj]]=180.
    endif
    if ((non_point gt 0.) and (in[pil[kkk[0]]].size gt 0.)) then begin
      uvdis=sqrt(bl[pbl].u^2 + bl[pbl].v^2)
      radius = in[pil].size / 2.0 
      bw     = sp[psl].fres
      freq   = sp[psl].fsky    
      datatime = mJD+in[pil].dhrs/24.
      if keyword_set(orig_flux) then res = flux_primary(strlowcase(c.source[gai_souids[j]]),radius,freq,xflux) else res=flux_casa(strlowcase(c.source[gai_souids[j]]),radius,freq,bw,datatime,xflux) 
;      kkk=where(in[pil].souid eq gai_souids[j])
      result=uti_gaussqs(uvdis(kkk),radius(kkk),freq(kkk),vis)
      if res lt 0. then begin
         print,'***************************************'
         print,'***************************************'
         print,' Warning: No internal flux model for **'
         print,'            ',c.source[gai_souids[j]]
         print,'   Will use the input value as the   **'
         print,'   zero-spacing flux.                **'
         print,'***************************************'
         print,'***************************************'
         print,''
         print,format='("Gain cal source(s) ",A,T35,"  flux (Jy): ",F12.3)',c.source[gai_souids[j]], modelamps[kkk[0]]
         print,''
         modelamps[kkk]=abs(vis)*modelamps[kkk] 
         jj=where(vis lt 0.,count)
         if count gt 0. then modelphas[kkk[jj]]=180.
      endif else begin
         modelamps[kkk]=abs(vis)*xflux(kkk)
         jj=where(vis lt 0.,count)
         if count gt 0. then modelphas[kkk[jj]]=180.
         print,''
         print,format='("Gain cal source(s) ",A,T35,"  flux (Jy): ",F12.3)',c.source[gai_souids[j]], mean(xflux[kkk])
         print,''
      endelse
   endif else begin
      print,''
;      print,'Gain Cal source(s)      ',c.source[gai_souids[j]],': ', modelamps[kkk[0]],' Jy'
      print,format='("Gain cal source(s) ",A,T35,"  flux (Jy): ",F12.3)',c.source[gai_souids[j]], modelamps[kkk[0]]
      print,''
   endelse
endfor 

;
; PLANETS (or SATIILITES)
;
; not implemented yet

;
; USER INPUT MODELS
; not implemented yet

;
; normalize by source fluxes
;
;
; normalize data by source model in complex-variable form
;

   if (e.debug) then print,"DATA NORMALIZATION by SOURCE MODEL"
  
   complexdata=make_array(n_elements(pbl),/dcomplex)
   uti_conv_apc, complexdata, amps, phas, /complex

   complexmodel=make_array(n_elements(pbl),/dcomplex)
   uti_conv_apc, complexmodel, modelamps, modelphas, /complex

   complexr=make_array(n_elements(pbl),/dcomplex)
   complexr = complexdata/complexmodel

   complexcr=make_array(n_elements(pbl),/dcomplex)
   complexcr= conj(complexdata)/complexmodel

;
; Generate gain curve for display/fitting
;
;
case tel_bsl of
;
; baseline-based gain-curve
;
  'baseline': begin

    print,'**** BASELINE-BASED GAIN CALIBRATION **** '

    ampg=make_array(n_elements(pbl),/float)
    phag=make_array(n_elements(pbl),/float)

    ; amp/phase gain curve is the amp/phase from the normalized ratio
    uti_conv_apc, complexr, ampg, phag, /amp_pha

;
; setting up the corresponding x-coord array
;
    case x_var of
      'hours' : xs=bl(pbl).avedhrs
      'int'   : xs=in(pil).int
      'ha'    : xs=in[pil].ha
      'el'    : xs=in[pil].el
    endcase

;
; segment gain curves (y-coord) by blcd, receiver, sb, and band
;

    bls=c.blcd(bl(pbl).iblcd)
    recs=c.rec(bl(pbl).irec)
    ;
    ; change from rec=1,2 to 3mm,1mm
    ;
    j=where(recs eq '1')
    if max(j) ne -1 then recs(j)= '3mm' 
    j=where(recs eq '2')
    if max(j) ne -1 then recs(j)= '1mm'
    ;
    ; Upper Case sb and band, then find all distinct combinations 
    ; of blcd, receiver, sb, and band
    ;
    sbs=strupcase(c.sb(bl(pbl).isb))
    bands=strupcase(c.band(sp(psl).iband))
    ;
    ; setup the variables which will discriminate frames
    ;
    frames=bls+' '+recs+' '+sbs+' '+bands
    distinct_frames=uti_distinct(frames,nframes,/many_repeat)

    if (e.debug) then print,'distinct_frames include: ', distinct_frames

    ;
    ; setting up color/symbol indeices
    ;
    if keyword_set(multicolor) then colors=strupcase(c.source(in(pil).isource)) $
                         else colors=''
    symbols=''

  end
;
; end of baseline-based fit
;

;
; antenna-based gain-curve
;
  'telescope': begin

    print,'**** ANTENNA-BASED GAIN CALIBRATION **** '

; 
;   set up record-based data for fitting
;

    wt = wts
    vm = conj(complexmodel) *       complexdata * wt
    vv =  abs(complexmodel) * abs(complexmodel) * wt
    mm =   abs(complexdata) *  abs(complexdata) * wt

;
; segment the data by receiver(rx%), sb(u/lsb), band(c%,s%), and integrations
;

    recs=c.rec(bl(pbl).irec)
;
; change from rec=1,2 to 3mm,1mm
;
    jj=where(recs eq '1')
    if max(jj) ne -1 then recs(jj)= '3mm' 
    jj=where(recs eq '2')
    if max(jj) ne -1 then recs(jj)= '1mm'
;
; sb, band, and integrations
;
    sbs=strupcase(c.sb(bl(pbl).isb))
    bands=strupcase(c.band(sp(psl).iband))
    int = strtrim(string((in(pil).int)),1)
;
; create all distinct combinations of receiver, sb, band, and int
; setup the variables which will discriminate frames
;
    if (not keyword_set(preavg)) then begin
      frames=recs+' '+sbs+' '+bands+' '+int
    endif else begin
      grps = strtrim(string(grptag),1)
      frames=recs+' '+sbs+' '+bands+' '+grps
    endelse

    distinct_frames=uti_distinct(frames,nframes,/many_repeat)
    if (e.debug) then print,' distinct_frames include: ', distinct_frames

; NOTE: currently, data are segmented by rx,sb,band AND INTEGRATIONS
;       gain solutions are obtained on an "integration" bases
;       time-averaging is then done in the later interactive-fitting part
;       This is not optimal if Tsys varies significantly, as the relative
;       weights among different ints are not correctly taking into accout.
;       Similar time averaging can also be done by moving the time-averaging
;       (time-convolving) part from the interactive-fitting part up to here,
;       but again weights are not correctly treated.
;
;       The correct(?) approach is to time-segment, instead of integration-segment,
;       data correctly using the desired time-interval. In this case, each
;       data point will be used with a proper weight in one-time interval.
;       This will be different from a "boxcar" time-smoothing fashion,
;       further smoothing-fitting will then be done in the interactive-fitting part.
;       This also requires a "re-bin" in the x-axis in display, since the number
;       of x coordinates will be already different from the original.

;
; get the combination counts for making the ant-based gain solution array
;
    distinct_recs=uti_distinct(recs,nofrec,/many_repeat)
    distinct_sbs=uti_distinct(sbs,nofsb,/many_repeat)
    distinct_bands=uti_distinct(bands,nofband,/many_repeat)

    if (not keyword_set(preavg)) then begin
      totalints = in(pil).int
      intlist = totalints(  uniq(totalints,sort(totalints) ))
      nofint = n_elements(intlist)
    endif else begin
      grplist = grptag(  uniq(grptag,sort(grptag) ))
      nofgrp = n_elements(grplist)
    endelse

    totaltels = [bl[pbl].itel1,bl[pbl].itel2]
    tellist = totaltels(  uniq(totaltels,sort(totaltels) ))
    noftel    = n_elements(tellist)
 
    if (not keyword_set(preavg)) then begin
      if e.debug then print,"Number of rec, sb, band, int, antenna:", $
                      nofrec,nofsb,nofband,nofint,noftel
  
      gain_size = nofrec * nofsb * nofband * nofint * noftel
    endif else begin
      if e.debug then print,"Number of rec, sb, band, group, antenna:", $
                      nofrec,nofsb,nofband,nofgrp,noftel
  
      gain_size = nofrec * nofsb * nofband * nofgrp * noftel
    endelse

        xs = make_array(gain_size,/float)
      ampg = make_array(gain_size,/float,value=1.0)
      phag = make_array(gain_size,/float,value=0.0)
       coh = make_array(gain_size,/float,value=1.0)
       wt2 = make_array(gain_size,/float,value=-1.0)
    pframe = make_array(gain_size,/string)
    pcolor = make_array(gain_size,/string)

    plotframes=recs+' '+sbs+' '+bands

;
; some more minor initialization
;
    niter_max = 100
    epsi1=1.0e-8
    epsi2=1.0e-4

    good_sol = 0
;
; start looping all segments
;

    for fcount=0,(nframes -1) do begin
      ;
      ; going through dataset by rx%,u/lsb,c%/s%, and int
      ;
      j=where(frames eq distinct_frames[fcount] and wts gt 0.,count)

      if (e.debug) then print, 'Solving: ',distinct_frames[fcount]
      ;
      ; quick check on the data integity
      ;

      weight = 1.

      alltels = [bl[pbl[j]].itel1,bl[pbl[j]].itel2]
      listtel = alltels(  uniq(alltels,sort(alltels) ))
      ntel    = n_elements(listtel)
      if (e.debug) then print, distinct_frames[fcount]+' has total ', ntel,' antennas'

      allbsls = [bl[pbl[j]].iblcd]
      listbsl = allbsls(  uniq(allbsls,sort(allbsls) ))
      nbsl   = n_elements(listbsl)
      if (e.debug) then print, distinct_frames[fcount]+ ' has total ', nbsl,' baselines'

      if (nbsl ne ((ntel)*(ntel-1)/2)) then begin
         print, 'WARNING: mismatch between number of ANTENNAS and number of BASELINES at '+ $
                          distinct_frames[fcount]+'.'
         if (not keyword_set(loose)) then begin
            ; strict flagging, automatic flag data at this scan as bad gain-cal points
            print, '         all records at this rx/sb/band/int are flagged in solution derivation !!!'
            weight = -1.
         endif else begin
            print, '         "loose" setting selected, will continue to derive the gain solution.'
            print, '         Phase closure condition may not be satisfied by the data in this segment.'
         endelse
      endif

      if (where(listtel eq refant) eq -1) then begin
         print, 'WARNING: missing data from the reference antenna at ' + $
                          distinct_frames[fcount]+'.'
         print, '         all records at this rx/sb/band/int are flagged in solution derivation !!!'
         weight = -1.
      endif

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

           ptel1 = where(listtel eq bl[pbl[j[ibsl]]].itel1)
           ptel2 = where(listtel eq bl[pbl[j[ibsl]]].itel2)

           sum(ptel1) = sum(ptel1) + telg(ptel2) * vm(j[ibsl])
           sum(ptel2) = sum(ptel2) + telg(ptel1) * conj(vm(j[ibsl]))

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

      if (e.debug) then print,'Total iteration : ',niter

      if (incremt/ntel lt epsi2) then converge = 1 else converge = -1

      ;
      ; do another "amp/phase-combo" minimization if necessary
      ; with the earlier phase solution used as a initial guess
      ;

      if (converge eq 1 and y_vars eq 'amp,pha') then begin 

        ;
        ; a bit more calculations for the inital guess 
        ;
        sumrvm = 0
        sumrvv = 0

        for ibsl = 0, (n_elements(j) -1) do begin
  
           ptel1 = where(listtel eq bl[pbl[j[ibsl]]].itel1)
           ptel2 = where(listtel eq bl[pbl[j[ibsl]]].itel2)

           sumrvm = sumrvm + conj(telg(ptel1)) * telg(ptel2) * vm(j[ibsl])
           sumrvv = sumrvv + vv(j[ibsl])

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
  
             ptel1 = where(listtel eq bl[pbl[j[ibsl]]].itel1)
             ptel2 = where(listtel eq bl[pbl[j[ibsl]]].itel2)

             sum(ptel1) = sum(ptel1) + telg(ptel2) * vm(j[ibsl])
             sum(ptel2) = sum(ptel2) + telg(ptel1) * conj(vm(j[ibsl]))

             sum2(ptel1) = sum2(ptel1) + abs(telg(ptel2))^2 * vv(j[ibsl])
             sum2(ptel2) = sum2(ptel2) + abs(telg(ptel1))^2 * vv(j[ibsl])

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
        print, 'No convergance at '+distinct_frames[fcount]
        print, 'All gains are set to 1.0 (no gain corrections!)'
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
      ; get index to store xs, plotframe/label, and ant-based gain solution
      ;
      if (not keyword_set(preavg)) then begin
        curint = in(pil[j(0)]).int
        index_int = where(intlist eq curint)
      endif else begin
        curgrp = grptag(j(0))
        index_grp = where(grplist eq curgrp)
      endelse

;      if (c.rec(bl(pbl(j(0))).irec) eq '1') then currec = '3mm'
;      if (c.rec(bl(pbl(j(0))).irec) eq '2') then currec = '1mm'
      currec = c.rec(bl(pbl(j(0))).irec)
      index_rec = where(distinct_recs eq currec)

      cursb = strupcase(c.sb(bl(pbl(j(0))).isb))
      index_sb = where(distinct_sbs eq cursb)

      curband=strupcase(c.band(sp(psl).iband))
      index_band = where(distinct_bands eq curband)

      if (keyword_set(preavg)) then $
      curindex = (index_rec + (index_sb * nofrec) + index_band * (nofrec * nofsb) + $
           index_grp * (nofrec * nofsb * nofband )) * noftel

      if (not keyword_set(preavg)) then $
      curindex = (index_rec + (index_sb * nofrec) + index_band * (nofrec * nofsb) + $
           index_int * (nofrec * nofsb * nofband )) * noftel

      ;
      ; fill in the corresponding x-coord array
      ;
      case x_var of
        'hours' : xs(curindex:curindex+noftel-1) = bl(pbl(j(0))).avedhrs
        'int'   : xs(curindex:curindex+noftel-1) = in(pil[j(0)]).int
        'ha'    : xs(curindex:curindex+noftel-1) = in[pil[j(0)]].ha
        'el'    : xs(curindex:curindex+noftel-1) = in[pil[j(0)]].el
      endcase

      ;
      ; converting antenna complex gains into amp+pha terms
      ;
      uti_conv_apc, telg, tmp_amp, tmp_pha, /amp_pha

      ;
      ; fill in the corresponding gain solution array and other 
      ; arrays for plotting purposes
      ;
      if (ntel eq noftel) then begin
        ; full antennas are present for this interval
        ampg(curindex:curindex+noftel-1) = tmp_amp
        phag(curindex:curindex+noftel-1) = tmp_pha
        coh(curindex:curindex+noftel-1) = 1.0
        wt2(curindex:curindex+noftel-1) = weight
        pframe(curindex:curindex+noftel-1) = string(tellist) + ' ' + plotframes(j(0))
        pcolor(curindex:curindex+noftel-1) = strupcase(c.source(in(pil(j(0))).isource))

        ;      print, "NOFILL In ",  distinct_frames[fcount], " assign PFRAME FOR ",curindex, curindex+noftel-1
        ;      stop,pframe(curindex:curindex+noftel-1)

      endif else begin 
        ; some missing antenna(s) for this interval
        ; fill in the gain solution array for the antennas that are present
        ; and fill in the rest with unit gain and NEGATIVE weight
        itel = 0
        jtel = 0
        while (itel lt noftel) do begin
          if (jtel lt ntel) then begin
            if (tellist(itel) eq listtel(jtel)) then begin
              ampg(curindex+itel) = tmp_amp(jtel)
              phag(curindex+itel) = tmp_pha(jtel)
              coh(curindex+itel) = 1.0
              wt2(curindex+itel) = weight
              itel = itel + 1
              jtel = jtel + 1
            endif else begin
              ampg(curindex+itel) = 1.
              phag(curindex+itel) = 0.
              coh(curindex+itel) = 1.
              wt2(curindex+itel) = -1.
              itel = itel + 1
            endelse
          endif else begin
            ampg(curindex+itel) = 1.
            phag(curindex+itel) = 0.
            coh(curindex+itel) = 1.
            wt2(curindex+itel) = -1.
            itel = itel + 1
          endelse
        endwhile

        pframe(curindex:curindex+noftel-1) = string(tellist) + ' ' + plotframes(j(0))
        pcolor(curindex:curindex+noftel-1) = strupcase(c.source(in(pil(j(0))).isource))
        ;      print, "TOFILL In ",  distinct_frames[fcount], " assign PFRAME FOR ",curindex, curindex+noftel-1
        ;      stop,pframe(curindex:curindex+noftel-1)

      endelse

    endfor

    ;
    ; Removing empty frame points which were reserved from certain 
    ; int(group)/rx/sb/blcd combination but not solved and filled
    ; due to complete missing data on all baselines
    ;
    j_goodframe = where(strcmp(pframe,'') ne 1)

    xs=xs[j_goodframe]
    ampg=ampg[j_goodframe]
    phag=phag[j_goodframe]
    coh=coh[j_goodframe]
    wt2=wt2[j_goodframe]
    pframe=pframe[j_goodframe]
    pcolor=pcolor[j_goodframe]

    ;
    ; putting new wt2,coh into wts,cohs (was in use earlier) for plotting
    ;
    wts = wt2
    cohs = coh

    ;
    ; putting new pframe into frames and create new distinct_frames for plotting
    ;
    frames=pframe
    distinct_frames=uti_distinct(frames,nframes,/many_repeat)
    if (e.debug) then print,' distinct_frames include: ',distinct_frames

    ;
    ; setting up color/symbol indeices
    ;
    if keyword_set(multicolor) then colors = pcolor else colors=''
    symbols=''

  end
;
; end of antenna-based fit
;
endcase

;
; Reform useable gain curves with good data only
;

; plot coherence only for amps > 0.4
;
;
;j=where(amps lt 0.4,count)
;   if count gt 0 then cohs(j) = !BAD_VALUE

;
; setting up pt indices 
;
 pt_first=0
 pt_npts=0
 multi_pt=0
;
; remove zero wt pts
;
 j=where(wts le 0.,count)
   if count gt 0 then begin
   ampg[j]=!BAD_VALUE
   phag[j]=!BAD_VALUE
   cohs[j]=!BAD_VALUE
 endif

;
; SETTING UP INTERACTIVE PLOTS
;

;
; set up gain/plot index
;
 gindex = 0

 pl[plid].plot_type='gai'
 if not e.java then pl[plid].plot_interact=plo_init(plid,gindex)

;
; setting up for color/symbol in plots
;
 distinct_colors=uti_distinct(colors, /many_repeat,ncolors)
 color_index=0.6*bytscl((indgen(max([n_elements(distinct_colors),1])) $
             mod 16)+1)+64.

 distinct_symbols=''
 symbol_pt_index=2 & symbol_line_index=0

;
; setting up all plotting variables
;
npts=n_elements(xs)

xmin=min(xs) & xmax=max(xs)
dx=xmax-xmin & xmin=xmin-0.05*dx & xmax=xmax+0.05*dx
ymin=0. & amp_max=max(ampg)
dy=amp_max-ymin & ymin=ymin-0.05*dy & amp_max=amp_max+0.05*dy
nor_xmin=0.1 & nor_xmax=0.95 & nor_ymin=0.1 & nor_ymax=0.94
if n_elements(distinct_colors) gt 2 then begin
 nor_xmax=0.95-0.05 
endif

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
nsub=0
case y_vars of
     'amp,pha': begin    ; actually 'amp,pha,coh'
                      nsub=3 
                      y_var=['amp','pha','coh'] & psym=[10,1,1]
                      sub_fymin=make_array(nsub,/float,value=0.) 
                      sub_fymax=make_array(nsub,/float,value=1.)
                      sub_fymin(2)=0.0 & sub_fymax(2)=0.2 
                      sub_fymin(1)=0.2 & sub_fymax(1)=0.5 
                      sub_fymin(0)=0.5 & sub_fymax(0)=1.
                      ymin=make_array(nframes,nsub,/float,value=0.)
                      ymax=make_array(nframes,nsub,/float,value=amp_max)
                      ymin(*,2)=0.4 & ymax(*,2)=1.2
                      ys=make_array(nsub,npts,/float,/nozero) 
                      ys(0,*)=ampg & ys(1,*)=phag & ys(2,*)=cohs
                    end
     'amp'    : begin        ; actually 'amp,coh'
                      nsub=2 
                      y_var=['amp','coh'] & psym=[10,1]
                      sub_fymin=make_array(nsub,/float,value=0.) 
                      sub_fymax=make_array(nsub,/float,value=1.)
                      sub_fymax(1)=0.3 & sub_fymin(0)=0.3  
                      ymin=make_array(nframes,nsub,/float,value=0.)
                      ymax=make_array(nframes,nsub,/float,value=amp_max)
                      ymin(*,1)=0.4 & ymax(*,1)=1.2
                      ys=[ampg,cohs]
                      ys=make_array(nsub,npts,/float,/nozero) 
                      ys(0,*)=ampg & ys(1,*)=cohs
                    end
     'pha'    : begin        ; actually 'pha,coh'
                      nsub=2 
                      y_var=['pha','coh'] & psym=[1,1]
                      sub_fymin=make_array(nsub,/float,value=0.) 
                      sub_fymax=make_array(nsub,/float,value=1.)
                      sub_fymax(1)=0.3 & sub_fymin(0)=0.3  
                      ymin=make_array(nframes,nsub,/float,value=0.)
                      ymax=make_array(nframes,nsub,/float,value=amp_max)
                      ymin(*,1)=0.4 & ymax(*,1)=1.2
                      ys=[phag,cohs]
                      ys=make_array(nsub,npts,/float,/nozero) 
                      ys(0,*)=phag & ys(1,*)=cohs
                    end
     'complex' : begin
                      nsub=3 
                      y_var=['amp','pha','coh'] & psym=[10,1,1]
                      sub_fymin=make_array(nsub,/float,value=0.) 
                      sub_fymax=make_array(nsub,/float,value=1.)
                      sub_fymin(2)=0.0 & sub_fymax(2)=0.2 
                      sub_fymin(1)=0.2 & sub_fymax(1)=0.5 
                      sub_fymin(0)=0.5 & sub_fymax(0)=1.
                      ymin=make_array(nframes,nsub,/float,value=0.)
                      ymax=make_array(nframes,nsub,/float,value=amp_max)
                      ymin(*,2)=0.4 & ymax(*,2)=1.2
                      ys=make_array(nsub,npts,/float,/nozero) 
                      ys(0,*)=amps & ys(1,*)=phas & ys(2,*)=cohs
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
    js=where(frames eq distinct_frames(i))
    yt=reform(ys(fir_ind,js),n_elements(js))
    if not keyword_set(no_unwrap) then result=uti_pha_unwrap(yt,smooth=wsmooth,ramp=ramp)
    ys(fir_ind,js)=yt
  endfor
  j=[where(ys[fir_ind,*] ne !BAD_VALUE)]
    uti_range_even,min([ys(fir_ind,j)]),max([ys(fir_ind,j)]),10.,ymi,yma
    ymin(*,fir_ind)=ymi & ymax(*,fir_ind)=yma
endif

  sub_fxmin=make_array(nsub,/float,value=0.) 
  sub_fxmax=make_array(nsub,/float,value=1.)
  xmin=make_array(nsub,/float,value=xmin)
  xmax=make_array(nsub,/float,value=xmax)

;
;  do baseline-based or telescope-based gain fit
;

yfs=make_array(nsub-1,npts,/float)

refit:
if y_vars eq 'amp,pha' or y_vars eq 'pha' then begin 
  fir_ind=0 & if y_vars eq 'amp,pha' then fir_ind=1
  j=[where(ys[fir_ind,*] ne !BAD_VALUE)]
    uti_range_even,min([ys(fir_ind,j)]),max([ys(fir_ind,j)]),10.,ymi,yma
    ymin(*,fir_ind)=ymi & ymax(*,fir_ind)=yma
endif

if (e.debug) then print, 'DT_SMOOTH  ', dt_smooth 

if (connect) then begin

  dt_smooth=-0.5

  result=gain_fit(tel_bsl,'connect',dt_smooth,x_var,y_vars,frames, $
               distinct_frames,xs,ys,wts,yfs)

endif else begin

  if (dt_smooth gt 0) then begin

    result=gain_fit(tel_bsl,'smooth',dt_smooth,x_var,y_vars,frames, $
                 distinct_frames,xs,ys,wts,yfs)

  endif

  if (dt_smooth le 0) then begin

    result=gain_fit(tel_bsl,'poly',dt_smooth,x_var,y_vars,frames, $
                 distinct_frames,xs,ys,wts,yfs)

  endif

endelse

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
gai_par={saved_par:saved_par,nor_xmax:nor_xmax,nor_ymax:nor_ymax, $
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
     m_options:'csfwrpne',control:'',$
     j_select:intarr(npts)-1L,i_select:intarr(npts),$
     x_select:dblarr(npts),y_select:dblarr(npts),$
     m_button_select:intarr(npts),n_select:0,yfs:yfs,$
     inhid_beg:inhid_beg,inhid_end:inhid_end,dt_smooth:dt_smooth}
     
gai=replicate(gai_par,1)
pl[plid].num_pages = ceil(float(gai[gindex].nframes) / float(gai[gindex].frames_per_page))
iframe=0

; START PLOTTING
;

;Plot the first page 
if (pl[plid].plot_device ne 'null') then loadct,39,/silent
j_first=iframe
j_last=min([iframe+frames_per_page-1,nframes-1])
gai[gindex].iframe=iframe & gai[gindex].j_first=j_first & gai[gindex].j_last=j_last
gai[gindex].frames_per_page=frames_per_page & gai[gindex].nframes=nframes
gai[gindex].nrow=nrow & gai[gindex].ncol=ncol
if not keyword_set(noplot) then result=plo_page(plid,gindex)
gai[gindex].color_index=color_index+150.
gai[gindex].nsub=n_elements(gai[gindex].y_var)-1
if not keyword_set(noplot) then result=plo_over_page(plid,gindex)
gai[gindex].nsub=n_elements(gai[gindex].y_var)

if not e.java then begin
;   pl[plid].plot_interact=plo_control(plid,gindex)
   if not keyword_set(noplot) then result=plo_control(plid,gindex)
   if result eq 2 then begin
      wts=gai[gindex].wts
      ys=gai[gindex].ys
;      fir_ind=where(gai[gindex].y_var eq 'pha')
;      if not keyword_set(no_unwrap) then result=uti_pha_unwrap(ys[fir_ind,*])
      pl[plid].plot_interact=1
      goto, refit 
      endif else pl[plid].plot_interact=result
print, "EXITING FROM THE INTERACTIVE MODE"
 
   if keyword_set(polrx) then result=gain_store(tel_bsl,x_var,y_vars,plid,gindex,polrx=polrx) else result=gain_store(tel_bsl,x_var,y_vars,plid,gindex)
   plo_plid_rel,plid
endif

  if (keyword_set(preavg)) then begin

    pil=pil_bak & pbl=pbl_bak & psl=psl_bak & pcl=pcl_bak & prl=prl_bak

  endif

return,pl[plid].num_pages

end
