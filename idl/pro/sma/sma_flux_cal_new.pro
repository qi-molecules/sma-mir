
function flux_scale_new,sources,channel,flux_inp=flux_inp,flags=flags, $
         weight=weight,coh_min=coh_min,div_coh=div_coh,$
         iuse=iuse,source_out=source_out,vflux=vflux,jycts=jycts,$
         wts=wts,sb=sb,iscan=iscan,bsl=bsl,vis=vis,tssb=tssb,$
         coh=coh,el=el,jct_bsl=jct_bsl,distinct_bsl=distinct_bsl,$
         distinct_sb=distinct_sb,scale_bsl=scale_bsl,jct_sig=jct_sig,$
         noprint=noprint,good_frames=good_frames,polar=polar,vis_min=vis_min
;
; Flux calibration (contact Charlie Qi cqi@cfa.harvard.edu for bugs)
; 
; A table will be made to print the average scale factor, the 
;   average scale factors for LSB and USB, and the average scale 
;   factors for each baseline. The user can select which type of
;   scaling they want to do.
;   for example: result=flux_scale('neptune','c1')
;
; To apply the scale factors, call cal_apply: 
;                result=cal_apply(gain='amp')
;
; The user can set their own weights for flux calibration.

    ; Command blocks
      common global
      common data_set

    ; Must be some sources
      nsources = n_elements(sources)
      if (nsources eq 0) then return,-1

    ; Sources and flux_inp must be identical in size
      if (nsources ne n_elements(flux_inp)) then begin
         print,'SOURCES and FLUX_INP must have the same size'
         return,-1
      endif

    ; User ID
      user = '%'
      if keyword_set(user_id) then user = user_id

    ; Determine weighting for data
      iweight = 0
      if keyword_set(weight) then iweight = 1 * weight

    ; Minimum coherence
	if not keyword_set(vis_min) then vis_min = 0.4
      mincoh = keyword_set(coh_min) ? 1.0 * coh_min : 40.0
      if (mincoh lt 1.0) then mincoh = mincoh * 100.0
;      if (e.campuslogin eq 'sma' or e.campuslogin eq 'cfa') then bl.coh=1      

    ; Set filter to extract only continuum data
      s = '"band" eq "' + channel + '"and "wt" gt "0.0"'
      if (dat_select(s_s,s,/reset,/no) eq 0) then return,-1

    ; Construct select statement to extract data
        ; initalize
          command = ''

        ; Make list of source names
          s = '"'
          if (nsources gt 1) then s = '["'
          for i=0,nsources-1 do $
              s = s + sources[i] + '","'
          s = strmid(s,0,strlen(s)-2)
          if (nsources gt 1) then s = s + ']'

        ; Add sources to the command
          command = command + '"source" '
          if (nsources gt 1) then command = command + ' in '
          if (nsources eq 1) then command = command + ' eq '
          command = command + s
          if  keyword_set(polar) then begin
             print, 'HERE IS POLARIZATION FILTER'
             command=command+' and ("ipol" eq "'+strtrim(polar[0],2)+'"'
             npolar=n_elements(polar)
             if npolar gt 1 then for i=1,npolar-1 do command=command+' or "ipol" eq "'+strtrim(polar[i],2)+'"'
             command=command+')'
             print,command
          endif

;print,command
;aa = "aa"
;read,aa

    ; Submit command
      result = dat_list(s_l,command,/reset,/no_notify)

    ; Make sure data was read in
      if (result eq 0) then return,-1

    ; Set number of data points
      ndata  = n_elements(pil)

    ; Store MMA parameters that need to be passed to ION
      source_out = c.source[in[pil].isource]
      iscan = in[pil].int
      tssb = sp[psl].tssb
      bsl   = c.blcd[bl[pbl].iblcd]
      sb    = c.sb[bl[pbl].isb]
      coh   = bl[pbl].coh * 100.0
      el    = in[pil].el



    ; Set weights
      wts = (iweight) ?  sp[psl].wts > 0.0 : replicate(1.0,ndata)

    ; Normalize amplitudes and weights by coherence, if necesary
      xfactor = 1.0
      if keyword_set(div_coh) then begin
         xfactor = fltarr(ndata)
         j = where(coh gt 0.0,nj)
         if (nj gt 0) then xfactor = 100.0 / coh
      end
      ampave = bl[pbl].ampave * xfactor
      wts    = wts * xfactor

    ; Store source parameters. If source is primary flux calibrator,
    ; then re-determine the flux. Otherwise, just adopt the input flux.
      radius = in[pil].size / 2.0
      freq   = sp[psl].fsky
      flux   = fltarr(ndata)
      for i = 0L, n_elements(sources)-1L do begin
         j = where(source_out eq sources[i],nj)
print,'Checking source ',source_out[0]
         if (nj gt 0) then begin
print,'Flags ',flags[i],' all flags ',flags
           if (flags[i] eq 1) then begin
print,'Found primary cal ',sources[i],flux[i]
             result = flux_primary(sources[i],radius[j],freq[j],xflux)
print,'New flux,',xflux
             if (result eq 1) then begin
                flux[j] = xflux
             endif else begin
                print,'no primary calibrator, input flux first.'
                return,-1
             endelse
           endif else begin
             flux[j] = flux_inp[i]
           endelse
         endif
      endfor

    ; Error message if flux is zero
      j = where(flux le 0.0,nj)
      if (nj gt 0) then begin
        print,'Error reading flux for ',c.source[in[pil[j[0]]].isource]
        return,-1
      endif

    ; Compute UV distances
      uvdis = sqrt(bl[pbl].u^2 + bl[pbl].v^2)

    ; Make correction for finite resolution if source is resolved
      result = uti_gaussqs(uvdis,radius,freq,vis)

    ; Set observed fluxes
      vflux = abs(vis) * flux

    ; Compute Jansky per counts
      jycts = dblarr(ndata)
      j = where(ampave gt 0.0,nj)
      if (nj gt 0) then jycts[j] = ampave[j] / vflux[j]
    ; Set which ones to use
      j = where(coh ge mincoh and ampave gt 0.0 and vis ge vis_min,nj)
      iuse = intarr(ndata)
      if nj gt 0 then iuse[j] = 1

    ; Print table
      if not keyword_set(noprint) then begin
      print,'# USE    SOURCE       VFLUX       J/ct      WT   SB INT BSL    VIS    TSSB     COH     EL'

      for i = 0L, ndata-1L do begin
         if (nj eq 0)  or  (iuse[i] eq 1 ) then $
	 print,$
           format='(3x,I2,2x,A8,2x,2(F10.2,2x),F6.2,2x,A1,2x,I3,2x,A3,2x,F5.1,2x,I5,2x,F8.1,2x,F4.1)',$
           iuse[i],source_out[i],vflux[i],1./jycts[i],wts[i],sb[i],iscan[i],$
           bsl[i],vis[i]*100.0,fix(tssb[i]),coh[i],el[i]
      endfor

      for i = 0L, n_elements(sources)-1L do begin
         j = where(source_out eq sources[i],nj)
         if (nj gt 0) then begin
		print,'Maximum coherence  for ',sources[i],' is ',max(coh[j])
		print,'Maximum visibility for ',sources[i],' is ',max(vis[j])
		if (max(coh[j]) lt mincoh or max(vis[j]) lt vis_min) then $
			print,'Coherence or visibility failed for ',sources[i]
	 endif
      endfor
			print,'Minimum coherence  ',mincoh
			print,'Minimum visibility ',vis_min

      if (nj eq 0) then begin
         print,format='(%"Data in band %s does not meet the coherence critera")',channel
         return,-1
      endif

      print,''
      print,'  Flux scaling factor for each sideband:'
   endif

    ; set scale factors for baselines, sidebands, all data
      j=where(iuse eq 1)
      bsls = bsl(j)
      distinct_bsl=uti_distinct(bsls,nbsls,/many_repeat)
      sbs = sb(j)
      distinct_sb=uti_distinct(sbs,nsbs,/many_repeat)
      good_frames = uti_distinct(channel + ' ' + bsls + ' ' + sbs,/many_repeat)
      jct_bsl=make_array(nsbs,nbsls,/double,value=1.d)
      jct_sig=make_array(nsbs,nbsls,/float)
      jct_sb=make_array(nsbs,/double,value=1.d)
      jct_all=total([(jycts[j]*wts[j])])/total([wts[j]])

      for i = 0L, nsbs-1L do begin
         jsb=j[where(sb[j] eq distinct_sb[i])]
         jbls=bsl[jsb]
         jct_sb[i]=total([(jycts[jsb]*wts[jsb])])/total([wts[jsb]])
         n_print=-1
         for k = 0L, nbsls-1L do begin
            n_jbl=-1
            j_bl=where(jbls eq distinct_bsl[k],n_jbl)
            if n_jbl gt 0 then begin
               jbl=jsb[j_bl]
               result = uti_meanvar(jycts[jbl],weights=wts[jbl])
               jct_bsl[i,k] = result[0]
               jct_sig[i,k] = result[2]
               n_print=n_print+i+2
            endif else begin
               jct_bsl[i,k]=jct_sb[i]
            endelse
         endfor   
      endfor
               if not keyword_set(noprint) then print,$
                 format='(2x,A1,4x,E8.2,3x,A1,6x,E8.2)',$
                  distinct_sb[0],1./jct_sb[0],distinct_sb[1],1./jct_sb[1]
      print,''

    ; Reset filter
    ; filter the data to apply scale factors
      result=dat_list(s_l,'"wt" gt "0"',/reset,/no)
      inhid_beg=min([in(pil).inhid]) & inhid_end=max([in(pil).inhid])
      wts=sp(psl).wt
      bls=c.blcd(bl(pbl).iblcd)
      recs=c.rec(bl(pbl).irec)
      sbs=c.sb(bl(pbl).isb)
      bands=c.band(sp(psl).iband)
      frames=bls+' '+recs+' '+sbs+' '+bands
      distinct_frames=uti_distinct(frames,nframes,/many_repeat)

      xs=in(pil).int
      npts=n_elements(pbl)
      yfs=make_array(npts,/double,value=jct_all) 
    
    ; choose the way to flux cal
      fluxcal_sel=0L 
      if not keyword_set(scale_bsl) then fluxcal_sel = 1L

      if fluxcal_sel ge 1L then begin
         for is=0L, nsbs-1L do begin
             js=where(sbs eq distinct_sb[is])
             yfs[js]=jct_sb[is]
             if fluxcal_sel eq 2L then begin
               for ib=0L,nbsls-1L do begin
                  jb=where((bls eq distinct_bsl[ib]) and $
                              (sbs eq distinct_sb[is]))
                 yfs[jb]=jct_bsl[is,ib]
               endfor
             endif
         endfor
      endif 

    ; use cal_store to store the scale factors
      result=cal_store(s_c,init=nframes) & irow=-1
      for i=0,nframes-1 do begin
         result=dat_comb_sep(distinct_frames[i], $
                      ['blcd','rec','sb','band'],codes, $
                      icodes,n_components)      
         js=where(distinct_frames[i] eq frames,n_values)
         js=js[where(yfs[js] ne !BAD_VALUE,n_values)]
         ys=reform([yfs[js]],n_values)
         irow=irow+1
         x_var='int'
         y_vars='amp'
         tel_bsl='baseline'
         dt_smooth=1
         result=cal_store(s_c,'gain',x_var,y_vars,tel_bsl,inhid_beg,inhid_end,$
                         codes,icodes,xs(js),ys,dt_smooth,irow,/save)   
      endfor
      result=cal_store(s_c,/transfer)

    ; Done
      result = dat_select(s_s,/reset,/no)
      return,1
end



function sma_flux_cal_ini_new, sources=sources,fluxes=fluxes,flags=flags

    ; Command blocks
      common global
      common data_set

; Re-set the local list that is used internally
 result=dat_list(s_l,/reset,/no_notify)

; Check for valid filter. 
 if n_elements(pil) eq 0 then begin
   all_souids=-1 & all_sources='' & all_fluxes=0.
   print, "No sources selected. Check previous select or dat_filter commands."
   return,-1
 endif

 all_souids=uti_distinct(in[pil].souid,ndistinct,/many_repeat)
 sources=make_array(ndistinct,/string)
 fluxes=make_array(ndistinct,/float)
 flags=make_array(ndistinct,/float)

 for j=0,ndistinct-1 do begin

; Reset the local list
   result=dat_list(s_l,/reset,/no_notify)

; Choose the data for the current source in the loop.
; i is the pointer to all scans for the current source.
   i=where(in[pil].souid eq all_souids[j])

; Get the source name off the first instance. Store in arg list.
   sources[j]=c.source[in[pil[i[0]]].isource]
      fluxes[j]=in[pil[i[0]]].sflux

; List of scans for this source, this band, with valid weights
   result=dat_list(s_l,'"souid" eq "'+ strtrim(string(all_souids[j]),2) + '"' + $
        ' and "wt" gt "0 " ',$
        /reset,/no_notify)

	if sources[j] eq 'mars'     or sources[j] eq 'uranus' $
	or sources[j] eq 'neptune'  or sources[j] eq 'jupiter' $
	or sources[j] eq 'venus'    or sources[j] eq 'mercury' $
	or sources[j] eq 'saturn'   or sources[j] eq 'pluto' $
	or sources[j] eq 'callisto' or sources[j] eq 'titan' $
	or sources[j] eq 'ganymede' or sources[j] eq 'ceres' $

	then begin
	bl[pbl].iaq = 1
	endif

	flags[j] = bl[pbl[0]].iaq
	word = 'NO'
	if bl[pbl[0]].iaq eq 1 then word = 'PRIMARY'
	if bl[pbl[0]].iaq eq 2 then word = 'SECONDARY'
    ; Print table
      if j eq 0 then print,"Current Table"
      if j eq 0 then print,'#   Source     Flux Cal.           Flux(Jy)'
         print,format='(A10,4x,A10,5x,g10.2)', sources[j],word,fluxes[j]
      endfor


again:

modified = 0
   print,'Enter source, new flux cal code: '
   print,'Use p, P, or 1 as code for primary flux cal'
   print,'Use s, S, or 2 as code for secondary flux cal'
   print,'Use n, N, NO, or 0 to deselect a calibrator'
   print,'or hit Return if all the sources are correctly specified'
   sngc = "sourcename"
   read,sngc
   if sngc eq '' then goto,finish
   modified = 1
   parts = strtrim(strsplit(sngc,' ',/extract),2)
   sn = parts[0]
   gc = parts[1]
   j = 0
   if (gc eq '0') then j = 0
   if (gc eq '1') then j = 1
   if (gc eq '2') then j = 2
   if STRCMP(gc, 'p', 1, /FOLD_CASE) then j = 1
   if STRCMP(gc, 's', 1, /FOLD_CASE) then j = 2
   if STRCMP(gc, 'y', 1, /FOLD_CASE) then j = 2
   if STRCMP(gc, 'n', 1, /FOLD_CASE) then j = 0

;   k = where(bl[pbl].iaq ne 1)
;   if k[0] ne -1 then bl[pbl[k]].iaq = j
   list = '"source" eq "'+strtrim(sn,2)+'"'
   result=dat_list(s_l,list,/reset,/no_notify)
   m = where(sources eq strtrim(sn,2))
   bl[pbl].iaq = j
   flags[m] = bl[pbl[0]].iaq
   if n_elements(parts) ge 3 then begin
        fluxes[m] = float(parts[2])
        in[pil].sflux = float(parts[2])
   endif


 for j=0,ndistinct-1 do begin
   list = '"source" eq "'+sources[j]+'"'
   result=dat_list(s_l,list,/reset,/no_notify)
	word = 'NO'
	if bl[pbl[0]].iaq eq 1 then word = 'PRIMARY'
	if bl[pbl[0]].iaq eq 2 then word = 'SECONDARY'
    ; Print table
      if j eq 0 then print,"Current Table"
      if j eq 0 then print,'#   Source     Flux Cal.           Flux(Jy)'
         print,format='(A10,4x,A10,5x,g10.2)', sources[j],word,fluxes[j]
      endfor

goto,again

finish:

j = where(flags ne 0)
sources = sources[j]
fluxes = fluxes[j]
flags=flags[j]

      return,1
end





pro sma_flux_cal_new,channel=channel,day_range=day_range,minvis=minvis, $
mincoh=mincoh,_extra=extra_keywords
;yes
;=Task:SMA_FLUX_CAL --- To excute flux calibration (SMA wrapper)
;#Type: calib 
;+Use:
;      SMA_FLUX_CAL determines the scale factors for flux correction
;      and apply, if needed, the scalings to the visibility data.
;      Both primary and secondary type of calibrators can be used.
;      When using primary flux calibrators (planets, planetary
;      satellites), make sure the sizes of them (diameter in arcsecs)
;      are correct. Their expected fluxes are calculated by theoretical 
;      models. When using secondary flux calibrators, specify your
;      trusted fluxes by entering the source name and the flux
;      separated by a space. Scale factors are derived on each sideband
;      - sideband combination using only data satisfy a minimum
;      visibility and coherence requirement. Scale factors on
;      baseline-bases, sideband-bases, and an overall average will be
;      shown separately, which can be used for scale the data.
;@channel:   
;      continuum channel to be used for deriving scale factors.
;      'c1' is used by default
;@day_range: 
;      number of days to search back in database for (secondary) 
;      calibrator fluxes. Not implemented currently as SMA MIR/IDL
;      is not linked to a database. Fluxes for secondary calibrators
;      need to be specified.
;
;&history:
;--------------------------------------------------------------------
;       syliu 09mar04 compiling the header
;---------------------------------------------------------------------

; Calls: 
;	sma_flux_cal_ini_new.pro
;       flux_scale_new.pro which calls uti_gaussqs.pro

; These are nearly the same as the original OVRO programs
;	flux_cal_ini.pro
;	flux_cal.pro which calls uti_gaussq.pro


common global
common data_set
common plo


result =  sma_flux_cal_ini_new(sources=sources,fluxes=fluxes,flags=flags)

if keyword_set(channel) eq 0 then channel='c1'

  result=flux_scale_new(sources,channel,flux_inp=fluxes,flags=flags,coh_min=mincoh,vis_min=minvis,_extra=extra_keywords)

  if result eq -1 then begin
     print, '***** The flux calibration failed. '
     return
  endif



aa = ''
read,aa,prompt='Apply Flux Calibration? [NO <YES>]:  '
if STRCMP(aa, 'y', 1, /FOLD_CASE) then begin
   print,'YES: apply flux cal'

; The flux calibration needs to be applied to all the data.
; Reset the dat_list
   result=dat_list(s_l,/reset)
   result=cal_apply(gain='amp')

print,'Re-calculating continuum fluxes '
uti_avgband
print,'Calibrated fluxes:'
flux_measure_new

endif else begin
   print,'NO: nothing done'
endelse


end

