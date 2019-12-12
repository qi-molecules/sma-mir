function flux_scale,sources,channel,flux_inp=flux_inp,$
         weight=weight,coh_min=coh_min,div_coh=div_coh,$
         iuse=iuse,source_out=source_out,vflux=vflux,jycts=jycts,$
         wts=wts,sb=sb,iscan=iscan,bsl=bsl,vis=vis,tssb=tssb,$
         coh=coh,el=el,jct_bsl=jct_bsl,distinct_bsl=distinct_bsl,$
         distinct_sb=distinct_sb,scale_bsl=scale_bsl,jct_sig=jct_sig,$
         noprint=noprint,good_frames=good_frames,polar=polar,casaflux=casaflux
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

    ; Must be some sources
      nsources = n_elements(sources)
      if (nsources eq 0) then return,-1

    ; Sources and flux_inp must be identical in size
;      if (nsources ne n_elements(flux_inp)) then begin
;         print,'SOURCES and FLUX_INP must have the same size'
;         return,-1
;      endif

    ; User ID
      user = '%'
      if keyword_set(user_id) then user = user_id

    ; Determine weighting for data
      iweight = 0
      if keyword_set(weight) then iweight = 1 * weight

    ; Minimum coherence
      mincoh = keyword_set(coh_min) ? 1.0 * coh_min : 90.0
      if (mincoh lt 1.0) then mincoh = mincoh * 100.0
      if (e.campuslogin eq 'sma' or e.campuslogin eq 'cfa') then bl.coh=1      

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
      bw     = sp[psl].fres
      datatime = mJD+in[pil].dhrs/24.
      flux   = fltarr(ndata)
      for i = 0L, n_elements(sources)-1L do begin
         j = where(source_out eq sources[i],nj)
         if (nj gt 0) then begin
           if (not keyword_set(flux_inp)) then begin
             if keyword_set(casaflux) then result = flux_casa(strlowcase(sources[i]),radius[j],freq[j],bw[j],datatime[j],xflux) else result = flux_primary(strlowcase(sources[i]),radius[j],freq[j],xflux)
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
      j = where(coh ge mincoh and ampave gt 0.0 and vis ge 0.4,nj)
      if (nj eq 0) then begin
         print,format='(%"Data in band %s does not meet the coherence critera")',channel
         return,-1
      endif
      iuse = intarr(ndata)
      iuse[j] = 1

    ; Print table
      if not keyword_set(noprint) then begin
      print,'# USE    SOURCE       VFLUX       J/ct      WT   SB INT BSL    VIS    TSSB     COH     EL'

      for i = 0L, ndata-1L do begin
         print,$
           format='(3x,I2,2x,A8,2x,2(F10.2,2x),F6.2,2x,A1,2x,I3,2x,A3,2x,F5.1,2x,I5,2x,F8.1,2x,F4.1)',$
           iuse[i],source_out[i],vflux[i],1./jycts[i],wts[i],sb[i],iscan[i],$
           bsl[i],vis[i]*100.0,fix(tssb[i]),coh[i],el[i]
      endfor

      print,''
      print,''
      print,'  Average jy/cts:'
      print,''
      print,'  all        sidebands           baselines'
      print,''
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
               if not keyword_set(noprint) then begin
                case n_print of
                1: begin
                 print,$
                 format='(2x,E8.2,3x,A1,4x,E8.2,3x,A1,2x,A3,6x,E8.2)',$
                  1./jct_all, distinct_sb[0],1./jct_sb[0],$
                  distinct_sb[i],distinct_bsl[k],1./jct_bsl[i,k]
                 end
                2: begin
                 print,$
                 format='(13x,A1,4x,E8.2,3x,A1,2x,A3,6x,E8.2)',$
                  distinct_sb[1],1./jct_sb[1],$
                  distinct_sb[i],distinct_bsl[k],1./jct_bsl[i,k]
                 end
                else: begin
                 print, $
                 format='(29x,A1,2x,A3,6x,E8.2)', $
                  distinct_sb[i], distinct_bsl[k],1./jct_bsl[i,k]
                endelse 
               endcase  
              endif  
            endif else begin
               jct_bsl[i,k]=jct_sb[i]
            endelse
         endfor   
      endfor

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
      if not keyword_set(scale_bsl) then begin
         print,'select which scale factors to scale data: 
         print,'1. sidebands; 2. baselines; other. single(all) '
         read,fluxcal_sel
      endif else $
         fluxcal_sel = 2

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
