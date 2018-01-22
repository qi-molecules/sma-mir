function flux_sf_derive,sources,flux_inp,channel,$
         weight=weight,coh_min=coh_min,div_coh=div_coh,$
         iuse=iuse,source_out=source_out,vflux=vflux,jycts=jycts,$
         wts=wts,sb=sb,iscan=iscan,bsl=bsl,vis=vis,tssb=tssb,$
         coh=coh,el=el,noprint=noprint,$
         jct_bsl=jct_bsl,jct_sig=jct_sig,jct_sb=jct_sb,jct_all=jct_all,$
         good_frames=good_frames,distinct_bsl=distinct_bsl,distinct_sb=distinct_sb
;
; Flux calibration (contact Charlie Qi cqi@cfa.harvard.edu for bugs)
; 
; A table will be made to print the average scale factor, the 
;   average scale factors for LSB and USB, and the average scale 
;   factors for each baseline. The user can select which type of
;   scaling they want to do.
;   for example: result=flux_cal(['neptune'],4.12,'c1')
;
; To apply the scale factors, call cal_apply: 
;                result=cal_apply(gain='amp')
;
; The user can set their own weights for flux calibration.

   ; Command blocks
     common global
     common data_set

   ; Get the number of sources
     nsources = n_elements(sources)
     if (nsources eq 0) then return,-1

   ; Sources and flux_inp must be identical in size
     if (nsources ne n_elements(flux_inp)) then begin
        print,'SOURCES and FLUX_INP must have the same size'
        return,-1
     endif

   ; Error message if flux is zero
     j = where(flux_inp le 0.0,nj)
     if (nj gt 0) then begin
       print,'Error reading flux for ',sources[j]
       return,-1
     endif

   ; Minimum coherence. Set between 0 and 100. I assume that any value less
   ; 1.0 is a decimal fraction instead of a percent and multiply by 100.
     mincoh = keyword_set(coh_min) ? 1.0 * coh_min : 90.0
     if (mincoh lt 1.0) then mincoh = mincoh * 100.0

   ; Construct select statement to extract data
       ; Continuum only data data
         command = '"band" eq "' + channel + '"and "wt" gt "0.0"'

       ; Make list of source names
         s = '"'
         if (nsources gt 1) then s = '["'
         for i=0,nsources-1 do $
             s = s + sources[i] + '","'
         s = strmid(s,0,strlen(s)-2)
         if (nsources gt 1) then s = s + ']'

       ; Add sources to the command
         command = command + ' and "source" '
         if (nsources gt 1) then command = command + ' in '
         if (nsources eq 1) then command = command + ' eq '
         command = command + s

   ; Submit command and make sure some data was read in
     result = dat_list(s_l,command,/reset,/no_notify)
     if (result eq 0) then return,-1

   ; Set number of data points
     ndata = n_elements(pil)

   ; Store MMA parameters that need to be passed to ION
     source_out = c.source[in[pil].isource]
     iscan = in[pil].int
     tssb = sp[psl].tssb
     bsl  = c.blcd[bl[pbl].iblcd]
     sb   = c.sb[bl[pbl].isb]
     coh  = bl[pbl].coh * 100.0
     el   = in[pil].el

   ; Set weights
     wts = keyword_set(weight) ? sp[psl].wts > 0.0 : replicate(1.0,ndata)

   ; Normalize amplitudes and weights by coherence, if necesary
     xfactor = 1.0
     if keyword_set(div_coh) then begin
        xfactor = fltarr(ndata)
        j = where(coh gt 0.0,nj)
        if (nj gt 0) then xfactor = 100.0 / coh
     end
     ampave = bl[pbl].ampave * xfactor
     wts    = wts * xfactor

   ; Compute UV distances
     uvdis = sqrt(bl[pbl].u^2 + bl[pbl].v^2)

   ; Store flux for individual baselines/sources
     flux = fltarr(ndata)
     for i = 0L, n_elements(sources)-1L do begin
        j = where(source_out eq sources[i],nj)
        flux[j] = flux_inp[i]
     endfor

   ; Make correction for finite resolution if source is resolved
     radius = in[pil].size / 2.0
     freq   = sp[psl].fsky
     result = uti_gaussq(uvdis,radius,freq,vis)

   ; Correcting expected fluxes for finite source visibilties
     vflux = abs(vis) * flux

   ; Compute Jansky per counts for each data point
     jycts = fltarr(ndata)
     iuse = intarr(ndata)
     j = where(ampave gt 0.0 and vis gt 0.0,nj)
     if (nj gt 0) then jycts[j] = vflux[j] / ampave[j]

   ; Set which baselines to use
     j = where(coh ge mincoh and ampave gt 0.0 and vis gt 0.0,nj)
     if (nj eq 0) then begin
        print,format='(%"Data in band %s does not meet the coherence critera")',channel
        return,-1
     endif
     iuse[j] = 1

   ; Print table
     if not keyword_set(noprint) then begin
        print,'# USE    SOURCE  VFLUX   J/ct     WT  SB  INT  BSL    VIS   TSSB   COH    EL'

        for i = 0L, ndata-1L do begin
           print,$
             format='(3x,I2,2x,A8,2x,2(F5.2,2x),F6.2,2x,A1,2x,I3,2x,A3,2x,F5.1,2x,I5,2x,F5.1,2x,F4.1)',$
             iuse[i],source_out[i],vflux[i],jycts[i],wts[i],sb[i],iscan[i],$
             bsl[i],vis[i]*100.0,fix(tssb[i]),coh[i],el[i]
        endfor

        print,''
        print,''
        print,'  Average jy/cts:'
        print,''
        print,'  all    sidebands      baselines'
        print,''
     endif

   ; Compute scale factors for baselines, sidebands, all data
     j=where(iuse eq 1)
     good_bsl = bsl(j)
     distinct_bsl=uti_distinct(good_bsl,ngood_bsl,/many_repeat)
     good_sb = sb(j)
     distinct_sb=uti_distinct(good_sb,ngood_sb,/many_repeat)
     good_frames = uti_distinct(channel + ' ' + good_bsl + ' ' + good_sb,/many_repeat)
     jct_bsl=make_array(ngood_sb,ngood_bsl,/float,value=1.)
     jct_sig=make_array(ngood_sb,ngood_bsl,/float)
     jct_sb=make_array(ngood_sb,/float,value=1.)
     jct_all=total([(jycts[j]*wts[j])])/total([wts[j]])

     for i = 0L, ngood_sb-1L do begin
        jsb=j[where(sb[j] eq distinct_sb[i])]
        jbls=bsl[jsb]
        jct_sb[i]=total([(jycts[jsb]*wts[jsb])])/total([wts[jsb]])
        n_print=-1
        for k = 0L, ngood_bsl-1L do begin
           n_jbl=-1
           j_bl=where(jbls eq distinct_bsl[k],n_jbl)
           if n_jbl eq 0 then begin
              jct_bsl[i,k]=jct_sb[i]
           endif else begin 
              jbl=jsb[j_bl]
              result = uti_meanvar(jycts[jbl],weights=wts[jbl])
              jct_bsl[i,k] = result[0]
              jct_sig[i,k] = result[2]
              n_print=n_print+i+2
              if not keyword_set(noprint) then begin
               case n_print of
               1: begin
                print,$
                format='(2x,F4.2,3x,A1,4x,F4.2,3x,A1,2x,A3,6x,F4.2)',$
                 jct_all, distinct_sb[0],jct_sb[0],$
                 distinct_sb[i],distinct_bsl[k],jct_bsl[i,k]
                end
               2: begin
                print,$
                format='(9x,A1,4x,F4.2,3x,A1,2x,A3,6x,F4.2)',$
                 distinct_sb[1],jct_sb[1],$
                 distinct_sb[i],distinct_bsl[k],jct_bsl[i,k]
                end
               else: begin
                print, $
                format='(21x,A1,2x,A3,6x,F4.2)', $
                 distinct_sb[i], distinct_bsl[k],jct_bsl[i,k]
               endelse 
               endcase    
              endif
           endelse
        endfor   
     endfor
end
