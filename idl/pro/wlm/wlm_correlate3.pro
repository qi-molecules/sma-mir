function wlm_correlate3,wlm_diff,xinit,$
                       phases_before=phases_before,phases_after=phases_after,$
                       sigma_before=sigma_before,sigma_after=sigma_after,$
                       ave_time=ave_time,factor=factor,frames=frames,$
                       plot=plot,derive=derive,get_wlmdata=get_wlmdata

  ; Common blocks
    common global
    common data_set
    common wlm

  ; Set booleans
    iget_wlm = keyword_set(get_wlmdata)
    iderive  = keyword_set(derive)

  ; Only continuum data can be present for this function
    jbad = where(sp(psl).nch ne 1,nbad)
    if (nbad) then begin
       printf,-1,'Cannot use line data in deriving scale factor'
       return,-1
    endif

  ; Set initial scale factor
    xfactor = xinit

  ; Read the complex data and convert to phases [deg] and amplitudes
    dat_get_rows,cmp,amps,phases_raw,t1,wts,pt_first,pt_npts,"hours",0,$
                     /amp_pha,/list

  ; Set number of frames (npts) and records
    npts     = n_elements(pil)
    nrec_tot = TOTAL(sp(psl).nrec)

  ; Compute start/stop times for each astronomy record
    tstart  = dblarr(nrec_tot)
    tstop   = dblarr(nrec_tot)
    prl_end = prl + sp(psl).nrec - 1L
    pt_end  = pt_first + pt_npts - 1L
    pstart  = lonarr(npts)
    n = 0L
    for i = 0L, npts-1L do begin
        pstart[i] = n
        nrec = sp[psl(i)].nrec
        tstart[n:n+nrec-1L] = t1[pt_first[i] + lindgen(nrec)]
        tstop[n:n+nrec-1L]  = re.integs(prl[i]:prl_end[i])
        n = n + nrec
    endfor
    tstop  = tstart + tstop/3600.0D

  ; Allocate memory to store phases before and after WLM corrections
    phases_after  = make_array(nrec_tot,/float,value=!BAD_VALUE)
    phases_before = phases_after
    sigma_before  = fltarr(npts)
    sigma_after   = fltarr(npts)

  ; Set various arrays for the wlm data, if needed
    if (iget_wlm) then begin
      ; Allocate memory
        wlm_diff = replicate(!BAD_VALUE,nrec_tot)
        factor   = replicate(!BAD_VALUE,npts)
        ave_time = fltarr(npts)
        frames   = strarr(nrec_tot)

      ; Set start/stop times for each WLM record (integration time = 2 seconds)
        dt = 2.0 / 3600.0
        wtimes_start = wlm_times - (dt / 2.0)
        wtimes_stop  = wtimes_start + dt
    endif

  ; Compute wavelength in millimeters
    lambda = (!CVEL / 1e6) / sp(psl).fsky

  ; Loop over all frame combinations
    for i = 0L, npts-1L do begin
       ; Set telescope numbers and frame ID number
         itel1 = long(c.tel1(bl[pbl(i)].itel1))
         itel2 = long(c.tel2(bl[pbl(i)].itel2))
         k1 = where(itel1 eq wlm_itel)
         k2 = where(itel2 eq wlm_itel)

       ; Bin WLM data to astronomy record time stamps
         if (k1[0] ne -1 and k2[0] ne -1) then begin
            ; Set indices to astronomy record data.
              nrec       = sp(psl[i]).nrec
              nstart_rec = pstart[i]
              nend_rec   = nstart_rec + nrec - 1L
              jrec       = nstart_rec + lindgen(nrec)
              ave_trec   = 0.5 * (tstart[jrec] + tstop[jrec])

            ; Get the WLM data for this baseline/integration/sideband
              if (not iget_wlm) then $
                wlm_k = wlm_diff[jrec] $
              else begin
                ; Find WLM data for this baseline
                  j1 = where(wlm_id eq itel1)
                  j2 = where(wlm_id eq itel2)

                ; Set frames
                  bsl = strcompress(string(itel1) +"-" +string(itel2),/remove)
                  frames[jrec] = strcompress("BSL " + bsl)

                ; Store WLM data for this integration
                  k = where(wtimes_start le tstop(nend_rec)    and $
                            wtimes_stop  ge tstart(nstart_rec) and $
                            wlm_cal(j1)  gt !BAD_VALUE         and $
                            wlm_cal(j2)  gt !BAD_VALUE,nk)
                  if (nk eq 0) then begin
                     wlm_k = !BAD_VALUE
                  endif else begin
                     ; Store subset of data
                       tstart_w = wtimes_start(k)
                       tstop_w  = wtimes_stop(k)
                       wdata    = wlm_cal(j1(k)) - wlm_cal(j2(k))

                     ; Interpolate WLM data onto the astronomy time grid.
                       wlm_k = fltarr(nrec)
                       for k = nstart_rec, nend_rec do begin
                         wlm_k[k-nstart_rec] = wlm_rec_ave(tstart_w,tstop_w,$
                                                       wdata,tstart(k),tstop(k))
                       endfor
                  endelse
              endelse

            ; Find where both the WLM and astronomy data are valid.
              good = where(wlm_k gt !BAD_VALUE and $
                           phases_raw(jrec) gt !BAD_VALUE,ngood)
              if (ngood gt 2) then begin
                 jgood = jrec(good)
                 wlm_k = wlm_k(good)
                 time  = ave_trec(good)
                 phases_deg = phases_raw(jgood)
              endif

            ; Store WLM data 
              if (iget_wlm and ngood gt 2) then begin
                 ; Remove offset from WLM data
                   if (keyword_set(linear)) then begin
                      uti_fit_lin,time,wlm_k,a,siga,b,sigb
                      wlm_k = wlm_k - (b(0)*time + a(0))
                   endif else begin
                      wlm_k = wlm_k - total(wlm_k) / n_elements(wlm_k)
                   endelse

                 ; Store data
                   wlm_diff[jgood] = wlm_k
              endif

            ; Derive conversion factor (if necessary)
              if (iderive and ngood gt 2) then begin
                ; Find best conversion factor
                  ave_time(i) = total(time) / n_elements(time)
                  result = wlm_xfit3(wlm_k,phases_deg,lambda[i],time,$
                                     xfactor,sig_xfactor,/derive)
                  factor[i] = xfactor
              endif

            ; Compute phase and residuals for best fit. Plot if needed.
              if (ngood gt 2) then begin
                 sb     = c.sb[bl[pbl(i)].isb]
                 band   = c.band[sp[psl(i)].iband]
                 bsl    = c.blcd[bl[pbl(i)].iblcd]
                 rec    = c.rec[bl[pbl[i]].irec]
                 iscan  = in[pil(i)].int
                 source = c.source[in(pil[i]).isource]
                 track  = in(pil[i]).traid
                 result = wlm_xfit3(wlm_k,phases_deg,lambda[i],time,xfactor,$
                          pderived=pderived,$
                          pbefore=pbefore,sb=sb,band=band,bsl=bsl,$
                          rms_before=rms_before,rms_after=rms_after,$
                          plot=plot,track=track,rec=rec,source=source,$ 
                          iscan=iscan)
                 phases_before(jgood) = pbefore
                 phases_after(jgood)  = pderived
                 sigma_before[i]      = rms_before
                 sigma_after[i]       = rms_after
              endif
         endif
    endfor

  ; Done
    return,1
end
