; wlm_tave, dt_wlm is in hours
function wlm_regrid,wlm_diff,lambda,wlm_data,wlm_tave,dt_wlm,wlm_id,$
                    itel1=itel1,itel2=itel2,iscan=iscan,times=times,$
                    linear=linear,coherence=coherence,phases=phases,$
                    labels=labels,bsl=bsl,isource=isource,store_data=store_data

  ; Common blocks
    common global
    common data_set

  ; Booleans
    icoherence = keyword_set(coherence)
    ilinear    = keyword_set(linear)
    istore     = keyword_set(store_data)

  ; If storing data, then must be continuum only
    if (istore) then begin
       jbad = where(sp(psl).nch ne 1,nbad)
       if (nbad) then begin
          printf,-1,'Cannot use line data in deriving scale factor'
          return,-1
       endif
    endif

  ; Set total number of unique bands (npts), channels and records
    npts     = n_elements(pil)
    nch_tot  = TOTAL(sp(psl).nrec * sp(psl).nch)
    nrec_tot = TOTAL(sp(psl).nrec)

  ; Compute start/stop times for each astronomy record
    nr = 0L
    tastro_start = dblarr(nrec_tot)
    tastro_stop  = dblarr(nrec_tot)
    wts_astro    = dblarr(nrec_tot)
    recstart     = lonarr(npts)
    prl_end = prl + sp(psl).nrec - 1L
    for i = 0L, npts-1L do begin
        recstart[i] = nr
        nr2 = nr + sp(psl[i]).nrec - 1L
        j = prl[i] + lindgen(sp(psl[i]).nrec)
        t = in[pil[i]].dhrs + (re.toffs[j] - 0.5*re.integs[j])/3600.0
        tastro_start[nr:nr2] = t
        tastro_stop[nr:nr2]  = t + re.integs(j)/3600.0
        wts_astro[nr:nr2]    = re.wts(j)
        nr = nr + sp(psl[i]).nrec
    endfor
    tastro_mid  = 0.5*(tastro_start + tastro_stop)

  ; Allocate memory
    if (not istore) then begin
       wlm_diff   = make_array(n_elements(ch),/float,value=!BAD_VALUE)
       lambda     = fltarr(n_elements(ch))
    endif else begin
       nch = 0L
       wlm_diff   = make_array(nch_tot,/float,value=!BAD_VALUE)
       itel1      = intarr(nch_tot)
       itel2      = intarr(nch_tot)
       iscan      = intarr(nch_tot)
       isb        = intarr(nch_tot)
       irec       = intarr(nch_tot)
       isource    = intarr(nch_tot)
       times      = fltarr(nch_tot)
       phases     = fltarr(nch_tot)
       ave_time   = fltarr(npts)
       lambda = fltarr(nch_tot)
    endelse

  ; Set start/stop times for each WLM record (integration time = 2 seconds)
    wtimes_start = wlm_tave - (dt_wlm / 2.0)
    wtimes_stop  = wtimes_start + dt_wlm

  ; Compute wavelength in millimeters
    wavelength = (!CVEL / 1e6) / sp(psl).fsky

  ; Set WLM ID strings to all telescopes before hand
    temp = {data:dblarr(n_elements(wlm_tave))}
    s = replicate(temp,!NTEL)
    for i = 0L, !NTEL-1L do begin
       j = where(wlm_id eq i+1L,nj)
       if (nj gt 0) then s[i].data[0L:nj-1L] = wlm_data[j]
    endfor

  ; Loop over all frame combinations
    for i = 0L, npts-1L do begin
       ; Set telescope numbers and frame ID number
         i1 = long(c.tel1(bl[pbl(i)].itel1))
         i2 = long(c.tel2(bl[pbl(i)].itel2))

       ; Set indices to astronomy record data.
         nrec       = sp(psl[i]).nrec
         nstart_rec = recstart[i]
         nend_rec   = nstart_rec + nrec - 1L

       ; Find WLM data for this baseline
         r1 = s[i1-1L].data
         r2 = s[i2-1L].data

       ; Find valid WLM data for this baseline/time range
         jgood = where(wtimes_start lt tastro_stop(nend_rec)    and $
                       wtimes_stop  gt tastro_start(nstart_rec) and $
                       r1 gt !BAD_VALUE and r2 gt !BAD_VALUE,ngood)

       ; Find average WLM value over integration
         if (ngood gt 0L) then begin
           ; Interpolate WLM data onto the astronomy time grid. This
           ; is for record data. I expand wlm_k into channels later.
             r  = r1(jgood) - r2(jgood)
             t1 = wtimes_start(jgood)
             t2 = wtimes_stop(jgood)
             wlm_k = replicate(!BAD_VALUE,nrec)
             good_rec = where(wts_astro[nstart_rec:nend_rec] ne 0.0,ngood_rec)
             times_record = tastro_mid[nstart_rec:nend_rec]
             if (ngood_rec gt 0) then begin
              for k = 0L, ngood_rec-1L do begin
               l = nstart_rec + good_rec(k)
               wlm_k[good_rec(k)] = wlm_rec_ave(t1,t2,r,tastro_start[l],tastro_stop[l])
              endfor
             endif

           ; If coherence corrections, remove any DC offset in WLM temperature
             if (icoherence) then begin
                jgood = where(wlm_k gt !BAD_VALUE,ngood)
                if (ilinear and ngood gt 1) then begin
                   uti_fit_lin,jgood,wlm_k(jgood),a,siga,b,sigb
                   wlm_k(jgood) = wlm_k(jgood) - (b(0)*jgood + a(0))
                endif else if (ngood gt 0) then begin
                   wlm_k(jgood) = wlm_k(jgood) - total(wlm_k(jgood)) / ngood
                endif
             endif

           ; If no WLM data, set astronomy weights to zero
             if (not istore) then begin
                jbad = where(wlm_k eq !BAD_VALUE,nbad)
                if (nbad gt 0) then re.wts[prl[i]+jbad] = 0.0
             endif

           ; Expand from records to channels
             if (not istore ) then begin
                for k = 0L, sp(psl[i]).nrec-1L do begin
                    ptr = pcl[i] + k + sp[psl[i]].nrec*lindgen(sp[psl[i]].nch)
                    wlm_diff[ptr] = wlm_k[k]
                    lambda[ptr]   = wavelength[i]
                endfor
             endif else if (ngood_rec gt 0) then begin 
                ; Construct points to channel data
                  ptr = pcl[i] + lindgen(sp[psl[i]].nrec)

                ; Convert complex data to amp/phases
                  uti_conv_apc,ch[ptr],amp,pha,/amp_pha

                ; Store data
                  j1 = nch
                  j2 = nch + ngood_rec - 1L
                  nch = nch + ngood_rec
                  wlm_diff[j1:j2] = wlm_k(good_rec)
                  phases[j1:j2] = pha(good_rec)
                  times[j1:j2]  = times_record(good_rec)
                  itel1[j1:j2]  = i1
                  itel2[j1:j2]  = i2
                  iscan[j1:j2]  = in[pil[i]].int
                  lambda[j1:j2] = wavelength[i]
                  isource[j1:j2] = in[pil[i]].isource
                  isb[j1:j2] = bl[pbl[i]].isb
                  irec[j1:j2] = bl[pbl[i]].irec
             endif
         endif
    endfor

  ; If store data, then determine values which are OK and use only them
    if (istore) then begin
       jbad  = where(wlm_diff le !BAD_VALUE or  phases le !BAD_VALUE,nbad)
       jgood = where(wlm_diff gt !BAD_VALUE and phases gt !BAD_VALUE,ngood)
       if (ngood eq 0) then return,-1
       wlm_diff = wlm_diff(jgood)
       phases   = phases(jgood)
       lambda   = lambda(jgood)
       isource  = isource(jgood)
       itel1    = itel1(jgood)
       itel2    = itel2(jgood)
       iscan    = iscan(jgood)
       isb      = isb(jgood)
       irec     = irec(jgood)
       times    = times(jgood)
       sb_upper = strupcase(c.sb)
       labels = c.source(isource) + $
             strcompress(", Scan " + string(iscan)) + $
             ",BSL " + strcompress(string(itel1) + "-" + string(itel2),/remove) + $
                ", " + strcompress(sb_upper(isb) + "SB",/remove) + $
                ", " + strcompress("RX" + c.rec(irec),/remove) 
       bsl = string(iscan) + ":" + string(itel1) + "-" + string(itel2)
       bsl = strcompress(bsl,/remove)
    endif

  ; Done
    return,1
end
