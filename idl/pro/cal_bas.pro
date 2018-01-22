; *************************************************************************
; FUNCTION
;      CAL_BAS
;
; WRITTEN 
;      February 13, 1998 by JMC
; UPDATED
;      May 10, 2002 by Qi, fix a little bug of phase correction in CH. 
;
; PURPOSE
;      Apply a new baseline solution (soid) to data
;
; INPUTS 
;      soid_new       : The ID number for the new baseline solution to apply
;      subtract_delay : If present, apply delay offsets to data.
;                       Otherwise, no delay offsets are applied
;
; OUTPUT
;     -1  : if data are not successfully corrected
;      1  : if data are successfully corrected
;
; EXAMPLES
;      result = cal_bas(743)
;      result = cal_bas(743,subtract_delay=1)
; *************************************************************************
function cal_bas,soid_new,subtract_delay
  ; Common blocks
    common global
    common data_set

  ; Select data in filter
    if dat_list(s_l,/reset,/no_notify) lt 1 then begin
       print,'*** No data in filter ***'
       return,-1
    endif

  ; Read the new baseline solution that will be applied to the data
    if dbi_soid_read(soid_new,positions_new) ne 1 then return,-1

  ; Set numerical constants used often in this function
    clat = cos(!TEL_LAT * !dtor)
    slat = sin(!TEL_LAT * !dtor)
    cvel = !CVEL / 1.e6  ; Divide by 1e6 for unit conversion

  ; Set up vectors that point to the channel and record complex visibility data
  ; (Both the continuum and spectral line data)
    npts=sp[psl].nrec*sp[psl].nch
    pcl_end=pcl+npts-1L

  ; Determine the number of unique soids in the track
    distinct_soids = uti_distinct(bl(pbl).soid,num_osoid,/many_repeat)
    j = where(distinct_soids eq soid_new,count)
;   if (count ne 0) then begin
;      com = 'SOID ' + string(soid_new) + ' has already been applied'
;      if (count gt 1) then com = com + ' to some of the data'
;      printf,-1,strcompress(com)
;      if (count eq 1) then return,-1
;   endif

  ; Segment the data by baselines, records, and sidebands and
  ; determine the number of unique combinations (excluding soids)
    bls   = c.blcd[bl[pbl].iblcd]
    recs  = c.rec[bl[pbl].irec]
    sbs   = c.sb[bl[pbl].isb]
    bcals = bls+' '+recs+' '+sbs
    distinct_bcals = uti_distinct(bcals,nbcals,/many_repeat)

  ; Set ending records
    prl_end = prl + sp[psl].nrec - 1L

  ; Loop through the unique combinations and apply new baseline solution.
  ; The soid is contained in a separate loop so that the baseline solutions 
  ; are read only once per SOID.
    for n = 0L, num_osoid-1L  do begin
      if dbi_soid_read(distinct_soids[n],positions_old) ne 1 then return,-1
      for i = 0L, nbcals-1L do begin
         ; Determine vector locations for this combination of soid/bls/rec/sbs
           js = where(distinct_bcals[i] eq bcals and $
                      distinct_soids[n] eq bl(pbl).soid,njs)
           if (njs gt 0L) then begin
             ; Determine the location of the continuum scans
               js_cont=where(sp(psl(js)).nch eq 1,njs_cont)

             ; Convert telescope number to vector location in the soid.
             ; I subtract 1 because telescope 1 is stored in vector element 0.
             ; ITEL1/ITEL2 are not vectors since only 1 baseline is selected
             ; at a time in distinct_bcals.
               itel1 = long(c.tel1(bl[pbl(js(0))].itel1)) - 1
               itel2 = long(c.tel2(bl[pbl(js(0))].itel2)) - 1

             ; Determine changes in the various baseline vectors
               de = (positions_new(itel2).dte-positions_new(itel1).dte) - $
                    (positions_old(itel2).dte-positions_old(itel1).dte)
               dn = (positions_new(itel2).dtn-positions_new(itel1).dtn) - $
                    (positions_old(itel2).dtn-positions_old(itel1).dtn)
               du = (positions_new(itel2).dtu-positions_new(itel1).dtu) - $
                    (positions_old(itel2).dtu-positions_old(itel1).dtu)
               da = (positions_new(itel2).dta-positions_new(itel1).dta) - $
                    (positions_old(itel2).dta-positions_old(itel1).dta)

             ; Change the delay errors, if necessary
               dsb = 0.0D
               dlo = 0.0D
               if keyword_set(subtract_delay) then begin
                  ; Set LO frequency
                    flo = sp(psl(js)).fsky+!IF_FREQ
                    ju = where(sbs(js) eq 'u',count)
                    if (count gt 0) then flo(ju) = flo(ju) - 2.0*!IF_FREQ

                  ; Compute offsets
                    dsb = -!IF_FREQ * !TWOPI * cvel/sp(psl(js)).fsky * $
                         ((positions_new(itel2).doff - positions_new(itel1).doff) - $
                         (positions_old(itel2).doff - positions_old(itel1).doff))
                    dlo = !dtor * cvel/sp(psl(js)).fsky * $
                           ((positions_new(itel2).loff - positions_new(itel1).loff) - $
                           (positions_old(itel2).loff - positions_old(itel1).loff))
                    if (count gt 0) then dsb(ju) = -dsb(ju)
               endif

             ; Loop over all combinations
               for j = 0L, njs-1L do begin
                  ; Set id number to js(j)
                    jsj = js(j)

                  ; Determine time difference between start and median time scan
                  ; and calculate change in pointing
                    toffs = re.toffs[prl[jsj]:prl_end[jsj]]
                    nrec = n_elements(toffs)
                    if (nrec eq 1) then toffs = [toffs]
                    tshift = shift(toffs,-1)
                    dtsec = toffs + 0.5*(tshift - toffs)
                    if (nrec gt 1) then $
                       dtsec[nrec-1L] = toffs[nrec-1L] + dtsec[nrec-2l] - toffs[nrec-2L]
                    dha = dtsec / 3600.0 * !TWOPI / 24.0
                    sx  = in(pil(jsj)).sx*cos(dha) + in(pil(jsj)).sy*sin(dha)
                    sy  = in(pil(jsj)).sy*cos(dha) - in(pil(jsj)).sx*sin(dha)
                    sz  = in(pil(jsj)).sz

                  ; Compute partial derivatives of phase with respect to changes in 
                  ; baseline solution
                  ; Phase  = (2. * pi * freq / c) * (bx * sx + by * sy + bz * xz) rad
                  ;        = 360.0 * (bx * sx + by * sy + bz * sz) / wl deg
                  ;        = 360.0 * (be * sy + bn * (sz*clat-sx*slat) + 
                  ;                   bu * (sz*slat+sx*clat))  / wl          degrees
                  ;        = 360.0 * (be * se + bn * sn + bu * su) / wl      degrees
                    d1 = -!TWOPI * sy
                    d2 = -!TWOPI * (sz*clat-sx*slat)
                    d3 = -!TWOPI * (sz*slat+sx*clat)
                    d4 = -!TWOPI * cos(in(pil(jsj)).el*!dtor)

                  ; Phase change in wavelengths
                    dphwl = d1*de + d2*dn + d3*du + d4*da + dsb + dlo

                  ; Correct narrow band data or wideband records
                    wlchn = cvel / sp(psl(jsj)).fsky
                    dph = (dphwl / wlchn) mod !TWOPI
                    k = where(dph lt -!DPI,nk)
                    if (nk gt 0) then dph(k) = dph(k) + !TWOPI
                    k = where(dph gt !DPI,nk)
                    if (nk gt 0) then dph(k) = dph(k) - !TWOPI
;                    cphz = complex(cos(dph),sin(dph))
                    cphz = complex(cos(dph[n_elements(dph)-1L]),sin(dph[n_elements(dph)-1L]))
                    j1 = pcl[jsj]
                    j2 = pcl_end[jsj]
                    ch[j1:j2] = ch[j1:j2] * cphz

                  ; Correct phase average
                    if (njs_cont gt 0) then begin
                       k = where(j eq js_cont,nk)
                       if (nk gt 0) then begin
                          bl(pbl(jsj)).phaave = uti_pha_180(bl(pbl[jsj]).phaave + $
                                         dph[n_elements(dph)-1L] * !radeg)
                       endif
                    endif

                  ; Change baseline solution id number
                    bl(pbl[jsj]).soid = soid_new
               endfor
           endif
      endfor
    endfor

  ; Completed function, presumably successfully
    return,1
end
