; *************************************************************************
; FUNCTION
;      wlm_derive_ini
;
; WRITTEN
;      June 13, 2000 by JMC
;
; PURPOSE
;      Set source names, sideband, receiver to derive scale factor
;
; INPUTS
;      sources    : source names
;      isource    : source ids
;      amp3mm     : 3mm amplitudes
;      amp1mm     : 1mm amplitudes
;      nscans3mm  : Number of scans at 3mm
;      nscans1mm  : Number of scans at 1mm
;      sidebands  : array containing gains
;      receivers  : array containing errors in gains
;      ibright    : Vector indicating which sources are selected by default
;
; OUTPUT
;      1 if successful
;     -1 if not successful
;
; EXAMPLES
;       result = wlm_derive_ini(sources,isources,amp3mm,amp1mm,nscans3mm,$
;                               nscans1mm,isidebands,ireceivers,ibright)
;
; *************************************************************************
function wlm_derive_ini,sources,isources,amp3mm,amp1mm,nscans3mm,nscans1mm,$
                       isidebands,ireceivers,ibright
  ; Common blocks
    common global
    common data_set

  ; Reset filter
    if (dat_list(s_l,/reset,/no_notify) le 0) then return,-1

  ; Sidebands - the 1* makes sures the vector is an array
    l = where(c.sb(bl(pbl).isb) eq 'l', nl)
    u = where(c.sb(bl(pbl).isb) eq 'u', nu)
    if (nl eq 0 and nu eq 0) then return,-1
    isidebands = [1*(nl gt 0), (nu gt 0)]

  ; Receivers - the 1* makes sures the vector is an array
    distinct_rec = uti_distinct(bl(pbl).irec,nrec,/many)
    r3 = where(c.rec(bl(pbl).irec) eq '1', n3)
    r1 = where(c.rec(bl(pbl).irec) eq '2', n1)
    if (n3 eq 0 and n1 eq 0) then return,-1
    ireceivers = [1*(n3 gt 0), (n1 gt 0)]

  ; Get unique source names, excluding the noise source
    isources = uti_distinct(in(pil).isource)
    sources = c.source(isources)
    j = where(sources ne "noise",nsources)
    if (nsources eq 0) then return,-1
    isources = isources(j)
    sources  = sources(j)
    nsources = n_elements(sources)

  ; Initialize vectors
    iuse    = intarr(nsources)

  ; Find gain calibrators or sources that are bright at 3mm
    for i = 0L, nsources-1L do begin
       ; Find source in structure
         j = where(in(pil).isource eq isources[i],nj)
         if (nj gt 0) then begin
            ; Make list of amplitude and calibrator flags
              acq = bl(pbl[j]).iaq + bl(pbl[j]).icq

            ; See if this source is a calibrator
              ical = where(acq ne 0,ncal)
              if (ncal gt 0) then begin
                 iuse[i] = 1
              endif else begin
                 ; Compute average amplitude at 3mm
                   k = where(bl(pbl[j]).irec eq 0 and bl(pbl[j]).coh ge 0.75,nk)
                   if (nk gt 0) then begin
                      a3mm = total(bl(pbl(j[k])).ampave) / nk
                      if (a3mm ge 1.0) then iuse[i] = 1
                   endif
              endelse
         endif
    endfor

  ; See how many good sources there are
    igood = where(iuse gt 0,ngood)
    if (ngood eq 0) then return,-1
    sources = sources(igood)
    isources = isources(igood)

  ; Initialize fluxes
    fluxes = fltarr(2,ngood)
    nscans = intarr(2,ngood)

  ; For each good source, compute flux at 3mm, 1mm, and number of scans in each
    for i = 0L, ngood-1L do begin
       ; Loop over receivers
         for irec = 0, nrec-1 do begin
            ; Find id numbers for source/receiver
              j = where(in(pil).isource eq isources[i] and $
                        bl(pbl).irec eq distinct_rec[irec],nj)
              if (nj gt 0) then begin
                 ; Find number of scans
                   l = uti_distinct(in(pil(j)).int,nl,/many)
                   nscans[irec,i] = nl

                 ; Find average flux
                   k = where(bl(pbl(j)).coh ge 0.75,nk)
                   if (nk gt 0) then $
                     fluxes[irec,i] = total(bl(pbl(j(k))).ampave) / nk
              endif
         endfor
    endfor

  ; Set amplitudes
    amp3mm = fluxes[0,*]
    amp1mm = fluxes[1,*]

  ; Set nscans
    nscans3mm = nscans[0,*]
    nscans1mm = nscans[1,*]

  ; Must have at least one source
    j = where(amp3mm gt 2.0, nbright)
    if (nbright eq 0) then begin
       ibright = replicate(1,ngood)
    endif else begin
       ibright = intarr(ngood)
       ibright[j] = 1
    endelse

  ; Set default WLM filter command in case SCALE is automatically set in ION
;   result = wlm_sources_set(isources,isidebands,ireceivers)

  ; Done
    return,1
end
