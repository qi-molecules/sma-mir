pro sma_cal_bas, subtract_delay=subtrack_delay, ntel=ntel,oldantennas=oldantennas, newantennas=newantennas, csoda=csoda
  ; Common blocks
    common global
    common data_set

  ; Select data in filter
    if dat_list(s_l,/reset,/no_notify) lt 1 then begin
       print,'*** No data in filter ***'
       return
    endif


;    if not keyword_set(filename) then filename='basefit.out'
    if not keyword_set(ntel) then ntel=8
    positions_old_i={tel:0,te:0.0D,tn:0.0D,tu:0.0D,dte:0.0D,dtn:0.0D,dtu:0.0D, $
          dta:0.0D,doff:0.0D,loff:0.0D}
    positions_old=replicate(positions_old_i,ntel)
    positions_old.tel=indgen(ntel)+1
    positions_new=positions_old
    
    filename='a'
  ; read in old antennas file
    oldbase=dblarr(4,ntel)
    if not keyword_set(oldantennas) then read,filename,prompt='Enter the current ANTENNAS file: ' else filename=oldantennas
    count=0 & result=findfile(filename,count=count)
    IF count eq 0 THEN BEGIN
       print,'Could not find file: ',filename
       print,'Failed. No baseline correction !'
       RETURN
    ENDIF
    openr,/get_lun, unit, filename
    readf,unit,oldbase
    free_lun,unit
    print,oldbase

  ; read in new antennas file
    newbase=dblarr(4,ntel)
    if not keyword_set(newantennas) then read,filename,prompt='Enter the new ANTENNAS file: ' else filename=newantennas
    count=0 & result=findfile(filename,count=count)
    IF count eq 0 THEN BEGIN
       print,'Could not find file: ',filename
       print,'Failed. No baseline correction !'
       RETURN
    ENDIF
    openr,/get_lun, unit, filename
    readf,unit,newbase
    free_lun, unit
    print,newbase

;    IF newbase(0,*)-oldbase(0,*) THEN BEGIN
;       print,'Inconsistent ANTENNAS file'
;       print,'Failed. No baseline correction !'
;       RETURN
;    ENDIF
    positions_new.tel=fix(reform(newbase[0,*]))
    newbase=newbase[1:3,*]
    oldbase=oldbase[1:3,*]
    xyz=(oldbase-newbase)*1000.d

;    print,xyz

    lat=19.82420526391d/57.29577951d
    m1=[[-sin(lat),0.d,cos(lat)],[0.d,1.d,0.d],[cos(lat),0.d,sin(lat)]]
    neu=xyz##m1
    neu=transpose(neu)
    positions_new.dte=neu(*,1)
    positions_new.dtn=neu(*,0)
    positions_new.dtu=neu(*,2)
    positions_new.dta=fltarr(ntel)

;    print,positions_new
    print, 'Starting the baseline correction now:'

  ; Set numerical constants used often in this function
    clat = cos(!TEL_LAT * !dtor)
    slat = sin(!TEL_LAT * !dtor)
    cvel = !CVEL / 1.e6  ; Divide by 1e6 for unit conversion

  ; Set up vectors that point to the channel and record complex visibility data
  ; (Both the continuum and spectral line data)
;    npts=sp[psl].nrec*sp[psl].nch+sp[psl+1].nrec*sp[psl+1].nch+sp[psl+2].nrec*sp[psl+2].nch+sp[psl+3].nrec*sp[psl+3].nch+sp[psl+4].nrec*sp[psl+4].nch
    npts=sp[psl].nrec*sp[psl].nch
    pcl_end=pcl+npts-1L

  ; Segment the data by baselines, records, and sidebands and
  ; determine the number of unique combinations (excluding soids)
    bls   = c.blcd[bl[pbl].iblcd]
    recs  = c.rec[bl[pbl].irec]
    sbs   = c.sb[bl[pbl].isb]
    bcals = bls+' '+recs+' '+sbs
;    bcals=bls+' '+sbs
    distinct_bcals = uti_distinct(bcals,nbcals,/many_repeat)

  ; Set ending records
    prl_end = prl + sp[psl].nrec - 1L

    for i = 0L, nbcals-1L do begin
  ; Determine vector locations for this combination of soid/bls/rec/sbs
       js = where(distinct_bcals[i] eq bcals,njs) 
  ; distinct_soids[n] eq bl(pbl).soid,njs)
       if (njs gt 0L) then begin
             ; Determine the location of the continuum scans
;          js_cont=where(abs(sp(psl(js)).fres) gt 100.,njs_cont)
          js_cont=where(sp[psl[js]].iband eq 0, njs_cont)
             ; Convert telescope number to vector location in the soid.
             ; I subtract 1 because telescope 1 is stored in vector element 0.
             ; ITEL1/ITEL2 are not vectors since only 1 baseline is selected
             ; at a time in distinct_bcals.
          itel1 = long(c.tel1(bl[pbl(js(0))].itel1)) 
          itel2 = long(c.tel2(bl[pbl(js(0))].itel2))
          ii=where(positions_new.tel eq itel1)
          jj=where(positions_new.tel eq itel2)

             ; Determine changes in the various baseline vectors
          de = (positions_new(jj).dte-positions_new(ii).dte) 
          dn = (positions_new(jj).dtn-positions_new(ii).dtn)
          du = (positions_new(jj).dtu-positions_new(ii).dtu)
          da = (positions_new(jj).dta-positions_new(ii).dta)
          if keyword_set(csoda) and itel2 eq 10 then da=-double(csoda)

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
;                    dha=dha-dha
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
;             wlchn = cvel / sp(psl(jsj)).fsky ; chunk based correction
             nch=sp[psl[jsj]].nch
             skyfreq=sp[psl[jsj]].fsky+0.001* $
               (findgen(nch)+(1.-float(nch))/2.)*sp[psl[jsj]].fres
             wlchn = cvel / skyfreq ; channel based correction
             if n_elements(dphwl) ne 1 then stop
             dph = dphwl[0] / wlchn
;             cphz = complex(cos(dph[n_elements(dph)-1L]),sin(dph[n_elements(dph)-1L])) ; chunk based 
             cphz = complex(cos(dph),sin(dph))
             if (sp[psl[jsj]].nch eq 1) then bl[pbl[jsj]].phaave=uti_pha_180(bl[pbl[jsj]].phaave+dph*!radeg)
             j1 = pcl[jsj]
             j2 = pcl_end[jsj] < (n_elements(ch)-1L)
             ch[j1:j2] = ch[j1:j2] * cphz

                  ; Correct phase average
;             if (njs_cont gt 0) then begin
;                k = where(j eq js_cont,nk)
;                if (nk gt 0) then begin
;                   bl(pbl(jsj)).phaave = uti_pha_180(bl(pbl[jsj]).phaave + $
;                     dph[n_elements(dph)-1L] * !radeg)
;                endif
;             endif

                  ; Change baseline solution id number
;                    bl(pbl[jsj]).soid = soid_new
          endfor
       endif
    endfor

    print, 'Baseline correction done !'
  ; Completed function, presumably successfully
    return
end
