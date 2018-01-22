pro uti_rfreq_update, bandlist=bandlist, rfreqlist = rfreqlist

common global
common data_set

  specdata = where(sp[psf].iband ne 0,nspecdata)
  if (nspecdata eq 0) then begin
    print,"No spectral data to be processed. Exit"
    return
  endif
  vabs = (1- (sp[psf[specdata]].fsky/sp[psf[specdata]].rfreq)^2)/(1+(sp[psf[specdata]].fsky/sp[psf[specdata]].rfreq)^2)*!cvel/1000.0
  vdoplist = uti_distinct((sp[psf[specdata]].vel-vabs),nvdop)
  if (nvdop eq 1) then begin
    print, "The observatory velocity in the headers is ",vdoplist
    spvdop = vdoplist[0]
  endif else begin
    print, "Multiple values of observatory velocity are found : ",vdoplist
    print, "The average value can be due to numertical erros and can be used, but please check the validity."
    ans = ''
    read,ans,prompt='Use the average? [NO <YES>]:  '
    if (ans eq 'YES' or ans eq 'yes' or ans eq 'Yes' or ans eq 'Y' or ans eq 'y') then begin
      spvdop = total(vdoplist)/nvdop
    endif else begin
      return
    endelse
  endelse

  rxlist = uti_distinct(bl[pbf].irec,nrx)
  sblist = uti_distinct(bl[pbf].isb,nsb)
  bdlist = uti_distinct(sp[psf].iband,nbd)
  totalnbd = nrx*nsb*nbd

  if (keyword_set(bandlist) and keyword_set(rfreqlist)) then begin
    if (n_elements(bandlist) ne n_elements(rfreqlist)) then begin
      print, "Inconsistent number of band and rest frequency items "
      return
    endif
    if (max(bandlist) ge totalnbd) then begin
      print, "Assigned band number exceed the actual number of bands"
      return
    endif
    bandflag = make_array(totalnbd,/int)
    newfreq = make_array(totalnbd,/double)
    for i=0, (n_elements(bandlist)-1) do begin
      bandflag[bandlist[i]]=1
      newfreq[bandlist[i]]=rfreqlist[i]
    endfor
  endif

  if ((not keyword_set(bandlist)) and keyword_set(rfreqlist)) then begin
    if (n_elements(rfreqlist) lt totalnbd) then begin
      print, "Insufficient items (",n_elements," instead of ",totalnbd, ") of rest frequencies provided."
      return
    endif
    bandflag = make_array(totalnbd,/int,value=1)
    newfreq = rfreqlist
  endif

  if ((not keyword_set(bandlist)) and (not keyword_set(rfreqlist))) then begin
    bandflag = make_array(totalnbd,/int)
    newfreq = make_array(totalnbd,/double)
    for irx = 0,(nrx-1) do begin
      for isb = 0, (nsb-1) do begin
        for ibd = 0,(nbd-1) do begin
          i = irx*nsb*nbd + isb*nbd + ibd
          idxlist = where(bl[pbf].irec eq rxlist[irx] and bl[pbf].isb eq sblist[isb] $
                          and sp[psf].iband eq bdlist[ibd])
          bdfreq = uti_distinct(sp[psf[idxlist]].fsky)

          tmprfreq = ''
          print,"Input rest frequency (in GHz) for band "+c.rec(rxlist[irx])+"  "+c.sb(sblist[isb])+"  "+c.band(bdlist[ibd]) $
                 +" with sky frequency centered at "+strtrim(string(bdfreq),2)+" GHz."
          read,tmprfreq
          if (tmprfreq gt 0) then begin
            bandflag[i] = 1
            newfreq[i]= double(tmprfreq)
          endif else begin
            bandflag[i] = 0
          endelse
        endfor
      endfor
    endfor
  endif

  for irx = 0,(nrx-1) do begin
    for isb = 0, (nsb-1) do begin
      for ibd = 0,(nbd-1) do begin
        i = irx*nsb*nbd + isb*nbd + ibd
        if (bandflag[i]) then begin
          print,"Reseting rest frequency and velocity for band "+c.rec(rxlist[irx])+"  "+c.sb(sblist[isb])+"  "+c.band(bdlist[ibd])
          idxlist = where(bl[pbf].irec eq rxlist[irx] and bl[pbf].isb eq sblist[isb] $
                          and sp[psf].iband eq bdlist[ibd])
          sp[psf[idxlist]].rfreq = newfreq[i]
          vabs = (1 - (sp[psf[idxlist]].fsky/sp[psf[idxlist]].rfreq)^2) $
                /(1 + (sp[psf[idxlist]].fsky/sp[psf[idxlist]].rfreq)^2) *!cvel/1000.0
          sp[psf[idxlist]].vel = vabs + spvdop
        endif
      endfor    
    endfor
  endfor

end
