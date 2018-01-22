function sma_flux_measure,channel,dayrange=dayrange,sideband=sideband,$
            names=names,flags=flags,amp=amp,flux=flux,snr=snr,nscans=nscans,$
            icalib=icalib,sybase=sybase, defaults=defaults
;yes
;=Task:SMA_FLUX_MEASURE --- To measure the flux of sources after flux calibration
;#Type: utility
;+Use:
;      One usually need to measure the fluxes of sources after flux
;      calibration by SMA_FLUX_CAL in order to get the source flux
;      which is needed when one use GAIN_CAL to calibrate gain variation.
;      You can shoose whether to do a scalar or vector average. Before 
;      choosing vector average, you should do phase calibration first to 
;      flatten the phase.
;@channel:
;      Bands used to calculate the fluxes of sources. Currently only 
;      continuum band is available.
;      The default is 'c1'
;@sideband:
;      Sibands used to measure the fluxes of sources. 
;      'u' --- upperside band
;      'l' --- lowerside band
;      Default is both
;&history:
;------------------------------------------------------------------------
;      cykuo 10feb04 adapting the header
;------------------------------------------------------------------------

    ; Command blocks
      common global
      common data_set

    ; Initialize 
      flux = 0.0
      amp  = 0.0
      source = ' '
      flags  = ' '
      if (keyword_set(sideband)) then begin
        result=dat_list(s_l,'"wt" gt "0" and "sb" eq "'+sideband+'"',/reset)
      endif else begin
        result=dat_list(s_l,'"wt" gt "0"',/reset)
      endelse

    ; Name must be set
      if (not keyword_set(channel)) then begin
         print,'Channel (c1/c2) must be specified'
         return,-1
      endif

      if (not keyword_set(sybase)) then sybase=0

    ; Set day range
      dt = 30
      if keyword_set(day_range) then dt = fix(abs(day_range))
      dt = abs(dt)

    ; Set user id
      user = '%'
      if (keyword_set(user_id)) then user = user_id

    ; Find unique source names/calibrator/gain flag combinations in track
      calib = string(in[pil].isource) + ' ' + $
              string(sp[psl].igq)
      distinct_calib = uti_distinct(calib,many=0,ncalib)
      if (ncalib eq 0) then return,-1

    ; Parse distinct calibrators into the three variables
      s = replicate({isource:0L, igq:0},ncalib)
      for i = 0L, ncalib-1L do begin
         reads,distinct_calib,s
      endfor

    ; Use SQL command to set date range
;      ut_start = ' '
;      ut_stop  = ' '
;      command = 'select dateadd(dd,'+string(-dt)+',"' + c.ut[0] + '"),'
;      command = command + 'dateadd(dd,'+string(dt)+',"' + c.ut[n_elements(c.ut)-1L] + '")'
;      result = dbi_sql_submit(command)
;      reads,format='(2(8x,A19))',result[2],ut_start,ut_stop

    ; Make list of distinct source IDs in filter
      distinct_isource = uti_distinct(s.isource,many=0,nsources)
      sources  = c.source[distinct_isource]

    ; Determine average frequency of the observations. For primary calibrators,
    ; the frequency dependence is done rigorously in flux_cal.pro. For
    ; secondary calibrators, I assume alpha=0 and don't distinguish between 
    ; sidebands
      freq = total(sp[psl].fsky) / n_elements(psl)

    ; Allocate memory to store amplitudes, fluxes, flags, and sources
      flux   = fltarr(ncalib)
      amp    = fltarr(ncalib)
      noi_amp= fltarr(ncalib)
      nscans = intarr(ncalib)
      rr     = fltarr(ncalib)
      noi_rr = fltarr(ncalib)
      img    = fltarr(ncalib)
      noi_img= fltarr(ncalib)
      vecamp = fltarr(ncalib)
      noi_vecamp=fltarr(ncalib)
      snr    = intarr(ncalib)
      snr_amp= intarr(ncalib)
      snr_vecamp=intarr(ncalib)
      mtime  = fltarr(ncalib)
      
    ; Loop over sources
      for i = 0L, nsources-1L do begin
         ; Get source radius in arcseconds
           j = where(in[pil].isource eq distinct_isource[i],nj)
           radius = 0.5 * total(in[pil[j]].size) / nj

         ; First, see if sources is in the primary list
           result = flux_primary(sources[i],radius,freq,xflux)

         ; If not in primary list, get the secondary flux
;           if (xflux[0] eq 0.0) then $
;              result = flux_secondary(sources[i],user,radius,freq,$
;                                      xflux,ut_start,ut_stop)

         ; Store flux in table
           j = where(s.isource eq distinct_isource[i])
           flux[j]  = xflux
      endfor

    ; OK - I now have fluxes for each source. I want to distinguish 
    ; primary and secondary calibrators, as some sources may appear twice
    ; but have different flags (usually by mistake). So sort the list of
    ; source names by primary calibrators, then secondary, and then other.
    ; Like source names appear together even if they have different flags.

    ; First, initialize 
;      iaq    = ['1', '2', ' ']
      iused  = intarr(ncalib)
      icalib = intarr(ncalib)
      ss     = s[0]
      ssflux = 0.0

    ; Now loop over calibrators
    ; Store source names
    ; Find unique names in calibrator list
      j = where(iused eq 0, nj)
      if (nj gt 0) then begin
         ids = uti_distinct(s[j].isource,nids)
    ; Loop over source names
         for k = 0, nids-1 do begin
          ; Find all sources with this name
            l = where(s[j].isource eq ids[k],nl)
                                ; Store sources
            if (nl gt 0) then begin
               iused[j[l]] = 1
               ss = [ss,s[j[l]]]
               ssflux = [ssflux,flux[j[l]]]
            endif
         endfor
      endif

    ; Remove dummy element from sorted source list
      s = ss[1:n_elements(ss)-1]
      flux = ssflux[1:n_elements(ssflux)-1]

    ; Set flags, source names, and primary/secondary calibrator flag
      names = c.source(s.isource)
      flags = c.gq[s.igq]

    ; Finally, loop over source names/flags and determine average amplitudes
    ; and number of available scans
      for i = 0L, ncalib-1L do begin
         ; Find sources with these flag settings
           j = where(in[pil].isource eq s[i].isource and $
                     sp[psl].igq eq s[i].igq and sp[psl].iband eq 0)

         ; Determine number of scans
           k = uti_distinct(in(pil(j)).int,nk)
           nscans[i] = nk
           mtime[i] = mean(in[pil[j]].dhrs)

         ; Determine average amplitude
           amp[i] = 0.0
           rr[i] =0.0
           img[i]=0.0
           vecamp[i]=0.0
           k =where(bl(pbl(j)).ampave gt 0.0,nk)
           nkc=n_elements(pcl[j[k]])
           if (nk gt 0) then begin
              amp[i] = total(bl(pbl(j(k))).ampave) / nk
;amp[i]=total(bl(pbl[j]).ampave)/n_elements(pbl[j])
;              noi_amp[i] = total(abs(bl[pbl[j[k]]].ampave - amp[i])) / nk
;              snr_amp[i] = fix(amp[i]/(total(abs(bl[pbl[j[k]]].ampave - amp[i])) / nk))
noi_amp[i]=sqrt( total( (bl[pbl[j[k]]].ampave-amp[i])^2 )/(nk*(nk-1))  )
snr_amp[i]=fix(amp[i]/noi_amp[i])
rr[i]=total(bl[pbl[j[k]]].ampave*cos(bl[pbl[j[k]]].phaave*!pi/180.))/nk
img[i]=total(bl[pbl[j[k]]].ampave*sin(bl[pbl[j[k]]].phaave*!pi/180.))/nk
noi_rr[i]=sqrt( total( (bl[pbl[j[k]]].ampave*cos(bl[pbl[j[k]]].phaave*!pi/180.)-rr[i])^2 )/ (nk*(nk-1))  )
noi_img[i]=sqrt( total( (bl[pbl[j[k]]].ampave*sin(bl[pbl[j[k]]].phaave*!pi/180.)-img[i])^2 )/ (nk*(nk-1))  )
              vecamp[i]=sqrt(rr[i]*rr[i]+img[i]*img[i])
;              noi_vecamp[i]=abs((rr[i]*noi_rr[i]+img[i]*noi_img[i])/vecamp[i])
noi_vecamp[i]=sqrt(  (rr[i]*noi_rr[i])^2 + (img[i]*noi_img[i])^2  )/vecamp[i]
;print,vecamp[i],noi_vecamp[i]
              snr_vecamp[i]=fix(vecamp[i]/noi_vecamp[i])
;print,snr_vecamp[i]
           endif           
      endfor

if not keyword_set(defaults) then begin

aa=''
read,aa,prompt='Scalar Average or Vector Average ? [S <V>]: '
if (aa eq 'V' or aa eq 'v') then begin
;  read,aa,prompt='Phase calibration applied ? [N <Y>]: '
;  if (aa eq 'YES' or aa eq 'yes' or aa eq 'Yes' or aa eq 'Y' or aa eq 'y') then begin
   jj=where(strpos(flags,'g') ge 0, njj)
  if(njj gt 0) then begin
print,'Vector average: '
     flux=vecamp
     snr=snr_vecamp
  endif else begin
    print,'Do phase calibration first. Exit.'
    return,-1
  endelse
endif else begin
print,'Scalar average: '
   flux=amp
   snr=snr_amp
endelse
    ; Print table
      print,'#   Source   Flags   Nscans  Flux(Jy)   SNR    meantime    REAL       IMAG'
      for i = 0L, ncalib-1L do begin
         if((amp[i] ne 0) and (names[i] ne 'mars') and $
             (names[i] ne 'saturn') and (names[i] ne 'jupiter') $
            and (names[i] ne 'neptune') and (names[i] ne 'uranus')) $
            then begin
;      print,'#   Source   Flags   Nscans  Flux(Jy)    SNR'
         print,format='(A10,4x,A3,5x,I3,2x,F8.4,4x,I4,4x,F6.2,4x,F8.4,4x,F8.4)',$
            names[i],flags[i],nscans[i],flux[i],snr[i], mtime[i],rr[i],img[i] 
if sybase eq 1 then begin
read,aa,prompt='Save the flux into database? [NO <YES>]: '
 if (aa eq 'YES' or aa eq 'yes' or aa eq 'Yes' or aa eq 'Y' or aa eq 'y') then begin
   print,'YES: save the flux'
   souid=dbi_sou_exist(names[i])
     quality=''
     comments=''
     read,quality,prompt='Set the quality of the flux measurement: '
     read,comments,prompt='Comments: '
     uto=c.ut[0]
     dbi_flux_write,uto,names[i],souid,'',flux[i],snr[i],freq,quality,comments 
 endif
endif 
         endif
      endfor

   endif else begin
      flux=vecamp
      snr=snr_vecamp
   endelse ; defaults


    ; Done
      return,1
end






