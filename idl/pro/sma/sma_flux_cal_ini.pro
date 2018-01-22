function sma_flux_cal_ini,channel,day_range,user_id,$
            names=names,flags=flags,amp=amp,flux=flux,nscans=nscans
    ; Command blocks
      common global
      common data_set

    ; Initialize 
      flux = 0.0
      amp  = 0.0
      source = ' '
      flags  = ' '

    ; Name must be set
      if (not keyword_set(channel)) then begin
         print,'Channel (c1/c2) must be specified'
         return,-1
      endif

    ; Set day range
      dt = 30
      if keyword_set(day_range) then dt = fix(abs(day_range))
      dt = abs(dt)

    ; Set user id
      user = '%'
      if (keyword_set(user_id)) then user = user_id

    ; Set filter to include band name but exclude noise source
    ; disabled on 2017Jun12
;      command = '"source" ne "noise" and "band" eq "' + channel + '"'
;      result  = dat_list(s_l,command,/reset)
;      if (result eq 0) then begin
;         print,'No data in band ',channel
;         return,-1
;      endif

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
      ut_start = ' '
      ut_stop  = ' '
      command = 'select dateadd(dd,'+string(-dt)+',"' + c.ut[0] + '"),'
      command = command + 'dateadd(dd,'+string(dt)+',"' + c.ut[n_elements(c.ut)-1L] + '")'
      if (e.campuslogin eq 'cqi') then begin
      result = dbi_sql_submit(command)
      reads,format='(2(8x,A19))',result[2],ut_start,ut_stop
      endif

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
      nscans = intarr(ncalib)

    ; Loop over sources
      for i = 0L, nsources-1L do begin
         ; Get source radius in arcseconds
           j = where(in[pil].isource eq distinct_isource[i],nj)
           radius = 0.5 * total(in[pil[j]].size) / nj

         ; First, see if sources is in the primary list
;           result = flux_primary(sources[i],freq,radius,xflux)

         ; If not in primary list, get the secondary flux
;           if (xflux[0] eq 0.0) then $
           if sources[i] eq 'mars' or sources[i] eq 'uranus' $
                or sources[i] eq 'neptune' or sources[i] eq 'jupiter' $
                or sources[i] eq 'venus' or sources[i] eq 'mercury' $
                or sources[i] eq 'saturn' or sources[i] eq 'pluto' $
                or sources[i] eq 'callisto' or sources[i] eq 'titan' $
                or sources[i] eq 'ganymede' or sources[i] eq 'ceres' then begin
              if radius le 0. then begin
                print,"A planet (",sources[i],") found with size 0 arcsec"
                aa=''
                read,aa,prompt='Enter diameter size here: '
                in[pil[j]].size=float(aa)
                radius=0.5*float(aa)
              endif
              result=flux_primary(strlowcase(sources[i]),radius,freq,xflux)
            endif else begin
              result = flux_secondary(sources[i],user,radius,freq,$
                                      xflux,ut_start,ut_stop)
            endelse

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
      iaq    = ['1', '2', ' ']
      iused  = intarr(ncalib)
      ss     = s[0]
      ssflux = 0.0

    ; Now loop over calibrators
      for i = 0, n_elements(iaq)-1 do begin
         ; Find all calibrators of this type
           j = where(iused eq 0,nj)

         ; Store source names
           if (nj gt 0) then begin
              ; Find unique names in calibrator list
                ids = uti_distinct(s[j].isource,nids)

              ; Loop over source names
                for k = 0, nids-1 do begin
                   ; Find all sources with this name
                     l = where(s[j].isource eq ids[k],nl)

                   ; Store sources
                     if (nl gt 0) then begin
                        iused[j(l)] = 1
                        ss = [ss,s[j(l)]]
                        ssflux = [ssflux,flux[j(l)]]
                     endif
                endfor
           endif
      endfor

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
                     sp[psl].igq eq s[i].igq)

         ; Determine number of scans
           k = uti_distinct(in(pil(j)).int,nk)
           nscans[i] = nk

         ; Determine average amplitude
           amp[i] = 0.0
           k =where(bl(pbl(j)).ampave gt 0.0,nk)
           if (nk gt 0) then amp[i] = total(bl(pbl(j(k))).ampave) / nk
      endfor

    ; Print table
      print,'#   Source   Flags   Nscans      Amp        Flux(Jy)'
      for i = 0L, ncalib-1L do begin
         print,format='(A10,4x,A3,5x,I3,2x,g10.2,3x,g10.4)',$
            names[i],flags[i],nscans[i],amp[i],flux[i]
      endfor

    ; Done
      return,1
end
