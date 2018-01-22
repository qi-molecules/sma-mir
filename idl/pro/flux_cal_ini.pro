function flux_cal_ini,day_range,user_id,$
     amp=amp,flags=flags,flux=flux,sources=sources,nint=nint,$
     freq=freq,icalib=icalib,noprint=noprint,all_sources=all_sources
    ; Command blocks
      common global
      common data_set

    ; Initialize 
      flux = 0.0
      amp  = 0.0
      source = ' '
      flags  = ' '

    ; Set day range and user ID
      dt   = keyword_set(day_range) ? abs(day_range) : 30.0
      user = keyword_set(user_id) ? user_id : '%'

    ; Set filter to include band name but exclude noise source.
    ; Also, If all sources is NOT set, then include only primary or 
    ; secondary calibrators
      command = '"source" ne "noise" and "band" like "c"'
      if not keyword_set(all_sources) then $
         command = command + ' and "aq" in ["1","2"]'
      result  = dat_list(s_l,command,/reset,/no)
      if (result eq 0) then return,-1

    ; Find unique source names/calibrator/gain flag combinations in track
      calib = string(in[pil].isource) + ' ' + $
              string(bl[pbl].iaq) + ' ' + $
              string(bl[pbl].icq) + ' ' + $
              string(sp[psl].igq)
      distinct_calib = uti_distinct(calib,ncalib,/many)
      if (ncalib eq 0) then return,-1

    ; Parse distinct calibrators into the four variables
      s = replicate({isource:0L, iaq:0, icq:0, igq:0},ncalib)
      for i = 0L, ncalib-1L do reads,distinct_calib,s

    ; Sort by source name
      isort = sort(c.source[s.isource])
      s = s(isort)

    ; Use SQL command to get min/max date range
    ; The UT times in c.ut are not necessarily in order. Making the assumption
    ; that int=1 is always taken before int=last, I find the min/max integration
    ; number and use that to find the min/max ut time.
      ut_start = ' '
      ut_stop  = ' '
      i1 = min(in(pil).int)
      i2 = max(in(pil).int)
      j1 = where(in.int eq i1)
      j2 = where(in.int eq i2)
      iutmin = in(j1).iut
      iutmax = in(j2).iut
      command = 'select dateadd(dd,'+string(-dt)+',"' + c.ut[iutmin] + '"),' + $
                'dateadd(dd,'+string(dt)+',"' + c.ut[iutmax] + '")'
      result = dbi_sql_submit(command)
      reads,format='(2(8x,A19))',result[2],ut_start,ut_stop

    ; Make list of distinct source IDs in filter
      distinct_isource = uti_distinct(s.isource,nsources,/many)
      sources  = c.source[distinct_isource]

    ; Determine the fluxes for the sources. Fluxes for planets are determined
    ; from tabulated flux vs. frequency tables. Fluxes for quasars are 
    ; determined from the database. For secondary calibrators, I use
    ; alpha stored in the database.

    ; Allocate memory to store amplitudes, fluxes, flags, and sources
      sources = c.source[s.isource]
      amp   = fltarr(ncalib)
      flags = c.aq[s.iaq] + c.cq[s.icq] + c.gq[s.igq]
      flux  = fltarr(ncalib)
      freq  = fltarr(ncalib)
      nint  = intarr(ncalib)
      icalib = intarr(ncalib)

    ; icalib indicates which are designated primary, secondary, or
    ; non calibrators.
    ;    1 -> primary
    ;    2 -> secondary
    ;    0 -> non-calibrator
    ; This is essentially c.aq turned into an integer representation
      saq = c.aq[s.iaq]
      j = where(saq ne ' ',nj)
      for i=0, nj-1 do begin
         iflag = 0
         if valid_num(saq[j(i)],iflag,/integer) then icalib[j(i)] = iflag
      endfor

    ; Loop over sources
      for i = 0L, nsources-1L do begin
         ; Find data with these flag settings
           j = where(in[pil].isource eq s[i].isource and $
                     bl[pbl].iaq eq s[i].iaq and $
                     bl[pbl].icq eq s[i].icq and $
                     sp[psl].igq eq s[i].igq,nj)

         ; Get source radius in arcseconds
           radius = 0.5 * total(in[pil[j]].size) / nj

         ; Compute average frequency
           freq[i] = total(sp[psl(j)].fsky) / nj

         ; Get primary souce flux. If result = -1, then source is not a 
         ; primary calibrator.
           result = flux_primary(sources[i],radius,freq[i],xflux)

         ; If not in primary list (result=-1), get the secondary flux
           if result eq -1 then $
              result = flux_secondary(sources[i],user,radius,freq[i],$
                                      xflux,ut_start,ut_stop)

         ; Store flux in array
           flux[i] = xflux

         ; Determine number of integrations for this source
           k = uti_distinct(in(pil(j)).int,nk)
           nint[i] = nk

         ; Determine average observed amplitude
           amp[i] = total(bl(pbl[j]).ampave) / nj
      endfor

    ; Print table
      if not keyword_set(noprint) then begin
         print,'#   Source   Flags  Nint    Amp   Flux(Jy) Freq(GHz)'
         for i = 0L, ncalib-1L do begin
            print,format='(A10,4x,A3,3x,I3,2x,F6.2,3x,F6.2,3x,F8.3)',$
               sources[i],flags[i],nint[i],amp[i],flux[i],freq[i]
         endfor
      endif

    ; Done
      return,1
end
