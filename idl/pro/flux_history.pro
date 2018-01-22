; *************************************************************************
; FUNCTION
;      flux_history
;
; WRITTEN 
;      September 23, 2000 by JMC
;
; PURPOSE
;      Print/plot flux history
;
; INPUTS 
;      SOURCE      : Source name. Wildcard characters (*,%) accepted
;      BAND        : Band to plot (-1->all, 0->7mm, 1->3mm, 2->2mm, 3->1mm)
;      FREQUENCY   : Frequency (GHz) to report fluxes
;      ALPHA       : Frequency dependence of fluxes
;      PLID        : Used for IDL
;      USERID      : User ID for fluxes (e.g. ["jmc","mm"])
;      SNRMIN      : Minimum signal to noise to plot
;      UT_START    : Starting UT date (e.g. 'Jan 1 1950 12:00:00AM')
;      UT_STOP     : Ending   UT date (e.g. 'Jan 1 2050 12:00:00AM')
;      WEIGHT      : 0->uniform (default), !=0 ->SNR
;      NOPLOT      : 1->Do not plot data; 0->plot data (default)
;      DT          : Search for fluxes within DT days around observing date
;
; OUTPUT
;      -1,    if could not find any fluxes
;      else,  number of pages plotted
;
;      Output arrays
;         nbands           : Number of output bands
;         distinct_sources : List of distinct sources
;         labels           : List of distinct bands
;         ave_fluxes       : Average fluxes, size nsources x nbands
;         source_out       : List of sources for each measurement
;         flux_out         : List of fluxes  for each measurement
;         snr_out          : List of SNR     for each measurement
;         freq_out         : List of frequency for each measurement
;         utdate_out       : UT date for each measurement
;         uid_out          : User ID for each measurement
;
; EXAMPLES
;      result = flux_history('3c345')
;
; *************************************************************************
function flux_history,source,plid,$
         band=band,iband=iband,alpha=alpha,frequency=frequency,userid=userid,$
         snrmin=snrmin,ut_start=ut_start,ut_stop=ut_stop,weight=weight,$
         nbands=nbands,distinct_sources=distinct_sources,$
         labels=labels,ave_fluxes=ave_fluxes,source_out=source_out,$
         flux_out=flux_out,snr_out=snr_out,freq_out=freq_out,$
         utdate_out=utdate_out,uid_out=uid_out,noplot=noplot,noprint=noprint,$
         dt=dt,obsdate=obsdate

    ; Plot data
      iplot = keyword_set(noplot) ? 0 : 1
      if (iplot) then loadct,39,/silent

    ; Determine weighting for data
      iweight = keyword_set(weight)

    ; Set frequency and alpha for output fluxes
      if (not keyword_set(alpha)) then alpha = 0.0
      freq  = keyword_set(frequency) ? frequency : 0.0
      freq = freq > 0.0

    ; Set starting/end utdates
      ut1 = 'Jan 1 1950 12:00:00AM'
      ut2 = 'Jan 1 2050 12:00:00AM'
      if keyword_set(ut_start) then ut1 = ut_start
      if keyword_set(ut_stop)  then ut2 = ut_stop
      dateadd = 0
      if keyword_set(dt) and keyword_set(obsdate) then begin
        dateadd = 1
        ut1 = 'dateadd(hh,' + string(-24.0*abs(dt)) + ',"' + obsdate + '")'
        ut2 = 'dateadd(hh,' + string(24.0*abs(dt))  + ',"' + obsdate + '")'
      endif

    ; Set SNR
      snr = keyword_set(snrmin) ? snrmin : 0.0

    ; Set band: 0 -> all
    ; Dividing line between bands is freq/68
    ; band 1 0-68 , 2 68-136 , 3 136-204 , 4 204-272
      band_names = ['7mm', '3mm', '2mm', '1mm']
      if not keyword_set(iband) then begin
         iband = -1
         if keyword_set(band) then begin
            j = where(band eq band_names,nj)
            if (nj gt 0) then iband = j[0]
         endif
      endif else $
         iband = fix(iband[0])

    ; Change any '*' characters to wildcard
      uti_stri_replace,source,'*','%'

    ; Is there a wildcard in the source name?
      iwildcard = strpos(source,'%')

    ; Create SQL command
      command = 'select flux,freq,snr,datediff(day,uto,getdate()),convert(int,freq/68.0),reid,ut,source from FLU '
      swild = (iwildcard ge 0) ? 'where source like "' + source + '" ' : $
                                 'where source ="' + source + '" ' 
      sdate = dateadd ? ' and uto >= ' + ut1 + ' and uto <= ' + ut2 : $
                        ' and uto >= "' + ut1 + '" and uto <= "' + ut2 + '"'
      command = command + swild + sdate + ' and snr >= ' + string(snr)
      if (iband ne -1) then $
         command = command + ' and convert(int,freq/68.0)=' + string(iband)
      if keyword_set(userid) then begin
         command = command + ' and ('
         for i = 0, n_elements(userid)-1L do begin
            if (i gt 0) then command = command + ' or '
            command = command + ' reid = "' + userid[i] + '"'
         endfor
         command = command + ')'
      endif

    ; Compress string 
      command = strcompress(command)

    ; Execute command
      result = dbi_sql_submit(command)

    ; Make sure data was read in
      j = where(result eq '(0 rows affected)',nj)
      if (nj ne 0) then return,-1

    ; Set structure to read table
      ndata = n_elements(result) - 4L
      temp = {flux:0.0, freq:0.0, snr:0.0, dt:0.0, band:0L, $
              uid:' ', ut: ' ', source:' '}
      s = replicate(temp,ndata)
      reads,format='(3F21.6,2I12,A4,A28,A13)',result(2L:2L+ndata-1L),s

    ; Only keep 3mm and 1mm data 
;     j = where(s.band eq 1 or s.band eq 3,nj)
;     if (nj eq 0) then return,-1
;     s = s[j]
;     ndata = nj

    ; If necesary, extrapolate database frequencies to observed frequencies
    ; Also, set the band string for the plots.
      if (freq ne 0.0) then begin
         s.flux = s.flux * (freq/s.freq)^alpha
         s.freq = freq
         s.band = fix(s.freq/68.0)
         sband  = replicate(string(format='(F10.2," GHz")',freq),ndata)
         sband  = strcompress(sband)
      end else begin
         sband = band_names[s.band]
      endelse

    ; Determine number of sources
      distinct_sources = uti_distinct(s.source,nsources,/many_repeat)
      if (nsources gt 1) then sband = strcompress(s.source + " - " + sband)

    ; Determine number of bands
      distinct_bands = uti_distinct(s.band,nbands,/many_repeat)
      distinct_bands = distinct_bands(sort(distinct_bands))

    ; Determine average flux per source per band
      ave_fluxes = make_array(nsources,nbands,/float,val=-1.0)
      npts = intarr(nsources,nbands)
      for i = 0L, nsources-1L do begin
         for j = 0L, nbands-1L do begin
            ; Find sources for this band
              k = where(s.source eq distinct_sources[i] and $
                        s.band eq distinct_bands[j],nk)
              if (nk gt 0) then begin
                 weights = s[k].snr
                 if (iweight eq 0) then weights = replicate(1.0,nk)
                 ave_fluxes[i,j] = total(weights*s[k].flux) / total(weights)
              endif
              npts[i,j] = nk
         endfor
      endfor

    ; Set label names
      labels = strarr(nbands)
      for j = 0L, nbands-1L do begin
         if (freq ne 0) then begin
            labels[j] = sband[0]
         endif else begin
            labels[j] = band_names[distinct_bands[j]]
         endelse
      endfor

    ; Multiply dt by -1
      s.dt = -1.0 * s.dt

    ; Set label string for plot
      blabel = "Flux history"
      if (nsources eq 1) then begin
          blabel = blabel + ' for ' + s[0].source
      endif
      if keyword_set(userid) then blabel = blabel + ", UserID = " + userid
      blabel = strcompress(blabel)

    ; Print most recent data first
      j = reverse(sort(s.dt))

    ; Set return data - this overwrites some variables
      source_out = s(j).source
      flux_out   = s(j).flux
      snr_out    = s(j).snr
      freq_out   = s(j).freq
      utdate_out = s(j).ut
      uid_out    = s(j).uid

    ; Plot data
      if (iplot) then begin
         ydata  = reform(transpose(s.flux),1,ndata)
         result = wlm_plot_data(s.dt,ydata,s.band,$
                    sband,"Days since current UT",$
                    ["Flux (Jy)"],plot=1,m_options="cs",blabel=blabel,$
                    bad_value=(min(s.dt)-1.0),psym=1,plid,nframes_max=2)
      endif

    ; Print table of source fluxes
      if not keyword_set(noprint) then begin
          if (nbands eq 1) then begin
             print," Source      Flux("+labels[0]+")  N("+labels[0]+")
             print," ------      ---------  ------"
          endif else begin
             print," Source      Flux("+labels[0]+")  Flux("+labels[1]+")  N("+labels[0]+")  N("+labels[1]+")
             print,"               (Jy)       (Jy)"
             print," ------      ---------  ---------  ------  ------"
          endelse
          for j=0L, nsources-1L do begin
             if (nbands eq 1) then begin
                print,format='(A12,F8.3,2x,I6)',$
                   distinct_sources[j],ave_fluxes[j,0],npts[j,0]
             endif else begin
                print,format='(A12,F8.3,3x,F8.3,2(2x,I6))',distinct_sources[j],ave_fluxes[j,0],ave_fluxes[j,1],npts[j,0],npts[j,1]
             endelse
          endfor
      endif

    ; Print fluxes
;     for i = 0L, ndata-1L do begin
;        print,source_out[i],freq_out[i],flux_out[i],snr_out[i],$
;              utdate_out[i],uid_out[i]
;     endfor

    ; Return number of plot pages
      result = uti_distinct(sband,nplots,/many_repeat)
      return,fix(0.5*nplots + 0.5)
end
