; *************************************************************************
; FUNCTION
;      wlm_derive
;
; WRITTEN 
;      July 31, 1998 by JMC
;
; PURPOSE
;      Calibrates phase changes with change in WLM temperatures
;
; INPUTS 
;      noplot  : If set, WLM plots are not shown
;     outroot  : Output root name for postscript files
;   postscript : If set, then write a postscript file
;
; OUTPUT
;      -1   Calibration failed.
;       1   Calibrated WLM data stored successfully
;
; EXAMPLES
;      result = wlm_derive()
;
; BUGS
;      (1) Cannot flag/remove "bad" water-line data from final calibration
;      (2) No way of telling if WLM data in CAL structure is the same as
;          the astronomy data. But this should not be a problem as long as
;          we re-initialize the cal structure everytime a track is read in.
;
; COMMENTS ON COMPUTING TIME STAMPS FOR ASTRONOMY DATA
;   AUTHOR: NZS on Aug 1, 1998, amended by Steve Scott Aug 2, 1998
;
;   (1) in 'in' structure
;          dhrs=convert(float,datediff(ms,ref_time,ut))/3.6e+6
;          where ut is the ut for the beginning of the integration.
;          Therefore for the first integration, the hours offset from 
;          the reference time is : in[0].dhrs
;
;          The in.ut is a somewhat arbitrary timestamp near the start 
;          of the integration. Its accuracy is +-20 or even 30 seconds 
;          from absolute time.
;
;   (2) Note that the ref_time in ascii form is the calendar date (0 hrs ut)
;       which precedes the earliest data in the data set. Therefore all dhrs 
;       will be positive. the integer code which points to the ref_time is 
;       given by in[0].iref_time and to get this ascii string for the first 
;       integration, use :  c.ref_time[in[0].iref_time]
;
;   (3) in 'bl' structure
;          avedhrs=convert(float,datediff(ms,th.ref_time,utave))/3.6e+6
;          (this time seems to have no real utility)
;
;   (4) re.toffs
;          time offset in sec from in.dhrs for each record. Toff is gotten
;          by taking the absolute average time of the record data and
;          subtracting in.ut.
;
;   (5) re.integs
;          the integration time of the record in sec
;
;   (6) To compute the time offset in fractional hours at the middle 
;       of record (i),
;          t_rec = in.dhrs + (re.toffs[i] / 3600.0)
; *************************************************************************

function wlm_derive3,plot=plot,defaults=defaults,mklog=mklog,$
                    outroot=outroot,postscript=postscript,$
                    receiver=receiver,sideband=sideband,cband=cband,$
                    allplots=allplots,linear=linear,$
                    scale_factor_init=scale_factor_init,$
                    scale_factor_use=scale_factor_use
  ; Common blocks
    common global
    common data_set
    common wlm

  ; BOOLEANS
    ilog      = keyword_set(mklog)
    idefaults = keyword_set(defaults)
    iplot     = keyword_set(plot)
    ipostscript = (iplot and keyword_set(postscript) and keyword_set(outroot))

  ; RECEIVER
    irec = 1
    if keyword_set(receiver) then begin
      irec = fix(receiver)
      if (irec lt 1 or irec gt 2) then begin
        printf,-1,'Error entering receiver number'
        return,-1
      endif
    endif
    print,strcompress('--- Using receiver '+ string(irec))
    srec = strcompress('"rec" eq "' + string(irec) + '"')

  ; Filter data to get gain sources
    if (dat_list(s_l,'"band" like "c" and ' + srec + 'and "gq" like "g"',$
       /reset,/no_notify) le 0) then begin
       print,"Need gain calibrators to determine WLM-phase correlation."
       return,-1
    end

  ; Determine available continuum bands for this receiver
    bands = c.band(sp(psl).iband)
    distinct_bands = uti_distinct(bands,nbands)
    ic1 = 0
    ic2 = 0
    j = where(distinct_bands eq 'c1',nj)
    if (nj gt 0) then ic1 = 1
    j = where(distinct_bands eq 'c2',nj)
    if (nj gt 0) then ic2 = 1

  ; SOURCE NAMES
    source = c.source(in(pil).isource)
    distinct_sources = uti_distinct(source)
    j = where(distinct_sources ne "noise",nsources)
    if (nsources eq 0) then begin
       print,"No gain calibrator in track"
       return,-1
    endif
    distinct_sources = distinct_sources(j)

  ; BRIGHT SOURCES
    ibright = [0]
    bright_sources = ['3c273', '3c345', '3c454.3', '3c111', 'bllac', '3c84',$
                      '3c279', '0923+392', '1334-127', 'nrao530']
    for i = 0, nsources-1L do begin
        j = where(distinct_sources[i] eq bright_sources,nj)
        if (nj gt 0) then ibright = [ibright,i]
    endfor
    nbright = n_elements(ibright) - 1L
    if (nbright gt 0) then ibright = ibright[1:nbright]

  ; Pick source name(s) for WLM phase calibration
    if nbright gt 0 then ids_default=ibright else ids_default=lindgen(nsources)
    ids = ids_default
    if (nsources gt 1 and not idefaults) then begin
       irepeat = 1
       while (irepeat) do begin
         print,format='("Choose source(s) for WLM-phase calibration ")'
         print,format='("(separate ID numbers by spaces or commas)")'
         com = "default: All sources"
         if (nbright gt 0) then begin
            com = "default: "
            for i = 0, nbright-1L do $
               com = com + " " + distinct_sources[ibright[i]]
            com = strcompress(com)
         endif
         print,format='("      0 --> ",a)',com
         for i = 0L, nsources-1L do $
           print,format='("     ",I2," --> ",a)',i+1,distinct_sources(i)
         print,format='($,a)',"      Choice? "
         ids = ids_default
         input = " "
         read,input
         repeat begin
           pos = strpos(input,',')
           if (pos ne -1) then strput,input,' ',pos
         endrep until (pos eq -1)
         input = strcompress(input)
         irepeat = 0
         if (input ne '') then begin
            input = uti_distinct(long(strsplit(strcompress(input),' ',/extract))) - 1L
            if (not (n_elements(input) eq 1 and input(0) eq -1)) then begin
              ids = input
              j = where(ids lt 0 or ids ge nsources,nj)
              if (nj gt 0) then begin
                 irepeat = 1
                 print," "
                 print,"                *** INVALID ENTTRY ***"
                 print," "
              endif
            endif
         endif
       endwhile
    endif
    print,"--- Sources used for WLM-phase calibration : ",distinct_sources(ids)
    print," "
    print," "

  ; SIDEBAND
    sb = c.sb(bl(pbl).isb)
    distinct_sb = uti_distinct(sb,nsb)
    sb_default = "L"
    irepeat = 1
    if keyword_set(sideband) then begin
       sb = strupcase(strmid(sideband,0,1))
       irepeat = 0
       if (sb ne 'B' and sb ne 'L' and sb ne 'U') then begin
          print,"Error entering SIDEBAND"
          irepeat = 1
       endif
    endif else if idefaults then begin
       sb = sb_default
       irepeat = 0
    endif
    while (irepeat) do begin
        print,format='("Choose sidebands for WLM-phase calibration ")'
        print,"      L --> Lower (default)"
        print,"      U --> Upper"
        print,"      B --> Both"
        print,format='($,"      Choice? ")'
        irepeat = 0
        sb = sb_default
        input = " "
        read,input
        if (input ne '') then begin
           sb = strupcase(strsplit(strcompress(input),' ',/extract))
           if (sb(0) ne 'B' and sb(0) ne 'L' and sb(0) ne 'U') then begin
              irepeat = 1
              print," "
              print,"                *** INVALID ENTTRY ***"
              print," "
           endif
        endif
    endwhile
    sb = sb(0)
    case sb of 
      "B": print,"--- Using LOWER and UPPER sideband data"
      "L": print,"--- Using LOWER sideband data only"
      "U": print,"--- Using UPPER sideband data only"
    endcase
    print," "
    print," "

  ; Set the filter command --- SIDEBAND
    filter = srec + ' and "gq" like "g"' 
    case sb of 
       "B": 
       "L": filter = filter + ' and "sb" eq "l"'
       "U": filter = filter + ' and "sb" eq "u"'
    endcase

  ; BAND
    band_default = "C1"
    irepeat = 1
    if keyword_set(cband) then begin
       band = strupcase(cband)
       irepeat = 0
       if (band ne 'E' and band ne 'B' and band ne 'C1' and band ne 'C2') then begin
          print,"Error entering CBAND"
          irepeat = 1
       endif
    endif else if idefaults then begin
       band = band_default
       irepeat = 0
    endif
    if (irepeat and (ic1 eq 0 or ic2 eq 0)) then begin
       band = "C1"
       irepeat = 0
       print,"Only one continuum band available..."
       if (ic2 ne 0) then band = "C2"
    endif
    while (irepeat) do begin
        print,format='("Choose sidebands for WLM-phase calibration ")'
        print,"      E --> C1 if available, else C2 (default)"
        print,"     C1"
        print,"     C2"
        print,"      B --> Both"
        print,format='($,"      Choice? ")'
        irepeat = 0
        band = band_default
        input = " "
        read,input
        if (input ne '') then begin
           band = strupcase(input)
           if (band ne 'E' and band ne 'B' and band ne 'C1' and band ne 'C2') then begin
              irepeat = 1
              print," "
              print,"                *** INVALID ENTTRY ***"
              print," "
           endif
        endif
    endwhile
    if (band eq "E") then begin
      if (ic1 eq 1) then begin
         band = "C1"
      endif else begin
         band = "C2"
      endelse
    endif
    case band of 
      "B" : print,"--- Using C1 and C2 data"
      "C1": print,"--- Using C1 data only"
      "C2": print,"--- Using C2 data only"
    endcase
    print," "
    print," "

  ; Set the filter command --- BAND
    case band of 
       "B" : filter = filter + ' and "band" like "c"'
       "C1": filter = filter + ' and "band" eq "c1"'
       "C2": filter = filter + ' and "band" eq "c2"'
    endcase

  ; Set the filter command --- SOURCE NAMES
    if (n_elements(ids) ne nsources) then begin
       filter = filter + ' and ('
       for i = 0, n_elements(ids)-1L do begin
          if (i ne 0) then filter = filter + " or"
          filter = filter + ' "source" eq "' + distinct_sources(ids(i)) + '"'
       endfor
       filter = filter + ')'
    endif

  ; Filter the data
    if (dat_list(s_l,filter,/reset,/no_notify) le 0) then begin
       print,"No gain calibrators for this band/receiver"
       print,filter
       return,-1
    end

  ; Regrid WLM data onto astronomy time stamps
    dt_wlm = 2.0D/3600.0D
    result = wlm_regrid(wlm_diff,lambda,wlm_cal,wlm_times,dt_wlm,wlm_id,$
                       itel1=itel1,itel2=itel2,iscan=iscan,isource=isource,$
                       times=times,phases=phases,linear=linear,labels=labels,$
                       /coherence,/store)
    if (result eq -1) then begin
       printf,-1,'Cannot derive scale factor'
       printf,-1,'No WLM data present for the calibrator source'
       scale_factor_use = 0.0
       return,-1
    endif

  ; Derive the scale factor for each integration
    print," "
    print," "
    print,"--- Deriving scale factor for individual scans ... "
    time = systime(1)
    result = wlm_scale_derive(wlm_diff,phases,lambda,itel1,itel2,iscan,$
               times,factor=factor,ave_time=ave_time,plot=allplots,$
               linear=linear,sigma_before=sigma_before,sigma_after=sigma_after)
    s = wlm_settime(time)
    print,strcompress("--- Finished deriving phase corrections ... time = "+s)
    print,""
    print,""

  ; Set median scale factor
    xadopt  = 0.0
    xmedian = 0.0
    j = where(factor gt !BAD_VALUE,nj)
    if (nj eq 0) then begin
       printf,-1,'Could not derive scale factor'
       scale_factor_use = 0.0
       return,-1
    endif
    factor = factor(j)
    ave_time = ave_time(j)
    xmedian = median(factor,/even)

  ; Plot conversion factors
    if (iplot) then begin
       npts = n_elements(factor)
       x = ave_time - ave_time[0]
       y    = fltarr(1,npts)
       yfit = fltarr(1,npts)
       y[0,*] = factor
       yfit[0,*] = xmedian
       wts = make_array(npts,/int,value=1)
       str = strcompress("Median = " + string(xmedian,format='(F5.2)'))
       ids = make_array(npts,/string,value=str)
       blabel = "Scale factor [mm/K] vs Time for Track " + $
                string(in(pil(0)).traid)
       blabel = strcompress(blabel)
       if ipostscript then wlm_open,strcompress(outroot + '_factor.ps')
       a = findgen(16) * (!DPI*2/16.)
       usersym,cos(a),sin(a),/fill
       result = wlm_plot_data(x,y,wts,ids,$
                  "Time [hours]",["X"],yfit=yfit,psym=[8],$
                  plot=plot,m_options="cspne",blabel=blabel)
       if ipostscript then wlm_close
    endif

  ; Call xfit routine with X=0 to compute unwrapped phases
    result = wlm_xfit3(wlm_diff,phases,lambda,times,0.0,labels,$
                       pderived=pderived,linear=linear,rms_before=rms_before)
    pbefore = pderived

  ; Set good and bad WLM values
    jbad  = where(wlm_diff le !BAD_VALUE,nbad)
    jgood = where(wlm_diff gt !BAD_VALUE,ngood)
    if (ngood eq 0) then begin
       printf,-1,'Cannot derive scale factor'
       printf,-1,'No WLM data present for the calibrator source'
       scale_factor_use = 1.0
       return,-1
    endif

  ; Initialize variables to generate grid of scale factors/phase residuals
    nstart =  2
    nstop  = 20
    nstep  =  2
    ngrid  = (nstop-nstart)/nstep + 1
    phases_mm = pbefore
    rms    = rms_before
    ids    = replicate(0,ngood)
    xsample = lindgen(ngood)+1
    xfactor = 0.0

  ; Generate plot of phase residuals with various scale factors
    print," "
    print," "
    print,"--- Computing grid of scale factors ... "
    time = systime(1)
    n = 0
    for i = nstart, nstop, nstep do begin
       ; Compute residuals for this scale factor
         result = wlm_xfit3(wlm_diff,phases,lambda,times,float(i),labels,$
                            pderived=pderived,linear=linear,$
                            rms_after=rms_after)

       ; Compute residuals
         residuals = pderived - wlm_diff*i

       ; Compute residuals rms
         rms = [rms,rms_after]

       ; Store results
         phases_mm  = [phases_mm,residuals]
         ids     = [ids,replicate(i,ngood)]
         xsample = [xsample,lindgen(ngood)+1]
         xfactor = [xfactor,1.0*i]
    endfor
    s = wlm_settime(time)
    print,"--- Done generating grid ... time = " + s

  ; Print table of before/after residuals
    print," "
    print," "
    print,strcompress("#       RESULTS FOR TRACK " + string(in(pil(0)).traid))
    print,"#   SCALE        RMS       Percent"
    print,"#  FACTOR       [mm]       Decrease"
    print,"# --------   ----------  -----------"
    for i = 0L, n_elements(xfactor)-1L do begin
        if (xfactor[i] eq 0.0) then begin
           print,format='("  Raw Data   ",F8.3,"         ****")',rms[i]
        endif else begin
           print,format='(3x,F5.2,6x,F7.3,6x,F7.1)',xfactor[i],rms[i],$
                    -(rms[i]-rms[0])/rms[0]*100.0
        endelse
    endfor
    print,"# NOTE: PERCENT DECREASE SHOULD BE POSITIVE IF"
    print,"#       THE WLM CORRECTIONS IMPROVED THE DATA"
    print," "
    print," "

  ; Plot grid
    if ipostscript then wlm_open,strcompress(outroot + '_grid.ps')
    blabel = 'Track  ' + string(in(pil(0)).traid)
    blabel = strcompress(blabel)
    a = findgen(16) * (!DPI*2/16.)
    usersym,0.25*cos(a),0.25*sin(a),/fill
    ys = fltarr(1,n_elements(phases_mm))
    ys[0,*] = phases_mm
    result = wlm_plot_data(xsample,ys,xsample,ids,$
                 "Sample number",["Residuals"],plid,$
                 psym=[8,8,8],plot=allplots,$
                 m_options="cspne",blabel=blabel,nframes_max=ngrid+1)
    if ipostscript then wlm_close

  ; If in plot mode, determine the scale factor the user wants to adopt
    if keyword_set(scale_factor_use) then $
       xadopt = scale_factor_use $
    else if idefaults and ngood gt 0 then $
       xadopt = xmedian $
    else begin
       irepeat = 1
       while (irepeat) do begin
          print," "
          print," "
          print,format='("Hit return to use default scale factor or enter desired value")'
          print,format='("    Default Scale factor = ",F5.2," mm/K")',xmedian
          print,format='($,a)',"    Choice? "
          xnew = xmedian
          input = ' '
          read,input
          if (input ne '') then reads,input,xnew
          irepeat = 0
          if (xnew lt 0.0) then begin
             irepeat = 1
             printf,-1,' '
             printf,-1,' **** INVALID SCALE FACTOR ****'
             printf,-1,' '
          endif
       endwhile
       xadopt = xnew
       if (xadopt eq 0) then return,-1
    endelse
    print," "

  ; Print results to a file
    if ilog then begin
      output = strcompress(outroot + "_x.dat",/remove)
      openw,unit,output,/get_lun
      printf,unit,format='("# XADOPT  ",F10.3)',xadopt
      printf,unit,format='("# XMEDIAN ",F10.3)',xmedian
      j = where(factor ne !BAD_VALUE and sigma_before gt 0.0,nj)
      if (nj gt 0) then $
        change = 100.0 * (sigma_before(j)-sigma_after(j)) / sigma_before(j)
      printf,unit,"# ID       TIME           FACTOR    BEFORE  AFTER   CHANGE  BSL"
      for i = 0L, nj-1L do begin
         printf,unit,format='(I4,F13.7,1x,F15.6,2(1x,F7.3),1x,F8.2,2x,a)',$
            i+1,ave_time[j(i)]-wlm_times(0),factor[j(i)],$
            sigma_before[j(i)], sigma_after[j(i)],change[i],$
            c.blcd[bl(pbl(i)).iblcd]
      endfor
      if (nj eq 0) then $
         printf,unit,format='(I4,F13.7,F12.6,2F7.3,1x,F8.2,2x,a)',$
              0, 0.0, 0.0, 0.0, 0.0, 0.0, 'no_data'
      close,unit
      free_lun,unit
    endif

  ; Recompute the phase residuals based on the adopted scale factor
    if ipostscript and keyword_set(allplots) then $
       wlm_open,strcompress(outroot + '_unwrap.ps')
    result = wlm_xfit3(wlm_diff,phases,lambda,times,xadopt,labels,$
               track=in(0).traid,pderived=pderived,linear=linear,plot=allplots)
    if ipostscript and keyword_set(allplots) then wlm_close

  ; Scale the wlm data by the conversion factor
    wlm_diff = wlm_diff * xadopt
    frames = "BSL " + strcompress(string(itel1) + "-" + string(itel2),/remove)

  ; Set frame ids to plot by baseline
    xs     = lonarr(ngood)
    xs_all = lonarr(ngood)
    xs_all = lindgen(ngood)
    distinct_frames = uti_distinct(frames,many_distinct=0,sample=10,nframes)

  ; Compute RMS before and after water line corrections
    wavelength = lambda(jgood[0])
    nrms       = lonarr(nframes+1)
    rms_before = fltarr(nframes+1)
    rms_after  = fltarr(nframes+1)
    rms_change = fltarr(nframes+1)
    for i = 0, nframes do begin
       if (i lt nframes) then $
          j = where(frames eq distinct_frames[i],nj) $
       else begin
          nj = n_elements(wlm_diff)
          j  = lindgen(nj)
       endelse
       if (nj gt 2) then begin
          k = where(pbefore(j) gt !BAD_VALUE and $
                    wlm_diff(j) gt !BAD_VALUE,nk)
          if (i ne nframes) then xs[j(k)]=lindgen(nk)
          if nk gt 0 then begin
            rms_before[i] = stddev(pbefore(j(k)))
            rms_after[i]  = stddev(pderived(j(k))-wlm_diff(j(k)))
            nrms[i] = n_elements(k)
            if (rms_before[i] ne 0.0) then $
              rms_change[i] = (rms_before[i]-rms_after[i]) / rms_before[i]*100.0
            if (ilog and i ne nframes) then begin
              output = outroot + "_" + strmid(distinct_frames[i],4,3) + ".dat"
              output = strcompress(output,/remove)
              openw,unit,output,/get_lun
              printf,unit,format='("# LAMBDA     ",F10.4," mm")',wavelength
              printf,unit,format='("# RMS_BEFORE ",F10.4," mm")',rms_before[i]
              printf,unit,format='("# RMS_AFTER  ",F10.4," mm")',rms_after[i]
              printf,unit,format='("# RMS_CHANGE ",F10.4," mm")',rms_change[i]
              printf,unit,"# CONFIG     ",c.cocd
              printf,unit,"#    ID          PHASE            WLM              DIFF             TIME"
              for l = 0, n_elements(k)-1L do $
               printf,unit,format='(I7,4(4x,F13.6))',$
                 l+1,pderived[j(k(l))],wlm_diff[j(k(l))],$
                     pderived[j(k(l))]-wlm_diff[j(k(l))],times[j(k(l))]
              close,unit
              free_lun,unit
            endif
          endif
       endif
    endfor

  ; Plot Phase vs. WLM
    if ipostscript then wlm_open,outroot + '_slopes.ps'
    ys   = fltarr(1,ngood,/nozero)
    wts  = make_array(ngood,/long,value=1L)
    ys[0,*] = pderived
    ; ys[0,*] = pbefore
    blabel = 'Phase (mm) vs. WLM (mm) for Track ' + string(in(pil[0]).traid)
    blabel = strcompress(blabel)
    a = findgen(16) * (!DPI*2/16.)
    usersym,0.75*cos(a),0.75*sin(a),/fill
    result = wlm_plot_data(wlm_diff,ys,wts,frames,$
                 "WLM",["Phases"],psym=[8],plot=plot,$
                 m_options="cspne",blabel=blabel,/same,nframes_max=15)
    if ipostscript then wlm_close

  ; Create arrays for plotting WLM, phases, and residuals
    ys      = fltarr(3,ngood,/nozero)
    wts     = make_array(ngood,/long,value=1L)
    ys[0,*] = wlm_diff
    ys[1,*] = pbefore
    ys[2,*] = reform(pderived - ys[0,*])

  ; Plot all baselines
    if ipostscript then wlm_open,strcompress(outroot + '_residuals.ps')
    f = make_array(ngood,/string,value="All")
    blabel = $
        'Track  ' + string(in(pil(0)).traid) + "; " + $
        'X = ' + string(xadopt,format='(F5.2)') + " mm/K; " + $
        'RMS init = ' + string(rms_before(nframes),format='(F8.2)')+ " mm; " + $
        'RMS final = ' + string(rms_after(nframes),format='(F8.2)')+ " mm; " + $
        'Change = ' + string(rms_change(nframes),format='(F8.1)') + "%"
    a = findgen(16) * (!DPI*2/16.)
    usersym,0.25*cos(a),0.25*sin(a),/fill
    blabel = strcompress(blabel)
    result = wlm_plot_data(xs_all,ys,wts,f,$
                 "Sample number",["WLM", "Phases",$
                 "Residuals"],psym=[8,8,8],plot=plot,$
                 m_options="cspne",blabel=blabel,/same)

  ; Plot individual baselines
    a = findgen(16) * (!DPI*2/16.)
    usersym,0.5*cos(a),0.5*sin(a),/fill
    for i = 0, nframes-1 do begin
        nk = 0
        j = where(frames eq distinct_frames[i],nj)
        if nj gt 2 then $
          k = where(pbefore(j) gt !BAD_VALUE and wlm_diff(j) gt !BAD_VALUE,nk)
        if (nj gt 2 and nk gt 2) then begin
          w = intarr(ngood)
          w(j(k)) = 1
          blabel = $
            'Track  ' + string(in(pil(0)).traid) + "; " + $
            'X = ' + string(xadopt,format='(F5.2)') + " mm/K; " + $
            'RMS init = ' + string(rms_before[i],format='(F8.2)') + " mm; " + $
            'RMS final = ' + string(rms_after[i],format='(F8.2)') + " mm; " + $
            'Change = ' + string(rms_change[i],format='(F8.1)') + "%"
          blabel = strcompress(blabel)
          result = wlm_plot_data(xs[j(k)],ys[*,j(k)],w(j[k]),frames(j[k]),$
                   "Sample number",["WLM", "Phases", "Residuals"], $
                   psym=[8,8,8],plot=plot,$
                   m_options="cspne",nframes_max=1,blabel=blabel,/same)
        endif
    endfor
    ; if ipostscript then wlm_close

  ; Print header to indicate changes in the phase rms
    com1 = strcompress('RESULTS FOR TRACK ' + string(in(pil(0)).traid))
    com2 = strcompress('SCALE FACTOR = ' + string(xadopt,format='(F6.3)'))
    for i = 0, ilog do begin
       unit = -1
       if i eq 1 then begin
          output = strcompress(outroot + "_change.dat",/remove)
          openw,unit,output,/get_lun
       endif
       printf,unit,format='("# LAMBDA ",F8.4," mm")',wavelength
       printf,unit,format='("# FREQ   ",F8.4," GHz")',!CVEL/wavelength/1e6
       printf,unit,"# CONFIG ",c.cocd
       printf,unit,format='("#        ********* ",a23," *********")',com1
       printf,unit,format='("#        ********* ",a23," *********")',com2
       printf,unit,"#"
       printf,unit,"# BASELINE   RMS_BEFORE  RMS_AFTER   % Change    NPTS"
       printf,unit,"#               [mm]       [mm]"
       printf,unit,"# --------   ----------  ---------   --------   ------"
    endfor
    for i = 0, nframes do begin
       label  = "All"
       if (i lt nframes) then label = strmid(distinct_frames[i],4,3)
       for j = 0, ilog do begin
          iunit = -1
          if (ilog and j eq ilog) then iunit = unit
          if (nrms[i] gt 0) then $
           printf,iunit,format='("   ",a7,2("     ",F6.3),"  ",F10.2,"    ",I6)',$
                 label,rms_before[i],rms_after[i],rms_change[i],nrms[i]
       endfor
    endfor
    if ilog then begin
       close,unit
       free_lun,unit
    endif

  ; Reset the filter
    if dat_list(s_l,/reset,/no_notify) le 0 then begin
       print,'Error resetting in wlm_derive.'
       return,-1
    endif

  ; Set scale factor
    scale_factor_use = xadopt

  ; Finished
    return,1
end
