; *************************************************************************
; testing
; FUNCTION
;       wlm_db_read
;
; PURPOSE
;       Get wlm data from db given the timerange of the current track 
;       or a given time range and read them into idl variables.
;
; CATEGORY
;       WLM routines
;
; INPUTS
;       plot      : If set, then the WLM data are plotted
;       defaults  : If set, then defaults are used for all options without
;                   prompting the user. 
;       dt_smooth : Smoothing time (minutes) for WLM calibration variables
;                   If dtsmooth < 0, then use average value
;                   Default value: average value in track
;       boxcar    : If present, uses boxcar smoothing on calibration variables
;                   Default: Hanning smoothings
;       tname     : Option - name of astronomy table 
;
; OUTPUTS
;       wlm_ntel       The number of telescopes present with WLM data
;       wlm_itel       A [wlm_ntel] vector indicating which antennas are present
;       wlm_raw        Vector of WLM data [wlm_ntel * N_data/per_ant]
;       wlm_id         Telescope ID numbers for wlm_raw
;       wlm_times      UT hours since c.ref_time for each WLM sample
;       wlm_wts        Flag useable WLM data. > 0 -> good;  <= 0 -> bad
;
; COMMON BLOCKS
;       wlm, global, data_set
;
; MODIFICATIONS
;     (1) 97-05-07 by KBM
;           Created from original read prog
;     (2) 98-08-06 by JMC
;           Modified ut_day calculation to be more general
;     (3) 98-10-20 by JMC
;           Removed temp common block and renamed variables
;     (4) 99-11-26 by JMC
;           Added wlm_mask routine
;
; CALLING SEQUENCE
;     result = wlm_db_read(/plot,/defaults) 
;
; *************************************************************************

function wlm_db_read,plid,plot=plot,defaults=defaults,dt_smooth=dt_smooth,$
                     boxcar=boxcar,tel_online=tel_online,nodrop=nodrop,$
                     postscript=postscript,mklog=mklog,outroot=outroot,$
                     writecal=writecal,lmc=lmc,con=con,ut_cal=ut_cal,$
                     itel_cal=itel_cal,tname=tname
   ; Common blocks
     common wlm
     common global
     common data_set

   ; Time this routine
     time = systime(1)
     if keyword_set(tname) then $
       print,strcompress("--- Reading WLM data for track " + tname + " ...")

   ; Boolean values
     ilog        = keyword_set(mklog)
     iplot       = keyword_set(plot)
     idefaults   = keyword_set(defaults)
     ipostscript = keyword_set(postscript)

   ; Output root name 
     out = 'temp'
     if keyword_set(outroot) then out = outroot

   ; Guesstimate as to which antennas were online if needed
     if not keyword_set(tel_online) then begin 
        itel1 = uti_distinct(c.tel1(bl(pbl).itel1),/many)
        itel2 = uti_distinct(c.tel2(bl(pbl).itel2),/many)
        tel_online = uti_distinct([itel1,itel2])
        print,format='($,"--- Antenna online  :")'
        for i = 0L, n_elements(tel_online)-1L do $
           print,format='($," ",I2)',tel_online[i]
        print," "
     endif

   ; Smoothing width for calibration values.
   ; -1 -> Use average value
     smooth = -1.0
     if keyword_set(dt_smooth) then smooth = dt_smooth

   ; Set start/stop times for astronomy data
   ; These refer to the start of the integration, so I add in the
   ; integration length below.
     start_string = c.ut[in[min(pil)].iut]
     stop_string  = c.ut[in[max(pil)].iut]

   ; Set date stings to extract the WLM data from the database
   ; I add 2*integration length to ut_stop to get that last bit
   ; of WLM data. For the CAL SQL call, I add +/- 24 hours to get 
   ; extra calibration/interpolation data for the WLM boxes. This is especially
   ; important for the most recent data since the boxes are calibrated once
   ; a day now.
     ut_start = 'datediff(ss,"Jan  1 1997 12:00:00:000AM","' + $
                 start_string + '")'
     ut_stop  = 'datediff(ss,"Jan  1 1997 12:00:00:000AM",dateadd(ss,' + $
            string(long(2*in[max(pil)].rinteg)) + ',"' + stop_string + '"))'
     sget_wlm  = " where wlm# between "     + ut_start + " and " + ut_stop
     stime_wlm = "datediff(ms,'" + c.ref_time  + $
                 "',dateadd(ss,wlm#,'Jan  1 1997 12:00:00:000AM'))"

     ut_start = 'datediff(ss,"Jan  1 1997 12:00:00:000AM",dateadd(hh,-24.0,' + $
            '"' + start_string + '"))'
     ut_stop  = 'datediff(ss,"Jan  1 1997 12:00:00:000AM",dateadd(hh,24.0,' + $
            '"' + stop_string + '"))'
     sget_wlmcal  = " where wlm_cal# between " + ut_start + " and " + ut_stop
     stime_wlmcal = "datediff(ms,'" + c.ref_time  + $
                    "',dateadd(ss,wlm_cal#,'Jan  1 1997 12:00:00:000AM'))"

   ; Determine which WLM database to use
     result = dbi_sql_submit('select dstart= ' + ut_start)
     If (n_elements(result) ne 5) then begin
        printf,-1,"Error reading WLM id number"
        return,-1
     endif
     if (long(result[2]) le 60527374L) then $
       prefix = 'mm9798' $
     else if (long(result[2]) le 81201214L) then $
       prefix = 'mm9899' $
     else if (long(result[2]) le 110881829) then $
       prefix = 'mm9900' $
     else if (long(result[2]) le 144581437) then $
       prefix = 'mm0001' $
     else if (long(result[2]) le 175311247) then $
       prefix = 'mm0102' $
     else $
       prefix = 'mm'
     database_wlm    = strcompress(prefix + '..WLMS',/remove)
     database_wlmcal = strcompress(prefix + '..WLM_CALS',/remove)

   ; Read data from WLM table within the appropriate UT range
     com = "select c1,lmc1,c2,lmc2,c3,lmc3,c4,lmc4,c5,lmc5,c6,lmc6," + stime_wlm
     result = dbi_sql_submit(com + " from " + database_wlm+sget_wlm,/no_notify)
     n = n_elements(result)
     if (n le 100) then begin
       printf,-1,'WLM data not available for this track'
       return,-1
     endif

   ; Set number of rows and data points
     nstart  = 2L
     nend    = n - 3L
     nrows   = nend - nstart + 1L
     npts    = nrows * !NTEL

   ; Read WLM table
     temp = {val: fltarr(!NTEL*2), time:0L}
     s = replicate(temp,nrows)
     reads,result[nstart:nend],s

   ; Break the data structure into vectors, and flag bad data to !BAD_VALUE
     j    = 2L*indgen(!NTEL)
     con  = reform(s.val(j),nrows*!NTEL)
     lmc  = reform(s.val(j+1L),nrows*!NTEL)
     jbad = where(con eq -1000.0 or lmc eq -1000.0,nbad)
     if (nbad gt 0) then begin
       con(jbad) = !BAD_VALUE
       lmc(jbad) = !BAD_VALUE
     endif
     jgood = where(lmc gt !BAD_VALUE and con gt !BAD_VALUE,ngood)
     if (ngood lt 100) then begin
        printf,-1,'Not enough WLM data for this track'
        return,-1
     endif
     wlm_id = (lindgen(npts) mod 6) + 1L
     wlm_times = double(s.time) / (1000.0 * 3600.0)

   ; Determine which WLM boxes have valid data. If tel_online is set, then
   ; any WLM boxes not in tel_online are flagged as bad.
     wlm_itel = uti_distinct(wlm_id(jgood),/many)
     wlm_itel = wlm_itel[uniq(wlm_itel,sort(wlm_itel))]
     if keyword_set(tel_online) then begin
        for i = 0, n_elements(wlm_itel)-1L do begin
           j = where(wlm_itel[i] eq tel_online,nj)
           if (nj eq 0) then wlm_itel[i] = 0
        endfor
     endif

   ; Block out antenna 1 and antenna 6 wlm box
     if not keyword_set(nodrop) then begin
       if (in(pil(0)).traid ge 4818 and in(pil(0)).traid le 5020) then begin
          j = where(wlm_itel eq 6,nj)
          if (nj gt 0) then begin
            wlm_itel(j) = 0
            print,'*********************************************************** '
            printf,-1,"--- WARNING: BLOCKING OUT WLM BOX 6"
            print,'*********************************************************** '
          endif
       endif else if in(pil(0)).traid ge 4600 and $
                     in(pil(0)).traid lt 4818 then begin
          j = where(wlm_itel eq 1,nj)
          if (nj gt 0) then begin
            wlm_itel(j) = 0
            print,'*********************************************************** '
            printf,-1,"--- WARNING: BLOCKING OUT WLM BOX 1"
            print,'*********************************************************** '
          endif
       endif
     endif
     j = where(wlm_itel gt 0L,wlm_ntel)
     if (wlm_ntel lt 2L) then begin
       printf,-1,'Error: not enough antennas/WLM boxes online'
       return,-1
     endif
     wlm_itel = wlm_itel(j)

   ; Add constant offsets to WLM box 5 for Track 4847
     if (in[0].traid eq 4847) then begin
       ; Print warning
         print,'***********************************************************'
         print,'--- WARNING: MODIFYING WLM VALUES FOR TRACK 4847          '
         print,'***********************************************************'

       ; Select all ant 5 data
         j = where(wlm_id eq 5)
         jbad = where(lmc eq !BAD_VALUE,nbad)

       ; Add constant offsets
         k = where(wlm_times gt 4.00)
         lmc(j(k)) = lmc(j(k)) + 1.70
         k = where(wlm_times gt 6.760833)
         lmc(j(k)) = lmc(j(k)) - 1.30

       ; Keep bad data
         if (nbad gt 0) then lmc(jbad) = !BAD_VALUE
     endif

   ; Read data from WLM_CAL table
     com = "select lgain,lmcgain,cgain,ctrec,lmctrec,tel," + stime_wlmcal
     result = dbi_sql_submit(com + " from " + database_wlmcal + sget_wlmcal,$
                             /no_notify)
     n = n_elements(result)
   ; Set number of rows
     nstart = 2L
     nend   = n - 3L
     nrows  = nend - nstart + 1L
     if (nrows lt 1) then begin
       printf,-1,'Could not read WLM calibration data'
       return,-1
     endif

   ; Read table
     temp = {lgain: 0.0, lmcgain: 0.0, cgain: 0.0, ctrec: 0.0, lmctrec: 0.0,$
             telcal: 0, time: 0L}
     s = replicate(temp,nrows)
     reads,result[nstart:nend],s
 
   ; All telescopes must have calibration data
     for i = 0L, wlm_ntel-1L do begin
       j = where(wlm_itel[i] eq s.telcal,nj)
       if (nj eq 0) then begin
         print,'No calibration data for telescope ',wlm_itel[i]
         return,-1
       endif
     endfor

   ; Convert UT dates to hours since reference time
     ut_cal = double(s.time) / (1000.0 * 3600.0)
     itel_cal = s.telcal
   ; Write the calibration data
     if keyword_set(writecal) then begin
        ; Open output file
          output = strcompress(out + "_cal.dat",/remove)
          openw,unit,output,/get_lun

        ; Print header
          printf,unit,"# UT [hours]   Tel   L Gain  LMC Gain  Con Gain Con Trec    LMC Trec"
          printf,unit,"#     (1)      (2)     (3)     (4)       (5)       (6)        (7)   "

        ; Print one row per line
          for i = 0L, n_elements(ut_cal)-1L do begin
             printf,unit,format='(F12.6,1x,I4,5F10.5)',$
               ut_cal[i], s[i].telcal, s[i].lgain, s[i].lmcgain, $
               s[i].cgain, s[i].ctrec, s[i].lmctrec
          endfor
 
        ; Close output file
          close,unit
          free_lun,unit
     endif

   ; Open postscript file
     if ipostscript then wlm_open,strcompress(out + '_receiver.ps')

   ; Plot the gain calibration factors and allow the user to set any
   ; time variable parameters
     iquit = 0
     ilgain   = 0
     icgain   = 1
     ilmcgain = 2
     ictrec   = 3
     ilmctrec = 4
     n = 0
     iplot_cont = iplot
     ; The following line disables plotting the calibration data
     iplot_cont = 0
     choice = ' '
     while (not iquit and n lt 5) do begin
        case n of 
          ilgain  : begin
                      y = s.lgain
                      ylabel = 'L GAIN'
                      output = out + "_lgain.dat"
                    end
          icgain  : begin
                      y = s.cgain
                      ylabel = 'C GAIN'
                      output = out + "_cgain.dat"
                    end
          ilmcgain: begin
                      y = s.lmcgain
                      ylabel = 'L-C GAIN'
                      output = out + "_lmcgain.dat"
                    end
          ictrec  : begin
                      y = s.ctrec
                      ylabel = 'C TREC'
                      output = out + "_ctrec.dat"
                    end
          ilmctrec: begin
                      y = s.lmctrec
                      ylabel = 'L-C TREC'
                      output = out + "_lmctrec.dat"
                    end
          else: iquit = 1
        endcase
        result = wlm_fit(ut_cal,y,ysmooth,wlm_itel,s.telcal,smooth,$
                         ylabel,plot=iplot_cont,boxcar=boxcar)
        nstart = n
        n = n + 1
        if ilog then begin
           openw,unit,strcompress(output,/remove),/get_lun
           printf,unit,"#  I  ITEL       UT            Y           YFIT        Y-YFIT"
           for i = 0L, n_elements(y)-1L do begin
              j = where(long(s[i].telcal) eq wlm_itel,nj)
              if (nj gt 0) then $
                printf,unit,format='(2I4,1x,F13.7,3(1x,F12.4))',$
                   i+1,long(s[i].telcal),ut_cal[i]-wlm_times(0),$
                   y[i],ysmooth[i],y[i]-ysmooth[i]
           endfor
           close,unit
           free_lun,unit
        endif
        if (iplot_cont and idefaults) then begin
           printf,-1,' '
           printf,-1,'Enter new smoothing width:'
           printf,-1,'    a -> Use average value'
           printf,-1,'    n -> Next variable (default)'
           printf,-1,'    x -> Do not plot remaining receiver parameters
           printf,-1,' time -> Smoothing width in minutes'
           printf,-1,format='($,a)','Choice ?'
           read,choice
           if keyword_set(choice) then begin
              choice = strcompress(strupcase(choice),/remove_all)
              case choice[0] of
                'X' : iplot_cont = 0
                'A' : begin
                       smooth = -1
                       n = n - 1
                      end
                'N' :
                else: begin
                       smooth = ABS(float(choice))
                       n = n - 1
                      end
              endcase
           endif
        endif
        if (n ne nstart) then begin
           case nstart of 
             ilgain   : lgain   = ysmooth
             icgain   : cgain   = ysmooth
             ilmcgain : lmcgain = ysmooth
             ictrec   : ctrec   = ysmooth
             ilmctrec : lmctrec = ysmooth
          endcase
        endif
     endwhile
     if (iplot_cont and idefaults) then begin
       print," "
       print,"Finished plotting variables"
       print," "
     endif

   ; Close postscript file
     if ipostscript then wlm_close

   ; Allocate memory 
     wlm_raw = make_array(n_elements(lmc),/float,value=!BAD_VALUE)
     wlm_wts = intarr(n_elements(lmc))

   ; Define the WLM structure
     for i = 0L, wlm_ntel-1L do begin
        ; Find ID numbers for WLM variables
          j  = where(wlm_id eq wlm_itel[i])
          k  = where(lmc(j) gt !BAD_VALUE,nk)
          jk = j(k)

        ; Find ID numbers for WLM_CAL variables
          l = where(s.telcal eq wlm_itel[i])
          m = uniq(ut_cal[l])
          lm = l(m)

        ; Determine receiver variables as a function of time
          wlmtime  = wlm_times[k]
          caltime  = ut_cal[lm]
          xcgain   = uti_interp(cgain[lm],caltime,wlmtime)
          xctrec   = uti_interp(ctrec[lm],caltime,wlmtime)
          xlgain   = uti_interp(lgain[lm],caltime,wlmtime)
          xlmctrec = uti_interp(lmctrec[lm],caltime,wlmtime)
          xlmcgain = uti_interp(lmcgain[lm],caltime,wlmtime)

        ; Set weights and compute "raw" WLM data in Kelvin
          ; wlm_wts[jk] = wlm_itel[i]
          wlm_wts[jk] = 1
          wlm_raw[jk] = (lmc[jk]-xlmcgain*(con[jk]/xcgain-xctrec))/xlgain $
                             - xlmctrec

        ; Print to log file
          if (ilog) then begin
             output = out + "_wlm_" + string(wlm_itel[i]) + ".dat"
             openw,unit,strcompress(output,/remove),/get_lun
             printf,unit,"#     ID         TIME           WLM"
             for l = 0L,nk-1L do $
                printf,unit,format='(I8,2(1x,F13.7))',$
                   l+1,wlmtime(l)-wlm_times(0),wlm_raw(j(k(l)))
             close,unit
             free_lun,unit
          endif
     endfor

   ; Flag large LMC values
;    jbad = where(abs(wlm_raw) ge 1000.0,nbad)
;    if (nbad gt 0) then begin
;       wlm_raw(jbad) = !BAD_VALUE
;       wlm_wts(jbad) = 0
;    endif

   ; Plot data
     if iplot then begin
        if ipostscript then wlm_open,strcompress(out + '_wlm.ps')
        ydata = reform(transpose(wlm_raw),1,n_elements(wlm_raw))
        blabel = strcompress ('Raw WLM data [K] vs Time ' + $
             ' [Track ' + string(in(pil[0]).traid) + ']')
        result = wlm_plot_data(wlm_times,ydata,wlm_wts,$
                     wlm_id,"Time (hours)", ["WLM"],plot=plot,$
                     m_options="cspne",blabel=blabel,/expand,plid)
        if ipostscript then wlm_close
     endif

   ; Done
     s = wlm_settime(time)
     print,strcompress("--- Read WLM data successfully ... time = " + s)
     return,1
end
