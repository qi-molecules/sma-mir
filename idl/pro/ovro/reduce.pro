pro reduce,strack,noread=noread,coh_min=coh_min,div_coh=div_coh,$
           noadd_fluxes=noadd_fluxes,noapply_flux=noapply_flux,$
           ntrim_max=ntrim_max,defaults=defaults,nologs=nologs,noplots=noplots,$
           comments_only=comments_only
   ; Common blocks
     common global
     common data_set
     common plo

   ; Directory for the logs
     dir_logs = (e.campuslogin eq 'ovro') ? '/mm/logs/' : './'

   ; Only write data to database if we are working on inyo
     savedb_ok = (getenv("DSQUERY") eq 'SYBASE') ? 1 : 0

   ; If user is creating logs in an interactive mode, then choose an editor
     if not keyword_set(defaults) and not keyword_set(nologs) then $
        result = reduce_editor()

   ; If the program exited ungracefully during a batch processing, the 
   ; plot devices may be stuck in "null" mode. reset them to "x"
     j = where(strlowcase(pl.plot_device) ne 'x',nj)
     if (nj gt 0) then pl[j].plot_device = 'x'

   ; If "defaults" or noplots is set, turn off plotting.
     plot_device = pl.plot_device
     doplots = (not keyword_set(defaults) and not keyword_set(noplots)) ? 1 : 0
     if not doplots then pl.plot_device  = 'null'

   ; Read track number
     track = reduce_readtrack(strack,noread=noread)
     if (track le 0) then return

   ; Open track log
     unit = 0
     if not keyword_set(nologs) then begin
        ; Set file name
          subdir = (e.campuslogin eq 'ovro') ? 'redlog/' : './'
          logfile = strcompress(dir_logs + subdir + 'ovro_redlog_' + string(track),/remove)

        ; See if it exists already. If it does, then prompt the user to see if they
        ; want to overwrite the file.
          if file_test(logfile) eq 1 then begin
             print,""
             print,"WARNING: The data reduction log for this track already exists!"
             print,""
             print,"Select option:"
             print,"   (1) Exit program (default)"
             print,"   (2) Continue, but do not generate a new reduction log"
             print,"   (3) Continue  and overwrite existing reduction log"
             print,format='($,"Choice? ")'
             input = ''
             read,input
             input = strtrim(input,2)
             print,""
             if input eq '2' then begin
                nologs = 1
             endif else if input ne '3' then begin
                print,""
                print,"*** ENDING PROGRAM ***"
                print,""
                return
             endif
          endif

        ; Open logfile if needed
          if not keyword_set(nologs) then openw,unit,logfile,/get_lun
     endif

   ; Read track
     if not keyword_set(noread) then begin
        print,""
        print,format='(%"Reading track %d from the database...")',track
        result = dbi_track_read(track)
     endif else begin
        print,""
        print,"Using track stored in memory"
     endelse

   ; Always filter out COBRA data
     result = dat_filter(s_f,'"band" not like "w"',/reset,/no)

   ; Print header
     print,""
     printlog,format='(%"                 OVRO Data reduction log for Track %d")',track,unit=unit
     printlog," ",unit=unit

   ; Print track summary
     result = reduce_summary(project=project,title=title,obsdate=obsdate,$
          wvp=wvp,config=config,piproject=piproject,unit=unit,defaults=defaults)

   ; Flag shadowed baselines
     printlog,"",unit=unit
     nshadow = dat_shadow()
     printlog,format='(%"%d rows removed because of shadowing")',$
         nshadow,/onlylog,unit=unit
     printlog,"",unit=unit

   ; Apply new soid if needed
     printlog,"                     ******************************",unit=unit
     printlog,"                     *      BASELINE SOLUTION     *",unit=unit
     printlog,"                     ******************************",unit=unit
     result = reduce_soid(defaults=defaults,unit=unit)

   ; Always filter out wt=0 data
     result = dat_filter(s_f,'"wt" gt "0"',/no)

   ; Estimate atmospheric opacity
     printlog,"                     ******************************",unit=unit
     printlog,"                     *     TAU/TSYS/COHERENCES    *",unit=unit
     printlog,"                     ******************************",unit=unit
     result = reduce_tau(avetsys_zen=avetsys_zen,avefreq=avefreq,unit=unit)

   ; Print coherences
     result = reduce_coh(unit=unit)

   ; Plot cobra data
     print,"                     ******************************"
     print,"                     *        COBRA  DATA         *"
     print,"                     ******************************"
     result = reduce_cobra(/plotpdf,unit=unit,doplots=doplots)
     result = dat_filter(s_f,'"band" not like "w"',/reset,/no)
     if (doplots) then begin
        j = where(strlowcase(pl.plot_device) eq 'ps',nj)
        if (nj gt 0) then pl[j].plot_device = 'x'
     endif

   ; Plot continuum data for general inspection
     if doplots and not keyword_set(comments_only) then begin
        result = dat_select(s_s,/reset,/no)
        printlog,""
        printlog,"*** PLOTTING CONTINUUM AMPLITUDES VS. INTEGRATION NUMBER ***"
        printlog,""
        x_var = 'int'
        y_var = 'amp,coh'
        cont_data = 'int'
        frame_vars = 'rec,band'
        color_vars = 'blcd'
        symbol_vars = ''
        frames_per_page = 1
        result = plo_cont(x_var,y_var,cont_data,frame_vars,color_vars,$
                          symbol_vars,frames_per_page)
     endif

   ; Plot tsys for general inspection - exclude noise source
     if doplots and not keyword_set(comments_only) then begin
        result = dat_select(s_s,'"source" ne "noise"',/reset,/no)
        print,""
        print,"*******************************************"
        print,"*   PLOTTING TSYS VS. INTEGRATION NUMBER  *"
        print,"*******************************************"
        print,""
        distinct_bands = uti_distinct(sp(pss).iband,nbands)
        tel = uti_distinct([c.tel1,c.tel2],ntel)
        for i=0,nbands-1 do begin
            band = c.band(distinct_bands[i])
            print,""
            print,"*******************************************"
            print,'Plotting Tsys vs Integration for band ',band
            print,"*******************************************"
            print,""
            result = dat_list(s_l,'"band" eq "'+band+'"',/reset,/no)
            x_var = 'int'
            y_var = 'tssb'
            frame_var = 'blcd'
            color_var = 'blcd'
            symbol_var = ''
            nbsl = ntel*(ntel-1)/2 
            ncol=fix(sqrt(nbsl)+1)
            nrow=fix(nbsl/ncol+1)
            frames_per_page = nrow*ncol
            result = plo_var(x_var,y_var,frame_var=frame_var,color_var=color_var,$
                          symbol_var=symbol_var,frames_per_page=frames_per_page)
        endfor
        result = dat_select(s_s,/reset,/no)
     endif

   ; Perform flux calibration on receiver at a time
     if not keyword_set(comments_only) then begin
        result = dat_select(s_s,/reset,/no)
        printlog,"                     ******************************",unit=unit
        printlog,"                     *        SCALE FACTORS       *",unit=unit
        printlog,"                     ******************************",unit=unit
        iapply = keyword_set(noapply_flux) ? 0 : 1
        iadd = not keyword_set(noadd_fluxes) and savedb_ok ? 1 : 0
        good_frames = ""
        cal_type = 0
        for i=0, n_elements(c.rec)-1 do begin
           printf,-1,'Calibrating fluxes for Rx' + c.rec[i] + '...'
           cal_type = 1
           if (reduce_fluxcal(c.rec[i],coh=coh_min,div_coh=div_coh,frames=frames,$
               cal_sources=cal_sources,cal_primary=cal_primary,defaults=defaults,$
               apply=iapply,method=method,unit=unit) ne -1) then $
               good_frames = [good_frames,frames]
               if (cal_primary eq 0) then cal_type = 0
        endfor
     endif

   ; Measure fluxes
     if (n_elements(good_frames) gt 1 and not keyword_set(comments_only)) then begin
        result = dat_select(s_s,/reset,/no)
        printlog,"                     ******************************",unit=unit
        printlog,"                     *       MEASURED FLUXES      *",unit=unit
        printlog,"                     ******************************",unit=unit
        good_frames = good_frames[1:n_elements(good_frames)-1]
        result = reduce_fluxmeasure(coh=coh_min,div_coh=div_coh,method=method,$
                     cal_sources=cal_sources,cal_primary=cal_type,add=iadd,$
                     frames=good_frames,defaults=defaults,unit=unit)
     endif

   ; Passband
     if not keyword_set(comments_only) then begin
        result = dat_select(s_s,/reset,/no)
        printlog,"                     ******************************",unit=unit
        printlog,"                     *    PASSBAND CALIBRATION    *",unit=unit
        printlog,"                     ******************************",unit=unit
        com = 'Notes:'
        com = [com,'   (a) It is recommended that you apply the noise source for ']
        com = [com,'       high spectral resolution galactic data, but not ']
        com = [com,'       low spectral extragalactic data.']
        com = [com,'   (b) The noise source, if selected, will be applied/derived first']
        com = [com,'       with no smoothing.']
        com = [com,'   (c) Any quasar passband will be derived/applied last.']
        com = [com,'']
        print,transpose(com)
        for i=0, n_elements(c.rec)-1 do begin
           redo = 1
           while (redo) do begin
              result = reduce_pass(c.rec[i],ntrim_max=ntrim_max,$
                          redo=redo,defaults=defaults,unit=unit,doplots=doplots)
              if (redo) then begin
                 print,""
                 print,"*** Redo passband calibration for Rx ",c.rec[i]
              endif
           endwhile
        endfor
     endif

   ; Enter comments for passband calibration
     if not keyword_set(defaults) and not keyword_set(nologs) and $
        not keyword_set(comments_only) then $
        result = reduce_comments(unit,"passband calibration")

   ; Derive gains one receiver at a time
     if not keyword_set(comments_only) then begin
        printlog,"                     ******************************",unit=unit
        printlog,"                     *      GAIN CALIBRATION      *",unit=unit
        printlog,"                     ******************************",unit=unit
        ampsig = [0.0]
        phasig = [0.0]
        gcal = ""
        for i=0, n_elements(c.rec)-1 do begin
           printf,-1,'Performing gain/phase calibration for Rx' + c.rec[i] + '...'
           redo = 1
           while (redo) do begin
              result = dat_select(s_s,/reset,/no)
              result = reduce_gain(c.rec[i],smoothing=smoothing,cal=cal,$
                             redo=redo,defaults=defaults,unit=unit)
              if (redo) then begin
                 print,""
                 print,"*** Redo gain calibration for Rx ",c.rec[i]
              endif else if (cal[0] ne "") then $
                 gcal = [gcal,cal]
           endwhile
           if (result ne -1) then begin
               result = reduce_stats_gain(c.rec[i],sources=cal,$
                          smoothing=smoothing,sigamp=sigamp,sigpha=sigpha,$
                          defaults=defaults,unit=unit)
               if result ne -1 then begin
                  ampsig = [ampsig,sigamp]
                  phasig = [phasig,sigpha]
               endif
           endif
        endfor
     endif

   ; Plot continuum data for general inspection
     if doplots and not keyword_set(comments_only) then begin
        com = '"gq" like "g"'
        if (n_elements(gcal) gt 1) then com = com + ' and ('
        for i=1,n_elements(gcal)-1 do begin
           if (i gt 1) then com = com + ' or '
           com = com + ' "source" eq "' + gcal[i] + '"'
        endfor
        if (n_elements(gcal) gt 1) then com = com + ')'
        result = dat_select(s_s,com,/reset,/no)
        print,""
        print,"*** PLOTTING CONTINUUM AMPLITUDES/PHASES VS. INTEGRATION ***"
        print,"*** PHASES SHOULD BE CENTERED AROUND 0.0                 ***"
        print,"*** AMPLITUDES SHOULD BE CENTERED ON CALIBRATOR FLUX     ***"
        print,""
        x_var = 'int'
        y_var = 'amp,pha,coh'
        cont_data = 'int'
        frame_vars = 'rec,band'
        color_vars = 'blcd'
        symbol_vars = ''
        frames_per_page = 1
        result = plo_cont(x_var,y_var,cont_data,frame_vars,color_vars,$
                          symbol_vars,frames_per_page)
     endif

   ; Enter comments for gain calibration
     if not keyword_set(defaults) and not keyword_set(nologs) and $
        not keyword_set(comments_only) then $
        result = reduce_comments(unit,"gain calibration")

   ; Plot calibrated spectra
     if doplots and not keyword_set(comments_only) then begin
        print,""
        print,"****************************************"
        print,"*  PLOT CALIBRATED SPECTRAL LINE DATA  *"
        print,"****************************************"
        print,""
        result = dat_select(s_s,/reset)
        result = reduce_specplot(ntrim_max=ntrim_max)
     endif

   ; Plot calibrated continuum data
     if doplots and not keyword_set(comments_only) then begin
        print,""
        print,"************************************"
        print,"*  PLOT CALIBRATED CONTINUUM DATA  *"
        print,"************************************"
        print,""
        result = dat_select(s_s,/reset)
        result = map_driver()
     endif

   ; Major kludge. If defaults is set, reset plot_device
     pl[0:n_elements(plot_device)-1].plot_device  = plot_device

   ; Get track quality/status
     if not keyword_set(defaults) and not keyword_set(nologs) then begin
        printlog,"                     ******************************",unit=unit
        printlog,"                     *        TRACK QUALITY       *",unit=unit
        printlog,"                     ******************************",unit=unit
        result = reduce_quality(track,track_quality,track_status,$
              unit=unit,savedb_ok=savedb_ok)
     endif

   ; Enter comments for the overall track and enter them into the database
     if not keyword_set(defaults) and not keyword_set(nologs) then begin
        result = reduce_comments(unit,"the track overall",$
                          savedb_ok=savedb_ok,track=track,/enter)
     endif

   ; Create data reduction log - this will be emailed to the project PI
     if not keyword_set(nologs) then begin
        printlog,""
        printlog,"Generating track log..."
        subdir = (e.campuslogin eq 'ovro') ? 'tralog/' : './'
        tralog = strcompress(dir_logs + subdir + 'ovro_tralog_' + string(track),/remove)
        result = rep_tra(track,fn=tralog)
        printlog,""
     endif

   ; Send email
     if not keyword_set(defaults) and not keyword_set(nologs) then begin
        result = reduce_email(track,project,title,obsdate,$
                         track_quality,track_status,unit=unit,dir_logs=dir_logs)
     endif else if keyword_set(unit) then begin
        close,unit
        free_lun,unit
     endif

   ; Ask use if they want to write table to MMA
     if not keyword_set(defaults) then begin
        printlog,""
        printlog,"To write the data to MMA, enter a table name. Otherwise, hit return to exit."
        input = ''
        read,prompt='Choice ? ',input
        if strtrim(input,2) eq '' then begin
           printlog,"Reduced data not written to MMA"
        endif else begin
           result = dbi_utable_write(input)
           if (result eq -1) then begin
              printlog,"Error writing data to mma"
           endif
           printlog,'Data written to mma as table "',input,'"'
        endelse
        printlog,""
     endif

   ; Write out summary statistics if on inyo
     if (e.campuslogin eq 'ovro' and not keyword_set(nologs)) then begin
       ; Set average amp/pha sigma - first element is a dummy value
         nsig = n_elements(ampsig)-1
         ampsig = nsig gt 0 ? string(format='(%"%6.2f")',total(ampsig[1:nsig])/nsig) : '  null'
         phasig = nsig gt 0 ? string(format='(%"%6.2f")',total(phasig[1:nsig])/nsig) : '  null'

       ; Print results for this track
         result = reduce_stats(track,config,piproject,project,$
               obsdate,track_quality,track_status,avefreq,wvp,$
               avetsys_zen,ampsig,phasig)
         if (result eq -1) then print,"*** ERROR GENERATING SUMMARY FILE ***"
     endif

   ; Done
     if (doplots) then begin
        printlog,""
        printlog,format='(%"Finished data reduction script for track %d")',track
        printlog,""
        result = dat_select(s_s,/reset)
     endif
end
