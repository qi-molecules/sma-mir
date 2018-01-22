; *************************************************************************
; PROCEDURE
;      wlm
;
; WRITTEN
;      November 8, 1998 by JMC
;
; PURPOSE
;      Interactive interface to derive/apply WLM corrections to a track
;
; INPUTS
;   USED IN READING/WRITING TRACKS AND WLM DATA
;      track       : Track number to read and process in IDL
;      table_name  : Name of table in database (eg. 'last' or 'tr3500_1')
;      table_owner : Owner of table (default: current IDL user)
;      noapply     : If set, do not apply phase corrections
;      nodrop      : If set, do not automatically drop WLM box if it is bad.
;      nowrite     : If set, do not write calibrated track to MMA
;
;   USED TO DERIVE SCALE FACTOR
;      defaults    : If set, default parameters are used throughout
;      noderive    : If set, do not derive the scale factor. Either use
;                    the default value (12 mm/K) or the value set by xfactor.
;      receiver    : Uses phases from this receiver
;                    Default: 1 (3mm receiver)  options: 1 or 2
;      sideband    : The sideband to use to establish WLM-phase correlation
;                    Default: both,  options: both, lower, or double
;      cband       : The continuum band to use establish WLM-phase correlation
;                    Default: C1,  options: both, c1, or c2
;      use_boxes   : Vector indicating which WLM boxes to use 
;      xfactor     : Scale factor to convert temperatures to mm of delay
;      linear      : Subtract linear fit from WLM values before applying 
;                    coherence corrections
;
;   SPECIAL FUNCTION
;      noise       : If set, assumes track is a special WLM test track where
;                    the WLM receivers alternate between looking at the sky
;                    and a cold load every 5 minutes.
;
;   USED IN PLOTTING/PRINTING RESULTS
;      all_plots   : Graph *all* plots
;      mklog       : If set, print out various log files describing results
;      noplot      : If set, data are not plotted at the various stages
;      postscript  : Output files sent to postscript files
;
;   USED FOR FULL PHASE CORRECTIONS (/phase must be set to use these parameters)
;      phase       : If set, apply both phase and coherence corrections.
;                    Default: Apply coherence corrections only
;      dt_bin      : Binning width (in seconds) for the WLM data before doing
;                    phase corrections
;      dt_off      : Allow DC offsets to change every dt_off seconds.
;      offjumps    : If set, fit D.C. offset whenever WLM boxes calibrated
;      offsplines  : If set, fit D.C. offsets with spline fit
;
; OUTPUT
;      Calibrated track is written to MMA with the table name t<tracknumber>_coh
;
; EXAMPLES
;      wlm                             <-- User prompted for all information
;      wlm,4847,/noderive,/defaults    <-- Process track with no intervention
;      wlm,4847,/default,sideband=lower,/noplots,x=10.0,/noderive
;
; *************************************************************************
; wlm.pro              : Script to run the WLM routines
; wlm_apply.pro        : Apply WLM corrections
; wlm_calibrate.pro    : Normalizes WLM gains 
; wlm_close.pro        : Close postscript file
; wlm_correlate.pro    : Determine scale factor from K to mm of delay using
;                        non-linear least sqaures fit.
; wlm_db_read.pro      : Read WLM data from database
; wlm_derive.pro       : Driver to derive scale factor from K to mm of delay
; wlm_fit.pro          : Fits a smooth function vs. time for 
;                        WLM calibration variables
; wlm_int_ave.pro      : Determine integration averages of two water line boxes
; wlm_mask.pro         : Masks out WLM data taken while telescopes were not
;                        on source
; wlm_open.pro         : Opens a postscript file
; wlm_phase.pro        : Fits full phase corrections to WLM data
; wlm_plot_data.pro    : All purpose plotting routine for WLM data
; wlm_print_phases.pro : Prints WLM data/phases to data file
; wlm_rec_ave.pro      : Computes average WLM value within a record
; wlm_settime.pro      : Prints time interval in string format 
; wlm_smooth.pro       : Smooths data to remove DC drifts
; wlm_xfit.pro         : Function used to derive scale factor for WLM data

pro wlm,track,table_name=table_name,table_owner=table_owner,$
        noderive=noderive,receiver=receiver,sideband=sideband,cband=cband,$
        noread=noread,noapply=noapply,nowrite=nowrite,nodrop=nodrop,$
        defaults=defaults,allplots=allplots,noplots=noplots,$
        mklog=mklog,postscript=postscript,$
        phase=phase,xfactor=xfactor,use_boxes=use_boxes,$
        offsplines=offsplines,offjumps=offjumps,$
        dt_off=dt_off, dt_bin=dt_bin,linear=linear,noise=noise

   ; Common blocks
     common global
     common data_set
     common wlm
     common plo

   ; Restrict access?
     itest = 0
     if (itest ne 0) then begin
        print,"WLM program is not available.
        print,"It is being tested by JMC"
        if (e.user_name ne 'jmc') then return
     endif

   ; BOOLEANS --- full-phase or coherence-only corrections
     ; iphase   = keyword_set(phase)
     iphase     = 0
     icoherence = not iphase
     if (iphase) then begin
        ioff_spline = keyword_set(offsplines) and keyword_set(dt_off)
        ioff_jumps  = keyword_set(offjumps) and not ioff_spline
     endif

   ; BOOLEANS - noise track?
     inoise = keyword_set(noise)

   ; BOOLEANS --- Derive scale factor
     iderive = 1 - keyword_set(noderive)

   ; BOOLEANS --- plotting and output
     ilog        = keyword_set(mklog)
     iplot       = 1 - keyword_set(noplots)
     ipostscript = keyword_set(postscript) and iplot

   ; BOOLEANS --- read/apply/write
     iapply      = 1 - keyword_set(noapply)
     idefaults   = keyword_set(defaults)
     iwrite      = 1 - keyword_set(nowrite)

   ; Set nominal scale factor (Kelvin -> millimeters of delay) 
   ; if not deriving best fit value
     if (not iderive) then scale_factor_use = 12.0D
     if keyword_set(xfactor) then scale_factor_use = double(xfactor)

   ; Set start/stop times for each WLM record (integration time = 2 seconds)
     dt_wlm = 2.0D / 3600.0D

   ; Read track from the database
     result = wlm_track_read(track,noread=noread,outroot=outroot,$
                             table_name=table_name,table_owner=table_owner,$
                             tname=tname,track_num=track_num)
     if (result ne 1) then return

   ; OUTPUT PLOT DEVICE
     if (iplot) then begin
       if (ipostscript) then begin
          wlm_open,'t' + string(track_num) + ".ps"
       endif else begin
          loadct,39,/silent
          pl.plot_device = 'x'
       endelse
     endif

   ; Read the WLM data
     result = wlm_db_read(tel_online=tel_online,dt_smooth=0.0,plot=iplot,$
                          ut_cal=ut_cal,itel_cal=itel_cal,nodrop=nodrop,$
                          mklog=mklog,outroot=outroot,tname=tname)
     if (result ne 1) then return

   ; Set which WLM boxes are OK to use
     result = wlm_choose_box(use_boxes=use_boxes,defaults=defaults)

   ; Compute gains for the WLM boxes and store calibrated WLM data in wlm_cal.
     result = wlm_calibrate(plot=iplot,outroot=outroot,mklog=mklog)
     if (result eq -1) then return

   ; Print statistics if noise track
     if (inoise) then begin
        ; Re-set options for noise tracks
          iapply  = 1
          iderive = 0
          iwrite  = 0

        ; Filter data to get gain sources
          noise_query = '"band" in ["c1"] and "gq" like "g" and "sb" like "l"'
          if (dat_list(s_l,noise_query,/reset,/no_notify) le 0) then begin
             print,"Need WLM test data available."
             return
          end

        ; Print noise statistics for the WLM boxes
          wlm_noise,iload=iload,defaults=defaults,noplots=noplots,$
                    postscript=postscript,outroot=outroot,box_noise=box_noise

        ; Compute phases before applying waterline corrections
          result = wlm_phase_stat(rms_before,coh_before,linear=linear)

        ; Set scale factor
          if not keyword_set(scale_factor_use) then scale_factor_use = 10.0
     endif

   ; Derive scale factor
     if (iderive) then begin
        result = wlm_derive3(plot=iplot,defaults=defaults,mklog=mklog,$
	    outroot=outroot,postscript=ipostscript,allplots=allplots,$
	    receiver=receiver,sideband=sideband,cband=cband,linear=linear,$
            scale_factor_init=12.0,scale_factor_use=scale_factor_use)
        if (result eq -1) then begin
           if (not keyword_set(scale_factor_use)) then begin
             printf,-1," "
             printf,-1,"Scale factor = 0"
             printf,-1,"No WLM corrections to apply"
             printf,-1,"Exiting WLM program ..."
           endif else begin
             printf,-1,"Error deriving scaling factor"
           endelse
           if dat_list(s_l,/reset,/no_notify) le 0 then $
             print,'No data after wlm_derive (check filter).'
           return
        endif
     endif

   ; Do full phase corrections, if necessary
     if (iphase) then result = wlm_phase(dt_wlm)

   ; Initialize cal structure
     npts = n_elements(wlm_cal)
     result = cal_store(s_c,init=6,max_pts_per_row=npts)

   ; Store calibrated wlm curves in cal structure
     inhid_beg = min(in.inhid)
     inhid_end = max(in.inhid)
     codes = ["all","all","all","all"]
     icodes = [1,1,1,1]
     irow = 0
     result=cal_store(s_c,'wlm',"hours","dt_wlm","antenna",inhid_beg,$
           inhid_end,codes,icodes,dt_wlm,dt_wlm,track_num,irow,/save)
     irow = 1
     result=cal_store(s_c,'wlm',"hours","wlm_times","antenna",inhid_beg,$
           inhid_end,codes,icodes,wlm_times,1,track_num,irow,/save)
     irow = 2
     result=cal_store(s_c,'wlm',"hours","ids","antenna",inhid_beg,$
           inhid_end,codes,icodes,wlm_id,1,track_num,irow,/save)
     irow = 3
     result=cal_store(s_c,'wlm',"hours","resid","antenna",inhid_beg,$
           inhid_end,codes,icodes,wlm_cal,1,track_num,irow,/save)
     irow = 4
     result=cal_store(s_c,'wlm',"hours","xfactor","antenna",inhid_beg,$
            inhid_end,codes,icodes,scale_factor_use,scale_factor_use,$
            track_num,irow,/save)
     irow = 5
     result=cal_store(s_c,'wlm',"hours","linear","antenna",inhid_beg,$
            inhid_end,codes,icodes,keyword_set(linear),keyword_set(linear),$
            track_num,irow,/save)
     result=cal_store(s_c,/transfer)

   ; Make plots
     if (iplot and iphase) then $
        make_phase_plots,tname,a,fa,wlm_cal_old,offsets,$
                         mklog=mklog,outroot=outroot

   ; Print results to a data file
     if (ilog and iphase) then begin
        ; Get gains and offsets
          offsets= compute_spline_old(ioff_spline,atime,a,$
                                time,id_data,ioff,wlm_ntel)
 
        ; Find unique times/elevations
          j = uniq(time,sort(time))

        ; WLM, RESIDUALS, GAINS, and OFFSETS
          for i = 0L, wlm_ntel-1L do begin
            ; Open output file
              output  = outroot + "_" + string(wlm_itel[i]) + ".dat"
              openw,unit,strcompress(output,/remove),/get_lun

            ; Print data
              j = where(id_data eq i,nj)
              for k = 0L, nj-1L do $
                printf,unit,format='(F10.6,4(1x,F8.4))',$
                    time[j(k)],data[j(k)],model[j(k)],data[j(k)]-model[j(k)],$
                    offsets[j(k)]

            ; Close output file
              close,unit
              free_lun,unit
          endfor
     endif

   ; Apply WLM corrections
     if (iapply) then begin
       if (not inoise) then result = dat_list(s_l,/reset,/no_notify)
; ilog = 1
; iplot = 1
       if icoherence then $
          result = cal_apply(wlm='coherence',mklog=(ilog and iplot)) $
       else $
          result = cal_apply(wlm='pha',mklog=(ilog and iplot))
       if (result eq 1 and iwrite) then begin
          if icoherence then s = "_coh_1" else s = "_pha_1"
          outtable = strcompress(string(track_num)+s,/remove)
          print,'--- Writing calibrated table (' + outtable + ') to MMA'
          time = systime(1)
          result = dbi_utable_write(outtable)
          s = wlm_settime(time)
          print,'--- Finished writing table ... time = ',s
       endif
     endif

   ; If noise track, then compute rms/phases after the fact
     if (inoise) then begin
        ; Re-filter the data
          if (dat_list(s_l,noise_query,/reset,/no_notify) le 0) then begin
             print,"Need WLM test data available."
             return
          end

        ; Compute phases after applying waterline corrections
          result = wlm_phase_stat(rms_after,coh_after,$
                         rms_before,rms_coh_before,iload,box_noise,$
                         scale_factor_use,linear=linear,/print)
     endif

   ; Reset filter
     result = dat_list(s_l,/reset,/no_notify)

   ; Close output postscript file
     if ipostscript then wlm_close

   ; Done!
     return
end
