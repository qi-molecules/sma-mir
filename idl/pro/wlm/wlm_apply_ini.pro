; *************************************************************************
; FUNCTION
;      wlm_apply_ini
;
; WRITTEN 
;      June 15, 2000 by JMC
;
; PURPOSE
;      Compute residuals and calibrate WLM data
;
; INPUTS 
;      xfactor : Adopted scale factor
;
; OUTPUT
;      -1   Calibration failed.
;       1   Calibrated WLM data stored successfully
;
; EXAMPLES
;      result = wlm_apply_ini(baseline,rms_before,rms_after,perdiff)
; *************************************************************************
function wlm_apply_ini,plid,baseline,rms_before,rms_after,perdiff,$
                    noplot=noplot,noapply=noapply,write=write,mklog=mklog
  ; Common blocks
    common global
    common data_set
    common wlm

  ; BOOLEANS
    iapply = 1 - keyword_set(noapply)
    iwrite = keyword_set(write)

  ; Compute final residuals
    nframes = wlm_residuals(plid,baseline,rms_before,rms_after,perdiff,$
                        noplot=noplot,noapply=noapply,write=write,mklog=mklog)

  ; Initialize cal structure
    npts = n_elements(wlm_cal)
    result = cal_store(s_c,init=5,max_pts_per_row=npts)

  ; Store calibrated wlm curves in cal structure
    inhid_beg = min(in.inhid)
    inhid_end = max(in.inhid)
    codes = ["all","all","all","all"]
    icodes = [1,1,1,1]
    irow = 0
    track_num = in(0).traid
    dt_wlm = 2.0D / 3600.0D
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
           inhid_end,codes,icodes,wlm_xfactor,wlm_xfactor,$
           track_num,irow,/save)
    result=cal_store(s_c,/transfer)

  ; Apply WLM corrections
    if (iapply) then begin
      if not keyword_set(wlm_icoherence) then begin
         print,'*** Must derive water line corrections first ****'
         return,-1
      endif
      result = dat_list(s_l,/reset,/no_notify)
      if (wlm_icoherence) then begin
         result = cal_apply(wlm='coherence',mklog=mklog)
      endif else begin
         result = cal_apply(wlm='pha',mklog=mklog)
      endelse
      if result eq 1 and iwrite then begin
         if (wlm_icoherence) then s = "_coh_1" else s = "_pha_1"
         outtable = strcompress(string(in(0).traid)+s,/remove)
         print,'--- Writing calibrated table (' + outtable + ') to MMA'
         result = dbi_utable_write(outtable)
         print,'--- Finished writing table'
      endif
    endif

  ; Finished
    return,nframes
end
