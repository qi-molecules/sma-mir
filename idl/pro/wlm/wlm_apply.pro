; *************************************************************************
; PROCEDURE
;      wlm_apply
;
; WRITTEN
;      November 28, 1999 by JMC
;
; PURPOSE
;      Applies WLM corrections to astronomy data
;
; INPUTS
;      /coherence  : If set, apply coherence corrections only
;      xfactor     : Scale factor from Kelvin to mm of delay
;
; OUTPUT
;      phase_corr  : Phase corrections (degrees) to apply to astronomy data
;                    Dimensions: Number of channels
;      1 -> Successful
;     -1 -> Not successful
;
; EXAMPLES
;      result = wlm_apply(/coh)
;
; *************************************************************************

function wlm_apply,phase_corr,coherence=coherence,xfactor=xfactor,mklog=mklog
  ; Common blocks
    common global
    common data_set
    common wlm

  ; Time this subroutine
    time = systime(1)

  ; Set booleans
    icoherence = keyword_set(coherence)

  ; Indicate type of corrections
    if (icoherence)     then print,"--- Applying COHERENCE corrections only"
    if (not icoherence) then print,"--- Applying FULL PHASE corrections only"

  ; Read calibration data from the cal structure
    inhid_beg = min(in.inhid)
    inhid_end = max(in.inhid)
    tel_bsl = "antenna"
    codes = ["all","all","all","all"]
    icodes = [1,1,1,1]
    r = intarr(6)
    r[0] =cal_store(s_c,'wlm',"hours","dt_wlm",tel_bsl,inhid_beg,inhid_end,$
                      codes,icodes,dt_wlm,junk,track_num,/restore)
    r[1] =cal_store(s_c,'wlm',"hours","wlm_times",tel_bsl,inhid_beg,inhid_end,$
                      codes,icodes,twlm_ave,junk,track_num,/restore)
    r[2] =cal_store(s_c,'wlm',"hours","ids",tel_bsl,inhid_beg,inhid_end,$
                      codes,icodes,ids,junk,track_num,/restore)
    r[3] =cal_store(s_c,'wlm',"hours","resid",tel_bsl,inhid_beg,inhid_end,$
                      codes,icodes,resid,junk,track_num,/restore)
    r[4] =cal_store(s_c,'wlm',"hours","xfactor",tel_bsl,inhid_beg,inhid_end,$
                       codes,icodes,xfactor,junk,track_num,/restore)
    r[5] =cal_store(s_c,'wlm',"hours","linear",tel_bsl,inhid_beg,inhid_end,$
                       codes,icodes,linear,junk,track_num,/restore)
    s = '--- Using scale factor of ' + string(xfactor,format='(F100.3)')
    print,strcompress(s)
    j = where(r ne 1,nj)
    if (nj ne 0) then begin
       printf,-1,"Could not read WLM data from CAL structure."
       return,-1
    endif

  ; Set number of frames and records
    nframes   = n_elements(pil)
    nrec_tot  = TOTAL(sp(psl).nrec)

  ; Grid the WLM data on to the astronomy data for each channel
    result = wlm_regrid(phase_corr,lambda,resid,twlm_ave,dt_wlm,ids,$
                        coherence=coherence,linear=linear)

  ; Note phase values which are still bad 
    jbad  = where(phase_corr eq !BAD_VALUE,nbad)
    jgood = where(phase_corr ne !BAD_VALUE,ngood)

  ; Convert phase corrections from kelvin to degrees
    if (ngood gt 0) then begin
       kelvin_to_deg = 360.0 / lambda(jgood) * xfactor
       phase_corr(jgood) = phase_corr(jgood) / lambda(jgood) * (360.0 * xfactor)
    endif

  ; Change bad phase corrections to zero
    if (nbad gt 0) then phase_corr(jbad) = 0.0;

  ; Print how long it took
    s = wlm_settime(time)
    print,strcompress("--- Finished setting corrections for all data ... time = " + s)

  ; Done
    return,1
end
