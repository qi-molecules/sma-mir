; *************************************************************************
; FUNCTION
;      wlm_phase
;
; WRITTEN
;      November 28, 1999 by JMC
;
; PURPOSE
;      Makes full phase corrections
;
; INPUTS
;      Needs further work
;
; OUTPUT
;      -1   Failed.
;       1   Successful
;
; *************************************************************************
function set_splines,ispline,ijumps,time,dt,atime,itel_cal,ut_cal,$
                  id_data1,id_data2,wlm_itel,id_param1,id_param2,nparam
   ; Find min/max time
     ndata = n_elements(time)
     ntel  = n_elements(wlm_itel)
     tmin  = min(time)
     tmax  = max(time)
     time_range = tmax - tmin
 
   ; Determine number of parameters to fit
     if (ispline) then begin
       nparam_per_tel = long(time_range/dt)+1L + (time_range mod dt gt 0)
       nparam = nparam_per_tel * ntel
       id_param1 = lindgen(nparam) mod ntel
       id_param2 = id_param1
       atime = replicate(tmin,ntel)
       for i = 1L, nparam_per_tel-1L do $
         atime=[atime,replicate(tmin+dt*i,ntel)]
       if (nparam_per_tel eq 2L) then atime[ntel + lindgen(ntel)] = tmax
       atime = (atime < tmax)
     endif else if (ijumps) then begin
        nparam = 0L
        atime = 0.0
        id_param1 = replicate(-1,ndata)
        id_param2 = id_param1
        for i = 0L, ntel-1L do begin
           j = where(itel_cal eq wlm_itel[i],nj)
           l = where(ut_cal(j) gt time(j[0]))
           for k = MAX([0L,l[0]-1L]), nj-1L do begin
              m = where((id_data1 eq i or id_data2 eq i) and $
                        time ge ut_cal[j(k)],nm)
              if (nm gt 0L) then begin
                q = where(id_data1 eq i,nq)
                if (nq gt 0) then id_param1[q] = nparam
                q = where(id_datid_data,nq)
                if (nq gt 0) then id_param2[q] = nparam
                nparam = nparam + 1L
              endif
           endfor
        endfor
     endif else begin
        id_param1 = id_data1
        id_param2 = id_data2
        atime = 0.0
        nparam = ntel
     endelse
 
   ; Done
     return,1
end

function compute_spline_old,ispline,atime,params,iparams,time,id_data,ntel
   if ispline then begin
      output = dblarr(n_elements(id_data))
      for i = 0L, ntel-1L do begin
         j = where(id_data eq i,nj)
         k = where(iparams eq i,nk)
         if (nj gt 0 and nk gt 0) then $
           output(j) = spl_interp(atime(k),params(k),$
                                 spl_init(atime(k),params(k)),time(j))
       endfor
   endif else begin
      output = params(iparams)
   endelse
   return,output
end

function compute_spline_new,ispline,atime,time,params,iparams1,id_data1,$
                                                      iparams2,id_data2,ntel
   if ispline then begin
      output = dblarr(n_elements(id_data1))
      for i1 = 0L, ntel-2L do begin
         j1 = where(id_data1 eq i1,nj1)
         k1 = where(iparams1 eq i1,nk1)
         if (nj1 gt 0 and nk1 gt 0) then begin
           for i2 = i1+1L, ntel-1L do begin
              j2 = where(id_data2(j1) eq i2,nj2)
              k2 = where(iparams2     eq i2,nk2)
              if (nj2 gt 0 and nk2 gt 0) then begin
                x = params(k1)-params(k2)
                j = j1(j2)
                l = where(x ne 0.0,nl)
                if (nl gt 0) then $
                  output(j) = spl_interp(atime(k1),x,$
                                         spl_init(atime(k1),x),time(j)) $
                else $
                  output(j) = 0.0
              endif
           endfor
         endif
      endfor
   endif else begin
      output = params(iparams1) - params(iparams2)
   endelse
   return,output
end

pro iprint,myfunct,p,iter,fnorm,FUNCTARGS=fa,parinfo=parinfo,quiet=quiet
   ; Print time
     printf,-1,strcompress("Iteration " + string(iter) + " --- " + systime())

   ; Get model fit
     residuals = wlm_afit(p,_EXTRA=fa,model=model)*fa.err

   ; Plot residuals, if necessary
     if (fa.iplot) then begin
       plot,fa.time,residuals,psym=3,xtitle='Time',ytitle='Residuals',$
            ymargin=[4,0]
       oplot,fa.time,dblarr(n_elements(model)),color=64
     endif

   ; OFFSETS only
     nparams = n_elements(p)
     if nparams le fa.ntel*8L then begin
        n = nparams / fa.ntel
        print,"     Tel     Offsets -->"
        for i = 0L, fa.ntel-1L do begin
           a = p[lindgen(n) * fa.ntel + i]
           print,format='($,"   ",i4)',fa.wlm_itel[i]
           for j = 0L, n-1L do print,format='($,1x,F8.3)',a[j]
           print,""
        endfor
     endif
end

pro make_phase_plots,tname,a,fa,wlm_cal_old,offsets,mklog=mklog,outroot=outroot
   ; Common blocks
     common wlm
     common global
     common data_set

   ; Raw data
     blabel = strcompress("Track " + tname)
     ydata  = fltarr(1,n_elements(wlm_cal))
     ydata[0,*] = wlm_cal_old
     result=wlm_plot_data(wlm_times,ydata,wlm_id,wlm_id,$
                          "Time  [hours]",["WLM"],/plot,$
                          m_options="cspne",blabel=blabel,nframes_max=6,$
                          psym=[3],/expand)

   ; Prepare for DC OFFSETS
     times = [0.0]
     y     = [0.0]
     ymod  = [0.0]
     bsl = ['']
     for i = 0L, wlm_ntel-2L do begin
        tel1 = where(wlm_id eq wlm_itel[i])
        x1 = wlm_cal_old(tel1)
        o1 = offsets(tel1)
        for j = i+1L, wlm_ntel-1L do begin
           tel2 = where(wlm_id eq wlm_itel[j])
           x2   = wlm_cal_old(tel2)
           o2   = offsets(tel2)
           k    = where(x1 gt !BAD_VALUE and x2 gt !BAD_VALUE,nk)
           if (nk gt 0L) then begin
              times = [times,wlm_times(k)]
              z  = x1(k) - x2(k)
              y  = [y,x1(k) - x2(k)]
              ymod = [ymod,o1(k)-o2(k)]
              s = strcompress(string(wlm_itel[i])+ "-" +string(wlm_itel[j]),/re)
              bsl = [bsl,replicate(s,nk)]
           endif
        endfor
     endfor

   ; RESIDUALS
     n = n_elements(y) - 1L
     ydata = fltarr(2,n)
     yfit  = fltarr(2,n)
     ydata[0,*] = y[1:n]
     ydata[1,*] = y[1:n]-ymod[1:n]
     yfit[0,*]  = ymod[1:n]
     yfit[1,*]  = 0.0
     result=wlm_plot_data(times[1:n],ydata,replicate(1,n),bsl[1:n],$
                          "Time  [hours]",["Offsets","Res"],/plot,yfit=yfit,$
                          m_options="cspne",blabel=blabel,nframes_max=15,$
                          psym=[3,3])
end

function wlm_phasefit,a,time=time,data=data,err=err,$
         atime=atime,ioff_spline=ioff_spline,ntel=ntel, $
         id_data1=id_data1,id_data2=id_data2,ioff1=ioff1,ioff2=ioff2, $
         model=model

   ; Sort the UT_CAL data in order of increasing time
     j = sort(ut_cal)
     ut_cal = ut_cal(j)
     itel_cal = itel_cal(j)

   ; Set offsets
     off1 = compute_spline_old(ioff_spline,atime,a,ioff1,time,id_data1,ntel)
     off2 = compute_spline_old(ioff_spline,atime,a,ioff2,time,id_data2,ntel)

   ; Compute model
     model = off1 - off2

   ; Compute model
;    model = compute_spline_new(ioff_spline,atime,time,a,ioff1,id_data1,$
;                                                        ioff2,id_data2,ntel)
; plot,model-model1
; print,mean(model),mean(model1)

   ; Compute residual
     residual = (data-model)/err

   ; Return residuals
     return,residual
end

function wlm_phase,dt_wlm
   ; Set time steps for full phase corrections.
   ; DT_OFF is entered in minutes and converted to hours.
   ; DT_BIN in entered in seconds, converted to hours, and then
   ; converted to a "half-width" time interval.
     if not keyword_set(dt_off) then dt_off=0.0D
     if not keyword_set(dt_bin) then dt_bin=0.0D
     if dt_off lt 0.0 or dt_bin lt 0.0 then begin
       print,'DT must >= 0.0'
       return,-1
     endif
     dt_off = dt_off / 60.0D
     dt_bin = dt_bin / 3600.0D / 2.0D

   ; Define astronomy stop times based on integration time stamps.
     tstart_astro = in.dhrs
     tstop_astro  = tstart_astro + (in.rinteg / 3600.)

   ; Set start/stop times for each WLM record (integration time = dt_wlm)
     wtimes_start = wlm_times - (dt_wlm / 2.0)
     wtimes_stop  = wtimes_start + dt_wlm

   ; Initialize data
     ntimes = n_elements(wlm_times)
     id_data1 = [0]
     id_data2 = [0]
     data = [0.0]
     time = [0.0]
     err  = [0.0] 
     id_all  = replicate(-1,n_elements(wlm_cal))
     id_time = replicate(-1,n_elements(wlm_cal))
     wlm_used = intarr(ntimes)
     err_o = 0.007 * sqrt(2.0)

   ; Loop over all scans and store WLM data that were taken while the 
   ; telescopes were on-source 
     igood = where(c.source[in.isource] ne "noise",ngood)
     for ii = 0L,ngood-1L do begin
       ; Integration
         i = igood(ii)

       ; Loop over each baseline
         for i1 = 0L, wlm_ntel-2L do begin
            k1 = where(wlm_id eq wlm_itel[i1],nk)
            for i2 = i1+1, wlm_ntel-1L do begin
               k2 = where(wlm_id eq wlm_itel[i2],nk)
               l = where(wlm_cal(k1) ne !BAD_VALUE and $
                         wlm_cal(k2) ne !BAD_VALUE and $
                         wtimes_start le tstop_astro[i] and $
                         wtimes_stop  ge tstart_astro[i],nl)
               if (nl gt 0) then begin
                 ; Store data
                   if (dt_bin eq 0.0) then begin
                      id_data1 = [id_data1,replicate(i1,nl)]
                      id_data2 = [id_data2,replicate(i2,nl)]
                      data = [data,wlm_cal[k1(l)]-wlm_cal[k2(l)]]
                      time = [time,wlm_times[l]]
                      err  = [err,replicate(err_o,nl)]
                   endif else begin
                      used = intarr(nl)
                      wt   = wlm_times[l]
                      wd   = wlm_cal[k1(l)]-wlm_cal[k2(l)]
                      m    = 0L
                      while (m lt nl) do begin
                         n = where(not used and abs(wt-wt[m]) le dt_bin,nn)
                         if (nn gt 0) then begin
                           id_data1 = [id_data1,i1]
                           id_data2 = [id_data2,i2]
                           data = [data,TOTAL(wd[n])/nn]
                           time = [time,TOTAL(wt[n])/nn]
                           err  = [err,err_o/sqrt(nn)]
                           used(n) = 1
                           m = n[nn-1L]-1L
                         endif
                         m = m + 1L
                      endwhile
                   endelse
                   wlm_used[l]    = 1
                   id_all[k1(l)]  = i1
                   id_time[k1(l)] = l
                   id_all[k2(l)]  = i2
                   id_time[k2(l)] = l
               endif
            endfor
         endfor
     endfor

   ; Remove dummy index
     ndata = n_elements(data) - 1L
     id_data1 = id_data1[1L:ndata]
     id_data2 = id_data2[1L:ndata]
     data = data[1L:ndata]
     time = time[1L:ndata]
     err  = err[1L:ndata]
   ; Determine number of OFFSETS to fit for
     result = set_splines(ioff_spline,ioff_jumps,time,dt_off,atime,$
                          itel_cal,ut_cal,id_data1,id_data2,wlm_itel,$
                          ioff1,ioff2,nparams)

   ; Make sure everything looks OK
     j = where(ioff1 lt 0 or ioff2 lt 0,nbad)
     if (nbad gt 0) or (n_elements(ioff1) ne n_elements(ioff2)) then begin
       print,'Error setting OFFSET paremeters'
       stop
       return,-1
     endif

   ; Are there enough data points
     if (nparams ge ndata) then begin
        printf,-1,"Ndata   = ",ndata
        printf,-1,"Nparams = ",nparams
        printf,-1,"Not enough data points to constrain the fit'
        printf,-1,"Try dt <= ",long(time_range/ndata*60.0)," minutes"
        stop
        return,-1
     endif

   ; Set structure that can constrain fitted parameters, if desired
     parinfo = replicate({value:0.D, fixed:0, limited:[0,0],$
                          limits:[0.D,0], step:0.0D, tied:''}, nparams)

   ; First offset is always zero
     parinfo[0].fixed = 1

   ; Set structure to hold data
     fa = {time:time, data:data, err:err, user_device:pl.plot_device, $
           iplot:iplot, atime:atime, ioff_spline:ioff_spline, $
           id_data1:id_data1, id_data2:id_data2, ioff1:ioff1, ioff2:ioff2, $
           ntel:wlm_ntel, wlm_itel:wlm_itel}

   ; Print summary
     s = "--- Fitting " + string(ndata) + " data points with " + $
         string(nparams) + " parameters"
     soff = "single value for track per box"
     if (ioff_spline) then $
        soff = "spline" $
     else if (ioff_jumps) then $
        soff = "new value per calibration"
     print,strcompress(s)
     print,format='("   ",I6," OFFSET parameters --- ",a)',nparams,soff
     print," "

   ; Get solution
     extime = systime(1)
     ainit = dblarr(nparams)
     a = mpfit('wlm_afit',ainit,FUNCTARGS=fa,parinfo=parinfo,auto=1,nprint=1,$
               iterproc='iprint',perror=sigmaa,niter=niter,maxiter=200,/nocatch)
     residuals = wlm_phasefit(a,_EXTRA=fa,model=model)*err
     extime = systime(1)-extime

   ; Print solution
     print," "
     print," "
     print," "
     print,"  *** SOLUTION CONVERGED ***"
     print," "
     print," "
     print," "
     print,format='("Track    ",I4)',tname
     print,format='("Niter    ",I4)',niter
     stime = strcompress(string(extime,format='(F100.1)'),/remove)
     print,format='("Time     ",a," seconds")',stime
     sdata = strcompress(string(ndata,format='(I6)'),/remove)
     print,format='("N data   ",a)',sdata
     print,format='("N params ",I4," --- ",a)',nparams,soff
     print," "
     if (nparams eq wlm_ntel) then begin
       print,"        Antenna       Offset   Uncertainty"
       for i = 0L, wlm_ntel-1L do $
        print,format='(i15,"      ",F7.3,"   ",F9.5)',wlm_itel[i],a[i],sigmaa[i]
     endif

   ; Compute DC offsets for the full resolution WLM data
     jall = where(id_all ge 0)
     time_all = wlm_times(id_time(jall))
     result = set_splines(ioff_spline,ioff_jumps,time_all,dt_off,atime,$
                   itel_cal,ut_cal,id_all[jall],id_all[jall],wlm_itel,$
                   ioff_all,junk)
     offsets = dblarr(n_elements(wlm_cal))
     offsets(jall) = compute_spline_old(ioff_spline,atime,a,ioff_all,time_all,$
                              id_all[jall],wlm_ntel)

   ; Compute offset subtracted curves
     wlm_cal_old = wlm_cal
     j = where(wlm_wts lt 0,nj)
     if (nj gt 0) then wlm_cal_old[j] = !BAD_VALUE
     wlm_cal[*] = !BAD_VALUE
     wlm_cal[jall] = wlm_cal_old[jall] - offsets(jall)
end
