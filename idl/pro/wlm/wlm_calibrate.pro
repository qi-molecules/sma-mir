; *************************************************************************
; FUNCTION
;      wlm_calibrate
;
; WRITTEN 
;      July 23, 1998 by JMC
;
; PURPOSE
;      Solves for the gains on each WLM box by comparing the changes in WLM
;      temperatures from one integration to the next.
;
; INPUTS 
;      NOTE          : WLM data are assumed to be in the WLM common block
;      max_time_diff : Maximum time difference [minutes] permitted between 
;                      scans for computing the gains
;      plot          : Plots data at various stages of analysis
;      /mklog        : If set, print out diagnostics to data file
;      outroot       : Output root name for data file if mklog is set.
;
; OUTPUT
;     Gains stored in wlm_gains
;      1  -> sucessful
;     -1  -> not sucessful
;
; EXAMPLES
;      result = wlm_calibrate(use=[1,2,3,4,5])
;
; *************************************************************************

function wlm_calibrate,max_time_diff=max_time_diff,plot=plot,$
                      outroot=outroot,mklog=mklog,use=use,postscript=postscript
  ; Common blocks
    common global
    common data_set
    common wlm

  ; Time this routine
    print,"--- Calibrating WLM boxes ... "
    time = systime(1)

  ; Set interactive keywords
    iplot = keyword_set(plot)
    ilog  = keyword_set(mklog)
    ipostscript  = keyword_set(postscript)

  ; Output root name
    if not keyword_set(outroot) then $
      root = "temp" $
    else $
      root = outroot

  ; Max time difference
    if not keyword_set(max_time_diff) then $
       dt_max = 1e35 $
    else $
       dt_max = max_time_diff
    dt_max = dt_max / 60.0

  ; Determine the maximum number of rows in the matrix. The "+ 1L"
  ; is from the constraint that the average gain is 1.0 by definition.
    nscans = n_elements(in)
    max_rows = (nscans - 1L) * wlm_ntel * (wlm_ntel-1L) / 2L + 1L

  ; Allocate memory for the matrix and data storage
    phi    = make_array(wlm_ntel,max_rows,/double)
    idata1 = make_array(max_rows,/long)
    idata2 = make_array(max_rows,/long)
    data1  = make_array(max_rows,/double)
    data2  = make_array(max_rows,/double)

  ; Initialize row counter
    nrow = 0L

  ; Loop over all baseline combinations.
    for itel1 = 0L,       wlm_ntel-2L do begin
    for itel2 = itel1+1L, wlm_ntel-1L do begin
       ; Compute integration averages for these telescopes
         result = wlm_int_ave(wlm_itel[itel1],xdata1,wlm_itel[itel2],xdata2,$
                              avetime,nscans)

       ; Compute the change in the WLM temperature from one integration
       ; to the next, and store these values. Only use scans that are
       ; within DT_MAX of each other.
         ngood = 0L
         i1 = where(avetime ne !BAD_VALUE,ni1)
         if (ni1 gt 1) then begin
           ishift   = shift(i1,-1)
           timediff = avetime(i1[ishift]) - avetime(i1)
           good = where(timediff[0L:ni1-1L] le dt_max,ngood)
         endif
         for i = 0L, ngood-2L do begin
            ; Compute difference in WLM temperatures
              dt1 = xdata1(i1[good(i+1)]) - xdata1(i1[good(i)])
              dt2 = xdata2(i1[good(i+1)]) - xdata2(i1[good(i)])

            ; Add elements to matrix/vectors
              data1[nrow]     =  dt1
              data2[nrow]     = -dt2
              idata1[nrow]    = itel1
              idata2[nrow]    = itel2
              phi[itel1,nrow] = data1[nrow]
              phi[itel2,nrow] = data2[nrow]

            ; Increment row counter
              nrow = nrow + 1L
              if (nrow eq max_rows) then begin
                 print,"Never here"
                 stop
              endif
         endfor
    endfor
    endfor

  ; Truncate the data
    phi    = phi[*,0L:nrow-1L]
    data1  = data1[0L:nrow-1L]
    data2  = data2[0L:nrow-1L]
    idata1 = idata1[0L:nrow-1L]
    idata2 = idata2[0L:nrow-1L]

  ; Take transpose of data matrix
    phi_t = transpose(phi)

  ; Multiply data transpose by data and invert
    MI = invert(phi_t ## phi)

  ; Set constraint to average gains to unity
    N = make_array(wlm_ntel,1,/double,value=1.0)
    Z = [1.0D*wlm_ntel]

  ; Take transpose
    NT = transpose(N)
    ZT = transpose(Z)

  ; Solve for parameters
    wlm_gains = Z ## invert(N ## MI ## NT) ## (N ## MI)

  ; Compute residuals
    R  = data1*wlm_gains(idata1) + data2*wlm_gains(idata2)
    RT = transpose(R)

  ; Compute uncertainties
    sigma_o = (RT ## R + Z ## invert(N ## MI ## NT) ## Z) / (nrow-wlm_ntel+1)
    variance = sigma_o[0] * (MI - MI ## NT ## invert(N ## MI ## NT) ## N ## MI)
    i = indgen(wlm_ntel) * (wlm_ntel+1)
    k = where(variance[i] gt 0.0,nk)
    wlm_egains = dblarr(wlm_ntel)
    if (nk gt 0) then wlm_egains[k] = sqrt(variance[i(k)])

  ; Summarize results
    for i = 0, ilog do begin
       unit = -1
       if (i eq 1) then begin
         output = strcompress(root + '_gains.dat',/remove)
         openw,unit,output,/get_lun
       endif
       printf,unit,format='("    # ANTENNA       GAIN          EGAIN")'
       printf,unit,format='("    # -------   -----------    ----------")'
       for j = 0L, wlm_ntel-1L do $
         printf,unit,format='(6x,I4,2F15.5)',wlm_itel(j),wlm_gains(j),wlm_egains(j)
       if (i eq 1) then begin
         close,unit
         free_lun,unit
       endif
    endfor

  ; Multiply raw data by gains and store in the wlm_cal
    wlm_cal = make_array(n_elements(wlm_raw),/float,value=!BAD_VALUE)
    for i = 0L, wlm_ntel-1 do begin
       j = where(wlm_id eq wlm_itel(i) and wlm_raw ne !BAD_VALUE,nj)
       if (nj gt 0) then wlm_cal(j) = wlm_raw(j)*wlm_gains(i)
    endfor

  ; Load color table for plotting
    if iplot then color = 0.6*bytscl((indgen(wlm_ntel+1) mod 16)+1)+64.

  ; Make a roughly equivalent plot to the above analysis. First, compute the 
  ; average WLM temperature for each integration. I do *NOT* use this to 
  ; derive the gains since one box may be offline for part of the
  ; track, but I want a plot showing that roughly shows the correct gains.
    val = dblarr(wlm_ntel,nscans)
    for i=0L,wlm_ntel-1L do begin 
       result = wlm_int_ave(wlm_itel[i],xdata1,/single)
       val[i,*] = xdata1
    endfor

  ; Compute average for each integration
    ave = make_array(nscans,/double,value=!BAD_VALUE)
    for i=0L, nscans-1L do begin
       j = where(val[*,i] gt !BAD_VALUE,nj)
       if (nj gt 0) then ave[i] = total(val[j,i]) / nj
    endfor

  ; Subtract adjacent scans 
    diff = make_array(wlm_ntel,nscans-1L,/double,val=!BAD_VALUE)
    x = [0.0]
    y = [0.0]
    yfit = [0.0]
    ids = [0]
    for i=0L, wlm_ntel-1L do begin
        z = val[i,*]
        j = where(z ne !BAD_VALUE,nj)
        if (nj gt 1) then begin
           z = z[j]
           zdiff = (z - shift(z,-1))[0:nj-2]
           xdiff = (ave[j] - shift(ave[j],-1))[0:nj-2]
           isort = sort(xdiff)
           x   = [x,xdiff(isort)]
           y   = [y,zdiff(isort)]
           yfit= [y,xdiff(isort)/wlm_gains[i]]
           ids = [ids,replicate(wlm_itel[i],nj-1)]
        endif
    endfor

  ; Plot data
    if iplot then begin
       if ipostscript then wlm_open,strcompress(out + '_gains.ps')
       x   = x[1:n_elements(x)-1]
       y   = y[1:n_elements(y)-1]
       yfit= yfit[1:n_elements(y)-1]
       ids = ids[1:n_elements(ids)-1]
       y = reform(y,1,n_elements(y))
       yfit = reform(yfit,1,n_elements(yfit))
       wts = replicate(1,n_elements(y))
       blabel = strcompress ('Gains for ' + $
            ' [Track ' + string(in(pil[0]).traid) + ']')
       a = findgen(16) * (!DPI*2/16.)
       usersym,0.75*cos(a),0.75*sin(a),/fill
       result = wlm_plot_data(x,y,wts,ids,$
                    "Average", ["WLM"],/plot,psym=[8],yfit=yfit,$
                    m_options="cspne",blabel=blabel,plid)
       if ipostscript then wlm_close
    endif

  ; Finished!
    s = wlm_settime(time)
    print,strcompress("--- Finished calibrating WLM boxes ... time = " + s)
    return,1
end
