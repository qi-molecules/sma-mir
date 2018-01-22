function gain_fit,tel_bsl,funct,dt_smooth,x_var,y_var,baselines, $
                 distinct_baselines,xs,ys,wts,yfs, $
                 xsmooth_amp=xsmooth_amp,xsmooth_pha=xsmooth_pha
;
; Gain fit for amp and phase.
;
; The data on the gain calibrators is fit by baseline or telescope
;
; parameters : tel_bsl    -- type of fit ('telescope' or 'baseline')
;                            NOT USED AT ALL is this rountine, all
;                            we care here is fitting the amp and/or phase
;                            v.s. the x_var
;              dt_smooth  -- as in gain.pro, if dt_smooth > 0, boxcar
;                            smoothing is used and this is the smoothing
;                            interval size.
;                            if dt_smooth <=0 (NEGATIVE INTEGER), polynomical
;                            fitting is used, and this is the order of the
;                            polynomical
;                            if dt_smooth =-0.5, point-to-point connect
;                            will be used.
;              x_var      -- x-ccord :'int', 'hours', el'
;                            NOT USED AT ALL currently. All we do is
;                            fitting whatever xs vs the input y_var
;              y_var      -- y-coord : fit 'amp,pha','amp','pha','complex'
;              xs         -- x_coord's
;              ys         -- amp, pha, or complex
;              baselines  -- labeling of the data rx/band/bsl/sb combination
;              distinct_baselines -- 
;              wts        -- weight for each data point (neg. excluded
;                            from fit)
;              yfs        -- fitted curve
;
; result = -1 (error), 1 (succesful)
;
; To do a gain fit for amp w/ smoothing over 2 hours:  
; eg. : result=gai_fit(tel_bsl,2.,'hours','amp',4,xs,ys,yf)
;
common global
common data_set

if e.debug then begin
    print, 'Starting gain_fit with y_var of ',y_var
endif

n_singular=0
n_baselines=n_elements(distinct_baselines)
npts=n_elements(baselines)
; Set amplitude and phases smoothing parameters
dx_amp = keyword_set(xsmooth_amp) ? xsmooth_amp : (keyword_set(dt_smooth) ? dt_smooth : 10.0)
dx_pha = keyword_set(xsmooth_pha) ? xsmooth_pha : (keyword_set(dt_smooth) ? dt_smooth : dx_amp)

for ib=0,n_baselines-1 do begin
   
  ; Debug message
   if e.debug then print," Now doing baseline ", distinct_baselines(ib)
  ; Find good baselines
   jgood=where(distinct_baselines(ib) eq baselines and wts gt 0.,ngood)
  ; Mark any bad values
   jbad=where(distinct_baselines(ib) eq baselines and wts le 0.,nbad)
   nx=(size(yfs,/dim))[0]
   if nbad gt 0 then begin
      for i = 0,nx-1 do yfs(i,jbad)=!BAD_VALUE 
   endif
  ; Skip rest of loop if there are no good data
   if (ngood eq 0) then continue
  ; Set xdata
   xdata = reform(xs(jgood),ngood)

  ; Set amplitudes/phases
   amp = reform(ys(0,jgood),ngood)
   indx = (nx gt 1) ? 1 : 0     ; Index for phases
   pha = reform(ys(indx,jgood),ngood)
  ;
  ;  amp fit
  ;
   if y_var eq 'amp,pha' or y_var eq 'amp' then begin
      
      if e.debug then print, "fitting amplitude curve "

      if (funct eq 'connect') then begin
         if e.debug then print, "Connect mode, no fitting "
         yfs(0,jgood)=ys(0,jgood)
      endif

      if (funct eq 'smooth') then begin
         if e.debug then print, "smooth with interval ", dx_amp
         yfs(0,jgood) = uti_boxcar(xdata,amp,dx_amp)
      endif

      if (funct eq 'poly') then begin
         if e.debug then print, "polyfit with order ", -dt_smooth
         order=-dt_smooth
         y_temp = make_array(ngood,/double)
;         coeff = svdfit(xdata,amp,order,YFIT=y_temp,/double)
         coeff = poly_fit(xdata, amp,order,YFIT=y_temp,/double)
         if e.debug then print, "coeffs are ",coeff
         print, "coeffs for ",distinct_baselines(ib)," are ",coeff
         yfs(0,jgood)=y_temp
      endif

   endif

  ;
  ;  pha fit
  ;
   if y_var eq 'amp,pha' or y_var eq 'pha' then begin
      if e.debug then print, "fitting phase curve "

      if (funct eq 'connect') then begin
         if e.debug then print, "Connect mode, no fitting "
         yfs(indx, jgood) = ys(indx, jgood)
      endif

      if (funct eq 'smooth') then begin
         if e.debug then print, "smooth with interval ", dx_pha
         yfs(indx,jgood) = uti_boxcar(xdata,pha,dx_pha)
      endif
      
      if (funct eq 'poly') then begin
         
         if e.debug then print, "polyfit with order ", -dt_smooth
         order=-dt_smooth
         y_temp = make_array(ngood,/double)
;         coeff = svdfit(xdata,pha,order,YFIT=y_temp,/double)
         coeff = poly_fit(xdata, pha ,order,YFIT=y_temp,/double)
         if e.debug then print, "coeffs are ",coeff
         print, "coeffs for ",distinct_baselines(ib)," are ",coeff
         yfs(indx,jgood)=y_temp
      endif

   endif

   if y_var eq 'complex' then begin
      if e.debug then print, "fitting complex curve "

      uti_conv_apc,cmp,reform(amp,ngood),reform(pha,ngood), /complex

      if (funct eq 'connect') then begin
         if e.debug then print, "Connect mode, no fitting "
         print, 'Not implemented correctly yet --- '
         print, 'Treatment for the complex-amp/pha conversion needs to be done.'
         return, -1
      endif
      
      if (funct eq 'smooth') then begin
         if e.debug then print, "smooth with interval ", dt_smooth
         y_temp = uti_boxcar(xdata,cmp,dt_smooth)
         uti_conv_apc,cmp,amp_temp,pha_temp,/amp_pha
         yfs(0,jgood) = amp_temp
         yfs(1,jgood) = pha_temp
;         yfs(0,jgood) = uti_boxcar(xdata,cmp,dt_smooth)
      endif

      if (funct eq 'poly') then begin
         print, 'Not implemented correctly yet --- '
         print, 'poly_fit.pro does not work on complex array directly.'
         return, -1
;      if e.debug then print, "polyfit with order ", -dt_smooth
;      order=-dt_smooth
;      y_temp = make_array(n_elements(js),/complex)
;      coeff = poly_fit(xs(js), cmp(0,js),order,YFIT=y_temp,/double)
;      if e.debug then print, "coeffs are ",coeff
;
;      uti_conv_apc,y_temp,amp_temp,pha_temp, $
;                   /amp_pha
;      yfs(0,js)=amp_temp
;      yfs(1,js)=pha_temp
;      if count_bad gt 0 then yfs(0,js_bad)=!BAD_VALUE 
;      if count_bad gt 0 then yfs(1,js_bad)=!BAD_VALUE 
;
      endif

   endif

endfor

return,1

end
