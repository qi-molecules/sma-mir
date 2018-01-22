function pass_fit,tel_bsl,funct,dch_smooth,x_var,y_var,baselines, $
                 distinct_baselines,xs,ys,wts,pt_first,pt_npts,yfs,$
                 npoly=npoly, delay=delay, icursor=icursor, svd=svd
;
; Gain fit for amp and phase.
;
; The data on the gain calibrators is fit by baseline or telescope
;
; parameters : tel_bsl    -- type of fit ('telescope' or 'baseline')
;              dch_smooth  -- smoothing number of channels
;              x_var      -- x-ccord :'int' or 'hours'
;              y_var      -- y-coord : fit 'amp,pha','amp','pha','complex'
;              xs         -- x_coord's
;              ys         -- amp, pha, or complex
;              wts        -- weight for each data point (neg. excluded
;                            from fit)
;              yfs        -- fitted curve
;
; result = -1 (error), 1 (succesful)
;
; To do a gain fit for amp w/ smoothing over 2 hours:  
; eg. : result=pass_fit(tel_bsl,2.,'hours','amp',4,xs,ys,yf)
;
common global
common data_set

;
; Check paramters
;
if tel_bsl ne 'baseline' and tel_bsl ne 'telescope' then begin
  print,'tel_bsl: '+tel_bsl+' not recognized.'
  return, -1
endif
if funct ne 'boxcar' and funct ne 'poly' then begin
  print,'funct: '+funct+' not recognized.'
  return, -1
endif
if funct eq 'poly' then begin
  if n_elements(npoly) ne 1 then begin
    print, 'npoly invalid or not defined'
    return, -1
  endif
;  if npoly ne 0 then begin
;    print, 'npoly ne 0 not implemented yet'
;    return, -1
;  endif
endif
if y_var ne 'amp,pha' and y_var ne 'amp' and y_var ne 'pha' $
  and y_var ne 'complex' then begin
  print,'y_var: '+y_var+' not recognized'
  return, -1
endif

;
; set local paramters
;
n_singular=0
n_baselines=n_elements(distinct_baselines)
npts=n_elements(baselines)
wt = wts
negidx = where(wts lt 0.0, n_neg)
if n_neg gt 0 then wt[negidx] = 0.0
if total(wt) eq 0.0 then begin
  print,'Error! total(wt) = 0'
  return, -1
endif

if keyword_set(delay) then begin
   print, 'Setting delay'
; note here baselines are actually frames
   for ib=0,n_baselines-1 do begin
      js=min(where(distinct_baselines(ib) eq baselines,count))
      jx=max(where(distinct_baselines(ib) eq baselines,count))
      all_npts=pt_first[jx]+pt_npts[jx]-pt_first[js]
      col=0
      npoly=1
      x_int=xs[pt_first[js]:pt_first[jx]+pt_npts[jx]-1L] 
      y_int=reform(ys[col,pt_first[js]:pt_first[jx]+pt_npts[jx]-1L], $
        all_npts)

      nf=long(max([1,dch_smooth]))
      nch = n_elements(y_int)
      y_smo = fltarr(nch)
;      ntrim = keyword_set(ntrim_max)? (nch gt 2*ntrim_max+nf ? ntrim_max : 0) : 0
      ntrim=0
      pha_min=0. & freq_min=0.
      pha_max=0. & freq_max=0.
      nwrap=0.
      case icursor of
         0: yfs(col,pt_first[js]:pt_first[jx]+pt_npts[jx]-1L)=0
         1: begin
            phaa=(delay[4]*360.+delay[3]-delay[1])/(delay[2]-delay[0])
            phab=delay[1]-phaa*delay[0]
            print,phaa
            y_smo=(phaa*x_int+phab) mod 360.
            iy=where(y_smo ge 180., count)
            if count gt 0 then y_smo(iy)=y_smo(iy)-360.
            iy=where(y_smo lt -180., count)
            if count gt 0 then y_smo(iy)=y_smo(iy)+360.
            yfs(col,pt_first[js]:pt_first[jx]+pt_npts[jx]-1L)=y_smo
         end
         2: begin
            parts=strsplit(distinct_baselines(ib),/extract)
            if tel_bsl eq 'baseline' then begin
               parts=strsplit(parts[0],'-',/extract)
               idtel=where(delay(0,*) eq parts[0],count)
               phaa1=delay[1,idtel]
;phab1=delay[2,idtel]
;print,delay[*,idtel]
               idtel=where(delay(0,*) eq parts[1],count)
               phaa2=delay[1,idtel]
;phab2=delay[2,idtel]
;print,delay[*,idtel]
               phaa=phaa1-phaa2
;phab=phab1-phab2
            endif else begin
               idtel=where(delay(0,*) eq parts[0],count)
               phaa=delay[1,idtel]
            endelse

            print,phaa

;            y_smo=(phaa[0]*x_int+phab[0]) mod 360.
            y_smo=(phaa[0]*x_int) mod 360.
            iy=where(y_smo ge 180., count)
            if count gt 0 then y_smo(iy)=y_smo(iy)-360.
            iy=where(y_smo lt -180., count)
            if count gt 0 then y_smo(iy)=y_smo(iy)+360.
            yfs(col,pt_first[js]:pt_first[jx]+pt_npts[jx]-1L)=y_smo   
         end
      endcase
   endfor
   return, 1
endif

;
; derive passband curves
;
  for ib=0,n_baselines-1 do begin
    js=max(where(distinct_baselines(ib) eq baselines,count))
;
;  amp fit
;
    if y_var eq 'amp,pha' or y_var eq 'amp' then begin
;
        x_int=xs[pt_first[js]:pt_first[js]+pt_npts[js]-1L]
        y_int=reform(ys[0,pt_first[js]:pt_first[js]+pt_npts[js]-1L],pt_npts[js])

    ;
    ; apply funct
    ;
     case funct of 
     'boxcar' : $
       begin
;
; boxcar smooth it in channel #. However, if the smoothing box contains more
; channels then the spectrum, then just remove the median. The median is used
; instead of the mean to guard against bad channels at the edge of the 
; spectrometer.
;
        nf=long(max([1,dch_smooth]))
        nch = n_elements(y_int)
        y_smo = fltarr(nch)
;        ntrim = keyword_set(ntrim_max)? (nch gt 2*ntrim_max+nf ?
;        ntrim_max : 0) : 0
        ntrim=0
        ch1 = ntrim
        ch2 = nch-ntrim-1
        if dch_smooth ge nch then $
           y_smo[ch1:ch2] = median(y_int[ch1:ch2]) $
        else begin
           if (nf mod 2) ne 1 then nf=nf+1
           filter=make_array(nf,/float,value=(1./nf))
           y_smo[ch1:ch2]=convol(y_int[ch1:ch2],filter,/edge_truncate)
        endelse
        if (ntrim gt 0) then begin
           y_smo[0:ch1-1] = y_smo[ch1]
           y_smo[ch2+1:nch-1] = y_smo[ch2]
        endif
        yfs(0,pt_first[js]:pt_first[js]+pt_npts[js]-1L)=y_smo
       end
     'poly' : $
            begin
             case npoly of
               0 : yfs(0,pt_first[js]:pt_first[js]+pt_npts[js]-1L) $
                     = total(y_int*wt)/total(wt) 
               else: begin
                   nch = n_elements(y_int)
                   y_smo = fltarr(nch)
;                   ntrim = keyword_set(ntrim_max)? (nch gt
;                   2*ntrim_max ? ntrim_max : 0) : 0
                   ntrim=0
                   ch1 = ntrim
                   ch2 = nch-ntrim-1
                   if not keyword_set(svd) then begin
                      coeff = poly_fit(x_int[ch1:ch2],y_int[ch1:ch2],npoly,YFIT=y_temp,/double)
                   endif else begin
                      coeff=svdfit(x_int[ch1:ch2],y_int[ch1:ch2],npoly+1,YFIT=y_temp,/double,status=status) ; in svdfit, the order is the degree of polynomial+1
                   endelse
                   y_smo[ch1:ch2]=y_temp
                   if (ntrim gt 0) then begin
                      y_smo[0:ch1-1] = poly(x_int[0:ch1-1],coeff)
                      y_smo[ch2+1:nch-1] = poly(x_int[ch2+1:nch-1],coeff)
                   endif  
                   yfs(0,pt_first[js]:pt_first[js]+pt_npts[js]-1L)=y_smo
                   end   
             endcase
           end
        endcase  
    endif
;
;  pha fit
;
    if y_var eq 'amp,pha' or y_var eq 'pha' then begin
;
        x_int=xs[pt_first[js]:pt_first[js]+pt_npts[js]-1L] 
       if y_var eq 'amp,pha' then col=1
       if y_var eq 'pha' then col=0               
       y_int=reform(ys[col,pt_first[js]:pt_first[js]+pt_npts[js]-1L], $
                     pt_npts[js])
;        if y_var eq 'amp,pha' then $                
;          y_int=reform(ys[1,pt_first[js]:pt_first[js]+pt_npts[js]-1L], $
;                pt_npts[js])
;        if y_var eq 'pha' then $ 
;          y_int=reform(ys[0,pt_first[js]:pt_first[js]+pt_npts[js]-1L], $
;                pt_npts[js])
      ;
      ; apply funct
      ;
      case funct of
      'boxcar' : $
        begin 
;
; boxcar smooth it in channel #. However, if the smoothing box contains more
; channels then the spectrum, then just remove the median. The median is used
; instead of the mean to guard against bad channels at the edge of the 
; spectrometer.
;
        nf=max([1,dch_smooth])
        nch = n_elements(y_int)
        y_smo = fltarr(nch)
;        ntrim = keyword_set(ntrim_max)? (nch gt 2*ntrim_max+nf ? ntrim_max : 0) : 0
        ntrim=0
        ch1 = ntrim
        ch2 = nch-ntrim-1
        if dch_smooth ge n_elements(y_int) then $
           y_smo[ch1:ch2] = median(y_int) $
        else begin
           if (nf mod 2) ne 1 then nf=nf+1
           filter=make_array(nf,/float,value=(1./nf))
           y_smo[ch1:ch2]=convol(y_int[ch1:ch2],filter,/edge_truncate)
        endelse
        if (ntrim gt 0) then begin
           y_smo[0:ch1-1] = y_smo[ch1]
           y_smo[ch2+1:nch-1] = y_smo[ch2]
        endif
        if y_var eq 'amp,pha' then begin
          yfs(1,pt_first[js]:pt_first[js]+pt_npts[js]-1L)=y_smo
        endif
        if y_var eq 'pha' then begin
          yfs(0,pt_first[js]:pt_first[js]+pt_npts[js]-1L)=y_smo
        endif
       end
      'poly': $
           begin
             case npoly of
               0 : begin
                   yfs(col,pt_first[js]:pt_first[js]+pt_npts[js]-1L) $
                     =  total(y_int*wt)/total(wt)
                   end
               else: begin
                   nch = n_elements(y_int)
                   y_smo = fltarr(nch)
;                   ntrim = keyword_set(ntrim_max)? (nch gt
;                   2*ntrim_max ? ntrim_max : 0) : 0
                   ntrim=0
                   ch1 = ntrim
                   ch2 = nch-ntrim-1
                   if not keyword_set(svd) then begin
                      coeff = poly_fit(x_int[ch1:ch2],y_int[ch1:ch2],npoly,YFIT=y_temp,/double)
                   endif else begin
                      coeff=svdfit(x_int[ch1:ch2],y_int[ch1:ch2],npoly+1,YFIT=y_temp,/double,status=status) ; in svdfit, the order is the degree of polynomial+1
                   endelse
                   y_smo[ch1:ch2]=y_temp
                   if (ntrim gt 0) then begin
                      y_smo[0:ch1-1] = poly(x_int[0:ch1-1],coeff)
                      y_smo[ch2+1:nch-1] = poly(x_int[ch2+1:nch-1],coeff)
                   endif  
                   yfs(col,pt_first[js]:pt_first[js]+pt_npts[js]-1L)=y_smo
                   end                     
             endcase
           end
        endcase
    endif
   if y_var eq 'complex' then begin
     uti_conv_apc,cmp,reform(ys(0,pt_first[js]:pt_first[js]+pt_npts[js]-1L), $
        pt_npts[js]),reform(ys(1,pt_first[js]:pt_first[js]+pt_npts[js]-1L),pt_npts[js]), $
                /complex
;
     x_int=xs(pt_first[js]:pt_first[js]+pt_npts[js]-1L) 
     real_int=float(cmp)
     imag_int=imaginary(cmp)
    ;
    ; apply funct
    ;
    case funct of
     'boxcar' : $
       begin
;
; boxcar smooth it in channel #
;
     nf=max([1,dch_smooth])
     if (nf mod 2) ne 1 then nf=nf+1
     filter=make_array(nf,/float,value=(1./nf))
     real_smo=convol(real_int,filter,/edge_truncate)
     imag_smo=convol(imag_int,filter,/edge_truncate)
     uti_conv_apc,complex(real_smo,imag_smo),amp_smo,pha_smo, $
                /amp_pha
     result=uti_pha_unwrap(pha_smo)
     yfs(0,pt_first[js]:pt_first[js]+pt_npts[js]-1L)=amp_smo & yfs(1,pt_first[js]:pt_first[js]+pt_npts[js]-1L)=pha_smo
     end
    'poly' : $
           begin
             case npoly of
               0 : begin
                  real_smo = total(real_int*wt)/total(wt)
                  imag_smo = total(imag_int*wt)/total(wt)
     uti_conv_apc,complex(real_smo,imag_smo),amp_smo,pha_smo, $
                /amp_pha
     result=uti_pha_unwrap(pha_smo)
     yfs(0,pt_first[js]:pt_first[js]+pt_npts[js]-1L)=amp_smo & yfs(1,pt_first[js]:pt_first[js]+pt_npts[js]-1L)=pha_smo
;                  comp_smo= complex(real_smo,imag_smo)
;                  yfs(0,pt_first[js]:pt_first[js]+pt_npts[js]-1L) $
;                   =abs(comp_smo)
;                  yfs(1,pt_first[js]:pt_first[js]+pt_npts[js]-1L)  $
;                   =atan(imag_smo,real_smo)*!RADEG
                 end
             endcase
           end
      endcase
   endif
   endfor
   return,1

end
