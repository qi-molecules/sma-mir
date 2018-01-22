function pas_fit,tel_bsl,funct,dch_smooth,x_var,y_var,baselines, $
                 distinct_baselines,xs,ys,wts,pt_first,pt_npts,yfs 
;
; Gain fit for amp and phase.
;
; The data on the gain calibrators is fit by baseline or telescope
;
; parameters : tel_bsl    -- type of fit ('telescope' or 'baseline')
;              dch_smooth  -- smoothing number of channels
;              x_var      -- x-ccord :'channel', 'fsky' ,'velocity'
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
; eg. : result=gai_fit(tel_bsl,2.,'hours','amp',4,xs,ys,yf)
;
common global
common data_set

n_singular=0
n_baselines=n_elements(distinct_baselines)
npts=n_elements(baselines)
case tel_bsl of
;
;   baseline-based fit
;  
     'baseline': begin
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
; then boxcar smooth it in channel
;
        nf=long(max([1,dch_smooth]))
        if (nf mod 2) ne 1 then nf=nf+1
        filter=make_array(nf,/float,value=(1./nf))
        y_smo=convol(y_int,filter,/edge_truncate)
        yfs(0,pt_first[js]:pt_first[js]+pt_npts[js]-1L)=y_smo
    endif
;
;  pha fit
;
    if y_var eq 'amp,pha' or y_var eq 'pha' then begin
;
        x_int=xs[pt_first[js]:pt_first[js]+pt_npts[js]-1L] 
        if y_var eq 'amp,pha' then $                
          y_int=reform(ys[1,pt_first[js]:pt_first[js]+pt_npts[js]-1L], $
                pt_npts[js])
        if y_var eq 'pha' then $ 
          y_int=reform(ys[0,pt_first[js]:pt_first[js]+pt_npts[js]-1L], $
                pt_npts[js])
;
; boxcar smooth it in channel #
;
        nf=max([1,dch_smooth])
        if (nf mod 2) ne 1 then nf=nf+1
        filter=make_array(nf,/float,value=(1./nf))
        y_smo=convol(y_int,filter,/edge_truncate)
        if y_var eq 'amp,pha' then begin
          yfs(1,pt_first[js]:pt_first[js]+pt_npts[js]-1L)=y_smo
        endif
        if y_var eq 'pha' then begin
          yfs(0,pt_first[js]:pt_first[js]+pt_npts[js]-1L)=y_smo
        endif
    endif
   if y_var eq 'complex' then begin
     uti_conv_apc,cmp,reform(ys(0,pt_first[js]:pt_first[js]+pt_npts[js]-1L), $
        count),reform(ys(1,pt_first[js]:pt_first[js]+pt_npts[js]-1L),count), $
                /complex
;
     x_int=xs(pt_first[js]:pt_first[js]+pt_npts[js]-1L) 
     real_int=float(cmp)
     imag_int=imaginary(cmp)
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
   endif
   endfor
   return,1
end

;   telescope-based fit
;  
     'telescope': begin
;
;  convert to complex visibilities for all baselines
;
   dat_conv_apc,cmp,reform(ys(0,js),npts),reform(ys(1,js),npts),/complex
  
end
     else: begin 
       print,'***', tel_bsl,'-based gain fit not recognized !'
       return,-1
     endelse
endcase
end
