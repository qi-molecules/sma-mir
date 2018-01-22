function uti_pha_unwrap,pha,smooth=smooth,ramp=ramp  
;
; Unwrap phases.
;
; The data are converted to complex (with unity radius), the real
; and imaginary are smoothed and this function
; is used to track the phase through +- 180 deg.
;
; parameters : pha    -- phases to be unwrapped
;      keyword smooth -- sets number of pts to smooth over
;                        default=3
;              ramp   -- expected phase ramp between adjacent points
;                        default=10.
;
; result = -1 (error), unwrapped phase array (succesful)
;
; to generate test data :
; eg  : pha =20*findgen(115)
; eg. : for i=0,n_elements(pha)-1 do begin & $
; eg. :  pha(i)=pha(i)+50*randomu(seed) & $
; eg. : endfor
; eg. : pha=uti_pha_180(pha)
;
; eg. : result=uti_pha_unwrap(pha)
;
; eg. : plot,findgen(30),pha
;
common global
j_good=where(pha ne !BAD_VALUE)
if n_elements(j_good) le 3 then return,1
pha_all=pha
pha=pha[j_good]
pha=uti_pha_180(pha)
npts=n_elements(pha)
;
; first compute st_dev of pha from derivative of (phases - smoothed phases)
;
filter=make_array(3.,/float,value=1./3.)
;if not keyword_set(wsmooth) then wsmooth=3.
;filter=make_array(wsmooth,/float,value=1./wsmooth)
smo=pha-convol(pha,filter,/edge_truncate)
j=where(abs(smo) lt 70.,count)
if count gt 2 then smo=smo(j) 
npts_smo=n_elements(smo)
result=moment(smo-[smo(1:npts_smo-1),smo(npts_smo-1)],sdev=sdev)
;
; convert phases to re and im vis since these one can smooth
; w/o smoothing through discontinuities (since the complex
; vis is a continuous function
;
uti_conv_apc,cmp,make_array(npts,/float,value=1.),pha,/complex
re=float(cmp) & im=imaginary(cmp)
;
; smooth the complex vis and convert back to amp,pha
;
if not keyword_set(ramp) then ramp=10.
if not keyword_set(smooth) then smooth=(sdev/float(ramp))^2
smooth=long(smooth)
if (smooth mod 2L) ne 1L then smooth=smooth+1L
if smooth gt (n_elements(pha)-2L) then smooth=n_elements(pha)-2L
smooth=max([smooth,1L])
filter=make_array(smooth,/float,value=1./smooth)
uti_conv_apc,complex(convol(re,filter,/edge_truncate), $
      convol(im,filter,/edge_truncate)),ampnf,phaf,/amp_pha
pha_jmp=make_array(npts,/float,value=0.)
;
; compare the smoothed phases with themselves shifted by one point
; ie. look for jumps
;
diff=phaf-[phaf(1:npts-1),phaf(npts-1)]
; 
; first find correct lobes for smoothed phases
;
; jp => positive jump , jm => negative jump
;
jp=where(diff gt 180.) & jm=where(diff lt -180.)
if max(jp) ne -1 then begin 
  for i=0,n_elements(jp)-1 do begin  
    pha_jmp(jp(i)+1:npts-1)= pha_jmp(jp(i)+1:npts-1) +360. 
  endfor 
endif
if max(jm) ne -1 then begin 
  for i=0,n_elements(jm)-1 do begin 
    pha_jmp(jm(i)+1:npts-1)= pha_jmp(jm(i)+1:npts-1) -360. 
  endfor 
endif
;
;  put phase jumps into smoothed phases
;
phaf=phaf+pha_jmp
pha=pha+pha_jmp
;
; now make sure actual phases follow lobes of smoothed phases
;
diff=pha-phaf
pha_jmp=make_array(npts,/float,value=0.)
jp=where(diff gt 180.) & jm=where(diff lt -180.)

if max(jp) ne -1 then begin 
  for i=0,n_elements(jp)-1 do begin  
    pha_jmp(jp(i))= pha_jmp(jp(i)) -360.
  endfor 
endif
if max(jm) ne -1 then begin 
  for i=0,n_elements(jm)-1 do begin 
    pha_jmp(jm(i))= pha_jmp(jm(i)) +360. 
  endfor 
endif
;
; put in phase jumps and set mean value to lobe closest to zero
;
pha=pha+pha_jmp
;
lobe=float(round(total(pha)/max([n_elements(pha),1])/360.)) 
phas=pha-lobe*360.
pha=pha_all & pha[j_good]=phas
return,1
end

