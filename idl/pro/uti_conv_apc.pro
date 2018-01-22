pro  uti_conv_apc,cmp,amp,pha,amp_pha=amp_pha,complex=complex 
;
; Converts between complex and amp/pha data  
; 
;
; parameters : cmp      -- complex array 
;              amp      -- amp array 
;              pha      -- pha array 
;              amp_pha  -- keyword to compute amp and phase
;              complex  -- keyword to compute complex
;
; eg. : uti_conv_apc,cmp,amp,pha,/complex ; convert amp,pha to complex 
; eg. : uti_conv_apc,cmp,amp,pha,/amp_pha ; convert complex to amp,pha
;
common global
common data_set

if keyword_set(amp_pha) then begin
  j_bad=where(cmp eq !BAD_COMPLEX,count)
  amp=abs(cmp) & pha=!radeg*atan(imaginary(cmp),float(cmp))
  if count gt 0 then begin
    amp(j_bad)=!BAD_VALUE & pha[j_bad]=!BAD_VALUE
  endif
endif
if keyword_set(complex) then begin
  j_bad=where(amp eq !BAD_VALUE or pha eq !BAD_VALUE,count)
  pha_rad=pha*!dtor
  cmp=amp*complex(cos(pha_rad),sin(pha_rad))
  if count gt 0 then begin
    cmp(j_bad)=!BAD_COMPLEX
  endif
endif
end
