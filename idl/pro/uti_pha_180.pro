function uti_pha_180 ,phas
;
; Converts phase data to be in range +-180 deg.
;
; parameters : phas -- input phase array
; result = modified phases
; eg. : phas=uti_pha_180(phas)
;

common global
; next line adds 4 turns to the phase and then mods with 360.
  phas_180=phas
  j_good=where(phas ne !BAD_VALUE,count)
  if count gt 0 then begin
    phas_180[j_good]=(1440. + phas[j_good]) mod 360. 
    j=where(phas_180[j_good] gt 180.) 
    if max(j) ne -1 then phas_180(j_good[j])=phas_180(j_good[j])-360.
  endif
return,phas_180
end
