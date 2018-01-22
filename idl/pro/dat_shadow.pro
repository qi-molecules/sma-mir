; DAT_SHADOW set the weights to negative values for shadowed baselines
; and blocked antennas. The optional parameter "shadow" is the maximum 
; allowed shadowing percentage (0-100). If shadow=0, then any baselines
; less than the telescope diameter are considered shadowed.

function dat_shadow,shadow,blocked_list=blocked_list

   ; Common blocks
     common global
     common data_set

   ; Set minimum allowed shadowed percentage
     shadow_default = 0.0
     shadow = keyword_set(shadow) ? shadow : shadow_default
     if (shadow lt 0.0 or shadow gt 100.0) then begin
        print,"Error entering shadowing percentage in dat_shadow()"
        return,-1
     endif

   ; Find maximum project baseline for the input shadowing percentage
     bmin = uti_bmin_calc(shadow,!TEL_DIAM)

   ; Reset filter
     if (dat_list(s_l,/reset,/no) le 0) then return,-1

   ; If the data are shadowed more than that specified by "shadow",
   ; set the weight to negative. This is complicated because
   ; one must look for baselines which involve telescopes blocked
   ; on shorter baselines. The procedure is as follows:
   ;     (1) Loop over integrations that contain shadowed baselines
   ;     (2) For each integration, determine which telescopes are blocked.
   ;     (3) Flag the shadowed baselines and all baselines involving the
   ;         blocked antennas.
   ;
   ; First, get the shadowed baselines and initialize a list of blocked 
   ; antennas
     blocked_list = -1
     count_shadow = 0
     j=where(bl[pbl].prbl lt bmin and sp(psl).wt gt 0.0,nj)

   ; Loop over shadowed baselines
     if nj gt 0 then begin
        ; Find integration numbers for the shadowed baselines
          count_shadow = 0
          shadowed_int = uti_distinct(in[pil(j)].int)

        ; Loop over integrations with shadowed baselines
          for i=0,n_elements(shadowed_int)-1 do begin
             ; Find shadowed baselines for this integration
               js = j(where(in[pil(j)].int eq shadowed_int(i),njs))
               tel1 = c.tel1[bl(pbl(js)).itel1]
               tel2 = c.tel2[bl(pbl(js)).itel2]

             ; Set east-west and north-south products that will be used
             ; to determine which antennas is blocked
               northsouth = (in(pil[js]).decr-!TEL_LAT*!DTOR)*bl(pbl[js]).bln 
               eastwest   = in(pil[js]).ha*bl(pbl[js]).ble

             ; Loop over shadowed baselines lines and determined which antenna
             ; is blocked
               for k=0,njs-1 do begin
                  ; Initialize
                    blocked_dish = tel1[k]

                  ; If we are pointed west (east), then the eastern (western)
                  ; telescope is blocked. If we are pointed north (south),
                  ; then the southern (northern) telescope is blocked.
                    if (abs(bl(pbl[js(k)]).bln) lt 1.0) then begin
                         if (northsouth[k] lt 0.0) then blocked_dish = tel2[k]
                    endif else if (eastwest[k] gt 0.0) then $
                       blocked_dish = tel2[k]
               endfor
               blocked_list = [blocked_list,blocked_dish]

             ; Flag blocked dishes. This will also get the shadowed baselines
               k = where((c.tel1[bl[pbl].itel1] eq blocked_dish or $
                          c.tel2[bl[pbl].itel2] eq blocked_dish) and $
                          in(pil).int eq shadowed_int(i),nk)
               sp(psl[k]).wt=-abs(sp(psl[k]).wt)
               count_shadow=count_shadow+nk
          endfor
      endif
      blocked_list = uti_distinct(blocked_list)
      j = where(blocked_list gt 0,nj)
      if (nj gt 0) then print,blocked_list[j] else blocked_list = 0

    ; Print error message
      print,format='(%"%d spectrometer bands removed due to shadowing")',$
         count_shadow
      return,count_shadow
end
