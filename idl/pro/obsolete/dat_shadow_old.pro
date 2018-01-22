function dat_shadow,shadow

   ; Common blocks
     common global
     common data_set

   ; Set minimum allowed shadowed percentage
     shadow_default = 0.0001
     shadow = keyword_set(shadow) ? $
                (shadow gt shadow_default) lt 100. : shadow_default

   ; Initialize
     bmin=uti_bmin_calc(shadow,!TEL_DIAM)

   ; Reset filter
     if (dat_list(s_l,/reset,/no) le 0) then return,-1

   ; If the data are shadowed more than that specified by "shadow",
   ; the weight is set to negative -- this is complicated because
   ; one must look for baselines which involve telescopes blocked
   ; on shorter baselines
     shad_tel=-1
     shad_inhid=0
     j=where(bl[pbl].prbl lt bmin,count_shadow)
     if count_shadow gt 0 then begin
        count_shadow=0
        jw_neg=where(bl[pbl[j]].w lt 0.,count_neg)
        jw_pos=where(bl[pbl[j]].w gt 0.,count_pos)
        if count_neg gt 0 then begin
           shad_tel=[shad_tel,bl[j[jw_neg]].itel1]
           shad_inhid=[shad_inhid,in[pil[[j[jw_neg]]]].inhid]
        endif 
        if count_pos gt 0 then begin
           shad_tel=[shad_tel,bl[j[jw_pos]].itel1]
           shad_inhid=[shad_inhid,in[pil[[j[jw_pos]]]].inhid]
        endif
        if n_elements(shad_tel) gt 1 then begin
          for i=1,n_elements(shad_tel)-1 do begin
            j=where(in[pil].inhid eq shad_inhid[i])
            js=j(where(bl[pbl[j]].itel1 eq shad_tel[i] or  $
                       bl[pbl[j]].itel2 eq shad_tel[i],count))
            sp(psl[js]).wt=-abs(sp(psl[js]).wt)
            count_shadow=count_shadow+count
          endfor
        endif 
      endif

    ; Print error message
      print,format='(%"%d spectrometer bands removed due to shadowing")',$
         count_shadow
      return,count_shadow
end
