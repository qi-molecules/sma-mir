function gain_store_xpol,tel_bsl,x_var,y_vars,plid,sindex, polrx=polrx, combinations=combinations
;
;  After the gain calibration is complete - this routine is called to store
;  the cal in the ca structure
;
; parameters :  tel_bsl -- solution type: 'telescope' or 'baseline' -based
;               x_var   -- header variable for x-ccord :'int','hours','el'
;               y_vars  -- variables plotted and fit as y-coord : 
;                          'amp','pha','amp,pha'
;               plid    -- plot id
;               sindex  -- solution index

common global
common data_set
common plo


result=dat_list(s_l,/reset)

;
;
; save in ca (cal) structure
;
ncombinations=n_elements(combinations)
;result=cal_store(s_c,init=gai[sindex].saved_par.nframes) & irow=-1
result=cal_store(s_c,init=ncombinations) & irow=-1
for i=0,gai[sindex].saved_par.nframes-1 do begin
   result=dat_comb_sep(gai[sindex].distinct_frames[i],['blcd','rec','sb','band'],codes, $
     icodes,n_components)
   if keyword_set(polrx) then icodes[0]=polrx
   js=where(gai[sindex].distinct_frames[i] eq gai[sindex].frames,n_values)
   js=js[where(gai[sindex].yfs[0,js] ne !BAD_VALUE,n_values)]
   if y_vars eq 'amp' or y_vars eq 'pha' then begin
      ys=reform([gai[sindex].yfs[0,js]],n_values)
   endif      
   if y_vars eq 'amp,pha' then begin
      ys=[reform(gai[sindex].yfs[0,js],n_values),reform(gai[sindex].yfs[1,js],n_values)]
   endif

   if e.debug then begin
      print, "sindex", sindex
      print, " To store inh ", gai[sindex].inhid_beg, gai[sindex].inhid_end
   endif

   isbcode=icodes[2]
   for j=0,ncombinations-1 do begin      
      result=dat_comb_sep(combinations[j],['blcd','rec','sb','band'],codes, icodes,n_components)

      if icodes[2] eq isbcode then begin
         irow=irow+1
         result=cal_store(s_c,'gain',x_var,y_vars,tel_bsl,gai[sindex].inhid_beg,gai[sindex].inhid_end,$
        codes,icodes,gai[sindex].xs(js),ys,gai[sindex].dt_smooth,irow,/save)
      endif
   endfor
endfor
result=cal_store(s_c,/transfer)

return,1
end
