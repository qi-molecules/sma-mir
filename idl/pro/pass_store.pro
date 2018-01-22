function pass_store,x_var,y_vars,dch_smooth,frame_vars,sources,plid,sindex,polrx=polrx
;
;  After the gain calibration is complete - this routine is called to store
;  the cal in the ca structure
;
common global
common data_set
common plo

result=dat_list(s_l,/reset)

;
; save in ca (cal) structure
;
result=cal_store(s_c,init=pas[sindex].saved_par.nframes) & irow=-1
for i=0,pas[sindex].saved_par.nframes-1 do begin
  result=dat_comb_sep(pas[sindex].distinct_frames[i],['blcd','rec','sb','band'],codes, $
                      icodes,n_components)
  if keyword_set(polrx) then icodes[0]=polrx
  js=max(where(pas[sindex].distinct_frames[i] eq pas[sindex].frames,n_values))
  if y_vars eq 'amp' or y_vars eq 'pha' then begin
    ys=reform([pas[sindex].yfs[0,pas[sindex].pt_first[js]:pas[sindex].pt_first[js]+pas[sindex].pt_npts[js]-1L]],pas[sindex].pt_npts[js])
  endif      
  if y_vars eq 'amp,pha' then begin
    ys=[reform(pas[sindex].yfs[0,pas[sindex].pt_first[js]:pas[sindex].pt_first[js]+pas[sindex].pt_npts[js]-1L],pas[sindex].pt_npts[js]), $
        reform(pas[sindex].yfs[1,pas[sindex].pt_first[js]:pas[sindex].pt_first[js]+pas[sindex].pt_npts[js]-1L],pas[sindex].pt_npts[js])]
  endif
  irow=irow+1
  result=cal_store(s_c,'pass',x_var,y_vars,pas[sindex].tel_bsl,pas[sindex].inhid_beg,pas[sindex].inhid_end, $
        codes,icodes,pas[sindex].xs[pas[sindex].pt_first[js]:pas[sindex].pt_first[js]+pas[sindex].pt_npts[js]-1L], $
        ys,dch_smooth,irow,/save)
endfor
result=cal_store(s_c,/transfer)

return,1
end
