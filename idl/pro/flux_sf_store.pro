function flux_sf_store,good_bsl,good_sb,$
          jct_bsl,jct_sb,jct_single,method=method
   ; Command blocks
     common global
     common data_set

   ; Reset filter
     result = dat_list(s_l,/reset,/no)

   ; Set method for scaling data. Default is to use a single scale factor
     scale_method = keyword_set(method) ? method : 'single'

   ; Set parameters used to store scale factors
     inhid_beg=min([in(pil).inhid]) 
     inhid_end=max([in(pil).inhid])
     bsl=c.blcd(bl(pbl).iblcd)
     recs=c.rec(bl(pbl).irec)
     sbs=c.sb(bl(pbl).isb)
     bands=c.band(sp(psl).iband)
     frames=bsl+' '+recs+' '+sbs+' '+bands
     distinct_frames=uti_distinct(frames,nframes,/many_repeat)

   ; Scale data by integration. The default scale factor is the average over
   ; all baselines. Initialize yfs to this value, and still in the array
   ; with valid scale factors depending on baseline and/or sideband based.
     npts=n_elements(pbl)
     xs=in(pil).int
     yfs=replicate(1./jct_single,npts)

   ; Determine which scale factors to apply
     case strlowcase(scale_method) of
        'bsl' : begin
                  for is=0L, n_elements(good_sb)-1L do begin
                    js=where(sbs eq good_sb[is])
                    yfs[js]=1./jct_sb[is]
                    for ib=0L,n_elements(good_bsl)-1L do begin
                      jb=where(bsl eq good_bsl[ib] and sbs eq good_sb[is])
                      yfs[jb]=1./jct_bsl[is,ib]
                    endfor
                  endfor
               end
        'sb' : begin
                 for is=0L, n_elements(good_sb)-1L do begin
                    js=where(sbs eq good_sb[is])
                    yfs[js]=1./jct_sb[is]
                 endfor
               end
        else : 
     endcase

   ; use cal_store to store the scale factors
     result=cal_store(s_c,init=nframes) 
     irow=-1
     for i=0,nframes-1 do begin
        result=dat_comb_sep(distinct_frames[i], $
                     ['blcd','rec','sb','band'],codes, $
                     icodes,n_components)      
        js=where(distinct_frames[i] eq frames,n_values)
        js=js[where(yfs[js] ne !BAD_VALUE,n_values)]
        ys=reform([yfs[js]],n_values)
        irow=irow+1
        x_var='int'
        y_vars='amp'
        tel_bsl='baseline'
        dt_smooth=1
        result=cal_store(s_c,'gain',x_var,y_vars,tel_bsl,inhid_beg,inhid_end,$
                        codes,icodes,xs(js),ys,dt_smooth,irow,/save)   
     endfor
     result=cal_store(s_c,/transfer)

   ; Reset filter
     result = dat_list(s_l,/reset,/no)

   ; Done
     return,1
end
