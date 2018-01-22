pro uti_tsysamp

common global
common data_set

      yfs = sp[psl].tssb
      bls=c.blcd(bl(pbl).iblcd)
      recs=c.rec(bl(pbl).irec)
      sbs=c.sb(bl(pbl).isb)
      bands=c.band(sp(psl).iband)
      frames=bls+' '+recs+' '+sbs+' '+bands
      distinct_frames=uti_distinct(frames,nframes,/many_repeat)
      for i=0,nframes-1 do begin
         result=dat_comb_sep(distinct_frames[i], $
                      ['blcd','rec','sb','band'],codes, $
                      icodes,n_components)      
         js=where(distinct_frames[i] eq frames,n_values)
         ys=yfs[js]
         for j=0,n_values-1 do begin
            ch[pcl[js[j]]:pcl[js[j]]+sp[psl[js[j]]].nch-1]=ys[j]*ch[pcl[js[j]]:pcl[js[j]]+sp[psl[js[j]]].nch-1]
         endfor 
      endfor

      result=dat_list(s_l,'"band" like "c"',/reset,/no_notify)
      bl[pbl].ampave=bl[pbl].ampave*sp[psl].tssb

      result=dat_list(s_l,/reset)
end


