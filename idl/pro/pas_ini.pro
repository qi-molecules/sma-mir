function pas_ini,use,all_souids,all_sources,all_amps,nint=nint
;
; Outputs a list of source ids, names and their amplitudes for the pass window.
;
;
; result = -1 (error), 0 (no rows) >0 (number of all_sources)
; returns -1 in souids arrays if no sources found
;
; get pas qualified data and the measured amps
; the list of source id#'s , names and amplitudes are returned
; in arrays : all_souids, all_sources and all_amps
; the array 'use' is 0 or 1 depending on whether it is not flagged
; or flagged for passband use
;
; all_souids -- list of souids for passband
; all_sources - list of sources names
; all_amps   -- raw amplitudes for sources
; use        -- 0 or 1 depending on whether it is not flagged
;               or flagged for passband use
;
; eg. : result=dat_filter(s_f,/reset) 
; eg. : result=pas_ini(use,all_souids,all_sources,all_amps)
;
common global
common data_set
common plo

result=dat_list(s_l,'"pq" eq "p"',/reset,/no_notify)
 if n_elements(pil) gt 0 then begin
   pas_souids=uti_distinct(in[pil].souid,ndistinct,/many_repeat)
 endif else begin
   pas_souids=-1 & pas_sources='' & pas_amps=0.
 endelse


; get names of all sources in dataset and the measured amps 
;  in current idl dataset
; the list of source names and amplitudes are returned
; in arrays : all_sources and all_amps
;
 result=dat_list(s_l,/reset,/no_notify)
 if n_elements(pil) gt 0 then begin
 all_souids=uti_distinct(in[pil].souid,ndistinct,/many_repeat)

 all_sources=make_array(ndistinct,/string)
 all_amps=make_array(ndistinct,/float)
 nint=intarr(ndistinct)
 for j=0,n_elements(all_souids)-1 do begin
   result=dat_list(s_l,/reset,/no_notify)
   i=where(in[pil].souid eq all_souids[j])
   k=uti_distinct(in[pil(i)].int,nk,/many_repeat)
   nint[j] = nk
   all_sources[j]=c.source[in[pil[i[0]]].isource]
     result=dat_list(s_l,'"souid" eq "'+ $
               string(all_souids[j])+'"',/reset,/no_notify)
     all_amps[j]=mean([bl[pbl].ampave])
 endfor
 endif else begin
   all_souids=-1 & all_sources='' & all_amps=0.
 endelse

use=make_array(n_elements(all_souids),/int)
if n_elements(pas_souids) gt 0 then begin
  for k=0,n_elements(pas_souids)-1 do begin 
   h=where(pas_souids[k] eq all_souids,count) 
   if count gt 0 then use[h]=1 
  endfor
endif

return,n_elements(all_souids)

end
