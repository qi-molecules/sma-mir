function plo_spe_ini,use,all_souids,all_sources,all_amps
;
; Outputs a list of source ids, names and their amplitudes for the spe window.
;
;
; result = -1 (error), 0 (no rows) >0 (number of all_sources)
; returns -1 in souids arrays if no sources found
;
; all_souids -- list of souids for passband
; all_sources - list of sources names
; all_amps   -- raw amplitudes for sources
; use        -- 0 or 1 depending on whether it is not flagged
;               or flagged for passband use
;
; eg. : result=dat_filter(s_f,/reset) 
; eg. : result=spe_ini(use,all_souids,all_sources,all_amps)
;
common global
common data_set
common plo

result=dat_list(s_l,'"band" like "s"',/reset,/no_notify)
if n_elements(pil) gt 0 then begin
   all_souids=uti_distinct(in[pil].souid,ndistinct,/many_repeat)
   all_amps=make_array(ndistinct,/float)
   all_sources=make_array(ndistinct,/string)
   for j=0,n_elements(all_souids)-1 do begin
     result=dat_list(s_l,/reset,/no_notify)
     i=where(in[pil].souid eq all_souids[j])
     all_sources[j]=c.source[in[pil[i[0]]].isource]
     result=dat_list(s_l,'"souid" eq "'+ $
               string(all_souids[j])+'"',/reset,/no_notify)
     all_amps[j]=mean([bl[pbl].ampave])
   endfor
endif else begin
   all_souids=-1 & all_sources='' & all_amps=0.
endelse


use=make_array(n_elements(all_souids),/int)
use[0] = 1

return,n_elements(all_souids)

end
