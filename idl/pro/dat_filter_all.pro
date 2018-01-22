function dat_filter_all,fil_souids,all_sources,all_blcds, $
    all_aqs,all_bqs,all_cqs,all_gqs,all_oqs,all_pqs,all_tels,all_sbs, $
    all_bands,all_wave_bands,all_ints

;
; Outputs a list of source ids, names and available data flags 
; for the current dataset.
;
;
; result = -1 (error), 0 (no rows) >0 (number of all_sources)
; returns -1 in souids arrays if no sources found
;
; eg. : result=dat_filter_all(all_souids,all_sources)
;
common global
common data_set
common plo

;
; get complete dataset 
; the list of source id#'s , names and are returned
; in arrays : all_souids, all_sources and 
;
 if n_elements(pi) gt 0 then begin
   all_souids=uti_distinct(in[pi].souid,ndistinct,/many_repeat)
   all_sources=make_array(ndistinct,/string)
   for j=0,n_elements(all_souids)-1 do begin
     i=where(in[pi].souid eq all_souids[j])
     all_sources[j]=c.source[in[pi[i[0]]].isource]
   endfor
 endif else begin
   all_souids=-1 & all_sources='' 
 endelse
;
 if n_elements(pb) gt 0 then begin
   iblcds=uti_distinct(bl[pb].iblcd,ndistinct,/many_repeat)
   all_blcds=make_array(ndistinct,/string)
   for j=0,n_elements(iblcds)-1 do begin
     all_blcds[j]=c.blcd[iblcds[j]]
   endfor
 endif else begin
   all_blcds=''  
 endelse
all_aqs=c.aq
all_bqs=c.bq
all_cqs=c.cq
all_gqs=c.gq
all_oqs=c.oq
all_pqs=c.pq
all_tels=uti_distinct([c.tel1,c.tel2],ndistinct)
all_sbs=c.sb
all_bands=c.band
all_wave_bands=[0]
j=where(sp[ps].fsky le 150.,count)
if count gt 0 then all_wave_bands=[all_wave_bands,0]
j=where(sp[ps].fsky gt 150.,count)
if count gt 0 then all_wave_bands=[all_wave_bands,1]
all_wave_bands=all_wave_bands[1:*]
all_ints=[min(in[pi].int),max(in[pi].int)]


if all_souids[0] eq -1 then return,0
;
; get 
;

return,n_elements(all_souids)

end
