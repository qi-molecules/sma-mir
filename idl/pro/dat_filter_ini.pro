function dat_filter_ini,fil_souids,fil_sources,fil_blcds, $
    fil_aqs,fil_bqs,fil_cqs,fil_gqs,fil_oqs,fil_pqs,fil_tels,fil_sbs, $
    fil_bands,fil_wave_bands,fil_ints

;
; Outputs a list of source ids, names and available data flags 
; for the filter window.
;
;
; result = -1 (error), 0 (no rows) >0 (number of fil_sources)
; returns -1 in souids arrays if no sources found
;
; eg. : result=dat_filter_ini(fil_souids,fil_sources)
;
common global
common data_set
common plo

;
; get filtered data 
; the list of source id#'s , names and are returned
; in arrays : fil_souids, fil_sources and 
;
 if n_elements(pif) gt 0 then begin
   fil_souids=uti_distinct(in[pif].souid,ndistinct,/many_repeat)
   fil_sources=make_array(ndistinct,/string)
   for j=0,n_elements(fil_souids)-1 do begin
     i=where(in[pif].souid eq fil_souids[j])
     fil_sources[j]=c.source[in[pif[i[0]]].isource]
   endfor
 endif else begin
   fil_souids=-1 & fil_sources='' 
 endelse
;
 if n_elements(pbf) gt 0 then begin
   iblcds=uti_distinct(bl[pbf].iblcd,ndistinct,/many_repeat)
   fil_blcds=make_array(ndistinct,/string)
   for j=0,n_elements(iblcds)-1 do begin
     fil_blcds[j]=c.blcd[iblcds[j]]
   endfor
 endif else begin
   fil_blcds=''  
 endelse
fil_aqs=c.aq
fil_bqs=c.bq
fil_cqs=c.cq
fil_gqs=c.gq
fil_oqs=c.oq
fil_pqs=c.pq
fil_tels=uti_distinct([c.tel1,c.tel2],ndistinct,/many_repeat)
fil_sbs=c.sb
fil_bands=c.band
fil_wave_bands=[0]
j=where(sp[psf].fsky le 150.,count)
if count gt 0 then fil_wave_bands=[fil_wave_bands,0]
j=where(sp[psf].fsky gt 150.,count)
if count gt 0 then fil_wave_bands=[fil_wave_bands,1]
fil_wave_bands=fil_wave_bands[1:*]
fil_ints=[min(in[pif].int),max(in[pif].int)]

print,'blcds', fil_blcds
print,'aqs ',fil_aqs
print,'bqs ',fil_bqs
print,'cqs ',fil_cqs
print,'gqs ',fil_gqs
print,'oqs ',fil_oqs
print,'pqs ',fil_pqs
print,'tels ',fil_tels
print,'sbs ',fil_sbs
print,'bands  ' ,fil_bands
print,'wave_bands ',fil_wave_bands
print,'ints ',fil_ints

if fil_souids[0] eq -1 then return,0
;
; get 
;

return,n_elements(fil_souids)

end
