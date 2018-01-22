; *************************************************************************
; FUNCTION
;      map_ini.pro
;
; WRITTEN
;      Aug 4, 2000 by syl
;
; PURPOSE
;      providing sources and bands available for MIR map.pro
;
; INPUTS
;      none
;
; OUTPUT
;       use         : source selection array
;       all_souids  : -1 (error); 0 (no rows); >0 (number of sources)
;       all_sources : source names
;       all_bands   : bands available
;
; EXAMPLES
;       result = map_ini(use,all_souids,all_sources,all_bands)
;
; *************************************************************************
;
function map_ini,use,all_souids,all_sources,all_bands
;
common global
common data_set
common plo
;
; setup dataset 
;
 result=dat_list(s_l,/reset,/no_notify)
 if n_elements(pil) gt 0 then begin

; get the band setup

 all_bands=c.band

;
; get names of all sources in dataset
; the list of source names is returned
; in the array : all_sources
;
 all_souids=uti_distinct(in[pil].souid,ndistinct,/many_repeat)
 use=make_array(ndistinct,/int)
 all_sources=make_array(ndistinct,/string)
 select=0
 for j=0,n_elements(all_souids)-1 do begin
   i=where(in[pil].souid eq all_souids[j])
   all_sources[j]=c.source[in[pil[i[0]]].isource]
   if select eq 0 then begin
   k=where(sp[psl].inhid eq in[pil[i[0]]].inhid)
     if sp[psl[k[0]]].igq+sp[psl[k[0]]].ipq eq 0 then begin
     use[j]=1
     select=1
     endif
   endif
 endfor
 endif else begin
   all_souids=-1 & all_sources=''
 endelse

return,n_elements(all_souids)

end
