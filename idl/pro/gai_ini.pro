function gai_ini,delta_days,use,all_souids,all_sources,all_amps,numbs_3mm,numbs_1mm, $
                fluxes_3mm,fluxes_1mm
;
; Outputs a list of source ids, names and their amplitudes for the gain window.
; Also get the fluxes these sources should have from the flux archive in the db
; The fluxes which are returned are w/i delta_days of the present data
; and they are refernece to fiducial frequencuies of 100 and 230 GHz
;
; parameters : delta_days -- day range for db flux search   
;
; result = -1 (error), 0 (no rows) >0 (number of all_sources)
; returns -1 in souids arrays if no sources found
;
; eg. : result=dat_filter(s_f,/reset) 
; eg. :result=gai_ini(30.,use,gai_souids,gai_sources,all_amps,numbs_3mm,numbs_1mm,gai_fluxes_3mm,gai_fluxes_1mm)
;
common global
common data_set
common plo

result=dat_list(s_l,'"gq" eq "g"',/reset,/no_notify)
 if n_elements(pil) gt 0 then begin
   gai_souids=uti_distinct(in[pil].souid,ndistinct,/many_repeat)
 endif else begin
   gai_souids=-1 & gai_sources='' & gai_amps=0.
 endelse
;
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

if gai_souids[0] eq -1 then return,0
 fluxes_3mm=make_array(ndistinct,/float)
 fluxes_1mm=make_array(ndistinct,/float)
 numbs_3mm=make_array(ndistinct,/int)
 numbs_1mm=make_array(ndistinct,/int)
;
; get 100 GHz and 230 GHz fluxes for all sources
;
uto=c.ut[0]
if all_souids[0] ne -1 then $
res=dbi_flux_read(c.ut[0],delta_days,all_numbs,all_fluxes,souids=all_souids)
for i=0,ndistinct-1 do fluxes_3mm[i]=all_fluxes[2*i]
for i=0,ndistinct-1 do fluxes_1mm[i]=all_fluxes[2*i+1]
for i=0,ndistinct-1 do numbs_3mm[i]=all_numbs[2*i]
for i=0,ndistinct-1 do numbs_1mm[i]=all_numbs[2*i+1]

use=make_array(n_elements(all_souids),/int)
if n_elements(gai_souids) gt 0 then begin
  for k=0,n_elements(gai_souids)-1 do begin 
   h=where(gai_souids[k] eq all_souids,count) 
   if count gt 0 then use[h]=1 
  endfor
endif

return,n_elements(all_souids)

end
