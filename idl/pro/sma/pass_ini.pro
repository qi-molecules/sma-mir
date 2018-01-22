function pass_ini,use,all_souids,all_sources,all_amps,defaults=defaults
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

; get names of all sources in dataset and the measured amps 
;  in current idl dataset
; the list of source names and amplitudes are returned
; in arrays : all_sources and all_amps
;
 result=dat_list(s_l,/reset,/no_notify)
 if n_elements(pil) gt 0 then begin
 all_souids=uti_distinct(in[pil].souid,nsources,/many_repeat)

print,'Check: Found a total of ',nsources,'  different sources in the data set.'

 all_sources=make_array(nsources,/string)
 all_amps=make_array(nsources,/float)
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


; This is a loop that allows the user to set the passband qualifier code
; for each source by hand.
if (e.campuslogin eq 'sma' or e.campuslogin eq 'cfa') then begin

fluxes_3mm = make_array(nsources,/float)
fluxes_1mm = make_array(nsources,/float)
gain_sel  = make_array(nsources,/int)
 
print,'Set which sources will be used as calibrators and set the calibrator fluxes'
print,'The flux should be in Jy at the frequency of the observation'
 
   again:
   print,'These are the sources and their current passband codes '
   for i = 0,nsources-1 do begin
    list = '"source" eq "'+strtrim(all_sources[i],2)+'"'
    result=dat_list(s_l,list,/reset,/no_notify)
      fluxes_3mm[i] = in[pil[0]].sflux
      fluxes_1mm[i] = in[pil[0]].sflux
      wd = 'NO '
      if (sp[psl[0]].ipq eq 1) then begin
         wd = 'YES'
         sp[psl].ipq=1
         gain_sel[i] = 1
 ;       print,format='("name:  ",A,T25,"  passband cal: ",A,"  flux (Jy): ",F)' , $
 ;       all_sources[i],wd,fluxes_3mm[i]
      endif else begin
 ;       print,format='("name:  ",A,T25,"  passband cal: ",A)' , $
 ;       all.sources[i],wd
         sp[psl].ipq = 0
         gain_sel[i] = 0
      endelse
      print,format='("name:  ",A,T25,"  passband cal: ",A)' , $
      all_sources[i],wd
   endfor
   sngc = ''
   if not keyword_set(defaults) then begin
      print,'Enter source and new cal code. eg: 3C273 YES' 
      print,'or hit Return if all the sources are correctly specified'
      read,sngc
      if sngc eq '' then goto,finish
      parts = strtrim(strsplit(sngc,' ',/extract),2)
      sn = parts[0]
      list = '"source" eq "'+strtrim(sn,2)+'"'
      result=dat_list(s_l,list,/reset,/no_notify)
      jj=where(strtrim(sn,2) eq all_sources)
      j = 0
      gc = parts[1]
      if (gc eq 'YES' or gc eq 'yes' or gc eq 'Yes' or gc eq 'Y' or gc eq 'y') then j = 1
      sp[psl].ipq = j
      if sn ne 'all' then begin
         if (j eq 1) then begin
            if (n_elements(parts) ge 3) then begin
               fluxes_3mm[jj] = float(parts[2])
               fluxes_1mm[jj] = float(parts[2])
               in[pil].sflux = float(parts[2])
            endif
            gain_sel[jj] = 1
         endif else begin
            if (n_elements(parts) ge 3) then begin
               fluxes_3mm[jj] = float(parts[2])
               fluxes_1mm[jj] = float(parts[2])
               in[pil].sflux = float(parts[2])
            endif
            gain_sel[jj] = 0
         endelse
      endif
      result=dat_list(s_l,/reset)
      goto,again
   endif
   finish: 
endif
 
print,'OK, now checking the data...'                                                                           

result=dat_list(s_l,'"pq" eq "p"',/reset,/no_notify)
 if result gt 0 then begin
   pas_souids=uti_distinct(in[pil].souid,ndistinct,/many_repeat)
   print,'Check: Found ',ndistinct,'  sources set to be used as passband calibrators'
 endif else begin
   pas_souids=-1 & pas_sources='' & pas_amps=0.
;   print,'Check: Found no sources in data set to be used as passband calibrators'
;   print,'Check: This is not going to work.'
 endelse

if pas_souids[0] eq -1 then return,0

print,'Check: The average amplitudes of the sources in the data set: '
for i = 0,n_elements(all_souids)-1 do begin
print,all_sources[i],all_amps[i]
endfor

use=make_array(n_elements(all_souids),/int)
if n_elements(pas_souids) gt 0 then begin
  for k=0,n_elements(pas_souids)-1 do begin 
   h=where(pas_souids[k] eq all_souids,count) 
   if count gt 0 then use[h]=1 
  endfor
endif


return,n_elements(all_souids)

end
