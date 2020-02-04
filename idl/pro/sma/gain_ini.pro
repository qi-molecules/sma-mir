function gain_ini,delta_days,use,all_souids,all_sources,all_amps,numbs_3mm,numbs_1mm, $
                fluxes_3mm,fluxes_1mm,defaults=defaults, orig_flux=orig_flux
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

;
; get gain qualified data and the measured amps
; the list of source id#'s , names and amplitudes are returned
; in arrays : all_souids, all_sources and all_amps
; the array 'use' is 0 or 1 depending on whether it is not flagged
; or flagged for gain use
;

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

if (e.campuslogin eq 'sma' or e.campuslogin eq 'cfa') then begin
;nsources = n_elements(c.source)
fluxes_3mm = make_array(ndistinct,/float)
fluxes_1mm = make_array(ndistinct,/float)
numbs_3mm = make_array(ndistinct,/float,value=1.0)
numbs_1mm = make_array(ndistinct,/float,value=1.0)
gain_sel  = make_array(ndistinct,/int)
 
print,'Set which sources will be used as calibrators and set the calibrator fluxes'
print,'The flux should be in Jy at the frequency of the observation'
 
   again:
   print,'These are the sources and their current gain codes '
   for i = 0,ndistinct-1 do begin
    list = '"source" eq "'+strtrim(all_sources[i],2)+'"'
    result=dat_list(s_l,list,/reset,/no_notify)
      fluxes_3mm[i] = in[pil[0]].sflux
      fluxes_1mm[i] = in[pil[0]].sflux
      wd = 'NO '
      if (sp[psl[0]].igq eq 1) then begin
        wd = 'YES'
        gain_sel[i] = 1
;        print,format='("name:  ",A,T30,"  gain code: ",A,"  flux (Jy): ",F)' , $
;        all_sources[i],wd,fluxes_3mm[i]
      endif else begin
;        print,format='("name:  ",A,T30,"  gain code: ",A)' , $
;        all_sources[i],wd
        gain_sel[i] = 0
      endelse
      print,format='("name:  ",A,T30,"  gain code: ",A,"  flux (Jy): ",F)' , $
           all_sources[i],wd,fluxes_3mm[i]
   endfor
   sngc = ''
   if not keyword_set(defaults) then begin
   
   if not keyword_set(orig_flux) then begin
     print,''
     print,'***PLEASE READ !'
     print,'The default flux calibration models are now from CASA (Butler-JPL-Horizons 2012).'
     print,"If you want to use the original flux models, type x to quit,"
     print," then repeat the command with /orig keyword."
     print,'***'
     print,''
   endif

   print,'Enter source, cal code, and if cal, flux in Jy, eg: 3C273 YES 3.1'
   print,'or hit Return if all the sources are correctly specified'
   read,sngc
   if sngc eq '' then goto,finish
   parts = strtrim(strsplit(sngc,' ',/extract),2)
   sn = parts[0]
   list = '"source" eq "'+strtrim(sn,2)+'"'
   result=dat_list(s_l,list,/reset,/no_notify)
   jj=where(strtrim(sn,2) eq all_sources)
   j = 0
   if n_elements(parts) eq 1 then begin
      print,'Wrong input format !'
      return,0
   endif
   gc = parts[1]
   if (gc eq 'YES' or gc eq 'yes' or gc eq 'Yes' or gc eq 'Y' or gc eq 'y') then j = 1
   sp[psl].igq = j
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
 endif else begin
   in[pil].sflux=1.
 endelse
   result=dat_list(s_l,/reset)
   goto,again
   endif
   finish:
 
endif
; finished with sma/cfa section

result=dat_list(s_l,'"gq" eq "g"',/reset,/no_notify)
 if result gt 0 then begin
   gai_souids=uti_distinct(in[pil].souid,ndistinct,/many_repeat)
 endif else begin
   gai_souids=-1 & gai_sources='' & gai_amps=0.
 endelse

if gai_souids[0] eq -1 then return,0
if (e.campuslogin eq 'caltech' or e.campuslogin eq 'nick' or e.campuslogin eq 'ovro') then begin
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

endif

use=make_array(n_elements(all_souids),/int)
if n_elements(gai_souids) gt 0 then begin
  for k=0,n_elements(gai_souids)-1 do begin 
   h=where(gai_souids[k] eq all_souids,count) 
   if count gt 0 then use[h]=1 
  endfor
endif

return,n_elements(all_souids)

end
