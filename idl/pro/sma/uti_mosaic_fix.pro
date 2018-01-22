pro uti_mosaic_fix, source=source

common global
common data_set
; converge all the mosaic source names to the target source name

if not keyword_set(source) then begin
   print,'Please provide the mosaic target source name as'
   print,' IDL> uti_mosaic_fix, source="xx"'
   print,'Quit!'
   return
endif

;select,source=source,/reset
result=dat_filter(s_f,'"source" like "'+source+'"',/no_notify,/reset)
if result eq 0 then begin
   print,'No such source found in data: ',source
   print,'Quit!'
   return
endif

temp=uti_distinct(in[pif].isource,nsources,/many)
c.source[temp]=''
in[pif].isource=0
in[pif].souid=0
c.source[0]=source

print, 'All mosaic source names have been converged to ',source 

end

