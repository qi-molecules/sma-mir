pro write_fits,filename=filename, $
    source=source,band=band,sideband=sideband


if (keyword_set(filename) eq 0) then begin
  print,"Need an output filename. eg: write_fits,filename='mydata'"
  return
endif

if (keyword_set(source) eq 0) then begin
  print,"Need a source name. eg: write_fits,source='pluto'"
  return
endif

if (keyword_set(band) eq 0) then begin
  print,"Need a band. eg: write_fits,band='c1'"
  return
endif

if (keyword_set(sideband) eq 0) then begin
  print,"Need a sideband. eg: write_fits,sideband='u'"
  return
endif

;if (n_elements(sideband) gt 1) then begin
;  print,"Only one sideband at a time"
;  return
;endif

if (n_elements(band) gt 1) then begin
  print,"Multiple bands might be possible, if they"
  print,"all have the same number of channels."
  print,"Continuum and spectra together won't work."
endif





result=fits_out(filename,'',source,'','',band,sideband,'','')

end
