pro write_fits,filename=filename, extension=extension, $
    source=source,band=band,sideband=sideband
;yes
;=Task:WRITE_FITS --- To output data to FITS format
;#Type: conversion
;+Use: 
;     Data can be written to a FITS file with the write_fis command.
;     Here are the two examples:
;        Example1: 
;     write_fits, filename='3c279', extension='UVDATA', source='3c279',
;     band='c1', sideband='l'. In this case the output filename is 
;     '3c279.UVDATA'
;        Example2:
;     write_fits, filename='3c279', source='3c279', band='c1', 
;     sideband='l'. In this case, the output filenamd is '3c279.FITS'. 
;     WRITE_FITS add the extension 'FITS' as a default.
;@filename: 
;     The filename of the output FITS file. It must be enclosed in quotes.
;@source:
;     The source of the output FITS file. It must be enclosed in quotes.
;@band:
;     The chunks to be written out in the FITS file. Choices for the SMA
;     are: c1 or any combination of s1 or s2 or s3 or s4. A combination 
;     is entered as a list. For example, band=['s1','s2']. c1 cannot be 
;     selected in combination with any other band.
;@sideband:
;     'u' : Upper sideband
;     'l' : Lower sideband
;&historyi:
;--------------------------------------------------------------------
;    cykuo 19dec03 Adapting the header
;---------------------------------------------------------------------

if (keyword_set(filename) eq 0) then begin
  print,"Need an output filename. eg: write_fits,filename='mydata'"
  return
endif

if (keyword_set(extension) eq 0) then begin
  extension='FITS'
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





result=fits_out(filename,extension,source,'','',band,sideband,'','')

end
