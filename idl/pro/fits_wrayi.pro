function fits_wrayi,idata,flush=flush
;
;     Write out as signed 32 bit integers.  
;
; eg. result=fits_wrayi,idata
;
common fits,str_buf,unit
if keyword_set(init) then begin
  idata[*]=0
  return,1
endif
npt=n_elements(idata)
;print,'npt in idata ',npt
nwrite=long(ceil(1.*npt/720))
; print,'nwrite ',nwrite
ntot=nwrite*720
;print,'ntot ',ntot
if ntot gt npt then idata=[idata,make_array(ntot-npt,/int)]
;print,'idata now ',n_elements(idata)
if keyword_set(flush) then begin
  for i=1L,nwrite do begin
;print,'i start end ',i,(i-1L)*720L,i*720L-1L
    writeu,unit,idata[(i-1L)*720L:i*720L-1L]
;    if (i eq 1) then print,'idata ',idata[(i-1L)*720L:i*720L-1L]
  endfor
endif
return,1
end
