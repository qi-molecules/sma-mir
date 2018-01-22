pro mir_fits
common global
common data_set

result=dat_list(s_l,'"wt" gt "0"',/reset,/no_notify)
distinct_source=uti_distinct(c.source[in[pil].isource],nsources, /many_repeat)
sources=''
for i=0,nsources-1L do begin
sources=sources+distinct_source[i]+' '
endfor

temp=' '
read,temp,prompt='Enter File Name [test]: '
if temp eq '' then filename='test' else filename=temp


;source=strarr(ii)
print,'Select sources ('+sources+') below:'
sources=''
j=0
for i=0,nsources-1 do begin
;read,temp,prompt='Source('+strcompress(string(i+1),/remove_all)+')'+' '+distinct_source[i]+']: '
read,temp,prompt='   Output '+ distinct_source[i]+' ? '+'Y/[N]:'
if (temp eq 'Y' or temp eq 'y') then begin
  j=j+1
  if (j eq 1) then sources=distinct_source[i] else sources=[sources,distinct_source[i]]
endif
endfor
if sources[0] eq '' then begin
  print, " NO source selected ! EXIT! "
 return
endif

distinct_band=uti_distinct(c.band[sp[psl].iband],nbands,/many_repeat)
bands=''

j=0
print, 'Select bands below:'
for i=0,nbands-1 do begin
;read,temp,prompt='Band Name('+strcompress(string(i+1),/remove_all)+'): '
read,temp,prompt='   Output '+distinct_band(i)+' ? '+'Y/[N]:'
if (temp eq 'Y' or temp eq 'y') then begin
  j=j+1
  if (j eq 1) then bands=distinct_band[i] else bands=[bands,distinct_band[i]] 
endif
endfor

if bands[0] eq '' then begin
  print, " NO band selected ! EXIT! "
 return
endif

if (n_elements(bands) gt 1) then begin
  print
  print,"Multiple bands might be possible, if they"
  print,"all have the same number of channels."
  print,"Continuum and spectra together won't work."
  print
endif


distinct_sb=uti_distinct(c.sb[bl[pbl].isb],nsbs,/many_repeat)
sideband=''
;nsbs=2
;distinct_sb=['u','l']
j=0
print, 'Select sideband below:'
for i=0,nsbs-1 do begin
read,temp,prompt='   Output '+ distinct_sb(i)+' ? '+'Y/[N]:'
if (temp eq 'Y' or temp eq 'y') then begin
  j=j+1
  if (j eq 1) then begin 
    sideband=distinct_sb[i] 
  endif else begin
    print,"Only one sideband at a time ! EXIT !"
    return
  endelse 
endif
endfor

if sideband eq '' then begin
  print, " NO sideband selected ! EXIT! "
 return
endif

result=fits_out(filename,'UVF',sources,'','',bands,sideband,'','')

end



