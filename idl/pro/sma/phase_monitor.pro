function phase_monitor,sideband,dataselect

common global
common data_set
common plo

common eng, en

e.debug = 0
pcs = 0
prs = 0

junk = bl.iblcd
distinct_iblcd=junk( uniq(junk,sort(junk)) )
nbaselines = n_elements(distinct_iblcd)
; distinct_iblcd=uti_distinct(bl.iblcd,nbaselines,/many_repeat)
print,'there are ',nbaselines,' baselines:  ',c.blcd[distinct_iblcd]

junk = in.isource
distinct_isource=junk( uniq(junk,sort(junk)) )
nsources = n_elements(distinct_isource)
print,'there are ',nsources,' sources :  ',c.source[distinct_isource]

color_index=0.6*bytscl((indgen(nsources+1) mod 16)+1)+64.
top = 235
;color_index=bytscl(indgen(nsources+1),top=top) + 255 - top
;print,' color index ',color_index

i1 = make_array(nsources,/int)
i2 = make_array(nsources,/int)

scale = fltarr(nsources)
for is = 0, nsources-1 do begin
  scale[is] = 1.
  if (c.source[distinct_isource[is]] eq "jupiter") then scale[is]= 100.
  if (c.source[distinct_isource[is]] eq "saturn" ) then scale[is]= 100.
  if (c.source[distinct_isource[is]] eq "uranus" ) then scale[is]= 6.
  if (c.source[distinct_isource[is]] eq "neptune") then scale[is]= 2.
  if (c.source[distinct_isource[is]] eq "mars"   ) then scale[is]= 50.
  if (c.source[distinct_isource[is]] eq "venus"  ) then scale[is]= 100.
endfor

conv = 1.
if max(bl.phaave) gt 10 then conv = !radeg
if min(bl.phaave) lt -10 then conv = !radeg

;!P.MULTI = [0, 1, nbaselines*2 ]
!P.MULTI = [0, 1, nbaselines ]

x1 = min(in.dhrs+in.iref_time*24)
x2 = max(in.dhrs+in.iref_time*24)
x1 = x1 - .1*(x2-x1)
xr = [x1,x2]
!X.STYLE=3

; this section sets up the plot +++++++++++++++++++++++++++++++++++++++++
for ib = 0, nbaselines-1 do begin

  result=dat_list(s_l,'"blcd" eq "'+ c.blcd[distinct_iblcd[ib]]+'"',/reset,/no_notify)
  result=dat_list(s_l,'"band" eq "s3"',/no_notify)
  result=dat_list(s_l,'"sb" eq "'+sideband+'"',/no_notify)

  if n_elements(in[pil].dhrs) le 1 then return,1

  plot,in[pil].dhrs+in[pil].iref_time*24.,bl[pbl].phaave/conv, $
    psym=1, yrange=[-4.,4.], xrange=xr, /nodata

  ant1 = bl[pbl[0]].itel1
  ant2 = bl[pbl[0]].itel2
  jj = where(en.antenna_number eq ant1)
  kk = where(en.antenna_number eq ant2)

  xyouts, 0.05,1.-(1+4.*ib)/(nbaselines*4.), $
    'Ants '+c.blcd[distinct_iblcd[ib]] ,charsize=10, /NORMAL
  xyouts, 0.05,1.-(1.5+4.*ib)/(nbaselines*4.), $
    'Pads '+strtrim(string(en[jj[0]].pad_number),2)+'-'+strtrim(string(en[kk[0]].pad_number),2),$
     charsize=10, /NORMAL

; finished setting up the plotting boxes, axes, labels
; loop over sources, reselect the data for each source  +++++++++++++++++++++++++++

  for is = 0, nsources-1 do begin
    result=dat_list(s_l,'"blcd" eq "'+ c.blcd[distinct_iblcd[ib]]+'"',/reset,/no_notify)
    result=dat_list(s_l,'"sb" eq "'+sideband+'"',/no_notify)
    result=dat_list(s_l,'"band" eq "s3"',/no_notify)
    result=dat_list(s_l,'"source" eq "'+ c.source[distinct_isource[is]]+'"',/no_notify)

  if dataselect eq 'v' then begin

    jj = where(bl[pbl].ibq eq 1,count)
    if count ne 0 then begin
;      print,'deleted ',n_elements(pbl)-n_elements(jj),' bad data'
;      pil = pil[jj]
;      pbl = pbl[jj]
    endif

  endif

    if is eq 0 then begin
      ya = bl[pbl].ampave/scale[is]
      yp = bl[pbl].phaave/conv
      x = in[pil].dhrs+in[pil].iref_time*24.
      iv = bl[pbl].ibq
      i1[is] = 0
      i2[is] = n_elements(x) - 1
    endif else begin
      ya = [ya,bl[pbl].ampave/scale[is]]
      yp = [yp,bl[pbl].phaave/conv]
      x = [x,in[pil].dhrs+in[pil].iref_time*24.]
      iv = [iv,bl[pbl].ibq]
      i1[is] = i2[is-1] + 1
      i2[is] = n_elements(x) - 1
    endelse

  endfor ; +++++++++++++++++ end of loop over sources


maxamp = max(ya)
minamp = min(ya)
range = maxamp - minamp
;print,' range ',range

; scale the amplitudes to fit into the +- 2pi range 
if (range ne 0.) then begin
  z = (ya - minamp)/range * 2  * !PI - !PI
endif else begin
  z = ya * 0. - !PI
endelse

  j = where(iv ne 1,count)
  if count ne 0 then iv(j) = 6

  if n_elements(x) gt 1 then begin
  for is = 0, nsources-1 do begin
  for i = i1[is],i2[is] do begin
     if dataselect eq 'a' or iv(i) eq 1 then $
       plots,x[i],yp[i], psym=iv[i],color=color_index[is+1 mod 16]
  endfor
  endfor

  q = sort(x)
  x = x(q)
  z = z(q)
  iv = iv(q)

  i = 0
       plots,x[i],z[i], color=color_index[0]
  if dataselect eq 'v' then begin
;  for i = 1L,n_elements(x)-1 do begin
  for i = 1,n_elements(x)-1 do begin
       if iv(i) eq 1 and iv(i-1) eq 1 then begin
       plots,x[i],z[i], color=color_index[0],/continue
       endif else begin
       plots,x[i],z[i], color=color_index[0]
    endelse
  endfor
  endif else begin
;      for i = 1,n_elements(x)-1 do begin
      for i = 1L,n_elements(x)-1 do begin
         plots,x[i],z[i], color=color_index[0],/continue
      endfor
  endelse

  endif


endfor ; ++++++++ end of loop over baselines +++++++++++++++++++++++++++++++++++++

for is = 0, nsources-1 do begin
xyouts, (1.+is)/(nsources+1.),  1.-(0+4.*1)/(nbaselines*4.), $
    c.source[distinct_isource[is]] ,color=color_index[is+1 mod 16],charsize=10, /NORMAL
endfor

end
