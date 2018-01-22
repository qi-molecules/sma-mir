pro plot_pseudo_continuum,source=source,sb=sb,band=band,blcd=blcd

common global
common data_set

result=dat_filter(/reset)
result=dat_list(s_l,'"sb" eq "' +sb+ '"')
result=dat_list(s_l,'"band" eq "' +band+ '"')
result=dat_list(s_l,'"blcd" eq "' +blcd+ '"')
result=dat_list(s_l,'"source" eq "' +source+ '"')

dat_get_rows,cmp,amp,pha,x,wts,first,npts,'channel',js,/list,/amp_pha

print,'The number of integrations is ',n_elements(first)

skip = fix(  (1 - 82./104.)*npts/2.  )
;print,'skip channels ',skip

;ncol=!D.N_COLORS/5
;colors=ncol*INDGEN(4)+ncol

red = [0,1,1,0,0,1]
green = [0,1,0,1,0,1]
blue = [0,1,0,0,1,0]
; color 2 = red
;       3 = green
;       4 = blue
;       5 = yellow
 
tvlct, 255*red, 255*green, 255*blue 

avgpha = fltarr(n_elements(first))

for i = 0, n_elements(first)-1 do begin
;print, first[i]+skip[i], first[i]+npts[i]-skip[i]-1
  avgpha[i] = total(  pha[   first[i]+skip[i] : first[i]+npts[i]-skip[i]-1  ])
  avgpha[i] = avgpha[i]/(npts[i]-2.*skip[i])/ !radeg
endfor


avgreal =  fltarr(n_elements(first))
avgimag =  fltarr(n_elements(first))
 
for i = 0, n_elements(first)-1 do begin
  check = 0
  for j  = first[i]+skip[i],  first[i]+npts[i]-skip[i]-1 do begin
      check = check + float(cmp[j])
  endfor
  avgreal[i] = total(  float (cmp[   first[i]+skip[i] : first[i]+npts[i]-skip[i]-1  ])  )
  if check ne avgreal[i] then print,'total real failure ',check,avgreal[i]
  avgreal[i] = check
  avgreal[i] = avgreal[i]/(npts[i]-2.*skip[i])
  check = 0
  for j  = first[i]+skip[i],  first[i]+npts[i]-skip[i]-1 do begin
      check = check + imaginary(cmp[j])
  endfor
  avgimag[i] = total(  imaginary(cmp[   first[i]+skip[i] : first[i]+npts[i]-skip[i]-1  ])  )
  if check ne avgimag[i] then print,'total imag failure ',check,avgimag[i]
  avgimag[i] = check
  avgimag[i] = avgimag[i]/(npts[i]-2.*skip[i])
endfor                                                                                      

phase2 = atan( avgimag,avgreal ) * 180.0 / !PI
amp2 = sqrt(avgimag*avgimag + avgreal*avgreal)
maxamp = max(amp2)
minamp = min(amp2)
range = maxamp - minamp
z2 = (amp2 - minamp)/range *2*!PI - !PI

maxamp = max(bl[pbl].ampave)
minamp = min(bl[pbl].ampave)
range = maxamp - minamp
z = (bl[pbl].ampave - minamp)/range *2*!PI - !PI

if (sb eq 'u') then sidebandstring = 'Upper Sideband' $
else sidebandstring = 'Lower Sideband'

plot,in[pil].dhrs,phase2,psym=3,yrange=[-190,190], ystyle=1,$
xtitle='UT (hours)', ytitle='D e g r e e s', $
title='Baseline '+blcd+' Phase of '+sidebandstring+', Chunk: '+strmid(band,1)
;oplot,in[pil].dhrs,z2,psym=10,color=colors[1]

end
