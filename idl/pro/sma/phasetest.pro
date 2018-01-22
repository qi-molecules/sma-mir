pro phasetest,source=source,sb=sb,band=band,blcd=blcd

;usage:
; phasetest,source='mars',sb='l',blcd='4-9',band='s3'
; phasetest,source='mars',sb='u',blcd='1-4',band=['s1','s2','s3','s4']

common global
common data_set

red = [0,1,1,0,0,1]
green = [0,1,0,1,0,1]
blue = [0,1,0,0,1,0]
; color 2 = red
;	3 = green
;	4 = blue
;	5 = yellow

tvlct, 255*red, 255*green, 255*blue

for k = 0,n_elements(band)-1 do begin

result=dat_list(/reset)
if keyword_set(sb) then begin
  print,'select sideband ',sb
  result=dat_list(s_l,'"sb" eq "' +sb+ '"')
endif
if keyword_set(band) then begin
  print,'select band ',band[k]
  result=dat_list(s_l,'"band" eq "' +band[k]+ '"') 
endif
if keyword_set(blcd) then begin
  print,'select baseline ',blcd
  result=dat_list(s_l,'"blcd" eq "' +blcd+ '"')
endif
if keyword_set(source) then begin
  print,'select source ',source
  result=dat_list(s_l,'"source" eq "' +source+ '"')
endif

dat_get_rows,cmp,amp,pha,x,wts,first,npts,'channel',js,/list,/amp_pha

print,'The number of points is ',n_elements(first)

skip = fix(  (1 - 82./104.)*npts/2.  )
;print,'skip channels ',skip

if (k eq 0) then begin
  avgreal =  fltarr(n_elements(first))
  avgimag =  fltarr(n_elements(first))
endif
 
for i = 0, n_elements(first)-1 do begin
  avgreal[i] = avgreal[i] + total(  float (cmp[   first[i]+skip[i] : first[i]+npts[i]-skip[i]  ])  )/(npts[i]-2.*skip[i] + 1)
  avgimag[i] = avgimag[i] + total(  imaginary(cmp[   first[i]+skip[i] : first[i]+npts[i]-skip[i]  ])  )/(npts[i]-2.*skip[i] + 1)
endfor                                                                                      
endfor

avgreal = avgreal/n_elements(band)
avgimag = avgimag/n_elements(band)

print,'number of points to plot',n_elements(avgreal)

phase2 = atan( avgimag,avgreal )
amp2 = sqrt(avgimag*avgimag + avgreal*avgreal) 
maxamp = max(amp2)
minamp = min(amp2)
range = maxamp - minamp
z2 = (amp2 - minamp)/range *2*!PI - !PI

maxamp = max(bl[pbl].ampave)
minamp = min(bl[pbl].ampave)
range = maxamp - minamp
z = (bl[pbl].ampave - minamp)/range *2*!PI - !PI

minamp = min([amp2,bl[pbl].ampave])
maxamp = max([amp2,bl[pbl].ampave])

;print,'bl[pbl].phaave'
;print,bl[pbl].phaave
;print,'phase2'
;print,phase2*180./!PI
;print,'weight',sp[psl].wt

if (sb eq 'u') then sidebandstring = 'Upper Sideband' $
else sidebandstring = 'Lower Sideband'   

bandstring = strmid(band[0],1)
if (n_elements(band) gt 1) then begin
  for i = 1,n_elements(band)-1 do begin
    bandstring = bandstring + ' ' + strmid(band[i],1)
  endfor
endif

 !p.multi=[0,1,2]
 plot,in[pil].dhrs,bl[pbl].phaave*!PI/180.,yrange=[-4.,4.],/nodata $
,xtitle='UT (hours)', ytitle='D e g r e e s', $
title='Baseline '+blcd+' Phase of '+sidebandstring+', Chunks: '+ bandstring $
,color=1
 oplot,in[pil].dhrs,bl[pbl].phaave*!PI/180.,psym=1,color=2
 oplot,in[pil].dhrs,phase2,psym=4,color=3

 plot,in[pil].dhrs,bl[pbl].ampave,yrange=[minamp,maxamp],/nodata $
,xtitle='UT (hours)', ytitle='A m p l i t u d e', $
title='Baseline '+blcd+' Amplitude of '+sidebandstring+', Chunks: '+ bandstring $
,color=1
 oplot,in[pil].dhrs,bl[pbl].ampave,psym=1,color=2
 oplot,in[pil].dhrs,amp2,psym=4,color=3

xyouts, 0.75, 0.5, 'Diamonds: visibility data',color=3,charsize=10,/NORMAL
xyouts, 0.25, 0.5, 'Crosses: header data',color=2,charsize=10,/NORMAL

end
