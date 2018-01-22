pro gc,source=source,sb=sb,band=band,blcd=blcd

common global
common data_set

result=dat_filter(/reset)
result=dat_list(s_l,'"sb" eq "' +sb+ '"')
result=dat_list(s_l,'"band" eq "s3" ') 
;result=dat_list(s_l,'"band" eq "s4" or "band" eq "s1" or "band" eq "s2" or "band" eq "s3"') 
result=dat_list(s_l,'"blcd" eq "' +blcd+ '"')
result=dat_list(s_l,'"source" eq "' +source+ '"')

dat_get_rows,cmp,amp,pha,x,wts,first,npts,'channel',js,/list,/amp_pha

print,'The number of integrations is ',n_elements(first)

skip = fix(  (1 - 82./104.)*npts/2.  )
;print,'skip channels ',skip


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

print,'bl[pbl].phaave'
print,bl[pbl].phaave
print,'phase2'
print,phase2

 !p.multi=[0,1,2]
 plot,in[pil].dhrs,bl[pbl].phaave*!PI/180.,psym=1,yrange=[-4.,4.]
 oplot,in[pil].dhrs,phase2,psym=4
; plot,in[pil].dhrs,bl[pbl].phaave,psym=2,yrange=[-4.,4.]
; oplot,in[pil].dhrs,z,psym=10

 plot,in[pil].dhrs,bl[pbl].ampave,psym=1,yrange=[0.,1.2e4]
 oplot,in[pil].dhrs,amp2,psym=4

end
