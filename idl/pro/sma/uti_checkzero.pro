pro uti_checkzero, sample=sample

common global
common data_set

if not keyword_set(sample) then sample=10

nch=sp[psl].nch
j=where(nch gt 1,count)
ibad=where(abs(ch[pcl[j]+nch[j]/2+sample]) eq 0., count)
if (count gt 0) then begin
   print, 'zero-spectrum points in:'
   for i=0, count-1 do begin
      print,"int#",in[pil[j[ibad[i]]]].int,', baseline ',c.blcd[bl[pbl[j[ibad[i]]]].iblcd],', sideband ',c.sb[bl[pbl[j[ibad[i]]]].isb], ', receiver ',c.rec[bl[pbl[j[ibad[i]]]].irec], ', band ',c.band[sp[psl[j[ibad[i]]]].iband]
   endfor
endif
end
