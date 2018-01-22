pro uti_avgrx

common global
common data_set

print,'No filter setting needed ... '
result=uti_distinct(bl.iblcd,nblcd,/many)

select,band='c1',/reset
ints=in[pif].int
ii=uti_distinct(ints,nint,/many_repeat)

for i=0L,nint-1L do begin
 
   tmp_idx = where(ints eq ii[i], ncombo)

   if ncombo ne nblcd*4 then begin
      print,'No consistent RX data and no average is done in int#', ii[i]
   endif else begin
      carray=ch[pcf[tmp_idx]]
      newcarray=reform(carray,nblcd*2,2)
      avgcarray=mean(newcarray,dim=2)
;      avgcarray=avg(newcarray,1,/double)
      avgcarray2=reform([[avgcarray],[avgcarray]],ncombo)
      uti_conv_apc,avgcarray2,avgamp,avgpha,/amp_pha
      bl[pbf[tmp_idx]].blhdbl2=avgamp
   endelse
endfor

print,'Receiver continuum vector-averaged values stored in blhdbl2.'
end
      

