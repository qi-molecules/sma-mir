function shift_spectrum, data_ch, nc, sh

common data_set
common global

data_shft=complexarr(nc)
half=nc/2.
alldata=fft(data_ch)
for i=1L, nc do begin
   if i le half then phi=(i-1)*!pi*(-1.)*sh/half else phi=(i-nc-1)*!pi*(-1.)*sh/half
   cphi=complex(cos(phi),sin(phi))
   data_shft(i-1)=alldata(i-1)*cphi
endfor
return, fft(data_shft,/inverse)
;return, fft(alldata,/inverse)
end


      
