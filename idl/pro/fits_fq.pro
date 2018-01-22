function fits_fq,no_frqid,no_if,freqoffset,chwidth,bandwidth,sideband

common global
common data_set
common plo
common fits,str_buf,unit

nt = strtrim(string(no_if),2)
blanks = '      '
if (no_if gt 9) then blanks = '     '
wbytes = 4 + no_if*20

res=fits_buff('',/init)
result=fits_wstr('XTENSION','BINTABLE','Extension type')
result=fits_wint('BITPIX  ',8,'Binary data') 
result=fits_wint('NAXIS   ',2,'Table is a matrix ')
result=fits_wint('NAXIS1  ',wbytes,'Width of table in bytes')
result=fits_wint('NAXIS2  ',no_frqid,'Number of entries in table ')
result=fits_wint('PCOUNT  ',0,'Random parameter count')
result=fits_wlog('GCOUNT  ','1','Group count')
result=fits_wint('TFIELDS ',5,'Number of fields in each row')
result=fits_wstr('EXTNAME ','AIPS FQ','AIPS table file')
result=fits_wint('EXTVER  ',1,'Version number of table')


result=fits_wstr('TFORM1','1J      ',        'Fortran format of field 1')
result=fits_wstr('TTYPE1','FRQSEL          ','Type (heading) of field 1')
result=fits_wstr('TUNIT1','        ',        'Physical units of field 1')

result=fits_wstr('TFORM2',nt+'D'+blanks,     'Fortran format of field 2')
result=fits_wstr('TTYPE2','IF FREQ         ','Type (heading) of field 2')
result=fits_wstr('TUNIT2','HZ      ',        'Physical units of field 2')

result=fits_wstr('TFORM3',nt+'E'+blanks,     'Fortran format of field 3')
result=fits_wstr('TTYPE3','CH WIDTH        ','Type (heading) of field 3')
result=fits_wstr('TUNIT3','HZ      ',        'Physical units of field 3')

result=fits_wstr('TFORM4',nt+'E'+blanks,     'Fortran format of field 4')
result=fits_wstr('TTYPE4','TOTAL BANDWIDTH ','Type (heading) of field 4')
result=fits_wstr('TUNIT4','HZ      ',        'Physical units of field 4')

result=fits_wstr('TFORM5',nt+'J'+blanks,     'Fortran format of field 5')
result=fits_wstr('TTYPE5','SIDEBAND        ','Type (heading) of field 5')
result=fits_wstr('TUNIT5','        ',        'Physical units of field 5')

result=fits_wint ('NO_IF ',no_if,' ')

result=fits_wend()

;print,'sideband array',sideband

for i = 0, no_frqid-1 do begin
  j  = long(i+1)  & byteorder,j,/HTONL &  writeu,unit,j
  for k = 0, no_if-1 do begin 
    d = freqoffset[i,k] & byteorder,d,/DTOXDR  & writeu,unit,d
  endfor
  for k = 0, no_if-1 do begin 
    f  = chwidth[i,k] & byteorder,f,/FTOXDR & writeu,unit,f
  endfor
  for k = 0, no_if-1 do begin 
    f  = bandwidth[i,k]  & byteorder,f,/FTOXDR & writeu,unit,f
  endfor
  for k = 0, no_if-1 do begin 
    j  = long(sideband[i,k])  & byteorder,j,/HTONL &  writeu,unit,j
;    print,'sideband j',j
  endfor
endfor

for i = 1, 2880 - no_frqid*wbytes do begin
  j = byte(0) & writeu,unit,j
endfor


return,1
end



