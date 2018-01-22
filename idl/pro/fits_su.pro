function fits_su,no_if,nsources,source,suid,qual,calcode,iflux, $
    raepo,decepo,epoch,lsrvel,restfreq,veldef,veltyp

common global
common data_set
common plo
common fits,str_buf,unit

wbytes = 92 + no_if*40
nt = strtrim(string(no_if),2)
blanks = '      '
if (no_if gt 9) then blanks = '     '

res=fits_buff('',/init)
result=fits_wstr('XTENSION','BINTABLE','Extension type')
result=fits_wint('BITPIX  ',8,'Binary data') 
result=fits_wint('NAXIS   ',2,'Table is a matrix ')
result=fits_wint('NAXIS1  ',wbytes,'Width of table in bytes')
result=fits_wint('NAXIS2  ',nsources,'Number of entries in table ')
result=fits_wint('PCOUNT  ',0,'Random parameter count')
result=fits_wlog('GCOUNT  ','1','Group count')
result=fits_wint('TFIELDS ',19,'Number of fields in each row')
result=fits_wstr('EXTNAME ','AIPS SU','AIPS table file')
result=fits_wint('EXTVER  ',1,'Version number of table')


result=fits_wstr('TFORM1','1J      ',        'Fortran format of field 1')
result=fits_wstr('TTYPE1','ID. NO.         ','Type (heading) of field 1')
result=fits_wstr('TUNIT1','        ',        'Physical units of field 1')

result=fits_wstr('TFORM2','16A     ',        'Fortran format of field 2')
result=fits_wstr('TTYPE2','SOURCE          ','Type (heading) of field 2')
result=fits_wstr('TUNIT2','        ',        'Physical units of field 2')

result=fits_wstr('TFORM3','1J      ',        'Fortran format of field 3')
result=fits_wstr('TTYPE3','QUAL            ','Type (heading) of field 3')
result=fits_wstr('TUNIT3','        ',        'Physical units of field 3')

result=fits_wstr('TFORM4','4A      ',        'Fortran format of field 4')
result=fits_wstr('TTYPE4','CALCODE         ','Type (heading) of field 4')
result=fits_wstr('TUNIT4','        ',        'Physical units of field 4')

result=fits_wstr('TFORM5',nt+'E'+blanks,     'Fortran format of field 5')
result=fits_wstr('TTYPE5','IFLUX           ','Type (heading) of field 5')
result=fits_wstr('TUNIT5','JY      ',        'Physical units of field 5')

result=fits_wstr('TFORM6',nt+'E'+blanks,     'Fortran format of field 6')
result=fits_wstr('TTYPE6','QFLUX           ','Type (heading) of field 6')
result=fits_wstr('TUNIT6','JY      ',        'Physical units of field 6')

result=fits_wstr('TFORM7',nt+'E'+blanks,     'Fortran format of field 7')
result=fits_wstr('TTYPE7','UFLUX           ','Type (heading) of field 7')
result=fits_wstr('TUNIT7','JY      ',        'Physical units of field 7')

result=fits_wstr('TFORM8',nt+'E'+blanks,     'Fortran format of field 8')
result=fits_wstr('TTYPE8','VFLUX           ','Type (heading) of field 8')
result=fits_wstr('TUNIT8','JY      ',        'Physical units of field 8')

result=fits_wstr('TFORM9',nt+'D'+blanks,     'Fortran format of field 9')
result=fits_wstr('TTYPE9','FREQOFF         ','Type (heading) of field 9')
result=fits_wstr('TUNIT9','HZ      ',        'Physical units of field 9')

result=fits_wstr('TFORM10','1D      ',        'Fortran format of field 10')
result=fits_wstr('TTYPE10','BANDWIDTH       ','Type (heading) of field 10')
result=fits_wstr('TUNIT10','HZ      ',        'Physical units of field 10')

result=fits_wstr('TFORM11','1D      ',        'Fortran format of field 11')
result=fits_wstr('TTYPE11','RAEPO           ','Type (heading) of field 11')
result=fits_wstr('TUNIT11','DEGREES ',        'Physical units of field 11')

result=fits_wstr('TFORM12','1D      ',        'Fortran format of field 12')
result=fits_wstr('TTYPE12','DECEPO          ','Type (heading) of field 12')
result=fits_wstr('TUNIT12','DEGREES ',        'Physical units of field 12')

result=fits_wstr('TFORM13','1D      ',        'Fortran format of field 13')
result=fits_wstr('TTYPE13','EPOCH           ','Type (heading) of field 13')
result=fits_wstr('TUNIT13','YEARS   ',        'Physical units of field 13')

result=fits_wstr('TFORM14','1D      ',        'Fortran format of field 14')
result=fits_wstr('TTYPE14','RAAPP           ','Type (heading) of field 14')
result=fits_wstr('TUNIT14','DEGREES ',        'Physical units of field 14')

result=fits_wstr('TFORM15','1D      ',        'Fortran format of field 15')
result=fits_wstr('TTYPE15','DECAPP          ','Type (heading) of field 15')
result=fits_wstr('TUNIT15','DEGREES ',        'Physical units of field 15')

result=fits_wstr('TFORM16',nt+'D'+blanks,     'Fortran format of field 16')
result=fits_wstr('TTYPE16','LSRVEL          ','Type (heading) of field 16')
result=fits_wstr('TUNIT16','M/SEC   ',        'Physical units of field 16')

result=fits_wstr('TFORM17',nt+'D'+blanks,     'Fortran format of field 17')
result=fits_wstr('TTYPE17','RESTFREQ        ','Type (heading) of field 17')
result=fits_wstr('TUNIT17','HZ      ',        'Physical units of field 17')

result=fits_wstr('TFORM18','1D      ',        'Fortran format of field 18')
result=fits_wstr('TTYPE18','PMRA            ','Type (heading) of field 18')
result=fits_wstr('TUNIT18','DEG/DAY ',        'Physical units of field 18')

result=fits_wstr('TFORM19','1D      ',        'Fortran format of field 19')
result=fits_wstr('TTYPE19','PMDEC           ','Type (heading) of field 19')
result=fits_wstr('TUNIT19','DEG/DAY ',        'Physical units of field 19')

result=fits_wint('NO_IF   ',no_if,' ')
result=fits_wstr('VELTYP  ',veltyp,' ')
result=fits_wstr('VELDEF  ',veldef,' ')
result=fits_wint('FREQID  ',-1,' ')

result=fits_wend()

m = ' '

for i = 0, nsources-1 do begin

; ID_NO
  j  = long(suid[i])    & byteorder,j,/HTONL &  writeu,unit,j

; SOURCE
  nchars = strlen(source[i])
  for k = 0,nchars-1 do begin
      writeu,unit,strupcase(strmid(source[i],k,1))
  endfor
  if (nchars lt 16) then begin
    for k = nchars,15 do begin
      writeu,unit,m
    endfor
  endif

; QUAL
  j  = long(qual(i))  & byteorder,j,/HTONL &  writeu,unit,j

; CALCODE
  nchars = strlen(calcode[i])
  for k = 0,nchars-1 do begin
      writeu,unit,strmid(calcode[i],k,1)
  endfor
  if (nchars lt 4) then begin
    for k = nchars,3 do begin
      writeu,unit,m
    endfor
  endif

; IFLUX
  for n = 0,no_if-1 do begin
    f  = iflux[i]     & byteorder,f,/FTOXDR  & writeu,unit,f
  endfor
; QFLUX
  for n = 0,no_if-1 do begin
    f  = 0.0          & byteorder,f,/FTOXDR  & writeu,unit,f
  endfor
; UFLUX
  for n = 0,no_if-1 do begin
    f  = 0.0          & byteorder,f,/FTOXDR  & writeu,unit,f
  endfor
; VFLUX
  for n = 0,no_if-1 do begin
    f  = 0.0          & byteorder,f,/FTOXDR  & writeu,unit,f
  endfor

; FREQOFF
  for n = 0,no_if-1 do begin
    d =  double(0.0)  & byteorder,d,/DTOXDR  & writeu,unit,d
  endfor

; BANDWIDTH
  d =  double(0.0)  & byteorder,d,/DTOXDR  & writeu,unit,d

  d =  raepo[i]     & byteorder,d,/DTOXDR  & writeu,unit,d
  d =  decepo[i]    & byteorder,d,/DTOXDR  & writeu,unit,d
  d =  epoch[i]     & byteorder,d,/DTOXDR  & writeu,unit,d
  d =  raepo[i]     & byteorder,d,/DTOXDR  & writeu,unit,d
  d =  decepo[i]    & byteorder,d,/DTOXDR  & writeu,unit,d
  for n = 0,no_if-1 do begin
    d =  lsrvel[i]    & byteorder,d,/DTOXDR  & writeu,unit,d
  endfor
  for n = 0,no_if-1 do begin
    d =  restfreq[i]  & byteorder,d,/DTOXDR  & writeu,unit,d
  endfor

; PMRA
  d =  double(0.0)  & byteorder,d,/DTOXDR  & writeu,unit,d
; PMDEC
  d =  double(0.0)  & byteorder,d,/DTOXDR  & writeu,unit,d
endfor

for i = 1,2880 - wbytes*nsources do begin
  j = byte(0) & writeu,unit,j
endfor


return,1
end




