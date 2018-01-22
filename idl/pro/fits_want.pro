function fits_want,ns,ver,stnum,stname,stx,sty,stz,freq0,datobs,site
;-----------------------------------------------------------------------
; Append AIPS antenna table as a FITS table extension to the FITS file.
;
; Arguments:
;  ns      (input, I*4)       Number of antennas.
;  ver     (input, i*4)       version number of table.
;  stnum   (input, I*4 array) Numbers of antennas
;  stname  (input, C*8 array) Names of antennas.
;  stx     (input, R*8 array) Antenna x-coordinates (m).
;  sty     (input, R*8 array) Antenna y-coordinates (m).
;  stz     (input, R*8 array) Antenna z-coordinates (m).
;-----------------------------------------------------------------------
;
common fits,str_buf,unit

wbytes = 70

res=fits_buff('',/init)
result=fits_wstr('XTENSION','BINTABLE','Extension type')
result=fits_wint('BITPIX  ',8,'Binary data')
result=fits_wint('NAXIS   ',2,'Table is a matrix ')  
result=fits_wint ('NAXIS1', wbytes, 'WIDTH OF TABLE IN BYTES')
result=fits_wint ('NAXIS2', ns, 'NUMBER OF ENTRIES IN TABLE')
result=fits_wint ('PCOUNT', 0, 'NO RANDOM PARAMETERS')
result=fits_wint ('GCOUNT', 1, 'GROUP COUNT')
result=fits_wint ('TFIELDS', 12, 'NUMBER OF FIELDS IN EACH ROW')
result=fits_wstr ('EXTNAME', 'AIPS AN', 'AIPS ANTENNA TABLE')
result=fits_wint ('EXTVER',ver, 'VERSION NUMBER OF TABLE')

result=fits_wstr('TFORM1','8A      ',        'Fortran format of field 1')
result=fits_wstr('TTYPE1','ANNAME          ','Type (heading) of field 1')
result=fits_wstr('TUNIT1','        ',        'Physical units of field 1') 

result=fits_wstr('TFORM2','3D      ',        'Fortran format of field 2')
result=fits_wstr('TTYPE2','STABXYZ         ','Type (heading) of field 2')
result=fits_wstr('TUNIT2','METERS  ',        'Physical units of field 2') 

result=fits_wstr('TFORM3','1J      ',        'Fortran format of field 4')
result=fits_wstr('TTYPE3','NOSTA           ','Type (heading) of field 4')
result=fits_wstr('TUNIT3','        ',        'Physical units of field 4')
 
result=fits_wstr('TFORM4','1J      ',        'Fortran format of field 5')
result=fits_wstr('TTYPE4','MNTSTA           ','Type (heading) of field 5')
result=fits_wstr('TUNIT4','        ',        'Physical units of field 5')
 
result=fits_wstr('TFORM5','0D      ',        'Fortran format of field 3')
result=fits_wstr('TTYPE5','ORBPARM         ','Type (heading) of field 3')
result=fits_wstr('TUNIT5','        ',        'Physical units of field 3')

result=fits_wstr('TFORM6','1E      ',        'Fortran format of field 6')
result=fits_wstr('TTYPE6','STAXOF          ','Type (heading) of field 6')
result=fits_wstr('TUNIT6','METERS  ',        'Physical units of field 6')
 
result=fits_wstr('TFORM7','1A      ',        'Fortran format of field 7')
result=fits_wstr('TTYPE7','POLTYA          ','Type (heading) of field 7')
result=fits_wstr('TUNIT7','DEGREES ',        'Physical units of field 7')
 
result=fits_wstr('TFORM8','1E      ',        'Fortran format of field 8')
result=fits_wstr('TTYPE8','POLAA           ','Type (heading) of field 8')
result=fits_wstr('TUNIT8','DEGREES ',        'Physical units of field 8')
 
result=fits_wstr('TFORM9','2E      ',        'Fortran format of field 9')
result=fits_wstr('TTYPE9','POLCALA         ','Type (heading) of field 9')
result=fits_wstr('TUNIT9','        ',        'Physical units of field 9')
 
result=fits_wstr('TFORM10','1A      ',        'Fortran format of field 10')
result=fits_wstr('TTYPE10','POLTYB          ','Type (heading) of field 10')
result=fits_wstr('TUNIT10','        ',        'Physical units of field 10')
 
result=fits_wstr('TFORM11','1E      ',        'Fortran format of field 11')
result=fits_wstr('TTYPE11','POLAB           ','Type (heading) of field 11')
result=fits_wstr('TUNIT11','DEGREES ',        'Physical units of field 11')
 
result=fits_wstr('TFORM12','2E      ',        'Fortran format of field 12')
result=fits_wstr('TTYPE12','POLCALB         ','Type (heading) of field 12')
result=fits_wstr('TUNIT12','        ',        'Physical units of field 12') 


; The array center coordinates have the X axis pointing along
; longitude 0.0 (Greenwich and East). This is different from the
; convention used in STABXYZ above where the X axis points along
; 0 hour angle and the Y axis points along -6 HA (East). 
; The difference between the 2 systems is that X and Y are interchanged
; and the sign of Y is reversed.

if (site eq 'saosma') then begin
; This is the SMA Pad 1 location.
  result=fits_wdble('ARRAYX',-5.46242840757d6,'')
  result=fits_wdble('ARRAYY',-2.49196045618d6,'')
  result=fits_wdble('ARRAYZ', 2.28652665343d6,'')
endif 
if (site eq 'ovromma') then begin
; This is the OVRO VLBA site which is not quite the
; correct location for the reference point of the 
; millimeter array.
  result=fits_wdble('ARRAYX',-2.40612582165d6,'')
  result=fits_wdble('ARRAYY',-4.47312856557d6,'')
  result=fits_wdble('ARRAYZ', 3.85876003303d6,'')
endif

;result=fits_wdble('GSTIAO',0.d0,'')
;result=fits_wdble('DEGPDY',0.d0,'')
result=fits_wdble('FREQ  ',freq0,'')
;result=fits_wstr ('RDATE ',datobs,'')
;result=fits_wdble('POLARX',0.d0,'')
;result=fits_wdble('POLARY',0.d0,'')
;result=fits_wdble('UT1UTC',0.d0,'')
;result=fits_wstr ('TIMSYS ','UTC     ','')
;result=fits_wint ('NUMORB',0,'')
;result=fits_wint ('NOPCAL',0,'')
;result=fits_wint ('NUMORB',0,'')
;result=fits_wint ('FREQID',-1,'')
;result=fits_wdble('IATUTC',0.d0,'')


if (site eq 'saosma') then  result=fits_wstr('ARRNAM ','SAO SMA ','')
if (site eq 'ovromma') then result=fits_wstr('ARRNAM ','OVRO MMA','')

result=fits_wend ()

print,'stnum  ',stnum
print,'stname ',stname

m = ' ' 

for i=0,ns-1 do begin

;ANNAME
  nchars = strlen(stname[i]) 
  for k = 0,nchars-1 do begin
      writeu,unit,strupcase(strmid(stname[i],k,1))
  endfor
  if (nchars lt 8) then begin
    for k = nchars,7 do begin
      writeu,unit,m
    endfor
  endif   

;STABXYZ
    d =  double(stx[i])  & byteorder,d,/DTOXDR  & writeu,unit,d
    d =  double(sty[i])  & byteorder,d,/DTOXDR  & writeu,unit,d
    d =  double(stz[i])  & byteorder,d,/DTOXDR  & writeu,unit,d

;NOSTA
  j  = long(stnum[i])  & byteorder,j,/HTONL &  writeu,unit,j


;MNTSTA
  j  = 0L              & byteorder,j,/HTONL &  writeu,unit,j

;ORBPARM
;    d = 0.d0 & byteorder,d,/DTOXDR  & writeu,unit,d

;STXOF
    f  = 0.0          & byteorder,f,/FTOXDR  & writeu,unit,f

;POLTYA
    writeu,unit,'R'  

;POLAA
    f  = 0.0          & byteorder,f,/FTOXDR  & writeu,unit,f

;POLCALA
    f  = 0.0          & byteorder,f,/FTOXDR  & writeu,unit,f
    f  = 0.0          & byteorder,f,/FTOXDR  & writeu,unit,f

;POLTYB
    writeu,unit,'L'

;POLAB
    f  = 0.0          & byteorder,f,/FTOXDR  & writeu,unit,f

;POLCALB
    f  = 0.0          & byteorder,f,/FTOXDR  & writeu,unit,f
    f  = 0.0          & byteorder,f,/FTOXDR  & writeu,unit,f


endfor



for i = 1,2880 - wbytes*ns do begin
  j = byte(0) & writeu,unit,j
endfor  



end


