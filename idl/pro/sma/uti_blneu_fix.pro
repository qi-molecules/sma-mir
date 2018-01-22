pro uti_blneu_fix, antennasFile

common global
common data_set

openr,unit,antennasFile,/get_lun
data=dblarr(4,10)
line=dblarr(4)
index=0
print, 'Antenna positions used:'
while not EOF(unit) do begin
    readf,unit,line
    data[*,index]=line
    print,line
    index=index+1
endwhile
free_lun,unit
print, 'There are ',index,' antennas in file ',antennasFile
;print, 'Antenna positions used:'
;print,data
distinct_blcd=uti_distinct(c.blcd[bl[pbl].iblcd],nblcds,/many_repeat)

for i=0L,nblcds-1L do begin
   ant=fix(strtrim(strsplit(distinct_blcd[i],'-',/extract),2))
   xyz=data[*,ant[0]-1]-data[*,ant[1]-1]
;   print,xyz
   xyz=xyz[1:3]
   lat=19.82420526391d/57.29577951
   m1=[[-sin(lat),0,cos(lat)],[0,1,0],[cos(lat),0,sin(lat)]]
   neu=xyz##m1
;   print,neu
   result=dat_list(s_l,'"blcd" eq "' +distinct_blcd[i]+'"',/reset,/no_notify)
   bl[pbl].bln=neu[0]
   bl[pbl].ble=neu[1]
   bl[pbl].blu=neu[2]
endfor

print,'BL positions fixed.'
result=dat_list(s_l,/reset)
end
