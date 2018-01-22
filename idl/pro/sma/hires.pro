pro hires, band=band

common global
common data_set

iband=fix(strmid(band,1))
band2=strcompress(string(iband+4),/remove)
if (iband+4) lt 10 then band2='s0'+band2 else band2='s'+band2
print,'This code only works for hi-res data in adjacent blocks.'
print,'Averaging ',band, ' and ',band2
print,' and storing the result in ',band,'...'

command='"band" eq "'+band+'" and "wt" gt "0"'
print,command
result=dat_list(s_l,command,/reset,/no_notify)

if result le 0 then begin
   print,'No data selected !'
   print,'Quit !'
   return
endif

for i=0L, n_elements(psl)-1L do begin

   if (sp[psl[i]+1].wt gt 0) then begin 
      ch[pcl[i]:(pcl[i]+2047L)]= (ch[pcl[i]:(pcl[i]+2047L)]+ch[(pcl[i]+2048L):(pcl[i]+2047L+2048L)])/2.
   endif
endfor
print,'Done!'
result=dat_list(s_l,/reset)
end

