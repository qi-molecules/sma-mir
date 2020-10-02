pro uti_c1_fix
common global
common data_set

nc=sp[psl[0]].nch

if nc gt 10 then stop

for i=0,nc-1 do begin

   bandid=strcompress(string(i+1),/remove_all)
   result=dat_list(s_l,'("iband" eq "0" or "iband" eq "'+bandid+'") and "wt" gt "0"',/reset,/no_notify)
   print,'Averaging s',+bandid+'...'
   uti_avgband,c1_order=i+1
endfor

end

