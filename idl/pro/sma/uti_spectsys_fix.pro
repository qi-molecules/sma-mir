pro uti_spectsys_fix, highlimit=highlimit

common global
common data_set

if not keyword_set(highlimit) then highlimit=1000.

result=dat_filter(s_f,'"pointing" eq "1"',/no_notify, /reset)
if result gt 0 then flag,/flag

result=dat_filter(s_f,'"iband" ne "0" and "wt" gt "0"',/no_notify, /reset)
distinct_iband=uti_distinct(sp[psf].iband,nbands,/many)
distinct_irec=uti_distinct(bl[pbf].irec,nrecs,/many)
for j=0, nrecs-1 do begin
   for i=0, nbands-1 do begin
      tmp1=strcompress(string(distinct_iband[i]),/remove)
      tmp2=strcompress(string(distinct_irec[j]),/remove)
      command='"irec" eq "'+tmp2+'" and "iband" eq "'+tmp1+'" and "wt" gt "0"'
      ;print,command
      result=dat_filter(s_f,command,/no_notify,/reset)
      if result gt 0 then begin
         res=dat_filter(s_f,'"ha" lt "0"',/no_notify)
         if res gt 0 then uti_tsys_fix,high=highlimit,/auto
      endif
      result=dat_filter(s_f,command,/no_notify,/reset)
      if result gt 0 then begin
         res=dat_filter(s_f,'"ha" ge "0"',/no_notify)  
         if res gt 0 then uti_tsys_fix,high=highlimit,/auto
      endif
   endfor
endfor

print,'Done!'
result=dat_filter(s_f,/no_notify,/reset)

end



