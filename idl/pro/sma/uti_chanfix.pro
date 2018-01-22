pro uti_chanfix,channel=channel, sample=sample

common global
common data_set

if not keyword_set(sample) then sample=1L

k=where(channel eq 1 or channel ge sp[psl[0]].nch, count)
if count gt 0 then begin
   print,"Channel can't be set to ",channel[k]
   print,"Quit !"
   return
endif

for j = 0L,n_elements(pcl) -1L do begin
   for i=0, n_elements(channel)-1 do begin      
      ptr=pcl[j]+channel[i]-1L
      ch[ptr]=mean([ch[ptr-sample],ch[ptr+sample]])
   endfor
endfor

end
