pro dbi_flux_write, uto,source, souid, pos, flux,snr,freq, quality, comments
common global


sql_statement='select maximum_fluid=max(flu#) from FLU'
result_str=dbi_sql_submit(sql_statement)
fluid=strcompress(string(result_str(2)+1),/remove_all)
source='"'+strcompress(source,/remove_all)+'"'
reid='"'+strcompress(strmid(e.user_name,0,3),/remove_all)+'"'
;parts=strtrim(strsplit(e.start_date,' ',/extract),2L)
;temp=parts(4)
;parts(4)=strmid(parts(3),0,strpos(parts(3),':',/reverse_search))
;parts(3)=temp
;ut='"'
;for i=1L,n_elements(parts)-1 do begin
;ut=ut+parts(i)+' '
;endfor
;ut=ut+'"'
;uto='Jan 1 2002 17:20'
ut='"'+strmid(systime(0),4,3)+' '+strmid(systime(0),8,2)+','+strmid(systime(0),20,4)+' '+strmid(systime(0),11,8)+'"'
uto='"'+uto+'"'
souid=strcompress(string(souid),/remove_all)
pos='"'+pos+'"'
flux=strcompress(string(flux),/remove_all)
snr=strcompress(string(snr),/remove_all)
freq=strcompress(string(freq),/remove_all)
others='0.,0.," ","n","n","n","n","n","n","n","n","n"'
comments='"'+comments+'"'
quality='"'+quality+'"'
sql_statement='insert FLU values('+fluid+' ,'+ $
         reid + ' ,'+ut+' ,'+souid+' ,'+source+' ,'+pos+' ,'+ $
         uto+' ,'+flux+' ,'+snr+' ,'+freq+' ,'+others+' ,' + $
         quality+' ,'+comments+')'
result_str=dbi_sql_submit(sql_statement)
if e.debug then print,result_str

end


