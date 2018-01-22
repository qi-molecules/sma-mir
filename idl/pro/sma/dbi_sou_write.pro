pro dbi_sou_write
common global
common data_set

for i=0,n_elements(c.source)-1L do begin
      result=dat_list(s_l,'"source" eq '+'"'+c.source[i]+'"',/reset,/no_notify)
      ra=strtrim(c.ra[in[pil[0]].ira],2L)
      dec=strtrim(c.dec[in[pil[0]].idec],2L)
;  result=dbi_sou_exist(c.source[i],ra,dec)
    result=dbi_sou_exist(c.source[i])
  if result eq 0 then begin
;     reid='"'+strtrim(e.user_name,2L)+'"'
;      result=dat_list(s_l,'"source" eq '+'"'+c.source[i]+'"',/reset,/no_notify)
      sql_statement='select maximum_souid=max(sou#) from SOU'
      result_str=dbi_sql_submit(sql_statement)
      souid=strcompress(string(result_str(2)+1),/remove_all)
      source='"'+strtrim(c.source[i],2L)+'"'
      pos='""'
;      pos='"'+strtrim(c.pos[in[pil[0].ipos]],2L)+'"'
      offx=strcompress(string(in[pil[0]].offx),/remove_all)
      offy=strcompress(string(in[pil[0]].offy),/remove_all)
      offtype='"'+c.offtype[in[pil[0]].iofftype]+'"'
;      ra='"'+strtrim(c.ra[in[pil[0].ira]],2L)+'"'
;      dec= '"'+c.dec[in[pil[0].idec]]+'"'
      ra='"'+ra+'"'
      dec='"'+dec+'"'
      rar=strcompress(string(in[pil[0]].rar),/remove_all)
      decr=strcompress(string(in[pil[0]].decr),/remove_all)
      ut='"'+c.ut[in[pil[0]].iut]+'"'
      epoch=strcompress(string(in[pil[0]].epoch),/remove_all)
      sflux=strcompress(string(in[pil[0]].sflux),/remove_all)
      size=strcompress(string(in[pil[0]].size),/remove_all)
     sql_statement='insert SOU values('+souid+' ,'+ $
         source + ' ,'+pos+' ,'+offx+' ,'+offy+' ,'+offtype+' ,'+ $
         ra+' ,'+dec+' ,'+rar+' ,'+decr+' ,'+ut+' ,'+epoch+' ,'+ $
         sflux+' ,'+size+')'
;     if e.debug then print,sql_statement
     result_str=dbi_sql_submit(sql_statement)
     if e.debug then print,result_str
  endif
endfor

end
