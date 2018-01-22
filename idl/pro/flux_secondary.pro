function flux_secondary,source,user,radius,freq,flux,ut_start,ut_stop
    ; Common blocks
      common global
      common data_set

    ; Initialize 
      flux  = 0.0
      iband = fix(freq / 68.0)

    ; Find the flux at this frequency. Look only at values in current band 
    ; (1.3 vs 3mm). Dividing lines between bands is freq/68.0
    ; band 0 0-68 GHZ , 1 68-136 GHZ , 2 136-204 GHZ , 3 204-272 GHz
    ; In computing the average flux, extrapolate for frequency based upon
    ; alpha stored in database, and weight by SNR.
      com = 'declare @sou int,@pri int'
      s = 'select @sou = sou# from SOU where source like lower("' + $
             source + '") and pos="" and offx = 0 and offy = 0'
      com = [com,s]
      com = [com,'select @pri = pri# from SOU_ALI where sec#=@sou']
      s = 'select sum(snr*flux*power(' + string(freq) 
      s = s + '/freq,alpha))/sum(snr) from FLU where '
      s = s + 'sou# in ( select sec# from SOU_ALI where pri#=@pri)'
      s = s + ' and uto >="' + ut_start + '"'
      s = s + ' and uto <="' + ut_stop  + '"'
      s = s + ' and convert(int,freq/68.0)=' + string(iband)
      if (user ne '%' and user ne '*') then $
           s = s + ' and reid like "' + user + '"'
      com = [com,s]

if (e.campuslogin ne 'cfa' and e.campuslogin ne 'sma') then begin
    ; Submit command
      result = dbi_sql_submit(com)

    ; Make sure data was read in
      j = where(result eq '(0 rows affected)',nj)
      if (nj ne 0) then return,-1

    ; Read data
      sflux = ' '
      reads,result[4],sflux
      sflux = strcompress(sflux,/remove)
      if (not valid_num(sflux,flux)) then flux = 0.0
endif else begin
      flux=0
      return,-1
endelse
end
