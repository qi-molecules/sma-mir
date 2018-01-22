function dbi_flux_read,ut,delta_days,numbs,fluxes, $
       souids=souids,sources=sources,poss=poss,freq=freq,reid=reid
;
; Retrieves and average flux values for a source from db.
;
; parameters :
;              ut      -- ut date for desired flux estimate
;              delta_days -- use flux archive w/ measureements
;                            from ut+/-delta_days
;              souids  -- list of souids
;              (if souids are supplied, the routine ignores sources and poss)
;              sources -- list of source names
;              poss    -- list of positions in sources
;              freq    -- freq of desired flux (divided into
;                         discrete bands of width 68 Ghz 
;                         (if freq not supplied, does 100 and 230 GHz)
;
; on return the number of measures and average fluxes are in freq 
; order for each source in the arrays numbs and fluxes
;
; eg. : res=dbi_flux_read('1/1/96',30.,numbs,fluxes, $
;           sources=['3c273','3c84'],poss=['',''],freq=100.)
;     to get 3c273 fluxes w/i 30 days of 1/1/96
; eg. : res=dbi_flux_read('1/1/96',30.,numbs,fluxes,souids=[7,5])
;
common global

if not keyword_set(freq) then freq=[100.,230.]
ibands=long(freq/68.)

;
;  find ave flux but look only at values in the current band (1.3 vs 3mm)
;  dividing line between bands is freq/68
;  band 1 0-68 , 2 68-136 , 3 136-204 , 4 204-272
;
;
; assumes spectral index -0.5
;
k=-1
ut=strtrim(ut,2L)
if not keyword_set(souids) then begin
numbs=make_array(n_elements(sources),/long)
fluxes=make_array(n_elements(sources),/float)
source='"' & pos='"'
for i=0,n_elements(sources)-1 do begin
  source=source+strtrim(sources[i],2L)+'","'
  pos=pos+strtrim(poss[i],2L)+'","'
endfor
  source=strmid(source,0,strlen(source)-2) 
  pos=strmid(pos,0,strlen(pos)-2)
  sql_statement='select source,pos,sum(snr*flux*power('+string(freq)+ $
          '/freq,-0.5))/sum(snr),'+ $
          'count(*) from FLU where source in ('+source+ $
          ') and pos in ('+pos+') and snr > 3 and '+ $
       '(convert(float,datediff(dd,"'+ut+'",uto))>= '+ $
       string(-abs(delta_days))+' and '+ $
       ' convert(float,datediff(dd,"'+ut+'",uto))<= ' $
       + string(abs(delta_days))+')' + $
    ' and convert(int,freq/68.0)=convert(int,'+string(freq[0])+'/68.0)'
  if keyword_set(reid) then sql_statement= $   
    sql_statement+' and reid like "'+reid+'"'
  sql_statement=sql_statement+' group by source,pos'
  result=dbi_sql_submit(sql_statement)
  for j=0,n_elements(sources)-1 do begin
   k=k+1 & numbs[k]=long(strmid(result[2+j],47,12))
   if numbs[k] gt 0 then fluxes[k]=float(strmid(result[2+j],25,22))
  endfor

endif else begin

  numbs=make_array(n_elements(souids)*n_elements(freq),/long)
  fluxes=make_array(n_elements(souids)*n_elements(freq),/float)
  for i=0,n_elements(souids)-1 do begin
  for j=0,n_elements(freq)-1 do begin

   sql_statement='select   "sou#:",'+string(souids[i])+','+string(freq[j])+ $
        ',count(*),sum(snr*f.flux*power('+string(freq[j])+'/f.freq,' + $
       '-0.5))/sum(snr)  from FLU f where sou#=(select pri# '+ $
       'from SOU_ALI  where sec# ='+ string(souids[i])+') and snr > 3 ' + $ 
       'and convert(int,'+string(freq[j])+'/68.) = ' + $ 
       ' convert(int,f.freq/68.) and '+ $
       '(convert(float,datediff(dd,"'+ut+'",uto))>= '+ $
       string(-abs(delta_days))+' and '+ $
       ' convert(float,datediff(dd,"'+ut+'",uto))<= ' $
       + string(abs(delta_days))+')'
       if keyword_set(reid) then $
          sql_statement=sql_statement+' and reid like "'+ reid+'"'  
   result=dbi_sql_submit(sql_statement)
   print,'sql_statement is ',sql_statement
   print,'Result is',result
   k=k+1 & numbs[k]=long(strmid(result[2],31,9))
   if numbs[k] gt 0 then fluxes[k]=float(strmid(result[2],43,18))
  endfor
  endfor
endelse
return,1
end
