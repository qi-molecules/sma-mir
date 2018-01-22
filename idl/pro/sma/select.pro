pro select,source=source,baseline=baseline,rx=rx,band=band,sideband=sideband, $
           integration=integration, antenna=antenna, reset=reset, $
           display=display,pos_wt=pos_wt, if1=if1, if2=if2, asic=asic, $
           if3=if3, if4=if4, swarm=swarm, state=state
;yes
;=Task:SELECT --- To select subsets of data (SMA wrapper)
;#Type: utility
;+Use:
;      SELECT is used to select the subsets of the data. When called with 
;      no arguments, the tasks will display the sources, baselines, etc.
;      of the current data without taking any further action. Select may
;      be run more than once to select a subset of previously selected data.
;      One can use the parameter /reset to clear any previous selections,
;      and use the parameter /pos_wt to select data with positive wights.
;      Selection keywords include source, baseline, band, integration,
;      sideband, and selection flag include reset, display, and pos_wt.
;      The following are some examples:
;      >select,source='jupiter',/reset
;      >select,source=['jupiter','3c273'],/reset
;      >select,baseline=['1-2','1-3','1-4']
;      >select,integration=[1,10,100,110,200,210],sideband='u'
;      >select,/pos_wt
;      >select,source='jupiter',sideband='u', /pos_wt
;      >select,ant='-3',/pos_wt,/reset
;
;
;&history:
;------------------------------------------------------------------------
;      cykuo 26feb04 adapting the header
;------------------------------------------------------------------------

common global
common data_set
common plo

printall = 0
if keyword_set(source) eq 0 and keyword_set(baseline) eq 0 $
   and  keyword_set(rx) eq 0 and keyword_set(band) eq 0 and $
   keyword_set(sideband) eq 0  and keyword_set(antenna) eq 0 $
   and keyword_set(integration) eq 0 and keyword_set(pos_wt) $
   eq 0 and keyword_set(if1) eq 0 $
   and keyword_set(if2) eq 0 and keyword_set(polarization) eq 0 $
   then printall = 1
if keyword_set(display)  then printall = 1

if printall then begin
print,'All Sources: ',c.source
print,'All Baselines: ',c.blcd
print,'All Recs: ',c.rec[uti_distinct(bl.irec,nrec,/many)]
print,'All Bands: ',c.band
print,'All Sidebands : ',c.sb
print,'All Polarization states: ',c.pol
print,'All Integrations: ',strtrim(string(in[0].int),2),'-', $
  strtrim(string(in[n_elements(in)-1].int),2)
endif


if (keyword_set(reset)) then result=dat_filter(s_f,/reset,/no_notify)

list=''

if (keyword_set(pos_wt)) then list=list+'("wt" gt "0") and '

if tag_exist(bl,'tpvar') then newformat=0 else newformat=1

swmonly=0
if newformat eq 1 then begin
   result=uti_distinct(sp.sphint1,/many,ntmp)
   if ntmp eq 1 and result[0] eq 1 then swmonly=1
endif

if (keyword_set(if1)) then if swmonly then list=list+'("sphint1" eq "1") and ("iband" le "1") and ' else list=list+'("iband" le "24") and '

if (keyword_set(if2)) then if swmonly then list=list+'("sphint1" eq "1") and (("iband" eq "2") or ("iband" eq "0")) and ' else list=list+'(("iband" eq "0") or ("iband" ge "25" and "iband" le "48")) and '

if (keyword_set(asic)) then list=list+'("sphint1" eq "0") and ("iband" le "48") and '

if (keyword_set(if3)) then if swmonly then list=list+'("sphint1" eq "1") and (("iband" eq "3") or ("iband" eq "0")) and ' else list=list+'(("iband" eq "0") or ("iband" eq "49")) and '

if (keyword_set(if4)) then if swmonly then list=list+'("sphint1" eq "1") and (("iband" eq "4") or ("iband" eq "0")) and ' else list=list+'(("iband" eq "0") or ("iband" eq "50")) and '

if (keyword_set(swarm)) then list=list+'(("sphint1" eq "1") or ("iband" eq "0") or ("iband" ge "49")) and '

if (keyword_set(source)) then begin
   source=strtrim(source,2)
   if (strmid(source[0],0,1) eq '-') then begin
      op='not like ' & source[0]=strmid(source[0],1,strlen(source[0])-1)
   endif else op='like '
   list=list+' ("source" '+op+'"'+source[0]+'"'
   if n_elements(source) gt 1 then begin
      for i = 1, n_elements(source)-1 do begin
         if (strmid(source[i],0,1) eq '-') then begin
            op='not like ' & source[i]=strmid(source[i],1,strlen(source[i])-1)
            list=list+' and  "source" '+op+'"'+source[i]+'"'
         endif else begin
             op='like '
             list=list+' or  "source" '+op+'"'+source[i]+'"'
         endelse
      endfor
   endif
   list = list + ') and '
endif

if (keyword_set(baseline)) then begin
   baseline=strtrim(baseline,2)

   if (strmid(baseline[0],0,1) eq '-') then begin
      op='ne ' & baseline[0]=strmid(baseline[0],1,strlen(baseline[0])-1)
   endif else op='eq '
   list=list+' ("blcd" '+op+'"'+baseline[0]+'"'
   if n_elements(baseline) gt 1 then begin
      for i = 1, n_elements(baseline)-1 do begin
         if (strmid(baseline[i],0,1) eq '-') then begin
            op='ne ' & baseline[i]=strmid(baseline[i],1,strlen(baseline[i])-1)
            list=list+' and  "blcd" '+op+'"'+baseline[i]+'"'
         endif else begin
             op='eq '
             list=list+' or  "blcd" '+op+'"'+baseline[i]+'"'
         endelse
      endfor
   endif
   list = list + ') and '
endif

if (keyword_set(antenna)) then begin
   antenna=strtrim(antenna,2)
   if (strmid(antenna[0],0,1) eq '-') then begin
      op='not like ' & antenna[0]=strmid(antenna[0],1,strlen(antenna[0])-1)
   endif else op='like '
   list=list+' ("blcd" '+op+'"'+antenna[0]+'"'
   if n_elements(antenna) gt 1 then begin
      for i = 1, n_elements(antenna)-1 do begin
         if (strmid(antenna[i],0,1) eq '-') then begin
            op='not like ' & antenna[i]=strmid(antenna[i],1,strlen(antenna[i])-1)
            list=list+' and  "blcd" '+op+'"'+antenna[i]+'"'
         endif else begin 
             op='like '
             list=list+' or  "blcd" '+op+'"'+antenna[i]+'"'
         endelse
      endfor
   endif
   list = list + ') and '
endif


if (keyword_set(rx)) then begin
   rx=strtrim(rx,2)

   if (strmid(rx[0],0,1) eq '-') then begin
      op='ne ' & rx[0]=strmid(rx[0],1,strlen(rx[0])-1)
   endif else op='eq '
   list=list+' ("rec" '+op+'"'+rx[0]+'"'
   if n_elements(rx) gt 1 then begin
       for i = 1, n_elements(rx)-1 do begin
           if (strmid(rx[i],0,1) eq '-') then begin
               op='ne ' & rx[i]=strmid(rx[i],1,strlen(rx[i])-1)
               list=list+' and  "rec" '+op+'"'+rx[i]+'"'
           endif else begin
               op='eq '
               list=list+' or  "rec" '+op+'"'+rx[i]+'"'
           endelse
      endfor
  endif
  list = list + ') and '
endif

if (keyword_set(band)) then begin
   iband=intarr(n_elements(band))
   band=strlowcase(strtrim(band,2))
   band=band[sort(band)]
   ic=where(band eq 'c1' or band eq '-c1',count)
   if count eq 1 then begin
      band0=band[ic]
      is=where(band ne band0[0],count)     
      if count gt 1 then band=[band0,band[is]]
      if band0 eq 'c1' then op='eq ' else op='ne '
   endif else begin
      iband[0]=fix(strsplit(band[0],escape='s',/extract))
      if (iband[0] lt 0) then begin
         op='ne ' 
         iband[0]=-1*iband[0]
      endif else begin
         op='eq '
      endelse
   endelse
   list=list+' ("iband" '+op+'"'+strcompress(string(iband[0]),/remove)+'"'
   if n_elements(band) gt 1 then begin
      for i = 1, n_elements(band)-1 do begin
         iband[i]=fix(strsplit(band[i],escape='s',/extract))
         if (iband[i] lt 0) then begin
            op='ne ' & iband[i]=-1*iband[i] 
            list=list+' and  "iband" '+op+'"'+strcompress(string(iband[i]),/remove)+'"'
         endif else begin
            op='eq '
            list=list+' or  "iband" '+op+'"'+strcompress(string(iband[i]),/remove)+'"'
         endelse
      endfor
   endif
   list = list + ') and '
endif


if (keyword_set(sideband)) then begin

   sb=strtrim(sideband,2)

   if (strmid(sb[0],0,1) eq '-') then begin
      op='ne ' & sb[0]=strmid(sb[0],1,strlen(sb[0])-1)
   endif else op='eq '
   list=list+' ("sb" '+op+'"'+sb[0]+'"'
   if n_elements(sb) gt 1 then begin
      for i = 1, n_elements(sb)-1 do begin
         if (strmid(sb[i],0,1) eq '-') then begin
            op='ne ' & sb[i]=strmid(sb[i],1,strlen(sb[i])-1)
            list=list+' and  "sb" '+op+'"'+sb[i]+'"'
         endif else begin
             op='eq '
             list=list+' or  "sb" '+op+'"'+sb[i]+'"'
         endelse
      endfor
   endif
   list = list + ') and '
endif

if (keyword_set(state)) then begin

   pol=strtrim(state,2)

   if (strmid(pol[0],0,1) eq '-') then begin
      op='ne ' & pol[0]=strmid(pol[0],1,strlen(pol[0])-1)
   endif else op='eq '
   list=list+' ("pol" '+op+'"'+pol[0]+'"'
   if n_elements(pol) gt 1 then begin
      for i = 1, n_elements(pol)-1 do begin
         if (strmid(pol[i],0,1) eq '-') then begin
            op='ne ' & pol[i]=strmid(pol[i],1,strlen(pol[i])-1)
            list=list+' and  "pol" '+op+'"'+pol[i]+'"'
         endif else begin
             op='eq '
             list=list+' or  "pol" '+op+'"'+pol[i]+'"'
         endelse
      endfor
   endif
   list = list + ') and '
endif


if (keyword_set(integration)) then begin

int = string(integration)

if (int[0] ne '') then begin
  list=list+'(  ("int" ge "'+strtrim(int[0],2)+'"'
  list=list+' and  "int" le "'+strtrim(int[1],2)+'")'
   if n_elements(int) gt 2 then begin
      for i = 2, n_elements(int)-1,2 do begin
            list=list+' or ("int" ge "'+strtrim(int[i],2)+'"'
            list=list+' and  "int" le "'+strtrim(int[i+1],2)+'")'
      endfor
   endif
  list = list + ')'
endif              
list = list + ' and '
endif

if list ne '' then begin
  nch = strlen(list)
  list = strmid(list,0,nch-5)
  print,'Here are the data selection criteria '
  print,list
  npts=dat_filter(s_f,list)
endif

if (keyword_set(display)) then begin

junk = in[pif].isource
distinct_i=junk( uniq(junk,sort(junk)) )
n = n_elements(distinct_i)
print,'Selected sources:  ',c.source(distinct_i)

junk = bl[pbf].iblcd
distinct_i=junk( uniq(junk,sort(junk)) )
n = n_elements(distinct_i)
print,'Selected baselines:  ',c.blcd(distinct_i)

junk = bl[pbf].irec
distinct_i=junk( uniq(junk,sort(junk)) )
n = n_elements(distinct_i)
print,'Selected recs:  ',c.rec(distinct_i)

junk = sp[psf].iband
distinct_i=junk( uniq(junk,sort(junk)) )
n = n_elements(distinct_i)
print,'Selected bands:  ',c.band(distinct_i)

junk = bl[pbf].isb
distinct_i=junk( uniq(junk,sort(junk)) )
n = n_elements(distinct_i)
print,'Selected sidebands:  ',c.sb(distinct_i)

junk = in[pif].int
distinct_i=junk( uniq(junk,sort(junk)) )
n = n_elements(distinct_i)
listints = strtrim(string(distinct_i[0]),2)
for i = 1,n-2 do begin
 if (distinct_i[i]+1 ne distinct_i[i+1]) then begin
   listints = listints + '-' + strtrim(string(distinct_i[i]),2) $
     + ', '+strtrim(string(distinct_i[i+1]),2)
 endif
endfor
listints = listints + '-'+strtrim(string(distinct_i[n-1]),2)
print,'Selected integrations: ',listints
endif

end

