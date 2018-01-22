pro uti_addhdr, sources=sources, pol=pol, antennas=antennas
common global
common data_set

if tag_exist(bl,'csnr') ne 0 then begin
   pol_icode=6
   source_icode=23
   tel1_icode=13
   tel2_icode=14
   band_icode=18
endif else begin
   pol_icode=3
   source_icode=12
   tel1_icode=5
   tel2_icode=6
   band_icode=10
endelse

if keyword_set(sources) then n_sources=n_elements(sources)
if keyword_set(antennas) then begin
   realants=''
   for i=0, n_elements(antennas)-1 do begin
      j=where(c.tel1 eq antennas[i],count)
      if count eq 1 then begin
         print,'Header exists. No need to add antenna ',antennas[i]
;         print,'Quit !'
;         return
      endif else begin
         if i eq 0 then realants=antennas[i] else realants=[realants,antennas[i]]
      endelse
   endfor
   if realants eq '' then return else antennas=realants
   n_antennas=n_elements(antennas)
endif
   
ncodes=intarr(n_tags(c))
for i = 0,n_tags(c)-1 do begin
   ncodes[i] = n_elements(c.(i))
;   if i eq 23 then ncodes[i]=n_elements(c.(i))+n_sources
endfor
if keyword_set(pol) then ncodes[pol_icode]=ncodes[pol_icode]+1
if keyword_set(sources) then ncodes[source_icode]=ncodes[source_icode]+n_sources
;ncsets = total(ncodes,2)
;nctags = total(ncodes,1)
;if keyword_set(fullband) then ncodes[band_icode]=25
if keyword_set(antennas) then begin
   ncodes[tel1_icode]=ncodes[tel1_icode]+n_antennas
   ncodes[tel2_icode]=ncodes[tel2_icode]+n_antennas
endif
str_temp=strarr(total(ncodes))
nctags=ncodes

k=0

for i = 0,n_tags(c)-1 do begin
;   if keyword_set(fullband) and i eq band_icode then begin
;      str_temp[k]='c1'
;      k=k+1
;      for j=1, 9 do begin
;         str_temp[k]='s0'+strcompress(string(j),/remove_all)
;         k=k+1
;      endfor
;      for j=10, 24 do begin
;         str_temp[k]='s'+strcompress(string(j),/remove_all)
;         k=k+1
;      endfor
;   endif else begin

      for j = 0,n_elements(c.(i))-1 do begin
         if keyword_set(pol) and i eq pol_icode and j eq 0 then begin 
            str_temp[k]='bad'
            k=k+1
         endif
         str_temp[k] = c.(i)[j]
         if keyword_set(sources) and (i eq source_icode) and (j eq n_elements(c.(i))-1) then begin
            for kk =0, n_sources-1 do begin
               k=k+1
               str_temp[k]=sources[kk]
            endfor
         endif
         if keyword_set(antennas) and (i eq tel1_icode) and (j eq n_elements(c.(i))-1) then begin
            for kk =0, n_antennas-1 do begin
               k=k+1
               str_temp[k]=antennas[kk]
            endfor
         endif
         if keyword_set(antennas) and (i eq tel2_icode) and (j eq n_elements(c.(i))-1) then begin
            for kk =0, n_antennas-1 do begin
               k=k+1
               str_temp[k]=antennas[kk]
            endfor
         endif
         k = k+1
      endfor
;   endelse
endfor

;print,str_temp

if tag_exist(bl,'csnr') ne 0 then begin
   c = { $
     cocd:strarr(nctags[0]),ut:strarr(nctags[1]), $
     ref_time:strarr(nctags[2]), $
     tq:strarr(nctags[3]),vctype:strarr(nctags[4]),$
     sb:strarr(nctags[5]),pol:strarr(nctags[6]),$
     aq:strarr(nctags[7]),bq:strarr(nctags[8]),$
     cq:strarr(nctags[9]),oq:strarr(nctags[10]),$
     rec:strarr(nctags[11]),$
     ifc:strarr(nctags[12]),tel1:strarr(nctags[13]),$
     tel2:strarr(nctags[14]),blcd:strarr(nctags[15]),$
     gq:strarr(nctags[16]),pq:strarr(nctags[17]),$
     band:strarr(nctags[18]),pstate:strarr(nctags[19]),$
     vtype:strarr(nctags[20]),taper:strarr(nctags[21]),$
     trans:strarr(nctags[22]),$
     source:strarr(nctags[23]),pos:strarr(nctags[24]),$
     offtype:strarr(nctags[25]),$
     ra:strarr(nctags[26]),dec:strarr(nctags[27]),$
     icode_s:strarr(nctags[28]),icode_tag:strarr(nctags[29])}
endif else begin
   c = { $
     ut:strarr(nctags[0]), $
     ref_time:strarr(nctags[1]), $
     sb:strarr(nctags[2]),pol:strarr(nctags[3]),$
     rec:strarr(nctags[4]),$
     tel1:strarr(nctags[5]),$
     tel2:strarr(nctags[6]),blcd:strarr(nctags[7]),$
     gq:strarr(nctags[8]),pq:strarr(nctags[9]),$
     band:strarr(nctags[10]),pstate:strarr(nctags[11]),$
     source:strarr(nctags[12]),vrad:strarr(nctags[13]),$
     ra:strarr(nctags[14]),dec:strarr(nctags[15]),$
     icode_s:strarr(nctags[16]),icode_tag:strarr(nctags[17])}
endelse
   
k = 0
m = intarr(n_tags(c))
for i = 0,n_tags(c)-1 do begin
   for j = 0,ncodes[i]-1 do begin
;     if i eq 23 then print,str_temp[k]
     aa = str_temp[k]
     c.(i)[ j + m[i] ] = aa
     k = k+1
   endfor
;   if i eq 23 then print,c.(i)
   m[i] = m[i] + ncodes[i]   
endfor

end

