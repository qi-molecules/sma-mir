function dat_var
;
; Returns a structure with a list of all variables in the 
; in,bl,sp,c structures with integer codes removed and 
; duplicate varaibles names removed
;
; returns a structure in result set up as :
; vars={tags:tags,labels:labels,types:types,formats:formats}
; where vars.tags is a list of the variable names
;       vars.labels is a list of labels for variables (usually the name)
;       vars.types is the variable type (int,long,float,double,string)
;       vars.formats is a list of the print format codes
;
; eg. result = dat_var()
;
; 
;
common global
common data_set
common plo

stags=ms.stag
tags=ms.tag
labels=ms.label
formats=ms.format
types=ms.type
j=where(strmid(stags,0,3) eq 'in.' or strmid(stags,0,3) eq 'bl.'  $
        or strmid(stags,0,3) eq 'sp.' or strmid(stags,0,2) eq 'c.' $
        and stags ne 'c.icode_s' and stags ne 'c.icode_tag')
stags=stags[j]
tags=tags[j]
labels=labels[j]
formats=formats[j]
types=types[j]
jc=where(strmid(stags,0,2) eq 'c.')
;
; eliminate integer codes
;
ji=0
tags_short=strmid(tags,1)
tags_first=strmid(tags,0,1)
for i=0,n_elements(jc)-1 do begin
  jk=where(tags_first eq 'i' and tags_short eq tags[jc[i]],count)
  if count gt 0 then ji=[ji,jk[0:*]]   
endfor
ji=ji[1:*]
i=0
j=indgen(n_elements(tags))
while i lt n_elements(ji)-1 do begin
  jk=where(ji gt ji[i],count)
  case 1 of 
  (ji[i] eq 0): j=[j[ji[i]+1:*]]
  (ji[i] eq n_elements(j)-1): j=[j[0:ji[i]-1]]
  (ji[i] gt 0 and ji[i] lt n_elements(j)-1): j=[j[0:ji[i]-1],j[ji[i]+1:*]]
  endcase
  if count gt 0 then ji[jk]=ji[jk]-1
  i=i+1
endwhile
stags=stags[j]
tags=tags[j]
labels=labels[j]
formats=formats[j]
types=types[j]
ji=0
distinct_tags=uti_distinct(tags,ntags,/many_repeat)
for i=0,n_elements(distinct_tags)-1 do begin
  j=where(tags eq distinct_tags[i],count)
  ji=[ji,j[0]]
endfor
ji=ji[1:*]
stags=stags[ji]
tags=tags[ji]
labels=labels[ji]
formats=formats[ji]
types=types[ji]

vars={tags:tags,labels:labels,types:types,formats:formats}
  
return,vars
end
