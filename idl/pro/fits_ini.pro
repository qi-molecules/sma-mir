function fits_ini,sources,poss,transs,tqs
;
; Outputs a list of source inames, pos, trans, tq for the fits window.
; (used as a preliminary to fits_ini_band which gets the band choices
; once a sources has been selected)
;
; result = -1 (error), 0 (no rows) >0 (number of all_sources)
; returns -1 no sources found
;
; sources - list of sources names
; poss   -- source positions
; transs  -- transitions
; tqs     -- tuning qualifiers
;
; eg. : result=dat_filter(s_f,/reset) 
; eg. : result=fits_ini(sources,poss,transs,tqs)
;
common global
common data_set
common plo

result=dat_list(s_l,/reset,/no_notify)
if n_elements(pil) gt 0 then begin
    combinations='#0'+c.source[in[pil].isource]+'#1'+c.pos[in[pil].ipos]+ $
                '#2'+c.trans[sp[psl].itrans]+'#3'+c.tq[in[psl].itq]+'#4'
    d_comb=uti_distinct(combinations,ndistinct,/many_repeat)
    sources=make_array(ndistinct,/string)  & poss=make_array(ndistinct,/string)
    transs=make_array(ndistinct,/string)  & tqs=make_array(ndistinct,/string)
    for i=0,ndistinct-1 do begin
       i1=strpos(d_comb[i],'#0')+2 & nchar=strpos(d_comb[i],'#1')-i1
       sources[i]=strmid(d_comb[i],i1,nchar)
       i1=strpos(d_comb[i],'#1')+2 & nchar=strpos(d_comb[i],'#2')-i1
       poss[i]=strmid(d_comb[i],i1,nchar)
       i1=strpos(d_comb[i],'#2')+2 & nchar=strpos(d_comb[i],'#3')-i1
       transs[i]=strmid(d_comb[i],i1,nchar)
       i1=strpos(d_comb[i],'#3')+2 & nchar=strpos(d_comb[i],'#4')-i1
       tqs[i]=strmid(d_comb[i],i1,nchar)     
    endfor
    return,1 
endif else begin
   source='' & pos='' & trans='' & tq=''
   return,-1
endelse

return,n_elements(sources)

end
