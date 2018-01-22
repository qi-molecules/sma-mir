function fits_ini_band,source,pos,trans,tq, $
      bands,sbs,pols,nchs,bws,fress,vels,vress,velws
;
; Outputs a list of  bands,sbs,pols,nchs,bws,fress,vels,vress and
;                    velws for the fits window.
; (used in conjunction with fits_ini which gives a list of sources)
;
; result = -1 (error), 0 (no rows) >0 (number of all_sources)
; returns -1 no bands found
;
; source - desired source name
; pos   -- desired position
; trans -- desired transition
; tq    -- desired tuning qualifier
;
; eg. : result=dat_filter(s_f,/reset) 
; eg. : result=fits_ini_band(source,pos,trans,tq, $
;               bands,sbs,pols,nchs,bws,fress,vels,vress,velws)
;
common global
common data_set
common plo

    sel_str='"source" eq "'+source+'" and "pos" eq "'+pos+ $
            '" and "trans" eq "'+trans+'" and "tq" eq "'+tq+'"'
    result=dat_list(s_l,sel_str,/reset,/no_notify)
    vels=string(round(sp[psl].vel)) 
    bws=string(round(abs(sp[psl].nch*sp[psl].fres)))
    velws=string(round(abs(sp[psl].nch*sp[psl].vres))) 
    nchs=string(sp[psl].nch) 
    fress=string(round(abs(sp[psl].fres))) 
    vress=string(100.*round(100.*abs(sp[psl].vres)))
    combinations='#0'+c.band[sp[psl].iband]+'#1'+c.sb[bl[pbl].isb]+ $
                 '#2'+c.pol[bl[pbl].ipol]+'#3'+vels+ $
                 '#4'+bws+'#5'+velws+ $
                 '#6'+nchs+'#7'+fress+ $
                 '#8'+vress+'#9'
    d_comb=uti_distinct(combinations,ndistinct,/many_repeat)
    bands=make_array(ndistinct,/string)  & sbs=make_array(ndistinct,/string)
    pols=make_array(ndistinct,/string)  & vels=make_array(ndistinct,/string)
    bws=make_array(ndistinct,/string)  & velws=make_array(ndistinct,/string)
    nchs=make_array(ndistinct,/string)  & fress=make_array(ndistinct,/string)
    vress=make_array(ndistinct,/string) 
    for i=0,ndistinct-1 do begin
       i1=strpos(d_comb[i],'#0')+2 & nchar=strpos(d_comb[i],'#1')-i1
       bands[i]=strmid(d_comb[i],i1,nchar)
       i1=strpos(d_comb[i],'#1')+2 & nchar=strpos(d_comb[i],'#2')-i1
       sbs[i]=strmid(d_comb[i],i1,nchar)
       i1=strpos(d_comb[i],'#2')+2 & nchar=strpos(d_comb[i],'#3')-i1
       pols[i]=strmid(d_comb[i],i1,nchar)
       i1=strpos(d_comb[i],'#3')+2 & nchar=strpos(d_comb[i],'#4')-i1
       vels[i]=strmid(d_comb[i],i1,nchar)          
       i1=strpos(d_comb[i],'#4')+2 & nchar=strpos(d_comb[i],'#5')-i1
       bws[i]=strmid(d_comb[i],i1,nchar)           
       i1=strpos(d_comb[i],'#5')+2 & nchar=strpos(d_comb[i],'#6')-i1
       velws[i]=strmid(d_comb[i],i1,nchar)          
       i1=strpos(d_comb[i],'#6')+2 & nchar=strpos(d_comb[i],'#7')-i1
       nchs[i]=strmid(d_comb[i],i1,nchar)     
       i1=strpos(d_comb[i],'#7')+2 & nchar=strpos(d_comb[i],'#8')-i1
       fress[i]=strmid(d_comb[i],i1,nchar)     
       i1=strpos(d_comb[i],'#8')+2 & nchar=strpos(d_comb[i],'#9')-i1
       vress[i]=strmid(d_comb[i],i1,nchar)     
    endfor
    result=dat_list(s_l,/reset,/no_notify)
    return,1 

return,n_elements(bands)

end
