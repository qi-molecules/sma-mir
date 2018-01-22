function cal_app_ini,int_min,int_max,bands,tqs,sbs,cals_available
;
; Outputs lists of for initializing the cal_apply window
; based on the current data visible in the filter and select
; outputs include : tq values => tqs
;                   sb values => sbs
;                   bands (1mm or 3mm)
;                   int_min
;                   int_max
;                   cals_available (gain,passband,wlm)
;
; parameters : none   
;
; result = -1 (error), 0 (no rows) >0 (number of int)
;
; eg. : result=dat_filter(s_f,/reset) 
; eg. : result=cal_app_ini(int_min,int_max,bands,tqs,sbs,cals_available)
;
common global
common data_set
common plo

;
; get passband qualified data and the measured amps
; the list of source names and amplitudes are returned
; in arrays : gai_sources and gai_amps
;

result=dat_list(s_l,/reset,/no_notify)
 tqs=uti_distinct(c.tq[in[pil].itq],ndistinct,/many_repeat)
 sbs=uti_distinct(c.sb[bl[pbl].isb],ndistinct,/many_repeat)
 j=where(sp[psl].fsky gt 150.,count_1mm) 
 j=where(sp[psl].fsky le 150.,count_3mm)
 if count_1mm gt 0 then bands=['1mm'] 
 if count_3mm gt 0 then bands=['3mm'] 
 if count_1mm gt 0 and count_3mm gt 0 then bands=['3mm','1mm']
 cals=uti_distinct(ca.cal_type+' '+ca.y_var,ndistinct,/many_repeat)
 cals_available='none'
for i=0,n_elements(cals)-1 do begin
 if strpos(cals[i],'gain amp') ne -1 then cals_available=[cals_available,'gainamp']
 if strpos(cals[i],'gain pha') ne -1 or strpos(cals[i],'gain amp,pha') ne -1  $
           then cals_available=[cals_available,'gainpha']
 if strpos(cals[i],'pass amp') ne -1 then cals_available=[cals_available,'passamp']
 if strpos(cals[i],'pass pha') ne -1 or strpos(cals[i],'pass amp,pha') ne -1  $
           then cals_available=[cals_available,'passpha']
 if n_elements(cals_available) gt 1 then cals_available=cals_available[1:*]
endfor
 cals_available=uti_distinct(cals_available,ndistinct,/many_repeat)
 int_min=min([in[pil].int])
 int_max=max([in[pil].int])
 ints=uti_distinct(in[pil].int,ndistinct,/many_repeat)


return,ndistinct

end
