pro phase_conjugate, force=force
;yes
;=Task:PHASE_CONJUGATE --- To flip the phases of the selected data.
;#Type: utility
;+Use:
;      This program flips the phase (complex conjugete) of the the 
;      visibility data for whatever baselines, sidebands, etc which 
;      were previously selected using either the select or dat_filter
;      commnads. For example, if you want flip the phase of the lower
;      sideband data, you can try
;      >select, sideband = 'l', /reset
;      >phase_conjugate
;&history:
;------------------------------------------------------------------------
;      cykuo 10feb04 adapting the header
;------------------------------------------------------------------------
;

common global
common data_set

juldat_stop=uti_jul_day(4,28,2005)+0.d0
months = ['Jan','Feb','Mar','Apr','May','Jun','Jul', $
          'Aug','Sep','Oct','Nov','Dec']
datobs=c.ref_time[in[pil[0]].iref_time]
mon=strmid(datobs,0,3)
j=where(mon eq months,count)
num_jdref=[fix(strtrim(strmid(datobs,8,4),2)), $
     fix(strtrim(string(j[0]+1),2)),(strtrim(strmid(datobs,4,2),2))]
juldat_day0 =  uti_jul_day(num_jdref[1],num_jdref[2],num_jdref[0]) + 0.d0
if (not keyword_set(force)) and (juldat_day0 ge juldat_stop) then begin
   print,'The data was taken on ',c.ref_time
   print,'There is no need to flip the lsb phase of the SMA data
   print,'     after April 28, 2005.'
   print,'NO phase flipping done yet !'
   print,'Otherwise, use phase_conjugate,/force to force the flip.'
   return
endif

bl[pbl].phaave=-bl[pbl].phaave
for j = 0L,n_elements(pcl) -1 do begin

    ptr = pcl[j] + lindgen(sp[psl[j]].nch)
    ch[ptr] = conj(ch[ptr])

endfor

end

