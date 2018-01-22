pro uti_ampchange, scale=scale 
;yes
;=Task:UTI_AMPCHANGE --- To change the amplitude the selected data by
;#Type: utility
;+Use:
;      This program subtract the phase of the 
;      visibility data for whatever baselines, sidebands, etc which 
;      were previously selected using either the select or dat_filter
;      commnads. For example, if you want flip the phase of the lower
;      sideband data, you can try
;      >select, sideband = 'l', /reset
;      >uti_ampchange, scale=2. 
;&history:
;------------------------------------------------------------------------
;      cykuo 10feb04 adapting the header
;------------------------------------------------------------------------
;

common global
common data_set

bl[pbl].ampave = bl[pbl].ampave * scale
for j = 0L,n_elements(pcl) -1 do begin

    ptr = pcl[j] + lindgen(sp[psl[j]].nch)
    ch[ptr] = ch[ptr] * scale
endfor

end

