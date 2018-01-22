pro uti_phasechange, angle=angle
;yes
;=Task:UTI_PHASECHANGE --- To change the phases of the selected data by
;                       the angle value in degs. 
;#Type: utility
;+Use:
;      This program subtract the phase of the 
;      visibility data for whatever baselines, sidebands, etc which 
;      were previously selected using either the select or dat_filter
;      commnads. For example, if you want flip the phase of the lower
;      sideband data, you can try
;      >select, sideband = 'l', /reset
;      >uti_phasechange, angle=180
;&history:
;------------------------------------------------------------------------
;      cykuo 10feb04 adapting the header
;------------------------------------------------------------------------
;

common global
common data_set

bl[pbl].phaave = bl[pbl].phaave + angle
for j = 0L,n_elements(pcl) -1 do begin

    ptr = pcl[j] + lindgen(sp[psl[j]].nch)
    ch[ptr] = ch[ptr] * complex(cos(angle*!pi/180.d),sin(angle*!pi/180.d))
endfor

end

