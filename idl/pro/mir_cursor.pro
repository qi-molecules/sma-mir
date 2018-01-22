function mir_cursor,x1,y1,x2,y2
;
; Draws the cursor during a drag
;

common global
common data_set
common plo

case pl[plid].plot_type of
      'con': s_page=con[sindex]
      'spe': s_page=spe[sindex]
      'gai': s_page=gai[sindex]
      'pas': s_page=pas[sindex]
      'wlm': s_page=wlm[sindex]
endcase

print,x1,x2,y1,y2
oplot,[x1,x2,x2,x1,x1],[y1,y1,y2,y2,y1]

return,1
end
