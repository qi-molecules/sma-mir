function plo_init,plindex,sindex
;
; Initializes the plot device if used in IDL only
; eg. : result=plo_init(plindex,sindex)  ; 
;
; parameters : plindex - plot index - index into pl array structure
;            : sindex  - index into con,gai,wlm, etc array
;
;
common global
common plo

;
; check if interactive device
;
pl[plindex].plot_interact = (pl[plindex].plot_device eq 'x') ? 1 : 0

;
; for initialize call, print help if interactive
;
if pl[plindex].plot_interact then begin
    print,'*** mouse sounds :'
    print,'      left button    => select panel to zoom  
    print,'      middle button  => keyboard options (print, etc) 
    print,'      right button   => unzoom or go to next page
    print,'******************'
endif
set_plot,pl[plindex].plot_device
pl[plindex].plot_date=strtrim(systime(0),2L)
pl[plindex].plot_key=''
pl[plindex].plot_zoom=0
return_val=pl[plindex].plot_interact

return,return_val

end 
