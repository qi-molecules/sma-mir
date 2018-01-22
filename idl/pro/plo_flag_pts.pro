pro plo_flag_pts,plid,sindex
; This routine initializes the n_select var for flagging - called
; flagging
;
; parameters 
;	     : plid - plot index - index into pl array structure
;	     : sindex - index into con,gai,wlm, etc array
;
; 
; Written by:  Kim Drongesen
; Date:  December 16, 1998

common global
common plo

case pl[plid].plot_type of
      'con': con[sindex].n_select = 0
      'spe': spe[sindex].n_select = 0
      'gai': gai[sindex].n_select = 0
      'pas': pas[sindex].n_select = 0
      'wlm': wlm[sindex].n_select = 0
      'var': var[sindex].n_select = 0
endcase

return
end
