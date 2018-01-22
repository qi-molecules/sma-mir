; *************************************************************************
; FUNCTION
;      plo_unzoom_panel.pro
;
; WRITTEN
;      October 21, 1998  Kim Drongesen
;      Sep 18, 2000      modified to handle 'map' plot_type
;                        remove redundant parameters (syl)
;
; PURPOSE
;      This routine "unzooms" any panel that has been zoomed and redraws the page.
;      In an IDL only environment, this routine is called from plo_control when
;      the user has clicked a right mouse button on a panel that has already been
;      zoomed.  In MIR, this routine is called when the user clicks on a panel that
;      is in zoom mode.
;
; INPUTS
;      plid   -- index into pl array of structs
;      sindex -- sindex - index into con,gai,wlm,map, etc... structures
;
; OUTPUT
;      none
;
; EXAMPLES
;      plo_unzoom_panel,plid,sindex
;
; *************************************************************************
;
pro plo_unzoom_panel,plid,sindex

common global
common plo

;Set up the data structure for the correct type of plot
case pl[plid].plot_type of
      'con': s_page=con[sindex]
      'spe': s_page=spe[sindex]
      'gai': s_page=gai[sindex]
      'pas': s_page=pas[sindex]
      'wlm': s_page=wlm[sindex]
      'map': s_page=map[sindex]
      'var': s_page=var[sindex]
endcase

;Set up parameters from pro_control
iframe=s_page.iframe
i_first=s_page.j_first & i_last=s_page.j_last
frames_per_page=s_page.frames_per_page
pan_xmin=s_page.pan_xmin & pan_xmax=s_page.pan_xmax
pan_ymin=s_page.pan_ymin & pan_ymax=s_page.pan_ymax
pan_dx=s_page.pan_dx & pan_dy=s_page.pan_dy
xmin=s_page.xmin & xmax=s_page.xmax
ymin=s_page.ymin & ymax=s_page.ymax
row=s_page.row & col=s_page.col

pan_xmin(pl[plid].plot_panel)=s_page.saved_par.xmin(pl[plid].plot_panel) 
pan_xmax(pl[plid].plot_panel)=s_page.saved_par.xmax(pl[plid].plot_panel)
pan_ymin(pl[plid].plot_panel)=s_page.saved_par.ymin(pl[plid].plot_panel) 
pan_ymax(pl[plid].plot_panel)=s_page.saved_par.ymax(pl[plid].plot_panel)
row(pl[plid].plot_panel)=s_page.saved_par.row(pl[plid].plot_panel) 
col(pl[plid].plot_panel)=s_page.saved_par.col(pl[plid].plot_panel)
xmin=s_page.saved_par.data_xmin & xmax=s_page.saved_par.data_xmax
ymin=s_page.saved_par.data_ymin & ymax=s_page.saved_par.data_ymax
pan_dx=s_page.saved_par.pan_dx & pan_dy=s_page.saved_par.pan_dy
iframe=max([iframe-frames_per_page,-frames_per_page]) & save_par=1
s_page.iframe=iframe

s_page.pan_xmin=pan_xmin & s_page.pan_xmax=pan_xmax
s_page.pan_ymin=pan_ymin & s_page.pan_ymax=pan_ymax
s_page.pan_dx=pan_dx & s_page.pan_dy=pan_dy
s_page.xmin=xmin & s_page.xmax=xmax
s_page.ymin=ymin & s_page.ymax=ymax
s_page.row=row & s_page.col=col

;Back to plo_control
pan_xmin=s_page.pan_xmin & pan_xmax=s_page.pan_xmax
pan_ymin=s_page.pan_ymin & pan_ymax=s_page.pan_ymax
pan_dx=s_page.pan_dx & pan_dy=s_page.pan_dy
xmin=s_page.xmin & xmax=s_page.xmax
ymin=s_page.ymin & ymax=s_page.ymax
row=s_page.row & col=s_page.col


pan_xmin=s_page.pan_xmin & pan_xmax=s_page.pan_xmax
pan_ymin=s_page.pan_ymin & pan_ymax=s_page.pan_ymax
pan_dx=s_page.pan_dx & pan_dy=s_page.pan_dy
xmin=s_page.xmin & xmax=s_page.xmax
ymin=s_page.ymin & ymax=s_page.ymax
row=s_page.row & col=s_page.col
iframe=s_page.iframe+s_page.frames_per_page
j_first=iframe
j_last=min([iframe+s_page.saved_par.frames_per_page-1,s_page.saved_par.nframes-1])
s_page.iframe=iframe & s_page.j_first=j_first & s_page.j_last=j_last
s_page.frames_per_page=s_page.saved_par.frames_per_page & s_page.nframes=s_page.saved_par.nframes
s_page.nrow=s_page.saved_par.nrow & s_page.ncol=s_page.saved_par.ncol

case pl[plid].plot_type of
      'con': con[sindex]=s_page
      'spe': spe[sindex]=s_page
      'gai': gai[sindex]=s_page
      'pas': pas[sindex]=s_page
      'wlm': wlm[sindex]=s_page
      'map': map[sindex]=s_page
      'var': var[sindex]=s_page
endcase

result=plo_page(plid,sindex)
pl[plid].plot_zoom = 0

case pl[plid].plot_type of
    'gai':  begin
      	       gai[sindex].nsub=gai[sindex].nsub-1
               result=plo_over_page(plid,sindex)
               gai[sindex].nsub=gai[sindex].nsub+1
            end
    'pas':  begin
               pas[sindex].nsub=2
       	       result=plo_over_page(plid,sindex)
       	    end
else:
endcase

return
end
