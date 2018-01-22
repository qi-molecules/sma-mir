; *************************************************************************
; FUNCTION
;      plo_zoom_panel.pro
;
; WRITTEN
;      October 20, 1998  Kim Drongesen
;      Sep 18, 2000      modified to handle 'map' plot_type
;                        remove redundant parameters (syl)
;
; PURPOSE
;      This routine is called by plo_control when the user has done a
;      left mouse click on an unzoomed page (in an IDL only environment).
;      From Java this routine is called when the user is in zoom mode
;      and has clicked on an unzoomed panel.  This routine zooms to the
;      panel clicked on and redraws the page.
;
; INPUTS
;      x      -- x location of mouse - device coord
;      y      -- y location of mouse - device coord
;      plid   -- index into pl array of structs
;      sindex -- index into the plot specific struct (con,wlm,etc)
;
; OUTPUT
;      none
;
; EXAMPLES
;      plo_zoom_panel,x,y,plid,sindex
;
; *************************************************************************

pro plo_zoom_panel,x,y,plid,sindex

common global
common plo
;
;Set up the data structure for the correct type of plot
;
case pl[plid].plot_type of
      'con': s_page=con[sindex]
      'spe': s_page=spe[sindex]
      'gai': s_page=gai[sindex]
      'pas': s_page=pas[sindex]
      'wlm': s_page=wlm[sindex]
      'map': s_page=map[sindex]
      'var': s_page=var[sindex]
endcase
;
;Initialize everything that needs to be initialized
;
iframe=s_page.iframe
i_first=s_page.j_first & i_last=s_page.j_last
frames_per_page=s_page.frames_per_page
nor_xmax=s_page.nor_xmax & nor_ymax=s_page.nor_ymax
nor_dx=s_page.nor_dx & nor_dy=s_page.nor_dy
pan_xmin=s_page.pan_xmin & pan_xmax=s_page.pan_xmax
pan_ymin=s_page.pan_ymin & pan_ymax=s_page.pan_ymax
pan_dx=s_page.pan_dx & pan_dy=s_page.pan_dy
xmin=s_page.xmin & xmax=s_page.xmax
ymin=s_page.ymin & ymax=s_page.ymax
row=s_page.row & col=s_page.col

;
; get mouse positions
;
result = convert_coord(x,y,/device,/to_normal)
pl[plid].mouse_x = result[0]
if e.java then pl[plid].mouse_y = 1-result[1] else pl[plid].mouse_y = result[1]	;this fixes ION bug

pl[plid].plot_panel=-1
for i=i_first,i_last do begin
    if pl[plid].mouse_x ge pan_xmin[i] and pl[plid].mouse_x le pan_xmax[i] and  $
       pl[plid].mouse_y ge pan_ymin[i] and pl[plid].mouse_y le pan_ymax[i] then pl[plid].plot_panel=i
endfor

if pl[plid].plot_panel eq -1 then begin
    return
endif

if e.debug then print,'panel ',pl[plid].plot_panel
if e.debug then print,'x ',pl[plid].mouse_x,' y ',pl[plid].mouse_y,' button ',pl[plid].mouse_button


;Zooming on an individual panel
if e.debug then print,'zooming panel ',pl[plid].plot_panel
i_first=pl[plid].plot_panel & i_last=pl[plid].plot_panel
s_page.j_first=i_first & s_page.j_last=i_last
pan_xmin(pl[plid].plot_panel)=s_page.saved_par.nor_xmin 
pan_xmax(pl[plid].plot_panel)=s_page.saved_par.nor_xmax 
pan_ymin(pl[plid].plot_panel)=s_page.saved_par.nor_ymin 
pan_ymax(pl[plid].plot_panel)=s_page.saved_par.nor_ymax
pan_dx=nor_dx & pan_dy=nor_dy
row(pl[plid].plot_panel)=0 & col(pl[plid].plot_panel)=0 & save_par=1

s_page.pan_xmin=pan_xmin & s_page.pan_xmax=pan_xmax
s_page.pan_ymin=pan_ymin & s_page.pan_ymax=pan_ymax
s_page.pan_dx=pan_dx & s_page.pan_dy=pan_dy
s_page.xmin=xmin & s_page.xmax=xmax
s_page.ymin=ymin & s_page.ymax=ymax
s_page.row=row & s_page.col=col
s_page.j_first=i_first & s_page.j_last=i_last
s_page.frames_per_page=1 & s_page.nframes=pl[plid].plot_panel
s_page.nrow=1 & s_page.ncol=1

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

pl[plid].plot_zoom = 1

end
