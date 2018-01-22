; *************************************************************************
; FUNCTION
;      plo_new_panel.pro
;
; WRITTEN
;      October 30, 1998 Kim Drongesen
;      Sep 18, 2000  modified to handle 'map' plot_type
;                    redundant parameters (syl)
;
; PURPOSE
;      This routine plots the next or previous page depending on the
;      value of prevpan.  In the IDL only environment it is called from
;      plo_control when the user clicks the middle mouse button, and 
;      then types the 'n' or 'p' key.  It is called from MIR when the
;      user hits the 'prev' or 'next' buttons.  In MIR these buttons
;      are only available to the user when there is a previous or next plot.
;      This routine draws the prev or next page before it returns.
;
; INPUTS
;      prevpan - boolean value says that we are going back or forward
;      plid -- plot structure id
;      sindex -- index to the map structure (currently borrowed from con sturcture)
;
; OUTPUT
;      none
;
; EXAMPLES
;      plo_new_panel,1,plid,sindex
;
; *************************************************************************
pro plo_new_panel,prevpan,plid,sindex

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
;
;Initialize parameters that needs to be initialized
;
iframe=s_page.iframe
frames_per_page=s_page.frames_per_page
pan_xmin=s_page.pan_xmin & pan_xmax=s_page.pan_xmax
pan_ymin=s_page.pan_ymin & pan_ymax=s_page.pan_ymax
pan_dx=s_page.pan_dx & pan_dy=s_page.pan_dy
xmin=s_page.xmin & xmax=s_page.xmax
ymin=s_page.ymin & ymax=s_page.ymax
row=s_page.row & col=s_page.col

if prevpan eq 1 then begin
    iframe=max([iframe-2*frames_per_page,-frames_per_page])
    s_page.iframe=iframe
endif

if pl[plid].plot_zoom then begin
    pan_xmin(pl[plid].plot_panel)=s_page.saved_par.xmin(pl[plid].plot_panel) 
    pan_xmax(pl[plid].plot_panel)=s_page.saved_par.xmax(pl[plid].plot_panel)
    pan_ymin(pl[plid].plot_panel)=s_page.saved_par.ymin(pl[plid].plot_panel) 
    pan_ymax(pl[plid].plot_panel)=s_page.saved_par.ymax(pl[plid].plot_panel)
    row(pl[plid].plot_panel)=s_page.saved_par.row(pl[plid].plot_panel) 
    col(pl[plid].plot_panel)=s_page.saved_par.col(pl[plid].plot_panel)
    xmin=s_page.saved_par.data_xmin 
    xmax=s_page.saved_par.data_xmax
    ymin=s_page.saved_par.data_ymin 
    ymax=s_page.saved_par.data_ymax
    pan_dx=s_page.saved_par.pan_dx
    pan_dy=s_page.saved_par.pan_dy 
    pl[plid].plot_zoom=0

    s_page.pan_xmin=pan_xmin & s_page.pan_xmax=pan_xmax
    s_page.pan_ymin=pan_ymin & s_page.pan_ymax=pan_ymax
    s_page.pan_dx=pan_dx & s_page.pan_dy=pan_dy
    s_page.xmin=xmin & s_page.xmax=xmax 
    s_page.ymin=ymin & s_page.ymax=ymax
    s_page.row=row & s_page.col=col
endif

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


if j_first le j_last then begin
   
   result=plo_page(plid,sindex)

   case pl[plid].plot_type of
       'gai':  begin
        	 gai[sindex].nsub=n_elements(gai[sindex].y_var)-1
                 result=plo_over_page(plid,sindex)
                 gai[sindex].nsub=n_elements(gai[sindex].y_var)
               end
       'pas':  begin
      		 pas[sindex].nsub=2
       		 result=plo_over_page(plid,sindex)
       	       end
   else:
   endcase
endif

pl[plid].plot_zoom = 0

return
end
