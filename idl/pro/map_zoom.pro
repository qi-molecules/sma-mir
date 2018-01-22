function map_zoom,s_p,iframe,nframes,frames_per_page,i_first,i_last, $
             control=control
;
; Handles map zooming through mouse.
; parameters : s_p -- structure w/ saved panel paramters (see plo_map.pro)
;            : i_first -- index to first panel on screen
;            : i_last  -- index to last panel on screen
;            : pan_xmin,pan_xmax  -- min and max normal x-coord of panels
;            : pan_ymin,pan_ymax  -- min and max normal y-coord of panels
;            : row,col    -- row and col numbers of panels
;            : xmin,xmax    -- min and max data x-coord of panels
;            : ymin,ymax    -- min abd max data y-coord of panels
;            : control      -- 'zoom','unzoom','previous','next'
; result = 0 (non-interactive) 1 (interactive)
;
; eg. : result=map_zoom(s_p,iframe,nframes,frames_per_page,i_first,i_last, $
;                   control='zoom')
;
common global
common map_page,nor_xmax,nor_ymax,nor_dx,nor_dy, $
           pan_xmin,pan_xmax,pan_ymin,pan_ymax,pan_dx,pan_dy, $
           sub_fxmin,sub_fxmax,sub_fymin,sub_fymax, $
           xmin,xmax,ymin,ymax,psym,row,col,plot_scale
;
if not g_p.plot_interact then return,0
;
; zoom on a range of the data after having zoomed to one panel
;
if control eq 'zoom' then begin
  if g_p.plot_zoom and g_p.mouse_button eq 'l' and g_p.plot_panel ne -1 then begin
    xl=g_p.mouse_x
    yl=g_p.mouse_y
    range_pick:
    print,'*** use left mouse to pick right edge to zoom on'
    cursor,x,y,3,/norm
    g_p.mouse_x=x & g_p.mouse_y=y
    case !mouse.button of
      1: g_p.mouse_button='l' 
      2: g_p.mouse_button='m' 
      4: g_p.mouse_button='r'
    endcase
    if g_p.mouse_button eq 'l' then begin
      plot_panel=-1
      for i=i_first,i_last do begin
        if g_p.mouse_x ge pan_xmin(i) and g_p.mouse_x le pan_xmax(i) and  $
           g_p.mouse_y ge pan_ymin(i) and g_p.mouse_y le pan_ymax(i) then plot_panel=i
      endfor
      if plot_panel ne g_p.plot_panel then begin
        print,'*** must pick w/i plot area !'
        goto, range_pick
      endif
      result=convert_coord([xl,g_p.mouse_x],[yl,g_p.mouse_y],/normal,/to_data)
      xmin(*)=result(0,0)
      xmax(*)=result(0,1)
      ymin(*)=result(1,0)
      ymax(*)=result(1,1)
    endif 
  endif
  if g_p.mouse_button eq 'l' and g_p.plot_panel ne -1 then begin
    if e.debug then print,'zooming panel ',g_p.plot_panel
    i_first=g_p.plot_panel & i_last=g_p.plot_panel
    pan_xmin(g_p.plot_panel)=s_p.nor_xmin & pan_xmax(g_p.plot_panel)=s_p.nor_xmax 
    pan_ymin(g_p.plot_panel)=s_p.nor_ymin & pan_ymax(g_p.plot_panel)=s_p.nor_ymax
    pan_dx=nor_dx & pan_dy=nor_dy
    row(g_p.plot_panel)=0 & col(g_p.plot_panel)=0
    g_p.plot_zoom=1
  endif
endif
if control eq 'unzoom' then begin
    if g_p.plot_zoom then begin
      pan_xmin(g_p.plot_panel)=s_p.xmin(g_p.plot_panel) 
      pan_xmax(g_p.plot_panel)=s_p.xmax(g_p.plot_panel)
      pan_ymin(g_p.plot_panel)=s_p.ymin(g_p.plot_panel) 
      pan_ymax(g_p.plot_panel)=s_p.ymax(g_p.plot_panel)
      row(g_p.plot_panel)=s_p.row(g_p.plot_panel) 
      col(g_p.plot_panel)=s_p.col(g_p.plot_panel)
      xmin=s_p.data_xmin & xmax=s_p.data_xmax
      ymin=s_p.data_ymin & ymax=s_p.data_ymax
      pan_dx=s_p.pan_dx & pan_dy=s_p.pan_dy
      iframe=max([iframe-frames_per_page,-frames_per_page])
      g_p.plot_zoom=0
    endif
endif
if control eq 'previous' then begin
                if g_p.plot_zoom then begin
                   pan_xmin(g_p.plot_panel)=s_p.xmin(g_p.plot_panel) 
                   pan_xmax(g_p.plot_panel)=s_p.xmax(g_p.plot_panel)
                   pan_ymin(g_p.plot_panel)=s_p.ymin(g_p.plot_panel) 
                   pan_ymax(g_p.plot_panel)=s_p.ymax(g_p.plot_panel)
                   row(g_p.plot_panel)=s_p.row(g_p.plot_panel) 
                   col(g_p.plot_panel)=s_p.col(g_p.plot_panel)
                   xmin=s_p.data_xmin & xmax=s_p.data_xmax
                   pan_dx=s_p.pan_dx & pan_dy=s_p.pan_dy
                   g_p.plot_zoom=0
                 endif
                 iframe=max([iframe-2*frames_per_page,-frames_per_page])
endif
if control eq 'next' then begin
                if g_p.plot_zoom then begin
                   pan_xmin(g_p.plot_panel)=s_p.xmin(g_p.plot_panel) 
                   pan_xmax(g_p.plot_panel)=s_p.xmax(g_p.plot_panel)
                   pan_ymin(g_p.plot_panel)=s_p.ymin(g_p.plot_panel) 
                   pan_ymax(g_p.plot_panel)=s_p.ymax(g_p.plot_panel)
                   row(g_p.plot_panel)=s_p.row(g_p.plot_panel) 
                   col(g_p.plot_panel)=s_p.col(g_p.plot_panel)
                   pan_dx=s_p.pan_dx & pan_dy=s_p.pan_dy
                   xmin=s_p.data_xmin & xmax=s_p.data_xmax
                   g_p.plot_zoom=0
                 endif
endif
return,1
end

