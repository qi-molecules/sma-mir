function plo_zoom,plot_type
;
; Handles plot zooming through mouse.
; parameters : passed in structure s_page 
;            : i_first -- index to first panel on screen
;            : i_last  -- index to last panel on screen
;            : pan_xmin,pan_xmax  -- min and max normal x-coord of panels
;            : pan_ymin,pan_ymax  -- min and max normal y-coord of panels
;            : row,col    -- row and col numbers of panels
;            : xmin,xmax    -- min and max data x-coord of panels
;            : ymin,ymax    -- min abd max data y-coord of panels
;            : xs,ys        -- x,y values of data points
;            : frames       -- frames label of each data point
;            : distinct_frames -- distinct list of frame value
;            : y_var        -- y lables for subpanels
;            : pt_first     -- index in xs and ys for first pt of j
;            : pt_npts      -- number of pts for j
;            : multi_pt     -- 0 => 1 pt per header entry (eg. int-ave
;                                   data) , 1 => multiple pts per header
;                                   entry (eg. cont. records)
;            : control      -- 'zoom','unzoom','previous','next'
; result = 0 (non-interactive) 1 (interactive)
;
; eg. : result=plo_zoom()
;
common global
common plo

iframe=s_page.iframe
i_first=s_page.j_first & i_last=s_page.j_last
frames_per_page=s_page.frames_per_page & xs=s_page.xs
x_var=s_page.x_var & y_var=s_page.y_var
ys=s_page.ys & pt_first=s_page.pt_first
pt_npts=s_page.pt_npts & multi_pt=s_page.multi_pt
control=s_page.control
nor_xmax=s_page.nor_xmax & nor_ymax=s_page.nor_ymax
nor_dx=s_page.nor_dx & nor_dy=s_page.nor_dy
frames=s_page.frames & distinct_frames=s_page.distinct_frames
pan_xmin=s_page.pan_xmin & pan_xmax=s_page.pan_xmax
pan_ymin=s_page.pan_ymin & pan_ymax=s_page.pan_ymax
pan_dx=s_page.pan_dx & pan_dy=s_page.pan_dy
xmin=s_page.xmin & xmax=s_page.xmax
ymin=s_page.ymin & ymax=s_page.ymax
row=s_page.row & col=s_page.col

;
if not pl.plot_interact then return,0
;
; zoom on a range of the data after having zoomed to one panel
;
save_par=0
if control eq 'zoom' then begin
  if pl.plot_zoom and pl.mouse_button eq 'l' and pl.plot_panel ne -1 then begin
    xl=pl.mouse_x
    yl=pl.mouse_y
    range_pick:
    print,'*** use left mouse to pick right edge to zoom on'
    cursor,x,y,3,/norm
    pl.mouse_x=x & pl.mouse_y=y
    case !mouse.button of
      1: pl.mouse_button='l' 
      2: pl.mouse_button='m' 
      4: pl.mouse_button='r'
    endcase
    if pl.mouse_button eq 'l' then begin
      plot_panel=-1
      for i=i_first,i_last do begin
        if pl.mouse_x ge pan_xmin(i) and pl.mouse_x le pan_xmax(i) and  $
           pl.mouse_y ge pan_ymin(i) and pl.mouse_y le pan_ymax(i) then $
               plot_panel=i
      endfor
      if plot_panel ne pl.plot_panel then begin
        print,'*** must pick w/i plot area !'
        goto, range_pick
      endif 
      result=convert_coord([xl,pl.mouse_x],[yl,pl.mouse_y],/normal,/to_data)
      xmin(*)=result(0,0)
      xmax(*)=result(0,1)
      js=where(distinct_frames(plot_panel) eq frames,count)
      if count gt 0 then begin
        if multi_pt then begin        
          lls=0
          for n=0,n_elements(js)-1 do begin
            if pt_npts(js(n)) gt 0 then  $
              lls=[lls,pt_first(js(n))+indgen(pt_npts(js(n)))]
          endfor
          js=lls(1:n_elements(lls)-1)
        endif
        jjs=where(xs(js) ge result(0,0) and xs(js) le result(0,1),count)
        if count gt 0 then begin
          js=js(jjs)
          for i=0,n_elements(y_var)-1 do begin
            if y_var(i) ne 'coh' then begin
              jjs=where(ys[i,js] ne !BAD_VALUE,count)
              if count gt 0 then begin
                js_good=js(jjs)
                incr=1.
                if y_var(i) eq 'pha' then incr=20.
                min_y=min([ys(i,js_good)]) & max_y=max([ys(i,js_good)])
                if y_var(i) eq 'amp' then begin
                  incr=0.2
                  if max_y gt 5. then incr=1.
                endif
                uti_range_even,min_y,max_y, $
                       incr,min_rnd,max_rnd
                if y_var(i) ne 'amp' then ymin(plot_panel,i)=min_rnd
                ymax(plot_panel,i)=max_rnd
              endif
            endif
          endfor
        endif
      endif
    endif 
  endif
  if pl.mouse_button eq 'l' and pl.plot_panel ne -1 then begin
    if e.debug then print,'zooming panel ',pl.plot_panel
    i_first=pl.plot_panel & i_last=pl.plot_panel
    s_page.j_first=i_first & s_page.j_last=i_last
    pan_xmin(pl.plot_panel)=s_page.saved_par.nor_xmin 
    pan_xmax(pl.plot_panel)=s_page.saved_par.nor_xmax 
    pan_ymin(pl.plot_panel)=s_page.saved_par.nor_ymin 
    pan_ymax(pl.plot_panel)=s_page.saved_par.nor_ymax
    pan_dx=nor_dx & pan_dy=nor_dy
    row(pl.plot_panel)=0 & col(pl.plot_panel)=0 & save_par=1
    pl.plot_zoom=1
  endif
endif
if control eq 'unzoom' then begin
    if pl.plot_zoom then begin
      pan_xmin(pl.plot_panel)=s_page.saved_par.xmin(pl.plot_panel) 
      pan_xmax(pl.plot_panel)=s_page.saved_par.xmax(pl.plot_panel)
      pan_ymin(pl.plot_panel)=s_page.saved_par.ymin(pl.plot_panel) 
      pan_ymax(pl.plot_panel)=s_page.saved_par.ymax(pl.plot_panel)
      row(pl.plot_panel)=s_page.saved_par.row(pl.plot_panel) 
      col(pl.plot_panel)=s_page.saved_par.col(pl.plot_panel)
      xmin=s_page.saved_par.data_xmin & xmax=s_page.saved_par.data_xmax
      ymin=s_page.saved_par.data_ymin & ymax=s_page.saved_par.data_ymax
      pan_dx=s_page.saved_par.pan_dx & pan_dy=s_page.saved_par.pan_dy
      iframe=max([iframe-frames_per_page,-frames_per_page]) & save_par=1
      s_page.iframe=iframe
      pl.plot_zoom=0
    endif
endif
if control eq 'previous' then begin
                if pl.plot_zoom then begin
                   pan_xmin(pl.plot_panel)=s_page.saved_par.xmin(pl.plot_panel) 
                   pan_xmax(pl.plot_panel)=s_page.saved_par.xmax(pl.plot_panel)
                   pan_ymin(pl.plot_panel)=s_page.saved_par.ymin(pl.plot_panel) 
                   pan_ymax(pl.plot_panel)=s_page.saved_par.ymax(pl.plot_panel)
                   row(pl.plot_panel)=s_page.saved_par.row(pl.plot_panel) 
                   col(pl.plot_panel)=s_page.saved_par.col(pl.plot_panel)
                   xmin=s_page.saved_par.data_xmin 
                   xmax=s_page.saved_par.data_xmax
                   pan_dx=s_page.saved_par.pan_dx
                   pan_dy=s_page.saved_par.pan_dy  & save_par=1
                   pl.plot_zoom=0
                 endif
                 iframe=max([iframe-2*frames_per_page,-frames_per_page])
                 s_page.iframe=iframe
endif
if control eq 'next' then begin
                if pl.plot_zoom then begin
                   pan_xmin(pl.plot_panel)=s_page.saved_par.xmin(pl.plot_panel) 
                   pan_xmax(pl.plot_panel)=s_page.saved_par.xmax(pl.plot_panel)
                   pan_ymin(pl.plot_panel)=s_page.saved_par.ymin(pl.plot_panel) 
                   pan_ymax(pl.plot_panel)=s_page.saved_par.ymax(pl.plot_panel)
                   row(pl.plot_panel)=s_page.saved_par.row(pl.plot_panel) 
                   col(pl.plot_panel)=s_page.saved_par.col(pl.plot_panel)
                   pan_dx=s_page.saved_par.pan_dx 
                   pan_dy=s_page.saved_par.pan_dy
                   xmin=s_page.saved_par.data_xmin
                   xmax=s_page.saved_par.data_xmax  & save_par=1
                   pl.plot_zoom=0
                 endif
endif
if save_par then begin
  s_page.pan_xmin=pan_xmin & s_page.pan_xmax=pan_xmax
  s_page.pan_ymin=pan_ymin & s_page.pan_ymax=pan_ymax
  s_page.pan_dx=pan_dx & s_page.pan_dy=pan_dy
  s_page.xmin=xmin & s_page.xmax=xmax
  s_page.ymin=ymin & s_page.ymax=ymax
  s_page.row=row & s_page.col=col
endif
return,1
end

