function plo_over_page,plid,sindex
;
; Plots an overlay on page w/ multiple panels (call after plo_page)
;
; To understand how this routine is set up, see its useage in gain.pro.
;
; parameters 
;	     : plid - plot index - index into pl array structure
;	     : sindex - index into con,gai,wlm, etc array
; parameters : passed in structure s_page 
;              nor_xmax,nor_ymax    -- normalized coord max for display
;              j_first,j_last       -- panel indexes to be plotted
;              frames_per_page      -- number of frames per page
;              nframes              -- total number of frames
;              frames               -- frame variable char strings
;                                      (one entry for each data point)
;              distinct_frames      -- char string characterizing each frame
;              colors & symbols     -- similar to frames
;              color_index          -- color index for each distinct_color
;              symbol_pt_index,symbol_line_index -- similar to color_index
;              pan_xmin ...         -- normalized coord xmin ... for panel
;              pan_dx ..            -- normalized delta x for panel
;              nsub                 -- number of sub_panels
;              sub_fxmin ..         -- fraction of panel to move to sub-panel
;                                      xmin ..
;              xmin ..              -- data xmin ...
;              xs                   -- x-coords of data
;              x_var                -- label for x-coord
;              y_var                -- label for y-coord
;              ys                   -- y-coords of data 
;                                     (eg ys(3,n) for 3 sub_panel plot)          ;              wts                  -- weight of each data point
;                                      (negative wt points not plotted)
;              psym                 -- sym to use for plotted points or lines
;              row                  -- row on page for frame j
;              nrow                 -- number of rows 
;              col,ncol             -- similar to rows
;              bottom_label         -- label at bottom of plot
;              pt_first             -- index in xs and ys for first pt of j
;              pt_npts              -- number of pts for j
;              multi_pt             -- 0 => 1 pt per header entry (eg. int-ave
;                                      data) , 1 => multiple pts per header
;                                      entry (eg. cont. records)
;
; eg. result=plo_over_page(plid,sindex)
;


common global
common data_set
common plo

; If the plot device is null, then return
if not e.java and strlowcase(pl[plid].plot_device) eq "null" then return,1

  case pl[plid].plot_type of
      'con': s_page=con[sindex] 
      'spe': s_page=spe[sindex]
      'gai': s_page=gai[sindex]
      'pas': s_page=pas[sindex]
      'wlm': s_page=wlm[sindex]
      'var': s_page=var[sindex]
  endcase
j_first=s_page.j_first & j_last=s_page.j_last
frames_per_page=s_page.frames_per_page & nframes=s_page.nframes       
color_index=s_page.color_index & symbol_pt_index=s_page.symbol_pt_index
symbol_line_index=s_page.symbol_line_index & nsub=s_page.nsub
xs=s_page.xs & x_var=s_page.x_var
y_var=s_page.y_var & ys=s_page.yfs
wts=s_page.wts & nrow=s_page.nrow
ncol=s_page.ncol & bottom_label=s_page.bottom_label
pt_first=s_page.pt_first & pt_npts=s_page.pt_npts
multi_pt=s_page.multi_pt & nor_xmax=s_page.nor_xmax
nor_ymax=s_page.nor_ymax & frames=s_page.frames
distinct_frames=s_page.distinct_frames & colors=s_page.colors
distinct_colors=s_page.distinct_colors
symbols=s_page.symbols & distinct_symbols=s_page.distinct_symbols
pan_xmin=s_page.pan_xmin & pan_xmax=s_page.pan_xmax
pan_ymin=s_page.pan_ymin & pan_ymax=s_page.pan_ymax
pan_dx=s_page.pan_dx & pan_dy=s_page.pan_dy
sub_fxmin=s_page.sub_fxmin & sub_fxmax=s_page.sub_fxmax
sub_fymin=s_page.sub_fymin & sub_fymax=s_page.sub_fymax
xmin=s_page.xmin & xmax=s_page.xmax
ymin=s_page.ymin & ymax=s_page.ymax
psym=s_page.psym & row=s_page.row
col=s_page.col & plot_scale=s_page.plot_scale

  if strlowcase(!d.name) eq 'ps' then begin
    color_index_save=color_index
;   color_index=make_array(n_elements(color_index),/float,value=0.)
  endif
;
;
  xsy=0.1
  xc0=xsy
  dxc=0.2
  yc0=0.02
  dyc=0.
  if n_elements(distinct_colors) gt 2 then begin
    xc0=0.97
    dxc=0.
    yc0=nor_ymax
    dyc=0.9/n_elements(distinct_colors)
  endif
  for j=0,n_elements(distinct_colors)-1 do begin
     xyouts,xc0+(j+1)*dxc,yc0-j*dyc,color=color_index(j), $
           distinct_colors(j),alignment=0.5,/norm
  endfor
;
; now do each frame on page
;   
  for j=j_first,j_last do begin
    js=where(distinct_frames(j) eq frames)
;
; set plot position and scale (style=5 => don't draw, use exact range)
;
if (pl[plid].plot_type eq 'pas' and n_elements(y_var) eq 1) then nsub=s_page.nsub-1
    for m=0,nsub-1 do begin
      !p.position=[pan_xmin(j)+sub_fxmin(m)*pan_dx, $
                   pan_ymin(j)+sub_fymin(m)*pan_dy, $
                   pan_xmin(j)+sub_fxmax(m)*pan_dx, $
                   pan_ymin(j)+sub_fymax(m)*pan_dy]
      plot,[xmin(m),xmax(m)],[ymin(j,m),ymax(j,m)], $
                       xstyle=5,ystyle=5,/noerase,/nodata
      for k=0,n_elements(distinct_colors)-1 do begin
        ks=js(where(distinct_colors(k) eq colors(js)))
      for l=0,n_elements(distinct_symbols)-1 do begin
        ls=ks(where(distinct_symbols(l) eq symbols(ks)))
        if multi_pt then begin        
          lls=0
          for n=0,n_elements(ls)-1 do begin
            if pt_npts(ls(n)) gt 0 then  $
              lls=[lls,pt_first(ls(n))+indgen(pt_npts(ls(n)))]
          endfor
          if n_elements(lls) le 1 then goto, skip_plot 
          ls=lls(1:n_elements(lls)-1)
        endif
;
; for overlay, plot as triangle and long dashes to descriminate
; from the underlay plots
;
  ls=ls(where(wts(ls) gt 0.))
  xss=xs[ls] & yss=reform(ys[m,ls]) & np=n_elements(xss)
;;  xss=xs[ls]
;;  if m gt 1 then yss=reform(ys[m,ls]) else yss=reform(ys[ls])
;;  np=n_elements(xss)

  if (pl[plid].plot_type eq 'gai' or pl[plid].plot_type eq 'pas') then begin
      if np gt 2 then begin
        xss=[xss[0]+(xss[0]-xss[1])/2.,xss[0:np-1], $
           xss[np-1]+(xss[np-1]-xss[np-2])/2.]
        yss=[yss[0],yss,yss[np-1]]
      endif
      oplot,xss,yss,psym=-3, $
          min_value=!MIN_VALUE,color=color_index(k),linestyle=0
  endif else begin 
        if psym(m) eq 10 or psym(m) eq 0 then begin
          if np gt 2 and psym(m) eq 10 then begin
             xss=[xss[0]+(xss[0]-xss[1])/2.,xss[0:np-1], $
                  xss[np-1]+(xss[np-1]-xss[np-2])/2.]
             yss=[yss[0],yss,yss[np-1]]
          endif
          oplot,xss,yss,psym=psym(m), $
                min_value=!MIN_VALUE,color=color_index(k),linestyle=5
        endif
        if psym(m) ne 10 and psym(m) ne 0 then $
                oplot,xss,yss,psym=5, $
                     min_value=!MIN_VALUE,color=color_index(k),linestyle=5
  endelse
      skip_plot:
      endfor
      endfor
    endfor
  endfor
  if strlowcase(!d.name) eq 'ps' then color_index=color_index_save
  !p.position=[0.,0.,0.,0.]
s_page.plot_scale=plot_scale
if strupcase(!d.name) ne 'PS' then result=plo_zbuf(java=e.java)
return,1
end
