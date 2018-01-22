; *************************************************************************
; FUNCTION
;      plo_page.pro
;
; WRITTEN
;      October 21, 1998  Kim Drongesen
;      Sep 18, 2000      modified to handle 'map' plot_type
;                        remove redundant parameters (syl)
;
; PURPOSE
;      Plots a page w/ multiple panels.
;      To understand how this routine is set up, see its useage in plo_cont.pro.
;
; INPUTS
;      plid   -- plot structure id
;      sindex -- index into con,gai,wlm,map,etc... structures
; OUTPUT
;      return 1 on success
;
; EXAMPLES
;      result=plo_page(plid,sindex)
;
; parameters : passed in structure s_page
;              general to all plots
;                 j_first,j_last       -- panel indexes to be plotted
;                 frames_per_page      -- number of frames per page
;                 nframes              -- total number of frames
;                 color_index          -- color index for each distinct_color
;                 row                  -- row on page for frame j
;                 nrow                 -- number of rows 
;                 col,ncol             -- similar to rows
;                 nor_xmax,nor_ymax    -- normalized coord max for display
;                 pan_xmin ...         -- normalized coord xmin ... for panel
;                 pan_dx ..            -- normalized delta x for panel
;                 xmin ..              -- data xmin ...
;                 ymin ..              -- data ymin ...(one for each frame,panel)
;              used in 'con','spe','wlm' plots
;                 symbol_pt_index,symbol_line_index -- similar to color_index
;                 xs                   -- x-coords of data
;                 ys                   -- y-coords of data 
;                                         (eg ys(3,n) for 3 sub_panel plot)
;                 x_var                -- label for x-coord
;                 y_var                -- label for y-coord
;                 wts                  -- weight of each data point
;                                         (negative wt points are circled & red)
;                 bottom_label         -- label at bottom of plot
;                 pt_first             -- index in xs and ys for first pt of j
;                 pt_npts              -- number of pts for j
;                 multi_pt             -- 0 => 1 pt per header entry (eg. int-ave
;                                         data) , 1 => multiple pts per header
;                                         entry (eg. cont. records)
;                 frames               -- frame variable char strings
;                                         (one entry for each data point)
;                 distinct_frames      -- char string characterizing each frame
;                 colors & symbols     -- similar to frames
;                 nsub                 -- number of sub_panels
;                 sub_fxmin ..         -- fraction of panel to move to sub-panel
;                                         xmin ..
;                 psym                 -- sym to use for plotted points or lines
;                 plot_scale           -- returned data to norm coord transforms
;              used in 'map' plots
;                 levels               -- levels for contour plots
;                 u,v                  -- u, v data points
;                 framedata            -- all frame data
;                                         [uv-coverage, cfactor, dirtybeam, dirtymap,
;                                          clean comp, residual, cleanbeam, cleanmap]
;                                         in continuum mode
;                                         [uv-coverage, cfactor, dirtybeam, 0-moment,
;                                          clean channels] in speccube mode
;                 framemax             -- maximum value used for each frame to be
;                                         normalized to get contours
;                 frametitle           -- title array for frames
;                 npix                 -- map size in pixels
;                 x,y                  -- x,y coord for maps
;
; *************************************************************************
;
function plo_page,plid,sindex

common global
common data_set
common plo

case pl[plid].plot_type of
      'con': s_page=con[sindex]
      'spe': s_page=spe[sindex]
      'gai': s_page=gai[sindex]
      'pas': s_page=pas[sindex]
      'wlm': s_page=wlm[sindex]
      'map': s_page=map[sindex]
      'var': s_page=var[sindex]
endcase

j_first=s_page.j_first & j_last=s_page.j_last
frames_per_page=s_page.frames_per_page & nframes=s_page.nframes       
color_index=s_page.color_index
row=s_page.row & col=s_page.col
nrow=s_page.nrow & ncol=s_page.ncol 
nor_xmax=s_page.nor_xmax & nor_ymax=s_page.nor_ymax 
pan_xmin=s_page.pan_xmin & pan_xmax=s_page.pan_xmax
pan_ymin=s_page.pan_ymin & pan_ymax=s_page.pan_ymax
pan_dx=s_page.pan_dx & pan_dy=s_page.pan_dy
xmin=s_page.xmin & xmax=s_page.xmax
ymin=s_page.ymin & ymax=s_page.ymax

if (pl[plid].plot_type ne 'map') then begin
  symbol_pt_index=s_page.symbol_pt_index
  symbol_line_index=s_page.symbol_line_index 
  xs=s_page.xs & x_var=s_page.x_var
  ys=s_page.ys & y_var=s_page.y_var
  wts=s_page.wts 
  bottom_label=s_page.bottom_label
  pt_first=s_page.pt_first & pt_npts=s_page.pt_npts
  multi_pt=s_page.multi_pt 
  frames=s_page.frames & distinct_frames=s_page.distinct_frames
  colors=s_page.colors & distinct_colors=s_page.distinct_colors
  symbols=s_page.symbols & distinct_symbols=s_page.distinct_symbols
  nsub=s_page.nsub
  sub_fxmin=s_page.sub_fxmin & sub_fxmax=s_page.sub_fxmax
  sub_fymin=s_page.sub_fymin & sub_fymax=s_page.sub_fymax
  psym=s_page.psym & plot_scale=s_page.plot_scale
endif else begin
  levels=s_page.levels
  u=s_page.u & v=s_page.v
  framedata=s_page.framedata
  framemax=s_page.framemax
  frametitle=s_page.frametitle
  npix=s_page.npix
  x=s_page.x & y=s_page.y
endelse

;
; Initialize zbuffer
;
if strupcase(!d.name) ne 'PS' then result=plo_zbuf(/init)
 erase

;
; now plot frames
;
begin_page:
 loadct,39,/silent
 erase
 ;
 ; use black lines for ps and set single color to blue
 ;
 if strlowcase(!d.name) eq 'ps' then begin
       color_index_save=color_index
;      color_index=make_array(n_elements(color_index),/float,value=0.)
 endif else begin
       if n_elements(color_index) eq 1 then color_index=70
 endelse
 
 char_norm=(1.-(1.-0.6)*(frames_per_page/15.)) > 0.6


if (pl[plid].plot_type ne 'map') then begin
    ;
    ; annotate w/ the date label, track or ref_time, and keys for
    ; symbols (on bottom) and colors (on bottom if <= 2, left if more) 
    ;
    xsy=0.1
    if n_elements(distinct_symbols) gt 2 then begin
      xc0=0.97
      dxc=0.
      yc0=nor_ymax
      dyc=0.9/n_elements(distinct_symbols)
    endif
    for j=0,n_elements(distinct_symbols)-1 do begin
      if distinct_symbols(j) ne '' then begin
        if n_elements(distinct_symbols) le 2 then begin
          xsy=xsy+0.3
          if j eq 0 then xyouts,xsy,0.005,font=-1,charsize=char_norm, $
              distinct_symbols(j)+' : solid line, *',alignment=0.5,/norm
          if j eq 1 then xyouts,xsy,0.005,font=-1,charsize=char_norm, $
              distinct_symbols(j)+' : dotted line, +',alignment=0.5,/norm
        endif else begin
           xyouts,xc0+(j+1)*dxc,yc0-j*dyc,font=-1,charsize=char_norm, $
               distinct_symbols(j),alignment=0.5,/norm
        endelse
      endif
    endfor
    xc0=xsy
    dxc=0.2
    yc0=0.02
    yc0=min([pan_ymin(j_first:j_last)])-0.08
    dyc=0.
    if n_elements(distinct_colors) gt 2 then begin
      xc0=0.97
      dxc=0.
      yc0=nor_ymax
      dyc=0.9/n_elements(distinct_colors)
    endif
    for j=0,n_elements(distinct_colors)-1 do begin
       xyouts,xc0+(j+1)*dxc,yc0-j*dyc,color=color_index(j), $
               distinct_colors(j),alignment=0.5,/norm,font=-1,charsize=char_norm
    endfor
    xyouts,nor_xmax-0.1,(nor_ymax+0.06) < 0.98,pl[plid].plot_date, $
        alignment=0.5,/norm,font=-1,charsize=char_norm

    yc0=0.005
    xyouts,0.01,yc0,bottom_label,alignment=0.0,/norm,font=-1,charsize=char_norm
    ;
    ; now do each frame on page
    ;
    for j=j_first,j_last do begin
      xyouts,pan_xmax(j)-0.22*pan_dx,pan_ymax(j)-0.13*pan_dy, $
                   distinct_frames(j),alignment=0.5,/norm,font=-1,charsize=char_norm
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
        plot_scale(j-j_first,m,0,0:1)=!x.s[0:1]
        plot_scale(j-j_first,m,1,0:1)=!y.s[0:1]
        for k=0,n_elements(distinct_colors)-1 do begin
          ks=where(distinct_colors(k) eq colors(js),count_ks)
          if count_ks gt 0 then begin
            ks=js(ks)
            for l=0,n_elements(distinct_symbols)-1 do begin
              kss=where(distinct_symbols(l) eq symbols(ks),count_kss)
              if count_kss le 0 then goto, skip_plot
              ls=ks(kss)
              if multi_pt then begin        
                lls=0
                for n=0,n_elements(ls)-1 do begin
                  if pt_npts(ls(n)) gt 0 then  $
                    lls=[lls,pt_first(ls(n))+indgen(pt_npts(ls(n)))]
                endfor
                if n_elements(lls) le 1 then goto, skip_plot 
                  ls=lls(1:n_elements(lls)-1)
                endif
                j_neg=where(wts(ls) lt 0.,count_neg)
                if psym(m) eq 10 then begin
                   xss=xs[ls] & yss=reform(ys[m,ls]) & np=n_elements(xss)
                   if np gt 2 then begin
                     xss=[xss[0]+(xss[0]-xss[1])/2.,xss[0:np-1], $
                        xss[np-1]+(xss[np-1]-xss[np-2])/2.]
                     yss=[yss[0],yss,yss[np-1]]
                   endif
                   oplot,xss,yss,psym=psym(m), $
                         min_value=!MIN_VALUE,color=color_index(k), $
                         linestyle=symbol_line_index(l)
                endif
                if psym(m) ne 10 then oplot,xs(ls),ys(m,ls),psym=symbol_pt_index(l), $
                         min_value=!MIN_VALUE,color=color_index(k), $
                         linestyle=symbol_line_index(l)
                if count_neg gt 0 then oplot,xs(ls[j_neg]),ys(m,ls[j_neg]),psym=6, $
                         min_value=!MIN_VALUE,color=200, $
                         linestyle=symbol_line_index(l)
                skip_plot:
            endfor
          endif
        endfor
        ;
        ; put ticks and numbers on exterior axes (eg. xaxis=0 => bottom, point up)
        ;
        fchar=char_norm & yticks=0
        if frames_per_page ge 4 and m gt 1 then fchar=0.01
        if frames_per_page ge 4 and m gt 1 then yticks=4
        if row(j) eq 0 then $                                  ; <== top
             axis,pan_xmin(j),pan_ymax(j),xaxis=1,xstyle=1,xcharsize=fchar,/norm,font=-1
        if row(j) eq nrow-1 or j+ncol gt nframes-1 then begin  ; <== bottom
             axis,pan_xmin(j),pan_ymin(j),xaxis=0,xstyle=1,xcharsize=fchar,/norm,font=-1
             xyouts,(pan_xmin(j)+pan_xmax(j))/2.,pan_ymin(j)-0.07, $
             x_var,alignment=0.5,orientation=0,/norm,font=-1,charsize=char_norm
        endif
        if col(j) eq 0 then begin                              ; <== left
             axis,pan_xmin(j),pan_ymin(j),yaxis=0,ystyle=1,ycharsize=fchar, $
                  /norm,yticks=yticks,font=-1
             xyouts,!p.position(0)-0.07,(!p.position(1)+!p.position(3))/2., $
                  y_var(m),alignment=0.5,orientation=0,/norm, $
                  font=-1,charsize=char_norm
        endif
        if col(j) eq ncol-1 then  $                            ; <== right
                axis,pan_xmax(j),pan_ymin(j),yaxis=1,ystyle=1,ycharsize=fchar, $
                /norm,yticks=yticks,font=-1
        ;
        ; put ticks on interior axes (charsize=0.01 => leave off numbers)
        ;
        if row(j) ne nrow-1 and j+ncol le nframes-1 then  $ ; <== tics down
              axis,pan_xmin(j),pan_ymin(j),xaxis=1,xstyle=1,/norm,xcharsize=0.01
        if row(j) ne nrow-1 then  $                         ; <== tics up
              axis,pan_xmin(j),pan_ymin(j),xaxis=0,xstyle=1, $
              /norm,xcharsize=0.01
        if col(j) ne ncol-1 then  $                         ; <== tics left
              axis,pan_xmax(j),pan_ymin(j),yaxis=1,ystyle=1, $
              /norm,ycharsize=0.01,yticks=yticks
        if col(j) ne ncol-1 and j ne nframes-1 then $       ; <== tics right
              axis,pan_xmax(j),pan_ymin(j),yaxis=0,ystyle=1, $
              /norm,ycharsize=0.01,yticks=yticks
        if m ne nsub-1 then begin              ; <== line between sub-panels
              axis,pan_xmin(j),pan_ymin(j)+sub_fymin(m)*pan_dy, $
              xaxis=0,xstyle=1,/norm,xcharsize=0.01,xthick=0.5,xgridstyle=2
        endif
      endfor
    endfor
endif else begin
    for i=j_first,j_last do begin       
        xyouts,nor_xmax-0.1,nor_ymax+0.08,pl.plot_date,alignment=0.5,/norm, $
             font=-1,charsize=char_norm
        !p.position=[pan_xmin(i),pan_ymin(i),pan_xmin(i)+pan_dx,pan_ymin(i)+pan_dy]
        if i eq 0 then begin
            plot,[u,-u],[v,-v],linestyle=1,psym=3, /nodata, $
                 xrange=[xmin(i),xmax(i)], yrange=[ymin(i),ymax(i)], $
                 xstyle=1, ystyle=1, title=frametitle(i),/noerase
            oplot,u,v,linestyle=1,psym=3, $
            color=color_index[i]
        endif
        if i gt 0 then begin
            contour,framedata(*,*,i)/framemax(i),x,-y,levels=levels, $
                 xrange=[xmin(i),xmax(i)],yrange=[ymin(i),ymax(i)], xstyle=1, ystyle=1, $
                 c_colors=color_index[i],c_linestyle=(levels lt 0.0),title=frametitle(i),/noerase 
        endif
    endfor

endelse

if strlowcase(!d.name) eq 'ps' then color_index=color_index_save
!p.position=[0.,0.,0.,0.]
if (pl[plid].plot_type ne 'map') then s_page.plot_scale=plot_scale
case pl[plid].plot_type of
      'con': con[sindex]=s_page
      'spe': spe[sindex]=s_page
      'gai': gai[sindex]=s_page
      'pas': pas[sindex]=s_page
      'wlm': wlm[sindex]=s_page
      'map': map[sindex]=s_page
      'var': var[sindex]=s_page
endcase
;
; must separate gai and pas since they plot_over_page
;
if strupcase(!d.name) ne 'PS' and pl[plid].plot_type ne 'gai'  $
    and pl[plid].plot_type ne 'wlm' $
    and pl[plid].plot_type ne 'pas' then begin
  result=plo_zbuf(java=e.java)
endif

return,1

end
