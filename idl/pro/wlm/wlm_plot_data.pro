; *************************************************************************
; FUNCTION
;      wlm_plot_data
;
; WRITTEN 
;      October 21, 1998 by JMC
;
; PURPOSE
;      Plotting utility for the WLM routines
;
; INPUTS 
;      XDATA       : Input vector containing X axis data for all panels
;                    Dimensions: number of data points in all panels,
;                                except if /expand is set.
;      YS          : Input array containing Y axis data
;                    Dimensions: Number of sub panels X total number of data 
;                                points
;      WTS         : Weights of the points in XS.
;                      <= 0   --> point is not plotted
;                      >  0   --> point is     plotted
;      FRAME_IDS   : Vector indicating which points in XS and YS belong to
;                    which subpanel. 
;                    Dimensions: Number of data points
;      X_VAR       : Plot label for the X variable
;      Y_VAR       : Plot label for the Y variable
;      NFRAMES_MAX : Maximum number of frames to plot per page
;      PSYM        : Vector containing the plot symbols for IDL
;      PLOT        : Indicates whether to enter interactive (flagging) mode
;      BLABEL      : Label to put at bottom of the plot
;      YFIT        : If present, plot a second array on top of the YS data.
;      M_OPTIONS   : The available options when hitting the middle mouse button
;      EXPAND      : Expand XDATA to full dimensions of YS
;      SAME        : Residuals have same plot limits as raw data
;      PLID        : plot id - generated in IDL only
;      BAD_VALUE   : Ignore data <= BAD_VALUE; default: !BAD_VALUE
;
; OUTPUT
;      An integer indicating data were (1) or were not (0) flagged.
;
; CALLED BY
;      wlm_box_gain
;      wlm_derive
;
; EXAMPLES
;      flag_data = wlm_plot_data(xs,ys,weights,frame_ids,$
;                     "WLM", "Phase",plot=plot,$
;                     psym=[3,0],yfit=yfs,blabel='WLM data vs mm phase',$
;                     m_options='csfpne',nframes_max=15)
;
; *************************************************************************
function wlm_plot_data,xdata,ys,wts,frame_ids,$
                       x_var,y_var,nframes_max=nframes_max,$
                       psym=psym,plot=plot,expand=expand,same=same,$
                       blabel=blabel,yfit=yfit,m_options=m_options,$
                       bad_value=bad_value,plid
  ; Common blocks
    common global
    common data_set
    common plo

  ; Interactive plot?
    if (not keyword_set(plot)) then return,0

  ; Set bad value
    if not keyword_set(bad_value) then bad_value = !BAD_VALUE

  ; Initialize
    if not e.java then begin 
       plid = plo_plid_gen()
    endif
    windex = 0

  ; Expand X data
    xs = xdata
    if keyword_set(expand) then begin
       j = lindgen(!NTEL,n_elements(xdata))/!NTEL
       xs = reform(xdata(j),n_elements(j))
    endif else $
       xs = xdata

  ; Initialize plot types
    pl[plid].plot_type = 'wlm'
    if not e.java then pl[plid].plot_interact=plo_init(plid,windex)

  ; Set maximum number of frames per page
    if not keyword_set(nframes_max) then nframes_max = 6L

  ; Set options for middle mouse button
    if not keyword_set(m_options) then m_options = 'csfpne'
    
  ; Set number of subpanels and frame ids
    result = size(ys)
    nsub   = result(1)
    frames = frame_ids

  ; Set axes type and symbols
    if not keyword_set(x_var) then x_var = "Unknown"
    if not keyword_set(y_var) then $
        y_var = make_array(nsub,/string,value="Unknown")
    if not keyword_set(blabel) then blabel = ""
    if not keyword_set(psym) then psym = make_array(nsub,/int,value=10)
    bottom_label = blabel
    symbol_pt_index=psym 
    symbol_line_index=0

  ; Set color/plot symbols and number of frames
    colors=''
    symbols=''
    distinct_colors=''
    distinct_symbols=''
    j = where(wts gt 0, nj)
    if (nj gt 0) then begin
       distinct_frames = uti_distinct(frames(j),nframes,/many)
    endif else begin
       printf,-1,'Error setting distinct_frames in wlm_plot_data'
       stop
    endelse
    color_index=0.6*bytscl((indgen(max([n_elements(distinct_colors),1])) $
            mod 16)+1)+64.

  ; Set viewport
    nor_xmin=0.1 
    nor_xmax=0.95 
    nor_ymin=0.1 
    nor_ymax=0.94
    if (n_elements(distinct_colors) gt 2) then nor_xmax=0.95-0.05
    nor_dx=nor_xmax-nor_xmin 
    nor_dy=nor_ymax-nor_ymin

  ; Set number of rows and columns
    frames_per_page = min([nframes,nframes_max])
    ncol = max([long(sqrt(frames_per_page)),1])
    nrow = frames_per_page/ncol
    if (frames_per_page mod ncol ne 0) then nrow = nrow + 1

  ; Generate a page and frame location for each frames
    page = indgen(nframes)/frames_per_page
    frame_on_page = indgen(nframes)-page*frames_per_page
    row = frame_on_page/ncol & col=frame_on_page-row*ncol
    pan_dx   = nor_dx/ncol
    pan_dy   = nor_dy/nrow
    pan_xmin = nor_xmin+col*pan_dx 
    pan_xmax = nor_xmin+(col+1)*pan_dx
    pan_ymax = nor_ymax-row*pan_dy 
    pan_ymin = nor_ymax-(row+1)*pan_dy

  ; Allocate memory for plot limits
  ; Set X plot limits
    jx   = where(xs gt bad_value,njx)
    xmin = make_array(nsub,/float,value=min(xs[jx]))
    xmax = make_array(nsub,/float,value=max(xs[jx]))
    dx   = xmax - xmin
    xmin = xmin - 0.05*dx 
    xmax = xmax + 0.05*dx 

  ; Set Y limits
    ymin = fltarr(nframes,nsub)
    ymax = fltarr(nframes,nsub)
    if keyword_set(same) then begin
       jy = where(ys gt bad_value)
       y1 = min(ys[jy])
       y2 = max(ys[jy])
       if (keyword_set(yfit)) then begin
          y1 = min([yfit[jy],y1])
          y2 = max([yfit[jy],y2])
       endif
       for i = 0, nsub-1 do begin
         ymin[*,i] = y1
         ymax[*,i] = y2
       endfor
    endif else begin
       for i = 0L, nsub-1L do begin
           jy = where(ys[i,*] gt bad_value,njy)
           if (njy gt 0) then begin
             ymin[*,i] = min(ys[i,jy])
             ymax[*,i] = max(ys[i,jy])
             if (keyword_set(yfit)) then begin
                y1 = min(yfit[i,jy])
                y2 = max(yfit[i,jy])
                ymin[*,i] = min([y1,ymin[*,i]])
                ymax[*,i] = max([y2,ymax[*,i]])
             endif
           endif else begin
             ymin[*,i] = min(ys[i,*])
             ymax[*,i] = max(ys[i,*])
             if (keyword_set(yfit)) then begin
                y1 = min(yfit[i,*])
                y2 = max(yfit[i,*])
                ymin[*,i] = min([y1,ymin[i,*]])
                ymax[*,i] = max([y2,ymax[i,*]])
             endif
           endelse
       endfor
    endelse
    dy = ymax-ymin 
    ymin = ymin - 0.10*dy 
    ymax = ymax + 0.10*dy

  ; Setup sub-panels : sub_fxmin => fraction of panel dx to offset from min
  ;                    sub_fxmax => fraction of panel dx to offset from max
  ; Note : 0'th subpanel at top
    sub_fymin = reverse(findgen(nsub)/nsub)
    sub_fymax = sub_fymin + 1.0/nsub
    sub_fxmin = make_array(nsub,/float,value=0.)
    sub_fxmax = make_array(nsub,/float,value=1.)

  ; save the original parameters
    s_p={xmin:pan_xmin,xmax:pan_xmax,ymin:pan_ymin,ymax:pan_ymax, $
         pan_dx:pan_dx,pan_dy:pan_dy,row:row,col:col,data_xmin:xmin, $
         data_xmax:xmax,data_ymin:ymin,data_ymax:ymax, $
         nor_xmin:nor_xmin,nor_xmax:nor_xmax,nor_ymin:nor_ymin, $
         nor_ymax:nor_ymax}

  ; Initialize some plot variables
    iframe = 0
    pt_first=0
    pt_npts=0
    multi_pt=0
    fit=1
    iflag = 0

  ; If overplotting a fit, create special sorted arrays
    y = [0]
    if (keyword_set(yfit)) then begin
       x = xs
       y = yfit
       w = ABS(wts)
       jx = where(xs gt bad_value,njx)
       for i = 0L, nframes-1L do begin
          j = where(distinct_frames[i] eq frames(jx),nj)
          if (nj gt 0) then begin
             js = sort(xs(jx(j)))
             x(jx(j)) = xs(jx(j(js)))
             for k = 0L, nsub-1L do y(k,jx(j)) = yfit(k,jx(j(js)))
          endif
       endfor
    endif

  ; save the original parameters
    saved_par={xmin:pan_xmin,xmax:pan_xmax,ymin:pan_ymin,ymax:pan_ymax, $
       pan_dx:pan_dx,pan_dy:pan_dy,row:row,col:col,data_xmin:xmin, $
       data_xmax:xmax,data_ymin:ymin,data_ymax:ymax, $
       nor_xmin:nor_xmin,nor_xmax:nor_xmax,nor_ymin:nor_ymin, $
       nor_ymax:nor_ymax,nframes:nframes,frames_per_page:frames_per_page, $
   nrow:nrow,ncol:ncol}

  ; Create WLM plotting structure
    npts = n_elements(xs)
    wlm_par={saved_par:saved_par,nor_xmax:nor_xmax,nor_ymax:nor_ymax, $
         nor_dx:nor_dx,nor_dy:nor_dy,frames:frames, $
         distinct_frames:distinct_frames,colors:colors, $
         distinct_colors:distinct_colors,symbols:symbols, $
         distinct_symbols:distinct_symbols,pan_xmin:pan_xmin, $
         pan_xmax:pan_xmax,pan_ymin:pan_ymin,pan_ymax:pan_ymax,$
         pan_dx:pan_dx,pan_dy:pan_dy, $
         sub_fxmin:sub_fxmin,sub_fxmax:sub_fxmax, $
         sub_fymin:sub_fymin,sub_fymax:sub_fymax,$
         xmin:xmin,xmax:xmax,ymin:ymin,$
         ymax:ymax,psym:psym,row:row,col:col,$
         plot_scale:make_array(frames_per_page,nsub,2,2,/float),$
         iframe:0,j_first:0,j_last:0, $
         frames_per_page:frames_per_page,nframes:nframes, $
         color_index:color_index,symbol_pt_index:symbol_pt_index, $
         symbol_line_index:symbol_line_index,nsub:nsub,xs:xs,ys:ys,yfs:y,$
         x_var:x_var,y_var:y_var,wts:wts,nrow:nrow,ncol:ncol, $
         bottom_label:bottom_label,pt_first:pt_first,pt_npts:pt_npts, $
         multi_pt:multi_pt,initial:0, $
         m_options:'',control:'',$
         j_select:intarr(npts),i_select:intarr(npts),$
         x_select:fltarr(npts),y_select:fltarr(npts),$
         m_button_select:intarr(npts),n_select:0}

wlm=replicate(wlm_par,10)
	
  ; Set number of plotting pages
    pl[plid].num_pages = ceil(float(wlm[windex].nframes) / float(wlm[windex].frames_per_page))

  ; Set zoom/unzoom parameters
    j_first=iframe
    j_last=min([iframe+frames_per_page-1,nframes-1])
    fpp = frames_per_page
    nf = nframes
    nr = nrow
    nc = ncol
    not_zoomed = 1;
    wlm[windex].iframe = iframe
    wlm[windex].j_first = j_first
    wlm[windex].j_last  = j_last

  ; Store in WLM structure
    wlm[windex].color_index=color_index+20.
    wlm[windex].frames_per_page = fpp
    wlm[windex].nframes = nf
    wlm[windex].nrow = nr
    wlm[windex].ncol = nc

  ; Plot data
    result=plo_page(plid,windex)
    if (keyword_set(yfit)) then begin
      psym_old = wlm[windex].psym
      ys_old = wlm[windex].ys
      wlm[windex].psym = intarr(n_elements(psym_old))
      wlm[windex].ys = yfit
      result=plo_over_page(plid,windex)
      wlm[windex].psym = psym_old
      wlm[windex].ys = ys_old
    endif
    wts_start = wlm[windex].wts
    wlm[windex].m_options = m_options

  ; Plot the z buffer
    if (pl[plid].plot_device ne 'PS') then begin
      result=plo_zbuf(java=e.java)
    endif

  ; Get cursor input
    if not e.java then begin
       pl[plid].plot_interact = plo_control(plid,windex)
       plo_plid_rel,plid
    endif

    return,iflag
end
