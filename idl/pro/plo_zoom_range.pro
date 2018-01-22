; *************************************************************************
; FUNCTION
;      plo_zoom_range.pro
;
; WRITTEN
;      October 30, 1998 Kim Drongesen
;      Sep 18, 2000      modified to handle 'map' plot_type
;                        remove redundant parameters (syl)
;
; PURPOSE
;      This routine zooms on a range of data within zoomed panel.
;      In an IDL only environment - this routine is called  from
;      plo_control when the user does a left mouse click on a panel
;      which has already been zoomed. From MIR this routine is called
;      when then user drags on an already zoomed panel - and is in zoom mode.
;
; INPUTS
;      plid   -- plot structure id
;      sindex -- index into con,gai,wlm, map, etc ... structures
;
; OUTPUT
;      none
;
; EXAMPLES
;      plo_zoom_range,x1,y1,x2,y2,plid,sindex
;
; *************************************************************************
;
pro plo_zoom_range,x1,y1,x2,y2,plid,sindex

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
;Initialize everything that needs to be initialized -from plo_control & plo_zoom
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

if (pl[plid].plot_type ne 'map') then begin
 xs=s_page.xs & ys=s_page.ys
 x_var=s_page.x_var & y_var=s_page.y_var
 pt_first=s_page.pt_first
 pt_npts=s_page.pt_npts & multi_pt=s_page.multi_pt
 frames=s_page.frames & distinct_frames=s_page.distinct_frames
endif

result1 = convert_coord(x1,y1,/device,/to_normal)
if e.java then result1[1]=1-result1[1]
result2 = convert_coord(x2,y2,/device,/to_normal)
if e.java then result2[1]=1-result2[1]

pl[plid].mouse_x = result2[0]
if e.java then pl[plid].mouse_y = 1-result2[1] else pl[plid].mouse_y = result2[1]	;this fixes ION bug

;
;Take the range we have been given and zoom in on it
;
plot_panel=-1
for i=i_first,i_last do begin
   if result1[0] ge pan_xmin(i) and result1[0] le pan_xmax(i) and  $
      result1[1] ge pan_ymin(i) and result1[1] le pan_ymax(i) then $
      plot_panel=i
endfor

plot_pan2=-1
for i=i_first,i_last do begin
   if result2[0] ge pan_xmin(i) and result2[0] le pan_xmax(i) and  $
      result2[1] ge pan_ymin(i) and result2[1] le pan_ymax(i) then $
      plot_pan2=i
endfor

if((plot_panel ne plot_pan2) or (plot_panel lt 0) or (plot_pan2 lt 0)) then begin
   print,'*** must pick w/i plot area !'
   return
endif else begin
   pl[plid].plot_panel = plot_panel
endelse

conx1 = result1[0]
cony1 = result1[1]
conx2 = result2[0]
cony2 = result2[1]      
result=convert_coord([conx1,conx2],[cony1,cony2],/normal,/to_data)

xmin(*)=result(0,0)
xmax(*)=result(0,1)

if (pl[plid].plot_type ne 'map') then begin
  js=where(distinct_frames(plot_panel) eq frames,count)
  if count gt 0 then begin
    if multi_pt then begin        
       lls=0
       for n=0,n_elements(js)-1 do begin
          if pt_npts(js(n)) gt 0 then lls=[lls,pt_first(js(n))+indgen(pt_npts(js(n)))]
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
;;;              if y_var(i) eq 'pha' then incr=20.
              min_y=min([ys(i,js_good)]) & max_y=max([ys(i,js_good)])
              if y_var(i) eq 'amp' then begin
;;Qi change here for updating max and min of amplitude for SMA 01/12/31
;;    should change when amplitude goes to normal values.
;;                incr=0.2
;;;              if max_y le 0.02 then incr=max_y else incr=0.2
                 incr=(max_y-min_y)/3. 
;;EndOfChange
;;;                if max_y gt 5. then incr=1.
              endif
              uti_range_even,min_y,max_y,incr,min_rnd,max_rnd
;; JMC changed the following line so that amplitudes can be zoomed as well.
              ; if y_var(i) ne 'amp' then ymin(plot_panel,i)=min_rnd
              ymin(plot_panel,i)=min_rnd
              ymax(plot_panel,i)=max_rnd
            endif
          endif
       endfor
    endif
  endif
endif else begin
  ymin(*)=result(1,1)
  ymax(*)=result(1,0)
endelse

if e.debug then print,'zooming panel ',pl[plid].plot_panel
i_first=pl[plid].plot_panel & i_last=pl[plid].plot_panel
s_page.j_first=i_first & s_page.j_last=i_last
pan_xmin(pl[plid].plot_panel)=s_page.saved_par.nor_xmin 
pan_xmax(pl[plid].plot_panel)=s_page.saved_par.nor_xmax 
pan_ymin(pl[plid].plot_panel)=s_page.saved_par.nor_ymin 
pan_ymax(pl[plid].plot_panel)=s_page.saved_par.nor_ymax
pan_dx=nor_dx & pan_dy=nor_dy
row(pl[plid].plot_panel)=0 & col(pl[plid].plot_panel)=0 & save_par=1
pl[plid].plot_zoom=1

s_page.pan_xmin=pan_xmin & s_page.pan_xmax=pan_xmax
s_page.pan_ymin=pan_ymin & s_page.pan_ymax=pan_ymax
s_page.pan_dx=pan_dx & s_page.pan_dy=pan_dy
s_page.xmin=xmin & s_page.xmax=xmax
s_page.ymin=ymin & s_page.ymax=ymax
s_page.row=row & s_page.col=col

j_first=s_page.j_first & j_last=s_page.j_last
pan_xmin=s_page.pan_xmin & pan_xmax=s_page.pan_xmax
pan_ymin=s_page.pan_ymin & pan_ymax=s_page.pan_ymax
pan_dx=s_page.pan_dx & pan_dy=s_page.pan_dy
xmin=s_page.xmin & xmax=s_page.xmax
ymin=s_page.ymin & ymax=s_page.ymax
row=s_page.row & col=s_page.col
iframe=s_page.iframe+s_page.frames_per_page

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

pl[plid].plot_zoom=1

end
