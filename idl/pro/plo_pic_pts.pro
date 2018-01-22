function plo_pic_pts,x,y,flag,plid,sindex
;
; Handles picking a data point with mouse.
;
; parameters 
;	     : plid - plot index - index into pl array structure
;	     : sindex - index into con,gai,wlm, etc array
;	     : x - device x location of mouse
;	     : y - device y location of mouse
;	     : flag - identifies whether flagging or unflagging data
;
;
;  returns   : j_select     -- list of j indexes of selected points
;              i_select     -- sub-panel index of each selected point
;              x_select     -- x-data coord of points
;              y_select     -- y-data coord of points 
;              m_button_select -- mouse button for selection(-1,+1) 
; result = 0 (non-interactive) 1 (interactive)
;
; eg. result=plo_pic_pts(x,y,flag,plid,sindex)
;
common global
common plo
common data_set

;Set up the data structure for the correct type of plot
case pl[plid].plot_type of
      'con': s_page=con[sindex]
      'spe': s_page=spe[sindex]
      'gai': s_page=gai[sindex]
      'pas': s_page=pas[sindex]
      'wlm': s_page=wlm[sindex]
      'var': s_page=var[sindex]
endcase

ys=s_page.ys

if not pl[plid].plot_interact then return,0
;
;
; find data point to select or deselect it
;
j_select=-1
i_select=-1
x_select=!BAD_VALUE
y_select=!BAD_VALUE
m_button_select=0
 
 
result = convert_coord(x,y,/device,/to_normal)
pl[plid].mouse_x = result[0]
if e.java then pl[plid].mouse_y = 1-result[1] else pl[plid].mouse_y = result[1]
;this fixes ION bug
 

plot_panel=-1
for i=s_page.j_first,s_page.j_last do begin
   if pl[plid].mouse_x ge s_page.pan_xmin(i) and  $
      pl[plid].mouse_x le s_page.pan_xmax(i) and  $
      pl[plid].mouse_y ge s_page.pan_ymin(i) and  $
      pl[plid].mouse_y le s_page.pan_ymax(i) then plot_panel=i
endfor
   
if plot_panel eq -1 then begin
  print,'*** must pick point w/i plot area !'
  if e.debug then begin
    print,'**** plot panel ',plot_panel,'  active panel ',pl[plid].plot_panel
    print,'mouse x ',pl[plid].mouse_x,'mouse y ',pl[plid].mouse_y
    for i=s_page.j_first,s_page.j_last do begin
       print,'panel ',i,' xmin ', s_page.pan_xmin(i),'panel xmax ', s_page.pan_xmax(i)
       print,'panel ',i,' ymin ', s_page.pan_ymin(i),'panel ymax ', s_page.pan_ymax(i)
    endfor
  endif
  return,-1
endif

js=where(s_page.distinct_frames(plot_panel) eq s_page.frames,count)
if count gt 0 then begin

  ip_scale=plot_panel-s_page.j_first
  if s_page.multi_pt then begin        
    lls=0
    for n=0,n_elements(js)-1 do begin
      if s_page.pt_npts(js(n)) gt 0 then lls=[lls,s_page.pt_first(js(n))+ $
                                             indgen(s_page.pt_npts(js(n)))]
    endfor
    js=lls(1:n_elements(lls)-1)
  endif
  j_min=-1 & i_min=-1
  min_dist=1000.
  for i=0,n_elements(s_page.y_var)-1 do begin
     if s_page.y_var(i) ne 'coh' then begin
        jjs=where(ys[i,js] ne !BAD_VALUE,count)
        if count gt 0 then begin
           js_good=js(jjs)
        endif
        xsnorm=s_page.plot_scale(ip_scale,i,0,1)*s_page.xs[js_good]+ $
               s_page.plot_scale(ip_scale,i,0,0)
        ysnorm=s_page.plot_scale(ip_scale,i,1,1)*reform(ys[i,js_good])+ $
               s_page.plot_scale(ip_scale,i,1,0)
        distances=[(xsnorm-pl[plid].mouse_x)^2 + (ysnorm-pl[plid].mouse_y)^2]
        min_dist_new=min([distances])
        if min_dist_new lt min_dist then begin
           i_min=i
           j_min=where(distances eq min_dist_new) & min_dist=min_dist_new
        endif
     endif
  endfor
  
  j_select=[j_select,js_good[j_min]]
  i_select=[i_select,i_min]
  x_select=[x_select,s_page.xs[js_good[j_min]]]
  y_select=[y_select,reform(ys[i_min,js_good[j_min]])]
  if flag eq -1 then m_button_select=[m_button_select,-1]
  if flag eq 1 then m_button_select=[m_button_select,1]
endif
if e.debug then print,'x select= ',x_select
if e.debug then print,'j select= ',j_select 
if e.debug then print,'i select= ',i_select
print, 'integration number is ',in[where(in.inhid eq sp[psl[j_select[1]]].inhid)].int
n_select=n_elements(j_select)-1
if n_select gt 0 then begin
;; Qi flagging the points in pointers in ps 01/12/31
;;   sp[psl[j_select[1]]+indgen(n_elements(c.band))].wt = m_button_select[1] * abs(sp[psl[j_select[1]]+indgen(n_elements(c.band))].wt)
;   sp[psl[j_select[1]]].wt = m_button_select[1] * abs(sp[psl[j_select[1]]].wt)
;; EndOfChange
   j_select=j_select[1:n_elements(j_select)-1L]
   i_select=i_select[1:n_elements(i_select)-1L]
   x_select=x_select[1:n_elements(x_select)-1L]
   y_select=y_select[1:n_elements(y_select)-1L]
   m_button_select=m_button_select[1:n_elements(m_button_select)-1L]

   ;insert new selections into the grand scheme
   s_page.j_select[s_page.n_select:s_page.n_select+n_select-1L] = j_select
;   s_page.i_select[s_page.n_select:s_page.n_select+n_select-1L] = i_select
   s_page.i_select[s_page.n_select:s_page.n_select+n_select-1L] = plot_panel
   s_page.x_select[s_page.n_select:s_page.n_select+n_select-1L] = x_select
   s_page.y_select[s_page.n_select:s_page.n_select+n_select-1L] = y_select
   s_page.m_button_select[s_page.n_select:s_page.n_select+n_select-1L] = m_button_select

   s_page.n_select=s_page.n_select + n_select

endif

case pl[plid].plot_type of
      'con': con[sindex]=s_page
      'spe': spe[sindex]=s_page
      'gai': gai[sindex]=s_page
      'pas': pas[sindex]=s_page
      'wlm': wlm[sindex]=s_page
      'var': var[sindex]=s_page
endcase

return,1
end 
