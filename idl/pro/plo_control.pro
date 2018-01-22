; *************************************************************************
; FUNCTION
;      plo_control.pro
;
; WRITTEN
;      October 21, 1998  Kim Drongesen
;      Sep 18, 2000      modified to handle 'map' plot_type
;                        remove redundant parameters (syl)
;
; PURPOSE
;      This routine is the event loop for the plot - it takes input from
;      the user - and then calls the correct routine based on the input.
;      It is called from the plot routines to do further plotting based on
;      user input.  It is never called from MIR.  It is only called from the
;      IDL only environment to emulate what MIR does.
;
; INPUTS
;      plid - plot index - index into pl array structure
;      sindex - index into con,gai,wlm,map,etc... array
;      passed in structure s_page
;      pan_xmin,pan_xmax  -- min and max normal x-coord of panels
;      pan_ymin,pan_ymax  -- min and max normal y-coord of panels
;      row,col            -- row and col numbers of panels
;      y_var              -- y labels for subpanels
;      m_options          -- middle mouse options, eg. 'csfpne'
;                            default = 'cspne'
;                            'csfpne'=> copy,file,flag,prev,next,exit
;
; OUTPUT
;      result = 0 (non-interactive) 1 (interactive)
;
; EXAMPLES
;       result=plo_control(plid,mindex)
;
; REVISIONS
; 8/2/04 diono, new option to accept interactinve passband fitting
;
;
; *************************************************************************
;
function plo_control, plid, sindex

common global
common plo

; If the plot device is null, then return
if not e.java and strlowcase(pl[plid].plot_device) eq "null" then return,1


case pl[plid].plot_type of
      'con': m_options=con[sindex].m_options 
      'spe': m_options=spe[sindex].m_options 
      'gai': m_options=gai[sindex].m_options
      'pas': m_options=pas[sindex].m_options
      'wlm': m_options=wlm[sindex].m_options
      'map': m_options=map[sindex].m_options
      'var': m_options=var[sindex].m_options
endcase

;
; While the user has input - keep reading the mouse
;
if m_options eq '' then m_options='cspfne'
pl[plid].plot_done = 0
pl[plid].plot_zoom = 0

verbose = 1
if (pl[plid].plot_device eq 'ps') then verbose = 0

while pl[plid].plot_done ne 1 do begin
  if (verbose) then begin
;     print,'      squeak !'
     if pl[plid].plot_zoom eq 1 then begin
        print,'Click the left button to pick left edge to zoom on'
        print,'Click the right button to unzoom the current panel'
     endif else begin
        print,'Click left button to zoom the panel'
     endelse
     print,'Click the middle button for more options'
     cursor,x,y,3,/device
     case !mouse.button of
         1: pl[plid].mouse_button='l' 
         2: pl[plid].mouse_button='m' 
         4: pl[plid].mouse_button='r'
         else: pl[plid].mouse_button='m'
     endcase
  endif else $
     pl[plid].mouse_button = 'm'
  
  ; If we get a click in a panel that has not been zoomed - zoom it
  if ((pl[plid].mouse_button eq 'l') and (pl[plid].plot_zoom eq 0)) then begin
      plo_zoom_panel,x,y,plid,sindex
  endif else begin
     ; If a panel is zoomed - the left button signifies a range to zoom on
     if ((pl[plid].mouse_button eq 'l') and (pl[plid].plot_zoom eq 1)) then begin
        print,'Click left button to pick right edge to zoom on - right to abort'
        cursor,x2,y2,3,/device
        plo_zoom_range,x,y,x2,y2,plid,sindex
     endif
  endelse

  
  ; If the panel is zoomed - unzoom it
  if (pl[plid].mouse_button eq 'r') then begin
     if(pl[plid].plot_zoom eq 1) then begin
        plo_unzoom_panel,plid,sindex
     endif else begin
        plo_new_panel,0,plid,sindex
     endelse
  endif
 
  
  if(pl[plid].mouse_button eq 'm') then begin
      if (verbose) then begin
        print,'*** keyboard options :'
        if strpos(m_options,'c') ne -1 then $
           print,'***  c => print copy of current plot'  
        if strpos(m_options,'s') ne -1 then $
          print,'***  s => save to plot file 
        if strpos(m_options,'p') ne -1 then $
          print,'***  p => previous plot
        if strpos(m_options,'n') ne -1 then $
          print,'***  n => next plot'
        if strpos(m_options,'f') ne -1 then $
          print,'***  f => flag points'
        if strpos(m_options,'w') ne -1 then $
          print,'***  w => wrap the phase with +/- 2pi'
        if (strpos(m_options,'z') ne -1 and pl[plid].plot_zoom eq 1) then $
          print,'***  z => plot spectrum of current position'
        if strpos(m_options,'r') ne -1 then $
          print,'***  r => refit'
        if strpos(m_options,'e') ne -1 then $
          print,'***  e => exit all plots'

      endif
      m_options=strtrim(m_options,2L) & str=''
      for i=0,strlen(m_options)-1 do str=str+','+strmid(m_options,i,1)
      str='enter('+strmid(str,1,strlen(str))+'): '
      if (verbose) then $
         key = get_kbrd(1) $
      else $
         key = 'n'
      if strpos(m_options,key) eq -1 then key = 'n'
      if (verbose) then print," "
      pl[plid].plot_key=strmid(strtrim(key,2),0,1)
      case pl[plid].plot_key of
         'c' : begin
         	 plo_print_page,1,plid,sindex
               end
         's' : begin
            	 plo_print_page,0,plid,sindex
               end
         'p' : begin
                 plo_new_panel,1,plid,sindex
               end
         'f' : begin
         	 plo_flag_pts,plid,sindex
                 done = 0
                 while done eq 0 do begin
                    print,'*** use left mouse button to select data point'
                    print,'*** middle to deselect, right to quit'
                    cursor,x,y,3,/device
                    if e.debug then print,x,y,!mouse.button
                    case !mouse.button of
                      1: pl[plid].mouse_button='l' 
                      2: pl[plid].mouse_button='m' 
                      4: pl[plid].mouse_button='r'
                    endcase
     
                   if pl[plid].mouse_button eq 'r' then begin
                      done = 1
                   endif else begin
                      if pl[plid].mouse_button eq 'm' then begin
                         result = plo_pic_pts(x,y,1,plid,sindex)
                      endif else begin
                         result = plo_pic_pts(x,y,-1,plid,sindex)
                      endelse
                   endelse
                 endwhile
         	 plo_finish_flagging,plid,sindex
                 if pl[plid].plot_type eq 'gai' then begin
                    pl[plid].plot_zoom=0
                    pl[plid].plot_done=1
                    pl[plid].plot_interact=2
                 endif
              end
         'w' : begin
                 plo_flag_pts,plid,sindex
                 done = 0
                 while done eq 0 do begin
                    print,'*** use left mouse button to select data point'
                    print,'***                and subtract 2pi in phase'
                    print,'*** middle to add 2pi in phase, right to quit'
                    cursor,x,y,3,/device
                    if e.debug then print,x,y,!mouse.button
                    case !mouse.button of
                      1: pl[plid].mouse_button='l' 
                      2: pl[plid].mouse_button='m' 
                      4: pl[plid].mouse_button='r'
                    endcase
     
                   if pl[plid].mouse_button eq 'r' then begin
                      done = 1
                   endif else begin
                      if pl[plid].mouse_button eq 'm' then begin
                         result = plo_pic_pts(x,y,1,plid,sindex)
                      endif else begin
                         result = plo_pic_pts(x,y,-1,plid,sindex)
                      endelse
                   endelse
                 endwhile
         	 plo_finish_wrapping,plid,sindex
                 if pl[plid].plot_type eq 'gai' then begin
                    pl[plid].plot_zoom=0
                    pl[plid].plot_done=1
                    pl[plid].plot_interact=2
                 endif                    
              end
         'n' : begin
                 plo_new_panel,0,plid,sindex
               end
         'z' : begin
                 map_spe,x1,y1,plid,sindex
               end
         'r' : begin
                 pl[plid].plot_zoom=0
                 pl[plid].plot_done=1
                 pl[plid].plot_interact=2
               end
         'e' : begin
                 pl[plid].plot_zoom=0
                 pl[plid].plot_done=1
                 pl[plid].plot_interact=0
               end
        else: begin    ; do next panel by default 
                 plo_new_panel,0,plid,sindex
        endelse
      endcase
      
  endif
  
  if s_page.j_first gt s_page.j_last then begin
     pl[plid].plot_zoom=0
     pl[plid].plot_done=1
  endif
     
endwhile
  
return,pl[plid].plot_interact
end 
