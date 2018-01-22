; *************************************************************************
; FUNCTION
;      plo_print_page
;
; WRITTEN
;      Nov  9, 1998 Written by Kim Drongesen
;      Sep 18, 2000 modified to handle 'map' plot_type
;
; PURPOSE
;      This routine either prints the active page, or saves it to a file. It
;      is called from plo_control when the user clicks the middle mouse button,
;      and then hits the 'c' or 's' key.  It is never called from MIR, which
;      will will have it's own print support on the local machine.
; INPUTS
;      printit
;      plid
;      sindex
; OUTPUT
;      none
; EXAMPLES
;      plo_print_page,printit,plid,sindex
;
; *************************************************************************
;
pro plo_print_page,printit,plid,sindex

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
frames_per_page=s_page.frames_per_page & xs=s_page.xs
nframes=s_page.nframes
nrow=s_page.nrow
ncol=s_page.ncol

;From plo_control
set_plot,'ps'

if printit eq 1 then begin
   pl[plid].plot_device='printer'
   device,filename='cont_plot.ps',/landscape,/color
endif else begin
   pl[plid].plot_device=pl[plid].plot_file
   device,filename='plot_temp.ps',/landscape,/color
endelse

pl[plid].plot_copy=1

j_first=s_page.j_first & j_last=s_page.j_last

if not pl[plid].plot_zoom then begin
   j_first=iframe
   j_last=min([iframe+frames_per_page-1,nframes-1])
   s_page.iframe=iframe & s_page.j_first=j_first & s_page.j_last=j_last
   s_page.frames_per_page=frames_per_page & s_page.nframes=nframes
   s_page.nrow=nrow & s_page.ncol=ncol
   result=plo_page(plid,sindex)

   case pl[plid].plot_type of
       'gai':  begin
      	          s_page.nsub=2
                  result=plo_over_page(plid,sindex)
                  s_page.nsub=3
               end
       'pas':  begin
      		 s_page.nsub=2
       		 result=plo_over_page(plid,sindex)
       	       end
   else:
   endcase
endif

if pl[plid].plot_zoom then begin
  s_page.frames_per_page=1 & s_page.nframes=pl[plid].plot_panel
  s_page.nrow=1 & s_page.ncol=1
  result=plo_page(plid,sindex)
  
  case pl[plid].plot_type of
       'gai':  begin
      	         s_page.nsub=2
                 result=plo_over_page(plid,sindex)
                 s_page.nsub=3
               end
       'pas':  begin
      		 s_page.nsub=2
       		 result=plo_over_page(plid,sindex)
       	       end
   else:
   endcase
endif

;back to plo_control
device,/close
if printit eq 1 then begin
   print,'plot sent to printer'
   result=fil_print('cont_plot.ps',/remove)
endif else begin
   result=fil_append('plot_temp.ps',pl[plid].plot_file)
   print,'plot added to file : ',pl[plid].plot_file
endelse


case pl[plid].plot_type of
      'con': con[sindex]=s_page
      'spe': spe[sindex]=s_page
      'gai': gai[sindex]=s_page
      'pas': pas[sindex]=s_page
      'wlm': wlm[sindex]=s_page
      'map': map[sindex]=s_page
      'var': var[sindex]=s_page
endcase

pl[plid].plot_device='x'
set_plot,pl[plid].plot_device
pl[plid].plot_copy=0
pl[plid].plot_key=''

end

