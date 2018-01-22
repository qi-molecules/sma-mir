; *************************************************************************
; PROCEDURE
;      wlm_open 
;
; WRITTEN
;      November 28, 1999 by JMC
;
; PURPOSE
;      Opens postscript file
;
; INPUTS
;      outfile : Name of output postscript file
;      style   : Type of plot to open
;                Default: 'sq',  options: 'l', 'p', or 'sq'
;
; OUTPUT
;      None
;
; EXAMPLES
;      wlm_open,"file.ps"
;
; *************************************************************************
pro wlm_open,outfile,style=style
  ; Common blocks
    common plo

  ; Set output names
    set_plot,'PS'
    pl.plot_device = 'ps' 

  ; Set style parameter
    if keyword_set(style) then $
      pstyle = style $
    else $
      pstyle = 'sq'

  ; Set device 
    case pstyle of
       'l'  : device,/landscape,/inches,xsize=9.0,ysize=6.5,font_size=10,/bold
       'p'  : begin
                device,/portrait,/inches,xsize=6.5,ysize=9.0,font_size=10
                device,yoffset=2.,xoffset=2.
               end
      'sq'  : begin
                device,/portrait,/inches,xsize=7.0,ysize=7.0,font_size=10
                device,yoffset=6.0,xoffset=2.0
                device,/bold
                !P.charthick = 2
              end
      else  : begin
                print,' PSTYLE ERROR IN WLM_OPEN: ''/p'', ''/l'', or ''/sq'' '
                stop
              end
      endcase

   ; Open file name
     device,filename=strcompress(outfile,/remove_all)
end
