function plo_zbuf,java=java,init=init
;
; initialize and flush zbuffer to plot device or java (ie. ion window)
;
; Note : typical sequence is to call this routine
;        first to initiallize (ie. /init) before issuing
;        any plot commands, then to call it after all plot
;        commands to flush the z buffer to your plot device
;
; parameters : java     -- 1 (java) , 0 (idl command line)
;              xsize    -- x size of image in pixels
;              ysize    -- y size of image in pixels 
;              init     -- initialize z buffer
; result = -1 (error), 1 (success)
;
; eg. : result=plo_zbuf(xsize=300,ysize=300,/init) ; to initialize
;       result=plo_zbuf(java=java) ; to flush buffer after plot
common global

;
; in case user has resized, check and update sizes
;
if !d.x_size gt 30 then e.xsize=!d.x_size
if !d.y_size gt 30 then e.ysize=!d.y_size
if keyword_set(init) then begin
  set_plot,'Z'
  device,set_resolution=[0.97*e.xsize,0.97*e.ysize]
  return,1
endif
if not Keyword_set(java) then java=0
image = tvrd()
if not java then begin
;use the following block of code to test out of IONJAVA in IDL
serverDevice = strlowcase(!VERSION.OS_FAMILY)
case serverdevice of
	'windows': set_plot,'WIN'
	'unix': set_plot,'X'
	else: return,-1	;what other kind of server do you have???
endcase
endif else begin
set_plot,'ION'
endelse

erase
tvscl,image
empty	;force graphics buffer to flush


return,1
end
