; This program is used as an example in the "Widgets"
; chapter of the _Using IDL_ manual.
;
PRO widgetz_event, ev

;We need to save the value of the seed variable for the random number
;generator between calls to the event-handling routine. We do this
;using a COMMON block.

COMMON widz, seed    

;Retrieve the widget ID of the draw widget and make it the current
;IDL graphics window:

WIDGET_CONTROL, ev.top, GET_UVALUE=drawID
WSET, drawID

;Check the type of event structure returned. If it is a timer event,
;change the color table index to a random number between 0 and 40:

IF (TAG_NAMES(ev, /STRUCTURE_NAME) EQ 'WIDGET_TIMER') THEN BEGIN
   table = FIX(RANDOMU(seed)*41)
;print,'color table number',table
;   LOADCT, table
   result=read_data()
   result=phase_monitor()
   WIDGET_CONTROL, ev.id, TIMER=30.0
ENDIF

;If the event is a droplist event, change the type of plot displayed
;in the draw widget: 

IF (TAG_NAMES(ev, /STRUCTURE_NAME) EQ 'WIDGET_DROPLIST') THEN BEGIN
CASE ev.index OF
0: result=phase_monitor()
1: result=read_data()
2: WIDGET_CONTROL, ev.top, /DESTROY
ENDCASE
ENDIF

END

PRO widgetz

;Create a base widget containing a draw widget and a droplist menu.

select = ['Plot', 'Read Data', 'Quit']
base = WIDGET_BASE(TITLE='Phase and Amplitude',/COLUMN)
draw = WIDGET_DRAW(base, XSIZE=1000, YSIZE=500)
dlist = WIDGET_DROPLIST(base, VALUE=select)

;Realize the widget hierarchy, then retrieve the widget ID of the
;draw widget and store it in the user value of the base widget.
;Finally, set the timer value of the draw widget.

WIDGET_CONTROL, base, /REALIZE
WIDGET_CONTROL, draw, GET_VALUE=drawID
WIDGET_CONTROL, base, SET_UVALUE=drawID
WIDGET_CONTROL, draw, TIMER=0.0

;Set the droplist to display 'Shaded Surface' and place a shaded
;surface in the draw widget:

WIDGET_CONTROL, dlist, SET_DROPLIST_SELECT=0
wset, drawID
result=read_data()
LOADCT,39,/silent
result=phase_monitor()

;Register the widget with the XMANAGER:

XMANAGER, 'widgetz', base

END
