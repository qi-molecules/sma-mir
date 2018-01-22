PRO widjetz_event, ev

; Save the value of the seed variable for the random number
; generator between calls to the event-handling routine
; using a COMMON block.

COMMON widz, seed , sideband , dataselect , dlist

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
   result=dbi_head_read()
   result=dbi_eng_read()
   result=phase_monitor(sideband,dataselect)
   if sideband eq 'l' then WIDGET_CONTROL, dlist, SET_DROPLIST_SELECT=1
   if sideband eq 'u' then WIDGET_CONTROL, dlist, SET_DROPLIST_SELECT=0
   WIDGET_CONTROL, ev.id, TIMER=30.0
ENDIF

;If the event is a droplist event, change the type of plot displayed
;in the draw widget: 

IF (TAG_NAMES(ev, /STRUCTURE_NAME) EQ 'WIDGET_DROPLIST') THEN BEGIN
CASE ev.index OF
0: begin
     sideband = 'u' 
     result=phase_monitor(sideband,dataselect)
   end
1: begin
     sideband = 'l' 
     result=phase_monitor(sideband,dataselect)
   end
2: begin
     dataselect = 'a'
     result=phase_monitor(sideband,dataselect)
   end
3: begin
     dataselect = 'v'
     result=phase_monitor(sideband,dataselect)
   end
4: WIDGET_CONTROL, ev.top, /DESTROY
ENDCASE
ENDIF

END

PRO widjetz,sb,ds

COMMON widz, seed , sideband , dataselect , dlist

sideband = sb
dataselect = ds

;Create a base widget containing a draw widget and a droplist menu.

select = ['Upper Sideband', 'Lower Sideband', 'All Data', 'Valid Data', 'Quit']
base = WIDGET_BASE(TITLE='Phase and Amplitude',/COLUMN)
draw = WIDGET_DRAW(base, XSIZE=1000, YSIZE=500)
dlist = WIDGET_DROPLIST(base, VALUE=select, TITLE=OPTIONS)

;Realize the widget hierarchy, then retrieve the widget ID of the
;draw widget and store it in the user value of the base widget.
;Finally, set the timer value of the draw widget.

WIDGET_CONTROL, base, /REALIZE
WIDGET_CONTROL, draw, GET_VALUE=drawID
WIDGET_CONTROL, base, SET_UVALUE=drawID
WIDGET_CONTROL, draw, TIMER=0.0

;Set the droplist to display sideband 

if sideband eq 'l' then WIDGET_CONTROL, dlist, SET_DROPLIST_SELECT=1
if sideband eq 'u' then WIDGET_CONTROL, dlist, SET_DROPLIST_SELECT=0
wset, drawID
LOADCT,39,/silent
;LOADCT,38
;result=phase_monitor(sideband,dataselect)

;Register the widget with the XMANAGER:

XMANAGER, 'widjetz', base

END
