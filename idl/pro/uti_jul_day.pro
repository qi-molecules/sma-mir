; $Id: uti_jul_day.pro,v 1.1.1.1 2003/03/04 05:35:21 cqi Exp $
;
; Copyright (c) 1988-1998, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.

function uti_jul_day, MONTH, DAY, YEAR, Hour, Minute, Second
;+
; NAME:
;	JULDAY
;
; PURPOSE:
;	Calculate the Julian Day Number for a given month, day, and year.
;	This is the inverse of the library function CALDAT.
;	See also caldat, the inverse of this function.
; CATEGORY:
;	Misc.
;
; CALLING SEQUENCE:
;	Result = JULDAY(Month, Day, Year)
;
; INPUTS:
;	MONTH:	Number of the desired month (1 = January, ..., 12 = December).
;
;	DAY:	Number of day of the month.
;
;	YEAR:	Number of the desired year.
;
;	HOUR:	Number of the hour of the day.
;
;	MINUTE:	Number of the minute of the hour.
;
;	SECOND:	
;
; OPTIONAL INPUT PARAMETERS:
;	Hour, Minute, Second = optional time of day.
;
; OUTPUTS:
;	JULDAY returns the Julian Day Number (which begins at noon) of the 
;	specified calendar date.  If the time of day (Hr, Min, Sec), is 0,
;	the result will be a long integer, otherwise the result is a 
;	double precision floating point number.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	None.
;
; RESTRICTIONS:
;	Accuracy using IEEE double precision numbers is approximately
;	1/10000th of a second.
;
; MODIFICATION HISTORY:
;	Translated from "Numerical Recipies in C", by William H. Press,
;	Brian P. Flannery, Saul A. Teukolsky, and William T. Vetterling.
;	Cambridge University Press, 1988 (second printing).
;
;	AB, September, 1988
;	DMS, April, 1995, Added time of day.
;-
;
ON_ERROR, 2		; Return to caller if errors

MONTHS = ['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG', $
	  'SEP','OCT','NOV','DEC']

; Gregorian Calander was adopted on Oct. 15, 1582
GREG = 15L + 31L * (10L + 12L * 1582L)

; Process the input, if all are missing, use todays date.
NP = n_params()
if NP eq 0 then begin
	DATE = systime()
	L_MONTH = long(where(strupcase(strmid(DATE, 4, 3)) eq MONTHS))
	L_MONTH = L_MONTH[0] + 1L	; Scalarize it...
	L_DAY = long(strmid(DATE, 8, 2))
	L_YEAR = long(strmid(DATE, 20, 4))
endif else if np ge 3 then begin
	L_MONTH = LONG(MONTH)
	L_DAY = LONG(DAY)
	L_YEAR=LONG(YEAR)
	if (L_YEAR eq 0L) then message, 'There is no year zero.'
endif else message, 'Wrong number of parameters.'


if (L_YEAR lt 0L) then L_YEAR = L_YEAR + 1L
if (L_MONTH gt 2L) then begin
	JY = L_YEAR
	JM = L_MONTH + 1L
    endif else begin
	JY = L_YEAR - 1L
	JM = L_MONTH + 13L
    endelse

JUL = 1.d0*(long(365.25d0 * JY) + long(30.6001d0 * JM) + L_DAY + 1720995L)

; Test whether to change to Gregorian Calandar.
if ((L_DAY + 31L * (L_MONTH + 12L * L_YEAR)) ge GREG) then begin
	JA = long(0.01d0 * JY)
	JUL = JUL + 2L - JA + long(0.25d0 * JA)
	endif

if n_elements(Hour) + n_elements(Minute) + n_elements(Second) eq 0 then $
	return, JUL
if n_elements(Hour) eq 0 then Hour = 0L
if n_elements(Minute) eq 0 then Minute = 0L
if n_elements(Second) eq 0 then Second = 0L

return, 1.d0* JUL + (Hour / 24.0d0 - .5d0) + (Minute/1440.0d0) + (Second / 86400.0d0)
end
