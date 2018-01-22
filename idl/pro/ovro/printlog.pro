;+
; NAME:
;   PRINTLOG
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Captures transcript of console output
;
; CALLING SEQUENCE:
;   PRINTLOG, d1, d2, ..., FORMAT=, LOG=LOG, /ONLYLOG, UNIT=UNIT
;
; DESCRIPTION: 
;
;   The PRINTLOG procedure provides the ability to print an arbitrary
;   expression to the console or an open file UNIT, and also to
;   capture the text in a "log" or archive.  This archive can be used
;   as a verbatim record of console output, which is especially useful
;   when transactional history records must be maintained.
;
;   The log itself is stored as an array of strings which is passed
;   via the LOG keyword.  PRINTLOG simply adds the current output to
;   the existing array and returns.  When the transaction is complete,
;   the resulting array may be saved or printed as appropriate.  For
;   example, the following set of commands will accumulate a log which
;   can be saved later:
;
;     IDL> x = 0 & y = 1 & u = -17. & v = 12.    ;;; CREATE A LOG
;     IDL> PRINTLOG, X, Y, LOG=LOG
;            0       1
;     IDL> PRINTLOG, U, V, LOG=LOG
;          -17.0000      12.0000
;     IDL> PRINTLOG, 'Computation done.', LOG=LOG
;     Computation done.
;
;     IDL> print, log, format='(A)'              ;;; PRINT THE LOG
;            0       1
;          -17.0000      12.0000
;     Computation done.
;     
;
; INPUTS:
;
;   d1, d2, ... - the variables or expressions to be printed, as in
;                 the PRINT or PRINTF commands.  A maximum of twenty
;                 parameters are allowed.
;
; KEYWORDS:
;
;   LOG - input/output keyword, containing the accumulated transaction
;         log.  Upon input, LOG should be an array of strings
;         containing previously accumulated log.  Upon return, LOG
;         will have any new output appended.  If, upon input, LOG is
;         undefined, or contains a single element (-1L or ''), then
;         LOG will be initialized.
;
;   FORMAT - a standard format statement, as used by STRING, PRINT or
;            PRINTF.
;            Default: default output formatting is used.
;
;   UNIT - a file unit to be used for output.  If UNIT is undefined or
;          0, then output is made to the console.
;          Default: undefined (console output).
;
;   ONLYLOG - if set, then output will not be made to the screen, but
;             it will still be archived to LOG.  This may useful to
;             record archane but important dianostic information that
;             normally would not appear to the user.
;
; EXAMPLE:
;   See above.
;
; SEE ALSO:
;   PRINT, PRINTF, STRING
;   STATUSLINE - To print temporary status messages to console
;
; MODIFICATION HISTORY:
;   Written, CM, June 1999
;   Documented, CM, 25 Feb 2000
;   Added STATUSLINE to "SEE ALSO," CM, 22 Jun 2000
;
; TODO:
;   Have a way to internally store the log, rather than the LOG
;   keyword.
;
;  $Id: printlog.pro,v 1.1.1.1 2003/03/04 05:35:39 cqi Exp $
;
;-
; Copyright (C) 2000, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-
pro printlog,  d1,  d2,  d3,  d4,  d5,  d6,  d7,  d8,  d9, d10, $
              d11, d12, d13, d14, d15, d16, d17, d18, d19, d20, $
              format=format, log=log, onlylog=logonly, unit=unit, noline=noline

  on_error, 2
  np = n_params()
  if np GT 20 then $
    message, 'ERROR: number of parameters to PRINTLOG cannot exceed 20'
  cmd = string(lindgen(np)+1, $
               format='("str = string(",10("D",I0,:,","))')
  if n_elements(format) GT 0 then begin
     cmd = cmd + ",format=format(0))"
     print_eol = (strmatch(format(0),'*,$)*') eq 1 or $
                  strmatch(format(0),'*($,*') eq 1) ? 0 : 1
  endif else begin
     print_eol = 1
     cmd = cmd + ")"
  endelse

  str = ''
  result = execute(cmd)
  if result NE 1 then return
  if n_elements(unit) EQ 0 then unit = 0
  if keyword_set(print_eol) then begin
     if unit ne 0 then printf, unit, str 
     if NOT keyword_set(logonly) then print, str
  endif else begin
     if unit ne 0 then printf,unit,format='(%"%s",$)',str
     if NOT keyword_set(logonly) then print, format='(%"%s",$)',str
  endelse

  first = 0
  if n_elements(log) EQ 0 then first = 1
  sz = size(log)
  if n_elements(log) EQ 1 then if sz(sz(0)+1) NE 7 then $
    if long(log(0)) EQ -1 then first = 1
  if n_elements(log) EQ 1 then if sz(sz(0)+1) EQ 7 then $
    if log(0) EQ '' then first = 1
  if first then log = [str] $
  else log = [log, str]

  return
end
