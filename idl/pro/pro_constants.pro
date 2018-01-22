pro pro_constants
; *************************************************************************
; FUNCTION
;      pro_constants
;
; WRITTEN 
;      February 24, 1998 by JMC
; 
; REVISED
;	June 10, 1998 by Kim Drongesen
;
; PURPOSE
;      Define IDL system variables for MIR
;
; INPUTS 
;      none
;
; OUTPUT
;      none
;
; EXAMPLE
;      pro_constants
; *************************************************************************

; Common blocks
  common global

; Make variables read_only; set READ_ONLY=0 otherwise
  READ_ONLY = 1

; Site specific constants

if (e.campuslogin eq 'ovro' or e.campuslogin eq 'caltech' or e.campuslogin eq 'nick') then begin

  defsysv, '!TEL_NAME','OVRO MMA',READ_ONLY
  defsysv, '!TEL_DIAM',10.4,READ_ONLY
  defsysv, '!IF_FREQ',1.5D,READ_ONLY         ; IF frequency at OVRO    [GHz]
  defsysv, '!NTEL',6,READ_ONLY               ; Number of telescopes
  defsysv, '!TEL_LAT',37.23405556D,READ_ONLY ; Latitude for OVRO array [degrees]
endif else begin
  defsysv, '!TEL_NAM','SAO SMA',READ_ONLY
  defsysv, '!TEL_DIAM',6.0,READ_ONLY
  defsysv, '!IF_FREQ',5.0D,READ_ONLY 
  defsysv, '!NTEL',4,READ_ONLY
  defsysv, '!TEL_LAT',19.82420526391D,READ_ONLY
endelse

; Physical constants
  defsysv, '!TWOPI',2.0D*!DPI,READ_ONLY
  defsysv, '!CVEL', 299792458.0D,READ_ONLY   ; Speed of light          [m/sec]
  
; Misc constants
   defsysv, '!BAD_VALUE',-1001.,READ_ONLY
   defsysv, '!MIN_VALUE',-1000.,READ_ONLY
   defsysv, '!BAD_COMPLEX', complex(-190.999,-982.609), READ_ONLY
end



