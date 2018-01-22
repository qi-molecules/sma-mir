FUNCTION UTI_CHECKFIT,Y, YFIT, EPS, DEL, SIG, FRACDEV, NGOOD,W,B,nsig=nsig
;+
; NAME:
;	UTI_CHECKFIT
; PURPOSE:
;	Used by UTI_FIT_ROBUST ... to determine the quality of a fit and to
;	return biweights.
; CALLING SEQUENCE:
;	status = UTI_CHECKFIT( Y, YFIT, EPS, DEL, SIG, FRACDEV, NGOOD, W, B
;				NSIG = )
; INPUT:
;	Y     = the data
;	YFIT  = the fit to the data
;	EPS   = the "too small" limit
;	DEL   = the "close enough" for the fractional median abs. deviations
; RETURNS:
;	Integer status. if =1, the fit is considered to have converged
;
; OUTPUTS:
;	SIG   = robust standard deviation analog
;	FRACDEV = the fractional median absolute deviation of the residuals
;	NGOOD   = the number of input point given non-zero weight in the 
;		calculation
;	W     = the bisquare weights of Y
;	B     = residuals scaled by sigma
;
; OPTIONAL INPUT KEYWORD:
;	NSIG = allows changing the bisquare weight limit from default 3.0
;
; REVISION HISTORY:
;	Written, H.T. Freudenreich, HSTX, 1/94
;-

  ISTAT = 0

  IF KEYWORD_SET(NSIG) THEN BFAC=NSIG ELSE BFAC=3.

  DEV = Y-YFIT

  SIG=UTI_SIGMA(DEV,/ZERO)
; If the standard deviation = 0 then we're done:
  IF SIG LT EPS THEN GOTO,DONE

  IF DEL GT 0. THEN BEGIN
   ; If the fraction std. deviation ~ machine precision, we're done:
     Q=WHERE( ABS(YFIT) GT EPS, COUNT )
     IF COUNT LT 3 THEN FRACDEV=0. ELSE FRACDEV = UTI_MED( ABS( DEV(Q)/YFIT(Q) ) )
     IF FRACDEV LT DEL THEN GOTO,DONE
  ENDIF

  ISTAT = 1

; Calculate the (bi)weights:
  B = ABS(DEV)/(BFAC*SIG)
  S = WHERE( B GT 1.0,COUNT )  &  IF COUNT GT 0 THEN B(S) = 1.
  NGOOD = N_ELEMENTS(Y)-COUNT
  
  W=(1.-B^2)
  W=W/TOTAL(W)
DONE:
RETURN, ISTAT
END
