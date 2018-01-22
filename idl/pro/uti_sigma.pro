FUNCTION  UTI_SIGMA,Y, ZERO=REF
;
;+
; NAME:
;	UTI_SIGMA  
;
; PURPOSE:
;	Calculate a resistant estimate of the dispersion of a distribution.
;	For an uncontaminated distribution, this is identical to the standard
;	deviation.
;
; CALLING SEQUENCE:
;	result = UTI_SIGMA( Y, [ /ZERO ] )
;
; INPUT: 
;	Y = Vector of quantity for which the dispersion is to be calculated
; OPTIONAL INPUT KEYWORD:
;	ZERO - if set, the dispersion is calculated w.r.t. 0.0 rather than the
;		central value of the vector. If Y is a vector of residuals, this
;		should be set.
;
; OUTPUT:
;	UTI_SIGMA returns the dispersion. In case of failure, returns 
;	value of -1.0
;
; SUBROUTINE CALLS:
;	UTI_MED, which calculates the median
;
; PROCEDURE:
;	Use the median absolute deviation as the initial estimate, then weight 
;	points using Tukey's Biweight. See, for example, "Understanding Robust
;	and Exploratory Data Analysis," by Hoaglin, Mosteller and Tukey, John
;	Wiley & Sons, 1983.
;
; REVSION HISTORY: 
;	H. Freudenreich, STX, 8/90
;
;-

EPS = 1.0E-20
IF KEYWORD_SET(REF) THEN Y0=0. ELSE Y0  = UTI_MED(Y)
; First, the median absolute deviation about the median:
MAD = UTI_MED( ABS(Y-Y0) )/.6745
; If the MAD=0, try the MEAN absolute deviation:
IF MAD LT EPS THEN MAD=AVG( ABS(Y-Y0) )/.80
IF MAD LT EPS THEN BEGIN
   SIGGMA=0.
   RETURN,SIGGMA
ENDIF
; Now the biweighted value:
U   = (Y-Y0)/(6.*MAD)
UU  = U*U
Q   = WHERE(UU LE 1.0, COUNT)
IF COUNT LT 3 THEN BEGIN
   PRINT,'UTI_SIGMA: This distribution is TOO WEIRD! Returning -1'
   SIGGMA = -1.
   RETURN,SIGGMA
ENDIF
NUMERATOR = TOTAL( (Y(Q)-Y0)^2 * (1-UU(Q))^4 )
N     = N_ELEMENTS(Y)
DEN1  = TOTAL( (1.-UU(Q))*(1.-5.*UU(Q)) )
SIGGMA = N*NUMERATOR/(DEN1*(DEN1-1.))
IF SIGGMA GT 0. THEN SIGGMA = SQRT(SIGGMA) ELSE SIGGMA=0.

RETURN,SIGGMA
END
