;
; Edited from idl/lib/gauss2dfit.pro to suit for our purpose in clean beam model (syl)
; 1) no dc and scale parameter in the beam function
; 2) selectable area of dirty beam for fitting (output will be the full beam size)
;
PRO	map_beam_func, X, A, F, PDER
; NAME:
;	map_beam_func
; PURPOSE:
;	Evaluate function for map_beamfit.
; CALLING SEQUENCE:
;	map_beam_func,X,A,F,PDER
; INPUTS:
;	X = values of independent variables, encoded as: [nx, ny, x, y]
;	A = parameters of equation described below.
; OUTPUTS:
;	F = value of function at each X(i,j), Y(i,j).
;	Function is:
;		F(x,y) = EXP(-U/2)
;		where: U= (yp/A0)^2 + (xp/A1)^2
;
;	        A has 5 elements a rotation of the ellipse is present and:
;	 	 xp = (x-A2) * cos(A4) - (y-A3) * sin(A4)
;		 yp = (x-A2) * sin(A4) + (y-A3) * cos(A4)
;
; PROCEDURE:
;	Evaluate the clean beam function and partial derivatives.
;
; MODIFICATION HISTORY:
;     syl  sep 18, 2000
;

nx = long(x[0])		;Retrieve X and Y vectors
ny = long(x[1])

tilt = n_elements(a) eq 5	;TRUE if angle present.
if tilt then begin		;Rotate?
    xp = (x[2:nx+1]-a[2]) # replicate(1.0, ny)	;Expand X values
    yp = replicate(1.0, nx) # (x[nx+2:*]-a[3])	;expand Y values
    s = sin(a[4]) & c = cos(a[4])
    t =  xp * (c/a[0]) - yp * (s/a[0])
    yp = xp * (s/a[1]) + yp * (c/a[1])
    xp = temporary(t)
endif else begin
    print,'wrong number of beam parameters'
    return
endelse

n = nx * ny
u = reform(exp(-0.5 * (xp^2 + yp^2)), n)	;Exp() term, Make it 1D
F = u

PDER = FLTARR(n, n_elements(a))	;make partial array.
pder[*,0] = u * xp^2 / a[0]
pder[*,1] = u * yp^2 / a[1]
pder[*,2] = u * (c/a[0] * xp + s/a[1] * yp)
pder[*,3] = u * (-s/a[0] * xp + c/a[1] * yp)
pder[*,4] = -u * xp*yp*(a[0]/a[1]-a[1]/a[0])
END



Function map_beamfit, beam, g, a, FITAREA = fitarea
; NAME:
;	map_beamfit
;
; PURPOSE:
; 	To obtain the clean beam parameters from the dirty beam pattern 
;       by fitting a 2 dimensional elliptical gaussian equation to
;       rectilinearly gridded data.
;		beam = F(x,y) where:
; 		F(x,y) = EXP(-U/2)
;	   And the elliptical function is:
;		U= (x'/a)^2 + (y'/b)^2
;	The parameters of the ellipse U are:
;	   Axis lengths are 2*a and 2*b, in the unrotated X and Y axes,
;		respectively.
;	   Center is at (h,k).
;	   Rotation of T radians from the X axis, in the CLOCKWISE direction.
;	   The rotated coordinate system is defined as:
;		x' = (x-h) * cos(T) - (y-k) * sin(T)  <rotate by T about (h,k)>
;		y' = (x-h) * sin(T) + (y-k) * cos(T)
;
;	The coefficients of the function, are returned in a seven
;	element vector:
;	a(0) = a = width of gaussian in X direction.
;	a(1) = b = width of gaussian in Y direction.
;	a(2) = h = center X location.
;	a(3) = k = center Y location.
;	a(4) = T = Theta the rotation of the ellipse from the X axis
;		in radians, counterclockwise.
;
; CALLING SEQUENCE:
;	Result = map_beamfit(beam, g, a)
;
; INPUTS:
;	beam =  dirty beam pattern; 
;               dependent variable in a 2D array dimensioned (Nx, Ny).
;               Gridding must be rectilinear.
;       g = initial guess of the FWHM of the beam in pixels (single value)
;
; Optional Keyword Parameters:
;       FITAREA  = half length in pixels of the central square area of the 
;                  dirty beam to be used in fitting
;
; OUTPUTS:
;       The fitted beam is returned in full input beam size.
; OUTPUT PARAMETERS:
;	A:	The coefficients of the fit.
;		a[0] and a[1] are FWHM of the clean map in pixels.
;               a[2] and a[3] are x and y coordinates of the beam peak in pixels.
;               a[4] is the the rotation angle of the ellipse from the X axis
;                    in degrees, counterclockwise.
;
; PROCEDURE:
;	The peak/valley is found by first smoothing beam and then finding the
;	maximum or minimum respectively.  Then GAUSSFIT is applied to the row
;	and column running through the peak/valley to estimate the parameters
;	of the Gaussian in X and Y.  Finally, CURVEFIT is used to fit the 2D
;	Gaussian to the data.
;
; MODIFICATION HISTORY:
;       syl   sep 18, 2000
;
;
s = size(beam)
if s[0] ne 2 then $
	message, 'Beam must have two dimensions'
n = n_elements(beam)
nx0 = s[1]
ny0 = s[2]
np = n_params()
;
; default to fit the inner quarter of the input beam
;
 if not keyword_set(fitarea) then fitarea = nx0/4
; print,'fitarea =',fitarea

 z = beam ( (nx0/2 - fitarea): (nx0/2 + fitarea -1 ), (ny0/2 - fitarea): (ny0/2 + fitarea -1))
 n = n_elements(z)
 nx=2*fitarea
 ny=2*fitarea
 x = findgen(nx)
 y = findgen(ny)

 q = MAX(SMOOTH(z,3), i)	;Dirty peak finder

 ix = i mod nx
 iy = i / nx
 x0 = x[ix]
 y0 = y[iy]
 g = g / (2 * SQRT(2*ALOG(2)))

 est = [1, iy, g]
 yfit = gaussfit(y, z[ix,*], ay, ESTIMATE = est, NTERMS=3)

 est = [1, ix, g]
 xfit = gaussfit(x, z[*,iy], ax, ESTIMATE = est, NTERMS=3)
;
; First guess
;
 a = [ax[2], ay[2], ax[1], ay[1], 0.0]	;Widths, centers, and angle
; print,'1st guess for curvefit:',string(a,format='(8f10.4)')
;
 result = curvefit([nx, ny, x, y], reform(z, n, /OVERWRITE), $
		replicate(1.,n), a, itmax=50, $
		function_name = "map_beam_func", tol=0.001)
a[4] = a[4] mod !pi		;Reduce angle argument
;
;  Forming clean beam
;
 X = FINDGEN(nx0) # REPLICATE(1.0, ny0)
 Y = REPLICATE(1, nx0) # FINDGEN(ny0)
 xp = x - a[2] - nx0/2 + fitarea
 yp = y - a[3] - ny0/2 + fitarea
 s = sin(a[4]) & c = cos(a[4])
 t =  xp * (c/a[0]) - yp * (s/a[0])
 yp = xp * (s/a[1]) + yp * (c/a[1])
 xp = temporary(t)

 u = exp(-0.5 * (xp^2 + yp^2))
;
; Converting output beamwidth to FWHM in pixel units and rotation angle to degrees
;
 a[0] = a[0] * 2 * SQRT(2*ALOG(2))
 a[1] = a[1] * 2 * SQRT(2*ALOG(2))
 a[4] = a[4] * 360/(2*!pi) 
;
; Return clean beam matrix
;
 return, u

end