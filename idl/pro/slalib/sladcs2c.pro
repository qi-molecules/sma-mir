;+
;*  - - - - - - - -
;*   s l a D c s 2 c
;*  - - - - - - - -
;*
;*  Spherical coordinates to direction cosines.
;*
;*  (double precision)
;*
;*  Given:
;*     a,b      double     spherical coordinates in radians
;*                        (RA,Dec), (long,lat) etc
;*
;*  Returned:
;*     v        double[3]  x,y,z unit vector
;*
;*  The spherical coordinates are longitude (+ve anticlockwise
;*  looking from the +ve latitude pole) and latitude.  The
;*  Cartesian coordinates are right handed, with the x axis
;*  at zero longitude and latitude, and the z axis at the
;*  +ve latitude pole.
;*
;*  P.T.Wallace   Starlink   31 October 1993
;-
pro slaDcs2c, a, b, v

   n    = N_ELEMENTS( a )
   cosb = double( cos( b ) )
   v    = dblarr(3,n)
   v(0,*) = double( cos ( a ) * cosb )
   v(1,*) = double( sin ( a ) * cosb )
   v(2,*) = double( sin ( b ) )

end

