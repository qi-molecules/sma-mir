function slarverot, phi, ra, da, st
;  - - - - - - - - - -
;   s l a R v e r o t
;  - - - - - - - - - -
;
;  Velocity component in a given direction due to Earth rotation.
;
;  (single precision)
;
;  Given:
;     phi     float    latitude of observing station (geodetic)
;     ra,da   float    apparent RA,Dec
;     st      float    local apparent sidereal time
;
;     phi, ra, dec and st are all in radians.
;
;  Result:
;     Component of Earth rotation in direction ra,da (km/s)
;
;  Sign convention:
;     The result is +ve when the observer is receding from the
;     given point on the sky.
;
;  Accuracy:
;     The simple algorithm used assumes a spherical Earth, of
;     a radius chosen to give results accurate to about 0.0005 km/s
;     for observing stations at typical latitudes and heights.  For
;     applications requiring greater precision, use the routine
;     slaPvobs.
;
;  Last revision:   20 July 1994
;
;  Copyright P.T.Wallace.  All rights reserved.
ESPEED=0.4655 
;Nominal mean sidereal speed of Earth equator
;                          in km/s (the actual value is about 0.4651)
return, ESPEED * cos(double(phi)) * sin(double(st-ra)) * cos(double(da))

end

