function slarvlsrk, r2000, d2000
;  - - - - - - - - - -
;   s l a R v l s r k
;  - - - - - - - - - -
;
;  Velocity component in a given direction due to the Sun's motion
;  with respect to an adopted kinematic Local Standard of Rest.
;
;  (single precision)
;
;  Given:
;     r2000,d2000   float    J2000.0 mean RA,Dec (radians)
;
;  Result:
;     Component of "standard" solar motion in direction R2000,D2000 (km/s)
;
;  Sign convention:
;     The result is +ve when the Sun is receding from the given point on
;     the sky.
;
;  Note:  The Local Standard of Rest used here is one of several
;         "kinematical" LSRs in common use.  A kinematical LSR is the
;         mean standard of rest of specified star catalogues or stellar
;         populations.  The Sun's motion with respect to a kinematical
;         LSR is known as the "standard" solar motion.
;
;         There is another sort of LSR, the "dynamical" LSR, which is a
;         point in the vicinity of the Sun which is in a circular orbit
;         around the Galactic centre.  The Sun's motion with respect to
;         the dynamical LSR is called the "peculiar" solar motion.  To
;         obtain a radial velocity correction with respect to the
;         dynamical LSR use the routine slaRvlsrd.
;
;  Reference:  Delhaye (1965), in "Stars and Stellar Systems", vol 5, p73.
;
;  Called:  slaCs2c, slaVdv
;
;  Last revision:   27 November 1994
;
;  Copyright P.T.Wallace.  All rights reserved.
;
;  Standard solar motion (from Methods of Experimental Physics, ed Meeks,
;  vol 12, part C, sec 6.1.5.2, p281):
;
;  20 km/s towards RA 18h Dec +30d (1900).
;
;  The solar motion is expressed here in the form of a J2000.0
;  equatorial Cartesian vector:
;
;      va(1) = x = -speed*cos(ra)*cos(dec)
;      va(2) = y = -speed*sin(ra)*cos(dec)
;      va(3) = z = -speed*sin(dec)
va = [ -0.29000, 17.31726, -10.00141 ]
slacs2c,r2000,d2000,vb
return,slavdv(va,vb)
end
