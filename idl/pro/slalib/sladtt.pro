function sladtt, utc
;  - - - - - - -
;   s l a D t t
;  - - - - - - -
;
;  Increment to be applied to Coordinated Universal Time UTC to give
;  Terrestrial Time TT (formerly Ephemeris Time ET).
;
;  (double precision)
;
;  Given:
;     utc    double    UTC date as a modified JD (JD-2400000.5)
;
;  Result:  TT-UTC in seconds
;
;  Notes:
;
;  1  The UTC is specified to be a date rather than a time to indicate
;     that care needs to be taken not to specify an instant which lies
;     within a leap second.  Though in most cases UTC can include the
;     fractional part, correct behaviour on the day of a leap second
;     can only be guaranteed up to the end of the second 23:59:59.
;
;  2  Pre 1972 January 1 a fixed value of 10 + ET-TAI is returned.
;
;  3  See also the routine sla_DT, which roughly estimates ET-UT for
;     historical epochs.
;
;  Called:  slaDat
;
;  Last revision:   6 December 1994
;
;  Copyright P.T.Wallace.  All rights reserved.
return, 32.184 + slaDat ( utc )
end
