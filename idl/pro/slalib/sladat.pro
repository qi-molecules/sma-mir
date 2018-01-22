function sladat, utc
;  - - - - - - -
;   s l a D a t
;  - - - - - - -
;
;  Increment to be applied to Coordinated Universal Time UTC to give
;  International Atomic Time TAI.
;
;  (double precision)
;
;  Given:
;     utc      double      UTC date as a modified JD (JD-2400000.5)
;
;  Result:  TAI-UTC in seconds
;
;  Notes:
;
;  1  The UTC is specified to be a date rather than a time to indicate
;     that care needs to be taken not to specify an instant which lies
;     within a leap second.  Though in most cases UTC can include the
;     fractional part, correct behaviour on the day of a leap second
;     can only be guaranteed up to the end of the second 23:59:59.
;
;  2  Pre 1972 January 1 a fixed value of 10 sec is returned.
;
;     :-----------------------------------------:
;     :                                         :
;     :                IMPORTANT                :
;     :                                         :
;     :  This routine must be updated on each   :
;     :     occasion that a leap second is      :
;     :                announced                :
;     :                                         :
;     :  Latest leap second:  2006 January 1    :
;     :                                         :
;     :-----------------------------------------:
;
;  Last revision:   14 November 1995
;
;  Copyright P.T.Wallace.  All rights reserved.
; Increment to be added to UTC to give TAI 
; prior to the first UTC entry in leap_utc
npre=10

;Declare a table of the UTCs at which leap seconds were announced.
;Append the UTC of each new leap second to this table as they are announced.
;One second is added for each entry in the table.
;ftp://maia.usno.navy.mil/ser7/tai-utc.dat

leap_utc=[41499.0d, 41683.0d, 42048.0d,42413.0d,42778.0d,43144.0d,43509.0d,43874.0d,44239.0d,44786.0d,45151.0d,46247.0d,47161.0d,47892.0d,48257.0d,48804.0d,49169.0d,49634.0d,50084.0d,50630.5d,51179.5d,51179.5d,53736.5d,54832.5d]

;Record the number of entries in the table 
num_leap=n_elements(leap_utc)

;Find the date of the last leap second that preceded the requested UTC
ii=0
;while (i lt num_leap) AND (utc gt leap_utc[i]) do i=i+1
for i=0, num_leap-1 do if (utc gt leap_utc[i]) then ii=ii+1

;Return TAI-UTC for the specified date
return, double(npre+ii)

end

