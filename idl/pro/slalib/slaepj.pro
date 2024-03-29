function slaEPJ, DATE
;+
;     - - - -
;      E P J
;     - - - -
;
;  Conversion of Modified Julian Date to Julian Epoch (double precision)
;
;  Given:
;     DATE     dp       Modified Julian Date (JD - 2400000.5)
;
;  The result is the Julian Epoch.
;
;  Reference:
;     Lieske,J.H., 1979. Astron.Astrophys.,73,282.
;
;  P.T.Wallace   Starlink   February 1984
;-

      sla_EPJ = 2000D0 + (DATE-51544.5D0)/365.25D0

      return, double( sla_EPJ )

end
