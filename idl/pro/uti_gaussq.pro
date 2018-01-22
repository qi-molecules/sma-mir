function  uti_gaussq,uvdis,radius,freq,vis
;
;   this routine calculates visibilities for a uniform disk source
;   and a gaussian telescope beam using numerical integration by
;   Gaussian quadrature
;
;   units : radius  (")
;           uvdis   (klambda)
;
    ; Set constants
      NWEIGHTS = 10
      WEIGHT   = [0.29552D,0.29552D,0.26926D,0.26926D,0.21908D,0.21908D,$
                  0.14945D,0.14945D,0.06667D,0.06667D]
      ABSCISSA = [0.14887D,-0.14887D,0.43339D,-0.43339D,0.67940D,$
                  -0.67940D,0.86506D,-0.86506D,0.97390D,-0.97390D]

    ; Initialize visibilities
      vis = replicate(1.0,n_elements(uvdis))

    ; Find where radius is non-zero 
      j = where(radius gt 0.0,nj)
      if (nj eq 0) then return,1

    ; Compute radius in radius; assumes a hhpbw of 32" at 115 Ghz
      hphw = (32./206265.) * (115./freq[j])
      conexp=(0.693147) / hphw^2
      k= 2. * !DPI * uvdis[j] * 1.e3
      radiusrad = radius[j] * 4.84814e-06

    ; Initialize
      uplim=radiusrad
      lowlim=0.
      con1=(uplim-lowlim)/2.
      con2=(uplim+lowlim)/2.
      xint=0.

    ; Loop over weights
      xint = dblarr(nj)
      for i = 0, NWEIGHTS-1 do begin
         y=con1*ABSCISSA[i]+con2
         expy=exp(-conexp*y*y)
         yprime=k*y
         funcx=WEIGHT[i]*y*expy*beselj(yprime,0)
         xint = xint + funcx
      endfor

      xint=con1*xint
      vis[j] = (2./(radiusrad)^2)*xint
end
