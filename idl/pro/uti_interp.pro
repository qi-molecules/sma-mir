function uti_interp, v, x, u
;
; Linearly interpolate vectors with a regular or irregular grid.
; (modified and faster version of the idl distribution routine interpol)
; Returns a floating-point vector of n points determined by linearly 
; interpolating the input vector. If the input vector is double or 
; complex, the result is double or complex.
; procedure:
;	result(i) = v(x) + (x - fix(x)) * (v(x+1) - v(x))
;
;	where 	x = i*(m-1)/(n-1) for regular grids.
;		m = # of elements in v, i=0 to n-1.
;
;	for irregular grids, x = u(i).
;		m = number of points of input vector.
;
; parameters : v:	the input vector can be any type except string.
;       for regular grids:
;	n:	the number of points in the result when both input and
;		output grids are regular.  the output grid absicissa values
;		equal float(i)/n_elements(v), for i = 0, n-1.
;	irregular grids:
;	x:	the absicissae values for v.  this vecotr must have same # of
;		elements as v.  the values must be monotonically ascending 
;		or descending.
;	u:	the absicissae values for the result.  the result will have 
;		the same number of elements as u.  u does not need to be 
;		monotonic.
;	
; eg. :	result = interpol(v, n) 	;for regular grids.
; eg. : result = interpol(v, x, u)	;for irregular grids.
;
;
;
on_error,2                      ;return to caller if an error occurs
m = n_elements(v)               ;# of input pnts
if m le 1 then begin
  if m lt 1 then begin
    print,m,' points to interpolate' & return,0
  endif
  if n_params(0) eq 2 then r=v[0] else r=replicate(v[0],n_elements(u))
  return,r
endif

if n_params(0) eq 2 then begin	;regular?
    r = findgen(x)*(m-1)/(x-1>1) ;grid points in v
    rl = long(r)		;cvt to integer
    s = size(v)
    if s(s(0)+1) eq 1 then dif = v[1:*]-fix(v)  $ ;v[i+1]-v[i], signed for bytes
    else dif = v[1:*]-v    ;other types are already signed
    return, v[rl] + (r-rl)*dif[rl] ;interpolate
endif
;
if n_elements(x) ne m then $ 
  stop,'interpol - v and x must have same # of elements'
n= n_elements(u)                ;# of output points
m2=m-2                          ;last subs in v and x
x_s=[x(1l:m-1l),x(m-1l)+1.]     ; shifted version of x
js=make_array(n_elements(u),/long,value=-1l)
if x[1] - x[0] ge 0. then s1 = 1. else s1=-1. ;incr or decr x
;
m2=m-2            
ix = 0l                         ;current point
for i=0l,n-1 do begin           ;point loop
    d = s1*(u[i]-x[ix]) 	;difference
    if d eq 0. then js[i]=ix else begin  ;at point
      if d gt 0. then while (s1*(u[i]-x_s[ix]) gt 0.) and $
                  (ix lt m2) do ix=ix+1 else begin
            while (s1*(u[i]-x[ix]) lt 0.) and (ix gt 0) do ix=ix-1
          endelse
      js(i)=ix
    endelse
endfor
js1=js+1l
r=(u-x(js))*((v(js1)-v(js))/(x_s(js)-x(js)))+v(js)
return,r

end




