; uti_boxcar
; Purpose:
;     Boxcar smooth an input array
;
; Inputs:
;     x  : Input array
;     y  : Output array
;     dx_width : Total width of the boxcar, in the same units as "x"
;
; Output:
;     yout: Output array sampled on same interval as x

function uti_boxcar,x,y,dx
   ; X and Y must be specified
     if not keyword_set(x) or not keyword_set(y) then begin
        print,"Error setting X/Y in uti_boxcar"
        return,!MIR_ERR
     endif

   ; Copy y
     yout = y

   ; If DX=0, then just return yout
     if not keyword_set(dx) then return,yout
     if dx eq 0.0 then return,yout

   ; Loop over X
     halfwidth = abs(0.5 * dx)
     for i=0L,n_elements(x)-1L do begin
        xsub = abs(x - x[i])
        j = where(xsub le dx,nj)
        yout[i] = (nj gt 0) ? total(y[j]) / nj : 0.0
     endfor

   ; Done
     return,yout
end

function uti_boxcar2,x,y,dx_width
   ; X and Y must be specified
     if not keyword_set(xorig) or not keyword_set(yorig) then begin
        print,"Error setting X/Y in uti_boxcar"
        return,!MIR_ERR
     endif

   ; If DX_width=0, then just return yout
     if not keyword_set(dx_width) then return,yorig
     if dx_width eq 0.0 then return,yorig

   ; Find min/max X value
     min_x = min([x]) 
     max_x = max([x]) 

   ; Find minimum interval between xpoints. Enforce a minimum interval
   ; of 1e-4 (= 0.36 seconds!) to prevent interpolated arrays from becoming
   ; (computed below_ from becoming too big
     dx_min_allowed = 1e-4
     dx = abs(x[0:n_elements(x)-2]-x[1:n_elements(x)-1])
     j = where(dx gt dx_min_allowed,nj)
     min_dx = (nj gt 0) ? min(dx(j)) : dx_min_allowerd

   ; Interpolate X/Y grid onto finer scale. 
     nx_int = 1+long((max_x-min_x)/min_dx)
     x_int = min_dx*findgen(nx_int)+min_x
     y_int = uti_interp(y,x,x_int)

   ; Set boxcar filter
     nf = long(max([1,dx_width/min_dx]))
     if nf gt (nx_int-1) then nf=nx_int-1
     if (nf mod 2) ne 1 then nf=max([1,nf+1-2*(nf ge nx_int-1)])
     filter=make_array(nf,/float,value=(1./nf))

   ; Boxcar smooth
     y_conv=convol(y_int,filter,/edge_truncate)

   ; Set the edges of the convolved
     i = fix(nf-1/2.)/2
     if i ne 0 then begin
        y_temp1=make_array(i,/double,value=y_conv[i])
        y_temp2=make_array(i,/double,value=y_conv[nx_int-i-1])
        y_temp=[y_temp1,y_conv[i:nx_int-i-1],y_temp2]
     endif else $
        y_temp = y_conv

   ; Get smooth function on input grid
     y_smo = uti_interp(y_temp,x_int,x)

   ; Return smoothed array
     return,y_smo
end
