function  uti_bmin_calc,shadow,diameter
;
; calculates the minimum allowed projected baseline to 
; give the specified % shadowing
;
; paramters : shadow -- % shadowing (0= no shadow)
;             diameter -- dish diameter in meters
;
; eg bmin=uti_bmin_calc(shadow,10.4)
;
  common fits,str_buf,unit

  ; If shadow = 0, then the minimum baseline is the telescope diameter
    if not keyword_set(shadow) then return,diameter
    if shadow le 0.0 then return,diameter

  ;
  ; Solve the equation (theta - sin(theta))/pi = shadow
  ; to get theta. pick nearest tabulated value, then use newton-raphson
  ;  
    guess=[1.0,1.5,1.8,2.0,2.2,2.4,2.6,2.7,2.9,3.1]
    i = (fix(shadow / 10.0) > 0) < 9
    theta1 = 0.1 & theta2 = guess[i]
    while (abs(theta2/theta1 - 1.d0) gt 1.d-5) do begin
       theta1 = theta2
       f = (theta1 - sin(theta1))/!pi - shadow/100.
       fprime = (1.d0 - cos(theta1))/!pi
       theta2 = theta1 - f/fprime
    endwhile

  ;
  ; baseline is then 2rcos(theta/2)
  ;
    bmin = diameter * cos(theta2/2.d0)
    return,bmin
end
