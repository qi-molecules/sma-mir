function plo_ant_sym

;
; Purpose, defines a rough outline of a telescope for a plotting
; sympol.
;
; Usage:  myresult = plo_ant_sym()
;	then use plot with psym=8.
;


x = [-1,-1.5,-1.0,1,-1,0,-1,1,0,1,1,1,2,3]
y = [3,2,1,1.5,1,0,-2,-2,0,-0.5,1.5,-0.5,-0.7,0]
usersym,x,y
return,1
END
