FUNCTION cursor_loop2,array,click_location
If click_location[0] GE 0.3 AND click_location[0] LE 0.9 THEN BEGIN
	xval=ROUND((click_location[0]-0.3)*(n_elements(array)-1L)/(0.9-0.3))
	yval=array(xval)
	xyouts,xval+1,yval,'X:' + STRTRIM(STRING(xval,'(I4)'),2),/data
	xyouts,xval+1,yval-1,'Value:' +STRTRIM(STRING(yval,FORMAT='(f8.2)'),2),/data
	EMPTY	;Force Java to draw the canvas
	cursor,xpos,ypos,/normal
	RETURN,[xpos,ypos]
ENDIF ELSE RETURN, [0.3,0.9]
END
