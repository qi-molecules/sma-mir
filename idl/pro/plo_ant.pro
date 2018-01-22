function plo_ant,filname
;
; Makes a plot of the antenna locations for your track.
;
;
; parameters 
;            : filname => String for name of postscript file, .ps
;            :            will be attached.
; RETURNS    : X window or PostScript plot of uv distribution.
;
; EXAMPLE    : result = plo_ant('') for a plot of 3mm 
;              uv points to screen without a hardocopy 
;	       after drawing (after 5s short pause).
;	     : result = plo_ant('myplot') for a plot of antenna locations
;              in a postscript file called myplot
;
common global
common data_set
;
; Turn off system messages
;
!QUIET = 1L
;
; Set plot parms
;
!P.TICKLEN = -.01
; 
; Load antenna user symbol
;
myresult = plo_ant_sym()
;
; Determine if filename was passed, then set up for 
; PostScript plot production.
;
IF filname ne '' THEN BEGIN
set_plot,'ps'
device,filename=filname+'.ps',/color
END
;
; load color table so things look nice
;
loadct,40,/silent
;
; Get track number into a string for titles if only one track in dataset,
;  else define track title as "multiple tracks"
;
test = in.traid
result = test(uniq(test,sort(test)))
print,result
IF n_elements(result) ne 1 THEN BEGIN
 track = 'Multiple Track Plot' 
ENDIF ELSE BEGIN
 track = 'Track # '+strtrim(string(in[0].traid),2)+', '
ENDELSE
;
; Get antenna positions for a given soid.
;

FOR j = 0,n_elements(result)-1 DO BEGIN
result=dbi_sql_submit("select tel,te,tn from TPO where soid="+ $
       string(bl[j].soid),/no_notify) 
positions =  result[2:n_elements(result)-3]
new_pos = make_array(n_elements(positions),3,/float)
FOR i = 0, n_elements(positions)-1 DO BEGIN
;     new_pos[i,0:2] = float(str_sep(positions[i],'          ',/trim))
;; The line above uses an obsolate function str_sep, and is replaced
;; by the line below. 2002-JUL-29, KS.
     new_pos[i,0:2] = float(strmid(strsplit(positions[i],'          ',/extract),2))
END
east = new_pos[*,1]
north = new_pos[*,2]
;
; Set up color index
;
index = !D.N_COLORS/(n_elements(east)+10)
offset = .2*!D.N_COLORS
frame = !D.N_COLORS/2
;
; Determine Maxima
;
maxeast =  max(east) + abs(0.1 * max(east))
mineast = min(east) - abs(0.1 * min(east))
maxnorth = max(north) + abs(0.1 * max(north))
minnorth = min(north) - abs(0.1 * min(north))
if minnorth eq 0. then minnorth = -10.
if maxnorth eq 0. then maxnorth = 10.
; Scale by 10 percent to make plot look nice.
;
   plot,[mineast,maxeast],[minnorth,maxnorth],xrange=[mineast,maxeast],yrange=[minnorth,maxnorth],$
    /nodata, xtitle='East [m]',ytitle='North [m]',$
    title=track + 'Antenna locations',color=frame,xstyle = 1,ystyle = 1
;
FOR i = 0,n_elements(east) -1 DO BEGIN
   plots,east[i],north[i],color=index*(i+4)+offset ,psym=8
END

END
;
; Annotate the plot.
;

;
; Shut down PostScript plot if necessary.
;
if filname ne '' then begin
device,/close
set_plot,'x'
end
;
;
; Go back 
;
!P.TICKLEN = 0.02
!QUIET = 0L
return,1
end
