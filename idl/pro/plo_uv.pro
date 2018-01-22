function plo_uv,icode,filname,erase
;
; Makes a symmetrized uv plot for a dataset based on the integration
; baseline u and v's.  Assumes you have already called dat_list.pro
; to select the sideband you want.
;
;
; parameters : icode    => type of plot, 3=3mm, 1=1mm, both = both
;            : filname => String for name of postscript file, .ps
;            :            will be attached.
; RETURNS    : X window or PostScript plot of uv distribution.
;
; EXAMPLE    : result = plo_uv('3mm','','y') for a plot of 3mm 
;              uv points to screen without a hardocopy and erase immediately
;	       after drawing (after 5s short pause).
;	     : result = plo_uv('3mm','myplot','n') for a plot of 3mm
;	       uv points to plotfile = myplot.ps  in startup directory
;
common global
common data_set
;
; Turn off system messages
;
!QUIET = 1L
; 
; Set common plot parameters
;
!X.MARGIN = [10,15]
!Y.MARGIN = [4, 4 ]
;
; Determine how many parameters were passed, if 2 then set up for 
; PostScript plot production.
;
IF filname ne '' THEN BEGIN
set_plot,'ps'
device,filename=filname+'.ps',/color
END
;


; load color table so things look nice
;
loadct,39,/silent
;
; Get track number into a string for titles if only one track in dataset,
;  else define track title as "multiple tracks"
;
test = in.traid
result = test(uniq(test,sort(test)))
IF n_elements(result) ne 1 THEN BEGIN
 track = 'Multiple Track Plot' 
ENDIF ELSE BEGIN
 track = 'Track # '+strtrim(string(in[0].traid),2)+', '
ENDELSE
;
; Define filter strings based on input values.
;

rec_string = '"'+icode+'"'
;print,rec_string
IF icode eq '3mm' THEN rec_string = '"1"'
IF icode eq '1mm' THEN rec_string = '"2"'
IF icode eq 'both' THEN rec_string = '"1" or "irec" eq "2"'
;
; Call dat_list to get data we want.  Don't print junk to screen.
;
localresult = dat_list(s_l,'"rec" eq '+rec_string,/no_notify,/reset)
;
; Check to see if there was data passed
;
IF localresult eq 0 THEN BEGIN
 print, 'No data available for plotting...ending.'
 return,-1
ENDIF

; make a square plot with equal size axes
if !d.x_vsize gt !d.y_vsize then $
        !p.position= [.15*!d.y_vsize/!d.x_size, .15, 0.85*!d.y_vsize/!d.x_size, 0.85]
if !d.x_vsize lt !d.y_vsize then $
        !p.position= [.15, .15*!d.x_vsize/!d.y_size, 0.85, 0.85*!d.x_vsize/!d.y_size]

;
; define title strings based on input code.
;

titlestring = icode + '  UV data'
;print,titlestring
IF icode eq '3mm' THEN titlestring = ' 3mm uv data'
;
IF icode eq '1mm' THEN titlestring = ' 1mm uv data'
;
IF icode eq 'both' THEN titlestring = ' 3mm and 1mm uv data'
;
; Make single frequency plot
;
IF icode ne 'both' THEN BEGIN 
; get max of u or v  for square plot.
;
mymax = max([ max(bl[pbl].u), max(bl[pbl].v), -1*min(bl[pbl].u), -1*min(bl[pbl].v)] )
mymin = -1*mymax
;
; Scale by 10 percent to make plot look nice.
mymax = mymax + .1 * mymax
mymin = mymin + .1 * mymin
;


   plot,[mymin,mymax],[mymin,mymax],xrange=[mymin,mymax],yrange=[mymin,mymax],$
    /nodata, xtitle='U kilolambda',ytitle='V kilolambda',$
    title=track+titlestring,xstyle = 1,ystyle = 1
;    title=track+titlestring,color=255,xstyle = 1,ystyle = 1


;
   oplot,bl[pbl].u,bl[pbl].v,color=115,psym=2
;
   oplot,-1*bl[pbl].u,-1*bl[pbl].v,color=115,psym=2
;
; Add annotation and legend spot
;
x = mymax+.1*mymax
y = 0+.1*mymax
xyouts,x+.05*mymax,y,icode,color=115
plots,x,y+.02*mymax,color=115,psym=2
END
;
; Dual plot
;
IF icode eq 'both' THEN BEGIN 
;
; First figure out which data has longest u,v sizes.
;
localresult = dat_list(s_l,'"rec" eq "1"',/no_notify,/reset)
;
mymaxu = max( [ bl[pbl].u , -1*min(bl[pbl].u) ] )
myminu = -1*mymaxu
mymaxv = max( [ bl[pbl].v, -1*min(bl[pbl].v) ] )
myminv = -1*mymaxv
;
localresult = dat_list(s_l,'"rec" eq "2"',/no_notify,/reset)
;
mymax = max( [ mymaxu, max( [ bl[pbl].u, -1*min(bl[pbl].u) ] ), mymaxv, $
 max( [ bl[pbl].v, -1*min(bl[pbl].v) ] ) ] )
mymin = -1.*mymax
;
; Scale by 10 percent to make plot look nice.
;
mymax = mymax + .1 * mymax
mymin = mymin + .1 * mymin
;
; Restore pointers to BOTH receiver sets. Print out result of max,min
;
localresult = dat_list(s_l,'"rec" eq'+rec_string,/reset)
;
; Define plot frame based on max and min.
;
   plot,[mymin,mymax],[mymin,mymax],xrange=[mymin,mymax],yrange=[mymin,mymax],$
    /nodata, xtitle='u [kilolambda]',ytitle='v [kilolambda]',$
    title=track+titlestring,color=190,xstyle = 1, ystyle = 1
;
; Call filter to get receiver one, then plot.
;
localresult = dat_list(s_l,'"rec" eq "1"',/no_notify,/reset)
;
   oplot,bl[pbl].u,bl[pbl].v,color=115,psym=2
;
   oplot,-1*bl[pbl].u,-1*bl[pbl].v,color=115,psym=2
;
;
; Call filter to get receiver two (1mm), then plot.
;
localresult = dat_list(s_l,'"rec" eq "2"',/no_notify,/reset)
;
   oplot,bl[pbl].u,bl[pbl].v,color=215,psym=4
;
   oplot,-1*bl[pbl].u,-1*bl[pbl].v,color=215,psym=4
;
; Annotate and legend
x = mymax+.1*mymax
y = 0+.1*mymax
xyouts,x+.05*mymax,y,'3mm',color=115
plots,x,y+.02*mymax,color=115,psym=2
y = y-.2*mymax
xyouts,x+.05*mymax,y,'1mm',color=215
plots,x,y+.02*mymax,color=215,psym=4


END
;
; Shut down PostScript plot if necessary.
;
if filname ne '' then begin
device,/close
set_plot,'x'
end
;
;
if erase eq 'y' then BEGIN
wait,5
ERASE
END
;
; Go back 
;
!QUIET = 0L
!p.position=[0,1,0,1] 
return,1
end
