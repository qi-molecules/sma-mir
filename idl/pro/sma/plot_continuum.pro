pro plot_continuum,x_var=x_var,y_vars=y_vars,frame_vars=frame_vars, $
                   color_vars=color_vars,symbol_vars=symbol_vars, $
                   frames_per_page=frames_per_page,no_unwrap=no_unwrap,plid, $
                   cont_band=cont_band, unwrap=unwrap, maxval=maxval, $
                   _extra=extra_keywords 
;yes
;=Task:PLOT_CONTINUUM --- To plot and flag the continuum data (SMA wrapper)
; 
;#Type: plotting
;+Use: 
;      PLOT_CONTINUUM plots both of continuum amplitude and phase for
;      data inspection. In addition, the task allows you to flag the
;      data interactively. To do this, you can just follow the instructions
;      on the terminal when you are executing this program.
;      The data can be plotted with multi-frames on a page, multi-colors,
;      within a frame and multiple symbols for each color. In addition,
;      each panel can have subpanels to plot amp, pha and coh seperately.
;
;@x_var:
;      header variable for x-ccord: 
;      'int'   : integrations 
;      'hours' : observing time in hours --- the default
;@y_vars:
;      variables plotted as y-coord : 
;      'amp'    : amplitude only
;      'pha'    : phase only
;      'amp,pha': both amplitude and phase --- the default
;@frame_vars:
;      header variable used to separate data between plot pages.
;      'blcd'  : baseline
;      'rec'   : receiver
;      'sb'    : sideband
;      'band'  : chunk
;      'blcd, rec, sb, band': the combination of the above --- the default
;@color_vars:
;      header variable separating different colors.
;      'source' : observing sources
;@symbol_vars:
;      header variable separating different symbols. There are only 2 types
;      of symbols:
;      0 --- * & solid line
;      1 --- + and dash line
;@frames_per_page:
;      max numbser of frames per page
;      The default is 4
;@no_unwrap:
;      pase unwrapping 
;      0 = off
;      1 = on --- the default
;&history:
;------------------------------------------------------------------------
;      cykuo 10feb04 adapting the header
;------------------------------------------------------------------------

common global
common data_set
common plo



if (keyword_set(x_var) eq 0) then x_var = 'hours'
if (keyword_set(y_vars) eq 0) then y_vars = 'amp,pha'
;if (keyword_set(frame_vars) eq 0) then frame_vars = 'blcd'  
if (keyword_set(frame_vars) eq 0) then frame_vars = 'blcd,rec,sb,band'  
;if (keyword_set(color_vars) eq 0) then color_vars = 'sb' 
if (keyword_set(color_vars) eq 0) then color_vars = 'source'
if (keyword_set(symbol_vars) eq 0) then symbol_vars = ''
if (keyword_set(frames_per_page) eq 0) then frames_per_page = 4
;if (keyword_set(no_unwrap) eq 0) then no_unwrap=0 else no_unwrap=1
if (keyword_set(unwrap) eq 0) then no_unwrap=1 else no_unwrap=0
if not keyword_set(maxval) then maxval=0  ; added by KS

result=plo_cont(x_var,y_vars,'int',frame_vars,color_vars,symbol_vars,frames_per_page,no_unwrap=no_unwrap,maxval=maxval,_extra=extra_keywords) 


end

