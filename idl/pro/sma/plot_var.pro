pro plot_var,x_var=x_var,y_var=y_var,frame_vars=frame_vars, $
                   color_vars=color_vars,symbol_vars=symbol_vars, $
                   frames_per_page=frames_per_page
;yes
;=Task:PLOT_VAR --- To plot the variables within the dataset (SMA wrapper)
;#Type: plotting
;+Use:
;      The PLOT_VAR command can be used to plot the variables within the
;      dataset. It also prints out the avaiblable plottable variables.
;      By default, PLOT_VAR plots elevation vs tsys for each baseline with
;      color set for different sources. To plot variables, type,
;      >plot_var
;      Again, like in PLOT_CONTINUUM and PLOT_SPECTRA, different sources,
;      baselines, bands, and sidebands can be assigned different frames,
;      colors, and symbols. You can set x and y to the variable names (
;      with quote) to plot variables, like
;      >plot_var, x='el', y='tssb'
;      which does the same thing as default, plotting elevation vs. tsys.
;      Some variable plotting needs to use the select command to limit the
;      data.
;         The data can be plotted with multi-frames on a page, multi-colors,
;      within a frame and multiple symbols for each color. In addition,
;      each panel can have subpanels to plot amp, pha and coh seperately.
;@x_var:
;      header variable for x-ccord :
;      'int'  : integrations
;      'hours': hours
;@y_var:
;      variables plotted as y-coord : 
;@frame_vars 
;      header variable used to separate data between frames
;      'blcd'  : baseline --- the default
;      'rec'   : receiver
;      'sb'    : sideband
;      'band'  : chunk
;      'blcd, rec, sb, band': the combination of the above
;
;@color_vars:
;      header variable separating diff colors:
;      'source' : observing sources                      
;@symbol_vars:
;       header variable separating different symbols. There are only 2 types
;      of symbols:
;      0 --- * & solid line
;      1 --- + and dash line
;@frames_per_page 
;      max numbser of frames per page
;      The default is 4
;&history:
;------------------------------------------------------------------------
;      cykuo 26feb04 adapting the header
;------------------------------------------------------------------------

common global
common data_set
common plo

vars=dat_var()
vars_names=vars.tags[where(vars.types ne 'string')]
print, '**** Below are the variable names which can be plotted '
print
print,vars_names
print
print, '**** You can use "pro_hlp, variable=???" to find out the meaning of those variables '
print

if (keyword_set(x_var) eq 0) then x_var = 'el'
if (keyword_set(y_var) eq 0) then y_var = 'tssb'
;if (keyword_set(frame_vars) eq 0) then frame_vars = 'blcd'  
if (keyword_set(frame_vars) eq 0) then frame_vars = 'blcd'  
;if (keyword_set(color_vars) eq 0) then color_vars = 'sb' 
if (keyword_set(color_vars) eq 0) then color_vars = 'source'
if (keyword_set(symbol_vars) eq 0) then symbol_vars = ''
if (keyword_set(frames_per_page) eq 0) then frames_per_page = 4
if (keyword_set(dat) eq 0) then dot=1

if strpos(frame_vars,',') gt 0 then frame_vars=strsplit(frame_vars,',',/extract)

result=plo_var(x_var,y_var,symbol_var=symbol_vars,frame_var=frame_vars,color_var=color_vars,$
 frames_per_page=frames_per_page)

end

