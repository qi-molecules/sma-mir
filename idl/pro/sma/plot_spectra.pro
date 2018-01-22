pro plot_spectra,source=source, $
                   x_var=x_var,y_var=y_vars,frame_vars=frame_vars, $
                   color_vars=color_vars, $
                   symbol=symbol_vars,frames_per_page=frames_per_page,plid,$
                   ntrim_max=ntrim_max, old=old, _extra=extra_keywords
;yes
;=Task:PLOT_SPECTRA --- To plot the spectra of the line data (SMA wrapper)
;#Type: plotting
;+Use: The PLOT_SPECTRA command can be issued with no arguments to plot
;      the amplitude and phase fo the data as a function of channel numbers.
;      Different sources, baselines, bands, and sidebands can be assigned 
;      different  frames, colors, and symbols. Even so, plotting all the 
;      data might produce a complicated plot. Use the select command to 
;      limit the data. PLOT_SPECTRA includes a source selection argument
;      for convenience. 
;@x_var:
;      variable for x-coordinate:
;      'channel' : channel numbers
;      'fsky'    : sky frequency
;      'velocity': velocity
;@y_vars:
;      variable plotted as y-coordinate:
;      'amp'    : amplitude
;      'amp,pha': amplitude and phase
;@frame_vars:
;      header variable used to separate data between plot pages
;      'blcd,rec' : baseline + receiver --- the default
;      'sb'   : sideband
;      'band' : chunk
;      ;blcd' : baseline
;      'blcd,band'
;      'blcd,sb'
;      'rec'
;      'rec,sb'
;      'blcd,rec,sb,band'
;      'rec,band'
;@color_vars:
;      header variable separating different colors:
;      'sb' : sideband --- the default
;      'blcd'
;      'band'
;      'rec'
;      'rec,sb'
;@symbol_vars:
;      header variable separating different symboles. There are only
;      2 types of symbols:
;      0 --- * & solid line --- the default
;      1 --- + & dash line
;@frames_per_page:
;      maximum number of frames per page
;@source:
;      keyword to select a specific source
;@preavg: channel number to average
;@smoothing: channel number to smooth
;@normalize: normalize by continuum
;@unwrap: flag for phase unwrap
;&history:
;------------------------------------------------------------------------
;      cykuo 10feb04 adapting the header
;------------------------------------------------------------------------

common global
common data_set
common plo


if (keyword_set(x_var) eq 0) then x_var = 'channel'
if (keyword_set(y_vars) eq 0) then y_vars = 'amp,pha'
if (keyword_set(frame_vars) eq 0) then frame_vars = 'blcd,rec'  
if (keyword_set(color_vars) eq 0) then color_vars = 'sb' 
if (keyword_set(symbol_vars) eq 0) then symbol_vars = ''
if (keyword_set(frames_per_page) eq 0) then frames_per_page = 4

if (strpos(frame_vars,'pol',0) ne -1) or (strpos(color_vars,'pol',0) ne -1) then begin
   result = plo_spec2(x_var,y_vars,frame_vars,color_vars, $
                   symbol_vars,frames_per_page,plid, $
                   source=source,ntrim_max=ntrim_max,$
                   _extra=extra_keywords)
endif else begin
   result = plo_spec(x_var,y_vars,frame_vars,color_vars, $
                   symbol_vars,frames_per_page,plid, $
                   source=source,ntrim_max=ntrim_max,$
                   _extra=extra_keywords)
endelse

 
result=dat_list(s_l,/reset)
end

