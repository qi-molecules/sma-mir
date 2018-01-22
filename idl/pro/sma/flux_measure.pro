pro flux_measure,channel=channel,sideband=sideband

common global
common data_set


if keyword_set(channel) eq 0 then channel='c1'
if e.campuslogin ne 'cqi' then begin
 result=sma_flux_measure(channel,sideband=sideband,sybase=0)
endif else begin
 dbi_sou_write
 result=sma_flux_measure(channel,sideband=sideband,sybase=1)
endelse

end

