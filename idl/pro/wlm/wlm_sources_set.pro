; *************************************************************************
; FUNCTION
;      wlm_sources_set
;
; WRITTEN 
;      June 14, 2000 by JMC
;
; PURPOSE
;      Read source names, sidebands, and receivers from ION
;
; INPUTS 
;      sources    : Array of source names
;      isidebands : Integer use array for sidebands. First element is for 
;                   lower sideband, and second element for upper sideband
;      ireceivers : Integer use array for receivers. First element is for 
;                   3mm receiver, and second element is for 1mm receiver.
;
; OUTPUT
;      -1   Successful
;       1   Error
;
; EXAMPLES
;      result = wlm_sources_set(isources,isidebands,ireceivers)
; *************************************************************************

function wlm_sources_set,isources,isidebands,ireceivers
  ; Common blocks
    common global
    common data_set
    common wlm

  ; RECEIVER
    irec = where(ireceivers ne 0, nrec)
    if (nrec ne 1) then return,-1
    irec = irec[0] + 1   ; This assumes irec=1->3mm,  irec=2->1mm
    srec = strcompress(' and "rec" eq "' + string(irec) + '"')

  ; SIDEBAND
    filter = '"band" like "c" ' + srec
    if (isidebands[0] ne 0 and isidebands[1] eq 0) then begin
       filter = filter + ' and "sb" eq "l"'
    endif else if (isidebands[0] eq 0 and isidebands[1] ne 0) then begin
       filter = filter + ' and "sb" eq "u"'
    endif

  ; SOURCE NAMES
    sources = c.source(isources)
    filter = filter + ' and ('
    for i = 0, n_elements(sources)-1L do begin
       if (i ne 0) then filter = filter + " or"
       filter = filter + ' "source" eq "' + sources(i) + '"'
    endfor
    filter = filter + ')'
    wlm_filter = filter

  ; Filter the data
    if (dat_list(s_l,wlm_filter,/reset,/no_notify) le 0) then begin
       print,"Never here: WLM"
       print,filter
       return,-1
    end
end
