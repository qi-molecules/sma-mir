; *************************************************************************
; FUNCTION
;       wlm_choose_box
;
; PURPOSE
;       Select the WLM boxes the user wants to calibrate
;
; INPUTS
;       use_boxes : Optional integer array indicating whcih wlm boxes are OK
;                   to use.
;       defaults  : If set, then defaults are used for all options without
;                   prompting the user. 
;
; OUTPUTS
;       wlm_ntel  : The number of telescopes present with WLM data
;       wlm_itel  : A [wlm_ntel] vector indicating which boxes will be used.
;
; CALLING SEQUENCE
;     a. use_boxes = [1,2,3,4]
;        result = wlm_choose_box(use_boxes)
;
;     b. result = wlm_choose_box(use_boxes)
;
; *************************************************************************

function wlm_choose_box,use_boxes=use_boxes,defaults=defaults
   ; Common blocks
     common wlm

   ; Initialize array indicating possible WLM boxes to use
     use = wlm_itel
     irepeat = 1
     if keyword_set(use_boxes) then begin
        use = use_boxes
        irepeat = 0
     endif else if (keyword_set(defaults)) then begin
        use = wlm_itel
        irepeat = 0
     endif

   ; Enter look to get valid WLM boxes
     while (irepeat) do begin
        print,"Enter which WLM boxes to use: "
        print,"     (1) All available WLM boxes (default)"
        print,"     (2) Enter WLM box numbers"
        print,"         (i.e. ID numbers separated by spaces or commas)"
        print,format='($,"     Choice?")'
        ichoice = 0
        use = wlm_itel
        input = " "
        read,input
        repeat begin
           pos = strpos(input,',')
           if (pos ne -1) then strput,input,' ',pos
        endrep until (pos eq -1)
        input = strcompress(input)
        if (input eq '' or input eq '1') then $
           irepeat = 0 $
        else begin
           input = uti_distinct(long(strsplit(strcompress(input),' ',/extract)))
           if (not (n_elements(input) eq 1 and input(0) eq -1)) then begin
              use = input
              irepeat = 0
              j = where(use lt 1 or use gt !NTEL,nj)
              if (nj gt 0 or n_elements(use) lt 2) then begin
                 print," "
                 print," "
                 print," "
                 print,"                *** INVALID ENTRY ***"
                 print," "
                 irepeat = 1
              endif
           endif
         endelse
     endwhile

   ; Mask out unwanted boxes
     for i = 0L, n_elements(wlm_itel)-1L do begin
        j = where(wlm_itel[i] eq use,nj)
        if nj eq 0 then wlm_itel[i] = 0
     endfor
     j = where(wlm_itel ne 0, wlm_ntel)
     if wlm_ntel eq 0 then begin
        print,'No WLM boxes available because of USE_BOXES'
        return,-1
     endif
     wlm_itel = wlm_itel[j] 
     print,format='($,"--- Using WLM boxes :")'
     for i = 0L, n_elements(wlm_itel)-1L do $
        print,format='($," ",I2)',wlm_itel[i]
     print," "

   ; Done
     return,1
end
