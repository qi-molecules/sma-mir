function cal_store,s_c,cal_type,x_var,y_var,tel_bsl,inhid_beg,inhid_end, $
                 codes,icodes,xs,ys,cal_parm,ic,init=init,save=save, $
                 transfer=transfer,restore=restore,$
                 max_pts_per_row=max_pts_per_row
;
; Store or return calibration data one row at a time.
;
; parameters : s_c        -- structure to save cal temporarily
;              cal_type   -- 'gain','pass','wlm'
;              x_var      -- x-ccord :'int', 'hours', 'el', 'channel'
;              y_var      -- y-coord :'amp','pha','amp,pha','complex'
;                            refers to the type in the calling routine,
;                            for 'amp,pha', the amp's come first, then pha's
;                            for 'complex', the re's come first, then im's
;              tel_bsl    -- 'telescope','baseline'
;              codes      -- char codes describing this cal
;              icodes     -- int code equivalents to codes
;              xs         -- x_coord's (if n_elements(xs) eq 1, return
;                            with original xs of cal curve)
;              ys         -- amp, pha, or complex cal values
;              cal_parm   -- cal param to be recorded (eg. smoothing time)
;              ic         -- row number to save this cal in
;              max_pts_per_row -- maximum number of points per row (default=5000)
; keywords : init         -- initialize cal data arrays w/ init elements
;          : save         -- save this cal data in temporary arrays
;          : transfer     -- transfer temporary arrays to ca structure
;          : restore      -- get a previously stored cal and interpolate
;                            to the input xs points
;
; note : to completely initialize ca structure, set ca.cal_exist=0
;
; result = -1 (couldn't store or find) , 1 (succesful)
;
; See gai.pro for typical sequence of calls for saving and cal_apply.pro
; for sequence to get cals and apply them.
; To store gain fit for amp in row 5 :  
; eg. : result=cal_store(s_c,'gain','int','amp','baseline',inhid_beg, $
;            inhid_end,['1-2','1','u','c1'],[1,1,1,1],xs,ys,5,2.,/save)
;
; To get and interpolate a cal for inhid=35180 :
; inhid_beg=351800 & inhid_end=351800 & xs=findgen(30)
; result=cal_store(s_c,'gain','int','amp','baseline',inhid_beg, $
;            inhid_end,['1-2','1','u','c1'],[1,1,1,1],xs,ys,5,2.,/restore)
;
   common global
   common data_set

  if (e.debug) then begin
    if (keyword_set(codes)) then print, 'calling cal_store for codes ', codes
  endif

;
   IF keyword_set(init) THEN BEGIN
     ; Set maximum number of points per row
       if (not keyword_set(max_pts_per_row)) then max_pts_per_row = 50000
       if (max_pts_per_row lt 1) then begin
          printf,-1,'Error setting max_pts_per_row in cal_store'
          return,-1
       endif
;
; Initialize s_c structure
;
   s_c={cal_type:strarr(init),x_var:strarr(init),y_var:strarr(init), $
         tel_bsl:strarr(init),inhid_beg:lonarr(init), $
          inhid_end:lonarr(init),blcd_tel:strarr(init), $
           iblcd_itel:intarr(init),rec:strarr(init),irec:intarr(init), $
            isb:intarr(init),band:strarr(init),iband:intarr(init), $
             sb:strarr(init),pc_xbeg:lonarr(init),pc_xend:lonarr(init), $
              pc_ybeg:lonarr(init),pc_yend:lonarr(init),cal_parm:fltarr(init),$
               x:make_array(init*max_pts_per_row,/float,value=!values.f_nan), $
                y:make_array(init*max_pts_per_row,/float,value=!values.f_nan), $
                 pcx:0L,pcy:0L}
;
   IF n_params() LE 1 THEN RETURN,-1
   ENDIF
;
; save cal in ca structure 
;
   IF keyword_set(save) THEN BEGIN
;   print, 'Saving gain curve for tel/bsl:',codes[0],' rx:',codes[1], $
;                                 ' sb: ', codes[2],' band: ',codes[3]
      n_values=n_elements(xs)
      IF n_values EQ 0 THEN GOTO, save_error
      s_c.cal_type[ic]=cal_type
      s_c.x_var[ic]=x_var
      s_c.y_var[ic]=y_var
      s_c.tel_bsl[ic]=tel_bsl
      s_c.inhid_beg[ic]=inhid_beg
      s_c.inhid_end[ic]=inhid_end
      s_c.blcd_tel[ic]=codes[0]
      s_c.iblcd_itel[ic]=icodes[0]
      s_c.rec[ic]=codes[1]
      s_c.irec[ic]=icodes[1]
      s_c.sb[ic]=codes[2]
      s_c.isb[ic]=icodes[2]
      s_c.band[ic]=codes[3]
      s_c.iband[ic]=icodes[3]
      s_c.pc_xbeg[ic]=s_c.pcx
      s_c.pc_xend[ic]=s_c.pcx+n_values-1
      s_c.cal_parm[ic]=cal_parm
      ind=s_c.pcx+lindgen(n_values) 
      s_c.x[ind]=xs
;
; save gains 
;
      CASE cal_type OF 
;
         'gain': BEGIN
;
                    CASE y_var OF
;
                       'amp': BEGIN
                                s_c.pc_ybeg[ic]=s_c.pcy
                                s_c.pc_yend[ic]=s_c.pcy+n_values-1
                                ind=s_c.pcy+lindgen(n_values)
                                s_c.y[ind]=ys
                              END
;
                       'pha': BEGIN  
                                s_c.pc_ybeg[ic]=s_c.pcy
                                s_c.pc_yend[ic]=s_c.pcy+n_values-1
                                ind=s_c.pcy+lindgen(n_values)
                                s_c.y[ind]=ys
                              END
;
                       'amp,pha': BEGIN     ; store amp's, then pha's  
                                    s_c.pc_ybeg[ic]=s_c.pcy
                                    s_c.pc_yend[ic]=s_c.pcy+2*n_values-1
                                    ind=s_c.pcy+lindgen(2*n_values)
                                    s_c.y[ind]=ys
                                  END
;
                       'complex': BEGIN     ; store re's, then im's  
                                    s_c.pc_ybeg[ic]=s_c.pcy
                                    s_c.pc_yend[ic]=s_c.pcy+2*n_values-1
                                    ind=s_c.pcy+lindgen(2*n_values) 
                                    s_c.y[ind]=[float(ys),imaginary(ys)]
                                  END
;
                    ENDCASE
                 END 
;
; save passband 
;
         'pass': BEGIN
;
                        CASE y_var OF
;
                           'amp': BEGIN
;
                                     s_c.pc_ybeg[ic]=s_c.pcy
                                     s_c.pc_yend[ic]=s_c.pcy+n_values-1
                                     ind=s_c.pcy+lindgen(n_values)
                                     s_c.y[ind]=ys
;
                                  END
;
                           'pha': BEGIN  
;
                                     s_c.pc_ybeg[ic]=s_c.pcy
                                     s_c.pc_yend[ic]=s_c.pcy+n_values-1
                                     ind=s_c.pcy+lindgen(n_values)
                                     s_c.y[ind]=ys
;
                                  END
;
                           'amp,pha': BEGIN     ; store amp's, then pha's  
;
                                         s_c.pc_ybeg[ic]=s_c.pcy
                                         s_c.pc_yend[ic]=s_c.pcy+2*n_values-1
                                         ind=s_c.pcy+lindgen(2*n_values)
                                         s_c.y[ind]=ys
;
                                      END
;
                           'complex': BEGIN     ; store re's, then im's  
;
                                         s_c.pc_ybeg[ic]=s_c.pcy
                                         s_c.pc_yend[ic]=s_c.pcy+2*n_values-1
                                         ind=s_c.pcy+lindgen(2*n_values) 
                                         s_c.y[ind]=[real(ys),imaginary(ys)]
;
                                      END
;
                        ENDCASE
;
                     END
;
; save water line monitor cal 
;
         'wlm': BEGIN
                   s_c.pc_ybeg[ic]=s_c.pcy
                   s_c.pc_yend[ic]=s_c.pcy+n_values-1
                   ind=s_c.pcy+lindgen(n_values)
                   s_c.y[ind]=ys
                END
;
      ENDCASE
;
;
; update pointers to beg of next row
;
      s_c.pcx=s_c.pc_xend[ic]+1L
      s_c.pcy=s_c.pc_yend[ic]+1L 
;
   ENDIF
;
; transfer cal from temporary to ca structure 
; must concatenate with previously saved cals
; and discard s_c
;
   IF keyword_set(transfer) THEN BEGIN
;
   print, 'Stacking gain curves to the ca structure'
      icmax=max([where(s_c.cal_type ne '')])
      ic=lindgen(icmax+1)
      pcx_prev=max([ca.pc_xend])+1 
      pcy_prev=max([ca.pc_yend])+1
;
      IF ca.cal_exist THEN BEGIN
;
         cal_type=[ca.cal_type(*),s_c.cal_type[ic]]
         x_var=[ca.x_var(*),s_c.x_var[ic]]
         y_var=[ca.y_var(*),s_c.y_var[ic]]
         tel_bsl=[ca.tel_bsl(*),s_c.tel_bsl[ic]]
         inhid_beg=[ca.inhid_beg(*),s_c.inhid_beg[ic]]
         inhid_end=[ca.inhid_end(*),s_c.inhid_end[ic]]
         blcd_tel=[ca.blcd_tel(*),s_c.blcd_tel[ic]]
         iblcd_itel=[ca.iblcd_itel(*),s_c.iblcd_itel[ic]]
         rec=[ca.rec(*),s_c.rec[ic]]
         irec=[ca.irec(*),s_c.irec[ic]]
         sb=[ca.sb(*),s_c.sb[ic]]
         isb=[ca.isb(*),s_c.isb[ic]]
         band=[ca.band(*),s_c.band[ic]]
         iband=[ca.iband(*),s_c.iband[ic]]
         pc_xbeg=[ca.pc_xbeg(*),s_c.pc_xbeg[ic]+pcx_prev]
         pc_xend=[ca.pc_xend(*),s_c.pc_xend[ic]+pcx_prev]
         pc_ybeg=[ca.pc_ybeg(*),s_c.pc_ybeg[ic]+pcy_prev]
         pc_yend=[ca.pc_yend(*),s_c.pc_yend[ic]+pcy_prev]
         cal_parm=[ca.cal_parm(*),s_c.cal_parm[ic]]
         x=[ca.x(*),s_c.x[s_c.pc_xbeg[0]:s_c.pc_xend[icmax]]]
         y=[ca.y(*),s_c.y[s_c.pc_ybeg[0]:s_c.pc_yend[icmax]]]
;
      ENDIF ELSE BEGIN
;
         cal_type=s_c.cal_type[ic]
         x_var=s_c.x_var[ic]
         y_var=s_c.y_var[ic]
         tel_bsl=s_c.tel_bsl[ic]
         inhid_beg=s_c.inhid_beg[ic]
         inhid_end=s_c.inhid_end[ic]
         blcd_tel=s_c.blcd_tel[ic]
         iblcd_itel=s_c.iblcd_itel[ic]
         rec=s_c.rec[ic]
         irec=s_c.irec[ic]
         sb=s_c.sb[ic]
         isb=s_c.isb[ic]
         band=s_c.band[ic]
         iband=s_c.iband[ic]
         pc_xbeg=s_c.pc_xbeg[ic]
         pc_xend=s_c.pc_xend[ic]
         pc_ybeg=s_c.pc_ybeg[ic]
         pc_yend=s_c.pc_yend[ic]
         cal_parm=s_c.cal_parm[ic]
         x=s_c.x[s_c.pc_xbeg[0]:s_c.pc_xend[icmax]]
         y=s_c.y[s_c.pc_ybeg[0]:s_c.pc_yend[icmax]]
;
      ENDELSE
;
      cal_exist=1
      ca={cal_type:cal_type,x_var:x_var,y_var:y_var,tel_bsl:tel_bsl, $
           inhid_beg:inhid_beg,inhid_end:inhid_end,blcd_tel:blcd_tel, $
            iblcd_itel:iblcd_itel,rec:rec,irec:irec,sb:sb, $
             isb:isb,band:band,iband:iband,pc_xbeg:pc_xbeg,pc_xend:pc_xend, $
              pc_ybeg:pc_ybeg,pc_yend:pc_yend,cal_parm:cal_parm,x:x,y:y, $
               cal_exist:cal_exist}
      s_c=0
;
; to print, i'th row of cal :
; print,ca.cal_type[i],ca.x_var[i],ca.y_var[i],ca.tel_bsl[i], $
;    ca.inhid_beg[i],ca.inhid_end[i],ca.blcd_tel[i], $
;    ca.iblcd_itel[i],ca.rec[i],ca.irec[i],ca.sb[i], $
;    ca.isb[i],ca.band[i],ca.iband[i],ca.pc_xbeg[i],ca.pc_xend[i], $
;    ca.pc_ybeg[i],ca.pc_yend[i],ca.cal_parm[i], $
;    ca.x[ca.pc_xbeg[i]:ca.pc_xend[i]],ca.y[ca.pc_xbeg[i]:ca.pc_xend[i]]
;
   ENDIF
;
; restore cal from ca structure and interpolate to specified xs grid 
;
   IF keyword_set(restore) THEN BEGIN
;
; first find which row
;
   IF cal_type EQ 'pass' THEN BEGIN
      ic=max([where(ca.cal_type eq cal_type and ca.tel_bsl eq tel_bsl and $
             ca.x_var eq x_var and $ 
             ca.inhid_beg le (inhid_beg+10) and ca.inhid_end ge (inhid_end-10) $
             and ca.blcd_tel eq codes[0] and ca.rec eq codes[1] and $ 
             ca.sb eq codes[2] and ca.band eq codes[3] and ca.iblcd_itel eq icodes[0],count)])
   ENDIF
;
   IF cal_type EQ 'gain' THEN BEGIN
      ic=max([where(ca.cal_type eq cal_type and ca.tel_bsl eq tel_bsl and $
             ca.x_var eq x_var and $ 
             ca.inhid_beg le (inhid_beg+10) and ca.inhid_end ge (inhid_end-10)$
             and ca.blcd_tel eq codes[0] and ca.rec eq codes[1] and $ 
             ca.sb eq codes[2] and ca.iblcd_itel eq icodes[0],count)])
   ENDIF
;
;
   IF cal_type EQ 'wlm' THEN BEGIN
      ic=max([where(ca.cal_type eq cal_type and ca.tel_bsl eq tel_bsl and $
             ca.x_var eq x_var and ca.y_var eq y_var and $
             ca.inhid_beg ge inhid_beg and ca.inhid_end le inhid_end and $
             ca.blcd_tel eq codes[0] and ca.rec eq codes[1] and $
             ca.sb eq codes[2],count)])
   ENDIF
;
   IF count EQ 0 THEN BEGIN
      print,'could not find ',tel_bsl,'-based ',y_var,' ', $
           cal_type,'(',x_var,') for ',codes
      print,'inh# :',inhid_beg,' => ',inhid_end
      goto, restore_error
   ENDIF

; For WLM data, just return the appropriate CAL structure
  if (cal_type EQ 'wlm') then begin
     xs = ca.x[ca.pc_xbeg[ic]:ca.pc_xend[ic]]
     ys = ca.y[ca.pc_ybeg[ic]:ca.pc_yend[ic]]
     return,1
  endif

  if (e.debug) then  print, 'Successfully loaded gain curves for ', codes

; 
; convert to complex, interpolate then convert to desired output type
;
   n_y=ca.pc_yend[ic]-ca.pc_ybeg[ic]+1
;
   CASE ca.y_var[ic] OF
;
      'amp' : BEGIN
;                 uti_conv_apc,cmp,ca.y[ca.pc_ybeg[ic]:ca.pc_yend[ic]], $
;                    make_array(n_y,/float,value=0.),/complex
                  tmpamp = ca.y[ca.pc_ybeg[ic]:ca.pc_yend[ic]]
                  tmppha = make_array(n_y,/float,value=0.)
                 
              END
;
      'pha' : BEGIN
;                 uti_conv_apc,cmp,make_array(n_y,/float,value=1.), $
;                    ca.y[ca.pc_ybeg[ic]:ca.pc_yend[ic]],/complex
                  tmpamp = make_array(n_y,/float,value=1.)
                  tmppha = ca.y[ca.pc_ybeg[ic]:ca.pc_yend[ic]]
              END
;
      'amp,pha' : BEGIN
                     pc_amp_end=long((ca.pc_ybeg[ic]+ca.pc_yend[ic])/2L) 
                     pc_pha_beg=pc_amp_end+1L
;                     uti_conv_apc,cmp,ca.y[ca.pc_ybeg[ic]:pc_amp_end], $
;                         ca.y[pc_pha_beg:ca.pc_yend[ic]],/complex
                     tmpamp = ca.y[ca.pc_ybeg[ic]:pc_amp_end]
                     tmppha = ca.y[pc_pha_beg:ca.pc_yend[ic]]

                  END
;
      'complex' : BEGIN
;
                     pc_re_end=long((ca.pc_ybeg[ic]+ca.pc_yend[ic])/2) 
                     pc_im_beg=pc_re_end+1
                     cmp=complex([ca.y[ca.pc_ybeg[ic]:pc_re_end]], $
                        [ca.y[pc_im_beg:ca.pc_yend[ic]]])
                     uti_conv_apc,cmp,tmpamp,tmppha,/amp_pha

                  END
;
   ENDCASE

   IF n_elements(xs) EQ 1 THEN xs=ca.x[ca.pc_xbeg[ic]:ca.pc_xend[ic]]
   js=where(ca.x[ca.pc_xbeg[ic]:ca.pc_xend[ic]] eq xs,count)
   ;
   ; *** boxcar smoothing case ***
   ;
   if (ca.cal_parm[ic] gt 0) then begin
     ;
     ; check whether interpolation is necessary
     ;
     IF (count NE n_elements(xs) OR $
         count NE (ca.pc_xend[ic])-ca.pc_xbeg[ic]+1) THEN BEGIN
        if (e.debug) then print, 'Interpolation of solution for smoothing case'
        tmpamp=uti_interp(tmpamp, ca.x[ca.pc_xbeg[ic]:ca.pc_xend[ic]],xs)
        tmppha=uti_interp(tmppha, ca.x[ca.pc_xbeg[ic]:ca.pc_xend[ic]],xs)
     ENDIF
     uti_conv_apc,cmp,tmpamp,tmppha,/complex 
   endif
   ;
   ; polynomical fit or connecting case
   ;
   if (ca.cal_parm[ic] le 0) then begin

     ;
     ; connecting case
     ;
     if (ca.cal_parm[ic] eq -0.5) then begin
       ;
       ; check whether interpolation is necessary
       ;
       IF (count NE n_elements(xs) OR $
           count NE (ca.pc_xend[ic])-ca.pc_xbeg[ic]+1) THEN BEGIN
          if (e.debug) then print, 'Interpolation of solution for connecting case'
          tmpamp=uti_interp(tmpamp, ca.x[ca.pc_xbeg[ic]:ca.pc_xend[ic]],xs)
          tmppha=uti_interp(tmppha, ca.x[ca.pc_xbeg[ic]:ca.pc_xend[ic]],xs)

       ENDIF
       uti_conv_apc,cmp,tmpamp,tmppha,/complex 
     endif else begin
     ;
     ; polynomical fitting case
     ;
       ;
       ; check whether interpolation  is necessary
       ;
       IF (count NE n_elements(xs) OR $
           count NE (ca.pc_xend[ic])-ca.pc_xbeg[ic]+1) THEN BEGIN
         if (e.debug) then print, 'Interpolation of solution needed for polyfit case'
         order=-ca.cal_parm[ic]
         if (e.debug) then print, "polyfit with order ", order
         ampcoeff = poly_fit(ca.x[ca.pc_xbeg[ic]:ca.pc_xend[ic]],tmpamp,order,/double)
         if (e.debug) then print, "amp curve coeffs are ", ampcoeff
         phacoeff = poly_fit(ca.x[ca.pc_xbeg[ic]:ca.pc_xend[ic]],tmppha,order,/double)
         if (e.debug) then print, "pha curve coeffs are ", phacoeff

         new_amp = make_array(n_elements(xs),/double,value=0)
         new_pha = make_array(n_elements(xs),/double,value=0)

         for ocount = 0, order do begin
           new_amp = new_amp + ampcoeff[ocount] * xs^ocount
           new_pha = new_pha + phacoeff[ocount] * xs^ocount
         endfor

         uti_conv_apc,cmp,new_amp,new_pha,/complex    
       ENDIF ELSE BEGIN
         uti_conv_apc,cmp,tmpamp,tmppha,/complex 
       ENDELSE
     endelse

   endif

;
; convert to desired output variable type
;
      CASE y_var OF
;
         'amp' : BEGIN
;
                    uti_conv_apc,cmp,ys,dum,/amp_pha
;
                 END
;
         'pha' : BEGIN
;
                    uti_conv_apc,cmp,dum,ys,/amp_pha
;
                 END
;
         'amp,pha' : BEGIN
;
                        uti_conv_apc,cmp,amp,pha,/amp_pha
                        ys=[amp,pha]
;
                     END
;
         'complex' : BEGIN
;
                        ys=[float(cmp),imaginary(cmp)]
;
                     END
      ENDCASE
;
   ENDIF
   return,1
;
   save_error:
     print,'error in saving ',cal_type,' ',n_values
     return,-1
;
   restore_error:
     print,'error in restoring ',cal_type
     return,-1
;
END
