pro apply_we,dir=dir,sign=sign,smoothing=smoothing, noantenna=noantenna

common global
common data_set

   if not keyword_set(sign) then sign=1 
   if not keyword_set(smoothing) then smoothing=0.5

; load in we structure
   readwe,dir=dir,we=we
   if n_elements(we) gt n_elements(in) then we=we[0:n_elements(in)-1L]
   if n_elements(we) lt n_elements(in) then begin
       print, 'Wrong WE structure.'
       return
   endif

if keyword_set(noantenna) then begin 
; calculate the saturation water vapor pressure Psat in unit of hpa
; using the arguably the first reliable equation GOFF-GRATCH EQUATION.
; http://en.wikipedia.org/wiki/Goff-Gratch_equation
   Tst=373.15
   T=we.tamb[0]+273.
   est=1013.25
   Psat=-7.90298*(Tst/T-1)+5.02808*alog10(Tst/T) $
     -1.3816e-7*(10^(11.344*(1-T/Tst))-1) $
     +8.1328e-3*(10^((-3.49149)*(Tst/T-1))-1)+alog10(est)
   Psat=10^Psat

; calculate refractivity

   humidity=we.humid[0]/100.
   Pv=Psat*humidity
;   Nrefrac=77.6*(we.pressure[0]-Pv)/T+64.8*Pv/T+3.776e5*Pv/T/T
;;    Refractivity calculation based on page 509 of the book 
;;    by Thompson, Moran, and Swenson (2nd edition)  
   Nrefrac=77.6*(we.pressure[0]-Pv)/T+(70.4*Pv/T)+(3.739e5*Pv/(T*T))
;;    Refractivity calculation based on SMA memo #152, sec. 6.2 
;   plot,Nrefrac,/ynoz

; smooth Nrefrac over time with boxcar and sample on same interval
; with time.
   wetime=in[we.inhid].dhrs
   plot,wetime,Nrefrac,/ynoz,/nodata,xtitle='UT',ytitle='Refractivity'
   plotS,wetime,Nrefrac,psym=1
   print,'Smoothing in ',smoothing,' minutes:'
   yfs=uti_boxcar(wetime,Nrefrac,smoothing/60.)
   oplot,wetime,yfs

   aa = ''
   read,aa,prompt='Adopt the smoothed refractivity ? [NO <YES>] :'
   if (aa eq 'YES' or aa eq 'yes' or aa eq 'Yes' or aa eq 'Y' or aa eq 'y') then begin 

; calculate the phase change according to Mark Gurwell's log 2720. 

      ii=uti_distinct(in[pil].int,nint,/many_repeat)
      for i=0L, nint-1L do begin       
         result=dat_list(s_l,'"int" eq "'+strcompress(string(ii[i]),/remove)+'"',/reset,/no_notify)
         npts=sp[psl].nrec*sp[psl].nch
         pcl_end=pcl+npts-1L
         bls   = c.blcd[bl[pbl].iblcd]
         recs  = c.rec[bl[pbl].irec]
         sbs   = c.sb[bl[pbl].isb]
         bcals = bls+' '+recs+' '+sbs
         distinct_bcals = uti_distinct(bcals,nbcals,/many_repeat)
         drefrac=yfs[ii[i]]-we[ii[i]].refrac[0]
         for ical = 0L, nbcals-1L do begin
            js = where(distinct_bcals[ical] eq bcals,njs)
            if (njs gt 0L) then begin
               js_cont=where(abs(sp(psl(js)).nch) eq 1,njs_cont)
               for j=0L, njs-1L do begin
                  jsj=js[j]
                  lambda=!cvel/sp[psl[jsj]].fsky/1e9
                  dL=sign*drefrac*1e-6*bl[pbl[jsj]].blu/sin(in[pil[jsj]].el*!PI/180.) ; blu is in unit of meter
                  phase=dL/lambda
                  cphase=complex(cos(phase*!TWOPI),sin(phase*!TWOPI))
                  phase=phase*!TWOPI mod !TWOPI
                  k = where(phase lt -!DPI,nk)
                  if (nk gt 0) then phase(k) = phase(k) + !TWOPI
                  k = where(phase gt !DPI,nk)
                  if (nk gt 0) then phase(k) = phase(k) - !TWOPI   
                  if (sp[psl[jsj]].nch eq 1) then bl[pbl[jsj]].phaave=uti_pha_180(bl[pbl[jsj]].phaave+phase*360./!TWOPI)
                  j1 = pcl[jsj]
                  j2 = pcl_end[jsj]
                  ch[j1:j2]=ch[j1:j2]*cphase
               endfor
            endif
         endfor
      endfor
      result=dat_list(s_l,/reset)

   endif else begin

      print,'No WE correction. Returning!'
   endelse

endif else begin
   antrefrac=fltarr(8,n_elements(we.inhid))
   for iant=1, 8 do begin
      ; ignoring weather information for ant 9 and 10.
      
      Tst=373.15
      T=we.tamb[iant]+273.
      est=1013.25
      Psat=-7.90298*(Tst/T-1)+5.02808*alog10(Tst/T) $
        -1.3816e-7*(10^(11.344*(1-T/Tst))-1) $
        +8.1328e-3*(10^((-3.49149)*(Tst/T-1))-1)+alog10(est)
      Psat=10^Psat

; calculate refractivity

      humidity=we.humid[iant]/100.
      Pv=Psat*humidity
;   Nrefrac=77.6*(we.pressure[0]-Pv)/T+64.8*Pv/T+3.776e5*Pv/T/T
;;    Refractivity calculation based on page 509 of the book 
;;    by Thompson, Moran, and Swenson (2nd edition)  
      Nrefrac=77.6*(we.pressure[iant]-Pv)/T+(70.4*Pv/T)+(3.739e5*Pv/(T*T))
;;    Refractivity calculation based on SMA memo #152, sec. 6.2 
;   plot,Nrefrac,/ynoz

; smooth Nrefrac over time with boxcar and sample on same interval
; with time.
      wetime=in[we.inhid].dhrs
      plot,wetime,Nrefrac,/ynoz,/nodata,xtitle='UT',ytitle='Refractivity'
      plotS,wetime,Nrefrac,psym=1
      print,'Smoothing in ',smoothing,' minutes:'
      yfs=uti_boxcar(wetime,Nrefrac,smoothing/60.)
      oplot,wetime,yfs
      antrefrac[iant-1,*]=yfs
   endfor
   aa = ''
   read,aa,prompt='Adopt the smoothed refractivity ? [NO <YES>] :'
   if (aa eq 'YES' or aa eq 'yes' or aa eq 'Yes' or aa eq 'Y' or aa eq 'y') then begin 

; calculate the phase change according to Mark Gurwell's log 2720. 

      ii=uti_distinct(in[pil].int,nint,/many_repeat)
      for i=0L, nint-1L do begin       
         result=dat_list(s_l,'"int" eq "'+strcompress(string(ii[i]),/remove)+'"',/reset,/no_notify)
         npts=sp[psl].nrec*sp[psl].nch
         pcl_end=pcl+npts-1L
         bls   = c.blcd[bl[pbl].iblcd]
         recs  = c.rec[bl[pbl].irec]
         sbs   = c.sb[bl[pbl].isb]
         bcals = bls+' '+recs+' '+sbs
         distinct_bcals = uti_distinct(bcals,nbcals,/many_repeat)
         for ical = 0L, nbcals-1L do begin
            js = where(distinct_bcals[ical] eq bcals,njs)
            if (njs gt 0L) then begin
;               print,distinct_bcals[ical]
               ant1=bl[pbl[js[0]]].itel1
               ant2=bl[pbl[js[0]]].itel2
;               print,'itel1:',ant1
;               print,'itel2:',ant2
               drefrac=antrefrac[ant1-1,ii[i]]-antrefrac[ant2-1,ii[i]]
               js_cont=where(abs(sp(psl(js)).nch) eq 1,njs_cont)
               for j=0L, njs-1L do begin
                  jsj=js[j]
                  lambda=!cvel/sp[psl[jsj]].fsky/1e9
                  dL=sign*drefrac*1e-6*bl[pbl[jsj]].blu/sin(in[pil[jsj]].el*!PI/180.) ; blu is in unit of meter
                  phase=dL/lambda
                  cphase=complex(cos(phase*!TWOPI),sin(phase*!TWOPI))
                  phase=phase*!TWOPI mod !TWOPI
                  k = where(phase lt -!DPI,nk)
                  if (nk gt 0) then phase(k) = phase(k) + !TWOPI
                  k = where(phase gt !DPI,nk)
                  if (nk gt 0) then phase(k) = phase(k) - !TWOPI   
                  if (sp[psl[jsj]].nch eq 1) then bl[pbl[jsj]].phaave=uti_pha_180(bl[pbl[jsj]].phaave+phase*360./!TWOPI)
                  j1 = pcl[jsj]
                  j2 = pcl_end[jsj]
                  ch[j1:j2]=ch[j1:j2]*cphase
               endfor
            endif
         endfor
      endfor
      result=dat_list(s_l,/reset)

   endif else begin

      print,'No WE correction. Returning!'
   endelse
endelse

end


