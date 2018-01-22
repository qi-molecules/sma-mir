function cal_dual_scale, x_var=x_var,tel_bsl=tel_bsl, refant=refant, $
                         freqs = freqs, losbs = losbs, hisbs = hisbs, plotps=plotps
;
; Derive calibration offsets
;
;
; parameters : 
; keywords :  x_var    -- x-coordinate
;             tel_bsl  -- 'telescope' or 'baseline' -based solution
;             refant   -- NEWLY ADDED optional parameter to
;                         handle antenna-based cases, as 'ca'
;                         structure does not contain this
;                         information
;             dualfactor -- offset and scaling factors for output
;
; result = -1 (couldn't find or apply cal) , 1 (succesful)
;
   common global
   common data_set

   if not keyword_set(x_var) then x_var='int'
   if not keyword_set(refant) then refant=0
;
;
; first figure out what data descriptors we're looking at
;
   IF dat_list(s_l,'"wt" gt "0"',/reset,/no_notify) LE 0 THEN BEGIN
      print,'No cal application due to lack of data (check filter).'
      return,-1
   ENDIF

   recs=c.rec[bl[pbl].irec]
   distinct_recs = uti_distinct(recs, nrecs)

   if (nrecs lt 2) then begin
     print, "Only One Frequency Band Data Exist"
     return, -1
   endif

   if (tel_bsl ne 'baseline' and tel_bsl ne 'telescope') then begin
     print,'***** Unknown tel_bsl type in gain solution *****'
     return, -1
   endif

   bls=c.blcd[bl[pbl].iblcd]
   alltels = [bl[pbl].itel1,bl[pbl].itel2]

   if (tel_bsl eq 'baseline') then begin
     distinct_tbs = uti_distinct(bls, ntbs)
   endif

   if (tel_bsl eq 'telescope') then begin
     distinct_tbs=alltels(  uniq(alltels,sort(alltels) ))
     ntbs = n_elements(distinct_tbs)
   endif

   sfactor = {tel_bsl:'',losb:'',hisb:'',ratio:0.,offset:0.}
   dualfactor = replicate(sfactor,ntbs)

   avgmin = 10.0
   read,avgmin,prompt='INPUT PHASE AVERAGING TIME INTERVAL (in min) FOR RMS CALCULATION :'
   avgmin = float(avgmin)

   for i=0,ntbs-1 do begin

     print, ' ----- ',tel_bsl,'   ',string(distinct_tbs[i]), ' -----'

     inhid_beg=min([in(pil).inhid]) & inhid_end=max([in(pil).inhid])

     loxs=0
     hixs=0
     loys=0
     hiys=0

     print,' ----- Retreiving Low  Freq Band Phase Gains -----'

     for isb = 0, n_elements(losbs)-1 do begin

       if (tel_bsl eq 'baseline') then begin
         result=dat_comb_sep(distinct_tbs[i]+' '+distinct_recs[0]+' '+losbs[isb],['blcd','rec','sb'],codes, $
                             icodes,n_components)
       endif

       if (tel_bsl eq 'telescope') then begin
         result=dat_comb_sep(bls[0]+' '+distinct_recs[0]+' '+losbs[isb],['blcd','rec','sb'],codes, $
                             icodes,n_components)
         codes[0] = strtrim(distinct_tbs[i],2)
         icodes[0] = -1
       endif

       ;
       ; get gain solution
       ;
       result=cal_store(s_c,'gain',x_var,'pha',tel_bsl,inhid_beg, $
                        inhid_end,codes,icodes,loxs,tmpys,0.0,irow,/restore)
       if (result le 0) then begin
         print,'Errors occured when restoring gain curves for codes ',codes
         return, -1
       endif

       loys=loys+tmpys

     endfor

     loys=loys/n_elements(losbs)
     loys=uti_pha_180(loys)

     print,' ----- Retreiving High Freq Band Phase Gains -----'

     for isb = 0, n_elements(hisbs)-1 do begin

       if (tel_bsl eq 'baseline') then begin
         result=dat_comb_sep(distinct_tbs[i]+' '+distinct_recs[1]+' '+hisbs[isb],['blcd','rec','sb'],codes, $
                             icodes,n_components)
       endif

       if (tel_bsl eq 'telescope') then begin
         result=dat_comb_sep(bls[0]+' '+distinct_recs[1]+' '+hisbs[isb],['blcd','rec','sb'],codes, $
                             icodes,n_components)
         codes[0] = strtrim(distinct_tbs[i],2)
         icodes[0] = -1
       endif

       ;
       ; get and apply gain solution to int_ave and rec and channel data
       ;
       result=cal_store(s_c,'gain',x_var,'pha',tel_bsl,inhid_beg, $
                        inhid_end,codes,icodes,hixs,tmpys,0.0,irow,/restore)
       if (result le 0) then begin
         print,'Errors occured when restoring gain curves for codes ',codes
         return, -1
       endif

       hiys=hiys+tmpys

     endfor

     hiys=hiys/n_elements(hisbs)
     hiys=uti_pha_180(hiys)

     ; Gains Retrieved, phase solution matching

     print,' --- PHASE SOLUTION MATCHING AND RMS ESTIMATING ---'

     lorms = 0.
     hirms = 0.

     tmploys = loys
     tmphiys = hiys

     lomark = make_array(n_elements(loxs),/int)
     himark = make_array(n_elements(hixs),/int)

     lormsmk = make_array(n_elements(loxs),/int)
     hirmsmk = make_array(n_elements(hixs),/int)

     ; finding corresponding Hour, HA, EL for the solution integrations

     lohr = make_array(n_elements(loxs),/float)
     loha = make_array(n_elements(loxs),/float)
     loel = make_array(n_elements(loxs),/float)

     for ix=0,n_elements(loxs) -1 do begin
       j = where(in[pil].int eq loxs[ix])
       lohr[ix]=bl[pbl[j[0]]].avedhrs
       loha[ix]=in[pil[j[0]]].ha
       loel[ix]=in[pil[j[0]]].el
     endfor

     hihr = make_array(n_elements(hixs),/float)
     hiha = make_array(n_elements(hixs),/float)
     hiel = make_array(n_elements(hixs),/float)

     for ix=0,n_elements(hixs) -1 do begin
       j = where(in[pil].int eq hixs[ix])
       hihr[ix]=bl[pbl[j[0]]].avedhrs
       hiha[ix]=in[pil[j[0]]].ha
       hiel[ix]=in[pil[j[0]]].el
     endfor

     totalhrs = [lohr, hihr]
     hrlist = totalhrs(  uniq(totalhrs,sort(totalhrs) ))
     nofhrs = n_elements(hrlist)
     hrtag = make_array(nofhrs)

     interval = avgmin/60.

     grouping = 0
     good_tag = where(hrtag eq 0)
     while (grouping eq 0) do begin

       starttime = min(hrlist(good_tag))
       mark_tag = where(hrlist(good_tag) lt starttime+interval)
       hrtag[good_tag[mark_tag]] = 1

       lo_tag = where((lohr ge starttime) and (lohr le starttime + interval),nloingrp)
       if (nloingrp ge 5) then begin
          lormsmk(lo_tag) = 1
          tmpphase = tmploys[lo_tag]
          ; note: nloingrp can not be used for smooth below, somehow the value gets modified
          tmpflag = uti_pha_unwrap(tmpphase,smooth=n_elements(lo_tag))
          tmpavg = total(tmpphase)/nloingrp
          tmploys[lo_tag] = tmpphase - tmpavg
       endif else begin
;          print, 'TOO FEW (<5) POINTS IN THIS INTERVAL GROUP, NOT USED FOR RMS ESTIMATE'
       endelse

       hi_tag = where((hihr ge starttime) and (hihr le starttime + interval),nhiingrp)
       if (nhiingrp ge 5) then begin
          hirmsmk(hi_tag) = 1
          tmpphase = tmphiys[hi_tag]
          ; note: nhiingrp can not be used for smooth below, somehow the value gets modified
          tmpflag = uti_pha_unwrap(tmpphase,smooth=n_elements(hi_tag))
          tmpavg = total(tmpphase)/nhiingrp
          tmphiys[hi_tag] = tmpphase - tmpavg
       endif else begin
;          print, 'TOO FEW (<5) POINTS IN THIS INTERVAL GROUP, NOT USED FOR RMS ESTIMATE'
       endelse

       grpints = [loxs[lo_tag], hixs[lo_tag]]
       intlist = grpints(  uniq(grpints,sort(grpints) ))
       nofint = n_elements(intlist)

       for ic=0, nofint -1 do begin

           tmplomark = where(loxs eq intlist[ic])
           tmphimark = where(hixs eq intlist[ic])

           if (tmplomark ne -1) and (tmphimark ne -1) then begin
             lomark[tmplomark] = 1
             himark[tmplomark] = 1
           endif else begin
             print,'Dropping solution due to mismatching between two bands at integration ',long(intlist[ic])
           endelse
       endfor

       good_tag = where(hrtag eq 0, ngood_tag)
       if (ngood_tag eq 0) then grouping = 1

     endwhile

     lormsuse = where(lormsmk gt 0,nlormsuse)
     hirmsuse = where(hirmsmk gt 0,nhirmsuse)

     if (nlormsuse eq 0 or nhirmsuse eq 0) then begin
       print,'Phase rms cannot be properly calculated, please use a different averaging time scale.'
       return,0
     end

     tmploys =tmploys(lormsuse)
     lorms = stddev(tmploys,/double)
     tmploys =tmploys(hirmsuse)
     hirms = stddev(tmphiys,/double)

     print,'---------------------------------------------------'
     print,'Low  Freq Band Phase RMS :',lorms, ' deg'
     print,'High Freq Band Phase RMS :',hirms, ' deg'
     print,'---------------------------------------------------'

     louse = where(lomark gt 0)
     loxs = loxs(louse)
     loys = loys(louse)
     lohr = lohr(louse)
     loha = loha(louse)
     loel = loel(louse)

     hiuse = where(himark gt 0)
     hixs = hixs(hiuse)
     hiys = hiys(hiuse)

     ; MATCHED PHASE SEQUENCE, READY FOR FITTING AND PLOTTING

     result=uti_pha_unwrap(loys,smooth=10)
     locenter = mean(loys)

     sortidx = sort(loys)

     uloxs = loxs(sortidx)
     uloys = loys(sortidx)
     uhiys = hiys(sortidx)
       nhr = lohr(sortidx)
       nha = loha(sortidx)
       nel = loel(sortidx)

     result=uti_pha_unwrap(uhiys,smooth=10)

     ploys = locenter + uti_pha_180(uloys - locenter)
     phiys = uti_pha_180(uhiys)

     plotitle = 'Low Freq'+string(freqs[0])+'GHz'
     phititle = 'High Freq'+string(freqs[1])+'GHz'

     result = make_array(2,/float)
     freqratio = freqs[1]/freqs[0]

     xline=findgen(360)-180.0+locenter

     print, "Fixed High/Low Freq ratio is ", freqratio

     if (tel_bsl eq 'telescope' and long(distinct_tbs[i]) eq refant) then begin

       result[0] = 0.
       result[1] = freqratio

       yline = result[0] + result[1]*xline
       yline = uti_pha_180(yline)
       yres  = ploys

       yline0 = result[0] + result[1]*xline
       yline0 = uti_pha_180(yline0)
       yres0  = ploys

     endif else begin

       print, "   LINEAR FITTING OUTPUTS FOR ",tel_bsl," ",distinct_tbs[i]
       print, "                                     OFFSET   (ERR)           SLOPE (ERR)"

       merrors = make_array(n_elements(uloys),value=hirms)
       result = linfit(uloys,uhiys,measure_errors=merrors,sigma=sigma_o,covar=covar_o)
       result[0] = uti_pha_180(result[0])
       print, "W/O LOWFREQ-ERROR, FITTED SLOPE :  ",string(result[0],format='(f8.2)'),"(", $
                                              string(sigma_o[0],format='(f8.2)'),")", $
                                              '      ', $
                                              string(result[1],format='(f6.2)'),"(", $
                                              string(sigma_o[1],format='(f6.2)'),")"

       fitexy, uloys, uhiys, res_0, res_1, X_SIGMA=lorms, Y_SIGMA=hirms,sigma_A_B
       result = [res_0, res_1]
       result[0] = uti_pha_180(result[0])
       print, "W/  LOWFREQ-ERROR, FITTED SLOPE :  ",string(result[0],format='(f8.2)'),"(", $
                                              string(sigma_A_B[0],format='(f8.2)'),")", $
                                              '      ', $
                                              string(result[1],format='(f6.2)'),"(", $
                                              string(sigma_A_B[1],format='(f6.2)'),")"     
       yline = result[0] + result[1]*xline
       yline = uti_pha_180(yline)
       yres =  uti_pha_180(uhiys - (result[0] + result[1]*uloys))

       result[1]=freqratio
       result[0] = (total(uhiys)-result[1]*total(uloys))/n_elements(uloys)
       result[0] = uti_pha_180(result[0])
       mydeloff1 = 1/sqrt(n_elements(uloys)/hirms^2)
       print, "W/O LOWFREQ-ERROR, FIXED  SLOPE :  ",string(result[0],format='(f8.2)'),"(", $
                                              string(mydeloff1,format='(f8.2)'),")", $
                                              '      ', $
                                              string(result[1],format='(f6.2)')

       result[1]=freqratio
       wi = 1/(hirms^2+(result[1])^2*lorms^2)
       sumwi= wi *n_elements(uloys)
       sumwiyx = wi * total(uhiys - result[1] * uloys)
       result[0] = uti_pha_180(sumwiyx/sumwi)
       mydeloff2 = 1/sqrt(sumwi)
       print, "W/  LOWFREQ-ERROR, FIXED  SLOPE :  ",string(result[0],format='(f8.2)'),"(", $
                                              string(mydeloff2,format='(f8.2)'),")", $
                                              '      ', $
                                              string(result[1],format='(f6.2)')
       yline0 = result[0] + result[1]*xline
       yline0 = uti_pha_180(yline0)
       yres0  = uti_pha_180(uhiys - (result[0] + result[1]*uloys))

     endelse

     if (not (tel_bsl eq 'telescope' and long(distinct_tbs[i]) eq refant)) then begin

       ; PLOTTING

       plot,[locenter-180,locenter+180],[-180,180], psym=4, $
            xrange=[locenter-180,locenter+180],yrange=[-180,180], $
            /nodata,title=distinct_tbs[i],xtitle=plotitle,ytitle=phititle
       oplot,ploys,phiys,psym=4
       oplot,[ploys[0],ploys[0]],[phiys[0]-hirms,phiys[0]+hirms]
       oplot,[ploys[0]-lorms,ploys[0]+lorms],[phiys[0],phiys[0]]

       oplot,xline,yline,linestyle=1,color=47662
       oplot,xline,yline0,linestyle=2,color=47772

       if keyword_set(plotps) then begin
         defdevice = !D.NAME
         set_plot,'ps'
         device, filename='dualplot.'+strtrim(string(i),2)+'.ps',/landscape,/color

         plot,[locenter-180,locenter+180],[-180,180], psym=4, $
              xrange=[locenter-180,locenter+180],yrange=[-180,180], $
              /nodata,title=distinct_tbs[i],xtitle=plotitle,ytitle=phititle
         oplot,ploys,phiys,psym=4
         oplot,[ploys[0],ploys[0]],[phiys[0]-hirms,phiys[0]+hirms]
         oplot,[ploys[0]-lorms,ploys[0]+lorms],[phiys[0],phiys[0]]

         oplot,xline,yline,linestyle=1
         oplot,xline,yline0,linestyle=2

         device,/close
         set_plot, defdevice

       endif

       print,'PRESS ANY KEY TO CONTINUE ...'
       keyin = get_kbrd(1)

       !P.Multi = [0,1,3]

       tmptitle = string(tel_bsl)+" "+string(distinct_tbs[i])
       plot,[min(nhr),max(nhr)],[-180,180], psym=4, $
             /nodata, title=tmptitle, xtitle='Hours',ytitle='residual'
       oplot,[min(nhr),max(nhr)],[0,0],linestyle=2,color=1000
       oplot,nhr,yres0,psym=4

       plot,[min(nha),max(nha)],[-180,180], psym=4, $
             /nodata, title=tmptitle, xtitle='H.A.',ytitle='residual'
       oplot,[min(nha),max(nha)],[0,0],linestyle=2,color=1000
       oplot,nha,yres0,psym=4

       plot,[min(nel),max(nel)],[-180,180], psym=4, $
             /nodata, title=tmptitle, xtitle='Elev',ytitle='residual'
       oplot,[min(nel),max(nel)],[0,0],linestyle=2,color=1000
       oplot,nel,yres0,psym=4

       PRINT,'RESIDUAL PHASE FROM FIXED SLOP FITTING PLOTTED'
       PRINT,'PRESS ANY KEY TO CONTINUE......'
       keyin = get_kbrd(1)

       !P.Multi = [0,1,1]

     endif

     ; storing derived factors

     dualfactor[i].tel_bsl = distinct_tbs[i]
     dualfactor[i].losb = losbs
     dualfactor[i].hisb = hisbs
     dualfactor[i].ratio =  result[1]
     dualfactor[i].offset = result[0]
 
   endfor

   print, 'The adopted dual band factors are:'
   for i=0,ntbs-1 do begin
     print, 'Baseline/Antenna  ',dualfactor[i].tel_bsl
     print, '     Phase Ratio  ',dualfactor[i].ratio
     print, '     Phase Offset ',dualfactor[i].offset
   endfor
;
   RETURN, dualfactor
;
   apply_error:
   print,'error in deriving factors '
   RETURN,-1
;
END
