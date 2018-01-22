;yes
;=Task:UTI_TSYS_FIX --- To correct bad tsys values by interpolation
;#type: utility
;+Use:
;         Tsys values are important in SMA data calibration since they 
;      are directly related to the visibility weights and the amplitude 
;      calibration after apply_tsys. Unfortunately currently our continum 
;      detectors are not stable enough to track the tsys changes in the 
;      whole track. Sometimes no tsys values which mir records as 99999L 
;      to minimize the visibility weight, and sometimes some bad tsys 
;      values have been recorded which are shown in the plot of tsys versus 
;      elelation. The program UTI_TSYS_COR can actually correct those tsys v
;      alues according to the correlation between the tsys and el. This 
;      program fit the tsys vs. el, giving the atmospheric tau is very 
;      constant. Of couse we need to ignore the  obvious outliers(e.g. those 
;      no-tsys points) in order to get reasonable fit.
;         The program determine the bad points if the differnce of the  
;      tsys with the fit function is larger than a fixed value( the loose 
;      value), and then replace the bad tsys points with the fitted results.
;         To call this program: 
;      IDL>uti_tsys_cor, highlimit=400, lowlimit=150, loose=20
;      highlimit and lowlimit are the range you decide for obvious outliers,
;      and the  loose value is the fixed value you make the judgement for 
;      bad tsys values compared with the fitting result. The default values
;      for these  three keywords are 1000K, 50K, and 30K respectively. 
;      After the program plot out the fitted function and the real tsys
;      values, it will ask the users whether they'd like to fix the tsys 
;      using the function plotted. One can just use it, or abort it and
;      use different parameters to get better fit.   
;         If users would like to use the program in scripts without inquiring
;      about fixing tsys, they can use
;      IDL>uti_tsys_cor, highlimit=500,lowlimit=300,/no_display.
;&history:
;----------------------------------------------------------------------------
;      cykuo 03mar04 adapting the header
;---------------------------------------------------------------------------

PRO tsys_gfunct, X, A, F, pder
  bx = EXP(A[1] * X)
  F = A[0] * bx + A[2]
;If the procedure is called with four parameters, calculate the
;partial derivatives.
  IF N_PARAMS() GE 4 THEN $
    pder = [[bx], [A[0] * X * bx], [replicate(1.0, N_ELEMENTS(X))]]
END

pro uti_tsys_fix, highlimit=highlimit, lowlimit=lowlimit, loose=loose, tel_bsl=tel_bsl, $
                  refant=refant, no_display=no_display, refit=refit, verbose=verbose

common global
common data_set

if not keyword_set(loose) then loose=30
if not keyword_set(highlimit) then highlimit=1000.
if not keyword_set(lowlimit) then lowlimit=50.
if not keyword_set(tel_bsl) then tel_bsl = 'baseline'
if not keyword_set(verbose) then verbose=0

print, '########################################################'
print, 'THIS PROGRAM WILL RESET DATA LIST/FILTER IN THE PROCESS.'
print, 'IF INTERUPPTED, BE SURE TO MANUALLY RESET YOUR FILTER.'
print, '########################################################'
print, 'PRESS ANY KEY TO CONTINUE.....'
keyin = get_kbrd(1)

if (strcmp(tel_bsl,'baseline')) then begin

  distinct_source=uti_distinct(c.source[in[pil].isource],nsources,/many_repeat)
  distinct_blcd=uti_distinct(c.blcd[bl[pbl].iblcd],nblcds,/many_repeat)

  for ib=0,nblcds-1 do begin
    for is=0,nsources-1 do begin
      result=dat_list(s_l,'"blcd" eq "'+distinct_blcd[ib]+'" and "source" eq "'+distinct_source[is]+'"',/no_notify,/reset)
      ;print,distinct_blcd[ib],distinct_source[is]
      if result le 0 then goto, no_source
      x=1./sin(in[pil].el*!pi/180.)
      y=sp[psl].tssb
      i_good=where((sp[psl].tssb gt lowlimit) and (sp[psl].tssb lt highlimit),count)
      if count le 0 then begin
        print, 'No fitting for source '+distinct_source[is]+' on baseline '+distinct_blcd[ib]
;        result=dat_list(s_l,/reset)
;        print, "Filter Successfully Reset and Exit!"
;        return
     endif else begin
        xx=x[i_good]
        yy=y[i_good]
        weights=fltarr(count)+1.
      ;      weights=1/y^2
        c0=yy[0]-400.*exp(0.2*xx[0])
        A=[400.,0.2,c0]
        yfit=curvefit(xx,yy,weights,A,sigma,itmax=200,function_name='tsys_gfunct')
      ;      print,A
        yfit=A[0]*exp(A[1]*x)+A[2]
        i_int=where(abs(y-yfit) gt loose, count)

      if count gt 0 then begin
        aa='YES'
        if not keyword_set(no_display) then begin
          xline=(max(in[pil].el)-min(in[pil].el))/100.*indgen(100)+min(in[pil].el)
          yline=A[0]*exp(A[1]/(sin(xline*!pi/180.)))+A[2]
          print, 'Fitting tsys values for source '+distinct_source[is]+' on baseline '+distinct_blcd[ib]

          plot,in[pil].el,sp[psl].tssb,/ynoz,psym=4,title='Source '+distinct_source[is]+' on baseline '+distinct_blcd[ib],xtitle='EL',ytitle='TSYS'
          oplot,xline,yline

          read,aa,prompt='Fixing tsys ? [NO <YES>]:  '
        endif
        if (aa eq 'YES' or aa eq 'yes' or aa eq 'Yes' or aa eq 'Y' or aa eq 'y') then begin
          sp[psl[i_int]].tssb=yfit[i_int]
          ; update weights?
       endif
     endif
   endelse
      no_source:
    endfor
  endfor

endif

if (strcmp(tel_bsl,'telescope')) then begin

  distinct_source=uti_distinct(c.source[in[pil].isource],nsources,/many_repeat)
  distinct_blcd=uti_distinct(c.blcd[bl[pbl].iblcd],nblcds,/many_repeat)

  for is=0,nsources-1 do begin

      result=dat_list(s_l,'"source" eq "'+distinct_source[is]+'"',/no_notify,/reset)
      if (result gt 0) then begin

        all_ints=uti_distinct(in[pil].int,nint,/many_repeat)
        int_list=all_ints(uniq(all_ints,sort(all_ints)))

        alltels = [bl[pbl].itel1,bl[pbl].itel2]
        distinct_tels=alltels(  uniq(alltels,sort(alltels) ))
        nants = n_elements(distinct_tels)

        all_sb=uti_distinct(bl[pbl].isb,nsb,/many_repeat)
        all_spect=uti_distinct(sp[pbl].iband,nspect,/many_repeat)

        if (e.debug) then PRINT,'antennas: ', distinct_tels
        if (e.debug) then PRINT,'spec bands: ', all_spect
        if (e.debug) then PRINT,'sidebands: ', all_sb
        if (e.debug) then PRINT,'integrations: ',int_list

        ant_tsys = make_array(nants,nsb,nint,/float,value=0.0)
        ant_xx   = make_array(nants,nsb,nint,/float,value=0.0)
        ant_el   = make_array(nants,nsb,nint,/float,value=0.0)
        ant_flag = make_array(nants,nsb,nint,/long,value=0L)

        fix_flag = make_array(nants,nsb,nint,/long,value=0L)

        PRINT,"--- CYCLING THROUGH ALL INTEGRATIONS FOR SOURCE ",distinct_source[is], " ---"

        for i=0,(nint-1) do begin

          if (verbose) then print,"---  initialize antenna-based tsys matrices ---"

          ;  (wide+narrow) band tsys solutioln flag
          solflag = make_array(nsb,/float,value=0.0)

          ;  temporary (wide+narrow) band tsys matrices
          btsys  = make_array(nants,nants,nsb,/float,value=0.0)
          bflag  = make_array(nants,nants,nsb,/int,value=0)

          if (verbose) then print,'  INTEG ',int_list[i]

          pts = where(in[pil].int eq int_list[i])

          ant_xx[0:(nants-1),0:(nsb-1),i] = 1./sin(in[pil[pts[0]]].el*!pi/180.)
          ant_el[0:(nants-1),0:(nsb-1),i] = in[pil[pts[0]]].el

          bad_l = where((sp[psl[pts]].tssb le lowlimit) and (sp[psl[pts]].tssb ge highlimit), bad_cnt)

          if (bad_cnt gt 0) then begin

            bad_bsl=uti_distinct(bl[pbl[pts[bad_l]]].iblcd,bad_nbsl,/many_repeat)

            for j=0,(bad_nbsl-1) do begin

              if (verbose) then print,' BAD BSL ',c.blcd[all_bsl[j]]
              bad_m = where(bl[pbl[pts[bad_l]]].iblcd eq all_bsl[j])

              ant1 = bl[pbl[pts[bad_l[bad_m[0]]]]].itel1
              noa1 = where(distinct_tels eq ant1)
              noa1 = noa1[0]

              ant2 = bl[pbl[pts[bad_l[bad_m[0]]]]].itel2
              noa2 = where(distinct_tels eq ant2)
              noa2 = noa2[0]

              ; initialize sideband (wide+narrow) band baseline-based tsys array
              for k=0,(nsb-1) do begin

                bad_n = where(bl[pbl[pts[bad_l[bad_m]]]].isb eq all_sb[k] and sp[psl[pts[bad_l[bad_m]]]].iband eq 0, bad_ncount)
                if (bad_nount gt 0) then begin
                  fix_flag[noa1,k,i]=1L
                  fix_flag[noa2,k,i]=1L
                endif

              endfor

            endfor

          endif


          l = where((sp[psl[pts]].tssb gt lowlimit) and (sp[psl[pts]].tssb lt highlimit), count)

          if (count eq 0) then begin

            ant_tsys[0:(nants-1),0:(nsb-1),i] = 0.0 
            print, 'Bad tsys values on all baselines for source ', distinct_source[is],' at int ',int_list[i]
            fix_flag[*,*,i]=1L

          endif else begin

            all_bsl=uti_distinct(bl[pbl[pts[l]]].iblcd,nbsl,/many_repeat)

            if (e.debug) then begin
              print,'bl[pbl[pts[l]]].isb',bl[pbl[pts[l]]].isb
              print,'sp[psl[pts[l]]].iband',sp[psl[pts[l]]].iband
            endif

            if (verbose) then print,'  - cycling through all baselines to get baseline-based tsys  -'

            for j=0,(nbsl-1) do begin

              if (verbose) then print,'    BSL ',c.blcd[all_bsl[j]]
              m = where(bl[pbl[pts[l]]].iblcd eq all_bsl[j])

              ant1 = bl[pbl[pts[l[m[0]]]]].itel1
              noa1 = where(distinct_tels eq ant1)
              noa1 = noa1[0]

              ant2 = bl[pbl[pts[l[m[0]]]]].itel2
              noa2 = where(distinct_tels eq ant2)
              noa2 = noa2[0]

              ; initialize (wide+narrow) band baseline-based tsys array
              for k=0,(nsb-1) do begin

                n = where(bl[pbl[pts[l[m]]]].isb eq all_sb[k] and sp[psl[pts[l[m]]]].iband eq 0, ncount)

                if (ncount gt 0) then begin
                  btsys[noa1,noa2,k]=sp[psl[pts[l[m[n[0]]]]]].tssb
                  if (e.debug) then print,ant1,ant2,nbtsys[noa1,noa2,k],n
                  btsys[noa2,noa1,k]=sp[psl[pts[l[m[n[0]]]]]].tssb
                  bflag[noa1,noa2,k]=1
                endif

              endfor

            endfor

            if (e.debug) then begin
              print,'INT ', int_list[i]
              print,btsys[*,*,0]
            endif

            ; Solving the antenna-based tsys values

            ; going through all sidebands

            for k=0,(nsb-1) do begin

              torefant=0
              if (keyword_set(refant)) then begin
                irefant=where(distinct_tels eq refant)
                irefant=irefant[0]
                resultx=total(bflag[*,irefant,k])
                resulty=total(bflag[irefant,*,k])

                if (resultx eq 0 and resulty eq 0) then begin
                    print, "THE REFERENCE ANTENNA IS NOT PRESENT AT INT ", int_list[i], " FOR  SIDEBAND ", c.sb[all_sb[k]]
                    print, "WILL RE-ASSIGN A REFERENCE ANTENNA FOR THIS INTEGRATION"
                endif else begin
                  if (resulty gt resultx) then antl=where(bflag[irefant,*,k] gt 0) else antl=where(bflag[*,irefant,k] gt 0)
                  torefant=1
                endelse

              endif

              if (torefant eq 0) then begin
                resultx=max(total(bflag[*,*,k],1),bflagx)
                resulty=max(total(bflag[*,*,k],2),bflagy)
                if (resultx ge resulty) then begin
                  irefant=bflagx
                  antl=where(bflag[*,irefant,k] gt 0)
                endif else begin
                  irefant=bflagy
                  antl=where(bflag[irefant,*,k] gt 0)
                endelse
              endif

              antn=n_elements(antl)

              solflag[k]=0
              if (antn ge 2) then begin
                tosol=1
                p=0
                q=p+1
                solflag[k]=1
              endif else begin
                tosol=0
              endelse

              while (tosol eq 1) do begin
                an1=antl(p)
                an2=antl(q)
                if (bflag[an1,an2,k] eq 1) then begin
                  tref=btsys[irefant,an1,k]*btsys[irefant,an2,k]/btsys[an1,an2,k]
                  if (e.debug) then print,irefant,an1,an2,k,tref
                  solflag[k]=1
                  tosol = 0
                endif else begin
                  if (q lt antn-1) then begin
                    q=q+1
                  endif else begin
                    if (p lt antn-2) then begin
                      p=p+1
                      q=p+1
                    endif else begin 
                      tosol = 0
                    endelse
                  endelse
                endelse
              endwhile

              ; (wide+narrow) band tsys
              if (solflag[k] eq 1) then begin
                ;  tsys solution available
                ant_tsys[*,k,i] =  btsys[irefant,*,k]^2/tref
                ant_tsys[irefant,k,i] = tref
                ant_flag[*,k,i] = 1L

                zerolist = where(ant_tsys[*,k,i] eq 0, zerocnt)
                if (zerocnt gt 0) then begin
                  for iz = 0, zerocnt -1 do begin
                    tmpxlist = where((btsys[*,zerolist[iz],k]) gt 0, tmpxcnt)
                    tmpylist = where((btsys[zerolist[iz],*,k]) gt 0, tmpycnt)
                    if (tmpxcnt eq 0 and tmpycnt eq 0) then begin
                      print, " ### NOTE: NO DATA FOR ANT ",distinct_tels[zerolist[iz]], " AT INT ",int_list[i], $
                             "  SIDEBAND ", c.sb[all_sb[k]], " ###"
                      ant_flag[zerolist[iz],k,i] = 0L
                    endif else begin
                      zsol=0
                      if (tmpxcnt gt 0 and zsol eq 0) then begin
                        itmp = 0
                        while (zsol eq 0 and itmp lt tmpxcnt) do begin
                          if (ant_flag[tmpxlist[itmp],k,i] eq 1) then begin
                            ant_tsys[zerolist[iz],k,i] =  btsys[tmpxlist[itmp],zerolist[iz],k]^2/ant_tsys[tmpxlist[itmp],k,i]
                            zsol=1
                          endif
                          itmp = itmp + 1
                        endwhile
                      endif

                      if (tmpycnt gt 0 and zsol eq 0) then begin
                        itmp = 0
                        while (zsol eq 0 and itmp lt tmpycnt) do begin
                          if (ant_flag[tmpylist[itmp],k,i] eq 1) then begin
                            ant_tsys[zerolist[iz],k,i] =  btsys[zerolist[iz],tmpylist[itmp],k]^2/ant_tsys[tmpylist[itmp],k,i]
                            zsol=1
                          endif
                          itmp = itmp + 1
                        endwhile
                      endif

                      if (zsol eq 0) then begin
                        print, " ### NOTE: NOT SOLVABLE FOR ANT ",distinct_tels[zerolist[iz]], " AT INT ",int_list[i], $
                               "  SIDEBAND ", c.sb[all_sb[k]], " ###"
                        ant_flag[zerolist[iz],k,i] = 0L
                      endif
                    endelse
                  endfor
                endif

              endif else begin

                print, "!!! NO SOLUTION DUE TO NO BASELINE LOOP AT INTEGRATION ",int_list[i], "  SIDEBAND ", c.sb[all_sb[k]], " !!!"

              endelse

            endfor

          endelse

        endfor

        coeff = make_array(3,nants,nsb, /float,value=0.0)
        goodtsys = make_array(nants, /float,value=0.0)

        for k = 0, nsb -1 do begin 

          for iant=0, nants -1 do begin

            pl_flag = reform(ant_flag[iant,k,*],nint)
            pl_xx   = reform(  ant_xx[iant,k,*],nint)
            pl_tsys = reform(ant_tsys[iant,k,*],nint)
            pl_el   = reform(  ant_el[iant,k,*],nint)

            igood = where(pl_flag eq 1L, goodcnt)

            gd_flag = pl_flag(igood)
            gd_xx   = pl_xx(igood)
            gd_tsys = pl_tsys(igood)
            gd_el   = pl_el(igood)

            weights=fltarr(goodcnt)+1.

            c0=gd_tsys[0]-400.*exp(0.2*gd_xx[0])
            A=[400.,0.2,c0]
            gd_curve=curvefit(gd_xx,gd_tsys,weights,A,sigma,itmax=200,function_name='tsys_gfunct')

            if not keyword_set(no_display) then begin
              print,'Showing ANT ', distinct_tels(iant), "  SIDEBAND ", c.sb[all_sb[k]], " :"
              xline=(max(gd_el)-min(gd_el))/100.*indgen(100)+min(gd_el)
              yline=A[0]*exp(A[1]/(sin(xline*!pi/180.)))+A[2]
              print, 'TSYS CURVE and FIT FOR SOURCE '+distinct_source[is]+' on ANTENNA '+ string(distinct_tels[iant]), $
                     ' SIDEBAND ', c.sb[all_sb[k]]

              plot,gd_el,gd_tsys,/ynoz,psym=4, $
                   title='Source '+distinct_source[is]+' on antenna '+ string(distinct_tels[iant])+' sideband '+c.sb[all_sb[k]], $
                   xtitle='EL',ytitle='TSYS'
              oplot,xline,yline
            endif

            fu_curve=A[0]*exp(A[1]*pl_xx)+A[2]

            jgood = where(abs(pl_tsys - fu_curve) le loose and pl_tsys ne 0, goodcnt2)

            if (goodcnt2 gt 0) then begin

              ; there are good points, get those points, and plot them in red if display
              gd2_flag = pl_flag(jgood)
              gd2_xx   = pl_xx(jgood)
              gd2_tsys = pl_tsys(jgood)
              gd2_el   = pl_el(jgood)

              if not keyword_set(no_display) then begin
                oplot,gd2_el,gd2_tsys,psym=6,color=250
              endif
            endif

            if (goodcnt2 gt 3) then begin

              ; there are enough good points in the first fit, continue
              jbad = where((pl_flag eq 1L and abs(pl_tsys - fu_curve) gt loose), badcnt2)
              ; mark the bad points if there is any
              if (badcnt2 gt 0) then fix_flag[iant,k,jbad] = 1L

              if (keyword_set(refit)) then begin
                ; use the good points to make another refit if refit set

                weights=fltarr(goodcnt2)+1.
                gd2_curve=curvefit(gd2_xx,gd2_tsys,weights,A,sigma,itmax=200,function_name='tsys_gfunct')

                if not keyword_set(no_display) then begin
                  xline=(max(gd2_el)-min(gd2_el))/100.*indgen(100)+min(gd2_el)
                  yline=A[0]*exp(A[1]/(sin(xline*!pi/180.)))+A[2]
                  oplot,xline,yline,color=250
                endif
              endif
            endif else begin
              ; there are too few good points, initial fit does not work,
              ; simply forget about fitting, all points remain good to be used.
              print, "--- Too few good points remaining with a loose setting of ", loose, $
                     ' for ANT ', distinct_tels(iant), "  SIDEBAND ", c.sb[all_sb[k]], " ---"
              print, "### The fitting is disgarded and all points will remain good  ###"
              A=[0,0,0]

            endelse

            coeff(*,iant,k) = A

            if not keyword_set(no_display) then begin
              print, "PRESS ANY KEY TO CONTINUE....."
              keyin = get_kbrd(1)
            endif

          endfor

          fix_sum = total(fix_flag[*,k,*],1)

          fix_list = where(fix_sum gt 0L, fixcnt)
          if (fixcnt gt 0) then begin
            print, "List of integrations in need of Tsys fix"
            print, int_list[fix_list]

            aa=""
            print, '-------------------------------------------------------------------'
            read,aa,prompt='---> Adopting the antenna solutions for this source and fix tsys?  '
            if (aa eq 'YES' or aa eq 'yes' or aa eq 'Yes' or aa eq 'Y' or aa eq 'y') then begin
              for ifix=0,(fixcnt-1) do begin

                fix_ant = where(fix_flag[*,k,fix_list[ifix]] ne 0L, nfixant)
                if (verbose) then print, "INT ",int_list[fix_list[ifix]], " fix ant ", distinct_tels[fix_ant]

                tmpdata = where(in[pil].int eq int_list[fix_list[ifix]])
                x=1./sin(in[pil[tmpdata[0]]].el*!pi/180.)

                for iant=0, nants -1 do begin
                   if (fix_flag[iant,k,fix_list[ifix]] eq 0L) then begin
                     goodtsys[iant] = ant_tsys[iant,k,fix_list[ifix]]
                   endif else begin
                     FIXA= coeff(*,iant,k)
                     if (total(FIXA) eq 0) then begin
                       print, 'ANT ',distinct_tels[iant], 'Tsys marked bad due to outside limit range.'
                       print, 'No fitting curve is available for this antenna to re-calculate tsys, however '
                       print, 'An arbitrary Tsys of 1000K is inserted'
                       goodtsys[iant]=1000
                     endif else begin
                       goodtsys[iant]=FIXA[0]*exp(FIXA[1]*x)+FIXA[2]
                     endelse
                   endelse                    
                endfor

                if (e.debug) then print, "old tsys", ant_tsys[*,k,fix_list[ifix]]
                if (e.debug) then print, "goodtsys", goodtsys

                for a1=0,(nants-2) do begin
                  for a2=a1+1,(nants-1) do begin
                    if (fix_flag[a1,k,fix_list[ifix]] ne 0 or fix_flag[a2,k,fix_list[ifix]] ne 0) then begin
                      fixpts = where(in[pil].int eq int_list[fix_list[ifix]] and bl[pbl].isb eq all_sb[k] and $
                                      bl[pbl].itel1 eq distinct_tels[a1] and bl[pbl].itel2 eq distinct_tels[a2], npts)
                      if (npts gt 0) then begin
                        sp[psl[fixpts]].tssb=sqrt(goodtsys[a1]*goodtsys[a2])
                        ; update weights?
                      endif
                    endif
                  endfor
                endfor
              endfor
            endif
          endif else begin
            print, '-------------------------------------------------------------------'
            print, ' --> NO TSYS FIXING IS NEEDED FOR THIS SOURCE ',distinct_source[is], " AT  SIDEBAND ", c.sb[all_sb[k]]
            print, "PRESS ANY KEY TO CONTINUE....."
            keyin = get_kbrd(1)
          endelse

        endfor

      endif

  endfor

endif

result=dat_list(s_l,/reset)
print, "Finished, Filter Reset!"

end
