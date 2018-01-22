function cal_apply2,gain=gain,passband=passband,wlm=wlm,mklog=mklog, $
                   x_var=x_var,tel_bsl=tel_bsl,refant=refant, $
                   dualmode=dualmode, dualfactor=dualfactor, $
                   poldualrx=poldualrx
;
; Apply calibration data to all data seen through filter.
;
; parameters : 
; keywords :  gain     -- apply gain solution ('amp','pha','amp,pha') 
;             passband -- apply passband ('amp,pha')
;             wlm      -- apply wlm corrections
;             mklog    --
;             x_var    -- x-coordinate
;             tel_bsl  -- 'telescope' or 'baseline' -based solution
;             refant   -- NEWLY ADDED optional parameter to
;                         handle antenna-based cases, as 'ca'
;                         structure does not contain this
;                         information
;
; result = -1 (couldn't find or apply cal) , 1 (succesful)
;
; to apply both amp and pha gains :
; eg. : result=cal_apply(gain='amp,pha')
; eg. : result=cal_apply(passband='amp,pha')
; eg. : result=cal_apply(wlm='pha')
;
   common global
   common data_set
IF not keyword_set(poldualrx) then poldualrx=[-1,-1] 
;
;
; first figure out what data descriptors we're looking at
;
 IF dat_list(s_l,'"wt" gt "0"',/reset,/no_notify) LE 0 THEN BEGIN
    print,'No cal application due to lack of data (check filter).'
    return,-1
 ENDIF

 if (keyword_set(dualmode)) then begin

   allrecs=c.rec[bl[pbl].irec]
   distinct_allrecs = uti_distinct(allrecs,nallrecs,/many_repeat)

   if (not keyword_set(dualfactor)) then begin
     IF dat_list(s_l,'("rec" eq "'+strtrim(distinct_allrecs[0],2)+'" and "wt" gt "0")', $
                 /reset,/no_notify) LE 0 THEN BEGIN
        print,'No cal application due to lack of data (check filter).'
        return,-1
     ENDIF
;     print,n_elements(pil)
   endif else begin
     IF dat_list(s_l,'("rec" eq "'+strtrim(distinct_allrecs[1],2)+'" and "wt" gt "0")', $
                 /reset,/no_notify) LE 0 THEN BEGIN
        print,'No cal application due to lack of data (check filter).'
        return,-1
     ENDIF
;     print,n_elements(pil)
   endelse

 endif


 IF keyword_set(wlm) THEN BEGIN
;
; set up array cal_cmp to contain the complex cal multiplicative
; factors to be accumulated from the gain, passband, and wlm. this
; array is initialized to amp = 1. pha = 0. in complex vis.
;
   uti_conv_apc,cmp_init,1.,0.,/complex
   cal_cmp=make_array(n_elements(ch),/complex,value=cmp_init)
 ENDIF

;
; segment the data by blcd, receiver, sb, and band
;
   bls=c.blcd[bl[pbl].iblcd]
   recs=c.rec[bl[pbl].irec]
   sbs=c.sb[bl[pbl].isb]
   bands=c.band[sp[psl].iband]
   distinct_bls=uti_distinct(bls,nbls,/many_repeat)
   distinct_recs=uti_distinct(recs,nrecs,/many_repeat)
   distinct_sbs=uti_distinct(sbs,nsbs,/many_repeat)
   distinct_bands=uti_distinct(bands,nbands,/many_repeat)
;
;
; ****************** GAINS **********************************
;
   IF keyword_set(gain) THEN BEGIN

      if not keyword_set(tel_bsl) then tel_bsl='baseline'

      if (tel_bsl ne 'baseline' and tel_bsl ne 'telescope') then begin
        print,'************* Unknown tel_bsl type in gain solution *******************'
        return, -1
      endif
;
; set up pointer arrays for indexes in the ch array (channel 
; and record complex vis data) :
; npts -- the product of the number of chan and recs for each
; pcl_end -- the last index for chans or recs for this spectrum
; 
      npts=sp[psl].nrec*sp[psl].nch
      pcl_end=pcl+npts-1L
;
; setup the variables which will discriminate gains
;
      gcals=bls+' '+recs+' '+sbs
      distinct_gcals=uti_distinct(gcals,ngcals,/many_repeat)

      if (keyword_set(dualmode) and keyword_set(dualfactor)) then begin
        g2cals=bls+' '+distinct_allrecs[0]+' '+sbs
        distinct_g2cals=uti_distinct(g2cals,ngcals,/many_repeat)
      endif

      if not keyword_set(x_var) then x_var='int'

      FOR i=0,ngcals-1 DO BEGIN

         result=dat_comb_sep(distinct_gcals[i],['blcd','rec','sb'],codes, $
                             icodes,n_components)
         js=where(distinct_gcals[i] eq gcals,count_js)

         if (keyword_set(dualmode) and keyword_set(dualfactor)) then begin
           result=dat_comb_sep(distinct_g2cals[i],['blcd','rec','sb'],codes, $
                               icodes,n_components)
         endif

         ;
         ; extract both antennas
         ;
         if (tel_bsl eq 'telescope') then begin
           tel1 = c.tel1[bl[pbl[js]].itel1]
           tel2 = c.tel2[bl[pbl[js]].itel2]
           atel1 = tel1(uniq(tel1))
           atel2 = tel2(uniq(tel2))
         endif

         inhid_beg=min([in(pil).inhid]) & inhid_end=max([in(pil).inhid])
         case x_var of 
             'int': begin
                      xs=float([in[pil[js]].int])
                    end
             'hours': begin
                      xs=float([bl[pbl[js]].avedhrs])
                    end
             'ha': begin
                      xs=float([in[pil[js]].ha])
                    end
             'el': begin
                      xs=float([in[pil[js]].el])
                    end
             else: begin
                 print,'*** ',x_var,' not recognized x-coord !'
                 return,-1
             endelse
          endcase

;
; get and apply gain solution to int_ave and rec and channel data
;

         FOR j=0,n_elements(gain)-1 DO BEGIN

         if (tel_bsl eq 'baseline') then begin
           result=cal_store(s_c,'gain',x_var,gain[j],tel_bsl,inhid_beg, $
                            inhid_end,codes,icodes,xs,ys,0.0,irow,/restore)

           if (result le 0) then begin
             print,'Errors occured when restoring gain curves for codes ',codes
             if  (keyword_set(dualmode)) then begin
               print,'In Dual Calibration Mode with Gain Scaling, Try Opposite Sideband.'
               if (codes[2] eq strtrim('u',2)) then begin
                 codes[2]='l'
                 icodes[2]=0
               endif else begin
                 if (codes[2] eq strtrim('l',2)) then begin
                   codes[2]='u'
                   icodes[2]=1
                 endif
               endelse
               result=cal_store(s_c,'gain',x_var,gain[j],tel_bsl,inhid_beg, $
                                inhid_end,codes,icodes,xs,ys,0.0,irow,/restore)
               if (result le 0) then begin
                 print,'Still Errors occured when restoring gain curves for codes ',codes
                 return, -1
               endif
             endif else begin
               return, -1
             endelse
           endif

           if (keyword_set(dualmode) and keyword_set(dualfactor)) then begin
               fidx = where(strtrim(dualfactor.tel_bsl,2) eq codes[0])
               ys = ys * dualfactor(fidx).ratio + dualfactor(fidx).offset
           endif
         endif

         if (tel_bsl eq 'telescope') then begin

            ;
            ; loading the first antenna gains
            ;
            codes[0]=atel1
            icodes[0]=-1
  
            result=cal_store(s_c,'gain',x_var,gain[j],tel_bsl,inhid_beg, $
                             inhid_end,codes,icodes,xs,ys1,0.0,irow,/restore)

            if (result le 0) then begin
              print,'Errors occured when restoring gain curves for codes ',codes
              if  (keyword_set(dualmode)) then begin
                print,'In Dual Calibration Mode with Gain Scaling, Try Opposite Sideband.'
                if (codes[2] eq strtrim('u',2)) then begin
                  codes[2]='l'
                  icodes[2]=0
                endif else begin
                  if (codes[2] eq strtrim('l',2)) then begin
                    codes[2]='u'
                    icodes[2]=1
                  endif
                endelse
                result=cal_store(s_c,'gain',x_var,gain[j],tel_bsl,inhid_beg, $
                                 inhid_end,codes,icodes,xs,ys1,0.0,irow,/restore)
                if (result le 0) then begin
                  print,'Still Errors occured when restoring gain curves for codes ',codes
                  return, -1
                endif
              endif else begin
                return, -1
              endelse
            endif

            if (keyword_set(dualmode) and keyword_set(dualfactor)) then begin
              fidx = where(strtrim(dualfactor.tel_bsl,2) eq codes[0])
              ys1 = ys1 * dualfactor(fidx).ratio + dualfactor(fidx).offset
            endif

            ;
            ; loading the second antenna gains
            ;
            codes[0]=atel2
            icodes[0]=-1

            result=cal_store(s_c,'gain',x_var,gain[j],tel_bsl,inhid_beg, $
                             inhid_end,codes,icodes,xs,ys2,0.0,irow,/restore)

            if (result le 0) then begin
              print,'Errors occured when restoring gain curves for codes ',codes
              if  (keyword_set(dualmode)) then begin
                print,'In Dual Calibration Mode with Gain Scaling, Try Opposite Sideband.'
                if (codes[2] eq strtrim('u',2)) then begin
                  codes[2]='l'
                  icodes[2]=0
                endif else begin
                  if (codes[2] eq strtrim('l',2)) then begin
                    codes[2]='u'
                    icodes[2]=1
                  endif
                endelse
                result=cal_store(s_c,'gain',x_var,gain[j],tel_bsl,inhid_beg, $
                                 inhid_end,codes,icodes,xs,ys2,0.0,irow,/restore)
                if (result le 0) then begin
                  print,'Still Errors occured when restoring gain curves for codes ',codes
                  return, -1
                endif
              endif else begin
                return, -1
              endelse
            endif

            if (keyword_set(dualmode) and keyword_set(dualfactor)) then begin
              fidx = where(strtrim(dualfactor.tel_bsl,2) eq codes[0])
              ys2 = ys2 * dualfactor(fidx).ratio + dualfactor(fidx).offset
            endif

            ;
            ; combining two antenna gains to a baseline gain
            ;
            CASE gain[j] of
                 'amp': begin
                           ys = ys1 * ys2
                        end
                 'pha': begin
                           ys = ys1 - ys2
                        end
                 'amp,pha': begin
                           ys=make_array(2 * count_js,/float)
                           ys[0:count_js-1L] = ys1[0:count_js-1L]* ys2[0:count_js-1L]
                           ys[count_js:2*count_js-1L] = $
                               ys1[count_js:2*count_js-1L] -  ys2[count_js:2*count_js-1L]
                        end
                 'complex': begin
                           ys = ys1 * conj(ys2)
                        end
            ENDCASE

         endif

            IF result EQ 1 THEN BEGIN
               CASE gain[j] OF
                  'amp' : BEGIN
                             IF count_js GT 1 THEN BEGIN
                                bl[pbl[js]].ampave= bl[pbl[js]].ampave/ys
                                sp[psl[js]].wt= sp[psl[js]].wt * ys * ys
                                re.wts[prl[js]]=sp[psl[js]].wt*re.integs[prl[js]]
                             ENDIF
                             IF count_js EQ 1 THEN BEGIN
                                bl[pbl[js]].ampave= bl[pbl[js]].ampave/ys[0]
                                sp[psl[js]].wt= sp[psl[js]].wt * ys[0] * ys[0]
                                re.wts[prl[js]]=sp[psl[js]].wt*re.integs[prl[js]]
                             ENDIF
                             uti_conv_apc,gain_cmp,ys, $
                             make_array(count_js,/float,value=0.),/complex
                          END
                  'pha' : BEGIN
                            IF count_js GT 1 THEN bl[pbl[js]].phaave= $
                                           uti_pha_180(bl[pbl[js]].phaave-ys)
                            IF count_js EQ 1 THEN bl[pbl[js]].phaave= $
                                           uti_pha_180(bl[pbl[js]].phaave-ys[0])
                            uti_conv_apc,gain_cmp, $
                            make_array(count_js,/float,value=1.),ys,/complex
                          END
                  'amp,pha' : BEGIN
                                 IF count_js GT 1 THEN BEGIN
                                    bl[pbl[js]].ampave= $
                                         bl[pbl[js]].ampave/ys[0:count_js-1]
	                            sp[psl[js]].wt= $
                                         sp[psl[js]].wt*ys[0:count_js-1]*ys[0:count_js-1]
                                    re.wts[prl[js]]=sp[psl[js]].wt*re.integs[prl[js]]
                                    ENDIF
                                 IF count_js GT 1 THEN bl[pbl[js]].phaave= $ 
                                           uti_pha_180(bl[pbl[js]].phaave- $
                                              ys[count_js:2*count_js-1])
                                 IF count_js EQ 1 THEN BEGIN
                                    amp_cor=ys[0] 
                                    pha_cor=ys[1]
                                    bl[pbl[js]].ampave=bl[pbl[js]].ampave/ $
                                                       amp_cor
                                    sp[psl[js]].wt=sp[psl[js]].wt*amp_cor*amp_cor
                                    re.wts[prl[js]]=sp[psl[js]].wt*re.integs[prl[js]]
                                    bl[pbl[js]].phaave= $
                                       uti_pha_180(bl[pbl[js]].phaave-pha_cor)
                                 ENDIF
                                 uti_conv_apc,gain_cmp, $
                                    ys[0:count_js-1],ys[count_js:2*count_js-1],$
                                                        /complex
                              END
                  'complex' : BEGIN
                                 uti_conv_apc,cmp,bl[pbl[js]].ampave, $
                                    bl[pbl[js]].phaave,/complex
                                 cmp=cmp/ys
                                 sp[psl[js]].wt=sp[psl[js]].wt*abs(ys)*abs(ys)
                                 re.wts[prl[js]]=sp[psl[js]].wt*re.integs[prl[js]]
                                 uti_conv_apc,cmp,bl[pbl[js]].ampave, $
                                    bl[pbl[js]].phaave,/amp_pha
                                 gain_cmp=ys
                              END
                  ELSE: BEGIN 
                           print,'*** ',gain[j],' invalid gain type !' &     
                           return,-1
                  ENDELSE
               ENDCASE
;
; set the correction factors for the channel and record data
;
               FOR k=0L,n_elements(js)-1L DO BEGIN
                  ch[pcl[js[k]]:pcl_end[js[k]]]= $
                  ch[pcl[js[k]]:pcl_end[js[k]]]/gain_cmp[k]
               ENDFOR
            ENDIF
         ENDFOR

;      stop
      ENDFOR
      gcals = 0
      distinct_gcals = 0  ; to free up memory
   ENDIF
;
; ****************** passband **********************************
;
   IF keyword_set(passband) THEN BEGIN

      if not keyword_set(tel_bsl) then tel_bsl='baseline'

      if (tel_bsl ne 'baseline' and tel_bsl ne 'telescope') then begin
        print,'************* Unknown tel_bsl type in passband solution *******************'
        return, -1
      endif
;
;
; set up a pointer arrays for indexes in the ch array (channel 
; and record complex vis data) :
; npts -- the product of the number of chan and recs for each
; pcl_end -- the last index for chans or recs for this spectrum
; 
      npts=sp[psl].nrec*sp[psl].nch
      pcl_end=pcl+npts-1L
;
; setup the variables which will discriminate passbands
;
;      pcals=bls+' '+recs+' '+sbs+' '+bands
;      distinct_pcals=uti_distinct(pcals,npcals,/many_repeat)

      a0=pil & a1=pbl & a2=psl & a3=pcl & a4=pcl_end & index=-1L
      inhid_beg=min([in(pil).inhid]) & inhid_end=max([in(pil).inhid])
      codes=strarr(4)
      icodes=intarr(4)
      FOR jbl=0,nbls-1 DO BEGIN
         codes[0]=distinct_bls[jbl]
         icodes[0]=min(where(c.blcd EQ codes[0]))
         FOR jrx=0,nrecs-1 DO BEGIN
            codes[1]=distinct_recs[jrx]
            icodes[1]=min(where(c.rec EQ codes[1]))
            FOR jsb=0,nsbs-1 DO BEGIN
               ; save icodes0 for antenna-based cal
               icodes0=icodes[0]
               codes[2]=distinct_sbs[jsb]
               icodes[2]=min(where(c.sb EQ codes[2]))
               n=where( (bl[a1].iblcd EQ icodes[0]) AND $
                 (bl[a1].irec EQ icodes[1]) AND $
                 (bl[a1].isb EQ icodes[2]), count)
               IF count EQ 0 THEN GOTO, jump1
               b0=a0[n] & b1=a1[n] & b2=a2[n] & b3=a3[n] & b4=a4[n]
               FOR m=0, nbands-1 DO BEGIN
                  codes[3]=distinct_bands[m]
                  IF strpos(codes[3],'c',0) EQ -1 THEN BEGIN
                     icodes[3]=min(where(c.band EQ codes[3]))
                     n=where( sp[b2].iband EQ icodes[3], count)
; psl[js_orig]=b2[js]
                     if count eq 0 then goto, jump2
            ;
            ; extract both antennas
            ;
            if (tel_bsl eq 'telescope') then begin
              tel1 = c.tel1[bl[b1[n]].itel1]
              tel2 = c.tel2[bl[b1[n]].itel2]
              atel1 = tel1(uniq(tel1))
              atel2 = tel2(uniq(tel2))
            endif

;            inhid_beg=min([in(pil).inhid]) & inhid_end=max([in(pil).inhid])
;
; get and apply gain solution to int_ave and rec and channel data
;
            FOR j=0,n_elements(passband)-1 DO BEGIN

;               xs=0.
;               dat_get_rows,tmp_cmp,tmp_amp, tmp_pha, tmp_x,tmp_wt,tmp_pt_first,tmp_pt_npts,x_var, js[0]
;               xs = tmp_x
               case x_var of
                  'channel'  : xs=findgen(sp[b2[n[0]]].nch)+1.
                  'fsky'     : xs=sp[b2[n[0]]].fsky+0.001* $
                    (findgen(sp[b2[n[0]]].nch)+ $
                    (1.-float(sp[b2[n[0]]].nch))/2.)*sp[b2[n[0]]].fres
                  'velocity' : xs=sp[b2[n[0]]].vel+ $
                    (findgen(sp[b2[n[0]]].nch)+ $
                    (1.-float(sp[b2[n[0]]].nch))/2.)*sp[b2[n[0]]].vres
                  else :
               endcase
                  
               if (tel_bsl eq 'baseline') then begin

                  result=cal_store(s_c,'pass',x_var,passband[j],tel_bsl,inhid_beg, $
                                inhid_end,codes,icodes,xs,ys,0.0,irow,/restore)
                  if (result le 0) then begin
                  print,'Errors occured when restoring gain curves for codes ',codes
                     return, -1
                  endif
               endif

               if (tel_bsl eq 'telescope') then begin
                  ;
                  ; loading the first antenna gains
                  ;
                  codes[0]=atel1
                  icodes[0]=poldualrx[0]

                  result=cal_store(s_c,'pass',x_var,passband[j],tel_bsl,inhid_beg, $
                                inhid_end,codes,icodes,xs,ys1,0.0,irow,/restore)

                  if (result le 0) then begin
                    print,'Errors occured when restoring gain curves for codes ',codes
                     return, -1
                  endif

                  ;
                  ; loading the second antenna gains
                  ;
                  codes[0]=atel2
                  icodes[0]=poldualrx[1]

                  result=cal_store(s_c,'pass',x_var,passband[j],tel_bsl,inhid_beg, $
                                inhid_end,codes,icodes,xs,ys2,0.0,irow,/restore)

                  if (result le 0) then begin
                    print,'Errors occured when restoring gain curves for codes ',codes
                    return, -1
                  endif

                  cur_nchs1 = n_elements(ys1)/2.0
                  cur_nchs2 = n_elements(ys2)/2.0

                  if (cur_nchs1 ne cur_nchs2) then begin
                    print,'Un-matching channel numbers in different ant-based gains'
                    return, -1
                  endif

                  ;
                  ; combining two antenna gains to a baseline gain
                  ;
                  CASE passband[j] of
                       'amp': begin
                                 ys = ys1 * ys2
                              end
                       'pha': begin
                                 ys = ys1 - ys2
                              end
                       'amp,pha': begin
                                 ys=make_array(cur_nchs1 * 2,/float)
                                 ys[0:cur_nchs1] = ys1[0:cur_nchs1]* ys2[0:cur_nchs1]
                                 ys[cur_nchs1:2*cur_nchs1-1L] = $
                                      ys1[cur_nchs1:2*cur_nchs1-1L] -  ys2[cur_nchs1:2*cur_nchs2-1L]
                              end
                       'complex': begin
                                 ys = ys1 * conj(ys2)
                              end
                  ENDCASE

               endif

               IF result eq 1 THEN BEGIN
                  CASE passband[j] OF
                      'amp,pha' : BEGIN
                                   uti_conv_apc,passband_cmp,ys[0:n_elements(ys)/2-1], $
                                      ys[n_elements(ys)/2:n_elements(ys)-1],/complex
                                 END
                     'complex' : BEGIN
                                    passband_cmp=ys
                                 END
                      'pha'     : BEGIN
                                   uti_conv_apc,passband_cmp,fltarr(n_elements(ys))+1,ys,/complex
                                END
                      'amp'     : BEGIN
                                   uti_conv_apc, passband_cmp,ys,fltarr(n_elements(ys)),/complex
                                END                      
                     ELSE: BEGIN 
                              print,'*** ',passband[j],' invalid passband type !' &     
                              return,-1
                     ENDELSE
                  ENDCASE
;
; set the correction factors for the channel and record data
;
                  FOR k=0,n_elements(n)-1L DO BEGIN
                     is=lindgen(sp[b2[n[k]]].nch,sp[b2[n[k]]].nrec)/sp[b2[n[k]]].nrec
                     ch[b3[n[k]]:b4[n[k]]]= $
                       ch[b3[n[k]]:b4[n[k]]]/passband_cmp[is]
                  ENDFOR
               ENDIF

            ENDFOR

         ENDIF ; all spectral bands

         jump2:
      ENDFOR                    ; bands
      jump1:
      icodes[0]=icodes0 
   ENDFOR                       ; sbs
ENDFOR                          ; recs
ENDFOR                          ; baselines


;      pcals = 0 & distinct_pcals = 0  ; to free up memory

   ENDIF
;
; ****************** water line monitor **************************
;
   IF keyword_set(wlm) THEN BEGIN
     ; Full-phase or coherence-only corrections?
       icoherence = 1
       if strmid(strupcase(wlm),0,3) eq 'PHA' then icoherence = 0
 
     ; Set up pointer arrays for indexes in the record header structure
       npts= TOTAL(sp[psl].nrec*sp[psl].nch)
       pcl_end=pcl+sp[psl].nrec*sp[psl].nch-1L
       prl_end=prl+sp[psl].nrec-1L
 
     ; Determine WLM corrections for each record/channel
       result = wlm_apply(phase_corr,coherence=icoherence,xfactor=xfactor,$
                          mklog=mklog)
       if (result ne 1) then begin
          printf,-1,"Error setting WLM phase corrections"
          return,-1
       endif
 
     ; Convert to complex
       amp_corr = replicate(1.0,npts)
       uti_conv_apc,wlm_cmp,amp_corr,phase_corr,/complex

     ; Multiply cal array by complex phase correction
       cal_cmp = cal_cmp * wlm_cmp
 
     ; Print phases/WLM data to file
       if keyword_set(mklog) then begin
          printf,-1,"--- Printing phase corrections to a file"
          wlm_print_phases,phase_corr,wlm_cmp,coherence=icoherence
       endif

     ; Message
       printf,-1,"--- Updating amplitude/phase averages"

     ; For continuum data, we update the amplitudes/phases averages
       jcontinuum = where(bands eq 'c1' or bands eq 'c2',ncont)
       coh_before = bl.coh
       amp_before = bl.ampave
       pha_before = bl.phaave
       iuse       = intarr(n_elements(pil))
 
     ; Compute new amplitude and phase averages for each continuum channel
       for i = 0L, ncont-1L do begin
          ; Set ID number to pil/psl/pbl pointers
            jcont = jcontinuum[i]
            pblj  = pbl[jcont]
 
          ; Set beginning and ending channels/records
            pc1 = pcl[jcont]
            pc2 = pcl_end[jcont]
            nch = pc2-pc1+1
            pr1 = prl[jcont]
            pr2 = prl_end[jcont]
            nstart = 0L
            if (i gt 0) then $
              nstart = long(TOTAL(sp[psl[0L:jcont-1L]].nrec*sp[psl[0L:jcont-1]].nch))
            nend = nstart + nch - 1L
 
          ; Set weights
            wts = re.wts(pr1:pr2)
            j = where(wts ne 0,nj)
            if (nj gt 0) then begin
              ; Get data
                wts = ABS(wts[j])
                tot_wts = total(wts)
                data = ch[pc1:pc2]
                corrections = cal_cmp[nstart:nend]
                data_cor = data(j) / corrections(j)
                k = where(phase_corr[nstart:nend] ne 0.0,nk)
                iuse[i] = nk

              ; Determine vector average phase/amplitude for integration
                mean_cmp = total(wts*data_cor) / tot_wts
                uti_conv_apc,mean_cmp,vec_amp_avg,vec_pha_avg,/amp_pha
                vec_pha_avg = uti_pha_180(vec_pha_avg) + 360.*(vec_pha_avg lt 0)
 
              ; Determine average scalar phase/amplitude
                uti_conv_apc,data_cor,amp_cor,pha_cor,/amp_pha
                sca_amp_avg = total(wts*amp_cor) / tot_wts
                sca_pha_avg = total(wts*pha_cor) / tot_wts

              ; Determine Rice correction factor
                amp_squared = total(wts*amp_cor^2) / tot_wts
                noise  = re.noises[pr1:pr2]
                noise2 = total(wts^2*(noise[j]/1000.0)^2) / total(wts^2)
                factor = amp_squared - 2.0*noise2

              ; Compute coherence 
                coh = 0.0
                if (factor gt 0.0) then $
                   coh = MIN([1.0,vec_amp_avg/sqrt(factor)])
 
              ; Set new amplitude/phase/coherence averages
                bl[pblj].ampave = vec_amp_avg
                bl[pblj].phaave = vec_pha_avg
                bl[pblj].coh    = coh
 ; print,coh_before[pblj],bl[pblj].coh,pha_before[pblj],bl[pblj].phaave
            endif
       endfor

     ; Print results to a data file
       if (keyword_set(mklog) and xfactor ge 0.0) then begin
         ext = '_' + string(fix(xfactor)) + '_'
         if (xfactor lt 10) then ext = '_0' + string(fix(xfactor)) + '_'
         if (icoherence) then ext = ext + "coh.dat" else ext = ext + "pha.dat"
         output = strarr(2,2)
         iout   = intarr(2,2)
         output[0,0] = 'track' + string(in(0).traid) + '_3mm_lsb' + ext
         output[0,1] = 'track' + string(in(0).traid) + '_3mm_usb' + ext
         output[1,0] = 'track' + string(in(0).traid) + '_1mm_lsb' + ext
         output[1,1] = 'track' + string(in(0).traid) + '_1mm_usb' + ext
         for irec = 0,1 do begin
         for isb  = 0,1 do begin
            openw,unit,strcompress(output[irec,isb],/remove),/get_lun
            iout[irec,isb] = unit
            printf,iout[irec,isb],"# SCAN   BSL      SOURCE    EL   AMP_I  AMP_F   DIFF   COH_I COH_A     DIFF   PHA_I   PHA_F    DIFF   PRBL"
           printf,iout[irec,isb],"#  (1)  (2-3)       (4)    (5)    (6)    (7)     (8)    (9)  (10)      (11)    (12)    (13)    (14)   (15)"
         endfor
         endfor
         for j = 0L, ncont-1L do begin
            i = jcontinuum[j]
            if ( (iuse[j] gt 0 or xfactor eq 0.0) and in(pil[i]).int mod 2 eq 0) then begin
              source = c.source[in(pil[i]).isource]
              coh_final = bl(pbl[i]).coh
              amp_final = bl(pbl[i]).ampave
              pha_final = bl(pbl[i]).phaave
              coh_init  = coh_before(pbl[i])
              amp_init  = amp_before(pbl[i])
              pha_init  = pha_before(pbl[i])
              pha_diff  = uti_pha_180(pha_final-pha_init)
              tel1 = c.tel1[bl(pbl[i]).itel1]
              tel2 = c.tel2[bl(pbl[i]).itel2]
              printf,iout[bl(pbl[i]).irec,bl(pbl[i]).isb],$
                  format='(I6,3x,a1,1x,a1,A12,2x,F5.1,3(1x,F6.3),2x,2(1x,F5.3),2x,F7.3,3(1x,F7.1),1x,F8.3)',$
                  in(pil[i]).int,tel1,tel2,source,in(pil[i]).el,$
                  amp_init,amp_final,amp_final-amp_init,$
                  coh_init,coh_final,coh_final-coh_init,$
                  pha_init,pha_final,pha_diff,bl(pbl[i]).prbl
            endif
         endfor
         for irec = 0,1 do begin
         for isb  = 0,1 do begin
            close,iout[irec,isb]
            free_lun,iout[irec,isb]
         endfor
         endfor
       endif
   ENDIF


 IF keyword_set(wlm) THEN BEGIN
;
; apply calibration
;
   ch=ch/cal_cmp
;
 ENDIF

   RETURN,1
;
   apply_error:
   print,'error in applying '
   RETURN,-1
;
END
