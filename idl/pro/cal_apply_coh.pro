function cal_apply, gain=gain,passband=passband,wlm=wlm
;
; Apply calibration data to all data seen through filter.
;
; parameters : 
; keywords :  gain     -- apply gain solution ('amp','pha','amp,pha') 
;             passband -- apply passband ('amp,pha')
;             wlm      -- apply wlm corrections
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
;
;
; first figure out what data descriptors we're looking at
;
   IF dat_list(s_f,/reset,/no_notify) LE 0 THEN BEGIN
      print,'No cal application due to lack of data (check filter).'
      return,-1
   ENDIF
; if (dat_list(s_l,'"band" like "c" and "rec" like "1" and "source" eq "i22178+6317"',/reset,/no_notify) le 0) then begin
; print,"Never more"
; return,-1
; end

;
; set up array cal_cmp to contain the complex cal multiplicative
; factors to be accumulated from the gain, passband, and wlm. this
; array is initialized to amp = 1. pha = 0. in complex vis.
;
   uti_conv_apc,cmp_init,1.,0.,/complex
   cal_cmp=make_array(n_elements(ch),/complex,value=cmp_init)
;
; segment the data by blcd, receiver, sb, and band
;
   bls=c.blcd[bl[pbl].iblcd]
   recs=c.rec[bl[pbl].irec]
   sbs=c.sb[bl[pbl].isb]
   bands=c.band[sp[psl].iband]
;
; ****************** GAINS **********************************
;
   IF keyword_set(gain) THEN BEGIN
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
      distinct_gcals=uti_distinct(gcals,ngcals,/many)
      tel_bsl='baseline' & x_var='int'
      FOR i=0,ngcals-1 DO BEGIN
         result=dat_comb_sep(distinct_gcals[i],['blcd','rec','sb'],codes, $
                             icodes,n_components)
         js=where(distinct_gcals[i] eq gcals,count_js)
         inhid_beg=min([in(pil).inhid]) & inhid_end=max([in(pil).inhid])
         xs=float([in[pil[js]].int])
;
; get and apply gain solution to int_ave and rec and channel data
;
         FOR j=0,n_elements(gain)-1 DO BEGIN
         result=cal_store(s_c,'gain',x_var,gain[j],tel_bsl,inhid_beg, $
                          inhid_end,codes,icodes,xs,ys,0.0,irow,/restore)
            IF result EQ 1 THEN BEGIN
               CASE gain[j] OF
                  'amp' : BEGIN
                             IF count_js GT 1 THEN bl[pbl[js]].ampave= $
                                                      bl[pbl[js]].ampave/ys
                             IF count_js EQ 1 THEN bl[pbl[js]].ampave= $
                                                      bl[pbl[js]].ampave/ys[0]
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
                                 IF count_js GT 1 THEN bl[pbl[js]].ampave= $
                                         bl[pbl[js]].ampave/ys[0:count_js-1]
                                 IF count_js GT 1 THEN bl[pbl[js]].phaave= $ 
                                           uti_pha_180(bl[pbl[js]].phaave- $
                                              ys[count_js:2*count_js-1])
                                 IF count_js EQ 1 THEN BEGIN
                                    amp_cor=ys[0] 
                                    pha_cor=ys[1]
                                    bl[pbl[js]].ampave=bl[pbl[js]].ampave/ $
                                                       amp_cor
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
               FOR k=0,n_elements(js)-1L DO BEGIN
                  cal_cmp[pcl[js[k]]:pcl_end[js[k]]]= $
                  cal_cmp[pcl[js[k]]:pcl_end[js[k]]]*gain_cmp[k]
               ENDFOR
            ENDIF
         ENDFOR
      ENDFOR
      gcals = 0
      distinct_gcals = 0  ; to free up memory
   ENDIF
;
; ****************** passband **********************************
;
   IF keyword_set(passband) THEN BEGIN
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
   pcals=bls+' '+recs+' '+sbs+' '+bands
   distinct_pcals=uti_distinct(pcals,npcals,/many_repeat)
   tel_bsl='baseline' & x_var='channel'
   FOR i=0,npcals-1 DO BEGIN
   result=dat_comb_sep(distinct_pcals[i],['blcd','rec','sb','band'],codes, $
                      icodes,n_components)
      IF strpos(codes[3],'c',0) EQ -1 THEN BEGIN
         js=where(distinct_pcals[i] eq pcals,count_js)
         inhid_beg=min([in(pil).inhid]) & inhid_end=max([in(pil).inhid])
;
; get and apply gain solution to int_ave and rec and channel data
;
         FOR j=0,n_elements(passband)-1 DO BEGIN
            xs=0.
            result=cal_store(s_c,'passband',x_var,passband[j],tel_bsl,inhid_beg, $
                    inhid_end,codes,icodes,xs,ys,0.0,irow,/restore)
            IF result eq 1 THEN BEGIN
               CASE passband[j] OF
                  'amp,pha' : BEGIN
                                uti_conv_apc,passband_cmp,ys[0:n_elements(ys)/2-1], $
                                   ys[n_elements(ys)/2:n_elements(ys)-1],/complex
                              END
                  'complex' : BEGIN
                                 passband_cmp=ys
                              END
                  ELSE: BEGIN 
                           print,'*** ',passband[j],' invalid passband type !' &     
                           return,-1
                  ENDELSE
               ENDCASE
;
; set the correction factors for the channel and record data
;
               is=lindgen(sp[psl[js[0]]].nch,sp[psl[js[0]]].nrec)/sp[psl[js[0]]].nrec
               FOR k=0,n_elements(js)-1L DO BEGIN
                  cal_cmp[pcl[js[k]]:pcl_end[js[k]]]= $
                     cal_cmp[pcl[js[k]]:pcl_end[js[k]]]*passband_cmp[is]
               ENDFOR
            ENDIF
         ENDFOR
      ENDIF
   ENDFOR
   pcals = 0 & distinct_pcals = 0  ; to free up memory
   ENDIF
;
; ****************** water line monitor **************************
;
   IF keyword_set(wlm) THEN BEGIN
     ; Set up pointer arrays for indexes in the record header structure
       npts= TOTAL(sp[psl].nrec*sp[psl].nch)
       pcl_end=pcl+sp[psl].nrec*sp[psl].nch-1L
       prl_end=prl+sp[psl].nrec-1L

     ; Get the conversion factor from the CAL stucture
       inhid_beg = in[pil(0)].traid
       inhid_end = in[pil(0)].traid
       x_var = "hours"
       y_var = "pha"
       tel_bsl = "baseline"
       codes = ["all","all","all","all"]
       icodes = [1,1,1,1]
       result = cal_store(s_c,'wlm',x_var,y_var,tel_bsl,inhid_beg,inhid_end,$
                        codes,icodes,track_number,xfactor,junk,irow,/restore)
       if (result ne 1) then begin
          printf,-1,"Could not read WLM SCALE FACTOR from CAL structure."
          return,-1
       endif

     ; Determine WLM corrections for each record/channel
       phase_corr = fltarr(npts)
       result = wlm_correlate(phase_corr,xfactor,/get_corrections)
       if (result ne 1) then begin
          printf,-1,"Error setting WLM phase corrections"
          return,-1
       endif

     ; Convert to complex
       amp_corr = make_array(npts,/float,value=1.)
       uti_conv_apc,wlm_cmp,amp_corr,phase_corr,/complex

     ; Multiply cal array by complex phase correction
       cal_cmp = cal_cmp * wlm_cmp

     ; For continuum data, we update the amplitudes/phases averages
       jcont = where(bands eq 'c1' or bands eq 'c2',ncont)

     ; Compute new amplitude and phase averages for each continuum channel
       n = 0L
       for i = 0L, ncont-1L do begin
          ; Set ID number to pil/psl/pbl pointers
            j = jcont[i]
            pblj = pbl[j]

          ; Set beginning and ending channels/records
            pc1 = pcl[j]
            pc2 = pcl_end[j]
            nch = pc2-pc1+1
            pr1 = prl[j]
            pr2 = prl_end[j]
            nstart = 0L
            if (i gt 0) then $
               nstart = long(TOTAL(sp[psl[0:j-1]].nrec*sp[psl[0:j-1]].nch))
            nend = nstart + nch - 1L

          ; Multiply complex records by phase corrections
            wt = re.wts(pr1:pr2)
            tot_wt = total(wt) 
            if (tot_wt le 0.0) then printf,-1,"WEIGHTS=0.0 in CAL_APPLY"
            if (tot_wt eq 0.0) then tot_wt = 1.0
            data_cor = ch[pc1:pc2] / cal_cmp[nstart:nend]

          ; Determine vector average phase/amplitude for integration
            mean_cmp = total(wt*data_cor) / tot_wt
            uti_conv_apc,mean_cmp,vec_amp_avg,vec_pha_avg,/amp_pha

          ; Determine average scalar phase/amplitude
            uti_conv_apc,data_cor,amp_cor,pha_cor,/amp_pha
            sca_amp_avg = total(wt*amp_cor) / tot_wt
            sca_pha_avg = total(wt*pha_cor) / tot_wt

          ; Set new amplitude/phase/coherence averages
            bl[pblj].ampave = vec_amp_avg
            bl[pblj].phaave = vec_pha_avg
            bl[pblj].coh    = vec_amp_avg/sca_amp_avg
       endfor
   ENDIF
;
; apply calibration
;
   ch=ch/cal_cmp
;
   RETURN,1
;
   apply_error:
   print,'error in applying '
   RETURN,-1
;
END
