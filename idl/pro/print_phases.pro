pro print_phases,phase_corr,cal_cmp,coherence=coherence
   ; Common blocks
     common global
     common data_set
     common wlm

   ; Initialize output files
     ntel = !NTEL
     output = strarr(ntel+1,ntel+1)
     ext = "_pha"
     if keyword_set(coherence) then ext = "_coh"
     for i = 1L,   ntel do begin
     for j = i+1L, ntel do begin
       if (i lt j) then begin
         output[i,j] = "track" + string(in(0).traid) + "_" + string(i) + $
                        "-" + string(j) + ext + ".dat"
         output[i,j] = strcompress(output[i,j],/remove)
         openw,unit,output[i,j],/get_lun
         close,unit
         free_lun,unit
       endif
     endfor
     endfor

   ; Symbol styles
     pstyle = [16, 6, 5, 2, 8, 9]

   ; Set up pointer arrays for indexes in the record header structure
     npts= TOTAL(sp[psl].nrec*sp[psl].nch)
     pcl_end=pcl+sp[psl].nrec*sp[psl].nch-1L
     prl_end=prl+sp[psl].nrec-1L

   ; Loop over all possible combinations
     for i = 0L, n_elements(pil)-1L do begin
       if (bl(pbl(i)).irec eq 0 and bl(pbl(i)).isb eq 0) then begin
         ; Get baseline number
           tel1 = c.tel1[bl[pbl(i)].itel1]
           tel2 = c.tel2[bl[pbl(i)].itel2]
           itel1 = fix(tel1)
           itel2 = fix(tel2)

         ; Open output file
           openw,unit,output[itel1,itel2],/get_lun,/append

         ; Set beginning and ending channels/records
           pc1 = pcl[i]
           pc2 = pcl_end[i]
           nch = pc2-pc1+1
           pr1 = prl[i]
           pr2 = prl_end[i]
           nstart = 0L
           if (i gt 0) then $
             nstart = long(TOTAL(sp[psl[0L:i-1L]].nrec*sp[psl[0L:i-1]].nch))
           nend = nstart + nch - 1L

         ; Set weights
           wts = re.wts(pr1:pr2)
           j = where(wts ne 0,nj)
           if (nj gt 0) then begin
             ; Get data
               wts = ABS(wts[j])
               tot_wts = total(wts)
               data = ch[pc1:pc2]
               cor_complex = cal_cmp[nstart:nend]
               cor_deg = phase_corr[nstart:nend]
               data_cor = data(j) / cor_complex(j)

             ; Determine phases before/after corrections
               uti_conv_apc,data,amp_bef,pha_bef,/amp_pha
               uti_conv_apc,data_cor,amp_aft,pha_aft,/amp_pha

             ; Unwrap phases
               result = uti_pha_unwrap(pha_bef)
               result = uti_pha_unwrap(pha_aft)

             ; Set time stamps
               times = in[pil(i)].dhrs + $
                       re.toffs(prl[i]:prl_end[i])/3600.0
               times = times(j)

             ; Print phases
               for k = 0L, nj-1L do begin
                  printf,unit,format='(F12.6,1x,3(1x,F12.6),I4,1x,a10)',$
                    times[k],pha_bef(k),cor_deg(j(k)),pha_aft(k),$
                    pstyle[in(pil[i]).isource],c.source[in(pil[i]).isource]
               endfor
           endif

         ; Close output file
           close,unit
           free_lun,unit
       endif
     endfor
end
