; baseline-based versionS - much faster than antennas based solution
function reduce_tau,avetsys_zen=avetsys_zen,avefreq=avefreq,$
                    noprint=noprint,unit=unit
   ; Common blocks
     common global
     common data_set

   ; Reset filter
     if (dat_select(s_s,/reset) eq 0) then return,-1

   ; Bands
     iband = uti_distinct(sp(pss).iband,nbands)

   ; Sidebands
     isb = uti_distinct(bl(pbs).isb,nsb)
     
   ; Baselines
     nbls = n_elements(c.blcd)

   ; Used to compute tau
     trec = 100.0
     tatm = 300.0
     nl = 0.8
     xatm = tatm * nl
     xfactor = (1.0 + trec/xatm)/nl
     airmass = 1.0/sin(in.el*!DTOR)

   ; Loop over sidebands/bands
     tau  = dblarr(nsb*nbands)
     tsys_zen = dblarr(nsb*nbands)
     tsys_ave = dblarr(nsb*nbands)
     etau = dblarr(nsb,nbands)
     freq = dblarr(nsb*nbands)
     band = strarr(nsb*nbands)
     sb   = strarr(nsb*nbands)
     n = 0
     for i=0,nbands-1 do begin
     for j=0,nsb-1 do begin
         ; Compute frequency
           command = '"source" ne "noise" and "band" eq "' + c.band[iband(i)] + $
                     '" and "sb" eq "' + c.sb[isb(j)] + '" and "wt" gt "0"'
           if dat_select(s_s,command,/reset,/no) gt 0 then begin
              ; Set parameters
                freq[n] = mean(sp(pss).fsky)
                tsys_ave[n] = mean(sp(pss).tssb)
                band[n] = c.band[iband(i)]
                sb[n] = (strlowcase(c.sb[isb(j)]) eq 'l') ? "LSB" : "USB"

              ; Estimate tau using baseline based system temeratures
                t  = [0.0]
                cnst = [0.0]
                et = [0.0]
                for k=0, nbls-1 do begin
                   l = where(bl(pbs).iblcd eq k,nl)
                   if (nl gt 2) then begin
                      x = airmass(pis(l))
                      y = -alog(xfactor / (1.0+sp(pss(l)).tssb/xatm))
                      ydistinct = uti_distinct(y,ndistinct)
                      if (ndistinct gt 2) then begin
                         result = uti_fit_robust(x,y,yfit,sig,coef_sig)
                         if n_elements(result) eq 2 then begin
                             cnst = [cnst,result[0]]
                             t  = [t,result[1]]
                             et = [et,coef_sig[1]]
                             xxx = xatm*(xfactor*exp(result[1]+result[0]) - 1.0)
                         endif
                      endif
                   endif
                endfor

              ; Compute average tau
                k = where(et gt 0,nk)
                if (nk gt 1) then begin
                   result = uti_meanvar(t(k),sig=et(k))
                   tau[n]  = result[0]
                   etau[n] = result[2]
                   result = uti_meanvar(cnst(k),sig=et(k))
                   cnst = result[0]
                endif

              ; Compute tsys at zenith
                tsys_zen[n] = (tau[n] le 0.0) ? 0.0 : $
                   xatm*(xfactor*exp(tau[n]+cnst) - 1.0)

              ; Increment counter
                n = n + 1
           endif
     endfor
     endfor

   ; Compute averages
     j = where(tsys_zen ne 0,nj)
     avetsys_zen = (nj gt 0) ? total(tsys_zen(j)) / nj : 0.0
     avefreq     = (nj gt 0) ? total(freq(j)) / nj : 0.0

   ; Print results
     if not keyword_set(noprint) then begin
        ; Sort in increasing frequency
          isort = sort(freq)

        ; Find good tau values
          j = where(etau(isort) gt 0,nj)

        ; Print only tau values
          if nj eq 0 then begin
              printlog,"Cannot make a reliable tau estimate - too few integrations",unit=unit
          endif else begin
            ; Print header
              printlog,"Atmosphere opacity and system temperatures",unit=unit
              printlog,"Band  SB  Frequency       Tau          Tssb    Tssb",unit=unit
              printlog,"            (GHz)       zenith        zenith    ave",unit=unit
              printlog,"---- ---  ---------  --------------  -------   ----",unit=unit

            ; Print results
              for k=0, n_elements(j)-1 do begin
                 i = isort(j[k])
                 printlog,format='(%" %s  %s  %8.4f  %5.2f +/- %5.2f  %5d  %7d")',$
                      band(i),sb(i),freq(i),tau(i),etau(i),tsys_zen(i),tsys_ave(i),unit=unit
              endfor
          endelse
          printlog," ",unit=unit
     endif
end
