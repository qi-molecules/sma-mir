function flux_casa,source,radius,freq,bw,datatime,flux
;   Use CASA 6.1.2 flux models 
; Common blocks
    common global
    common data_set
    iformat=0
    case source of
       "titan":    file=e.IDL_PRO+'SolarSystemModels/Titan_Tb.dat'
       "callisto": file=e.IDL_PRO+'SolarSystemModels/Callisto_Tb.dat'
       "ceres":    begin
          file=e.IDL_PRO+'SolarSystemModels/Ceres_fd_time.dat'
          iformat=2
       end
       "europa":   file=e.IDL_PRO+'SolarSystemModels/Europa_Tb.dat'
       "ganymede": file=e.IDL_PRO+'SolarSystemModels/Ganymede_Tb.dat'
       "io":       file=e.IDL_PRO+'SolarSystemModels/Io_Tb.dat'
       "juno":     file=e.IDL_PRO+'SolarSystemModels/Juno_Tb.dat'
       "jupiter":  file=e.IDL_PRO+'SolarSystemModels/Jupiter_Tb.dat'
       "lutetia":  begin
          file=e.IDL_PRO+'SolarSystemModels/Lutetia_fd_time.dat'
          iformat=2
       end
       "mars":     begin 
          file=e.IDL_PRO+'SolarSystemModels/Mars_Tb_time.dat'
          iformat=1
       end
       "neptune":  file=e.IDL_PRO+'SolarSystemModels/Neptune_Tb.dat'
       "pallas":   begin
          file=e.IDL_PRO+'SolarSystemModels/Pallas_fd_time.dat'
          iformat=2
       end
       "uranus":   file=e.IDL_PRO+'SolarSystemModels/Uranus_Tb.dat'
       "venus":    file=e.IDL_PRO+'SolarSystemModels/Venus_Tb.dat'
       "vesta":    begin
          file=e.IDL_PRO+'SolarSystemModels/Vesta_fd_time.dat'
          iformat=2
       end
       else: begin
          print,'No CASA model for ',source
          return, -1
       endelse
    endcase

    nfreq = n_elements(freq)
    btemp=dblarr(nfreq)

    if iformat gt 0 then begin
       nlines=file_lines(file)
       line=''
       openr, unit, file, /get_lun
       readf,unit,line
       freqm=float(strsplit(line,/extract))
       nfreqm=n_elements(freqm)
       temp=dblarr(6+nfreqm,nlines-1)
       readf,unit,temp
       close, unit & free_lun, unit

       mjdm=reform(temp[5,*])
       tbm=temp[6:nfreqm-1,*]

       ; narrow down time index with +/- 5 days
       time1=datatime[0]-5.
       time2=datatime[0]+5.
       itime1=round(interpol(findgen(nlines-1),mjdm,time1))
       itime2=round(interpol(findgen(nlines-1),mjdm,time2))

       if (itime1 le 0) or (itime2 ge nlines-2) then begin
          print,'***WARNING! WARNING! ***********************'
          print,'Data taken beyond the model time range for ',source
          print,'***WARNING! WARNING! ***********************'
          return,0
       endif

       new_mjdm=mjdm[itime1:itime2]
       new_tbm=tbm[*,itime1:itime2]
       new_nlines=itime2-itime1+1
       
       for i=0, nfreq-1 do begin
          ifreq=interpol(findgen(nfreqm),freqm,freq[i])
          imjd=interpol(findgen(new_nlines),new_mjdm, datatime[i])
          btemp[i]=bilinear(new_tbm, ifreq, imjd)
       endfor

    endif else begin

       nlines=file_lines(file)
       temp=dblarr(2,nlines)
       openr, unit, file,/get_lun
       readf,unit,temp
       close, unit & free_lun,unit
       freqm=reform(temp[0,*])
       btempm=reform(temp[1,*])
       rt=freqm[1:nlines-1]-freqm[0:nlines-2]
       fintv=[rt[0],rt,rt[nlines-2]]
       finv2=(fintv[0:nlines-1]+fintv[1:nlines])/2.


       for i=0, nfreq-1 do begin
          freq1=freq[i]-abs(bw[i])/2000.
          freq2=freq[i]+abs(bw[i])/2000.
          i1=value_locate(freqm,freq1)
          i2=value_locate(freqm,freq2)
          btmp=btempm[i1:i2]
          ftmp=freqm[i1:i2]
          n=n_elements(ftmp)
          if n ge 2 then begin
             intvtmp=finv2[i1:i2]
;       btemp[i]=mean([btempm[i1:i2]])
             btemp[i]=total(btmp*intvtmp)/total(intvtmp)
          endif else begin
             btemp[i]=btmp[0]
          endelse
       endfor
       
    endelse 
;    print,btemp

    print,'***'
    ; casa 6.1.2
    print,' Using CASA6.1 flux calibration models (Butler-JPL-Horizons 2012)...'

    ; Compute flux
    ; flux = 49.5 * (temp/200.0) * (radius/3.)^2 * (freq/110.201)^2
    flux= (1.088791135e-7)*freq*freq*freq*radius*radius/(exp(0.047992*freq/btemp)-1)
    if iformat eq 2 then flux=btemp
    
    ; Done
    return,1
end
