function flux_primary,source,radius,freq,flux
; Qi update for mjd calculation
    ; Common blocks
      common global
      common data_set

    ; Set primary flux standards
      PLANETS   = ["mercury", "venus", "mars", "jupiter", "saturn", "uranus",$
                   "neptune","pluto","callisto","ganymede","titan"]
      SIZES     = [4.045480E-5,2.263183E-5,4.615285E-4,3.833842E-4,$
                   1.695909E-4,1.623777E-4,1.629E-5,   4.653E-3]
      REST_FREQ = [31.4, 90.0, 112.8, 150.0, 227.0, 279.0, 337.0, 392.0, 808.0]
      BTEMP     = [[500., 500., 500., 500., 500., 500., 500., 500., 500.],$
                   [466., 367., 339., 294., 294., 294., 294., 294., 294.],$
                   [194., 207., 209., 212., 214., 216., 217., 217., 221.],$
                   [152., 172., 172., 172., 172., 172., 172., 162., 144.],$
                   [133., 149., 145., 137., 136., 136., 135., 136., 115.],$
; old mma uranus value   [136., 136., 122., 112., 93.7, 91.8, 87.8, 83.7, 64.2],$
                   [139.7,128.6,123.1,119.3,99.5,92.2,86.1,81.6,66.3],$ 
; old mma neptune value  [126.7, 126.7, 114.3, 110.3, 91.9, 85.4, 84.1, 78.7, 61.0],$
                   [135.2,125.6,118.0,112.7,91.1,90.4,80.7,79.0,55.5],$
                   [38.,  38.,  38.,  38.,  38.,  38.,  38.,  38.,  38.],$
                   [120., 120., 120., 120., 120., 120., 120., 120., 120.],$
                   [100., 100., 100., 100., 100., 100., 100., 100., 100.],$
                   [77., 77., 77., 77., 77., 77., 77., 77., 77.]]


    ; Initialize 
      nfreq = n_elements(freq)
      flux = fltarr(nfreq)

    ; Find source in the planet list
      j = where(source eq PLANETS,nj)
      if (nj eq 0) then return,-1

    ; Use linear interpolation to find brightness temperature at
    ; the observed frequency
      temp = fltarr(nfreq)

      if source eq "titan" then begin
            bw=abs(sp[psl[0]].fres)
            
            filename=e.IDL_PRO+'titan.tb.spec'
            text=''
;            nx=4
            ny=9401
;            data=fltarr(nx+1,ny)
            openr,lun,filename,/get_lun
            readf,lun,text
            readf,lun,text
            readf,lun,text
            text0=strsplit(text,/extract)
            nx=n_elements(text0)-1
            x=float(text0[1:nx])
            data=fltarr(nx+1,ny)
            readf,lun,data
            free_lun,lun
            tb=data[1:nx,*]
;            x=[3936.,1968.,984.,492.]
            y=data[0,*]
         endif

      for i=0L, nfreq-1L do begin
         temp[i] = uti_interp(BTEMP[*,j],REST_FREQ,freq[i])
         if source eq "titan" then begin
            ix0=interpol(findgen(nx),x,bw)
            iy0=interpol(findgen(ny),y,freq[i])
            temp[i]=bilinear(tb,ix0,iy0)
         endif

         if source eq "uranus" then temp[i]=-795.694+845.179*alog10(!cvel/(1000.*freq[i])) $
           -288.946*alog10(!cvel/(1000.*freq[i]))^2+35.2*alog10(!cvel/(1000.*freq[i]))^3
        if source eq "neptune" then temp[i]=-598.901+655.681*alog10(!cvel/(1000.*freq[i])) $
           -229.545*(alog10(!cvel/(1000.*freq[i])))^2+28.994*(alog10(!cvel/(1000.*freq[i])))^3
      endfor

    ; Calculate the second order correction for the Mars temperature
    ; based on the heliocentric position
      if (strupcase(source) eq 'MAR') then begin
         months = ['Jan','Feb','Mar','Apr','May','Jun','Jul', $
          'Aug','Sep','Oct','Nov','Dec']
         datobs=c.ref_time[in[pil[0]].iref_time]
         mon=strmid(datobs,0,3)
         j=where(mon eq months,count)
         if count le 0 then begin
            print,"couldn't decode UT date in data (",c.ref_time[in[pil[0]].iref_time],") !"
            return,-1
         endif
         num_day_obs=[fix(strtrim(strmid(datobs,8,4),2)), $
            fix(strtrim(string(j[0]+1),2)),(strtrim(strmid(datobs,4,2),2))]
         day=num_day_obs[2]
         year=num_day_obs[0]
         month=num_day_obs[1]
         mjd=uti_date2mjd(year,month,day)
         t=(mjd-15019.5)/36525.
;         flm=amod(360*53*t,360.)
         flm=360*53*t mod 360.
         flm=flm+(1.1184*t+222117.33)*t/3600.0
;         flm=amod(flm+293.747628,360.)
         flm=(flm+293.747628) mod 360.
         ameanl=flm
         anomaly = (ameanl - 335.8)*!DPI/180.0
         ell = 0.0934
         arad = 1.5237
         truanom = anomaly + (2.0*ell-(ell^3)/4.0)*sin(anomaly) + $
                     5.0*ell*ell/4.0 * sin(2.0*anomaly)
         trurad = arad*(1.0 - ell*ell)/(1.0 + ell*cos(truanom))
         temp = temp * sqrt(1.524/trurad)
      endif

    ; Compute flux
;      flux = 49.5 * (temp/200.0) * (radius/3.)^2 * (freq/110.201)^2
      flux= (1.088791135e-7)*freq*freq*freq*radius*radius/(exp(0.047992*freq/temp)-1)

    ; Done
      return,1
end
