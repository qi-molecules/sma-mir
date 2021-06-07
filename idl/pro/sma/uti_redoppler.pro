pro uti_redoppler, source=source, vlsr=vlsr, flipsign=flipsign, verbose=verbose
;+
; NAME:
;      uti_redoppler
; PURPOSE:
;      to apply Doppler tracking corrections
; EXPLANATION:
;      1. For MIR data version v4 and later. 
;      2. Need source name input.
;      3. Use keyword VERBOSE to print out the radial velocity corrections.
; Example:
;      IDL> uti_redoppler, source='mwc349a'
;      IDL> uti_redoppler, source='mwc349a', /verbose
; HISTORY:
;      Chunhua Qi     May-2021 
common global
common data_set

if c.filever[0] lt 3 then begin
   print,'MIR version earlier than v3 ... '
   print,'Quit !'
   return
endif else begin
   if not keyword_set(source) then begin
      print,'Use keyword SOURCE ... '
      print,'Quit !'
      return
   endif else begin
      result=dat_filter(s_f,'"source" eq "'+source+'" and "wt" gt "0"',/no_notify,/reset)
      if result le 0 then begin
         print,'No source ',source,' found in the data ...'
         print,'Quit !'
         return
      endif else begin
         globalRA=in[pif[0]].ara
         globalDec=in[pif[0]].adec
      endelse
   endelse
endelse

; unitVector
sladcs2c,globalRA,globalDec,unitVector


; vlsr from DSM_AS_IFLO_VELO_D

if not keyword_set(vlsr) then globalCatalogVelocity=sp[psf[0]].vradcat else globalCatalogVelocity=vlsr
print,'Source Catalog velocity (m/s) is ',globalCatalogVelocity

; set up parameters
sign=1.
if keyword_set(flipsign) then sign=-1.
SECONDS_PER_DAY=24.*60.*60.
globalRefLong=155.477522d*!dpi/180.d
globalRefLat=19.82420526391d*!dpi/180.d
globalRefRadius=6382.248*1000.d
M_PER_KM=1000.d
longitude=-155.477522d


; mJD
datobs=c.ref_time[in[0].iref_time]
months = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
mon=strmid(datobs,0,3)
j=where(mon eq months,count)
num_day_obs=[fix(strtrim(strmid(datobs,8,4),2)),fix(strtrim(string(j[0]+1),2)),(strtrim(strmid(datobs,4,2),2))]
day=strtrim(string(num_day_obs[2]),2)
yr =strtrim(string(num_day_obs[0]),2)
mo =strtrim(string(num_day_obs[1]),2)
mJD=uti_date2mjd(yr,mo,day)

; start with separate rx

distinct_rec=c.rec[uti_distinct(bl.irec,nrec,/many)]
for i=0, nrec-1 do begin
    result=dat_filter(s_f,'"source" eq "'+source+'" and "rec" eq "'+distinct_rec[i]+'" and "wt" gt "0"',/reset,/no_notify)
    if result le 0 then return
    crx=distinct_rec[i]
    print, 'Working on receiver: ',crx,+'...'

    print, '** using only subbands starting with "s"'
   
    result=dat_filter(s_f,'"band" like "s"',/no_notify)

    sbs=strupcase(c.sb(bl[pbf].isb))
    bands=strupcase(c.band(sp[psf].iband))
    recs=c.rec(bl(pbf).irec)
    bls=c.blcd(bl(pbf).iblcd)
    combinations=bls+' '+recs+' '+sbs+' '+bands
    distinct_combinations=uti_distinct(combinations,ncombinations,/many_repeat)
   
    ints = in[pif].int
    pcfs = pcf
    pifs = pif
    psfs = psf

    ii=uti_distinct(in[pif].int,nint,/many_repeat)

;; Here we loop over all integrations, performing a cubic interpolation
; of the visibility spectra
    for i=0L,nint-1L do begin

        tmp_idx = where(ints eq ii[i], ncombo)
        ;print,'Fixing integration ',ii[i], " which has ", ncombo, " combinations"

; now figure out velocity correction !

      ; dayFraction for the time of source transit (DFT)
        lst = in[pifs[[tmp_idx[0]]]].lst
        lstRad=lst*15.*!dpi/180.d
        ut  = in[pifs[[tmp_idx[0]]]].dhrs
        dayFraction=ut/24.
        ha=lst-globalRA*180.d/!dpi/15.d
        haRad=ha*15.d*!dpi/180.d
        sidereal_rate=double(86400.)/double(86164.)
        dayFraction0=dayFraction-haRad/(sidereal_rate*2.d*!dpi)
        tTMinusUTC = slaDtt(mJD+dayFraction)/SECONDS_PER_DAY
        tTMinusUTC0 = slaDtt(mJD+dayFraction0)/SECONDS_PER_DAY
      ;print,'tTMinusUTC = ',tTMinusUTC
        tT = mJD+dayFraction + tTMinusUTC
        tT0 = mJD+dayFraction0 + tTMinusUTC0

      ; Convert to Barycentric Dynamical Time
      ; slaRcc wants *west* longitude
        tDBMinusTT = slaRcc(mJD, dayFraction, globalRefLong, cos(globalRefLat)*globalRefRadius/M_PER_KM,sin(globalRefLat)*globalRefRadius/M_PER_KM)/SECONDS_PER_DAY
        tDBMinusTT0 = slaRcc(mJD, dayFraction0, globalRefLong, cos(globalRefLat)*globalRefRadius/M_PER_KM,sin(globalRefLat)*globalRefRadius/M_PER_KM)/SECONDS_PER_DAY
      ;print,'tDBMinusTT: ',tDBMinusTT
        tDB = tT + tDBMinusTT 
        tDB0 = tT0 + tDBMinusTT0
      ;print,'tDB: ',tDB

      ; Now get Heliocentric vector velocity of earth
        slaEvp, tDB, double(2000.0), dummy1, dummy2, dVH, dummy3
        slaEvp, tDB0, double(2000.0), dummy1, dummy2, dVH0, dummy3
        ASTRONOMICAL_UNIT=double(1.4959787e11) ;m 
; Now form the dot product of the unit vector and the velocity vector 
        earthOrbitalVelocity = slaDvdv(unitVector, dVH) * ASTRONOMICAL_UNIT
        earthOrbitalVelocity0 = slaDvdv(unitVector, dVH0) * ASTRONOMICAL_UNIT
        ; print,'earthOrbitalVelocity = ',earthOrbitalVelocity ; m/s ?
        ; print,'earthOrbitalVelocity at transit= ',earthOrbitalVelocity0

;-----the SUN's LSR velocity in the direction of the source
        kinematicLSRCorrection=slaRvlsrk(globalRA, globalDec)*M_PER_KM
;print,'kinematicLSRCorrection = ',kinematicLSRCorrection

;-----Earth Rotation
        earthRoto = slaRverot(globalRefLat, globalRA, globalDec, lstRad) * 1.0e3

; Radial velocity
        vRad0 = globalCatalogVelocity-earthOrbitalvelocity0+kinematicLSRCorrection
        ;print,'vRad (transit) = ',vRad0
        vRad=globalCatalogVelocity-earthOrbitalvelocity+kinematicLSRCorrection+earthRoto
        if keyword_set(verbose) then begin
             print,'Fixing integration ',ii[i], " which has ", ncombo, " combinations ..."
             print,'  Radial velocity correction  = ',vRad,' m/s at lst',sixty(lstRad*180.d/!dpi/15.)
             ;print,'vRad = ',vRad,' at lst',sixty(lstRad*180.d/!dpi/15.)
        endif


        for j=0L,ncombo-1L do begin
            nc=sp[psfs[[tmp_idx[j]]]].nch
            my_fres_hz = sp[psfs[[tmp_idx[j]]]].fres * 1000000
            my_fsky = sp[psfs[[tmp_idx[j]]]].fsky *1e9
            
            fsh = sign*my_fsky*(vRad/299792458.0D)/my_fres_hz
            data_ch=ch[pcfs[[tmp_idx[j]]]:pcfs[[tmp_idx[j]]]+nc-1]
                        
            data_out=interpolate(data_ch,findgen(nc)-fsh[0], cubic=-0.5)
            ch[pcfs[[tmp_idx[j]]]:pcfs[[tmp_idx[j]]]+nc-1]=data_out
        endfor
;   print,'fsh is ',fsh
;   read,iii
    endfor

endfor

end

