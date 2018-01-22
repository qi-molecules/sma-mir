;yes
;=Task:IDL2MIRIAD --- To convert uv data from IDL format to MIRIAD format. 
;#Type: conversion
;+Use:
;      One can use the program IDL2MIRIAD to convert uv data from IDL format
;      to MIRIAD format. A few benefits of directly exporting SMA MIR/IDL
;      data into MIRIAD are, for example, <1> system temperatures are
;      restored in MIRIAD systemp variable, and <2> the mismatch of both
;      channel frequency and increment direction in multi-chunk data can
;      be automatically taken care of in MIRIAD at the imaging stage.
;
;@dir:
;      name of the output directory 
;@source:
;      name of the source in dataset for output
;@sideband:
;      sideband(s) for output
;@band:
;      specific band(s) for output 
;@wide_remake:  
;      re-calculate the continuum channel (wide channel 1/2) 
;      based on only the selected narrow spectral bands. 
;               (NOT IMPLEMENTED YET) 
;@edge_trim:
;      the percentage of edge channels to be trimmed of
;      on each side when making pseudo-continuum channels 
;      with the narrow spectral windows (chunks). Default edge_trim = 0.1 
;@polar:
;      polar = 0    non polarization (default)
;      polar = 1    circularly polarized feeds
;      polar = 2    linearly polarized feeds
;@verbose:  
;      more (verbose) information output during the converting process.
;      Default is to show only limited amount of info. 
;
;&history:
;------------------------------------------------------------------------
;      cykuo 18feb04 adapting the header
;------------------------------------------------------------------------

; ############# SECTIONS OF IMPORTED SUB-PROCEDURES -- START #############

PRO JULDATE, DATE, JD, PROMPT = prompt
;+                                                                  
; NAME:
;     JULDATE
; PURPOSE:                                   
;     Convert from calendar to Reduced Julian Date
;
; EXPLANATION:
;     Julian Day Number is a count of days elapsed since Greenwich mean noon 
;     on 1 January 4713 B.C.  The Julian Date is the Julian day number
;     followed by the fraction of the day elapsed since the preceding noon. 
;
;     This procedure duplicates the functionality of the JULDATE() function in
;     in the standard IDL distribution, but also allows interactive input and
;     gives output as Reduced Julian date (=JD - 2400000.)  
;     (Also note that prior to V5.1 there was a bug in JULDATE() that gave 
;     answers offset by 0.5 days.)
;
; CALLING SEQUENCE:
;     JULDATE, /PROMPT           ;Prompt for calendar Date, print Julian Date
;               or
;     JULDATE, date, jd      
;
; INPUT:
;     DATE -  3 to 6-element vector containing year,month (1-12),day, and 
;              optionally hour, minute, and second all specified as numbers
;              (Universal Time).   Year should be supplied with all digits.
;              Years B.C should be entered as negative numbers (and note that
;              Year 0 did not exist).  If Hour, minute or seconds are not 
;              supplied, they will default to 0. 
;
;  OUTPUT:
;       JD - Reduced Julian date, double precision scalar.  To convert to
;               Julian Date, add 2400000.   JULDATE will print the value of
;               JD at the terminal if less than 2 parameters are supplied, or 
;               if the /PROMPT keyword is set
;      
;  OPTIONAL INPUT KEYWORD:
;       /PROMPT - If this keyword is set and non-zero, then JULDATE will prompt
;               for the calendar date at the terminal.
;
;  RESTRICTIONS:
;       The procedure HELIO_JD can be used after JULDATE, if a heliocentric
;       Julian date is required.
;
;  EXAMPLE:
;       A date of 25-DEC-1981 06:25 UT may be expressed as either
;
;       IDL> juldate, [1981, 12, 25, 6, 25], jd       
;       IDL> juldate, [1981, 12, 25.2673611], jd 
;
;       In either case, one should obtain a Reduced Julian date of 
;       JD = 44963.7673611
;
;  PROCEDURE USED:
;       GETOPT()
;  REVISION HISTORY
;       Adapted from IUE RDAF (S. Parsons)                      8-31-87
;       Algorithm from Sky and Telescope April 1981   
;       Added /PROMPT keyword, W. Landsman    September 1992
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Make negative years correspond to B.C. (no year 0), work for year 1582
;       Disallow 2 digit years.    W. Landsman    March 2000
;-
 On_error,2 

 if ( N_params() EQ 0 ) and (not keyword_set( PROMPT ) ) then begin
     print,'Syntax - JULDATE, date, jd          or JULDATE, /PROMPT'
     print, $
     '  date - 3-6 element vector containing [year,month,day,hour,minute,sec]'
     print,'  jd - output reduced julian date (double precision)'
     return
 endif

 if ( N_elements(date) EQ 0 ) then begin   

    opt = ''                                                          
    rd: read,' Enter Year,Month,Day,Hour, Minute, Seconds (All Numeric): ',opt
    date = getopt( opt, 'F' )

 endif

 case N_elements(date) of      

    6: 
    5: date = [ date, 0.0d]
    4: date = [ date, 0.0d,0.0d]    
    3: date = [ date, 0.0d, 0.0d,0.0d]
    else: message,'Illegal DATE Vector - must have a least 3 elements'

  endcase   

 iy = floor( date[0] ) 
 if iy lt 0 then iy = iy +1  else $
    if iy EQ 0 then message,'ERROR - There is no year 0'                   
 im = fix( date[1] )
 date = double(date)
 day = date[2] + ( date[3] + date[4]/60.0d + date[5]/3600.0d) / 24.0d
;
 if ( im LT 3 ) then begin   ;If month is Jan or Feb, don't include leap day

     iy= iy-1 & im = im+12 

 end

 a = long(iy/100)
 ry = float(iy)

 jd = floor(ry*0.25d) + 365.0d*(ry -1860.d) + fix(30.6001d*(im+1.)) + $
      day  - 105.5d

;Gregorian Calendar starts on Oct. 15, 1582 (= RJD -100830.5)
 if jd GT -100830.5 then jd = jd + 2 - a + floor(a/4)

 if N_params() LT 2 or keyword_set( PROMPT) then begin      
    yr = fix( date[0] )
    print, FORM='(A,I4,A,I3,A,F9.5)',$ 
       ' Year ',yr,'    Month', fix(date[1] ),'    Day', day 
    print, FORM='(A,F15.5)',' Reduced Julian Date:',JD                       
 endif
 
 return                               
 end                                  ; juldate


PRO GETDATE, ref_date, offset_hr, truedate

 truedate = make_array(4,/float)

 months = ['Jan','Feb','Mar','Apr','May','Jun','Jul', $
          'Aug','Sep','Oct','Nov','Dec']

 mon=strmid(ref_date,0,3)
 j=where(mon eq months,count)
 if count le 0 then begin
   print,"couldn't decode UT date in data (",ref_date,") !"
   return
 endif
 num_day_obs=[fix(strtrim(strmid(ref_date,8,4),2)), $
            fix(strtrim(string(j[0]+1),2)),(strtrim(strmid(ref_date,4,2),2))]
 day=strtrim(string(num_day_obs[2]),2) 
 yr =strtrim(string(num_day_obs[0]),2)
 mo =strtrim(string(num_day_obs[1]),2)

 truedate = [yr, mo, day, offset_hr]

end

PRO EPO2JUL, epoch, code, julian
; copied from epo2jul in miriad library ephem.o

  if (CODE eq ' ') then begin
     julian = (EPOCH gt 1984)
  endif else begin
     julian = ((CODE eq 'J') or (CODE eq 'j'))
  endelse

  if (julian) then begin
     julian = 365.25d0       *(EPOCH-2000.0) + 2451545.0d0
  endif else begin
     julian = 365.242198781d0 * (EPOCH-1900.0) + 2415020.31352d0
  endelse

end

FUNCTION MOBLIQ,jday

;c* Mobliqu -- Mean obliquity of the ecliptic
;c
;c  Return the mean obliquity of the ecliptic.
;c
;c  Input:
;c    jday       Julian day.
;c  Output:
;c    mobliq     Mean obliquity of the ecliptic, in radians.
;c
;c  Reference:
;c    Explanatory Supplement ... page 114.

        dpi = double(3.14159265358979323846)

;c
;c  Centuries from J2000
;c
        T = double((jday - 2451545.0d0) / 36525.0d0)
;c
;c  Mean obliquity.
;c
        mobliq = double(84381.448d0 - (46.8150d0+(0.00059d0-0.001813d0*T)*T)*T)
        mobliq = double(dpi/(180.0*3600.0) * mobliq)

        return,mobliq
END

PRO PRECESS, jday1, ra1, dec1, jday2, ra2, dec2
; copied from epo2jul in miriad library ephem.o

;c  A simple precession routine, to precess from one set of mean
;c  equatorial coordinates (RA,DEC), to another at a different epoch.
;c  This is accurate to order 0.3 arcsec over 50 years.
;c
;c  Reference:
;c    Explanatory Supplement to the Astronomical Almanac, 1993. p 105-106.
;c
;c  NOTE: This does not take account of atmospheric refraction,
;c  nutation, aberration nor gravitational deflection.
;c
;c  Input:
;c    jday1      Julian day of the known epoch.
;c    ra1,dec1   RA,DEC at the jday1 epoch (radians).
;c    jday2      Julian day of the new epoch.
;c  Output:
;c    ra2,dec2   Precessed coordinates (radians).

        dpi = 3.14159265358979323846d0

        T = double((jday1 - 2451545.0d0)/36525.0)
        M = dpi/180.0 * (1.2812323d0 + (0.0003879d0 + 0.0000101d0*T)*T)*T
        N = dpi/180.0 * (0.5567530d0 - (0.0001185d0 + 0.0000116d0*T)*T)*T
        rm = ra1 - 0.5*(M + N*sin(ra1)*tan(dec1))
        dm = dec1 - 0.5*N*cos(rm)
;c
;c  J2000 coordinates.
;c
        r0 = ra1 - M - N*sin(rm)*tan(dm)
        d0 = dec1 - N*cos(rm)
;c
;c  Coordinates of the other epoch.
;c
        T = double((jday2 - 2451545.0d0)/36525.0)
        M = dpi/180 * (1.2812323d0 + (0.0003879d0 + 0.0000101d0*T)*T)*T
        N = dpi/180 * (0.5567530d0 - (0.0001185d0 + 0.0000116d0*T)*T)*T
        rm = r0 + 0.5*(M + N*sin(r0)*tan(d0))
        dm = d0 - 0.5*N*cos(rm)
;c
        ra2 = r0 + M + N*sin(rm)*tan(dm)
        dec2 = d0 + N*cos(rm)

end

PRO Nutate, jday, rmean, dmean, rtrue, dtrue
; copied from epo2jul in miriad library ephem.o

;c
;c  Convert between mean and true equatorial coordinates, by
;c  accounting for nutation.
;c
;c  Input:
;c    jday       Julian day.
;c    rmean,dmean Mean (RA,DEC) at jday.
;c  Output:
;c    rtrue,dtrue True (RA,DEC) at jday.
;c
;c  Nutation parameters.
;c
        nuts,jday,dpsi,deps

;c  True obliquity.

        eps = mobliq(jday) + deps

;c  Various parameters.
        sineps = sin(eps)
        coseps = cos(eps)
        sinra  = sin(rmean)
        cosra  = cos(rmean)
        tandec = tan(dmean)

        rtrue = rmean + (coseps + sineps*sinra*tandec)*dpsi - cosra*tandec*deps
        dtrue = dmean + sineps*cosra*dpsi + sinra*deps

end

PRO ABERRATE,jday,ra,dec,raap,dapp,libfile=libfile, smamiriad=smamiriad

common global

;c* Aberrate -- Convert RA,DEC from true to geocentric apparent coords.

;c
;c  Account for the effect of annual aberration, to convert
;c  from a true (RA,DEC) to a geocentric apparent (RA,DEC).
;c
;c  Input:
;c    jday       Julian date.
;c    ra,dec     True (RA,DEC).
;c  Output:
;c    rapp,dapp  Geocentric apparent (RA,DEC).

        cmks = double(299792458.0d0)

        pos=make_array(3,/double)
        vel=make_array(3,/double)
 
        if not keyword_set(libfile) then begin
           if (strpos(!VERSION.ARCH,'86') ge 0) then begin
              if (strpos(!VERSION.ARCH,'64') ge 0) then begin
                 if keyword_set(smamiriad) then libfile=e.idl_sav+'libidlsmamiriad.so' else libfile=e.idl_sav+'libidl64mir.so'
              endif else begin
                 libfile=e.idl_sav+'libidlmir.so'
              endelse
           endif else begin
              print, 'QUIT !!! NO MIRIAD LIBRARY AVAILABLE FOR ',!VERSION.ARCH
              return
           endelse
        endif
        result=CALL_EXTERNAL(libfile, $
          'idl_vearth',double(jday),pos,vel)

        sinra = double(sin(ra))
        cosra = double(cos(ra))
        sindec = double(sin(dec))
        cosdec = double(cos(dec))

        rapp = double(ra +  (-vel(0)*sinra + vel(1)*cosra)/(0.001*cmks*cosdec))
        dapp = double(dec + (-vel(0)*cosra*sindec - vel(1)*sinra*sindec + vel(2)*cosdec)/(0.001*cmks))

END


PRO NUTS,jday,dpsi,deps

;c* Nuts -- Return nutation parameters.
;c
;c  Return nutation parameters. The claimed accuracy is 1 arcsec.
;c
;c  Input:
;c    jday       Julian date.
;c  Output:
;c    dpsi,deps  Difference between mean and true ecliptic latitude and
;c               longitude due to nutation, in radians.
;c
;c  Reference:
;c    Explanatory Supplement, page 120.

        dpi = 3.14159265358979323846d0

        d = double(jday - 2451545.0d0)
        t1 = double(dpi/180*(125.0 - 0.05295d0 * d))
        t2 = double(dpi/180*(200.9 + 1.97129d0 * d))
        dpsi = double(dpi/180 * (-0.0048*sin(t1) - 0.0004*sin(t2)))
        deps = double(dpi/180 * ( 0.0026*cos(t1) + 0.0002*cos(t2)))

END

PRO JULLST, jday, longitude, lst

;  Converted from Jullst(jday,longitude,lst) in MIRIAD /subs/ephem.for
;
;c  Reference: Explanatory Supplement to the Astronomical Almanac, p50-52.
;c  Accuracy appears to be 0.01 sec of time.
;c
;c  Input:
;c    jday       Julian day of interest.
;c    longitude      Observatory longitude (radians). East of Greenwich is
;c               positive.
;c  Output:
;c    lst        Local mean sidereal time (radians), in the range [0,2*pi].
;c
        dpi = 3.14159265358979323846d0

        T = double(floor(jday - 1.0) + 0.5)
        UT = double(jday - T)
        T = double((T - 2451545.0d0) / 36525.0d0)
;c
        GMST = double(24110.54841d0 +(8640184.812866d0 + (0.093104d0 - 6.2d-6*T)*T)*T)
        GMST = double(GMST / (3600.*24.) + UT * (1.002737909350795d0 + (5.9006d-11 - 5.9d-15*T)*T))

        lst = double(2*dpi*((GMST+longitude/(2*dpi))-floor(GMST+longitude/(2*dpi))))
        if (lst lt 0) then lst = lst + 2*dpi

END

; ############## SECTIONS OF IMPORTED SUB-PROCEDURES -- END ##############

pro idl2miriad,dir=dir,source=source,sideband=sideband,band=band, $
               cont=cont,edge_trim=edge_trim,polar=polar,verbose=verbose, $
               oldweight=oldweight, libfile=libfile, state=state, $
               smamiriad=smamiriad, trimflux=trimflux, auto=auto

common global
common data_set


if not keyword_set(libfile) then begin
   if (strpos(!VERSION.ARCH,'86') ge 0) then begin
      if (strpos(!VERSION.ARCH,'64') ge 0) then begin
         if keyword_set(smamiriad) then libfile=e.idl_sav+'libidlsmamiriad.so' else libfile=e.idl_sav+'libidl64mir.so' 
      endif else begin
         libfile=e.idl_sav+'libidlmir.so'
      endelse
   endif else begin
      print, 'QUIT !!! NO MIRIAD LIBRARY AVAILABLE FOR ',!VERSION.ARCH
      return
   endelse
endif

if not keyword_set(trimflux) then trimflux=10000.
fluxwarning=0

  PRINT, ""
  PRINT,' *********************** NOTICE ***********************'
  PRINT,' IDL2MIRIAD IS FOR CONVERTING SMA DATA FRORM MIR/IDL'
  PRINT,' FORMAT INTO MIRIAD FORMAT.'
  PRINT,' BE SURE TO MAKE A MIR_SAVED COPY BEFORE PROCEEDING'
  PRINT,' IN CASE THIS PROGRAM FAILS AND CORRUPTS YOUR DATA.'
  PRINT,''
  PRINT," USAGE: idl>idl2miriad,dir='outputdir',source='sourcename'
  PRINT,"          (,sideband='u',band='s1',edge_trim=0.1,polar=0,/verbose)"
  PRINT,"    or: idl>idl2miriad,dir='outputdir',source='sourcename'
  PRINT,"          (,sideband='u',/cont,edge_trim=0.1,polar=0,/verbose)"
  PRINT,''
  PRINT,' ALL DATA SATISFYING THE FILTER SETUP AND SELECTION'
  PRINT,' CRITERIA WILL BE OUTPUT. BAD (NEG. WEIGHT) DATA WILL'
  PRINT,' BE FLAGGED AS BAD (FALSE) IN MIRIAD DATASET. TO'
  PRINT,' EXCLUDE THOSE DATA BEING OUTPUT, PLEASE USE FILTER'
  PRINT,' WITH /POS_WT SETUP BEFORE RUNNING IDL2MIRIAD.'
  PRINT,''
  PRINT,' CONTINUUM BAND DATA WILL ALWAYS BE OUTPUT. BY DEFAULT,'
  PRINT,' AT LEAST ONE NARROW SPECTRAL WINDOW NEEDS TO EXIST IN'
  PRINT,' THE FILTERED DATASET OR TO BE SPECIFIED FOR PROPER'
  PRINT,' OUTPUT OF THE SPECTRAL DATA. ON THE OTHER HAND, ONE'
  PRINT,' CAN USE THE /cont FLAG TO OUTPUT ONLY CONTINUUM DATA'
  PRINT,' AS SPECTRAL CHANNEL(S). IN EITHER CASE, PSEUDO CONTINUUM'
  PRINT,' CHANNEL(S) WILL BE CREATED USING DATA FROM THE NARROW'
  PRINT,' SPECTRAL WINDOWS(CHUNKS). EDGE CHANNELS (10% ON EACH SIDE)'
  PRINT,' IN EACH CHUNK ARE TRIMMED OUT WHEN COMPUTING THE PSEUDO'
  PRINT,' CONTINUUM WITH SPECTRAL CHUNK DATA. THIS TRIMMING FRACTION'
  PRINT,' CAN BE MODIFIED WITH THE EDGE_TRIM KEYWORD.'
  PRINT,''
  PRINT,' FOR MOSAIC DATASETS, PLEASE MAKE SURE THAT ONLY GOOD DATA'
  PRINT,' ARE SELECTED. OTHERWISE, THE NUMBER OF MOSAIC FIELDS MAY '
  PRINT,' GET MIS-IDENTIFIED.'
  PRINT,''
  PRINT,' IF READY, PRESS ANY KEY TO CONTINUE......'
  if not keyword_set(auto) then keyin = get_kbrd(1)
  print,''

; ***** PREP WORK *****

  ; ***** check verbose keyword *****

  if (not keyword_set(verbose)) then verbose = 0
  if (e.debug) then verbose = 1

  ; ***** check designated output datadir *****

  if (not keyword_set(dir)) then begin
     print,"Please name the output directory"
     return
  endif

  if (file_test(dir) eq 1) then begin
    print, "The output data directory '",dir,"' already exists!!!"
    print, "Please use another directory name..."
    return
  endif

  ; ***** check selected source name *****

  if (not keyword_set(source)) then begin
     print,"Please specify the source name for outputting data"
     return
  endif

  all_sources=uti_distinct(c.source[in[pif].isource],ndistinct,/many_repeat)
  ; pif is used here to included all possible source names
  ; in the current filter setup

  match=0
  for i=0,ndistinct-1 do begin
    if (source eq all_sources[i] ) then match=1
  endfor

  if (match eq 0) then begin
    print, "No data associated with this source!!!"
    print, "Please check the dataset again..."
    return
  endif

  list=''

  list=list+' ("source" eq "'+source+'")'

  if (keyword_set(sideband)) then begin

    list = list + ' and '

    if (sideband[0] ne '') then begin
       list=list+' ("sb" eq "'+strtrim(sideband[0],2)+'"'
         if n_elements(sideband) gt 1 then begin
          for i = 1, n_elements(sideband)-1 do begin
              list=list+' or "sb" eq "'+strtrim(sideband[i],2)+'"'
          endfor
       endif
       list = list + ')'
    endif 

  endif

  if (keyword_set(cont)) then begin

      list = list + ' and ("band" eq "c1")'

  endif else begin

    cont = 0
    if (keyword_set(band)) then begin

      list = list + ' and '

      if (band[0] ne '') then begin
        list=list+' ("band" eq "c1" or "band" eq "'+strtrim(band[0],2)+'"'
        if n_elements(band) gt 1 then begin
          for i = 1, n_elements(band)-1 do begin
              list=list+' or "band" eq "'+strtrim(band[i],2)+'"'
          endfor
        endif
        list = list + ')'
      endif

    endif

  endelse

if keyword_set(state) then  list = list + ' and "pol" eq "'+strcompress(state,/remove)+'"'

  npts=dat_list(s_l,list,/reset,/no_notify)
  if (npts eq 0) then begin
     print,"No data record matches the sideband/band requirement for"
     print,"this source. Please check the dataset/filter again!"
     return
  endif

  if (not keyword_set(edge_trim)) then edge_trim = 0.1

  if (edge_trim lt 0 or edge_trim gt 0.5) then begin
    print, "edge_trim value is out of range (0-0.5)"
    print, "Please use another edge_trim value..."
    return
  endif

  ; ***** check polar keyword *****

  if (not keyword_set(polar)) then polar = 0
  if ((polar gt 2) or (polar lt 1)) then polar = 0
  if (polar eq 0) then begin
    print,"The MIR data are considered as non-polarization data, will use XX as default."
    mirpol = long(-5)
  endif
  if (polar eq 1) then begin
    print,"The polarization codes in MIR data are considered as circularly polarization."
    ; 0 -> unknown, 1 -> rr, 2-> rl, 3-> lr, 4-> ll
    mirpol = [long(0),long(-1),long(-3),long(-4),long(-2)]
  endif
  if (polar eq 2) then begin
    print,"The polarization codes in MIR data are considered as linearly polarization."
    ; 0 -> unknown, 1 -> xx, 2-> xy, 3-> yx, 4-> yy
    mirpol = [long(0),long(-5),long(-7),long(-8),long(-6)]
  endif

; ***** setting output file handle unit *****

  unit=0

; ***** observatory/telescope specific information *****

  telescop = 'SMA'
  version  = 'SMA 1.0'
  SMAlatitude = 19.82420526391d0 ; pad # 1 (extracted from fits_out.pro)
  SMAlatitude = SMAlatitude * 2 * !PI / 360.0

  SMAlongitude = (360. - (155.+(28.+37.20394/60.)/60.0))/360.*2.*!PI 
                 ; currently using JCMT's longitude on MKO webpage

  nants = long(8)
 
  alltels = [bl[pbl].itel1,bl[pbl].itel2]
  distinct_tels=alltels(  uniq(alltels,sort(alltels) ))
  inflag = make_array(nants,/int,value=0)
  inflag(distinct_tels[*]-1) = 1

  rant = (6.0/2)

  ; ***** obtaining LO info *****

    csbmark = make_array(2,/int)
    cfreq=make_array(2,/double)
    cwidth=make_array(2,/double)
    sbfreq=make_array(2,/double)
    basefreq=make_array(2,/double)

    ; getting first integration in the output sub-dataset
    first_int = min(in[pil].int)

    j = where(in[pil].int eq first_int and bl[pbl].isb eq 0 and sp[psl].iband eq 0, jcount)
      ; selecting lsb c1 for first (filtered) integration
      ; using pi,pb,ps in case data for lsb+c1 are filtered out
    if (jcount gt 0) then begin
      csbmark[0] = 1
      cfreq[0]= sp[psl[j[0]]].fsky
      cwidth[0] = double((sp[psl[j[0]]].fres))
    endif else begin
      print, 'Likely NO LSB Continuum Data Present.'
      cfreq[0] = 0
      cwidth[0]=0
    endelse

    j = where(in[pil].int eq first_int and bl[pbl].isb eq 1 and sp[psl].iband eq 0, jcount)
      ; selecting usb c1 for first (filtered) integration
      ; using pi,pb,ps in case data for usb+c1 are filtered out
    if (jcount gt 0) then begin
      csbmark[1] = 1
      cfreq[1]= sp[psl[j[0]]].fsky
      cwidth[1] = double((sp[psl[j[0]]].fres))
    endif else begin
      print, 'Likely NO USB Continuum Data Present.'
      cfreq[1] = 0
      cwidth[1]=0
    endelse

    csbidx = where(csbmark ne 0)

    sbfreq[0]=cfreq[0]
    sbfreq[1]=cfreq[1]
    basefreq[0]=cfreq[0]
    basefreq[1]=cfreq[1]

    LO = total (sbfreq * csbmark) / total(csbmark)

  ; get the assumed aperture efficiency 
  if (LO gt 150 and LO lt 300) then reff = 0.65
  if (LO gt 300 and LO lt 400) then reff = 0.50
  if (LO gt 400 and LO lt 700) then reff = 0.40                                 

  ; Jansky-per-Kelvin conversion factor
  jyperk = float((2 * 1.38e3 / !PI) / (reff * rant^2))

  ; pb hwhm (= 16.985 * 345 / LO) is based on beam calculator on TEST CENTRAL
  pbfwhm = float(2. * (16.985 * 345. / LO))


; ***** observed source information *****

  mosaic_flag = 0

  print, ""
  print, "Checking source positions/pointings"

  tmp_ra_list = uti_distinct(c.ra[in[pil].ira], nra)
  ra_index = make_array(n_elements(in[pil]),/int)
  for i=0, (nra -1) do begin
    matchidx = where(strcmp(c.ra[in[pil].ira],tmp_ra_list[i]))
    ra_index[matchidx] = i
  endfor
  tmp_dec_list = uti_distinct(c.dec[in[pil].idec], ndec)
  dec_index = make_array(n_elements(in[pil]),/int)
  for i=0, (ndec - 1) do begin
    matchidx = where(strcmp(c.dec[in[pil].idec],tmp_dec_list[i]))
    dec_index[matchidx] = i
  endfor
  off_index = ra_index * ndec + dec_index

  tmp_off_list = uti_distinct(off_index, noff)

  print, "The dataset for source ", source, " has ", noff, " positions/pointings."

  ref_ra_idx = -1
  ref_dec_idx = -1

  for i=0, (noff - 1) do begin
    tmp_raidx  = tmp_off_list[i]/ndec 
    tmp_decidx = tmp_off_list[i] mod ndec
    matchidx = where(strcmp(c.ra[in[pil].ira],tmp_ra_list[tmp_raidx]) and strcmp(c.dec[in[pil].idec],tmp_dec_list[tmp_decidx]))

    print,"Pointing ", i, " index ", tmp_raidx, tmp_decidx
    print," RA= ", tmp_ra_list[tmp_raidx], ", Dec= ", tmp_dec_list[tmp_decidx], ", RA off = ", in[pil[matchidx[0]]].offx, ", Dec off= ", in[pil[matchidx[0]]].offy

    if (in[pil[matchidx[0]]].offx eq 0.) then ref_ra_idx = matchidx[0]
    if (in[pil[matchidx[0]]].offy eq 0.) then ref_dec_idx = matchidx[0]
    if ((in[pil[matchidx[0]]].offx ne 0.) or (in[pil[matchidx[0]]].offy ne 0.)) then mosaic_flag = 1
  endfor

  if (ref_dec_idx ne -1 ) then begin
    srcdec = in[pil[ref_dec_idx]].decr
  endif else begin
    ddec = in[pil[matchidx[0]]].offy
    ddec = ddec * double(2. * !PI/ (60.*60.*360.))
    srcdec = in[pil[matchidx[0]]].decr - ddec
  endelse

  if (ref_ra_idx ne -1) then begin
    srcra  = in[pil[ref_ra_idx]].rar
  endif else begin
    dra = in[pil[matchidx[0]]].offx
    dra =dra * double(2. * !PI / (60.*60.*360.))
    srcra = in[pil[matchidx[0]]].rar - dra/cos(srcdec)
  endelse

  if (e.debug) then print, srcra, srcdec

  print,""
  if (mosaic_flag eq 0 and noff eq 1.) then print, "*** Dataset with a single source/pointing ***"
  if (mosaic_flag eq 0 and noff ne 1.) then print, "*** Dataset with a single but fast moving source ***"
  if (mosaic_flag eq 1 and noff ne 1.) then begin
    print, "*** Dataset with mosaic fields ***"

    srcra_tmp = srcra / (2.*!PI) * 24.
    srcra_h = floor(srcra_tmp)
    srcra_tmp = (srcra_tmp - srcra_h) * 60.
    srcra_m = floor(srcra_tmp)
    srcra_tmp = (srcra_tmp - srcra_m) * 60.
    srcra_s = srcra_tmp

    srcdec_tmp = double(srcdec / (2.*!PI) * 360.)
    if (srcdec_tmp ge 0) then begin
      srcdec_d = floor(srcdec_tmp)
      srcdec_tmp = double((srcdec_tmp - srcdec_d) * 60.)
      srcdec_m = floor(srcdec_tmp)
      srcdec_tmp = double((srcdec_tmp - srcdec_m) * 60.)
      srcdec_s = srcdec_tmp
    endif else begin
      srcdec_d = ceil(srcdec_tmp)
      srcdec_tmp = double((srcdec_d - srcdec_tmp) * 60.)
      srcdec_m = floor(srcdec_tmp)
      srcdec_tmp = double((srcdec_tmp - srcdec_m) * 60.)
      srcdec_s = srcdec_tmp
    endelse
    print, ""
    print, "Referencing Position :"
    print, "RA= ", srcra_h,":",srcra_m,":", srcra_s, ", (in radian) ", srcra
    print, "Dec =", srcdec_d,":",srcdec_m,":",srcdec_s, ", (in radian) ", srcdec
;    print, "RA= ", c.ra[pil[ref_ra_idx]], ", (in radian) ", srcra
;    print, "Dec= ", c.dec[pil[ref_dec_idx]], ", (in radian) ", srcdec
  endif
  print,""

  epoch  = float(in[pil[0]].epoch)

  epo2jul,epoch,' ',eporef

  getdate,c.ref_time(in(pil(0)).iref_time), 0.0, truedate
  juldate, truedate, timeref
  timeref=timeref+2400000

  precess,eporef,srcra,srcdec,timeref,obsra,obsdec
  nutate,timeref,obsra,obsdec,r0,d0
  aberrate,timeref,r0,d0,obsra,obsdec, libfile=libfile, smamiriad=smamiriad

; ***** observational setup information *****

  ; *** polarization ***

   all_pol=uti_distinct(bl[pbl].ipol,npol,/many_repeat)
   npol = long(npol)

  ; *** velocity system ***

;  all_vtype=uti_distinct(in[pil].ivctype,ndistinct,/many_repeat)
;  if (ndistinct gt 1) then begin 
;    print,'This routine will not handle datasets with MULTIPLE velocity tracking types'
;    return
;  endif else begin
;    vtype = in[pil[0]].ivctype
;  endelse
  vtype=0 ; turn off vtype reading for SMA new data format.


  case vtype of
   0   : veltype='VELO-LSR'
   1   : veltype='VELO-HEL'
   2   : veltype='VELO-HEL'
   3   : veltype='VELO-OBS'
   else: begin
          print,'Velocity reference system UNrecognized!!!'
          return
         endelse
  endcase

  ; *** narrow band spectral setup info ***

  if (not keyword_set(cont)) then begin

    all_sb=uti_distinct(bl[pbl].isb,nsb,/many_repeat)
    ; nsb: number of sidebands which include all valid narrow windows

    sbchunk=make_array(nsb,/long)
    sbchan=make_array(nsb,/long)

    ; counting narrow windows for each sideband
    for sbcount=0, (nsb-1) do begin
       j = where(bl[pbl].isb eq all_sb[sbcount] and sp[psl].iband gt 0, jcount)
       if (jcount eq 0) then chunknum = 0
       if (jcount gt 0) then all_band=uti_distinct(sp[psl[j]].iband,chunknum,/many_repeat)
       sbchunk[sbcount]=chunknum
    endfor

    ; total number of narrow windows
    nspect=long(total(sbchunk))

    if (nspect eq 0) then begin
      print,"At least one spectral channel needs to be specified!"
      print,"Please check dataset/filter again!"
      return
    endif

    if (nspect gt 0) then begin

      speccode = make_array(nspect,/int)
      restfreq = make_array(nspect,/double)
      sdf      = make_array(nspect,/double)
      sfreq    = make_array(nspect,/double)
      nschan   = make_array(nspect,/long)
      ischan   = make_array(nspect,/long)
      nbw      = make_array(nspect,/float)
      spvdop   = make_array(nspect,/double)

      ; *** getting narrow band spectral information sideband by sideband ***

      chunkcount=0
      for sbcount=0, (nsb-1) do begin

        if (sbchunk[sbcount] gt 0) then begin

           j = where(bl[pbl].isb eq all_sb[sbcount] and sp[psl].iband gt 0)
           all_band=uti_distinct(sp[psl[j]].iband,chunknum,/many_repeat)

           for ckcount=0, (sbchunk[sbcount]-1) do begin

             j = where(bl[pbl].isb eq all_sb[sbcount] and sp[psl].iband eq all_band[ckcount])

             ; "spectral bands"
             ; sp[psl[j[0]]].iband

             ; coding all spectra narrow bands with a number = 100 * bl.isb + sp.iband
               speccode[chunkcount+ckcount] = 100. * all_sb[sbcount] + sp[psl[j[0]]].iband

             ; "Rest Freq"
             restfreq[chunkcount+ckcount] = sp[psl[j[0]]].rfreq

             ; "Freq resolution"
             sdf[chunkcount+ckcount] = double(sp[psl[j[0]]].fres)

             ; "Calculated Vel resolution ( fres/fsky )"
             ; (-sp[psl[j[0]]].fres/sp[psl[j[0]]].fsky/1000*!cvel/1000.)
             ; "Recorded Vel resolution"
             ; sp[psl[j[0]]].vres

             ; "Sky Freq for central channel"
             sfreq[chunkcount+ckcount] = sp[psl[j[0]]].fsky
             fsky = sp[psl[j[0]]].fsky

             nschan[chunkcount+ckcount] = long(sp[psl[j[0]]].nch)

             case vtype of
             0   : begin
                     vabs = (1- (fsky/sp[psl[j[0]]].rfreq)^2)/(1+(fsky/sp[psl[j[0]]].rfreq)^2)*!cvel/1000.0 ; absolute vel
                     ; sp[psl[j[0]]].vel VLSR recorded
                     ; Deriving doppler velocity from the observer to the LSR
                     spvdop[chunkcount+ckcount] = (vabs - sp[psl[j[0]]].vel)
                   end
             else: begin
                   endelse
             endcase

           endfor

           sbchan[sbcount]=total(nschan[chunkcount:chunkcount+sbchunk[sbcount]-1])
           chunkcount = chunkcount + sbchunk[sbcount]

        endif

      endfor

      case vtype of
        0   : begin
                ; Taking the AVERAGE of Doppler velocity derived over all narrow bands
                ; (to remove the relatively small numerical errors)
                veldop = float(total(spvdop)/nspect)
                vsource =float(0.0)
              end      
        else: begin
                print,"CAUTION: calculation of vtype velocity not implemented!!!"
                vsource=0.0
                veldop=0.0
              endelse
      endcase

      ; some more modification for all narrow windows
        ; Switching sdf to GHz unit
        sdf = sdf / 1000.0                          

        ; Shifting the frequency from the window center (MIR/IDL) to the window edge (center of 1st channel) (MIRIAD)
        ; For a chuck of 128ch, for example, MIR has sfreq at the end ch64, use 63.5 * sdf to shift to the center of 1ch.
        ; For a cont. chan of a ch. MIR has sfreq at the center, no need to shift, or a shift of 0.
        ; sfreq = sfreq - sdf * floor((nschan-1)/2.0) (old/incorrect)
        sfreq = sfreq - sdf * ((nschan/2.0) - 0.5)
          
        nbw = (nschan * sdf)
        numchan = long(total(nschan))

        ischan[0]=1
        for chunkcount=1, (nspect-1) do begin
           ischan[chunkcount] = total(nschan[0:chunkcount-1])+1
        endfor

    endif

  endif

  if (keyword_set(cont)) then begin

    ; total number of wideband
    all_sb = csbidx
    nsb = n_elements(csbidx)
    nspect = nsb

    if (nspect eq 0) then begin
      print,"At least one sideband continuum needs to be specified!"
      print,"Please check dataset/filter again!"
      return
    endif

    if (nspect gt 0) then begin

      speccode = make_array(nspect,/int)
      restfreq = make_array(nspect,/double)
      sdf      = make_array(nspect,/double)
      sfreq    = make_array(nspect,/double)
      nschan   = make_array(nspect,/long)
      ischan   = make_array(nspect,/long)
      nbw      = make_array(nspect,/float)
      spvdop   = make_array(nspect,/double)

      ; *** getting wideband spectral information sideband by sideband ***

      for sbcount=0, (nspect -1) do begin

             j = where(bl[pbl].isb eq csbidx[sbcount] and sp[psl].iband eq 0)

             ; "spectral bands"
             ; sp[psl[j[0]]].iband

             ; coding all spectra narrow bands with a number = 100 * bl.isb + sp.iband
               speccode[sbcount] = 100. * csbidx[sbcount]

             ; "Rest Freq"
             restfreq[sbcount] = sp[psl[j[0]]].rfreq

             ; "Freq resolution"
             sdf[sbcount] = double(sp[psl[j[0]]].fres)

             ; "Calculated Vel resolution ( fres/fsky )"
             ; (-sp[psl[j[0]]].fres/sp[psl[j[0]]].fsky/1000*!cvel/1000.)
             ; "Recorded Vel resolution"
             ; sp[psl[j[0]]].vres

             ; "Sky Freq for central channel"
             sfreq[sbcount] = sp[psl[j[0]]].fsky
             fsky = sp[psl[j[0]]].fsky

             nschan[sbcount] = long(sp[psl[j[0]]].nch)

             case vtype of
             0   : begin
                     vabs = (1- (fsky/sp[psl[j[0]]].rfreq)^2)/(1+(fsky/sp[psl[j[0]]].rfreq)^2)*!cvel/1000.0 ; absolute vel
                     ; sp[psl[j[0]]].vel VLSR recorded
                     ; Deriving doppler velocity from the observer to the LSR
                     spvdop[sbcount] = (vabs - sp[psl[j[0]]].vel)
                   end
             else: begin
                   endelse
             endcase

      endfor

      case vtype of
        0   : begin
                ; Taking the AVERAGE of Doppler velocity derived over all narrow bands
                ; (to remove the relatively small numerical errors)
                veldop = float(total(spvdop)/nspect)
                vsource =float(0.0)
              end      
        else: begin
                print,"CAUTION: calculation of vtype velocity not implemented!!!"
                vsource=0.0
                veldop=0.0
              endelse
      endcase

      ; some more modification for all narrow windows
        ; Switching sdf to GHz unit
        sdf = sdf / 1000.0                          

        ; Shifting the frequency from the window center (MIR/IDL) to the window edge (center of 1st channel) (MIRIAD)
        ; For a chuck of 128ch, for example, MIR has sfreq at the end ch64, use 63.5 * sdf to shift to the center of 1ch.
        ; For a cont. chan of a ch. MIR has sfreq at the center, no need to shift, or a shift of 0.
        ; sfreq = sfreq - sdf * floor((nschan-1)/2.0) (old/incorrect)
        sfreq = sfreq - sdf * ((nschan/2.0) - 0.5)

        nbw = (nschan * sdf)
        numchan = long(total(nschan))

        ischan[0]=1
        for sbcount=1, (nspect-1) do begin
           ischan[sbcount] = total(nschan[0:sbcount-1])+1
        endfor

    endif

  endif

  ; *** wideband spectral setup info ***

  nwide=long(2+nspect)
  wfreq=make_array(nwide,/float)
  wwidth=make_array(nwide,/float)

  wfreq[csbidx] = cfreq[csbidx]
  wwidth[csbidx] = double(cwidth[csbidx])/1000.0
  ; ***** NOTE: current cwidth is hardcoded as 416MHz in DataCatcher.
  ;             This will need to be fixed.
  ; wwidth[csbidx] are divide by 1000.0 for units in GHz

  wfreq[2:nwide-1]  = sfreq
  wwidth[2:nwide-1] = double(nbw)

  wfreq = wfreq + 0.5 * wwidth

  if (e.debug) then begin
    print,"speccode",speccode
    print,"restfreq",restfreq
    print,"sfreq",sfreq
    print,"sdf",sdf
    print,"nschan",nschan
    print,"ischan",ischan
    print,"wfreq",wfreq
    print,"wwidth",wwidth
  endif

  ; *** All header information prepared ***

  one=long(1)

  print,"--- Creating/Opening new data directory ---"

    result=CALL_EXTERNAL(libfile, $
                     'idl_uvopen',dir,unit)

  print,"--- Writing brief history entry ---"

    result=CALL_EXTERNAL(libfile, $
                     'idl_hisappend',unit)
    result=CALL_EXTERNAL(libfile, $
                     'idl_hiswrite',unit,'IDL/MIR-to-MIRIAD, Version: Beta')
    result=CALL_EXTERNAL(libfile, $
                     'idl_hiswrite',unit,'based on library version xx-xx-xx')
    result=CALL_EXTERNAL(libfile, $
                     'idl_hiswrite',unit,'Target Source: '+source)
    result=CALL_EXTERNAL(libfile, $
                     'idl_hiswrite',unit,'LO Frequency: '+string(LO)+' GHz')
    result=CALL_EXTERNAL(libfile, $
                     'idl_hisclose',unit)

  print,"--- Inserting observer header info ---"

    result=CALL_EXTERNAL(libfile, $
                     'idl_uvputvr',unit,'a','observer', e.user_name)

  print,"--- Inserting observatory specific header info ---"

    result=CALL_EXTERNAL(libfile, $
                     'idl_uvputvr',unit,'a','telescop', telescop)
    result=CALL_EXTERNAL(libfile, $
                     'idl_uvputvr',unit,'a','version', version)
    result=CALL_EXTERNAL(libfile, $
                     'idl_uvputvr',unit,'d','latitud',SMAlatitude,one)
    result=CALL_EXTERNAL(libfile, $
                     'idl_uvputvr',unit,'d','longitu',SMAlongitude,one)

    result=CALL_EXTERNAL(libfile, $
                     'idl_uvputvr',unit,'i','nants',nants,one)
;    result=CALL_EXTERNAL(libfile, $
;                     'idl_uvputvr',unit,'r','jyperk',jyperk,one)
    result=CALL_EXTERNAL(libfile, $
                     'idl_uvputvr',unit,'r','pbfwhm',pbfwhm,one)

  print,"--- Inserting source header info ---"

    if (strlen(source) gt 8) then begin
       print,'NOTICE!!! MIRIAD only allows source name with a maximum of 8 characters'
       print,'Source name ',source,' in the header will be truncated into ',strmid(source,0,8)
       source=strmid(source,0,8)
    endif
    source = strupcase(source)

    result=CALL_EXTERNAL(libfile, $
                     'idl_uvputvr',unit,'a','source',source)
    result=CALL_EXTERNAL(libfile, $
                     'idl_uvputvr',unit,'d','ra',srcra,one)
    result=CALL_EXTERNAL(libfile, $
                     'idl_uvputvr',unit,'d','dec',srcdec,one)
    result=CALL_EXTERNAL(libfile, $
                     'idl_uvputvr',unit,'r','epoch',epoch,one)
    result=CALL_EXTERNAL(libfile, $
                     'idl_uvputvr',unit,'d','obsra',obsra,one)
    result=CALL_EXTERNAL(libfile, $
                     'idl_uvputvr',unit,'d','obsdec',obsdec,one)
    result=CALL_EXTERNAL(libfile, $
                     'idl_uvputvr',unit,'a','purpose','S')

    result=CALL_EXTERNAL(libfile, $
                     'idl_uvputvr',unit,'a','veltype',veltype)
    result=CALL_EXTERNAL(libfile, $
                     'idl_uvputvr',unit,'r','veldop',veldop,one)
    result=CALL_EXTERNAL(libfile, $
                     'idl_uvputvr',unit,'r','vsource',vsource,one)

  print,"--- Inserting observing setup header info ---"

    result=CALL_EXTERNAL(libfile, $
                     'idl_uvputvr',unit,'d','lo1',LO,one)

  print,"---         wide band channel header info ---"

    result=CALL_EXTERNAL(libfile, $
                     'idl_uvputvr',unit,'i','nwide',nwide,one)
    result=CALL_EXTERNAL(libfile, $
                     'idl_uvputvr',unit,'r','wfreq',wfreq,nwide)
    result=CALL_EXTERNAL(libfile, $
                     'idl_uvputvr',unit,'r','wwidth',abs(wwidth),nwide)

  print,"---       narrow band channel header info ---"

    result=CALL_EXTERNAL(libfile, $
                     'idl_uvputvr',unit,'i','numchan',numchan,one)
    result=CALL_EXTERNAL(libfile, $
                     'idl_uvputvr',unit,'i','nspect',nspect,one)
    result=CALL_EXTERNAL(libfile, $
                     'idl_uvputvr',unit,'d','restfreq',restfreq,nspect)
    result=CALL_EXTERNAL(libfile, $
                     'idl_uvputvr',unit,'d','sfreq',sfreq,nspect)
    result=CALL_EXTERNAL(libfile, $
                     'idl_uvputvr',unit,'d','sdf',sdf,nspect)
    result=CALL_EXTERNAL(libfile, $
                     'idl_uvputvr',unit,'i','nschan',nschan,nspect)
    result=CALL_EXTERNAL(libfile, $
                     'idl_uvputvr',unit,'i','ischan',ischan,nspect)

  print,"---              polarization header info ---"

    result=CALL_EXTERNAL(libfile, $
                     'idl_uvputvr',unit,'i','npol',npol,one)
    result=CALL_EXTERNAL(libfile, $
                     'idl_wrhdi',unit,'npol',npol)

  print,"---              fake antpos/corr header info ---"

    fakecormode = long(1)
    result=CALL_EXTERNAL(libfile, $
                     'idl_uvputvr',unit,'i','cormode',fakecormode,one)
    fakecorfin  = float([100.,200.,300.,400.])
    result=CALL_EXTERNAL(libfile, $
                     'idl_uvputvr',unit,'r','corfin',fakecorfin,4)
    fakecorbw  = float([20.,20.])
    result=CALL_EXTERNAL(libfile, $
                     'idl_uvputvr',unit,'r','corbw',fakecorbw,2)
    fakeantpos = make_array(nants*3,/double,value=100.0)
    result=CALL_EXTERNAL(libfile, $
                     'idl_uvputvr',unit,'d','antpos',fakeantpos,nants*3)


    all_ints=uti_distinct(in[pil].int,nint,/many_repeat)
    int_list=all_ints(uniq(all_ints,sort(all_ints)))


    PRINT,"--- READY TO CYCLE THROUGH ALL INTEGRATIONS ---"
;    if not keyword_set(auto) then begin
;      PRINT,''
;      PRINT, "PRESS ANY KEY TO CONTINUE......"
;      keyin = get_kbrd(1)
;    endif

    for i=0,(nint-1) do begin

       if (verbose) then print,"---  initialize antenna-based tsys matrices ---"

       ;  wideband channel tsys
       wtsys = make_array(nants,2+nspect,/float,value=0.0)
       wsolflag = make_array(2+nspect,/float,value=0.0)

       ;  wideband antenna tsys flag
       ant_wflag = make_array(nants,2+nspect,/int,value=0)

       ;  temporary wideband tsys matrices
       wbtsys  = make_array(nants,nants,2,value=0.0)
       wbflag  = make_array(nants,nants,2,/int,value=0)

       ;  narrowband channel tsys
       tsys = make_array(nants,nspect,/float,value=0.0)
       solflag = make_array(nspect,/float,value=0.0)

       ;  narrowband antenna tsys flag
       ant_flag = make_array(nants,nspect,/int,value=0)

       ;  temporary narrowband matrices
       nbtsys  = make_array(nants,nants,nspect,/float,value=0.0)
       nbflag  = make_array(nants,nants,nspect,/int,value=0)

       if (verbose) then print,'  INTEG ',int_list[i]

       l = where(in[pil].int eq int_list[i])

       inttime = in[pil[l[0]]].rinteg

       if (verbose) then print,"---          integration time header info ---"

       result=CALL_EXTERNAL(libfile, $
                            'idl_uvputvr',unit,'r','inttime',inttime,one)

;       getdate,c.ref_time(in(pil[l[0]]).iref_time),
;       bl[pbl[l[0]]].avedhrs - inttime/(60.*60.*2.) , truedate
       ; the time tag is shifted by half of the integration time
       ; as mir appears to time-tag the middle of an integration
       ; while miriad time-tags the beginning of an integration.
;the above might not be true according to Jun-Hui and miriad team.
;switching back to center. Qi April 11, 2006. 
       getdate,c.ref_time(in(pil[l[0]]).iref_time), bl[pbl[l[0]]].avedhrs, truedate
       juldate, truedate, jd
       jd=jd+2400000.d0

       ut = double( ((jd-0.5) - floor(jd-0.5)) * 2. * !PI)
       result=CALL_EXTERNAL(libfile, $
                        'idl_uvputvr',unit,'d','ut',ut, one)

       jullst,jd,SMAlongitude,lst
       result=CALL_EXTERNAL(libfile, $
                        'idl_uvputvr',unit,'d','lst',lst, one)

       if (mosaic_flag eq 1) then begin
         dra = in[pil[l[0]]].offx
         ddec = in[pil[l[0]]].offy

         if (verbose) then print, "mosaicing pointing offsets (in arcsecs)", dra, ddec
         dra =  dra * float(2. * !PI / (60.*60.*360.))
         ddec = ddec * float(2. * !PI / (60.*60.*360.))

         result=CALL_EXTERNAL(libfile, $
                        'idl_uvputvr',unit,'r','dra',dra, one)

         result=CALL_EXTERNAL(libfile, $
                        'idl_uvputvr',unit,'r','ddec',ddec, one)
       endif

       all_bsl=uti_distinct(bl[pbl[l]].iblcd,nbsl,/many_repeat)

       if (e.debug) then print,'bl[pbl[l]].isb',bl[pbl[l]].isb
       if (e.debug) then print,'sp[psl[l]].iband',sp[psl[l]].iband

       if (verbose) then print,'  - cycling through all baselines to get baseline-based tsys  -'

       for j=0,(nbsl-1) do begin

         if (verbose) then print,'    BSL ',c.blcd[all_bsl[j]]
         m = where(bl[pbl[l]].iblcd eq all_bsl[j])

         ant1 = bl[pbl[l[m[0]]]].itel1
         ant2 = bl[pbl[l[m[0]]]].itel2

         ; initialize wide band (continuum channel) baseline-based tsys array
         for k=0,(nsb-1) do begin

           n = where(bl[pbl[l[m]]].isb eq all_sb[k] and sp[psl[l[m]]].iband eq 0, ncount)

           if (all_sb[k] eq 0) then tosb = 0 ; lsb
           if (all_sb[k] eq 1) then tosb = 1 ; usb

           if (ncount gt 0) then begin
             wbtsys[(ant1-1),(ant2-1),tosb] = sp[psl[l[m[n[0]]]]].tssb
             wbtsys[(ant2-1),(ant1-1),tosb] = sp[psl[l[m[n[0]]]]].tssb
             wbflag[(ant1-1),(ant2-1),tosb] = 1
           endif

         endfor

         ; initialize narrow band (spectral window) baseline-based tsys array
         for k=0,(nspect-1) do begin

           specsb   = (speccode[k]/100)
           specband = speccode[k] - specsb * 100

           n = where(bl[pbl[l[m]]].isb eq specsb and sp[psl[l[m]]].iband eq specband, ncount)

           if (ncount gt 0) then begin
             nbtsys[(ant1-1),(ant2-1),k]=sp[psl[l[m[n[0]]]]].tssb
             nbtsys[(ant2-1),(ant1-1),k]=sp[psl[l[m[n[0]]]]].tssb
             nbflag[(ant1-1),(ant2-1),k]=1
           endif

         endfor

       endfor


       ; Solving the antenna-based tsys values

          ; wide band (continuum channel)

             for k=0,(nsb-1) do begin

               if (all_sb[k] eq 0) then tosb = 0 ; lsb
               if (all_sb[k] eq 1) then tosb = 1 ; usb

               resultx=max(total(wbflag[*,*,tosb],1),wbflagx)
               resulty=max(total(wbflag[*,*,tosb],2),wbflagy)

               if (resultx ge resulty) then begin
                 refant=wbflagx
                 antl=where(wbflag[*,refant,tosb] gt 0)
               endif else begin
                 refant=wbflagy
                 antl=where(wbflag[refant,*,tosb] gt 0)
               endelse

               antn=n_elements(antl)

               wsolflag[tosb]=0
               if (antn ge 2) then begin
                  tosol=1
                  p=0
                  q=p+1
                  wsolflag[tosb]=1
               endif else begin
                  tosol=0
               endelse

               while (tosol eq 1) do begin
                 an1=antl(p)
                 an2=antl(q)
                 if (wbflag[an1,an2,tosb] eq 1) then begin
                    tref=wbtsys[refant,an1,tosb]*wbtsys[refant,an2,tosb]/wbtsys[an1,an2,tosb]
                    if (e.debug) then print,refant,an1,an2,tosb,tref
                    wsolflag[tosb]=1
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

 
               ; wide band tsys
               if (wsolflag[tosb] eq 1) then begin
                 wtsys[*,tosb] =  wbtsys[refant,*,tosb]^2/tref
                 wtsys[refant,tosb] = tref
                 ant_wflag[*,tosb] = 1L


                 zerolist = where((wtsys[*,tosb] eq 0) and (inflag[*] eq 1), zerocnt)
                 if (zerocnt gt 0) then begin
                   for iz = 0, zerocnt -1 do begin
                     tmpxlist = where((wbtsys[*,zerolist[iz],tosb]) gt 0, tmpxcnt)
                     tmpylist = where((wbtsys[zerolist[iz],*,tosb]) gt 0, tmpycnt)
                     if (tmpxcnt eq 0 and tmpycnt eq 0) then begin
                       print, " ### NOTE: NO TSYS DATA FOR ANT ",(zerolist[iz]+1), " AT INT ",int_list[i], $
                              "  SIDEBAND ", c.sb[all_sb[k]], " ###"
                       print,'Arbitrary system temperature of 400 K is inserted for this antenna'
                       print,'and data on associated baselines will be flagged bad'
                       wtsys[zerolist[iz],tosb] = 400.0
                       ant_wflag[zerolist[iz],tosb] = 0
                     endif else begin
                       zsol=0
                       if (tmpxcnt gt 0 and zsol eq 0) then begin
                         itmp = 0
                         while (zsol eq 0 and itmp lt tmpxcnt) do begin
                           if (ant_wflag[tmpxlist[itmp],tosb] eq 1) then begin
                             wtsys[zerolist[iz],tosb] =  wbtsys[tmpxlist[itmp],zerolist[iz],tosb]^2/wtsys[tmpxlist[itmp],tosb]
                             zsol=1
                           endif
                           itmp = itmp + 1
                         endwhile
                       endif

                       if (tmpycnt gt 0 and zsol eq 0) then begin
                         itmp = 0
                         while (zsol eq 0 and itmp lt tmpycnt) do begin
                           if (ant_wflag[tmpylist[itmp],tosb] eq 1) then begin
                             wtsys[zerolist[iz],tosb] =  wbtsys[zerolist[iz],tmpylist[itmp],tosb]^2/wtsys[tmpylist[itmp],tosb]
                             zsol=1
                           endif
                           itmp = itmp + 1
                         endwhile
                       endif

                       if (zsol eq 0) then begin
                         print, " ### NOTE: NOT SOLVABLE FOR ANT ",(zerolist[iz]+1), " AT INT ",int_list[i], $
                                "  SIDEBAND ", c.sb[all_sb[k]], " ###"
                         print,'Arbitrary system temperature of 400 K is inserted for this antenna'
                         print,'and data on associated baselines will be flagged bad'
                         wtsys[zerolist[iz],tosb] = 400.0
                         ant_wflag[zerolist[iz],tosb] = 0
                       endif
                     endelse
                   endfor
                 endif

               endif else begin
                 print,'No solution for wideband ',c.sb[all_sb[k]],' at integration ',int_list[i],'.'
                 print,'Arbitrary system temperature of 400 K is inserted for all antennas'
                 print,'and data will be flagged bad'
                 wtsys[(distinct_tels[*]-1),tosb] = 400.0
               endelse

;               if (e.debug) then begin
;                 for an1=0,(nants-2) do begin
;                   for an2=(an1+1),(nants-1) do begin
;                     sqrttssb=sqrt(wtsys[an1,tosb]*wtsys[an2,tosb])
;                     if (sqrttssb ne 0) then begin
;                       print, an1,an2,wtsys[an1,tosb],wtsys[an2,tosb],wbtsys[an1,an2,tosb]/sqrttssb
;                     endif
;                   endfor
;                 endfor
;                 print, "wband tsys flag is ",wsolflag[tosb], "at int", int_list[i]
;               endif

             endfor

          ; narrow band (spectral window)

             for k=0,(nspect-1) do begin

               specsb   = (speccode[k]/100)
               specband = speccode[k] - specsb * 100

               resultx=max(total(nbflag[*,*,k],1),nbflagx)
               resulty=max(total(nbflag[*,*,k],2),nbflagy)

               if (resultx ge resulty) then begin
                 refant=nbflagx
                 antl=where(nbflag[*,refant,k] gt 0)
               endif else begin
                 refant=nbflagy
                 antl=where(nbflag[refant,*,k] gt 0)
               endelse

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
                 if (nbflag[an1,an2,k] eq 1) then begin
                    tref=nbtsys[refant,an1,k]*nbtsys[refant,an2,k]/nbtsys[an1,an2,k]
                    if (e.debug) then print,refant,an1,an2,k,tref
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

               ; narrow band tsys
               if (solflag[k] eq 1) then begin
                 ; channel tsys
                 tsys[*,k] =  nbtsys[refant,*,k]^2/tref
                 tsys[refant,k] = tref
                 ant_flag[*,k] = 1L

                 ; pseudo continuum tsys
                 wtsys[*,2+k] =  nbtsys[refant,*,k]^2/tref
                 wtsys[refant,2+k] = tref
                 ant_wflag[*,2+k] =1L

                 zerolist = where((tsys[*,k] eq 0) and (inflag[*] eq 1), zerocnt)
                 if (zerocnt gt 0) then begin
                   for iz = 0, zerocnt -1 do begin
                     tmpxlist = where((nbtsys[*,zerolist[iz],k]) gt 0, tmpxcnt)
                     tmpylist = where((nbtsys[zerolist[iz],*,k]) gt 0, tmpycnt)
                     if (tmpxcnt eq 0 and tmpycnt eq 0) then begin
                       ;print, " ### NOTE: NO TSYS DATA FOR ANT ",(zerolist[iz]+1), " AT INT ",int_list[i], $
                       ;       "  SIDEBAND ", c.sb[specsb]," CHUNK ", c.band[specband], " ###"
                       ;print,'Arbitrary system temperature of 400 K is inserted for this antenna'
                       ;print,'and data on associated baselines will be flagged bad'
                       tsys[zerolist[iz],k] = 400.0
                       ant_flag[zerolist[iz],k] = 0
                       wtsys[zerolist[iz],2+k] =  400.0
                       ant_wflag[zerolist[iz],2+k] = 0
                     endif else begin
                       zsol=0
                       if (tmpxcnt gt 0 and zsol eq 0) then begin
                         itmp = 0
                         while (zsol eq 0 and itmp lt tmpxcnt) do begin
                           if (ant_flag[tmpxlist[itmp],k] eq 1) then begin
                             tsys[zerolist[iz],k] =  nbtsys[tmpxlist[itmp],zerolist[iz],k]^2/tsys[tmpxlist[itmp],k]
                             wtsys[zerolist[iz],2+k] = tsys[zerolist[iz],k]
                             zsol=1
                           endif
                           itmp = itmp + 1
                         endwhile
                       endif

                       if (tmpycnt gt 0 and zsol eq 0) then begin
                         itmp = 0
                         while (zsol eq 0 and itmp lt tmpycnt) do begin
                           if (ant_flag[tmpylist[itmp],k] eq 1) then begin
                             tsys[zerolist[iz],k] =  nbtsys[zerolist[iz],tmpylist[itmp],k]^2/tsys[tmpylist[itmp],k]
                             wtsys[zerolist[iz],2+k] = tsys[zerolist[iz],k]
                             zsol=1
                           endif
                           itmp = itmp + 1
                         endwhile
                       endif

                       if (zsol eq 0) then begin
                         ;print, " ### NOTE: NOT SOLVABLE FOR ANT ",(zerolist[iz]+1), " AT INT ",int_list[i], $
                         ;       "  SIDEBAND ", c.sb[specsb]," CHUNK ", c.band[specband], " ###"
                         ;print,'Arbitrary system temperature of 400 K is inserted for this antenna'
                         ;print,'and data on associated baselines will be flagged bad'
                         tsys[zerolist[iz],k] = 400.0
                         ant_flag[zerolist[iz],k] = 0
                         wtsys[zerolist[iz],2+k] =  400.0
                         ant_wflag[zerolist[iz],2+k] = 0
                       endif
                     endelse
                   endfor
                 endif

               endif else begin
                 print,'No solution for narrowband ',k,' at integration ',int_list[i],'.'
                 print,'Arbitrary system temperature of 400 K is inserted for all antennas'
                 print,'and data will be flagged bad'
                 ; channel tsys
                 tsys[(distinct_tels[*]-1),k] = 400.0
                 ; pseudo continuum tsys
                 wtsys[(distinct_tels[*]-1),2+k] =  400.0
               endelse

             endfor

       wtsys2 = reform(wtsys,nants*(2+nspect))
       tsys2 = reform(tsys,nants*nspect)

       result=CALL_EXTERNAL(libfile, $
                            'idl_uvputvr',unit,'r','wsystemp',wtsys2, long(n_elements(wtsys2)))


       result=CALL_EXTERNAL(libfile, $
                            'idl_uvputvr',unit,'r','systemp',tsys2, long(n_elements(tsys2)))

       if (e.debug) then print, "wband tsys flag is ",wsolflag, "at int", int_list[i]
       if (e.debug) then print, "total", nbsl, " baselines for int ", int_list[i]

       if (verbose) then print,'  - cycling through all baselines again to store header/data -'
       for j=0,(nbsl-1) do begin

         preamble = make_array(4,/double)
         if (verbose) then print,'    BSL ',c.blcd[all_bsl[j]]

         for jpol = 0, (npol -1) do begin
           if (verbose) then print, '    POL ',all_pol[jpol]
   
           m = where(bl[pbl[l]].iblcd eq all_bsl[j] and bl[pbl[l]].ipol eq all_pol[jpol])
           if (verbose) then print, 'n_elements(m) : ',n_elements(m)

              ; inserting info for this integration/baseline/polarization
  
              thispol = mirpol[all_pol[jpol]]

 ;              mirpol = long(bl[pbl[l[m[0]]]].ipol)
 ;              if(polar eq 0) then thispol = long(-5)     ; no polcode
 ;              if(polar eq 1) then begin
 ;                if(mirpol eq 1) then thispol = long(-1) ; rr
 ;                if(mirpol eq 2) then thispol = long(-3) ; rl
 ;                if(mirpol eq 3) then thispol = long(-4) ; lr
 ;                if(mirpol eq 4) then thispol = long(-2) ; ll
 ;              endif
 ;              if(polar eq 2) then begin
 ;                if(mirpol eq 1) then thispol = long(-5) ; xx
 ;                if(mirpol eq 2) then thispol = long(-7) ; xy
 ;                if(mirpol eq 3) then thispol = long(-8) ; yx
 ;                if(mirpol eq 4) then thispol = long(-6) ; yy
 ;              endif

               result=CALL_EXTERNAL(libfile, $
                          'idl_uvputvr',unit,'i','pol',thispol, one)

              ; visibility header/data
;               preamble(0) = - ((bl(pbl(l(m(0)))).u * 1e3) * (!cvel/(basefreq[0]*1e9)) / (!cvel*1e-9))
;               preamble(1) = - ((bl(pbl(l(m(0)))).v * 1e3) * (!cvel/(basefreq[0]*1e9)) / (!cvel*1e-9))

               if (bl(pbl(l(m(0)))).isb eq 0) then begin
                 preamble(0) = - ((bl(pbl(l(m(0)))).u * 1e3) / basefreq[0])
                 preamble(1) = - ((bl(pbl(l(m(0)))).v * 1e3) / basefreq[0])
               endif

               if (bl(pbl(l(m(0)))).isb eq 1) then begin
                 preamble(0) = - ((bl(pbl(l(m(0)))).u * 1e3) / basefreq[1])
                 preamble(1) = - ((bl(pbl(l(m(0)))).v * 1e3) / basefreq[1])
               endif

               preamble(2) = jd

               ant1 = bl[pbl(l(m(0)))].itel1
               ant2 = bl[pbl(l(m(0)))].itel2

               if (verbose) then print, "ANTS", ant1, ant2

               preamble(3) = 256*ant1+ant2
               if (e.debug) then print, ant1,ant2, preamble

  ;             stop

               wdata=make_array(nwide,/complex)
               wflags = make_array(nwide,/long, value=0)

               data=make_array(numchan,/complex)
               flags = make_array(numchan,/long, value=0)

               chunkcount=0
               chancount=0

               ; wide bands
               if (verbose) then print,'  - continuum channel -'

               for k=0,(nsb-1) do begin

                 n = where(bl[pbl[l[m]]].isb eq all_sb[k] and sp[psl[l[m]]].iband eq 0, ncount)
                 if ncount le 0 then begin
                     print,'STOP! PLEASE OUTPUT INDIVIDUAL SIDEBAND SEPARATELY!'
                     return
                 endif
                 if (all_sb[k] eq 0) then tosb = 0 ; lsb
                 if (all_sb[k] eq 1) then tosb = 1 ; usb
               
                 if (ncount gt 0) then begin
                   uti_conv_apc, complexpair, bl[pbl[l[m[n[0]]]]].ampave,-bl[pbl[l[m[n[0]]]]].phaave, /complex
                   wdata[tosb]=complexpair
                   wflags[tosb]=(1+re.wts[prl[l[m[n]]]]/abs(re.wts[prl[l[m[n]]]]))/2 * ant_wflag[(ant1-1),tosb]*ant_wflag[(ant2-1),tosb]
                 endif

                 if (e.debug eq 1) then begin
                   if (wflags[tosb] eq 0) then print, "wideband ",tosb," is flagged at int", int_list[i]
                 endif
               endfor

               if (keyword_set(oldweight)) then begin
                 temp_jyperk = jyperk
               endif else begin
                 recweight = sp[psl[l[m[n[0]]]]].wt
                 recint = sp[psl[l[m[n[0]]]]].integ
                 temp_jyperk = sqrt(2*recint*abs(wwidth[tosb])*1e9/(recweight * (wtsys[ant1-1,tosb]*wtsys[ant2-1,tosb])))
               endelse

               ; narrow bands
               if (verbose) then print,'  - spectral window -'

               chancount = 0
               for k=0,(nspect-1) do begin

                 specsb   = (speccode[k]/100)
                 specband = speccode[k] - specsb * 100

                 n = where(bl[pbl[l[m]]].isb eq specsb and sp[psl[l[m]]].iband eq specband, ncount)

                 if (ncount gt 0) then begin
                   data[chancount:chancount+(nschan[k]-1)]=conj(ch[pcl[l[m[n[0]]]]:pcl[l[m[n[0]]]]+nschan[k]-1])
                   flagval = (1+sp[psl[l[m[n[0]]]]].wt/abs(sp[psl[l[m[n[0]]]]].wt))/2 * ant_flag[(ant1-1),k]*ant_flag[(ant2-1),k]

                   flags[chancount:chancount+(nschan[k]-1)] = flagval


                   ; creating pseudo-continuum
                   chan_trim = floor(nschan[k]*edge_trim)
                   ; if trimming possible, then trim, if not, then just sum up without trimming
                   if (chancount+chan_trim le chancount+(nschan[k]-chan_trim-1)) then begin
                     wdata[2+k] = total(data[chancount+chan_trim:chancount+(nschan[k]-chan_trim-1)])/(nschan[k]-2*chan_trim)
                   endif else begin
                     wdata[2+k] = total(data[chancount:chancount+(nschan[k]-1)])/nschan[k]
                   endelse

                   wflags[2+k] = flagval
                 endif

                 if (ncount le 0) then begin
                   data[chancount:chancount+(nschan[k]-1)]=0
                   flags[chancount:chancount+(nschan[k]-1)] = 0

                   ; creating sudo-continuum
                   wdata[2+k] = 0
                   wflags[2+k] = flagval
                 endif

                 chancount = chancount + nschan[k]

;                recweight = sp[psl[l[m[n[0]]]]].wt
;                recint = sp[psl[l[m[n[0]]]]].integ
;                temp_jyperk = sqrt(2*recint*abs(sdf[k])*1e9/(recweight * (tsys2[ant1-1]*tsys2[ant2-1])))

               endfor

               ;print,ant1,ant2,temp_jyperk

               result=CALL_EXTERNAL(libfile, $
                                'idl_uvputvr',unit,'r','jyperk',temp_jyperk,one)

               result=CALL_EXTERNAL(libfile, $
                                'idl_uvwwrite',unit,wdata,wflags,nwide)
;               bugtrim=where((real_part(data)) gt 1000 or abs(imaginary(data)) gt 1000, trimcount)
;               print, 'index of spextra data got trimmed out in this baseline', bugtrim
;               if (trimcount gt 0) then data(bugtrim) = 0
               itrim=where(abs(data) gt trimflux,trimcount)
               if (trimcount gt 0) then begin
                  fluxwarning=1
;                  print,'Index of spectra which got reset to 0 due to their amplitude higher than',trimflux,'(=trimflux):'
;                  print,itrim
;                  print,'Reset keyword "trimflux" if needed.'
;                  print,''
                  data[itrim]=0
               endif

               result=CALL_EXTERNAL(libfile, $
                                'idl_uvwrite',unit,preamble,data,flags,numchan)
         endfor

       endfor

    endfor

  print,"--- Closing up data directory ---"

    result=CALL_EXTERNAL(libfile, $
                     'idl_uvclose',unit)
  if fluxwarning eq 1 then begin
     print,''
     print,'WARNING! WARNING!'
     print,'Some channels got reset to 0 due to their amplitude higher than',trimflux,'(=trimflux).'
     print,'Reset keyword "trimflux" if needed.'
     print,''
  endif
  ; all done
  print,'Done!'

end

