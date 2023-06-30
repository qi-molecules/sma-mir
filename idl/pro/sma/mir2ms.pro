pro mir2ms, dir=dir, rx=rx, sideband=sideband, mymir=mymir, engdbfile=engdbfile, casa_call=casa_call, outname=outname, flipsign=flipsign, noms=noms
;+
; NAME:
;      mir2ms
; PURPOSE:
;      to convert mir data into  CASA measurement sets.
; EXPLANATION:
;      1. This program is based on Todd Hunter's program uti_to_topo_complete.pro which puts 
;         the visibility data into a true TOPO frame, like ALMA and VLA use. 
;      2. MIR data version v4 and later can be used directly for conversion.
;      3. For MIR data earlier than v4, engineering database file is needed: 
;            engdbfile: name of ASCII file exported from SMA engineering database
;                       which needs to contain DDS_TO_HAL_X:FREQ_V3_D 
;                           DSM_AS_IFLO_EROTAT_D
;                           DSM_AS_IFLO_VRADIAL_D 
;                           DSM_AS_IFLO_RA_D (if raref not set)
;                           DSM_AS_IFLO_DEC_D (if decref not set)
;                           DSM_AS_IFLO_SOUR_C34 (if reference not set)
;                           DSM_AS_IFLO_VELO_D (if vlsrk not set)
; NOTICE:
;      1. please work in your own work directory and it will create a lot of temporary files
;      2. need sma.py file for correct CASA ms concatenation
; EXMAPLE:
;      IDL> mir2ms, dir=<datadir>, rx=<receiver selection>
;      IDL> mir2ms, /mymir
;      IDL> mir2ms, engdbfile=<database file>
; OUTPUT: 
;      CASA measurement sets
; INPUT:
;      dir,rx - data location, receiver (230,345,240,400) selection.
;               if not set, use the current filtered mir data.
;      mymir  - the flag to use mymir.pro as the user's own mir commands.
;               default procedures include continuum regeneration and
;               tsys correction.
;      engdbfile - engineering database file for mir version earlier than v4.
;      casa_call - local machine casa command name, 'casa' by default
;      outname  - output ms name, 'out' by default
;      flipsign - the flag to flip the sign.
; HISTORY:
;      Todd Hunter    Nov-2019 (Topo frame conversion)
;      Chunhua Qi     Mar-2021 (MIR v4 data update)

common global
common data_set

; set up the CASA directory and output ms file name, out.ms is the default
if not keyword_set(outname) then outname='out'
if not keyword_set(casa_call) then casa_call='casa'

; set up parameters
sign=-1.
if keyword_set(flipsign) then sign=1.
SECONDS_PER_DAY=24.*60.*60.
globalRefLong=155.477522d*!dpi/180.d
globalRefLat=19.82420526391d*!dpi/180.d
globalRefRadius=6382.248*1000.d
M_PER_KM=1000.d
longitude=-155.477522d

journal

; load the mir data if necessary
if keyword_set(dir) then begin
   nchar = strlen(dir)
   checkslash = strmid(dir,nchar-1,1)
   if ( strmatch(checkslash,'/') le 0 ) then dir = dir + '/'
   e.idl_bcp=dir
   readdata,dir=dir,rx=rx,sideband=sideband
endif


; check the data criteria

if c.filever[0] lt 3 then begin
   if not keyword_set(engdbfile) then begin
      print,'Need engineering input files to proceed !'
      print,'Quit !'
   endif
endif else begin
   raref=in[0].vrra
   decref=in[0].vrdec
endelse


; run user's mir commands if there is one
if keyword_set(mymir) then begin
  mymir
endif else begin
  print,'Running default mir commands to regenerate continuum and apply tsys...'
  select,/p,/re
  uti_avgband
  apply_tsys
endelse

; separate rx 
result=dat_filter(s_f,'"wt" gt "0"',/reset,/no_notify)
distinct_rec=c.rec[uti_distinct(bl[pbf].irec,nrec,/many)]
for ir=0, nrec-1 do begin
   result=dat_filter(s_f,'"rec" eq "'+distinct_rec[ir]+'" and "wt" gt "0"',/reset,/no_notify)
   crx=distinct_rec[ir]
   print, 'Working on receiver: ',crx,+'...'

   if crx eq '230' or crx eq '345' then useLOFreq =1 else useLOFreq = 2
   print,'useLOFreq is ',useLOFreq

; now start working on TOPO frame conversion

; mJD
   datobs=c.ref_time[in[0].iref_time]
   months = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
   mon=strmid(datobs,0,3)
   j=where(mon eq months,count)
   num_day_obs=[fix(strtrim(strmid(datobs,8,4),2)),fix(strtrim(string(j[0]+1),2)),(strtrim(strmid(datobs,4,2),2))]
   day=strtrim(string(num_day_obs[2]),2)
   yr =strtrim(string(num_day_obs[0]),2)
   mo =strtrim(string(num_day_obs[1]),2)

; engdb reading and lotransit setting


   if keyword_set(engdbfile) then begin

; Free up the file descriptors
;      for i=100,128 do begin
;         free_lun, i
;      endfor
; Read the engineering database file 
      openr, lun, engdbfile, /get_lun
      header = strarr(10)
      readf, lun, header
      reads, header(7), rows, format='(x,I0)'
      print, 'header = ', header
      print, 'number of rows = ', rows
      engvalue = {UNIXTIME: lonarr(rows), C01: dblarr(rows), C02: dblarr(rows)}
      a = strarr(1)
      for j=0, rows-1 do begin
         readf, lun, a
         b = strsplit(a, ' ', /extract)
         engvalue.UNIXTIME(j) = b[0]
         engvalue.C01(j) = b[4]
         engvalue.C02(j) = b[5]
      endfor
      print,'unixtime(0) = ', engvalue.UNIXTIME(0)
      print,'lo1(0) = ', engvalue.C01(0), format='(A,F16.3)'
      print,'lo2(0) = ', engvalue.C02(0), format='(A,F16.3)'

      if eof(lun) then begin
         print,'This database file has only the FREQ_V3_D values. Command-line values and defaults will be used for other parameters.'
      endif else begin
         header = strarr(7)
         readf, lun, header
         print,'header = ', header
         reads, header(4), rows, format='(x,I0)'
         erotate = {UNIXTIME: lonarr(rows), C00: dblarr(rows)}
         for j=0, rows-1 do begin
            readf, lun, a
            b = strsplit(a, ' ', /extract)
            erotate.UNIXTIME(j) = b[0]
            erotate.C00(j) = b[3]
         endfor
         middle = round(n_elements(erotate.C00)/2)
         erotat = erotate.C00(middle)
         print,'erotat = ', erotat ;  meters/sec

         header = strarr(7)
         readf, lun, header
         print,'header = ', header
         reads, header(4), rows, format='(x,I0)'
         vrad = {UNIXTIME: lonarr(rows), C00: dblarr(rows)}
         for j=0, rows-1 do begin
            readf, lun, a
            b = strsplit(a, ' ', /extract)
            vrad.UNIXTIME(j) = b[0]
            vrad.C00(j) = b[3]
         endfor
         middle = round(n_elements(vrad.C00)/2)
         vradial = vrad.C00(middle)
         print,'vradial = ', vradial ; meters/sec

         header = strarr(7)
         readf, lun, header
         print,'header = ', header
         reads, header(4), rows, format='(x,I0)'
         ra = {UNIXTIME: lonarr(rows), C00: dblarr(rows)}
         for j=0, rows-1 do begin
            readf, lun, a
            b = strsplit(a, ' ', /extract)
            ra.UNIXTIME(j) = b[0]
            ra.C00(j) = b[3]
         endfor
         middle = round(n_elements(ra.C00)/2)
         raref = ra.C00(middle)
         print,'raref = ', raref

         header = strarr(7)
         readf, lun, header
         print,'header = ', header
         reads, header(4), rows, format='(x,I0)'
         dec = {UNIXTIME: lonarr(rows), C00: dblarr(rows)}
         for j=0, rows-1 do begin
            readf, lun, a
            b = strsplit(a, ' ', /extract)
            dec.UNIXTIME(j) = b[0]
            dec.C00(j) = b[3]
         endfor
         middle = round(n_elements(dec.C00)/2)
         decref = dec.C00(middle)
         print,'decref = ', decref

         header = strarr(7)
         readf, lun, header
         print,'header = ', header
         reads, header(4), rows, format='(x,I0)'
         source = {UNIXTIME: lonarr(rows), UTC: strarr(rows), HST: strarr(rows), C00: strarr(rows)}
         for j=0, rows-1 do begin
            readf, lun, a
            b = strsplit(a, ' ', /extract)
            source.UNIXTIME(j) = b[0]
            source.C00(j) = b[3]
         endfor
         middle = round(n_elements(source.C00)/2)
         reference = source.C00(middle)
         print,'source = ', reference

         header = strarr(7)
         readf, lun, header
         print,'header = ', header
         reads, header(4), rows, format='(x,I0)'
         source = {UNIXTIME: lonarr(rows), UTC: strarr(rows), HST: strarr(rows), C00: strarr(rows)}
         for j=0, rows-1 do begin
            readf, lun, a
            b = strsplit(a, ' ', /extract)
            source.UNIXTIME(j) = b[0]
            source.C00(j) = b[3]
         endfor
         middle = round(n_elements(source.C00)/2)
         vlsrk = source.C00(middle)
         print,'Vlsrk (m/s) = ', vlsrk
      endelse

      transitTime = call_function('transit_time', rarad=raref, longitude=-155.477522, unixtime=source.UNIXTIME(0))
      print,'transit = ', transitTime, format='(A0,F15.3)'

; Restore the template for reading the ASCII file saved from the SMA
; engineering database: DDS_TO_HAL_X:FREQ_V3_D for which data starts
; on eleventh line
;restore, 'freq_template.sav'
;engvalue = read_ascii(engdbfile, template=template)
      idx = value_locate(engvalue.UNIXTIME, transitTime)
      print, 'Read ', size(engvalue.UNIXTIME), ' lines'
      print, 'array index of the specified time closest to transit: ', idx
      lo1transit = engvalue.C01[idx]
      print, format='(A,F20.1)', 'lo1transit: ', lo1transit
      lo2transit = engvalue.C02[idx]

      v = abs(erotate.UNIXTIME - transitTime)
; find the array index of the closest entry in the engineering database
      argmin = array_indices(v, where(v EQ min(v)))
      erotat=erotate.C00[argmin]
      print,'Earth rotation at transit = ',erotat

      vc = vradial - vlsrk

   endif else begin

      cblcd=c.blcd[bl[pbf[0]].iblcd]
      csb=c.sb[bl[pbf[0]].isb]
;      crx=distinct_rec[i]

      result=dat_filter(s_f,'"wt" gt "0" and "blcd" eq "'+cblcd+'" and "sb" eq "'+csb+'" and "band" eq "c1" and "rec" eq "'+crx+'"',/reset,/no_notify)
      c0=1e9*(sp[psf].gunnlo+sp[psf].fdds)
      ut=in[pif].dhrs
      myunixtime = ymdut_to_unixtime(yr,mo,day,ut)
      transitTime=transit_time(rarad=raref, longitude=longitude,unixtime=myunixtime[0])
      print,'transit = ', transitTime, format='(A0,F15.3)'
      lotransit=interpol(c0,myunixtime,transitTime)
      print, format='(A,F20.1)', 'lotransit: ', lotransit
      
      vc = in[0].vc * double(1000.)
      print, 'vc = ', vc

   endelse

; Topo conversion starts...
   print, ''
   print, 'Topo starts ...'
   print, 'Using only subbands starting with "s"'
   
   result=dat_filter(s_f,'"wt" gt "0" and "band" like "s" and "rec" eq "'+crx+'"',/reset,/no_notify)

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
;      print,'Fixing integration ',ii[i], " which has ", ncombo, " combinations"

      if keyword_set(engdbfile) then begin
         ut=in[pifs[[tmp_idx[0]]]].dhrs
         myunixtime = ymdut_to_unixtime(yr,mo,day,ut)
         v = abs(engvalue.UNIXTIME - myunixtime)
; find the array index of the closest entry in the engineering database
         argmin = array_indices(v, where(v EQ min(v)))
         if useLOFreq EQ 1 then begin
            df = lo1transit - engvalue.C01[argmin]
         endif else begin
            df = lo2transit - engvalue.C02[argmin]
         endelse
;         print,'df is ',df
;         read,iii
      endif else begin
         df=lotransit-(sp[psfs[[tmp_idx[0]]]].fdds+sp[psfs[[tmp_idx[0]]]].gunnlo)*1e9
;         print,'df is ',df
;         read,iii
      endelse

      for j=0L,ncombo-1L do begin
         nc=sp[psfs[[tmp_idx[j]]]].nch
         my_fres_hz = sp[psfs[[tmp_idx[j]]]].fres * 1000000
         my_fsky = sp[psfs[[tmp_idx[j]]]].fsky *1e9
         fsh = -1.*sign*(-df/my_fres_hz)

         fsh += 1.*sign*my_fsky*(vc/299792458.0D)/my_fres_hz

         data_ch=ch[pcfs[[tmp_idx[j]]]:pcfs[[tmp_idx[j]]]+nc-1]
                                ; findgen(nc) returns an array of [0,1,2,3...]
         data_out=interpolate(data_ch,findgen(nc)-fsh[0], cubic=-0.5)
         ch[pcfs[[tmp_idx[j]]]:pcfs[[tmp_idx[j]]]+nc-1]=data_out
      endfor
;   print,'fsh is ',fsh
;   read,iii
   endfor

   result=dat_filter(s_f,'"wt" gt "0" and "rec" eq "'+crx+'"',/reset,/no_notify) 
   in[pif].epoch=2000.
; vesta has a 2021.9 epoch which is not accepted by importuvfits in casa


   distinct_source=c.source[uti_distinct(in[pif].isource,nsources,/many)]

   for i=0, nsources-1 do autofits,source=distinct_source[i]

;sourcelist=distinct_source+[REPLICATE(',', N_ELEMENTS(distinct_source)-1), ''] 
   sourcelist=strjoin(strlowcase(distinct_source),',')

   if not keyword_set(sideband) then outms=outname+'.rx'+crx+'.ms' else begin
       csb=c.sb[bl[pbf[0]].isb]
       outms=outname+'.rx'+crx+'.'+csb+'sb.ms'
   endelse
   if keyword_set(noms) then outms=outname+'.ms'
   openw, outunit,'uvfits2ms.py',/get_lun
   printf,outunit,'import sys'
   printf,outunit,'import os'
   line='sys.path.append("'+e.idl_sav+'")'
   printf,outunit,line
   line='from sma import concatSMAdataset'
   printf,outunit,line
   ;printf,outunit,line
   line="concatSMAdataset(targetlist='"+sourcelist+"',finalvis='"+outms+"',offset=100)"
   printf,outunit,line
   line='from sma import modifyCorrType'
   printf,outunit,line
   line="modifyCorrType('"+outms+"')"
   printf,outunit,line
   printf,outunit,"os.system('rm -rf *.part')"
   printf,outunit,"os.system('rm -rf *.UVFITS')"
   free_lun,outunit
   
   cmd=casa_call+' --nologger --log2term -c uvfits2ms.py'
   if not keyword_set(noms) then begin
       spawn,cmd
       print,'Receiver ',crx,' data output to ',outms,'!'
   endif else begin
       print,'***************************************************************'
       print,'* Only UVFITS output, no measurement set...                   *' 
       print,'* You can use uvfits2ms.py to output measurement set manually!*'
       print,'***************************************************************'
   endelse   

endfor 

journal
end



