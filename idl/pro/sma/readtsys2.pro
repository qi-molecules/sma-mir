pro readtsys2, dir=dir, rx=rx, sideband=sideband, replace=replace, spectsys=spectsys

common global
common data_set

if keyword_set(dir) then begin
   nchar = strlen(dir)
   checkslash = strmid(dir,nchar-1,1)
   if ( strmatch(checkslash,'/') le 0 ) then dir = dir + '/'
   e.idl_bcp=dir
endif
count=0

;if not keyword_set(dir) then dir=e.idl_bcp
result = findfile(e.idl_bcp+'tsys_read',count=count)
if (count eq 0) then begin
   print,'could not find file ',e.idl_bcp+'tsys_read'
   return
endif

badant=0
if keyword_set(replace) then begin
   print,'Replace Tsys values for antenna x with values from antenna y:'
   read,badant,prompt='x = '
   read,goodant,prompt='y = '
endif

print,'******** OS TYPE ***********'
print,'Platform =',!VERSION.ARCH
print,'TSYS reading ...'
;print,'This TSYS reading program will work on the full dataset' 
;print,'     and ignore any data filtering....'
;
;  Set up structures for tsys header table
;
;  Assumptions: 
;  1. the correlator setup is fixed for the whole track -
;  2. tsys file contains the tsys information from antennas used in the track
distinct_iband=uti_distinct(sp.iband,nbands,/many_repeat)
dist_irec=uti_distinct(bl.irec,nrecs,/many_repeat)

itmp=where(dist_irec ne -1, ncount)
if ncount ne nrecs then begin
   nrecs=nrecs-1
   distinct_irec=dist_irec[itmp]
   result=dat_filter(s_f,'"irec" eq "-1"',/reset,/no_notify)
   sp[psf].tssb=9999.
endif else distinct_irec=dist_irec

if nrecs gt 2 then begin
   print,'*****',nrecs, ' receivers data recorded.'
   print,'Please fix the receiver header first ' 
   print,' before using readtsys2 to load tsys again!'
   print,'Quit!'
   return
endif

openr,unit,e.idl_bcp+'tsys_read',/get_lun,error=err
if err ne 0 then begin
  print,!err_string
  return
endif

nM=0L

sb_indx=0
if keyword_set(sideband) then if sideband eq 'u' then sb_indx=1 

result=dat_filter(s_f,/reset,/no_notify)
ints=bl[pbf].inhid
ii=uti_distinct(in[pif].inhid,nint,/many_repeat)

for i=0L,nint-1L do begin
   ; 1. different int might have different ants.
   ; 2. for individual int, assign tsys for available ants.
   ; 3. assume tsys are same for both sidebands.
   tmp_idx = where(ints eq ii[i], ncombo)
   ants=[bl[pbf[tmp_idx[0]]].itel1,uti_distinct(bl[pbf[tmp_idx]].itel2,/many)]
   nants=n_elements(ants)
   ibsl=where((sp[psf[tmp_idx]].iband eq 0) and bl[pbf[tmp_idx]].isb eq sb_indx,count); ibsl: baseline index
   first_byte=bl[pbf[tmp_idx[ibsl[0]]]].ant1tsysoff
   point_lun,unit,first_byte
   readu,unit,nM
   if nM gt 2 and not keyword_set(spectsys) then begin
   ;   print,'************* More than ',nM,' TSYS measurements'
   ;   print,'************* Need to check the routine. Quit !'
   ;   return
       print,'Spectral tsys reading not ready !'
       print,'Switching to tsys reading from eng_read:'
       readtsys_eng,dir=dir
       print,'Tsys reading from ENG_READ is done!'
       return
   endif
   data=fltarr(4,nM)
   tsys_temp={itel:0,tssb:fltarr(nM)}
   tsys = replicate(tsys_temp,nants)
   readu,unit,data
                                ; for normal data, nM=2 (both IFs or RXs):
                                ; data[2,*] for lsb and data[3,*] for usb
;print,data[2,*]
;print,data[3,*]
;read,iii
   tsys[0].itel=bl[pbf[tmp_idx[ibsl[0]]]].itel1
   tmp_tsys=data[2,*]
   tmpi=where(tmp_tsys eq 0,count)
   if count gt 0 then tmp_tsys[tmpi]=99999.
   tsys[0].tssb=tmp_tsys
;   tsys[0].tssb=data[2,*] ; assuming data[2,*] (lsb) = data[3,*] (usb)
   for iant=1, nants-1 do begin
      first_byte=bl[pbf[tmp_idx[ibsl[iant-1]]]].ant2tsysoff
      point_lun,unit,first_byte
      readu,unit,nM
      data=fltarr(4,nM)
      readu,unit,data
      tsys[iant].itel=bl[pbf[tmp_idx[ibsl[iant-1]]]].itel2
      tmp_tsys=data[2,*]
      tmpi=where(tmp_tsys eq 0,count)
      if count gt 0 then tmp_tsys[tmpi]=99999.
      tsys[iant].tssb=tmp_tsys
;      tsys[iant].tssb=data[2,*] ; assuming data[2,*] (lsb) = data[3,*] (usb)
   endfor
; done with tsys reading, change tsys for replacement if necessary

   if badant then begin
      igood=where(tsys.itel eq goodant, count)
      if count le 0 then begin
         print, "No antenna y=", goodant
         return
      endif   
      goodtssb=tsys[igood].tssb

      ibad=where(tsys.itel eq badant,count)
      if count le 0 then begin
         print, "No antenna x=", badant
         return
      endif
      tsys[ibad].tssb=goodtssb
   endif

   for j=0,nants-2 do begin
      for k=j+1,nants-1 do begin
         if nrecs eq 2 then begin
            ; double rx
            loc=where(bl[pbl[tmp_idx]].itel1 eq ants[j] and bl[pbl[tmp_idx]].itel2 eq ants[k] and bl[pbl[tmp_idx]].irec eq distinct_irec[0], count)
            if count gt 0 then begin
               if nM eq 2 then begin
                  jj=where(tsys.itel eq ants[j],nbands)
                  tmpj=(tsys[jj].tssb[0] gt 0) ? tsys[jj].tssb[0]: 99999.
                  kk=where(tsys.itel eq ants[k])
                  tmpk=(tsys[kk].tssb[0] gt 0) ? tsys[kk].tssb[0]: 99999.
                  sp[psl[tmp_idx[loc]]].tssb=sqrt(tmpj*tmpk)
               endif else begin
                  jj=where(tsys.itel eq ants[j],nbands)
                  tmpj=tsys[jj].tssb[0:nM/2-1]
                  tmpj=[mean(tmpj),tmpj]
		  tmpj=[tmpj,tmpj]
                  kk=where(tsys.itel eq ants[k])
                  tmpk=tsys[kk].tssb[0:nM/2-1]
                  tmpk=[mean(tmpk),tmpk]
                  tmpk=[tmpk,tmpk]
                  sp[psl[tmp_idx[loc]]].tssb=sqrt(tmpj*tmpk)*2.
               endelse
            endif           
            loc=where(bl[pbl[tmp_idx]].itel1 eq ants[j] and bl[pbl[tmp_idx]].itel2 eq ants[k] and bl[pbl[tmp_idx]].irec eq distinct_irec[1], count)
            if count gt 0 then begin
               if nM eq 2 then begin
                  jj=where(tsys.itel eq ants[j])
                  tmpj=(tsys[jj].tssb[1] gt 0) ? tsys[jj].tssb[1]: 99999.
                  kk=where(tsys.itel eq ants[k])
                  tmpk=(tsys[kk].tssb[1] gt 0) ? tsys[kk].tssb[1]: 99999.
                  sp[psl[tmp_idx[loc]]].tssb=sqrt(tmpj*tmpk)
               endif else begin
                  jj=where(tsys.itel eq ants[j],nbands)
                  tmpj=tsys[jj].tssb[nM/2:nM-1]
                  tmpj=[mean(tmpj),tmpj]
                  tmpj=[tmpj,tmpj]
                  kk=where(tsys.itel eq ants[k])
                  tmpk=tsys[kk].tssb[nM/2:nM-1]
                  tmpk=[mean(tmpk),tmpk]
                  tmpk=[tmpk,tmpk]
                  sp[psl[tmp_idx[loc]]].tssb=sqrt(tmpj*tmpk)*2.
               endelse
            endif
                       
         endif else begin
            ; rx =400
            if float(rx) eq 400 or float(rx) eq 240 then begin
               loc=where(bl[pbl[tmp_idx]].itel1 eq ants[j] and bl[pbl[tmp_idx]].itel2 eq ants[k], count)
               if count gt 0 then begin
                  if nM eq 2 then begin
                     jj=where(tsys.itel eq ants[j])
                     tmpj=(tsys[jj].tssb[1] gt 0) ? tsys[jj].tssb[1]: 99999.
                     kk=where(tsys.itel eq ants[k])
                     tmpk=(tsys[kk].tssb[1] gt 0) ? tsys[kk].tssb[1]: 99999.
                     sp[psl[tmp_idx[loc]]].tssb=sqrt(tmpj*tmpk)
                  endif else begin
                     jj=where(tsys.itel eq ants[j],nbands)
                     tmpj=tsys[jj].tssb[nM/2:nM-1]
                     tmpj=[mean(tmpj),tmpj]
                     tmpj=[tmpj,tmpj]
                     kk=where(tsys.itel eq ants[k])
                     tmpk=tsys[kk].tssb[nM/2:nM-1]
                     tmpk=[mean(tmpk),tmpk]
                     tmpk=[tmpk,tmpk]
                     sp[psl[tmp_idx[loc]]].tssb=sqrt(tmpj*tmpk)*2.
                  endelse
               endif
            endif else begin
               ; single rx
               loc=where(bl[pbl[tmp_idx]].itel1 eq ants[j] and bl[pbl[tmp_idx]].itel2 eq ants[k] and sp[psl[tmp_idx]].iband le 24, count)
               if count gt 0 then begin

                  if nM eq 2 then begin
                     jj=where(tsys.itel eq ants[j],nbands)
                     tmpj=(tsys[jj].tssb[0] gt 0) ? tsys[jj].tssb[0]: 99999.
                     kk=where(tsys.itel eq ants[k])
                     tmpk=(tsys[kk].tssb[0] gt 0) ? tsys[kk].tssb[0]: 99999.
                     sp[psl[tmp_idx[loc]]].tssb=sqrt(tmpj*tmpk)
                  endif else begin
                     jj=where(tsys.itel eq ants[j],nbands)
                     tmpj=tsys[jj].tssb[0:nM/2-1]
                     tmpj=[mean(tmpj),tmpj]
                     tmpj=[tmpj,tmpj]
                     kk=where(tsys.itel eq ants[k])
                     tmpk=tsys[kk].tssb[0:nM/2-1]
                     tmpk=[mean(tmpk),tmpk]
                     tmpk=[tmpk,tmpk]
                     sp[psl[tmp_idx[loc]]].tssb=sqrt(tmpj*tmpk)*2.
                  endelse

               endif
               loc=where(bl[pbl[tmp_idx]].itel1 eq ants[j] and bl[pbl[tmp_idx]].itel2 eq ants[k] and sp[psl[tmp_idx]].iband gt 24, count)
               if count gt 0 then begin
                  jj=where(tsys.itel eq ants[j])
                  tmpj=(tsys[jj].tssb[1] gt 0) ?  tsys[jj].tssb[1]: 99999.
                  kk=where(tsys.itel eq ants[k])
                  tmpk=(tsys[kk].tssb[1] gt 0) ?  tsys[kk].tssb[1]: 99999. 
                  sp[psl[tmp_idx[loc]]].tssb=sqrt(tmpj*tmpk)
               endif
            endelse
         endelse
      endfor
   endfor
endfor

close,unit & free_lun,unit

; load continuum data
if (nM gt 2) or (keyword_set(spectsys)) then  readtsys_eng2

print,'finished reading TSYS structure'

return
end

