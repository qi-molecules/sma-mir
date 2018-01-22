pro readdata3,directory=directory, int_read=int_read, skip=skip, full=full, sideband=sideband, rx=rx, band_read=band_read, old=old, nbin=nbin
;yes
;=Task:READDATA --- To read data from specified directory
;#Type: i/o
;
;+Use:
;      READDATA will read raw SMA data from, by default, the current directory. 
;      If you want to read a data set called mydata from a different directory,
;      /home/smauser/ for example,  you can use
;      >readdata,dir='/home/smauser/mydata'
;      Specifying the directory will change the default to the specified 
;      directory.
;
;&history
;------------------------------------------------------------------------
;      cykuo 10feb04 adapting the header
;------------------------------------------------------------------------ 

common global
common data_set
common wlm

;print,directory
;print,keyword_set(directory) 

if not keyword_set(sideband) then sideband=0
if not keyword_set(rx) then rx=0
if not keyword_set(int_read) then int_read=0
if not keyword_set(band_read) then band_read=0
if not keyword_set(nbin) then nbin=4

if keyword_set(directory) then begin
    nchar = strlen(directory)
    checkslash = strmid(directory,nchar-1,1)
    if ( strmatch(checkslash,'/') le 0 ) then directory = directory + '/'
    e.idl_bcp=directory

    count = 0
    result = findfile(e.idl_bcp+'in_read',count=count)
    if (count eq 0) then begin
        print,'could not find file ',e.idl_bcp+'in_read'
        return
    endif
    count = 0
    result = findfile(e.idl_bcp+'bl_read',count=count)
    if (count eq 0) then begin
        print,'could not find file ',e.idl_bcp+'bl_read'
        return
    endif
    count = 0
    result = findfile(e.idl_bcp+'sp_read',count=count)
    if (count eq 0) then begin
        print,'could not find file ',e.idl_bcp+'sp_read'
        return
    endif
    count = 0
    result = findfile(e.idl_bcp+'codes_read',count=count)
    if (count eq 0) then begin
        print,'could not find file ',e.idl_bcp+'codes_read'
        return
    endif
    count = 0
    result = findfile(e.idl_bcp+'sch_read',count=count)
    if (count eq 0) then begin
        print,'could not find file ',e.idl_bcp+'sch_read'
        return
    endif

    if keyword_set(full) then begin
       result=dbi_read_data()
       res=dat_filter(s_f,/reset,/no_notify)
       distinct_bands=uti_distinct(sp[psf].iband,nbands,/many_repeat)
       bw=(nbands-1.)*82.
    endif else begin
       if not keyword_set(old) then begin
          iblfix=0
          result=dbi_head_read2(int_read=int_read,sideband=sideband,rx=rx,band_read=band_read,bw=bw,iblfix=iblfix)
          if result lt 0 then return
          result=dbi_chan_read3(int_read=int_read,nbin=nbin)
          if result lt 0 then result=dbi_chan_read()
          if iblfix then uti_uvw_fix
       endif else begin
          result=dbi_head_read_short()
          result=dbi_chan_read_short()
          if result lt 0 then result=dbi_chan_read()
          res=dat_filter(s_f,/reset,/no_notify)
          distinct_bands=uti_distinct(sp[psf].iband,nbands,/many_repeat)
          bw=(nbands-1.)*82.
       endelse
;       if result lt 0 then result=dbi_chan_read()
    endelse
 endif else begin
    if keyword_set(full) then begin
       result=dbi_read_data()
       res=dat_filter(s_f,/reset,/no_notify)
       distinct_bands=uti_distinct(sp[psf].iband,nbands,/many_repeat)
       bw=(nbands-1.)*82.
    endif else begin
       if not keyword_set(old) then begin
          iblfix=0
          result=dbi_head_read2(int_read=int_read,sideband=sideband,rx=rx,band_read=band_read,bw=bw,iblfix=iblfix)
          if result lt 0 then return
          result=dbi_chan_read3(int_read=int_read,nbin=nbin)
          if result lt 0 then result=dbi_chan_read()
          if iblfix then uti_uvw_fix
       endif else begin
          result=dbi_head_read_short()
          result=dbi_chan_read_short() 
          if result lt 0 then result=dbi_chan_read()
          res=dat_filter(s_f,/reset,/no_notify)
          distinct_bands=uti_distinct(sp[psf].iband,nbands,/many_repeat)
          bw=(nbands-1.)*82.
       endelse
;       if result lt 0 then result=dbi_chan_read()
    endelse
 endelse
if not keyword_set(band_read) then begin
   res=dat_filter(s_f,'"band" like "c"',/reset,/no_notify)
   if abs(sp[psf[0]].fres) ne bw then sp[psf].fres = (sp[psf].fres/abs(sp[psf].fres)) * bw
   res=dat_filter(s_f,/reset,/no_notify)
endif else begin
   i=where(band_read eq 'c1' or band_read eq 'c2',count)
   if count gt 0 then begin
      res=dat_filter(s_f,'"band" like "c"',/reset,/no_notify)
      sp[psf].fres=(sp[psf].fres/abs(sp[psf].fres)) * bw
      res=dat_filter(s_f,/reset,/no_notify)
   endif
endelse

p=strpos(c.blcd,'-')
;realants=uti_distinct([strmid(c.blcd,0,1),strmid(c.blcd,2)],nants)
realants=uti_distinct([strmid(c.blcd,0,1#p),strmid(c.blcd,1#p+1)],nants)
realants=realants[sort(fix(realants))]
for i=0, nants-1 do begin
   j=where(c.tel1 eq realants[i],count)
   if count eq 0 then uti_addhdr,ant=realants[i]
endfor

;if not iblfix then begin
   lat=19.82420526391d/57.29577951
   m1=[[-sin(lat),0,cos(lat)],[0,1,0],[cos(lat),0,sin(lat)]]
   bls=uti_distinct(c.blcd(bl[pbf].iblcd),nbls,/many_repeat)
   bflag=0
   list=""
   for i=0,nbls-1 do begin
      result=dat_filter(s_f,'"wt" gt "0" and "blcd" eq "'+bls[i]+'"',/no_notify,/reset)
      h=in[pif[0]].ha*15.d*!dpi/180.d
      dec=in[pif[0]].decr
      m2=[[sin(h),cos(h),0],[-sin(dec)*cos(h),sin(dec)*sin(h),cos(dec)],$
        [cos(dec)*cos(h),-cos(dec)*sin(h),sin(dec)]]
      neu=[bl[pbf[0]].bln,bl[pbf[0]].ble,bl[pbf[0]].blu]
      neu=transpose(neu)
      klam=1000.d*0.299792458d/sp[psf[0]].fsky
      uvw=reform(m2##m1##neu)/klam
      if ( (abs(bl[pbf[0]].u-uvw[0])/uvw[0] gt 0.1) or (abs(bl[pbf[0]].v-uvw[1])/uvw[1] gt 0.1) or (abs(bl[pbf[0]].w-uvw[2])/uvw[2] gt 0.1) ) then begin
         bflag=1
         list=[list,bls[i]]
         print,'UVW coords for '+bls[i]+' will be fixed !'
      endif
   endfor
   if bflag then begin
      list=list[1:n_elements(list)-1]
      select,baseline=list,/reset
      uti_uvw_fix
   endif
;endif

res=dat_filter(s_f,'"wt" gt "0"',/reset,/no_notify)

distinct_recs=uti_distinct(bl[pbf].irec,nrecs,/many_repeat)

if nrecs gt 1 then begin
   sint0=strcompress(string(in[pif[0]].int),/remove)

   distinct_sbs=uti_distinct(bl[pbf].isb,nsbs,/many_repeat)
;   distinct_bands=uti_distinct(sp[psf].iband,nbands,/many_repeat)
   distinct_bls=uti_distinct(bl[pbf].iblcd,nbls,/many_repeat)
;   npts=nrecs*nsbs*nbands*nbls
   temp_recs=c.rec[distinct_recs]
   result=dat_filter(s_f,'"rec" eq "'+temp_recs[0]+'"',/no_notify,/reset)
   bands1=uti_distinct(sp[psf].iband,nbands1,/many_repeat)
   result=dat_filter(s_f,'"rec" eq "'+temp_recs[1]+'"',/no_notify,/reset)
   bands2=uti_distinct(sp[psf].iband,nbands2,/many_repeat)
   nbands=nbands1+nbands2
   npts=nsbs*nbls*nbands

   res=dat_filter(s_f,'"int" eq "'+sint0+'"',/no_notify,/reset)
   if res ne npts then begin
      if res eq 2.*npts then begin
         print,'This dualrx track data has a DOUBLE DATA problem !'
         print,'To fix the problem, a dataset file "rawdata" containing' 
         print,'   the correct data will be created in local directory.'
         aa=''
         read,aa,prompt='Fix the DOUBLE DATA problem? [NO <YES>]:  '
         if (aa eq 'YES' or aa eq 'yes' or aa eq 'Yes' or aa eq 'Y' or aa eq 'y') then begin
            npt=nbands*nbls
            index=where((ps/700 mod 2) eq 1)
            sp[ps[index]].wt=-1.
            select,/p,/re
            mir_save,/new,'rawdata'
         endif else begin
            print,'NO: nothing done'
         endelse
      endif else begin
         print,'This dualrx track might have a wrong data structure .'
         print,'Please report this track directory to cqi@cfa.'
      endelse
   endif
endif

if n_elements(c.ref_time) gt 1 then begin
   result=dat_filter(s_f,'"iref_time" eq "1"',/reset,/no_notify)
   in[pif].dhrs=in[pif].dhrs+24.
   bl[pbf].avedhrs=bl[pbf].avedhrs+24.
   in[pif].iref_time=0
endif

res=dat_filter(s_f,/reset,/no_notify)

end




