pro readdata,directory=directory, newformat=newformat,int_read=int_read, skip=skip, full=full, sideband=sideband, rx=rx, band_read=band_read, old=old, if1=if1, if2=if2, if3=if3, if4=if4, asic=asic, swarm=swarm, swmavg=swmavg, windows=windows, defaults=defaults
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
if keyword_set(old) or keyword_set(full) then begin
   print,'OLD and FULL keywords are obsolete now,'
   print,'contact cqi@cfa for the old routines.'
   return
endif

if not keyword_set(sideband) then sideband=0
if not keyword_set(rx) then rx=0
if not keyword_set(int_read) then int_read=0
if not keyword_set(band_read) then band_read=0

;if keyword_set(windows) then begin
;   temp=size(windows)
;   if temp[0] eq 1 then nwindows=1 else nwindows=temp[2]
;   navg=intarr(2+nwindows)+1
;   navg[0]=n1
;   navg[1]=n2
;   for i=0, nwindows-1 do navg[2+i]=windows[3,i]
;endif else begin
;   navg=intarr(2)+1
;   navg[0]=n1
;   navg[1]=n2
;endelse

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

; set up the data format environment
    if not keyword_set(newformat) then begin
       openr,unit,e.idl_bcp+'in_read',/get_lun,error=err
       newformat=0L
       readu,unit,newformat
       close,unit & free_lun,unit
    endif
    if newformat ne 0 then begin
       print,'****************************New data format ...'
       msfile=e.idl_sav+'ms_newformat.save' 
    endif else begin
       print,'****************************Old data format ...'
       msfile=e.idl_sav+'ms.save'
    endelse
    restore,msfile

    iblfix=0
    if newformat ne 0 then begin
       result=dbi_head2_read2(int_read=int_read, $
         sideband=sideband,rx=rx,band_read=band_read,$
         iblfix=iblfix, endianFlag=endianFlag,newwindows=windows,$
         if1=if1, if2=if2, if3=if3, if4=if4, defaults=defaults,$
         asic=asic, swarm=swarm, swmavg=swmavg, nbins=nbins)
       if result lt 0 then return
       result=dbi_chan2_read2(int_read=int_read, $
         nbins=nbins, endianFlag=endianFlag)
       if result lt 0 then result=dbi_chan2_read2int(int_read=int_read, $
         nbins=nbins, endianFlag=endianFlag)
    endif else begin
       result=dbi_head_read2(int_read=int_read, $
         sideband=sideband,rx=rx,band_read=band_read,$
         bw=bw,iblfix=iblfix, endianFlag=endianFlag,defaults=defaults)
       if result lt 0 then return
       result=dbi_chan_read2(int_read=int_read, $
         endianFlag=endianFlag)
       if result lt 0 then result=dbi_chan_read()
    endelse
    if iblfix then uti_uvw_fix

 endif else begin

; set up the data format environment
    if not keyword_set(newformat) then begin
       openr,unit,e.idl_bcp+'in_read',/get_lun,error=err
       newformat=0L
       readu,unit,newformat
       close,unit & free_lun,unit
    endif
    if newformat ne 0 then begin
       print,'****************************New data format ...'
       msfile=e.idl_sav+'ms_newformat.save' 
    endif else begin
       print,'****************************Old data format ...'
       msfile=e.idl_sav+'ms.save'
    endelse
    restore,msfile

    iblfix=0
    if newformat ne 0 then begin
;       print,'newformat'
       result=dbi_head2_read2(int_read=int_read, $
         sideband=sideband,rx=rx,band_read=band_read,$
         iblfix=iblfix, endianFlag=endianFlag,newwindows=windows,$
         if1=if1, if2=if2, if3=if3, if4=if4, defaults=defaults,$
         asic=asic, swarm=swarm, swmavg=swmavg, nbins=nbins)
       if result lt 0 then return
       result=dbi_chan2_read2(int_read=int_read, $
         nbins=nbins, endianFlag=endianFlag)
       if result lt 0 then result=dbi_chan2_read2int(int_read=int_read, $
         nbins=nbins, endianFlag=endianFlag)
    endif else begin
       result=dbi_head_read2(int_read=int_read, $
         sideband=sideband,rx=rx,band_read=band_read,$
         bw=bw,iblfix=iblfix, endianFlag=endianFlag,defaults=defaults)
       if result lt 0 then return
       result=dbi_chan_read2(int_read=int_read, $
         endianFlag=endianFlag)
       if result lt 0 then result=dbi_chan_read()
    endelse
    if iblfix then uti_uvw_fix

 endelse

; setting flags and filling tsys info, holder for bw info for old data
if newformat ne 0 then begin
;   uti_tsys
   readtsys2, rx=rx, sideband=sideband
   res=dat_filter(s_f,'"flags" ne "0"',/reset,/no_notify)
   if res gt 0 then sp[psf].wt=-1
   res=dat_filter(s_f,/reset,/no_notify)
endif else begin
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
   res=dat_filter(s_f,'"int" eq "'+sint0+'"',/no_notify,/reset)
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
         if keyword_set(defaults) then begin
            aa='YES' 
         endif else begin
            aa=''
            read,aa,prompt='Fix the DOUBLE DATA problem? [NO <YES>]:  '
         endelse
         if (aa eq 'YES' or aa eq 'yes' or aa eq 'Yes' or aa eq 'Y' or aa eq 'y') then begin
            npt=nbands*nbls
            index=where((ps/npt mod 2) eq 1)
            sp[ps[index]].wt=-1.
            select,/p,/re
            mir_save,/new,'rawdata',/nowait
         endif else begin
            print,'NO: nothing done'
         endelse
      endif else begin
         print,'This dualrx track might have a wrong data structure .'
         print,'If this is not supposed to be a dual receiver track,'
         print,'there must be some header problems, which you can fix'
         print,'by type "bl.irec=1" after you load the data.'
         print,'Otherwise, please report this track directory to cqi@cfa.'
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




