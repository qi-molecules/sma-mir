pro readdata,directory=directory, newformat=newformat,int_read=int_read, skip=skip, full=full, sideband=sideband, rx=rx, band_read=band_read, old=old, if1=if1, if2=if2, if3=if3, if4=if4, asic=asic, swarm=swarm, swmavg=swmavg, windows=windows, nopolcor=nopolcor, defaults=defaults
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

res=dat_filter(s_f,'"wt" gt "0"',/reset,/no_notify)
distinct_bls=uti_distinct(c.blcd[bl[pbf].iblcd],nbls,/many_repeat)
distinct_recs=uti_distinct(c.rec[bl[pbf].irec],nrecs,/many_repeat)
distinct_sbs=uti_distinct(strupcase(c.sb[bl[pbf].isb]),nsbs,/many_repeat)
; Check data epoch
;if (fix(c.filever) lt 2) then begin
datobs=c.ref_time[in[pi[0]].iref_time]
months = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
mon=strmid(datobs,0,3)
j=where(mon eq months,count)
num_day_obs=[fix(strtrim(strmid(datobs,8,4),2)),fix(strtrim(string(j[0]+1),2)),(strtrim(strmid(datobs,4,2),2))]
day=strtrim(string(num_day_obs[2]),2)
yr =strtrim(string(num_day_obs[0]),2)
mo =strtrim(string(num_day_obs[1]),2)
mJD=uti_date2mjd(yr,mo,day)
;   print,'mjd: ', mjd
mjd2000=uti_date2mjd(2000,1,1)
newepoch=2000.+(mJD-mjd2000)/365.
;endif

; check uvw
lat=double(19.82420526391d*!dpi/180.d)
m1=[[-sin(lat),0,cos(lat)],[0,1,0],[cos(lat),0,sin(lat)]]

ii=uti_distinct(in[pif].int,nint,/many_repeat)
a0=pif & a1=pbf & a2=psf
for i=0, nbls-1 do begin
   blcd=distinct_bls[i]
   ibl=min(where(c.blcd eq blcd))
;   print,'baseline:',blcd
   for j=0, nrecs-1 do begin
      rec=distinct_recs[j]
      irx=min(where(c.rec eq rec))
;      print,'receiver:',rec
      for k=0, nsbs-1 do begin
         sb=distinct_sbs[k]
         isbd=min(where(c.sb eq strlowcase(sb)))
;         print,'sideband:',sb
         n=where( (bl[a1].irec eq irx) and (bl[a1].isb eq isbd) and (bl[a1].iblcd eq ibl), count)
         if count gt 0 then begin
            b0=a0[n] & b1=a1[n] & b2=a2[n]
            h=in[b0[0]].ha*15.d*!dpi/180.d
            dec=in[b0[0]].decr
            ra=in[b0[0]].rar
            uti_precess,ra,dec,2000,newepoch,/radian
            if min([fix(c.filever)]) ge 3 then dec=in[b0[0]].inhdbl5 ;adec
            m2=[[sin(h),cos(h),0],[-sin(dec)*cos(h),sin(dec)*sin(h),cos(dec)],$
                [cos(dec)*cos(h),-cos(dec)*sin(h),sin(dec)]]
            neu=[bl[b1[0]].bln,bl[b1[0]].ble,bl[b1[0]].blu]
            neu=transpose(neu)
;            klam=1000.d*0.299792458d/sp[b2[0]].fsky or !cvel/sp[0].fsky/1e6 
;            uvw=reform(m2##m1##neu)/klam
            klam=!cvel/sp[b2[0]].fsky/1e6
            uvw=reform(m2##m1##neu)/klam
            if min([fix(c.filever)]) ge 3 then begin
               bl[b1].u=bl[b1].u/klam
               bl[b1].v=bl[b1].v/klam
               bl[b1].w=bl[b1].w/klam
               bl[b1].prbl=sqrt(bl[b1].u*bl[b1].u+bl[b1].v*bl[b1].v)
            endif

;            stop
;            print,bl[b1[0]].u,bl[b1[0]].v,bl[b1[0]].w
;            print,uvw[0],uvw[1],uvw[2]
;            read,iii
            if e.debug then begin
               if ( (abs(bl[b1[0]].u-uvw[0]) gt 0.2) or (abs(bl[b1[0]].v-uvw[1]) gt 0.2) or (abs(bl[b1[0]].w-uvw[2]) gt 0.2) ) then print,'Check UVW coords for baseline: ',blcd,', receiver: ',rec,', sideband: ',sb
               for m =0L, nint-1L do begin
                  n=where( in[b0].int eq ii[m], count)
                  if count eq 0 then goto, jump2
                  c0=b0[n] & c1=b1[n] & c2=b2[n]
                  klam=!cvel/sp[c2[0]].fsky/1e6
;                  bl[c1].u=bl[c1].u/klam
;                  bl[c1].v=bl[c1].v/klam
;                  bl[c1].w=bl[c1].w/klam
;                  bl[c1].prbl=sqrt(bl[c1].u*bl[c1].u+bl[c1].v*bl[c1].v)  
                  h=in[c0[0]].ha*15.d*!dpi/180.d
                  dec=in[c0[0]].inhdbl5 ; adec
                  m2=[[sin(h),cos(h),0],[-sin(dec)*cos(h),sin(dec)*sin(h),cos(dec)],$
                      [cos(dec)*cos(h),-cos(dec)*sin(h),sin(dec)]]
                  neu=[bl[c1[0]].bln,bl[c1[0]].ble,bl[c1[0]].blu]
                  neu=transpose(neu)
;                  klam=1000.d*0.299792458d/sp[c2[0]].fsky
                  uvw=reform(m2##m1##neu)/klam
                  print,uvw
                  jump2:                        
               endfor           ; integration
            endif               ; debug
         endif                  ; count
      endfor                    ; sbs
   endfor                       ; recs
endfor                          ; baselines 

;res=dat_filter(s_f,'"wt" gt "0"',/reset,/no_notify)

;distinct_recs=uti_distinct(bl[pbf].irec,nrecs,/many_repeat)

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

; polarization correction checking

res=dat_filter(s_f,'"wt" gt "0"',/reset,/no_notify)
pols=uti_distinct(bl[pbf].ipol,npol,/many)
if (not keyword_set(nopolcor)) and (npol eq 4) then begin
   ; mJD from above
   datatime=mJD+in[pil[0]].dhrs/24.
   ;print,'datatime: ',datatime
   print,''
   print,'*** Checking data fixing for dual-pol data ...'
   datecor=[uti_date2mjd(2016,1,7),uti_date2mjd(2017,6,7),uti_date2mjd(2018,3,5),uti_date2mjd(2018,6,14),uti_date2mjd(2018,11,15),uti_date2mjd(2018,11,29)]
   ;print,'datecor: '
   ;print,datecor
   loc=value_locate(datecor,datatime)
   ;print,'loc: ',loc
   case loc of
      0: begin
         ;
         ; apply to polarization data taken from Oct 10th, 2016 up to and including June 6th, 2017
         ; correct for missing '-' sign errors in dataCathcher, equivalent to a 90d error:
         ; RxA-RxB LR, RR, should have added -90d, dataCathcher added 90, thus, correction is -180
         select,/res,/pos,state=['LR','RR']
         result=dat_filter(s_f,'("ant1rx" eq "0" and "ant2rx" eq "1")')
         if result gt 0 then uti_phasechange,angle=-180
         ; RxB-RxA LL, LR, should have added -90d, dataCathcher added 90, thus, correction is -180
         select,/res,/pos,state=['LL','LR']
         result=dat_filter(s_f,'("ant1rx" eq "1" and "ant2rx" eq "0")')
         if result gt 0 then uti_phasechange,angle=-180
         select,/p,/re
         uti_avgband
         print,''
         print,'*** Polarization data correction done based on polcor3.'
      end
      1: begin
         ; implementing phase corrections corresponding to evec=45
         ; when no online corrections are applied 
         ;
         ; RxA-RxB LL, RL, -90d
         select,/res,/pos,state=['LL','RL']
         result=dat_filter(s_f,'("ant1rx" eq "0" and "ant2rx" eq "1")')
         if result gt 0 then uti_phasechange,angle=-90

         ; RxA-RxB LR, RR, 90d
         select,/res,/pos,state=['LR','RR']
         result=dat_filter(s_f,'("ant1rx" eq "0" and "ant2rx" eq "1")')
         if result gt 0 then uti_phasechange,angle=90

         ; RxB-RxA LL, LR, 90d
         select,/res,/pos,state=['LL','LR']
         result=dat_filter(s_f,'("ant1rx" eq "1" and "ant2rx" eq "0")')
         if result gt 0 then uti_phasechange,angle=90

         ; RxB-RxA RL, RR, -90
         select,/res,/pos,state=['RL','RR']
         result=dat_filter(s_f,'("ant1rx" eq "1" and "ant2rx" eq "0")')
         if result gt 0 then uti_phasechange,angle=-90

         ; RxB-RxB LR, 180
         select,/res,/pos,state='LR'
         result=dat_filter(s_f,'("ant1rx" eq "1" and "ant2rx" eq "1")')
         if result gt 0 then uti_phasechange,angle=180

         ; RxB-RxB RL, -180
         select,/res,/pos,state='RL'
         result=dat_filter(s_f,'("ant1rx" eq "1" and "ant2rx" eq "1")')
         if result gt 0 then uti_phasechange,angle=-180

         select,/p,/re
         uti_avgband
         print,''
         print,'*** Polarization data correction done based on polcor1.'
      end
      3: begin
         ;Corrects L<->R labeling
         c.pol[0:4]=['unknown','LL','LR','RL','RR']
         select,state='LL',/re
         i_ll=pbf
         select,state='LR',/re
         i_lr=pbf
         select,state='RL',/re
         i_rl=pbf
         select,state='RR',/re
         i_rr=pbf
         bl[i_ll].ipol=4
         bl[i_lr].ipol=3
         bl[i_rl].ipol=2
         bl[i_rr].ipol=1

         ; Corrects incorrect angles applied
         result=dat_filter(s_f,'("ant1rx" eq "0" and "ant2rx" eq "1") or ("ant1rx" eq "1" and "ant2rx" eq "0")',/reset)
         result=dat_filter(s_f,'("ipol" eq "2") or ("ipol" eq "3")')
         if result gt 0 then uti_phasechange,angle=180
         select,/p,/re
         uti_avgband
         print,''
         print,'*** Polarization data correction done based on polcor2.'
      end
      4: begin
         ; implementing phase corrections corresponding to evec=45
         ; when no online corrections are applied 
         ;
         ; RxA-RxB LL, RL, -90d
         select,/res,/pos,state=['LL','RL']
         result=dat_filter(s_f,'("ant1rx" eq "0" and "ant2rx" eq "1")')
         if result gt 0 then uti_phasechange,angle=-90

         ; RxA-RxB LR, RR, 90d
         select,/res,/pos,state=['LR','RR']
         result=dat_filter(s_f,'("ant1rx" eq "0" and "ant2rx" eq "1")')
         if result gt 0 then uti_phasechange,angle=90

         ; RxB-RxA LL, LR, 90d
         select,/res,/pos,state=['LL','LR']
         result=dat_filter(s_f,'("ant1rx" eq "1" and "ant2rx" eq "0")')
         if result gt 0 then uti_phasechange,angle=90

         ; RxB-RxA RL, RR, -90
         select,/res,/pos,state=['RL','RR']
         result=dat_filter(s_f,'("ant1rx" eq "1" and "ant2rx" eq "0")')
         if result gt 0 then uti_phasechange,angle=-90

         ; RxB-RxB LR, 180
         select,/res,/pos,state='LR'
         result=dat_filter(s_f,'("ant1rx" eq "1" and "ant2rx" eq "1")')
         if result gt 0 then uti_phasechange,angle=180

         ; RxB-RxB RL, -180
         select,/res,/pos,state='RL'
         result=dat_filter(s_f,'("ant1rx" eq "1" and "ant2rx" eq "1")')
         if result gt 0 then uti_phasechange,angle=-180

         select,/p,/re
         uti_avgband
         print,''
         print,'*** Polarization data correction done based on polcor1.'
      end
      else: print, '*** No polarization data correction needed.'
   endcase
   if (not keyword_set(nopolcor)) and (npol eq 4) and (loc ge 0  and loc le 4) then begin
       print, 'Polarization correction was applied to this data based on the date of the observation. If you do not wish for the corrections to be applied, please re-run readdata with option nopolcor'
       print, 'For example: IDL> readdata,dir='''+directory+''',/nopolcor'
   endif
endif

res=dat_filter(s_f,/reset,/no_notify)

end
