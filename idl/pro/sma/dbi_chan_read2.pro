function dbi_chan_read2,int_read=int_read, nbin=nbin, endianFlag=endianFlag
common global
common data_set  

if (keyword_set(int_read)) then begin
   in_skip=int_read[0]
   nints_expected=int_read[1]-in_skip+1
endif else begin
   in_skip=0
   nints_expected=long(n_elements(in))
endelse

;nints_expected = long(n_elements(in))-1L

print, 'number of integrations ',nints_expected
nsp=long(n_elements(sp))
print, 'number of spectra ',nsp

ndata=long(total(sp.nch,/double))
nrecs=nsp
pts_per_int=16L
;integs=fltarr(nrecs,nozero=1)
;toffs=fltarr(nrecs,nozero=1)
;noises=fltarr(nrecs,nozero=1)
;scales=fltarr(nrecs,nozero=1)
;rwts=fltarr(nrecs,nozero=1)
;ch=complexarr(ndata,nozero=1)
pr=ps
temp_pc=long(total(sp[ps].nch,/cum,/double))
pc=[0,temp_pc[0:n_elements(ps)-2]]

nrec_prev=-1L & nch_prev = -1L
sch={inhid:0L,form:'1234',nbyt:0L,nbyt_pack:0L,$
     packdata:intarr(pts_per_int,nozero=0)}
first_byte=0L
pr_cur=0L
pc_cur=0L
openr,unit,e.idl_bcp+'sch_read',/get_lun,error=err

if err ne 0 then begin
  print,!err_string
  return,-1
endif

status = fstat(unit) & nbytes = long64(status.size)
;print,'number of bytes in sch_read ',nbytes

point_lun,unit,first_byte & readu,unit,sch
;if (strpos(!VERSION.ARCH,'86') ge 0) then $
if endianFlag eq 1 then sch = swap_endian(sch)

if (nbytes lt ((sch.nbyt+16L)*(nints_expected+in_skip))) then begin
   print,'***DATA STRUCTURE CHANGED WITHIN THE TRACK! **********'
   print,'***USING OLD READDATA ROUTINE TO READ THE DATA NOW ***'
   close,unit & free_lun,unit
   return,-1
endif
   
pts_per_int=sch.nbyt/2L
first_byte=long64(16L+sch.nbyt)*in_skip

sch={inhid:0L,form:'1234',nbyt:0L,nbyt_pack:0L,$
     packdata:intarr(pts_per_int,nozero=0)}

spindx=0L

if nints_expected lt 2000 then jread=100 else jread=200
nread=nints_expected/jread


for j=0,nread do begin
    nints_read=jread<(nints_expected-j*jread)
    if nints_read eq 0 then goto,finish
    print,'reading integration from',in_skip+j*jread,' to ',in_skip+j*jread+nints_read-1L
    data_sch=replicate(sch,nints_read)
    point_lun,unit,first_byte & readu,unit,data_sch
;    if (strpos(!VERSION.ARCH,'86') ge 0) then $
    if endianFlag eq 1 then data_sch = swap_endian(data_sch)

    first_byte=first_byte+long64((16L+data_sch[0].nbyt)*nints_read)

    tmp=where((data_sch.nbyt-data_sch[0].nbyt) ne 0, count)
    if (count gt 0) then begin
        print,'***DATA STRUCTURE CHANGED WITHIN THE TRACK! **********'
        print,'***USING OLD READDATA ROUTINE TO READ THE DATA NOW ***'
        close,unit & free_lun,unit
        return,-1
    endif

    rows=where(sp.inhid eq data_sch[0].inhid,count)
    npts=count*nints_read

    dataoffs = long(sp[rows].dataOFF)/2L
    nchs = long(sp[rows].nch)
    wts=sp[spindx:spindx+npts-1].wt
    spindx=spindx+npts
;;wts = sp.wt

;    for j=0L,count-1L do begin
;      pr(rows(j))=pr_cur
;      pc(rows(j))=pc_cur
    offsets=dataoffs
    offsets1=offsets+1L
    offsets2=offsets+2L
    offsets3=offsets+3L
    offsets4=offsets+4L
    integ = reform(data_sch.packdata(offsets)/10.e0,npts)
    toff  = reform(data_sch.packdata(offsets1),npts)
    noise = reform(long(data_sch.packdata(offsets3))*32768L +  $
                   long(data_sch.packdata(offsets2)),npts)
    rwt   = wts*integ 
    scale = 2.e0^data_sch.packdata(offsets4)

    ptr=make_array(total(nchs,/double),/long,/nozero)
    ptr_scale=make_array(total(nchs,/double),nints_read,/float,/nozero)
    iptr_end=-1L & nch_prev=-1L
    for i=0L, count-1L do begin
        iptr=iptr_end+1L & iptr_end=iptr+nchs[i]-1L
        if nchs[i] ne nch_prev then lin_nch=lindgen(nchs[i])
        nch_prev=nchs[i]
        ptr[iptr:iptr_end]=offsets[i]+lin_nch*2L+5L
        tmp=scale[i,*]
        ptr_scale[iptr:iptr_end,*]=rebin(scale[i,*],(iptr_end-iptr+1),nints_read,/sample)
    endfor
    scale=reform(scale,npts)
    ptr_i=ptr+1L
    if keyword_set(nbin) then begin
       rchtemp=ptr_scale*data_sch.packdata[ptr]
       ichtemp=ptr_scale*data_sch.packdata[ptr_i]

       i=where(nchs ne 1)
       nch=nchs[i[0]]
       nchs[i]=nchs[i]/nbin
       count2=count/n_elements(c.band)
       iptr_end=-1L
       for i=0,count2-1L do begin
          allnch=nch*(n_elements(c.band)-1)
          if (i eq 0) then chtemp=complex(rchtemp[iptr_end+1L,*],ichtemp[iptr_end+1L,*]) else chtemp=[temporary(chtemp),complex(rchtemp[iptr_end+1L,*],ichtemp[iptr_end+1L,*])]
          iptr=iptr_end+2L & iptr_end=iptr+allnch-1L
;    chtemp2=rebin(chtemp[iptr:iptr_end,*],allnch/nbin,nints_read)
          rchtemp2=rebin(rchtemp[iptr:iptr_end,*],allnch/nbin,nints_read)
          ichtemp2=rebin(ichtemp[iptr:iptr_end,*],allnch/nbin,nints_read)
;    rchtemp2, ichtemp2 are the rebinned spectra ch       
          chtemp=[temporary(chtemp),complex(rchtemp2,ichtemp2)]
       endfor
       rwt=rwt*nbin      
    endif else begin
       chtemp = ptr_scale*complex(data_sch.packdata[ptr],data_sch.packdata[ptr_i])
    endelse
    ch_npts=long(total(nchs,/double))*nints_read
    chtemp=reform(chtemp,ch_npts)
    if (j eq 0) then begin
        integs=integ
        toffs=toff
        noises=noise
        rwts=rwt
        scales=scale
        ch=chtemp
    endif else begin
        integs=[temporary(integs),integ]
        toffs=[temporary(toffs),toff]
        noises=[temporary(noises),noise]
        rwts=[temporary(rwts),rwt]
        scales=[temporary(scales),scale]
        ch=[temporary(ch),chtemp]
    endelse
endfor

finish:
print,"finished reading data"

close,unit & free_lun,unit

if keyword_set(nbin) then begin
   i=where(sp.nch ne 1)
   sp[i].nch=sp[i].nch/nbin
   sp[i].fres=sp[i].fres*nbin
   sp[i].wt=sp[i].wt*nbin
   temp_pc=long(total(sp[ps].nch,/cum,/double))
   pc=[0,temp_pc[0:n_elements(ps)-2]]
endif

pcf=pc
prf=pr
pcs=pc
prs=pr
pcl=pc
prl=pr
;
;  combine record headers into structure re
;
re={integs:integs,toffs:toffs,noises:noises,scales:scales,wts:rwts}
;

if e.prog_help then begin
 print,' '
 print,'  structure with record headers :'   
 help,/structure,re
 print,' '
 print,'    ch : complex array with continuum records and spectral channels'$
      ,'    ... the data is arrange in record, then channel order'$
      ,'    pc is a pointer array from sp rows to the first data pt in ch'$
      ,'    pr is a pointer array from sp rows to the first record of header'  $
      ,'    ... data in re'
endif
return,1
end


