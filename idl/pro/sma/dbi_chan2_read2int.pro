function dbi_chan2_read2int,int_read=int_read, nbins=nbins, endianFlag=endianFlag
common global
common data_set  

if (keyword_set(int_read)) then in_skip=int_read[0] else in_skip=0L
nints_expected=long(n_elements(in))

;nints_expected = long(n_elements(in))-1L

nbands=n_elements(c.band)
avgflag=0
if keyword_set(nbins) then begin
   if n_elements(nbins) ne nbands then stop
   nchunks=sp[0:nbands-1].nch/nbins
   avgflag=1
endif

print, 'number of integrations ',nints_expected
nsp=long(n_elements(sp))
print, 'number of spectra ',nsp

ndata=long64(total(sp.nch,/double))
nrecs=nsp
pts_per_int=16L
;integs=fltarr(nrecs,nozero=1)
;toffs=fltarr(nrecs,nozero=1)
;noises=fltarr(nrecs,nozero=1)
;scales=fltarr(nrecs,nozero=1)
;rwts=fltarr(nrecs,nozero=1)
;ch=complexarr(ndata,nozero=1)
pr=ps
temp_pc=long64(total(sp[ps].nch,/cum,/double))
pc=[0L,temp_pc[0:n_elements(ps)-2L]]

nrec_prev=-1L & nch_prev = -1L
sch={inhid:0L,nbyt:0L,$
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

;if (nbytes lt ((sch.nbyt+8L)*(nints_expected+in_skip))) then begin
;   print,'***DATA STRUCTURE CHANGED WITHIN THE TRACK! **********'
;   print,'***USING OLD READDATA ROUTINE TO READ THE DATA NOW ***'
;   close,unit & free_lun,unit
;   return,-1
;endif
   
pts_per_int=sch.nbyt/2L
first_byte=long64(8L+sch.nbyt)*in_skip

sch={inhid:0L,nbyt:0L,$
     packdata:intarr(pts_per_int,nozero=0)}

spindx=0L

nread=nints_expected
for j=0L,nread-1 do begin
    nints_read=1
    j_count=j/100
    j_mod=(j mod 100)
    if j_mod eq 0 then begin
       j_stop=((j_count+1L)*100-1)< (nread-1)
       print,'reading integration from', j_count*100,' to ',j_stop
    endif
    data_sch=sch
    point_lun,unit,first_byte & readu,unit,data_sch
;    if (strpos(!VERSION.ARCH,'86') ge 0) then $
    if endianFlag eq 1 then data_sch = swap_endian(data_sch)

    first_byte=first_byte+long64(8L+data_sch.nbyt)*nints_read
    rows=where(sp.inhid eq data_sch.inhid,count)

; in case no swarm bands exists for part of data
    if not (count mod (nbands-2)) then begin
;       print, 'No swarm data existed at integration ',j
;       print,count
       avgflag=0
    endif else if keyword_set(nbins) then avgflag=1

    npts=count*nints_read

    dataoffs = long(sp[rows].dataOFF)/2L
    nchs = long(sp[rows].nch)
    wts=sp[spindx:spindx+npts-1].wt
    itgs=sp[spindx:spindx+npts-1].integ
    spindx=spindx+npts
;;wts = sp.wt

;    for j=0L,count-1L do begin
;      pr(rows(j))=pr_cur
;      pc(rows(j))=pc_cur
    offsets=dataoffs
;    offsets1=offsets+1L
;    offsets2=offsets+2L
;    offsets3=offsets+3L
;    offsets4=offsets+4L
;    integ = reform(data_sch.packdata(offsets)/10.e0,npts)
    integ = itgs
;    toff  = reform(data_sch.packdata(offsets1),npts)
;    noise = reform(long(data_sch.packdata(offsets3))*32768L +  $
;                   long(data_sch.packdata(offsets2)),npts)
;    toff and noise are just preset random numbers
    toff = intarr(npts)
    noise = lonarr(npts)+100
    rwt   = wts*integ 
;    scale = 2.e0^data_sch.packdata(offsets4)
    scale = 2.e0^data_sch.packdata(offsets)

    ptr=make_array(total(nchs,/double),/long,/nozero)
    ptr_scale=make_array(total(nchs,/double),nints_read,/float,/nozero)
    iptr_end=-1L & nch_prev=-1L
    for i=0L, count-1L do begin
        iptr=iptr_end+1L & iptr_end=iptr+nchs[i]-1L
        if nchs[i] ne nch_prev then lin_nch=lindgen(nchs[i])
        nch_prev=nchs[i]
;        ptr[iptr:iptr_end]=offsets[i]+lin_nch*2L+5L
        ptr[iptr:iptr_end]=offsets[i]+lin_nch*2L+1L
        tmp=scale[i,*]
        ptr_scale[iptr:iptr_end,*]=rebin(scale[i,*],(iptr_end-iptr+1),nints_read,/sample)
    endfor
    scale=reform(scale,npts)
    ptr_i=ptr+1L
;    if keyword_set(nbins) then begin
    if avgflag then begin
       rchtemp=ptr_scale*data_sch.packdata[ptr]
       ichtemp=ptr_scale*data_sch.packdata[ptr_i]

       chtemp=complex(rchtemp[0,*],ichtemp[0,*])
       iptr_end=0L
       newnchs=nchs
       for i=1, count-1L do begin
          iptr=iptr_end+1L & iptr_end=iptr+nchs[i]-1L
          newnchs[i]=nchunks[i mod n_elements(c.band)]

          rchtemp2=rebin(rchtemp[iptr:iptr_end,*],newnchs[i],nints_read)
          ichtemp2=rebin(ichtemp[iptr:iptr_end,*],newnchs[i],nints_read)
;    rchtemp2, ichtemp2 are the rebinned spectra ch       
          chtemp=[temporary(chtemp),complex(rchtemp2,ichtemp2)]         
       endfor
       nchs=newnchs
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

;if keyword_set(nbins) then begin
if avgflag then begin
   for j=1, n_elements(c.band)-1 do begin
      i=where(sp.nch eq sp[j].nch and sp.iband eq sp[j].iband)
;      k=sp[j].nch/nchunks[j]
;      sp[i].nch=nchunks[j]
      sp[i].nch=sp[j].nch/nbins[j]
      sp[i].fres=sp[i].fres*nbins[j]
      sp[i].vres=sp[i].vres*nbins[j]
      sp[i].wt=sp[i].wt*nbins[j]
   endfor
   temp_pc=long64(total(sp[ps].nch,/cum,/double))
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


