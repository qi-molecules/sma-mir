function dbi_chan_read_short,int_read=int_read
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
if (strpos(!VERSION.ARCH,'x86') ge 0) or (strpos(!VERSION.ARCH,'alpha') ge 0) then $
  sch = swap_endian(sch)

if (nbytes lt ((sch.nbyt+16L)*(nints_expected+in_skip))) then begin
   print,'***DATA STRUCTURE CHANGED WITHIN THE TRACK! **********'
   print,'***USING OLD READDATA ROUTINE TO READ THE DATA NOW ***'
   close,unit & free_lun,unit
   return,-1
endif
   
pts_per_int=sch.nbyt/2L
sch={inhid:0L,form:'1234',nbyt:0L,nbyt_pack:0L,$
     packdata:intarr(pts_per_int,nozero=0)}
data_sch=replicate(sch,nints_expected)
if in_skip gt 0 then begin
   for i=0L,in_skip-1L do begin
      point_lun,unit,first_byte & readu,unit,sch
      if (strpos(!VERSION.ARCH,'x86') ge 0) or (strpos(!VERSION.ARCH,'alpha') ge 0) then $
        sch = swap_endian(sch)      
      first_byte=long64(first_byte+16L+sch.nbyt)
   endfor
endif 
point_lun,unit,first_byte & readu,unit,data_sch

if (strpos(!VERSION.ARCH,'x86') ge 0) or (strpos(!VERSION.ARCH,'alpha') ge 0) then $
  data_sch = swap_endian(data_sch)

tmp=where((data_sch.nbyt-data_sch[0].nbyt) ne 0, count)
if (count gt 0) then begin
   print,'***DATA STRUCTURE CHANGED WITHIN THE TRACK! **********'
   print,'***USING OLD READDATA ROUTINE TO READ THE DATA NOW ***'
   close,unit & free_lun,unit
   return,-1
endif

rows=where(sp.inhid eq data_sch[0].inhid,count)
npts=count*nints_expected

dataoffs = long(sp[rows].dataOFF)/2L
nchs = long(sp[rows].nch) 
wts = sp.wt

;    for j=0L,count-1L do begin
;      pr(rows(j))=pr_cur
;      pc(rows(j))=pc_cur
offsets=dataoffs
offsets1=offsets+1L
offsets2=offsets+2L
offsets3=offsets+3L
offsets4=offsets+4L
integs = reform(data_sch.packdata(offsets)/10.e0,npts)
toffs  = reform(data_sch.packdata(offsets1),npts)
noises = reform(long(data_sch.packdata(offsets3))*32768L +  $
                       long(data_sch.packdata(offsets2)),npts)
rwts   = wts*integs 
scales = 2.e0^data_sch.packdata(offsets4)

ptr=make_array(total(nchs,/double),/long,/nozero)
ptr_scale=make_array(total(nchs,/double),nints_expected,/float,/nozero)
iptr_end=-1L & nch_prev=-1L
for i=0L, count-1L do begin
   iptr=iptr_end+1L & iptr_end=iptr+nchs[i]-1L
   if nchs[i] ne nch_prev then lin_nch=lindgen(nchs[i])
   nch_prev=nchs[i]
   ptr[iptr:iptr_end]=offsets[i]+lin_nch*2L+5L
   tmp=scales[i,*]
   ptr_scale[iptr:iptr_end,*]=rebin(scales[i,*],(iptr_end-iptr+1),nints_expected,/sample)
endfor
scales=reform(scales,npts)
ptr_i=ptr+1L
ch = ptr_scale*complex(data_sch.packdata[ptr],data_sch.packdata[ptr_i])
ch_npts=long(total(nchs,/double))*nints_expected
ch=reform(ch,ch_npts)

print,"finished reading data"

close,unit & free_lun,unit
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


