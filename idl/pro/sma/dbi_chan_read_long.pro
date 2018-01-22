function dbi_chan_read_long,int_read=int_read
; 
; Reads the channel data from a bulk copy file into {ch}.
;
; parameters : none
; result = -1 (error), 1(ok)
; 
; eg. : result=dbi_chan_read()
; 
;  Data is dumped into arrays in a stream with pointers to the
;  start location appropriate to each particular spectral 
;  band (sp(i)).
; 
; Data and headers will be arranged in record order(then channels).
; For each band have :
;              integs(0 => nrec-1)  = integration time in secs
;              toffs (0 => nrec-1)  = time offset (secs) from 
;                                     c.ut(in(ps(i).iut)) 
;              noises(0 => nrec-1)  = noise
;              scales(0 => nrec-1)  = scale factor
;              wts   (0 => nrec-1)  = weight
;              ch( 0    => nrec-1  )= complex data for chan # 1
;              ch( nrec => 2*nrec-1)= complex data for chan # 2
; 
;  pr(i) is a pointer to the first record of header data (eg. integs)
;        associated w/ spectral band sp(i)
;  pc(i) is a pointer to the first record of channel #1
;        associated w/ spectral band sp(i)
; eg. to print the integs associated w/ row # 5 in sp structure :
; eg. print,re.integs(pr(5):pr(5)+(sp(5).nrec-1))
; eg. to print the toffs associated w/ row # 5 in sp structure :
; eg. print,re.toffs(pr(5):pr(5)+(sp(5).nrec-1))
; 
; eg. to print the complex channels associated w/ row # 5 in sp structure : 
; eg. (they appear in with rec # changing first, then chan #) 
; eg. print,ch( pc(5) : pc(5)+sp(5).nrec*sp(5).nch-1 )
;
common global
common data_set  

if (keyword_set(int_read)) then begin
   in_skip=int_read[0]
   nints_expected=int_read[1]+1 
endif else begin
   in_skip=0
   nints_expected=long(n_elements(in))
endelse

; ek: Find out how many integrations are expected by counting
;     the number of integrations in the integration header structure in
print, 'number of integrations ',nints_expected

; ek: Find out how many spectra by counting the number of spectra in the
;     spectrm header structure sp. The total number of spectra is 
;     (number of bands)*(number of baselines)*(number of integrations)
nsp=long(n_elements(sp))
print, 'number of spectra ',nsp

; ek: The number of data in each spectrum is the number of records
;     in each spectrum times the number of channels in each spectrum
;print,'records ', sp.nrec
;print,'channels ', sp.nch 
;print,'offsets ',sp.dataOFF
ndata=long(total(sp.nrec*sp.nch))
;print, 'total number of data ',ndata

; ek: The number of records in each spectrum
nrecs=long(total(sp.nrec))
;print, 'total number of records',nrecs

pts_per_int=16L
integs=fltarr(nrecs,nozero=1)
toffs=fltarr(nrecs,nozero=1)
noises=fltarr(nrecs,nozero=1)
scales=fltarr(nrecs,nozero=1)
rwts=fltarr(nrecs,nozero=1)
ch=complexarr(ndata,nozero=1)
pr=make_array(nsp,/long,value=0L)
pc=make_array(nsp,/long,value=0L)
nrec_prev=-1L & nch_prev = -1L
;
;  set up structure for short version of table
; ek: 	nbyt		the number of bytes in one integration
;	nbyt_pack	?
sch={inhid:0L,form:'1234',nbyt:0L,nbyt_pack:0L,$
     packdata:intarr(pts_per_int,nozero=0)}

; ek: first_byte is a counter to indicate where in the disk file to start
;     reading. It points to the begining of an integration which is the
;     same as the beginning of the sch structure.
first_byte=0L
pr_cur=0L
pc_cur=0L
openr,unit,e.idl_bcp+'sch_read',/get_lun,error=err
if err ne 0 then begin
  print,!err_string
  return,-1
endif

; ek: nbytes is total number of bytes in the data file on disk
status = fstat(unit) & nbytes = long64(status.size)
print,'number of bytes in sch_read ',nbytes
num_pts=20L

; ek: Loop over the number of integrations expected
for i=0L,nints_expected-1L do begin

;---------------------------------------------------
; ek: Check if the last byte expected exceeds the number of bytes
;     in the disk file. Adjust the number of bytes accordingly
  if ((first_byte+16L+2L*num_pts) gt nbytes) then begin

; ek: find_length: is a label
  find_length:  
;
; short last record in file
; adjust # pts from pts_per_int to num_pts
;
    num_pts=(nbytes-first_byte-16L)/2L
    pts_per_int=num_pts
    sch={inhid:0L,form:'1234',nbyt:0L,nbyt_pack:0L,$
         packdata:intarr(pts_per_int,nozero=0)}
  endif
; ek: end of the block:    if ((first_byte+16...
; ek: the num_pts expected has now been adjusted 
;--------------------------------------------------

;print,'pts_per_int ',pts_per_int

; ek: Read an sch structure which includes a small header
;     and the packdata integer array which contains the data.
  point_lun,unit,first_byte & readu,unit,sch

;  print,sch
  if (strpos(!VERSION.ARCH,'x86') ge 0) or (strpos(!VERSION.ARCH,'alpha') ge 0) then $
     sch = swap_endian(sch)
;  stop,sch

  if sch.form ne 'I2-C' then begin  ; check data format
    print,i,first_byte,sch.inhid,sch.form,sch.nbyt,sch.nbyt_pack, $
    sch.packdata(0:8),format='("rec:",i3," byte#:",i7," inh#:",i8,'+ $
    '" format:",a4," nbyt:",i8," nbyt_pack:",i8,/"   pack[0=>8]:",9i8)' 
    return,-1
  endif

;print,'n_elements(sch.packdata) ',n_elements(sch.packdata)
;npp = min([15,n_elements(sch.packdata)-1])
;    print,i,first_byte,sch.inhid,sch.form,sch.nbyt,sch.nbyt_pack, $
;    sch.packdata(0:npp), $
;    format='("rec:",i3," byte#:",i7," inh#:",i8,'+ $
;    '" format:",a4," nbyt:",i8," nbyt_pack:",i8/"   pack[0=> 7]:",8i8,'+ $
;    '/"   pack[8=>15]:",8i8)' 
; ek: Rows is a 1D array of indices where the integration numbers of the
;     spectra and the data headers match.
  rows = where (sp.inhid eq sch.inhid,count)

; print,'rows  ',rows
; print,'sp.inhid  ',sp.inhid
; print,'sch.inhid ',sch.inhid 

; ek: If any of the data and spectral headers refer to the same integration 
;     then proceed.
  if count gt 0L then begin  

; ek: dataoffs is a list of offsets which indicate the starting byte of 
;     each spectrum. The unit of dataoffs is 2 bytes (short).
;     Each offset in dataoffs is equal to a dataOFF/2 read from the spectrum
;     header struture sp where  dataOFF is the starting byte
;     (in units of 1 byte) of the spectrum. This number has to be
;     calculated by the program writing the disk file and written
;     into the sp header. This offset allows the idl program to
;     identify the appropriate header and spectrum data.
    dataoffs = long(sp(rows).dataOFF)/2L


    nchs = long(sp(rows).nch) 
;    nrecs= long(sp(rows).nrec)
    nrecs=long(intarr(count)+1)

; print,'dataoffs ',dataoffs
; print,'nchs ',nchs
; print,'nrecs ',nrecs

; ek: The number of bytes in packdata is 2 times num_pts
;     num_pts=long(total(nchs*nrecs)*2L+total(nrecs)*5L)
;     There are 4 numbers in the record header, but one of them is
;     4 bytes (written as 2 shorts). Every other number in the 
;     header (and the data) is a 2 byte short. So the 4 numbers 
;     in the header make 10 bytes.

    num_pts=sch.nbyt/2L

; ek: pts_per_int was initialized at 9, so on the first run through
;     the loop we will end up in the following if block.
;---------------------------------------------
    if (num_pts ne pts_per_int) then begin
      num_pts=min([num_pts,(nbytes-first_byte-10L)/2L])
      pts_per_int=num_pts
      sch={inhid:0L,form:'1234',nbyt:0L,nbyt_pack:0L,$
           packdata:intarr(pts_per_int,nozero=0)}
      point_lun,unit,first_byte & readu,unit,sch

;  print,sch
    if (strpos(!VERSION.ARCH,'x86') ge 0) or (strpos(!VERSION.ARCH,'alpha') ge 0) then $
     sch = swap_endian(sch)
;  stop,sch

      num_pts=sch.nbyt/2L
      if eof(unit) then goto, find_length 
    endif
;---------------------------------------------

    wts= sp(rows).wt

;print,'num_pts ',num_pts
;print,'wts ',wts

;
;   In the database archive, each record of data consists of 4 header 
;   numbers followed by a variable number of complex channels. 
;   The beginning of the record data stream is given by dataOFF from SPH.
;   Record format:
;      ITEM     SIZE OFF
;     integ      i*2   0       Integration time in .1secs
;     toff       i*2   2       Time offset from INH ut in secs
;     dnoise     i*4   4       Noise estimate in milliJanskies
;     scale      i*2   8       Scale exponent for data
;     ch[1].real i*2  10       Signed, scaled, short (2-byte integer) data
;     ch[1].imag i*2     
;     ch[2].real i*2      
;     ch[2].imag i*2     
;     ...
;
;   The total size of each record is 10+nch*4 bytes.
;   Each data point is stored as a 2 byte (short) integer. The restored data
;   (in Janskies) are computed as follows:
;                 restored_data = 2**scale * short_data
;
;   The noise estimate (dnoise) is stored as a 4byte integer in millijanskys. 
;
    for j=0L,count-1L do begin
      pr(rows(j))=pr_cur
      pc(rows(j))=pc_cur
      ks=lindgen(nrecs(j))
      offsets=dataoffs(j)+(5L+nchs(j)*2L)*(ks)
      offsets1=offsets+1L
      offsets2=offsets+2L
      offsets3=offsets+3L
      offsets4=offsets+4L
      pr_curs=pr_cur + lindgen(nrecs(j))

; ek: the integration time is converted from 0.1 secs to secs here
      integs(pr_curs)=sch.packdata(offsets)/10.e0
;print,'integs ',integs

      toffs(pr_curs)= sch.packdata(offsets1)
;print,'toffs ',toffs

; Some correction applicable to OVRO data from the distant past
;      if sch.inhid le 500784 and nchs(j) le 1 then begin
;        toffs(pr_curs)= toffs(pr_curs)+integs(pr_curs)/2.
;      endif

; ek: The noise estimate is a long integer written as two shorts and
;     reconstituted here.
      noises(pr_curs)= long(sch.packdata(offsets3))*32768L +  $
                       long(sch.packdata(offsets2))
;print,'noises ',noises

; ek: The scale factor read from the disk is an exponent, here it is 
; converted to a number before storing to memory
      scales(pr_curs)=2.e0^sch.packdata(offsets4)
;print,'scales ',scales

      rwts(pr_curs)= wts(j)*integs(pr_curs)
      ls=lindgen(nrecs(j)*nchs(j))
      doffsets=pc_cur+ls
;
;  to pick the data out of packdata for ch1: rec1 ... recn, then ch2
;  need a pointer array with index : 
;                  5, 5+5+2*nch, 5+10+2*nch, ... 5+(nrec-1)*5+(nrec-1)*nch 
;               2+(                        "                                )
;               4+(                        "                                )
;  this will fetch the real points, add 1 to this to get the imaginary pts
;  eg. nrec=3,nch=5 : 
;           5          20          35
;           7          22          37
;           9          24          39
;          11          26          41
;          13          28          43
; can get the list above w/ the following commands
;
; similarly for the scale factors want a pointer array which
; is : pr_curs(0) ... pr_curs(nrec-1),pr_curs(0) ... pr_curs(nrec-1) ... 
; can get this with the following command :

   if (nchs(j) ne nch_prev or nrecs(j) ne nrec_prev) then begin
     offsetsr= transpose( 2*lindgen(nchs(j),nrecs(j)) +5L $
                  +5L*( lindgen(nchs(j),nrecs(j))/nchs(j)))
     offsetsi=offsetsr+1L
     scale_ptrs= (lindgen(nrecs(j)) # make_array(nchs(j),/long,value=1L)) 
     nrec_prev=nrecs(j) & nch_prev=nchs(j)
   endif


; ek: The data are scaled here

      ch(doffsets)=scales(pr_cur+scale_ptrs)* $
          complex(sch.packdata(dataoffs(j)+offsetsr), $
                  sch.packdata(dataoffs(j)+offsetsi))    

if (sch.inhid eq -1) then begin
	for ii=0,n_elements(doffsets)-1 do begin
	print,'ii, doffsets[ii],pr_cur,scale_ptrs[ii],dataoffs[j],offsetsr[ii],offsetsi[ii]',$
	ii, doffsets[ii],pr_cur,scale_ptrs[ii],dataoffs[j],offsetsr[ii],offsetsi[ii]
	print,'scales,realS,imagS,realF,imagF ',scales(pr_cur+scale_ptrs[ii]), $
	sch.packdata(dataoffs(j)+offsetsr[ii]),sch.packdata(dataoffs(j)+offsetsi[ii]), $
	float(ch(doffsets[ii])),imaginary(ch(doffsets[ii]))
	endfor
endif



      pr_cur=pr_cur+nrecs(j)
      pc_cur=pc_cur+nrecs(j)*nchs(j) 

; ek: This ends the loop for j=0L,count-1L ...
;     The endif refers back to if count gt 0L
    endfor
  endif
pts_per_int=num_pts
first_byte=long64(first_byte+16L+sch.nbyt)
; ek: This is the end of the main loop over the integrations, for i=0L,nints ...
endfor

;print,sch.packdata
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
return,0
end


