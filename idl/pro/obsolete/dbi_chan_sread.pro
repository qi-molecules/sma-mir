function dbi_chan_sread
; 
; Reads the channel data from a db table into {ch}.
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
nints_expected = long(n_elements(in))
nsp=long(n_elements(sp))
ndata=long(total(sp.nrec*sp.nch))
nrecs=long(total(sp.nrec))
integs=fltarr(nrecs,nozero=1)
toffs=fltarr(nrecs,nozero=1)
noises=fltarr(nrecs,nozero=1)
scales=fltarr(nrecs,nozero=1)
rwts=fltarr(nrecs,nozero=1)
ch=complexarr(ndata,nozero=1)
pr=make_array(nsp,/long,value=0L)
pc=make_array(nsp,/long,value=0L)
nrec_prev=-1L & nch_prev = -1L
pr_cur=0L & pc_cur=0L
;
; Now that the header data is read in - read in the channels
;
maxpack = 0L
numrows = CALL_EXTERNAL(e.idl_src+'db.so', 'db_query_sch', e.dbprocess, maxpack)
if e.debug then print, "number of rows in sch_read", numrows
sch={inhid:0L, form:'00000000000000000000',nbyt:0L,nbyt_pack:0L,packdata:intarr((maxpack+1)/2L, nozero=0)}

test = CALL_EXTERNAL(e.idl_src+'db.so', 'db_bind_sch', e.dbprocess, sch, maxpack)

for i=0L,nints_expected-1L do begin

;define the structure based on the amount of channel data
test = CALL_EXTERNAL(e.idl_src+'db.so', 'db_get_sch', e.dbprocess, sch)
while(test NE -1) do begin


  rows = where (sp.inhid eq sch.inhid,count)
  if count gt 0L then begin  
    dataoffs = long(sp(rows).dataOFF)/2L
    nchs = long(sp(rows).nch) 
    nrecs= long(sp(rows).nrec)

    wts= sp(rows).wt
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
      integs(pr_curs)=sch.packdata(offsets)/10.e0
      toffs(pr_curs)= sch.packdata(offsets1)
      if sch.inhid le 500784 and nchs(j) le 1 then begin
        toffs(pr_curs)= toffs(pr_curs)+integs(pr_curs)/2.
      endif
;
; in the statement below changed 65536L to 32768L
;
      noises(pr_curs)= long(sch.packdata(offsets3))*32768L +  $
                       long(sch.packdata(offsets2))
      scales(pr_curs)=2.e0^sch.packdata(offsets4)
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
      ch(doffsets)=scales(pr_cur+scale_ptrs)* $
          complex(sch.packdata(dataoffs(j)+offsetsr), $
                  sch.packdata(dataoffs(j)+offsetsi))    
      pr_cur=pr_cur+nrecs(j)
      pc_cur=pc_cur+nrecs(j)*nchs(j) 
    endfor
endif
test = CALL_EXTERNAL(e.idl_src+'db.so', 'db_get_sch', e.dbprocess, sch)  
endwhile
endfor

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
