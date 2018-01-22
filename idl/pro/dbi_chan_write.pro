function dbi_chan_write
; 
; Writes the channel data to a bulk copy file from {ch}.
;
; parameters : none
; result = -1 (error), 1(ok)
; 
; eg. : result=dbi_chan_write()
; 
;  Data is dumped into file in a stream in channel order(then records).
;
   common global
   common data_set
;
   nints_expected = n_elements(in)
   nsp=n_elements(sp)
   ndata=total(long(sp.nrec*sp.nch))
   nrecs=total(long(sp.nrec))
;
;  open output file
;
   openw,unit,e.dataset_dir+'/tmp/sch_write',/get_lun,error=err
   IF err NE 0 THEN BEGIN
      print,!err_string
      return,-1
   ENDIF
;
;  Define nrec_prev and nch_prev
; 
   nrec_prev = -1L
   nch_prev  = -1L
;
; Define log2
;
   log2=alog10(2.d0)
;
; Begin loop over integrations
;  must set up arrays one integration at a 
;   time since # of bytes changes
;
   FOR i=0L,n_elements(in)-1L DO BEGIN
      inhid=in[i].inhid
      form='I2-C'
      rows = WHERE(sp.inhid eq in[i].inhid,count)
      dataoffs = sp[rows].dataOFF/2L
      nchs = long(sp[rows].nch) 
      nrecs= long(sp[rows].nrec)
      max_dataoffs=max([dataoffs])
      j=max([where(dataoffs eq max_dataoffs)])
      num_pts=long(max_dataoffs+nchs[j]*nrecs[j]*2L+nrecs[j]*5L)
      nbyt=2L*num_pts
      packdata=intarr(num_pts,nozero=0)
      first_byte=0L
      pr_cur=0L
      pc_cur=0L
      wts= sp[rows].wt
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
      data_max=-1.e4
      FOR j=0L,count-1L DO BEGIN
         pr_cur=pr[rows[j]]
         pc_cur=pc[rows[j]]
         ks=lindgen(nrecs[j])
         offsets=dataoffs[j]+(5L+nchs[j]*2L)*(ks)
         offsets1=offsets+1L
         offsets2=offsets+2L
         offsets3=offsets+3L
         offsets4=offsets+4L
         pr_curs=pr_cur + lindgen(nrecs[j])
         packdata[offsets]=re.integs[pr_curs]*10.e0
         packdata[offsets1]=re.toffs[pr_curs]
         ; JMC 3/28/99 changed the line below from sch.inhid to inhid
         if inhid le 500784 and nchs(j) le 1 then begin
           packdata[offsets1]= packdata[offsets1]-re.integs(pr_curs)/2.
         endif
;
; in the statement below changed 65536L to 32768L
;
         packdata[offsets3]=re.noises[pr_curs]/32768L
         packdata[offsets2]=re.noises[pr_curs]-long(packdata[offsets3])*32768L
         packdata[offsets4]=(alog10(double(re.scales[pr_curs]))/log2)-0.001
         ls=lindgen(nrecs[j]*nchs[j])
         doffsets=pc_cur+ls
;
;  to pick the data out of packdata for ch1: rec1 ... recn, then ch2
;  need a pointer array with index : 
;                  7, 7+5+2*nch, 7+10+2*nch, ... 7+(nrec-1)*5+(nrec-1)*nch 
;               2+(                        "                                )
;               4+(                        "                                )
;  this will fetch the real points, add 1 to this to get the imaginary pts
;  eg. nrec=3,nch=5 : 
;           7          22          37
;           9          24          39
;          11          26          41
;          13          28          43
;          15          30          45
; can get the list above w/ the following commands
;
; similarly for the scale factors want a pointer array which
; is : pr_curs(0) ... pr_curs(nrec-1),pr_curs(0) ... pr_curs(nrec-1) ... 
; can get this with the following command :

         IF (nchs[j] NE nch_prev OR nrecs[j] NE nrec_prev) THEN BEGIN
            offsetsr=transpose( 2L*lindgen(nchs[j],nrecs[j]) +5L $
               +5L*(lindgen(nchs[j],nrecs[j])/nchs[j]))
            offsetsi=offsetsr+1L
            scale_ptrs=(lindgen(nrecs[j])#make_array(nchs[j],/long,value=1L)) 
            nrec_prev=nrecs[j] 
            nch_prev=nchs[j]
         ENDIF
         data_max=max([data_max,abs(float(ch[doffsets])/ $
                      re.scales[pr_cur+scale_ptrs])])
         data_max=max([data_max,abs(imaginary(ch[doffsets])/ $
                      re.scales[pr_cur+scale_ptrs])])
         packdata[dataoffs[j]+offsetsr]=float(ch[doffsets])/ $
                                        re.scales[pr_cur+scale_ptrs]
         packdata[dataoffs[j]+offsetsi]=imaginary(ch[doffsets])/ $
                                        re.scales[pr_cur+scale_ptrs]
;
; scale factor needs to be changed
; 
        IF data_max GE 32768. THEN BEGIN
; take print statement out once we're sure this is correct
;         print,'rescaling data for inh# , sph# ',inhid,sp[rows[j]].sphid
          iscale=0L
          scale = 3.321928*alog10(data_max/32000.)
          if scale GT 0. THEN scale=scale+1.
          iscale=long(scale)
; Following two lines commented out by JMC on 2/14/00
;         scale=2.^iscale   
;         packdata[offsets4]=iscale
; Following three lines added by JMC on 2/14/00
          packdata[offsets4]=iscale+packdata[offsets4]
          scale=2.^packdata[offsets4]   
          scale = scale(transpose(lindgen(nchs[j],nrecs[j])/nchs[j]))
          packdata[dataoffs[j]+offsetsr]=float(ch[doffsets])/scale
          packdata[dataoffs[j]+offsetsi]=imaginary(ch[doffsets])/scale
        ENDIF
      ENDFOR
      WRITEU,unit,inhid,form,nbyt,nbyt,packdata
;
; read into server w/
; bcp sch_code in sch -f bcp.fmt_sch_write -T 300000 -U rdx -P rdxrdx -b 500
;
   ENDFOR
   CLOSE,unit 
   FREE_LUN,unit
   RETURN,1
END


