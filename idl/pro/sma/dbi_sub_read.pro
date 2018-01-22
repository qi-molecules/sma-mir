function dbi_sub_read, in_rx=in_rx, in_sideband=in_sideband, $
                       in_source = in_source, in_band = in_band, $
                       in_baseline=in_baseline, in_integration=in_integration, $
                       min_scan_length=min_scan_length, verbose=verbose, preview=preview
;
;
; usage: result=dbi_sub_read(sideband) 
;
; FIRST PART OF CODE FROM DBI_HEAD_READ()
; 
; Read header data from bulk copy files (in,bl,sp,codes) 
; into structures : in,bl,sp,c with pointers pi,pb,ps.
; result=-1 (error), 1(ok)
;
; Database tables for a specific track are first created using 
; file sql_read_track ( isql -U rdx -P rdxrdx < db_read_track )
; then, the tables are bulkcopied out to unix files.
;
; parameters : none
;
; Note that the bcp command described above should have created 
; four files in the working directory :
; in_read, bl_read, sp_read, codes_read. 
;
; eg. :   result=dbi_head_read() 
;
; pi, pb, and ps are pointer arrays from sp rows to rows
;  ... in structures in, bl, and sp -- for example :
;  ... bl(pb(i)) is the row in bl associated with row i in sp
;  ... Character strings are stored in structure 'c' with 
;  ... c.icode_s and c.icode_tag being the structure name
;  ... and tag of the integer variable each refers to.
;
; eg. : to print complete header associated w/ a spectrum =>
; print, in(pi(i)), bl(pb(i)),sp(i)
; print, in(pi(i)), bl(pb(i)),sp(ps(i))
; eg. : to get a list of all int's => 
; print,in.int 
;

common global
common data_set

;
print,'******** OS TYPE ***********'
print,'Platform =',!VERSION.ARCH
;
;  set up structures for header table
;
inh_temp={conid:0L,icocd:0,traid:0L,inhid:0L,$
         int:0L,itq:0,az:0e,el:0e,ha:0e,$
         iut:0,$
         iref_time:0,$
         dhrs:0d,vc:0e,ivctype:0,sx:0d,sy:0d,sz:0d,rinteg:0e,proid:0L,$
         souid:0L,isource:0,ipos:0,offx:0e,offy:0e,$
         iofftype:0,ira:0,$
         idec:0,rar:0d,decr:0d,epoch:0e,sflux:0e,size:0e}
blh_temp={blhid:0L,inhid:0L,isb:0,ipol:0,pa:0e,$
         iaq:0,ibq:0,icq:0,ioq:0,irec:0,$
         iifc:0,u:0e,v:0e,w:0e,prbl:0e,angres:0e,vis:0e,coh:0e,$
         sigcoh:0e,csnr:0e,vflux:0e,cnoise:0e,avedhrs:0d,$
         ampave:0e,phaave:0.e,$
         tpvar:0.e,blsid:0L,itel1:0,itel2:0,iblcd:0,ble:0.e,$
         bln:0e,blu:0e,soid:0L}
sph_temp={sphid:0L,blhid:0L,inhid:0L,igq:0,ipq:0,iband:0,$
         ipstate:0,tau0:0e,vel:0d,vres:0e,ivtype:0,fsky:0d,$
         fres:0e,tssb:0e,integ:0e,wt:0e,itaper:0,snoise:0e,$
         nch:0,nrec:0,dataOFF:0L,linid:0L,itrans:0,$
         rfreq:0d,pasid:0,gaiidamp:0,gaiidpha:0,$
         flcid:0,atmid:0}
code_temp={v_name:'123456789012',icode:0, $
           code:'12345678901234567890123456',ncode:0}

  print, " ---------- Pre-Reading Headers ----------"

 reftime0=systime(1)
 reftime=reftime0
;
openr,unit,e.idl_bcp+'in_read',/get_lun,error=err
if err ne 0 then begin
  print,!err_string
  return,-1
endif
;
; see how many rows, set up tables w/ that number of rows and read data
;
status = fstat(unit) & nrows = status.size/132
if e.debug then print,nrows, status.size,' inh rows'

in = replicate(inh_temp,nrows)
point_lun,unit,0L
for i=0L,(nrows-1L) do  begin
  readu,unit,inh_temp
  if (strpos(!VERSION.ARCH,'x86') ge 0) or (strpos(!VERSION.ARCH,'alpha') ge 0) then $
     inh_temp = swap_endian(inh_temp)
;  stop,inh_temp
  in(i)=inh_temp
;  if e.debug then print, in(i)
endfor
;
close,unit & free_lun,unit
;
  nowtime = systime(1)
  print, "Headers(in): Reading Finished, Time elapsed ", fix(nowtime -reftime), "sec"
  reftime=nowtime
;
openr,unit,e.idl_bcp+'bl_read',/get_lun,error=err
if err ne 0 then begin
  print,!err_string
  return,-1
endif
;
; see how many rows, set up tables w/ that number of rows and read data
;
status = fstat(unit) & nrows = status.size/118
if e.debug then print,nrows, status.size, 'blh rows'

bl = replicate(blh_temp,nrows)
point_lun,unit,0L
for i=0L,(nrows-1L) do  begin
  readu,unit,blh_temp
  if (strpos(!VERSION.ARCH,'x86') ge 0) or (strpos(!VERSION.ARCH,'alpha') ge 0) then $
     blh_temp = swap_endian(blh_temp)
;  stop,blh_temp
  bl(i)=blh_temp
;  print, 'i =',i
;  print, bl(i)
;  if e.debug then print, bl(i)
endfor
;
close,unit & free_lun,unit
;
  nowtime = systime(1)
  print, "Headers(bl): Reading Finished, Time elapsed ", fix(nowtime -reftime), "sec"
  reftime=nowtime
;
openr,unit,e.idl_bcp+'sp_read',/get_lun,error=err
if err ne 0 then begin
  print,!err_string
  return,-1
endif
;
; see how many rows, set up tables w/ that number of rows and read data
;
status = fstat(unit) & nrows = status.size/100
if e.debug then print,nrows, status.size,' sph rows'

if (nrows gt 1000) then begin

  ; TEST LOADING THE FIRST 1000 ROWS FOR TIME ESTIMATE
  sptmp = replicate(sph_temp,nrows)
  point_lun,unit,0L

  looptime=systime(1)

  for i=0L,(1000L-1L) do  begin
    readu,unit,sph_temp
    if (strpos(!VERSION.ARCH,'x86') ge 0) or (strpos(!VERSION.ARCH,'alpha') ge 0) then $
       sph_temp = swap_endian(sph_temp)
    sptmp(i)=sph_temp 
  endfor
 
  looptime=(systime(1)-looptime)*nrows/1000.0
  print, "Headers(sp): Reading  Started, Time Estimate             ", fix(looptime), "sec"
  ; END OF TEST LOADING

  sptmp=0 ; to free up memory

  sp = replicate(sph_temp,nrows)
  point_lun,unit,0L

  for i=0L,(nrows-1L) do  begin
    readu,unit,sph_temp
    if (strpos(!VERSION.ARCH,'x86') ge 0) or (strpos(!VERSION.ARCH,'alpha') ge 0) then $
       sph_temp = swap_endian(sph_temp)
    ;  stop,sph_temp
    sp(i)=sph_temp 
    ;  if e.debug then print, i,sp(i).dataOFF
  endfor

endif else begin

  ; NOT TOO MANY ROWS, JUST READ

  sp = replicate(sph_temp,nrows)
  point_lun,unit,0L

  for i=0L,(nrows-1L) do  begin
    readu,unit,sph_temp
    if (strpos(!VERSION.ARCH,'x86') ge 0) or (strpos(!VERSION.ARCH,'alpha') ge 0) then $
       sph_temp = swap_endian(sph_temp)
    ;  stop,sph_temp
    sp(i)=sph_temp 
    ;  if e.debug then print, i,sp(i).dataOFF
  endfor

endelse
;
close,unit & free_lun,unit
;
  nowtime = systime(1)
  print, "Headers(sp): Reading Finished, Time elapsed ", fix(nowtime -reftime), "sec"
  reftime=nowtime
;
openr,unit,e.idl_bcp+'codes_read',/get_lun,error=err
if err ne 0 then begin
  print,!err_string
  return,-1
endif
;
; see how many rows, set up tables w/ that number of rows and read data
;
status = fstat(unit) & nrows = status.size/42
if e.debug then print,nrows,' code rows'

aq = strarr(1)
band = strarr(1)
blcd = strarr(1)
bq = strarr(1)
cocd = strarr(1)
cq = strarr(1)
dec = strarr(1)
gq = strarr(1)
ifc = strarr(1)
offtype = strarr(1)
oq = strarr(1)
pol = strarr(1)
pos = strarr(1)
pq = strarr(1)
pstate = strarr(1)
ra = strarr(1)
rec = strarr(1)
ref_time = strarr(1)
sb = strarr(1)
source = strarr(1)
taper = strarr(1)
tel1 = strarr(1)
tel2 = strarr(1)
tq = strarr(1)
trans = strarr(1)
ut = strarr(1)
vctype = strarr(1)
vtype = strarr(1)

aa = bytarr(42)
point_lun,unit,0L
for i=0L,(nrows-1L) do  begin

  readu,unit,aa
  code_temp.v_name = string(byte(aa,0,12))
  code_temp.icode  = fix(aa,12)
  code_temp.code   = string(byte(aa,14,26))
  code_temp.ncode   = fix(aa,40)

  if (strpos(!VERSION.ARCH,'x86') ge 0) or (strpos(!VERSION.ARCH,'alpha') ge 0) then begin
     code_temp.icode = swap_endian(code_temp.icode)
     code_temp.ncode = swap_endian(code_temp.ncode)
  endif

; stop,code_temp

  result = execute('difd='+string(code_temp.icode)+ $
    '-n_elements('+strtrim(code_temp.v_name,2L)+')')
  if (difd ge 0) then begin
    result = execute(strtrim(code_temp.v_name,2L)+'=['+ $
        strtrim(code_temp.v_name,2l)+',strarr('+strtrim(string(difd),2L)+'+1)]')
    if (result ne 1) then begin 
        print,'could not concat array ',strtrim(code_temp.v_name,2L)
    endif
;  result = execute('difd='+ $
;    'n_elements('+strtrim(code_temp.v_name,2L)+')')
;	print,code_temp.v_name,' now ', difd ,' elements'
  endif
  case strtrim(code_temp.v_name,2L) of
       'aq'   : aq(code_temp.icode)=strtrim(code_temp.code,2L)          
       'band' : band(code_temp.icode)=strtrim(code_temp.code,2L)
       'blcd' : blcd(code_temp.icode)=strtrim(code_temp.code,2L)
       'bq'   : bq(code_temp.icode)=strtrim(code_temp.code,2L)
       'cocd' : cocd(code_temp.icode)=strtrim(code_temp.code,2L)
       'cq'   : cq(code_temp.icode)=strtrim(code_temp.code,2L) 
       'dec'  : dec(code_temp.icode)=strtrim(code_temp.code,2L)
       'gq'   : gq(code_temp.icode)=strtrim(code_temp.code,2L)
       'ifc'  : ifc(code_temp.icode)=strtrim(code_temp.code,2L) 
       'offtype' : offtype(code_temp.icode)=strtrim(code_temp.code,2L)
       'oq'   : oq(code_temp.icode)=strtrim(code_temp.code,2L)
       'pol'  : pol(code_temp.icode)=strtrim(code_temp.code,2L)
       'pos'  : pos(code_temp.icode)=strtrim(code_temp.code,2L) 
       'pq'   : pq(code_temp.icode)=strtrim(code_temp.code,2L)
       'pstate': pstate(code_temp.icode)=strtrim(code_temp.code,2L)
       'ra'   : ra(code_temp.icode)=strtrim(code_temp.code,2L)  
       'rec'  : rec(code_temp.icode)=strtrim(code_temp.code,2L) 
       'ref_time': ref_time(code_temp.icode)=strtrim(code_temp.code,2L)
       'sb'   : sb(code_temp.icode)=strtrim(code_temp.code,2L)
       'source': source(code_temp.icode)=strtrim(code_temp.code,2L)
       'taper': taper(code_temp.icode)=strtrim(code_temp.code,2L) 
       'tel1' : tel1(code_temp.icode)=strtrim(code_temp.code,2L)
       'tel2' : tel2(code_temp.icode)=strtrim(code_temp.code,2L)
       'tq'   : tq(code_temp.icode)=strtrim(code_temp.code,2L)
       'trans' : trans(code_temp.icode)=strtrim(code_temp.code,2L)
       'ut'   : ut(code_temp.icode)=strtrim(code_temp.code,2L)
       'vctype': vctype(code_temp.icode)=strtrim(code_temp.code,2L)          
       'vtype' : vtype(code_temp.icode)=strtrim(code_temp.code,2L)          
       else : print,strtrim(code_temp.v_name,2L),' not recognized !'
  endcase         
endfor
close,unit & free_lun,unit
;
if e.debug then print,'inh# ',in(0).inhid,' => ',in(n_elements(in)-1).inhid
if e.debug then print,'blh# ',bl(0).blhid,' => ',bl(n_elements(bl)-1).blhid
if e.debug then print,'sph# ',sp(0).sphid,' => ',sp(n_elements(sp)-1).sphid
;
; group all character codes strings into c structure
; make sure qualifiers have a blank space
;
    jb=where (aq eq '',count) & if count gt 0 then aq(jb)=' '
    jb=where (bq eq '',count) & if count gt 0 then bq(jb)=' '
    jb=where (cq eq '',count) & if count gt 0 then cq(jb)=' '
    jb=where (gq eq '',count) & if count gt 0 then gq(jb)=' '
    jb=where (oq eq '',count) & if count gt 0 then oq(jb)=' '
    jb=where (pq eq '',count) & if count gt 0 then pq(jb)=' '

    icode_s=['in','in','in','in','in','bl', $
             'bl','bl','bl','bl','bl','bl','bl', $
             'bl','bl','bl','sp','sp','sp','sp', $
             'sp','sp','sp','in','in', $
             'in','in','in']
    icode_tag=['icocd','iut','iref_time','itq','ivctype','isb', $
               'ipol', 'iaq','ibq','icq','ioq','irec','iifc', $
               'itel1','itel2','iblcd','igq','ipq','iband','ipstate', $
               'ivtype','itaper','itrans','isource','ipos', $
               'iofftype','ira','idec']

; icode_s and icode_tag must come last in the structure

c = { $
     cocd:cocd,ut:ut,ref_time:ref_time, $
     tq:tq,vctype:vctype,sb:sb,pol:pol,$
     aq:aq,bq:bq,cq:cq,oq:oq,rec:rec,$
     ifc:ifc,tel1:tel1,tel2:tel2,blcd:blcd,$
     gq:gq,pq:pq,band:band,pstate:pstate,$
     vtype:vtype,taper:taper,trans:trans,$
     source:source,pos:pos,offtype:offtype,$
     ra:ra,dec:dec,icode_s:icode_s,icode_tag:icode_tag}

;
; put in empty ca (cal) structure
;
  ca={cal_type:strarr(1),x_var:strarr(1),y_var:strarr(1), $
    tel_bsl:strarr(1),inhid_beg:lonarr(1), $
    inhid_end:lonarr(1),blcd_tel:strarr(1), $
    iblcd_itel:intarr(1),rec:strarr(1),irec:intarr(1), $
    isb:intarr(1),band:strarr(1),iband:intarr(1), $
    sb:strarr(1),pc_xbeg:lonarr(1),pc_xend:lonarr(1), $
    pc_ybeg:lonarr(1),pc_yend:lonarr(1),cal_parm:fltarr(1), $
    x:make_array(1,/float,value=!values.f_nan), $
    y:make_array(1,/float,value=!values.f_nan), $
    cal_exist:0}
;
; removal of wt le 0 data is take care of in db_read_utable 
; and db_read_track sql
;
;
;  sort the output structures, just in case
;

;
  nowtime = systime(1)
  print, "Headers all: Reading Finished, Time elapsed ", fix(nowtime -reftime), "sec"
  reftime=nowtime
;

; HEADER READING DONE, PREPARE INDICES FOR SPECTRA DATA READING

j=sort(in.inhid)
in=in(j)
j=sort(bl.blhid)
bl=bl(j)
j=sort(sp.sphid)
sp=sp(j)

  in_ori=in
  bl_ori=bl
  sp_ori=sp

;
  nowtime = systime(1)
  print, "Headers    : Pre-linking/Mark"
  reftime=nowtime
;
; set up pointer arrays for correspondence from sp to in and bl
; set up a similar one for sp even though it is not needed
;
pi_ori = make_array(n_elements(sp),/long,value=0)
pb_ori = make_array(n_elements(sp),/long,value=0)
ps_ori = make_array(n_elements(sp),/long,value=0)
in_oriprev=-1L
bl_oriprev=-1L
in_oriindex=0L
bl_oriindex=0L
in_orimax=n_elements(in)-1L
bl_orimax=n_elements(bl)-1L
for i=0L,(n_elements(sp)-1L) do begin
  if (sp(i).inhid ne in_oriprev) then begin
    in_oriupper= min([in_oriindex+2L,in_orimax])
    in_oriindex = in_oriindex + $
           max([where(in(in_oriindex:in_oriupper).inhid eq sp(i).inhid,count)])
    if (count le 0) then in_oriindex = max([where(in.inhid eq sp(i).inhid)])
    in_oriprev = sp(i).inhid
  endif
  pi_ori(i) = in_oriindex
  if (sp(i).blhid ne bl_oriprev) then begin
    bl_oriindex= max([0L,bl_oriindex-45L])
    bl_oriupper= min([bl_oriindex+90L,bl_orimax])
    bl_oriindex = bl_oriindex + $
           max([where(bl(bl_oriindex:bl_oriupper).blhid eq sp(i).blhid,count)])
    if (count le 0) then bl_oriindex = max([where(bl.blhid eq sp(i).blhid)])
    bl_oriprev = sp(i).blhid
  endif
  pb_ori(i) = bl_oriindex
  ps_ori(i) = i
endfor
;

; THIS IS THE SECTION FOR PREVIEW, GOING OVER HEADERS AND QUIT
  if keyword_set(preview) then begin
    print,"PREVIEW MODE"

    iprv = where(bl_ori.inhid eq in_ori[0].inhid)
    alltels = [c.tel1(bl_ori[iprv].itel1),c.tel2(bl_ori[iprv].itel2)]
    print," Antennas  : ", $
           alltels[uniq(alltels, sort(alltels))]
    print," Baselines : ", $
           c.blcd(bl_ori[uniq(c.blcd(bl_ori[iprv].iblcd), sort(c.blcd(bl_ori[iprv].iblcd)))].iblcd)

    jprv = uniq(c.rec(bl_ori[iprv].irec), sort(c.rec(bl_ori[iprv].irec)))
    print," Receiving Bands : ", $
           c.rec(bl_ori[jprv].irec)

    for lprv = 0, n_elements(jprv)-1 do begin
      print, " --- Band ",c.rec(bl_ori[jprv[lprv]].irec), " ---"
      kprv = where(sp_ori.blhid eq bl_ori[jprv[lprv]].blhid)
      print," Sidebands  : ", $
             c.sb(bl_ori[uniq(c.sb(bl_ori[kprv].isb), sort(c.sb(bl_ori[kprv].isb)))].isb)
      mprv = uniq(c.band(sp_ori[kprv].iband), sort(c.band(sp_ori[kprv].iband)))

      print," Spec Bands : ", $
             c.band(sp_ori[mprv].iband)
;      print," Spec Chan  : ", $
;             sp_ori[mprv].nch
;      print," Spec Width : ", $
;             string(sp_ori[mprv].nch*sp_ori[mprv].fres,format='(F8.2)')
    endfor

    iprv_last = 0
    for iprv = 0, n_elements(in_ori)-1 do begin
      if (c.source(in_ori[iprv].isource) ne c.source(in_ori[iprv_last].isource) ) then begin
        print, c.ut(in_ori[iprv_last].iut), " : INT ",string(in_ori[iprv_last].int,format='(I5)'), " to ",string(in_ori[iprv].int,format='(I5)'), " on ", c.source(in_ori[iprv_last].isource)
        iprv_last = iprv
      endif
    endfor
    print, c.ut(in_ori[iprv_last].iut), " : INT ",string(in_ori[iprv_last].int,format='(I5)'), " to ",string(in_ori[iprv-1L].int,format='(I5)'), " on ", c.source(in_ori[iprv_last].isource)

    return, 1
  endif

; THIS IS WHERE WE DECIDE WHAT INDICES OF DATA WE WILL EVENTUALLY READ
; Using similar methods in "select"

  cmd = "sp_indexlist = where ("
  tmpcmd = ''

  if keyword_set(in_integration) then begin
    in_int = string(in_integration)
    if (n_elements(in_int) ge 2) then begin
      tmpcmd = tmpcmd+'( (in(pi_ori).int ge '+strtrim(in_int[0],2)
      tmpcmd = tmpcmd+' and in(pi_ori).int le '+strtrim(in_int[1],2)+')'
      if n_elements(in_int) gt 2 then begin
        for i = 2, n_elements(in_int)-1, 2 do begin
          tmpcmd = tmpcmd +' or (in(pi_ori).int ge '+strtrim(in_int[i],2)
          tmpcmd = tmpcmd +' and in(pi_ori).int le '+strtrim(in_int[i+1],2)+')'
        endfor
      endif
      tmpcmd = tmpcmd + ')'
    endif
  endif

  if keyword_set(in_source) then begin
    if tmpcmd ne '' then tmpcmd = tmpcmd + ' and '
    in_source = string(in_source)
    if (in_source[0] ne '') then begin
      tmpcmd = tmpcmd+"( (c.source(in(pi_ori).isource) eq '"+ strtrim(in_source[0],2) + "')"
      if n_elements(in_source) gt 1 then begin
        for i = 1, n_elements(in_source)-1 do begin
          tmpcmd = tmpcmd+" or (c.source(in(pi_ori).isource) eq '"+ strtrim(in_source[i],2) + "')"
        endfor
      endif
      tmpcmd = tmpcmd + ' )'
    endif
  endif

  if keyword_set(in_rx) then begin
    if tmpcmd ne '' then tmpcmd = tmpcmd + ' and '
    in_rx = string(in_rx)
    if (in_rx[0] ne '') then begin
      tmpcmd = tmpcmd+"( (c.rec(bl(pb_ori).irec) eq '"+ strtrim(in_rx[0],2) + "')"
      if n_elements(in_rx) gt 1 then begin
        for i = 1, n_elements(in_rx)-1 do begin
          tmpcmd = tmpcmd+" or (c.rec(bl(pb_ori).irec) eq '"+ strtrim(in_rx[i],2) + "')"
        endfor
      endif
      tmpcmd = tmpcmd + ' )'
    endif
  endif

  if keyword_set(in_sideband) then begin
    if tmpcmd ne '' then tmpcmd = tmpcmd + ' and '
    in_sideband = string(in_sideband)
    if (in_sideband[0] ne '') then begin
      tmpcmd = tmpcmd+"( (c.sb(bl(pb_ori).isb) eq '"+ strtrim(in_sideband[0],2) + "')"
      if n_elements(in_sideband) gt 1 then begin
        for i = 1, n_elements(in_sideband)-1 do begin
          tmpcmd = tmpcmd+" or (c.sb(bl(pb_ori).isb) eq '"+ strtrim(in_sideband[i],2) + "')"
        endfor
      endif
      tmpcmd = tmpcmd + ' )'
    endif
  endif

  if keyword_set(in_baseline) then begin
    if tmpcmd ne '' then tmpcmd = tmpcmd + ' and '
    in_baseline = string(in_baseline)
    if (in_baseline[0] ne '') then begin
      tmpcmd = tmpcmd+"( (c.blcd(bl(pb_ori).iblcd) eq '"+ strtrim(in_baseline[0],2) + "')"
      if n_elements(in_baseline) gt 1 then begin
        for i = 1, n_elements(in_baseline)-1 do begin
          tmpcmd = tmpcmd+" or (c.blcd(bl(pb_ori).iblcd) eq '"+ strtrim(in_baseline[i],2) + "')"
        endfor
      endif
      tmpcmd = tmpcmd + ' )'
    endif
  endif

  if keyword_set(in_baseline) then begin
    if tmpcmd ne '' then tmpcmd = tmpcmd + ' and '
    in_baseline = string(in_baseline)
    if (in_baseline[0] ne '') then begin
      tmpcmd = tmpcmd+"( (c.blcd(bl(pb_ori).iblcd) eq '"+ strtrim(in_baseline[0],2) + "')"
      if n_elements(in_baseline) gt 1 then begin
        for i = 1, n_elements(in_baseline)-1 do begin
          tmpcmd = tmpcmd+" or (c.blcd(bl(pb_ori).iblcd) eq '"+ strtrim(in_baseline[i],2) + "')"
        endfor
      endif
      tmpcmd = tmpcmd + ' )'
    endif
  endif

  if keyword_set(in_band) then begin
    if tmpcmd ne '' then tmpcmd = tmpcmd + ' and '
    in_band = string(in_band)
    if (in_band[0] ne '') then begin
      tmpcmd = tmpcmd+"( (c.band(sp(ps_ori).iband) eq '"+ strtrim(in_band[0],2) + "')"
      if n_elements(in_band) gt 1 then begin
        for i = 1, n_elements(in_band)-1 do begin
          tmpcmd = tmpcmd+" or (c.band(sp(ps_ori).iband) eq '"+ strtrim(in_band[i],2) + "')"
        endfor
      endif
      tmpcmd = tmpcmd + ' )'
    endif
  endif

  if keyword_set(min_scan_length) then begin
    if (min_scan_length lt 10) then min_scan_length = 10
    if tmpcmd ne '' then tmpcmd = tmpcmd + ' and '
      tmpcmd = tmpcmd+"( (sp(ps_ori).integ gt '"+ strtrim(string(min_scan_length),2) + "')"
      tmpcmd = tmpcmd + ' )'
  endif

  if (tmpcmd ne '') then begin
    cmd = cmd + tmpcmd + ')'
    result = execute(cmd)
  endif else begin
    sp_indexlist = where(in(pi_ori).inhid ge 0)
  endelse

  if (n_elements(sp_indexlist) eq 1 and sp_indexlist[0] eq -1) then begin
    print, '!!!NOTHING TO LOAD IN!!!'
    return, -1
  endif

;  stop,cmd

  bl_indexlist =  pb_ori(sp_indexlist(uniq(bl(pb_ori(sp_indexlist)).blhid) ))
  in_indexlist =  pi_ori(sp_indexlist(uniq(in(pi_ori(sp_indexlist)).inhid) ))

  in=in(in_indexlist)
  bl=bl(bl_indexlist)
  sp=sp(sp_indexlist)

;if keyword_set(in_band) then begin
;   ii=where(in_band[0] eq c.band,count)
;   if count le 0 then begin
;      print,'***Wrong band name ! Quit !***'
;      return,-1
;   endif
;   i_band=sp.iband eq ii[0]
;   if n_elements(in_band) gt 1 then begin
;      for i=1,n_elements(in_band)-1 do begin
;         ii=where(in_band[i] eq c.band,count)
;         if count le 0 then begin
;            print,'***Wrong band name! Quit !***'
;            return,-1
;         endif
;         i_band=i_band or (sp.iband eq ii[0])
;      endfor
;   endif
;   sp=sp(where(i_band))
;endif

;
  nowtime = systime(1)
  print, "Headers    : Pre-linking/Mark, Time elapsed ", fix(nowtime-reftime), "sec"
  print, "Headers    : Linking"
  reftime=nowtime

; NOW WE KNOW WHAT SUBSET OF DATA TO READ FROM THE SP (SPECTRA DATAFILE)
; FIRST LINK THE HEADER INDICES FOR THE USEFUL DATASET

;
; set up pointer arrays for correspondence from sp to in and bl
; set up a similar one for sp even though it is not needed
;
pi = make_array(n_elements(sp),/long,value=0)
pb = make_array(n_elements(sp),/long,value=0)
ps = make_array(n_elements(sp),/long,value=0)
in_prev=-1L
bl_prev=-1L
in_index=0L
bl_index=0L
in_max=n_elements(in)-1L
bl_max=n_elements(bl)-1L
for i=0L,(n_elements(sp)-1L) do begin
  if (sp(i).inhid ne in_prev) then begin
    in_upper= min([in_index+2L,in_max])
    in_index = in_index + $
           max([where(in(in_index:in_upper).inhid eq sp(i).inhid,count)])
    if (count le 0) then in_index = max([where(in.inhid eq sp(i).inhid)])
    in_prev = sp(i).inhid
  endif
  pi(i) = in_index
  if (sp(i).blhid ne bl_prev) then begin
    bl_index= max([0L,bl_index-45L])
    bl_upper= min([bl_index+90L,bl_max])
    bl_index = bl_index + $
           max([where(bl(bl_index:bl_upper).blhid eq sp(i).blhid,count)])
    if (count le 0) then bl_index = max([where(bl.blhid eq sp(i).blhid)])
    bl_prev = sp(i).blhid
  endif
  pb(i) = bl_index
  ps(i) = i
endfor

;
  nowtime = systime(1)
  print, "Headers    : Linking         , Time elapsed ", fix(nowtime-reftime), "sec"
  reftime=nowtime
;

pif=pi
pbf=pb
psf=ps
pis=pi
pbs=pb
pss=ps
pil=pi
pbl=pb
psl=ps

if e.debug then begin
   print,strcompress('integrations from '+string(min(in.int))+' to '+string(max(in.int)))
endif

if e.prog_help then begin
  print,' '
  print,'  structure with integration headers :'   
  help,/structure,in
  print,' '
  print,'  structure with baseline headers :'   
  help,/structure,bl
  print,' '
  print,'  structure with spectrum headers :'   
  help,/structure,sp
  print,' '
  print,'  structure with string codes :'   
  help,/structure,c
  print,' '
  print,'    pi, pb, and ps are pointer arrays from sp rows to rows'$
       ,'    ... in structures in, bl, and sp -- for example :'$
       ,'    ... bl(pb(i)) is the row in bl associated with row i in sp'
  print,'    Character strings are store in structure c w/ '$
       ,'    c.icode_s and c.icode_tag being the structure name'$
       ,'    and tag of the integer variable each refers to.'
  print,' '
  print,'    To print complete header associated w/ i th spectrum :   '$
       ,'    ... print, in(pi(i)), bl(pb(i)),sp(i)'$
       ,'    .or print, in(pi(i)), bl(pb(i)),sp(ps(i))'
endif

; HEADER CONSTRUCTION DONE, READY FOR SPECTRA DATA READING

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
; ek: Find out how many integrations are expected by counting
;     the number of integrations in the integration header structure in
nints_expected = long(n_elements(in))
print, 'number of integrations to be accepted ',nints_expected

; ek: Find out how many spectra by counting the number of spectra in the
;     spectrm header structure sp. The total number of spectra is 
;     (number of bands)*(number of baselines)*(number of integrations)
nsp=long(n_elements(sp))
print, 'number of spectra to be accepted ',nsp

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

;
  nowtime = systime(1)
  print, "Spectra    : Reading  Reday to Start"
  reftime=nowtime
;

; ek: Loop over the original number of integrations

for i=0L,long(n_elements(in_ori))-1L do begin

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
  npp = min([15,n_elements(sch.packdata)-1])
  ;    print,i,first_byte,sch.inhid,sch.form,sch.nbyt,sch.nbyt_pack, $
  ;    sch.packdata(0:npp), $
  ;    format='("rec:",i3," byte#:",i7," inh#:",i8,'+ $
  ;    '" format:",a4," nbyt:",i8," nbyt_pack:",i8/"   pack[0=> 7]:",8i8,'+ $
  ;    '/"   pack[8=>15]:",8i8)' 
  ; ek: Rows is a 1D array of indices where the integration numbers of the
  ;     spectra and the data headers match.

  rows = where (sp.inhid eq sch.inhid,count)
  rows_ori = where(sp_ori.inhid eq sch.inhid,count_ori)

  ; ek: If any of the data and spectral headers refer to the same integration 
  ;     then proceed.
  if count gt 0L then begin  

   accept_list = make_array(count_ori,/int)

    index=0

    for iloop=0, count_ori -1 do begin
      if (index lt count) then begin
        if sp_ori(rows_ori(iloop)).sphid eq sp(rows(index)).sphid then begin
          accept_list(iloop)=1
          index = index + 1
        endif
      endif
    endfor

    ; ek: dataoffs is a list of offsets which indicate the starting byte of 
    ;     each spectrum. The unit of dataoffs is 2 bytes (short).
    ;     Each offset in dataoffs is equal to a dataOFF/2 read from the spectrum
    ;     header struture sp where  dataOFF is the starting byte
    ;     (in units of 1 byte) of the spectrum. This number has to be
    ;     calculated by the program writing the disk file and written
    ;     into the sp header. This offset allows the idl program to
    ;     identify the appropriate header and spectrum data.

    dataoffs_ori = long(sp_ori(rows_ori).dataOFF)/2L
    nchs_ori     = long(sp_ori(rows_ori).nch)
    nrecs_ori    = long(sp_ori(rows_ori).nrec)

    dataoffs = long(sp(rows).dataOFF)/2L
    nchs     = long(sp(rows).nch)
    nrecs    = long(sp(rows).nrec)

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

    index = -1
    for j = 0L,count_ori-1L do begin

      if (accept_list(j) eq 0) then begin

;        pr_curs=pr_cur + lindgen(nrecs_ori(j))
;        pr_cur=pr_cur+nrecs_ori(j)
;        pc_cur=pc_cur+nrecs_ori(j)*nchs_ori(j) 

;      print,'skip '

      endif else begin

        index = index + 1
;        print, index, rows(index), pr_cur, pc_cur
        pr(rows(index))=pr_cur
        pc(rows(index))=pc_cur
        ks=lindgen(nrecs_ori(j))
        offsets=dataoffs_ori(j)+(5L+nchs_ori(j)*2L)*(ks)
        offsets1=offsets+1L
        offsets2=offsets+2L
        offsets3=offsets+3L
        offsets4=offsets+4L
        pr_curs=pr_cur + lindgen(nrecs_ori(j))

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

        rwts(pr_curs)= wts(index)*integs(pr_curs)
        ls=lindgen(nrecs_ori(j)*nchs_ori(j))
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

        if (nchs_ori(j) ne nch_prev or nrecs_ori(j) ne nrec_prev) then begin
          offsetsr= transpose( 2*lindgen(nchs_ori(j),nrecs_ori(j)) +5L $
                  +5L*( lindgen(nchs_ori(j),nrecs_ori(j))/nchs_ori(j)))
          offsetsi=offsetsr+1L
          scale_ptrs= (lindgen(nrecs_ori(j)) # make_array(nchs_ori(j),/long,value=1L)) 
          nrec_prev=nrecs_ori(j) & nch_prev=nchs_ori(j)
        endif


        ; ek: The data are scaled here

        ch(doffsets)=scales(pr_cur+scale_ptrs)* $
          complex(sch.packdata(dataoffs_ori(j)+offsetsr), $
                  sch.packdata(dataoffs_ori(j)+offsetsi))    

        if (sch.inhid eq -1) then begin
          for ii=0,n_elements(doffsets_ori)-1 do begin
            print,'ii, doffsets[ii],pr_cur,scale_ptrs[ii],dataoffs_ori[j],offsetsr[ii],offsetsi[ii]',$
                   ii, doffsets[ii],pr_cur,scale_ptrs[ii],dataoffs_ori[j],offsetsr[ii],offsetsi[ii]
            print,'scales,realS,imagS,realF,imagF ',scales(pr_cur+scale_ptrs[ii]), $
                   sch.packdata(dataoffs_ori(j)+offsetsr[ii]),sch.packdata(dataoffs(j)+offsetsi[ii]), $
                   float(ch(doffsets[ii])),imaginary(ch(doffsets[ii]))
          endfor
        endif

        pr_cur=pr_cur+nrecs_ori(j)
        pc_cur=pc_cur+nrecs_ori(j)*nchs_ori(j) 

      endelse

    ; ek: This ends the loop for j=0L,count-1L ...
    ;     The endif refers back to if count gt 0L
    endfor
  
    if (i eq 0) then begin
      nowtime = systime(1)
      print, "Spectra    : Reading  Started, Time Estimate             ", fix((nowtime-reftime)*nints_expected), "sec"
    endif

    if keyword_set(verbose) then print, "-- accepting integration ", i, " --"
 
 endif else begin

    if (i eq 0) then begin
      nowtime = systime(1)
      print, "Spectra    : Reading  Started, Time Estimate             ", fix((nowtime-reftime)*nints_expected), "sec"
    endif

    if keyword_set(verbose) then print, "-- skipping  integration ", i, " --"
  endelse

  pts_per_int=num_pts
  first_byte=long64(first_byte+16L+sch.nbyt)

; ek: This is the end of the main loop over the integrations, for i=0L,nints ...
endfor

;
  nowtime = systime(1)
    print, "Spectra    : Reading Finished, Time elapsed ", fix(nowtime-reftime), "sec"
  reftime=nowtime
;

;print,sch.packdata
print," ----------- Finished Reading All Data ----------- "

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

  nowtime = systime(1)
    print, "TOTAL TIME ELAPSED ", fix(nowtime-reftime0), "sec"

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
