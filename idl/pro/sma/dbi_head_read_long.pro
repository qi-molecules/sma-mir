function dbi_head_read_long, int_read=int_read, band_read=band_read, dual=dual
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
nx= (keyword_set(dual)) ? 2L : 1L
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
;
openr,unit,e.idl_bcp+'codes_read',/get_lun,error=err
if err ne 0 then begin
  print,!err_string
  return,-1
endif
;
;
; see how many rows, set up tables w/ that number of rows and read data
;
status = fstat(unit) & nrows = status.size/42
if e.debug then print,nrows,' code rows'
;print,nrows,' code rows'
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
; print,code_temp

  if (strpos(!VERSION.ARCH,'x86') ge 0) or (strpos(!VERSION.ARCH,'alpha') ge 0) then begin
     code_temp.icode = swap_endian(code_temp.icode)
     code_temp.ncode = swap_endian(code_temp.ncode)
  endif
; print,code_temp
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

if (keyword_set(int_read)) then begin
   if (n_elements(int_read) mod 2) eq 1 then begin
      print, 'Please provide correct integration range !'
      print, 'Quit !'
      return,-1
   endif
   n_skip=n_elements(int_read)/2
   int_skip=intarr(n_skip)
   int_skip[0]=int_read[0]
   if n_skip gt 1 then begin
      for ii=1L,n_skip -1L do int_skip[ii]=int_read[ii*2]-int_read[ii*2-1]-1
   endif
   in_skip=total(int_skip)
;   in_skip=int_read[0]
;   in_rows=int_read[1]+1
   in_rows=int_read[n_elements(int_read)-1]+1
   blh_skip=int_skip*2L*n_elements(c.blcd)*nx
   sph_skip=blh_skip*n_elements(c.band)
   bl_skip=total(blh_skip)
   sp_skip=total(sph_skip)
   blh_read=int_read*2L*n_elements(c.blcd)*nx
   sph_read=blh_read*n_elements(c.band)
endif else begin
   in_rows=n_elements(in)
   n_skip=0
   in_skip=0
   bl_skip=0
   sp_skip=0
endelse

bl_rows=in_rows*2L*n_elements(c.blcd)*nx
sp_rows=bl_rows*n_elements(c.band)

ii=0L
iskip=0
openr,unit,e.idl_bcp+'in_read',/get_lun,error=err
if err ne 0 then begin
  print,!err_string
  return,-1
endif
;
; see how many rows, set up tables w/ that number of rows and read data
;
status = fstat(unit) & nrows = (keyword_set(int_read)) $
                   ? in_rows : (status.size/132) 
if e.debug then print,nrows,' inh rows'
in = replicate(inh_temp,nrows-in_skip)
point_lun,unit,0L
for i=0L,(nrows-1L) do  begin
  readu,unit,inh_temp
  if n_skip le 1 then begin
     if (i ge in_skip) then begin
        if (strpos(!VERSION.ARCH,'x86') ge 0) or (strpos(!VERSION.ARCH,'alpha') ge 0) then $
          inh_temp = swap_endian(inh_temp)
;  stop,inh_temp
        in(i-in_skip)=inh_temp
     endif
  endif else begin
     if i ge int_read[iskip*2] then begin
        if (i le int_read[iskip*2+1]) then begin
           if (strpos(!VERSION.ARCH,'x86') ge 0) or (strpos(!VERSION.ARCH,'alpha') ge 0) then $
             inh_temp = swap_endian(inh_temp)
;  stop,inh_temp
           in(ii)=inh_temp
           ii=ii+1L
        endif else begin
           iskip=iskip+1
        endelse
     endif
  endelse
endfor
;
close,unit & free_lun,unit

ii=0L
iskip=0
openr,unit,e.idl_bcp+'bl_read',/get_lun,error=err
if err ne 0 then begin
  print,!err_string
  return,-1
endif
;
; see how many rows, set up tables w/ that number of rows and read data
;
status = fstat(unit) & nrows = (keyword_set(int_read)) ? $
                     bl_rows : (status.size/118)
if e.debug then print,nrows,' blh rows'
bl = replicate(blh_temp,nrows-bl_skip)
point_lun,unit,0L
for i=0L,(nrows-1L) do  begin
  readu,unit,blh_temp
  if n_skip le 1 then begin
     if (i ge bl_skip) then begin
        if (strpos(!VERSION.ARCH,'x86') ge 0) or (strpos(!VERSION.ARCH,'alpha') ge 0) then $
          blh_temp = swap_endian(blh_temp)
;  stop,blh_temp
        bl(i-bl_skip)=blh_temp
     endif
  endif else begin
     if i ge blh_read[iskip*2] then begin
        if (i le blh_read[iskip*2+1]+2L*n_elements(c.blcd)*nx-1) then begin
           if (strpos(!VERSION.ARCH,'x86') ge 0) or (strpos(!VERSION.ARCH,'alpha') ge 0) then $
             blh_temp = swap_endian(blh_temp)
;  stop,blh_temp
           bl(ii)=blh_temp
           ii=ii+1L
        endif else begin
           iskip=iskip+1
        endelse
     endif
  endelse
endfor
close,unit & free_lun,unit

if n_elements(uti_distinct(bl.irec)) ne nx then begin
   if nx eq 2 then print, "*** Single receiver track, don't use /dual ***"
   if nx eq 1 then print, '*** Dual receiver track, please use /dual ***'
   return, -1
endif

ii=0L
iskip=0
openr,unit,e.idl_bcp+'sp_read',/get_lun,error=err
if err ne 0 then begin
  print,!err_string
  return,-1
endif
;
; see how many rows, set up tables w/ that number of rows and read data
;
status = fstat(unit) & nrows = (keyword_set(int_read)) ? $
                       sp_rows : (status.size/100)
if e.debug then print,nrows,' sph rows'
sp = replicate(sph_temp,nrows-sp_skip)
point_lun,unit,0L
for i=0L,(nrows-1L) do  begin
  readu,unit,sph_temp
  if n_skip le 1 then begin
     if (i ge sp_skip) then begin
        if (strpos(!VERSION.ARCH,'x86') ge 0) or (strpos(!VERSION.ARCH,'alpha') ge 0) then $
          sph_temp = swap_endian(sph_temp)
;  stop,sph_temp
        sp(i-sp_skip)=sph_temp
     endif
  endif else begin
     if i ge sph_read[iskip*2] then begin
        if (i le sph_read[iskip*2+1]+2L*n_elements(c.blcd)*n_elements(c.band)*nx-1) then begin
           if (strpos(!VERSION.ARCH,'x86') ge 0) or (strpos(!VERSION.ARCH,'alpha') ge 0) then $
             sph_temp = swap_endian(sph_temp)
;  stop,sph_temp
           sp(ii)=sph_temp
           ii=ii+1L
        endif else begin
           iskip=iskip+1
        endelse
     endif
  endelse
endfor
;  if e.debug then print, i,sp(i).dataOFF
close,unit & free_lun,unit

if keyword_set(band_read) then begin
   ii=where(band_read[0] eq c.band,count)
   if count le 0 then begin
      print,'***Wrong band name ! Quit !***'
      return,-1
   endif
   i_band=sp.iband eq ii[0]
   if n_elements(band_read) gt 1 then begin
      for i=1,n_elements(band_read)-1 do begin
         ii=where(band_read[i] eq c.band,count)
         if count le 0 then begin
            print,'***Wrong band name! Quit !***'
            return,-1
         endif
         i_band=i_band or (sp.iband eq ii[0])
      endfor
   endif
   sp=sp(where(i_band))
endif

if e.debug then print,'inh# ',in(0).inhid,' => ',in(n_elements(in)-1).inhid
if e.debug then print,'blh# ',bl(0).blhid,' => ',bl(n_elements(bl)-1).blhid
if e.debug then print,'sph# ',sp(0).sphid,' => ',sp(n_elements(sp)-1).sphid
;
; removal of wt le 0 data is take care of in db_read_utable 
; and db_read_track sql
;
;
;  sort the output structures, just in case
;
j=sort(in.inhid)
in=in(j)
j=sort(bl.blhid)
bl=bl(j)
j=sort(sp.sphid)
sp=sp(j)
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
;    if in_index lt 0 then return,-1
    in_index = in_index + $
           max([where(in(in_index:in_upper).inhid eq sp(i).inhid,count)])
    if (count le 0) then in_index = max([where(in.inhid eq sp(i).inhid)])
;print,'here:',in_index
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

print,'finished reading headers'

return,0
end
