function dbi_head2_read2,int_read=int_read,sideband=sideband, rx=rx, band_read=band_read,iblfix=iblfix, endianFlag=endianFlag, newwindows=newwindows, if1=if1, if2=if2, if3=if3, if4=if4, asic=asic, swarm=swarm, swmavg=swmavg, nbins=nbins, defaults=defaults
common global
common data_set


print,'******** OS TYPE ***********'
print,'Platform =',!VERSION.ARCH
;
;  set up structures for header table
;
inh_temp={traid:0L,inhid:0L,$
         int:0L,az:0e,el:0e,ha:0e,$
         iut:0,$
         iref_time:0,$
         dhrs:0d,vc:0e,sx:0d,sy:0d,sz:0d,rinteg:0e,proid:0L,$
         souid:0L,isource:0,ivrad:0,offx:0e,offy:0e,$
         ira:0,$
         idec:0,rar:0d,decr:0d,epoch:0e,size:0e,$
         inhint1:0L,inhint2:0L,inhint3:0L,$
         inhint4:0L,inhint5:0L,inhint6:0L,sflux:0d,$
         inhdbl2:0d,inhdbl3:0d,inhdbl4:0d,inhdbl5:0d,inhdbl6:0d}
blh_temp={blhid:0L,inhid:0L,isb:0,ipol:0,$
         ant1rx:0,ant2rx:0,pointing:0,irec:0,$
         u:0e,v:0e,w:0e,prbl:0e,coh:0e,$
         avedhrs:0d,ampave:0e,phaave:0e,$
         blsid:0L,itel1:0,itel2:0,ant1tsysoff:0L,ant2tsysoff:0L,$
         iblcd:0,ble:0e,$
         bln:0e,blu:0e,blhint1:0L,blhint2:0L,blhint3:0L,$
         blhint4:0L,blhint5:0L,blhint6:0L,blhdbl1:0d,$
         blhdbl2:0d,blhdbl3:0d,blhdbl4:0d,blhdbl5:0d,blhdbl6:0d}
sph_temp={sphid:0L,blhid:0L,inhid:0L,igq:0,ipq:0,iband:0,$
         ipstate:0,tau0:0e,vel:0d,vres:0e,fsky:0d,$
         fres:0e,gunnlo:0d,cabinlo:0d,corrlo1:0d,corrlo2:0d,$
         integ:0e,wt:0e,flags:0L,vradcat:0e,$
         nch:0,nrec:0,dataOFF:0L,$
         rfreq:0d,corrblock:0,corrchunk:0,$
         sphint1:0L,sphint2:0L,sphint3:0L,$
         sphint4:0L,sphint5:0L,sphint6:0L,tssb:0d,$
         sphdbl2:0d,sphdbl3:0d,sphdbl4:0d,sphdbl5:0d,sphdbl6:0d}
code_temp={v_name:'123456789012',icode:0, $
           code:'12345678901234567890123456',ncode:0}
;
endianFlag=0
openr,unit,e.idl_bcp+'in_read',/get_lun,error=err
if err ne 0 then begin
  print,!err_string
  return,-1
endif
;
; see how many rows, set up tables w/ that number of rows and read data
;
status = fstat(unit) & inrows = status.size/188
if e.debug then print,inrows,' inh rows'

in = replicate(inh_temp,inrows)
point_lun,unit,0L
readu,unit,in
if in[0].epoch ne 2000 then endianFlag=1

;if (strpos(!VERSION.ARCH,'86') ge 0) or (strpos(!VERSION.ARCH,'alpha') ge 0) then $
if endianFlag eq 1 then in = swap_endian(in)
;
close,unit & free_lun,unit
openr,unit,e.idl_bcp+'bl_read',/get_lun,error=err
if err ne 0 then begin
  print,!err_string
  return,-1
endif
;
; see how many rows, set up tables w/ that number of rows and read data
;
status = fstat(unit) & nrows = status.size/158
if e.debug then print,nrows,' blh rows'

bl = replicate(blh_temp,nrows)
point_lun,unit,0L
readu,unit,bl
;if (strpos(!VERSION.ARCH,'86') ge 0) or (strpos(!VERSION.ARCH,'alpha') ge 0) then $
if endianFlag eq 1 then bl = swap_endian(bl)
;
close,unit & free_lun,unit
openr,unit,e.idl_bcp+'sp_read',/get_lun,error=err
if err ne 0 then begin
  print,!err_string
  return,-1
endif
;
; see how many rows, set up tables w/ that number of rows and read data
;
status = fstat(unit) & nrows = status.size/188
if e.debug then print,nrows,' sph rows'

sp = replicate(sph_temp,nrows)
point_lun,unit,0L
readu,unit,sp
;if (strpos(!VERSION.ARCH,'86') ge 0) or (strpos(!VERSION.ARCH,'alpha') ge 0) then $
if endianFlag eq 1 then sp = swap_endian(sp)
;
close,unit & free_lun,unit
iband_distinct=uti_distinct(sp.iband,nbands,/many_repeat)
iband_order=sp[0:nbands-1].iband
; setup for nbins
if (keyword_set(swmavg) or keyword_set(newwindows)) then begin
   nbins=intarr(nbands)+1
   if keyword_set(swmavg) then begin
      result=uti_distinct(sp.sphint1,/many,ntmp)
      if ntmp eq 1 and result[0] eq 1 then jtmp=where(iband_order gt 0, jcount) else jtmp=where(iband_order gt 48, jcount)
      temp=size(swmavg)
      if temp[0] eq 0 then begin
         for j=0, jcount-1 do begin
            itmp=where(iband_order eq iband_order[jtmp[j]])
            nbins[itmp]=swmavg
         endfor
      endif else begin
         if temp[1] ne jcount then stop
         for j=0, jcount-1 do begin
            itmp=where(iband_order eq iband_order[jtmp[j]])
            nbins[itmp]=swmavg[j]
         endfor
      endelse
   endif
;   print,nbins
   if keyword_set(newwindows) then begin
      temp=size(newwindows)
      if temp[0] eq 1 then nwindows=1 else nwindows=temp[2]
      for jtmp=0, nwindows-1 do nbins=[nbins,newwindows[jtmp*4+3]]
   endif
endif else nbins=0

;print,'NBINS:'
;help,nbins
;print,nbins

;bw=(nbands-1)*82.

if keyword_set(newwindows) then begin

   temp=size(newwindows)
   if temp[0] eq 1 then nwindows=1 else nwindows=temp[2]
;   windows='s'+strcompress(string(indgen(nwindows)+nbands),/remove)
   windows='s'+strcompress(string(indgen(nwindows)+51),/remove)
   ns=nrows/nbands
   sp=reform(sp,nbands,ns)
   for i=0, nwindows-1 do begin
;  w_select is the selected chunk to extract data
;  w_chstart is the start channel starting with number 1
;  w_chend  is the end channel
;  w_nch is the number of channels in the new window
;  newsp.nch=w_chend-w_chstart+1
;  fsky channel value= (w_chend+w_chstart)/2.
;  newsp.fsky=newsp.fsky+(w_chend+wchstart-newsp.nch-1)/2.*newsp.fres
      itemp=where(iband_order eq newwindows[0,i], count)
      if (count eq 0) then begin
         print,'***************************'
         print,'Wrong windows setup, Quit !'
         print,'***************************'
         stop
         return,0
      endif
;      w_select=newwindows[0,i]
      w_select=itemp
      print,'W_SELECT:'
      print,w_select
      w_chstart=newwindows[1,i]
      w_nch=newwindows[2,i]-newwindows[1,i]+1
      newsp=sp[w_select,*]
;      newsp.iband=nbands+i
      newsp.iband=max(iband_distinct)+1+i
      newsp.fsky=newsp.fsky+0.001*(w_nch+2*w_chstart-newsp.nch-2)/2.*newsp.fres
      newsp.vel=newsp.vel+0.001*(w_nch+2*w_chstart-newsp.nch-2)/2.*newsp.vres
      newsp.nch=w_nch
;      newsp.dataoff=newsp.dataoff+(w_chstart-1L)*2L
      newsp.sphint1=w_chstart-1 ; mir data channel number start with 0
      sp=[sp,newsp]
   endfor
   sp=reform(sp,(nbands+nwindows)*ns)
   sp.sphid=lindgen((nbands+nwindows)*ns)+1L

endif


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
;pos = strarr(1)
vrad=strarr(1)
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

;  if (strpos(!VERSION.ARCH,'86') ge 0) or
;  (strpos(!VERSION.ARCH,'alpha') ge 0) then begin
  if endianFlag eq 1 then begin
     code_temp.icode = swap_endian(code_temp.icode)
     code_temp.ncode = swap_endian(code_temp.ncode)
  endif
; print,code_temp
; stop,code_temp
  result = execute('difd='+string(code_temp.icode)+ $
    '-n_elements('+strtrim(code_temp.v_name,2L)+')')
  if (difd ge 0 and code_temp.v_name ne 'pos') then begin
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
       'pos'  :
       'vrad'  : vrad(code_temp.icode)=strtrim(code_temp.code,2L) 
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

    icode_s=['in','in','bl', $
             'bl','bl','bl','bl','bl', $
             'sp','sp','sp','sp', $
             'in','in','in','in']
    icode_tag=['iut','iref_time','isb', $
               'ipol', 'irec','itel1','itel2','iblcd', $
               'igq','ipq','iband','ipstate', $
               'isource','ivrad','ira','idec']

; icode_s and icode_tag must come last in the structure

c = { $
     ut:ut,ref_time:ref_time,sb:sb,$
     pol:pol,rec:rec,tel1:tel1,tel2:tel2,blcd:blcd,$
     gq:gq,pq:pq,band:band,pstate:pstate,$
     source:source,vrad:vrad,ra:ra,dec:dec,$
     icode_s:icode_s,icode_tag:icode_tag}

if keyword_set(newwindows) then begin
   
   n_windows=n_elements(windows)
   ncodes=intarr(n_tags(c))
   for i = 0,n_tags(c)-1 do begin
      ncodes[i] = n_elements(c.(i))
   endfor
   ncodes[10]=ncodes[10]+n_windows
   str_temp=strarr(total(ncodes))
   nctags=ncodes
   k=0
   for i = 0,n_tags(c)-1 do begin
      for j = 0,n_elements(c.(i))-1 do begin
         str_temp[k] = c.(i)[j]
         if (i eq 10) and (j eq n_elements(c.(i))-1) then begin
            for kk =0, n_windows-1 do begin
               k=k+1
               str_temp[k]=windows[kk]
            endfor
         endif
         k = k+1
      endfor
   endfor

   c = { $
     ut:strarr(nctags[0]), $
     ref_time:strarr(nctags[1]), $
     sb:strarr(nctags[2]),pol:strarr(nctags[3]),$
     rec:strarr(nctags[4]),$
     tel1:strarr(nctags[5]),$
     tel2:strarr(nctags[6]),blcd:strarr(nctags[7]),$
     gq:strarr(nctags[8]),pq:strarr(nctags[9]),$
     band:strarr(nctags[10]),pstate:strarr(nctags[11]),$
     source:strarr(nctags[12]),vrad:strarr(nctags[13]),$
     ra:strarr(nctags[14]),dec:strarr(nctags[15]),$
     icode_s:strarr(nctags[16]),icode_tag:strarr(nctags[17])}

   k = 0
   m = intarr(n_tags(c))
   for i = 0,n_tags(c)-1 do begin
      for j = 0,ncodes[i]-1 do begin
         aa = str_temp[k]
         c.(i)[ j + m[i] ] = aa
         k = k+1
      endfor
      m[i] = m[i] + ncodes[i]   
   endfor
endif

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

if keyword_set(if1) then begin
   i_band=(sp.iband le 24) and (sp.sphint1 eq 0)
   sp=sp(where(i_band))
endif

if keyword_set(if2) then begin
   i_band=((sp.iband eq 0) or ((sp.iband gt 24) and (sp.iband le 48))) and (sp.sphint1 eq 0)
   sp=sp(where(i_band))
endif

if keyword_set(if3) then begin
   i_band=((sp.iband eq 0) or (sp.iband eq 49)) and (sp.sphint1 eq 0)
   sp=sp(where(i_band))
endif

if keyword_set(if4) then begin
   i_band=((sp.iband eq 0) or (sp.iband eq 50)) and (sp.sphint1 eq 0)
   sp=sp(where(i_band))
endif

if keyword_set(asic) then begin
   i_band=(sp.iband le 48) and (sp.sphint1 eq 0) 
   sp=sp(where(i_band))
endif

if keyword_set(swarm) then begin
   i_band=(sp.iband eq 0) or (sp.iband ge 49) or (sp.sphint1 eq 1)
   sp=sp(where(i_band))
endif

;
;  sort the output structures, just in case
;
; turn off sorting for adding additional sp at the end
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; remove the last integration which might be bad
if not keyword_set(int_read) then begin
   pts_end=min(where(sp.inhid eq in[inrows-1].inhid))
   sp=sp[0:pts_end-1L]
   pts_end=min(where(bl.inhid eq in[inrows-1].inhid))
   bl=bl[0:pts_end-1L]
   in=in[0:inrows-2]
endif else begin
   in_skip=int_read[0]-(in[0].int)>0
   in=in[where(in.int le int_read[1] and in.int ge int_read[0])]
;   pts_end=max(where(sp.inhid eq in[int_read[1]-int_read[0]].inhid))
   pts_end=max(where(sp.inhid eq in[n_elements(in)-1L].inhid))
   pts_start=min(where(sp.inhid eq in[0].inhid))
   sp=sp[pts_start:pts_end]
   pts_start=min(where(bl.inhid eq in[0].inhid))
;   pts_end=max(where(bl.inhid eq in[int_read[1]-int_read[0]].inhid))
   pts_end=max(where(bl.inhid eq in[n_elements(in)-1L].inhid))
   bl=bl[pts_start:pts_end]
   int_read[0]=in_skip
endelse

inrows = where(sp.inhid eq in[0].inhid,incount)
blrows = where(sp.blhid eq bl[0].blhid,blcount)

nints = n_elements(in)
nbls  = n_elements(bl)
nsp   = n_elements(sp) 

if (nsp ne nints*incount) or (nsp ne nbls*blcount) then begin
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
endif else begin
   pi_sample=lindgen(1,nints)
   pi=reform(rebin(pi_sample,incount,nints,/sample),nsp)
   
   pb_sample=lindgen(1,nbls)
   pb=reform(rebin(pb_sample,blcount,nbls,/sample),nsp)
   
   ps=lindgen(nsp)
endelse

if keyword_set(sideband) or keyword_set(rx) then begin
   if keyword_set(rx) then begin
      irx=where(c.rec eq strcompress(string(rx),/remove),count)
      if count eq 0 then begin
         print, rx, ' receiver not available. Quit!'
         return,-1
      endif
      if keyword_set(sideband) then begin
         isideband=where(c.sb eq sideband,count)
         if count eq 0 then begin
            print, rx, ' sideband not available. Quit!'
            return,-1
         endif
         sp=sp[where((bl[pb].isb eq isideband[0]) and (bl[pb].irec eq irx[0]))]
         bl=bl[where((bl.isb eq isideband[0]) and (bl.irec eq irx[0]))]
      endif else begin
         sp=sp[where(bl[pb].irec eq irx[0])]
         bl=bl[where(bl.irec eq irx[0])]
      endelse
   endif else begin
      isideband=where(c.sb eq sideband,count)
      if count eq 0 then begin
         print, rx, ' sideband not available. Quit!'
         return,-1
      endif
      sp=sp[where(bl[pb].isb eq isideband[0])]
      bl=bl[where(bl.isb eq isideband[0])]
   endelse

   inrows = where(sp.inhid eq in[0].inhid,incount)
   blrows = where(sp.blhid eq bl[0].blhid,blcount)

   nints = n_elements(in)
   nbls  = n_elements(bl)
   nsp   = n_elements(sp) 

   if (nsp ne nints*incount) or (nsp ne nbls*blcount) then begin
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
   endif else begin
      pi_sample=lindgen(1,nints)
      pi=reform(rebin(pi_sample,incount,nints,/sample),nsp)
   
      pb_sample=lindgen(1,nbls)
      pb=reform(rebin(pb_sample,blcount,nbls,/sample),nsp)
   
      ps=lindgen(nsp)
   endelse
endif
   
pif=pi
pbf=pb
psf=ps
pis=pi
pbs=pb
pss=ps
pil=pi
pbl=pb
psl=ps

icontinue=0

continue:
if icontinue eq 0 then begin
   antennasFile=e.idl_bcp+'antennas'
endif else begin
   print, 'Antenna file corrupted or not found!'
   read,antennasFile,prompt='Enter another antenna file (with full directory if necessary):'
endelse

file=file_search(antennasFile)
if file eq '' then begin
   print, 'Antenna file not found !'
   if keyword_set(defaults) then goto, finish
   aa=''
   read,aa,prompt='Load data without antenna file? [YES <NO>] :'
   if (aa eq 'NO' or aa eq 'no' or aa eq 'No' or aa eq 'N' or aa eq 'n') then begin
      icontinue=1
      goto, continue      
   endif else begin
      goto, finish      
   endelse
endif
   
openr,unit,antennasFile,/get_lun
data=dblarr(4,10)
line=dblarr(4)
index=0
;print, 'Antenna positions used:'
while not EOF(unit) do begin
    readf,unit,line
;    if line[0] ne 6 then temp=where(line eq 0, count)
;    if count gt 0 then begin
;       icontinue=1
;       free_lun,unit
;       goto, continue
;    endif
    data[*,index]=line
;    print,line
    index=index+1
endwhile
free_lun,unit
;distinct_blcd=uti_distinct(c.blcd[bl[pbl].iblcd],nblcds,/many_repeat)
distinct_blcd=uti_distinct(bl.iblcd,nblcds,/many_repeat)
iblfix=0
for i=0L,nblcds-1L do begin
   ant=fix(strtrim(strsplit(c.blcd[distinct_blcd[i]],'-',/extract),2))
   xyz=data[*,ant[0]-1]-data[*,ant[1]-1]
;   print,xyz
   xyz=xyz[1:3]

   temp=where(xyz eq 0, count)
   if count gt 0 then begin
      print,'Two antennas on the same pad !'
      icontinue=1
      free_lun,unit
      goto, continue
   endif   

   lat=19.82420526391d/57.29577951
   m1=[[-sin(lat),0,cos(lat)],[0,1,0],[cos(lat),0,sin(lat)]]
   neu=xyz##m1
;   print,neu
   j=where(bl.iblcd eq distinct_blcd[i])
;   result=dat_list(s_l,'"blcd" eq "'
;   +distinct_blcd[i]+'"',/reset,/no_notify)
   k=where( ((bl[j].bln-bl[j[0]].bln) ne 0) or ((bl[j].ble-bl[j[0]].ble) ne 0) or ((bl[j].blu-bl[j[0]].blu) ne 0), count)
   if count gt 0 then begin
      iblfix=1
      bl[j].bln=neu[0]
      bl[j].ble=neu[1]
      bl[j].blu=neu[2]
      print,'BL header for '+c.blcd[distinct_blcd[i]]+' has been fixed!'
   endif else begin
      if (abs(bl[j[0]].bln - neu[0]) gt 0.01) or (abs(bl[j[0]].ble - neu[1]) gt 0.01) or (abs(bl[j[0]].blu - neu[2]) gt 0.01) then begin
         iblfix=1
         bl[j].bln=neu[0]
         bl[j].ble=neu[1]
         bl[j].blu=neu[2]
         print,'BL header for '+c.blcd[distinct_blcd[i]]+' has been fixed!'
      endif 
   endelse
endfor

finish:
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
