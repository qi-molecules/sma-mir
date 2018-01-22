function dbi_cobra_read,cobra_file=cobra_file
; 
; Read cobra data from unix file and appropriate header variables
; from database and assembly the sp and c structures in preparation
; for writing to the database archive tables SPH_COBRA and SCH_COBRA
; result=-1 (error), 1(ok)
;avg
;
; parameters : none
;
;
; eg. :   result=dbi_cobra_read() 
; eg. :   result=dbi_cobra_read(cobra_file='rick') 
; eg. :   result=dbi_cobra_read(cobra_file='nick_m82plus')  
; eg. :   result=dbi_cobra_read(cobra_file='nick_m82plus_1') 
; eg. :   result=dbi_cobra_read(cobra_file='nick_m82plus_2') 
; eg. :   result=dbi_cobra_read(cobra_file='nick_m82plus_3') 
; eg. :   result=dbi_cobra_read(cobra_file='nick_m82plus_4') 
; eg. :   result=dbi_cobra_read(cobra_file='nick_3C84') 
;
;

common global
common data_set

;
e.debug=0
if not keyword_set(cobra_file) then cobra_file='rick'
result=fil_read(cobra_file,lines,nlines)
nlines=nlines+1
;
; see how many rows, set up tables w/ that number of rows and read data
;
nsb=2
baselines=make_array(nlines,/string)
bands=make_array(nlines,/long)
inhs=make_array(nlines,/long)
sidebands=make_array(nlines,/string)
numchanss=make_array(nlines,/long)
f0s=make_array(nlines,/double)
deltafs=make_array(nlines,/double)
integTimes=make_array(nlines,/double)
mjds=make_array(nlines,/double)
tssbs=make_array(nlines,/double)
rviss=make_array(nlines*50,/float)
iviss=make_array(nlines*50,/float)
dataOFF=make_array(nlines,/long)
;
; Define log2
;
   log2=alog10(2.d0)

print,nlines,' records'
ic1=0
num_sph_writes=0
num_sch_writes=0
inhid_prev=0L
inhid_pres=0L
inhid_uniq=0L
for i=0,nlines-1 do begin
  if e.debug then print,i,lines[i]
  vars=str_sep(lines[i],' ')
  baselines[i]=vars[0]
  bands[i]=long(vars[1])
  inhid_pres=long(vars[2])
  inhs[i]=inhid_pres
  sidebands[i]=vars[3]
  numchanss[i]=long(vars[4])
  f0s[i]=double(vars[5])
  if (f0s[i] eq 0.) then f0s[i]=100.
  deltafs[i]=double(vars[6])
  if (deltafs[i] eq 0.) then deltafs[i]=1.
  integTimes[i]=double(vars[7])
  mjds[i]=double(vars[8])
  tssbs[i]=double(vars[9])
  if (tssbs[i] le 0.) then tssbs[i]=1000.
  print,'record :',i,' ',inhs[i],' ',sidebands[i],' ',numchanss[i]
  ichs=2*lindgen(numchanss[i])
  dataOFF[i]=ic1
  rviss[ic1:ic1+numchanss[i]-1]=float(vars[10+ichs])
  iviss[ic1:ic1+numchanss[i]-1]=float(vars[11+ichs])
  ic1=ic1+numchanss[i]
  if (inhid_pres ne inhid_prev) then begin
    inhid_uniq=[inhid_uniq,inhid_pres]
  endif
  inhid_prev=inhid_pres
endfor

inhid_uniq=inhid_uniq[1:*]
nint=n_elements(inhid_uniq)
sph_dataoff=make_array(nsb,nint,/long)
sph_inhid=make_array(nsb,nint,/long)
sph_blhid=make_array(nsb,nint,/long)
sph_ut=make_array(nsb,nint,/string)
sph_gq=make_array(nsb,nint,/string)
sph_pq=make_array(nsb,nint,/string)
sph_band=make_array(nsb,nint,/string)
sph_dcvrt=make_array(nsb,nint,/string)
sph_sb=make_array(nsb,nint,/string)
sph_pstate=make_array(nsb,nint,/string)
sph_tau0=make_array(nsb,nint,/float)
sph_dtau0=make_array(nsb,nint,/float)
sph_linid=make_array(nsb,nint,/long)
sph_vel=make_array(nsb,nint,/double)
sph_vres=make_array(nsb,nint,/double)
sph_vtype=make_array(nsb,nint,/string)
sph_fsky=make_array(nsb,nint,/double)
sph_fres=make_array(nsb,nint,/double)
sph_tssb=make_array(nsb,nint,/float)
sph_integ=make_array(nsb,nint,/float)
sph_wt=make_array(nsb,nint,/float)
sph_taper=make_array(nsb,nint,/string)
sph_svar=make_array(nsb,nint,/float)
sph_snoise=make_array(nsb,nint,/float)
sph_nch=make_array(nsb,nint,/long)
sph_nrec=make_array(nsb,nint,/long)
sph_dataOFF=make_array(nsb,nint,/long)
sph_sphid=make_array(nsb,nint,/long)
sph_rfreq=make_array(nsb,nint,/double)
sch_rviss=make_array(nsb,nint,50,/float,value=0.)
sch_iviss=make_array(nsb,nint,50,/float,value=0.)

sbs=['l','u']

for i=0,nint-1 do begin
  inhid_pres=inhid_uniq[i]
  num_pts=long(32*nsb*2L+nsb*5L)         
  packdata=make_array(num_pts,/int)
  offset=0L
  packstring='0x'

for isb=0,nsb-1 do begin
  sb=sbs[isb]
  j=where(inhs eq inhid_pres and sidebands eq sb,count_sb)
  if (count_sb eq 0) then goto, endsb
  sql_str='set rowcount 1 select sp.inh#,sp.blh#,ut,gq,pq,band,' + $
    'dcvrt,sp.sb,pstate,tau0,dtau0,sp.lin#,vel,vres,vtype,'+ $
    'fsky,fres,tssb,integ,wt,taper,svar,snoise,nch,nrec,dataOFF,sp.sph#,lin.rfreq'+ $
    ' from mm..SPH sp,mm..BLH bl,mmdb..BLS bls, mmdb..LIN lin where  '+ $
    'bl.bls#= bls.bls# and bls.blcd="'+baselines[j[0]]+'" and sp.blh#=bl.blh# '+ $
    'and lin.lin#=sp.lin# and bl.sb="'+ sb + $
    '" and sp.inh# = '+string(inhid_pres)+' and band="s1"'
  if e.debug then print,'sql :',sql_str
  result=dbi_sql_submit(sql_str,/no_notify)

; inh#,blh#,ut,gq,pq,band,dcvrt,sb,pstate,tau0,dtau0,
; lin#,vel,vres,vtype,fsky,fres,tssb,integ,wt,taper,svar,
; snoise,nch,nrec,dataOFF
  
  vars=str_sep(strcompress(result[2]),' ',/remove_all)
  if n_elements(vars) lt 25 then begin
    print,'Warning : no header found for ',inhid_pres,' sb : ',sb,' s1 (skip this record)'
    goto, endsb
  endif 
  if e.debug then begin
    for ii=0,n_elements(vars)-1 do begin
;      print,'vars ',ii,' ',vars[ii]
    endfor
  endif
  sph_inhid[isb,i]=inhs[j[0]]
  sph_blhid[isb,i]=vars[2]
  ut='' & reads,result[2],ut,format='(32x,a19)'
  sph_ut[isb,i]=ut
  sph_gq[isb,i]=vars[7]
  io=0
  if (vars[7] ne 'g') then begin
    io=-1
    sph_gq[isb,i]=''
  endif
  sph_pq[isb,i]=vars[8+io]
  if (vars[8+io] ne 'p') then begin
    io=io-1
    sph_pq[isb,i]=''
  endif
  sph_band[isb,i]=vars[9+io] 
  sph_dcvrt[isb,i]=vars[10+io]
  sph_sb[isb,i]=vars[11+io]
  sph_pstate[isb,i]=vars[12+io]
  sph_tau0[isb,i]=vars[13+io]
  sph_dtau0[isb,i]=vars[14+io]
  sph_linid[isb,i]=vars[15+io]
  sph_vel[isb,i]=vars[16+io]*(f0s[j[0]]-double(vars[31+io]))/ $
             (double(vars[19+io])-double(vars[31+io]))
  sph_vres[isb,i]=vars[17+io]*(deltafs[j[0]]/double(vars[20+io]))
  sph_vtype[isb,i]=vars[18+io]
  sph_fsky[isb,i]=f0s[j[0]]
  sph_fres[isb,i]=deltafs[j[0]]
  sph_tssb[isb,i]=tssbs[j[0]]
  sph_integ[isb,i]=total(integTimes[j])
  sph_wt[isb,i]=vars[23+io]
  sph_taper[isb,i]=vars[24+io]
  sph_svar[isb,i]=vars[25+io]
  sph_snoise[isb,i]=vars[26+io]
  sph_nch[isb,i]=numchanss[j[0]]
  sph_nrec[isb,i]=1
  sph_dataOFF[isb,i]=offset
  offset=offset+2L*sph_nch[isb,i]+5L 
  sph_sphid[isb,i]=vars[30+io]
  sph_rfreq[isb,i]=vars[31+io]
;
; average records
;
  for jj=0,n_elements(j)-1 do begin
    jjj=j[jj]
    sch_rviss[isb,i,0:numchanss[jjj]-1]= $
        sch_rviss[isb,i,0:numchanss[jjj]-1]+ $
        rviss[dataOFF[jjj]:dataOFF[jjj]+numchanss[jjj]-1]
    sch_iviss[isb,i,0:numchanss[jjj]-1]= $
        sch_iviss[isb,i,0:numchanss[jjj]-1]+ $
        iviss[dataOFF[jjj]:dataOFF[jjj]+numchanss[jjj]-1]
  endfor
  sch_rviss[isb,i,0:sph_nch[isb,i]-1]= $
       sch_rviss[isb,i,0:sph_nch[isb,i]-1]/float(n_elements(j))
  sch_iviss[isb,i,0:sph_nch[isb,i]-1]= $
       sch_iviss[isb,i,0:sph_nch[isb,i]-1]/float(n_elements(j))

  maxvis=(max([sch_rviss[isb,i,*],sch_iviss[isb,i,*]]) > 10.)
  minvis=min([sch_rviss[isb,i,*],sch_iviss[isb,i,*]])
  print,maxvis,minvis
  scale = 3.321928*alog10(maxvis/32000.)
  if scale GT 0. THEN scale=scale+1.
  iscale=long(scale)
  scale=2.^iscale
  iscale=long((alog10(double(scale))/log2)-0.001)
  sch_rviss[isb,i,*]=sch_rviss[isb,i,*]/scale
  sch_iviss[isb,i,*]=sch_iviss[isb,i,*]/scale
  sph_snoise[isb,i]=sph_snoise[isb,i]/scale
  maxvis=max([sch_rviss[isb,i,*],sch_iviss[isb,i,*]])
  minvis=min([sch_rviss[isb,i,*],sch_iviss[isb,i,*]])
print,'maxvis,minvis',maxvis,minvis
;
; now do channels in sch
;
   ii=sph_dataOFF[isb,i]
   packdata[ii]=sph_integ[isb,i]*10.e0
   packdata[ii+1L]=sph_integ[isb,i]*10./2.
   top_noise=long(sph_snoise[isb,i]/32768L)
   bot_noise=sph_snoise[isb,i]-top_noise*32768L
   packdata[ii+2L]=top_noise
   packdata[ii+3L]=bot_noise
   packdata[ii+4L]=(alog10(double(scale))/log2)-0.001
   packdata[ii+5L+ichs]=sch_rviss[isb,i,0: $
        sph_nch[isb,i]-1]
   packdata[ii+6L+ichs]=sch_iviss[isb,i,0: $
        sph_nch[isb,i]-1]
   form='I2-C'

;
; check if there is already a row in sph_cobra, if so  delete it
;
  sql_str='select count(*) from mm..SPH_COBRA sp  where  '+ $
    ' sp.sph# = '+string(sph_sphid[isb,i])
  if e.debug then print,'sql :',sql_str
  result=dbi_sql_submit(sql_str,/no_notify)
  if e.debug then print,result
  ct=long(result[2])
  if (ct > 0) then begin
    sql_str='delete mm..SPH_COBRA where  '+ $
    ' sph# = '+string(sph_sphid[isb,i])
    if e.debug then print,'sql :',sql_str
    result=dbi_sql_submit(sql_str,/no_notify)
  endif
  sql_str='insert mm..SPH_COBRA values('+ $
    string(sph_sphid[isb,i])+','+string(sph_inhid[isb,i])+','+ $
    string(sph_blhid[isb,i])+',"'+string(sph_ut[isb,i])+'",'+ $
    '"'+string(sph_gq[isb,i])+'"'+','+'"'+string(sph_pq[isb,i])+'",'+ $
    '"'+string(sph_band[isb,i])+'",'+'"'+string(sph_dcvrt[isb,i])+'",'+ $
    '"'+string(sph_sb[isb,i])+'"'+','+'"'+string(sph_pstate[isb,i])+'",'+ $
    string(sph_tau0[isb,i])+','+string(sph_dtau0[isb,i])+ $
    ','+string(sph_linid[isb,i])+','+ $
    string(sph_vel[isb,i])+','+string(sph_vres[isb,i])+',"'+ $
    string(sph_vtype[isb,i])+'",'+string(sph_fsky[isb,i])+','+ $
    string(sph_fres[isb,i])+','+string(sph_tssb[isb,i])+','+ $
    string(sph_integ[isb,i])+','+string(sph_wt[isb,i])+','+ $
    '"'+string(sph_taper[isb,i])+'"'+','+string(sph_svar[isb,i])+','+ $
    string(sph_snoise[isb,i])+','+string(sph_nch[isb,i])+','+ $
    string(sph_nrec[isb,i])+','+string(2L*sph_dataOFF[isb,i])+')'

  if e.debug then print,'sql :',sql_str
  result=dbi_sql_submit(sql_str,/no_notify)
  if e.debug then print,'sql results : ',result
  num_sph_writes=num_sph_writes+1
;
; write a dummy continuum row also
;
  sql_str='set rowcount 1 select sp.sph#'+ $
    ' from mm..SPH sp,mm..BLH bl,mmdb..BLS bls, mmdb..LIN lin where  '+ $
    'bl.bls#= bls.bls# and bls.blcd="'+baselines[j[0]]+'" and sp.blh#=bl.blh# '+ $
    'and lin.lin#=sp.lin# and bl.sb="'+ sb + $
    '" and sp.inh# = '+string(inhid_pres)+' and band="c1"'
  if e.debug then print,'sql :',sql_str
  result=dbi_sql_submit(sql_str,/no_notify)
  vars=str_sep(strcompress(result[2]),' ',/remove_all)
  sph_cont=vars[1]
  sql_str='select count(*) from mm..SPH_COBRA sp  where  '+ $
    ' sp.sph# = '+string(sph_cont)
  if e.debug then print,'sql :',sql_str
  result=dbi_sql_submit(sql_str,/no_notify)
  if e.debug then print,result
  ct=long(result[2])
  if (ct > 0) then begin
    sql_str='delete mm..SPH_COBRA where  '+ $
    ' sph# = '+string(sph_cont)
    if e.debug then print,'sql :',sql_str
    result=dbi_sql_submit(sql_str,/no_notify)
  endif
  sql_str='insert mm..SPH_COBRA values('+ $
    string(sph_cont)+','+string(sph_inhid[isb,i])+','+ $
    string(sph_blhid[isb,i])+',"'+string(sph_ut[isb,i])+'",'+ $
    '"'+string(sph_gq[isb,i])+'"'+','+'"'+string(sph_pq[isb,i])+'",'+ $
    '"'+"c1"+'",'+'"'+string(sph_dcvrt[isb,i])+'",'+ $
    '"'+string(sph_sb[isb,i])+'"'+','+'"'+string(sph_pstate[isb,i])+'",'+ $
    string(sph_tau0[isb,i])+','+string(sph_dtau0[isb,i])+ $
        ','+string(sph_linid[isb,i])+','+ $
    string(sph_vel[isb,i])+','+string(sph_vres[isb,i])+',"'+ $
    string(sph_vtype[isb,i])+'",'+string(sph_fsky[isb,i])+','+ $
    string(sph_fres[isb,i])+','+string(sph_tssb[isb,i])+','+ $
    string(sph_integ[isb,i])+','+string(sph_wt[isb,i])+','+ $
    '"'+string(sph_taper[isb,i])+'"'+','+string(sph_svar[isb,i])+','+ $
    string(sph_snoise[isb,i])+','+'1'+','+ $
    '1'+','+string(2L*sph_dataOFF[isb,i])+')'
  if e.debug then print,'sql :',sql_str
  result=dbi_sql_submit(sql_str,/no_notify)
  if e.debug then print,'sql results : ',result
  print,'inh# ',sph_inhid[isb,i],' sb ',sph_sb[isb,i],' integ ',sph_integ[isb,i],' data offset ',sph_dataOFF[isb,i]

  endsb:
endfor
;
; now write into sch_cobra (have to read into a string and then 
; remove the first 4 letters because idl converts int to long
; before doing hex output)
;
  st=''
  for ii=0,n_elements(packdata)-1 do begin
    reads,string(packdata(ii),format='(z8)'),st,format='(4x,a4)'
    if e.debug then print,ii,packdata[ii],'__:',st
    packstring=packstring+st
  endfor

;
; then delete pre-existing row in sch_cobra
;
  sql_str='select count(*) from mm..SCH_COBRA sc  where  '+ $
    ' sc.inh# = '+string(inhid_pres)
  if e.debug then print,'sql :',sql_str
  result=dbi_sql_submit(sql_str,/no_notify)
  if e.debug then print,result
  ct=long(result[2])
  if (ct > 0) then begin
    sql_str='delete mm..SCH_COBRA where  '+ $
    ' inh# = '+string(inhid_pres)
    if e.debug then print,'sql :',sql_str
    result=dbi_sql_submit(sql_str,/no_notify)
  endif
  
  uti_stri_replace,packstring,' ','0'
  nbyt=strlen(packstring)
  if e.debug then print,'nbyt ',nbyt,' packstring : ',packstring

  sql_str='set textsize 700000 insert mm..SCH_COBRA values('+ $
    string(inhid_pres)+',"'+form+'",'+ $
    string(nbyt)+','+packstring+')'

  if e.debug then print,'sql :',sql_str
  result=dbi_sql_submit(sql_str,/no_notify)
  if e.debug then print,'sql results : ',result
  num_sch_writes=num_sch_writes+1
  jump:
endfor
print,'wrote ',num_sph_writes,' rows in sph'
print,'wrote ',num_sch_writes,' rows in sch'
return,0
end



