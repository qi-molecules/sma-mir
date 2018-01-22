pro uti_deltaza, antennasFile=antennasFile, reverse=reverse
common global
common data_set

if keyword_set(reverse) then sign=-1 else sign=1 
filename='a'
; read in antennas file
if not keyword_set(antennasFile) then read,filename,prompt='Enter the ANTENNAS file location (or return for ./antennas as default):' else filename=antennasFile
if filename eq '' then filename='./antennas'
count=0 & result=findfile(filename,count=count)
IF count eq 0 THEN BEGIN
   print,'Could not find file: ',filename
   print,'Failed. No deltaZA corrections !'
   RETURN
ENDIF
openr,/get_lun, unit, filename
base=dblarr(4,10)
line=dblarr(4)
ntel=0
print, 'Antenna positions used:'
while not EOF(unit) do begin
   readf,unit,line
   base[*,ntel]=line
   print,line
   ntel=ntel+1
endwhile
free_lun,unit
xyz=base[1:3,0:ntel-1]
; calculate neu
lat=19.82420526391d/57.29577951d
m1=[[-sin(lat),0.d,cos(lat)],[0.d,1.d,0.d],[cos(lat),0.d,sin(lat)]]
neu=xyz##m1
neu=transpose(neu)

positions_i={tel:0,dte:0.0D,dtn:0.0D,dtu:0.0D,dha:0.0D,dlat:0.0D}
positions=replicate(positions_i,ntel)
positions.tel=indgen(ntel)+1
positions.dte=neu[*,1]
positions.dtn=neu[*,0]
positions.dtu=neu[*,2]
positions.dha=neu[*,1]/6.378e6
positions.dlat=neu[*,0]/6.378e6

npts=sp[psl].nrec*sp[psl].nch
pcl_end=pcl+npts-1L

  ; Segment the data by baselines, records, and sidebands and
  ; determine the number of unique combinations 
bls   = c.blcd[bl[pbl].iblcd]
recs  = c.rec[bl[pbl].irec]
sbs   = c.sb[bl[pbl].isb]
bcals = bls+' '+recs+' '+sbs
distinct_bcals = uti_distinct(bcals,nbcals,/many_repeat)

  ; Set ending records
;prl_end = prl + sp[psl].nrec - 1L

for i = 0L, nbcals-1L do begin
  ; Determine vector locations for this combination of bls/rec/sbs
   js = where(distinct_bcals[i] eq bcals,njs) 
   if (njs gt 0L) then begin
  ; Determine the location of the continuum scans
      ant1=bl[pbl[js[0]]].itel1
      ant2=bl[pbl[js[0]]].itel2      
      js_cont=where(abs(sp(psl(js)).iband) eq 1,njs_cont)
      ii=where(positions.tel eq ant1)
      jj=where(positions.tel eq ant2)
  ; Loop over all combinations
      for j = 0L, njs-1L do begin
  ; Set id number to js[j]
         jsj = js[j]
 ;        lambda=!cvel/sp[psl[jsj]].fsky/1e6 ; mm
         nch=sp[psl[jsj]].nch
         skyfreq=sp[psl[jsj]].fsky+0.001* $
           (findgen(nch)+(1.-float(nch))/2.)*sp[psl[jsj]].fres
         lambda=!cvel/skyfreq/1e6 ; mm
         dec=in[pil[jsj]].decr
         ha=in[pil[jsj]].ha*15.*!DPI/180.
         sinalt1=sin(dec)*sin(lat+positions[ii].dlat)+cos(dec)*cos(lat+positions[ii].dlat)*cos(ha+positions[ii].dha)
         sinalt2=sin(dec)*sin(lat+positions[jj].dlat)+cos(dec)*cos(lat+positions[jj].dlat)*cos(ha+positions[jj].dha)
         dL=sign*(-1.)*(1./sinalt1 - 1./sinalt2)*1430.d
         phase=dL/lambda
         cphase=complex(cos(phase*!TWOPI),sin(phase*!TWOPI))
         if (sp[psl[jsj]].nch eq 1) then bl[pbl[jsj]].phaave=uti_pha_180(bl[pbl[jsj]].phaave-phase*360.)
         j1 = pcl[jsj]
         j2 = pcl_end[jsj]
         ch[j1:j2]=ch[j1:j2]*cphase
      endfor
   endif
endfor
   

print,'DeltaZA correction done!'      
end





