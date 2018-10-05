function gain_xpol4,gai_souids,gai_fluxes_3mm,gai_fluxes_1mm,tel_bsl, $
         x_var, y_vars,frames_per_page,dt_smooth=dt_smooth,plid, $
         refant=refant, loose=loose, connect=connect, preavg=preavg, $
         no_unwrap=no_unwrap,non_point=non_point, multicolor=multicolor, $
         noplot=noplot, ramp=ramp
common global
common data_set
common plo

;
; setting up constants
;
tel_bsl='baseline'

; a quick check on input parameters

if (x_var eq 'el') and y_vars ne 'amp' then begin
     print, 'cal_type must be set to amp only'
     return, -1
endif

;if (not keyword_set(non_point)) then non_point=0
if (not keyword_set(connect)) then connect=0

;
; debugging messages
;
if e.debug then begin
  print,'gai_soiuids = ',gai_souids
  print,'gai_fluxes_3mm = ',gai_fluxes_3mm
  print,'gai_fluxes_1mm = ',gai_fluxes_1mm
  print,'tel_bsl = ',tel_bsl
  print,x_var
  print,y_vars
  print,frames_per_page
  print,dt_smooth
  print,plid
  if (keyword_set(refant)) then print,refant
  if (keyword_set(loose)) then print,loose
endif
;
; interactive device setup
;
if not e.java then begin
plid = plo_plid_gen()
endif

;
; setting up plot label 
;
   case x_var of
     'hours' : bottom_label='reference time :'+c.ref_time(in(pil(0)).iref_time)
     'int'   : bottom_label='track : '+strtrim(string(in(pil(0)).traid),2)
     'ha'    : bottom_label='track : '+strtrim(string(in(pil(0)).traid),2)
     'el'    : bottom_label='track : '+strtrim(string(in(pil(0)).traid),2)
     else    : begin 
               print,'*** ',x_var,' not recognized x-coord !'
               return,-1
               endelse
   endcase
;
; in case user has not called gai_ini
;
if n_elements(gai_souids) eq 0 then begin
  result=gai_ini(30.,gai_souids,all_sources,all_amps,all_numbs, $
                gai_fluxes_3mm,gai_fluxes_1mm,use)
  j=where(use ne 0,count)
  if count gt 0 then begin
    gai_souids=gai_souids[j] 
    gai_fluxes_3mm=gai_fluxes_3mm[j] & gai_fluxes_1mm=gai_fluxes_1mm[j]
  endif
endif
;
; setting up integration header range
;
  result=dat_list(s_l, '"band" like "c" and "wt" gt "0"', /no_notify,/reset)
  inhid_beg=min([in(pil).inhid]) & inhid_end=max([in(pil).inhid])
  if (e.debug) then print, 'integration header limits ',inhid_beg, inhid_end
;
; check if there is any cal data at all
;
souid_str="("
k=0
for i=0,n_elements(gai_souids)-1 do begin
     k=k+1
     if k ne 1 then souid_str=souid_str+' or '
     souid_str=souid_str+'"souid" eq "'+strtrim(string(gai_souids[i]),2)+'"'
endfor
souid_str=souid_str+")"

result=dat_list(s_l,souid_str+' and "band" like "c" and "wt" gt "0"', /no_notify, /reset)

if result le 0 then begin
   result=dat_list(s_l,/reset)
   print,'No gain derivation due to lack of data (check filter).'
   return, -1
endif

; prepare for frames to apply solutions
bls=c.blcd(bl(pbl).iblcd)
recs=c.rec(bl(pbl).irec)
sbs=strupcase(c.sb(bl(pbl).isb))
bands=strupcase(c.band(sp(psl).iband))
combinations=bls+' '+recs+' '+sbs+' '+bands
distinct_combinations=uti_distinct(combinations,ncombinations,/many_repeat)
distinct_bls=uti_distinct(bls,nbls,/many_repeat)
;
;
; prepare OBSERVED uv data (first select uvdata and get integration limits)
;

cmps=make_array(1,/complex,/nozero)
result=dat_list(s_l,souid_str+'and (("ipol" eq "4")) and "iband" eq "0" and "wt" gt "0"',/reset)

a0=pil & a1=pbl & a2=psl & a3=pcl & a4=prl

ibls=uti_distinct(bl[a1].iblcd,nbls2,/many_repeat)
nb=fltarr(nbls2)
for i=0, nbls2-1 do begin
   j=where(bl[a1].iblcd eq ibls[i],count)
   nb[i]=count
endfor
i=where(nb eq max(nb))
refib=ibls[i[0]]
;print,'Using reference baseline  ', c.blcd[refib]

ints = in[pil].int
ii=uti_distinct(in[pil].int,nint,/many_repeat)
for i=0L,nint-1L do begin
   inti=ii[i]
   for j=0,1 do begin
      n=where( (bl[a1].isb eq j) and (in[a0].int eq inti),count)
      if count gt 0 then begin
         m=where( bl[a1[n]].iblcd eq refib, count)
         if count gt 0 then cmps=[cmps,mean(ch[a3[n]])]
      endif
   endfor
endfor

nc=n_elements(cmps)
cmps=cmps[1L:nc-1L]
uti_conv_apc,cmps,amps,phas,/amp_pha

souid_str=souid_str+' and "blcd" eq "'+c.blcd[refib]+'"'
result=dat_list(s_l,souid_str+' and (("ipol" eq "4")) and "band" like "c" and "wt" gt "0"', /no_notify, /reset)

if result ne (nc-1L) then begin
   result=dat_list(s_l,/reset)
   print,'Inconsistent cross rx data '
   print,'Quit'
   result=dat_list(/reset)
   return, -1
endif

if (e.debug) then print,"PREPARING OBSERVED UV DATA"
;amps=bl(pbl).ampave
;phas=uti_pha_180 (bl(pbl).phaave )
cohs=bl(pbl).coh
wts=sp(psl).wt

if (keyword_set(preavg)) then begin

   print,"[PREAVG SELECTED} Calculating visibility averages"
   
   pil_bak=pil & pbl_bak=pbl & psl_bak=psl & pcl_bak=pcl & prl_bak=prl
   
;   totalints = in(pil).int
;   intlist = totalints(  uniq(totalints,sort(totalints) ))
;   nofint = n_elements(intlist)
     
    tmpblcd=strcompress(string(bl[pbl[0]].iblcd),/remove)
    tmprec=strcompress(string(bl[pbl[0]].irec),/remove)
    tmpsb=strcompress(string(bl[pbl[0]].isb),/remove)
    command=' "iblcd" eq "'+tmpblcd+'" and "iband" eq "0" and "irec" eq "'+tmprec+'" and "isb" eq "'+tmpsb+'"'
    result=dat_list(s_l,command,/reset,/no_notify)
    intlist=in[pil].int 
    nofint=n_elements(intlist)
    templist=in[pil].isource 
    tempjump=where( (templist-shift(templist,1) ne 0) or (intlist - shift(intlist,1) ne 1))
    gindex_from=tempjump
    ngroup=n_elements(tempjump)
    gindex_to=gindex_from[1:ngroup-1]-1L
    gindex_to=[gindex_to,nofint-1]
    ;stop
    ;print, intlist(gindex_from)
    ;print, intlist(gindex_to)
 
;   gindex_from=where((intlist - shift(intlist,1)) ne 1)
;   gindex_to=where((intlist - shift(intlist,-1)) ne -1)
   gindex_mid= (gindex_from+gindex_to)/2
;   ngroup = n_elements(gindex_mid)

   pil=pil_bak & pbl=pbl_bak & psl=psl_bak & pcl=pcl_bak & prl=prl_bak

   print,'VISILITIES WILL BE GROUP AND AVERAGED'
   print,'OVER INTEGRATIONS IN THE FOLLOWING WAY:'
   print,'      GROUP                 INTEGRATION'
   for ib = 0, ngroup -1 do begin
      print, ib+1, '   int ',intlist(gindex_from(ib)), ' to ',intlist(gindex_to(ib))
   endfor
   
   marker = make_array(n_elements(pil),/int)
   grptag = make_array(n_elements(pil),/int)

;    for i = 0, ngroup -1 do begin
;      tmp_idx = where(in(pil).int eq intlist(gindex_mid(i)))
;      marker(tmp_idx) = 1
;    endfor
;    marker_idx = where (marker ne 0)

   bls=c.blcd(bl(pbl).iblcd)
   recs=c.rec(bl(pbl).irec)
    ;
    ; change from rec=1,2 to 3mm,1mm
    ;
   j=where(recs eq '1')
   if max(j) ne -1 then recs(j)= '3mm' 
   j=where(recs eq '2')
   if max(j) ne -1 then recs(j)= '1mm'
    ;
    ; Upper Case sb and band, then find all distinct combinations 
    ; of blcd, receiver, sb, and band
    ;
   sbs=strupcase(c.sb(bl(pbl).isb))
   bands=strupcase(c.band(sp(psl).iband))
    ;
    ; setup the variables which will discriminate frames
    ;
   frames=bls+' '+recs+' '+sbs+' '+bands
   distinct_frames=uti_distinct(frames,nframes,/many_repeat)

   for ia=0,nframes-1 do begin
      for ib = 0, ngroup -1 do begin

         js=where(frames eq distinct_frames(ia) and wts gt 0. and $
           in(pil).int ge intlist(gindex_from(ib)) and $
           in(pil).int le intlist(gindex_to(ib)) ,count)
         if (count le 0) then begin
            print, "missing data for group ", ib+1, " in ", distinct_frames(ia)
         endif else begin

;          to_mark = 0
;          ic=0
;          while (to_mark eq 0 and ic lt ngroup) do begin
;             tmp_idx = where(in(pil(js)).int eq intlist(gindex_mid(ic)),tmp_count)
;             stop,ia,ib,to_mark,ic
;             if (tmp_count ne 0) then to_mark = 1
;             ic=ic+1
;          endwhile

            distinct_source=uti_distinct(in[pil[js]].isource,ns,/many_repeat)
            if ns gt 1 then begin
               print,'Source has been changed between integration #',intlist(gindex_from(ib)), ' to ',intlist(gindex_to(ib))
               print,'Please flag out the integration during antenna slewing!'
               print,'Quit !'
               return,0
            endif

            tmp_idx = count/2

            if (e.debug) then begin
               print, 'JS ', js
               print, 'INTs ', frames(js),in(pil(js)).int
               print,'tmp_idx is ', tmp_idx
            endif
            
            tmp_data=make_array(n_elements(js),/dcomplex)
            uti_conv_apc, tmp_data, amps(js), phas(js), /complex

            tmp_avg = total(tmp_data)/float(n_elements(js))
            uti_conv_apc, tmp_avg, tmp_ampave, tmp_phaave, /amp_pha

            amps(js(tmp_idx))=tmp_ampave
            phas(js(tmp_idx))=tmp_phaave

            marker(js(tmp_idx)) = 1
            grptag(js(tmp_idx)) = ib+1

            if (e.debug) then begin
               print, 'DATA ', tmp_data,tmp_avg
               print,js(tmp_idx),tmp_ampave,tmp_phaave
            endif
            
         endelse

      endfor
   endfor

   marker_idx = where (marker ne 0)
   
   amps = amps(marker_idx)
   phas = phas(marker_idx)
   cohs = cohs(marker_idx)

   grptag = grptag(marker_idx)

   pil = pil(marker_idx)
   pbl = pbl(marker_idx)
   psl = psl(marker_idx)
   pcl = pcl(marker_idx)
   prl = prl(marker_idx)

endif

;
; set up MODEL uv data
;
if (e.debug) then print,"SETTING UP MODEL UV DATA"
;
; POINT SOURCES (quasars, setting up fluxes first)
;
for i=0,n_elements(gai_souids)-1 do begin
  ;
  ; if missing 1 or 3mm flux use the other, also cut out source with neither
  ; for different freqs use -0.5 spectral index
  ;
  if gai_fluxes_1mm[i] eq 0. then gai_fluxes_1mm[i] = gai_fluxes_3mm[i] / 0.66
  if gai_fluxes_3mm[i] eq 0. then gai_fluxes_3mm[i] = gai_fluxes_1mm[i] * 0.66
endfor

; modelamp (simply the input amplitude)
; modelpha (simply zero)
modelamps=make_array(n_elements(pbl),/float)
modelphas=make_array(n_elements(pbl),/float)

for j=0,n_elements(gai_souids)-1 do begin
    k=where(in[pil].souid eq gai_souids[j] and bl[pbl].irec ge 1,count)
    if count gt 0 then modelamps[k]=gai_fluxes_1mm[j]
    kk=where(in[pil].souid eq gai_souids[j] and bl[pbl].irec eq 0,count)
    if count gt 0 then modelamps[kk]=gai_fluxes_3mm[j]
    print,j,gai_souids[j],gai_fluxes_1mm[j],gai_fluxes_3mm[j]
;    if (c.source[gai_souids[j]] eq 'uranus' or
;    c.source[gai_souids[j]] eq 'neptune' or c.source[gai_souids[j]]
;    eq 'callisto')  then begin
    kkk=where(in[pil].souid eq gai_souids[j])
; 1830-210 is turned out to be a gravitationally lensed 'double'
; source with raoff 0.35 and decoff 0.394 arcsecs. 
    if (c.source[gai_souids[j]] eq '1833-210' or c.source[gai_souids[j]] eq '1830-210') then begin
    print, 'Correction for 1833-210 or 1830-210 applied !'
       dph=bl[pbl].u*1000.*(0.35/3600.)*(!pi/180.)+bl[pbl].v*1000.*(0.394/3600.)*(!pi/180.)
       modelamps[kkk]=modelamps[kkk]*abs(cos(dph[kkk]*!TWOPI))
       jj=where(cos(dph[kkk]*!TWOPI) lt 0., count)
       if count gt 0. then modelphas[kkk[jj]]=180.
    endif
;    if ((non_point gt 0.) and (in[pil[kkk[0]]].size gt 0.)) then begin
;      uvdis=sqrt(bl[pbl].u^2 + bl[pbl].v^2)
;      radius = in[pil].size / 2.0 
;      freq   = sp[psl].fsky    
;      res = flux_primary(c.source[gai_souids[j]],radius,freq,xflux)
;      result=uti_gaussqs(uvdis(kkk),radius(kkk),freq(kkk),vis)
;      if res lt 0. then begin
;         modelamps[kkk]=abs(vis)*modelamps[kkk] 
;         jj=where(vis lt 0.,count)
;         if count gt 0. then modelphas[kkk[jj]]=180.
;      endif else begin
;         modelamps[kkk]=abs(vis)*xflux(kkk)
;         jj=where(vis lt 0.,count)
;         if count gt 0. then modelphas[kkk[jj]]=180. 
;      endelse
;   endif
endfor 

;
; PLANETS (or SATIILITES)
;
; not implemented yet

;
; USER INPUT MODELS
; not implemented yet

;
; normalize by source fluxes
;
;
; normalize data by source model in complex-variable form
;

if (e.debug) then print,"DATA NORMALIZATION by SOURCE MODEL"
  
complexdata=make_array(n_elements(pbl),/dcomplex)
uti_conv_apc, complexdata, amps, phas, /complex

complexmodel=make_array(n_elements(pbl),/dcomplex)
uti_conv_apc, complexmodel, modelamps, modelphas, /complex

complexr=make_array(n_elements(pbl),/dcomplex)
complexr = complexdata/complexmodel

complexcr=make_array(n_elements(pbl),/dcomplex)
complexcr= conj(complexdata)/complexmodel

;
; Generate gain curve for display/fitting
;
;
print,'**** BASELINE-BASED GAIN CALIBRATION **** '

ampg=make_array(n_elements(pbl),/float)
phag=make_array(n_elements(pbl),/float)

    ; amp/phase gain curve is the amp/phase from the normalized ratio
uti_conv_apc, complexr, ampg, phag, /amp_pha

;
; setting up the corresponding x-coord array
;
case x_var of
   'hours' : xs=bl(pbl).avedhrs
   'int'   : xs=in(pil).int
   'ha'    : xs=in[pil].ha
   'el'    : xs=in[pil].el
endcase

;
; segment gain curves (y-coord) by blcd, receiver, sb, and band
;

bls=c.blcd(bl(pbl).iblcd)
recs=c.rec(bl(pbl).irec)
                                ;
                                ; change from rec=1,2 to 3mm,1mm
                                ;
j=where(recs eq '1')
if max(j) ne -1 then recs(j)= '3mm' 
j=where(recs eq '2')
if max(j) ne -1 then recs(j)= '1mm'
    ;
    ; Upper Case sb and band, then find all distinct combinations 
    ; of blcd, receiver, sb, and band
    ;
sbs=strupcase(c.sb(bl(pbl).isb))
bands=strupcase(c.band(sp(psl).iband))
    ;
    ; setup the variables which will discriminate frames
    ;
frames=bls+' '+recs+' '+sbs+' '+bands
distinct_frames=uti_distinct(frames,nframes,/many_repeat)

if (e.debug) then print,'distinct_frames include: ', distinct_frames

    ;
    ; setting up color/symbol indeices
    ;
if keyword_set(multicolor) then colors=strupcase(c.source(in(pil).isource)) else colors=''
symbols=''
;
; Reform useable gain curves with good data only
;

; plot coherence only for amps > 0.4
;
;
;j=where(amps lt 0.4,count)
;   if count gt 0 then cohs(j) = !BAD_VALUE

;
; setting up pt indices 
;
 pt_first=0
 pt_npts=0
 multi_pt=0
;
; remove zero wt pts
;
 j=where(wts le 0.,count)
   if count gt 0 then begin
   ampg[j]=!BAD_VALUE
   phag[j]=!BAD_VALUE
   cohs[j]=!BAD_VALUE
 endif

;
; SETTING UP INTERACTIVE PLOTS
;

;
; set up gain/plot index
;
 gindex = 0

 pl[plid].plot_type='gai'
 if not e.java then pl[plid].plot_interact=plo_init(plid,gindex)

;
; setting up for color/symbol in plots
;
 distinct_colors=uti_distinct(colors, /many_repeat,ncolors)
 color_index=0.6*bytscl((indgen(max([n_elements(distinct_colors),1])) $
             mod 16)+1)+64.

 distinct_symbols=''
 symbol_pt_index=2 & symbol_line_index=0

;
; setting up all plotting variables
;
npts=n_elements(xs)

xmin=min(xs) & xmax=max(xs)
dx=xmax-xmin & xmin=xmin-0.05*dx & xmax=xmax+0.05*dx
ymin=0. & amp_max=max(ampg)
dy=amp_max-ymin & ymin=ymin-0.05*dy & amp_max=amp_max+0.05*dy
nor_xmin=0.1 & nor_xmax=0.95 & nor_ymin=0.1 & nor_ymax=0.94
if n_elements(distinct_colors) gt 2 then begin
 nor_xmax=0.95-0.05 
endif

nor_dx=nor_xmax-nor_xmin & nor_dy=nor_ymax-nor_ymin
frames_per_page=min([nframes,frames_per_page])
ncol=max([long(sqrt(frames_per_page)),1]) 
nrow=frames_per_page/ncol 
frames_per_page = nrow * ncol 
if e.debug then print,nframes,' frames ',frames_per_page,' frames_per_page ',ncol,' cols ',nrow,' rows'
if e.debug then print,n_elements(distinct_frames),' frames ',n_elements(distinct_colors), $
        ' colors ',n_elements(distinct_symbols),' symbols '
;
; generate a page and frame location for each frames
;
page=indgen(nframes)/frames_per_page
frame_on_page=indgen(nframes)-page*frames_per_page
row=frame_on_page/ncol & col=frame_on_page-row*ncol
pan_dx=nor_dx/ncol
pan_dy=nor_dy/nrow
pan_xmin=nor_xmin+col*pan_dx & pan_xmax=nor_xmin+(col+1)*pan_dx
pan_ymax=nor_ymax-row*pan_dy & pan_ymin=nor_ymax-(row+1)*pan_dy
;
; setup sub-panels : sub_fxmin => fraction of panel dx to offset from min
;                    sub_fxmax => fraction of panel dx to offset from min
; note : o'th subpanel at top
;
nsub=0
case y_vars of
     'amp,pha': begin    ; actually 'amp,pha,coh'
                      nsub=3 
                      y_var=['amp','pha','coh'] & psym=[10,1,1]
                      sub_fymin=make_array(nsub,/float,value=0.) 
                      sub_fymax=make_array(nsub,/float,value=1.)
                      sub_fymin(2)=0.0 & sub_fymax(2)=0.2 
                      sub_fymin(1)=0.2 & sub_fymax(1)=0.5 
                      sub_fymin(0)=0.5 & sub_fymax(0)=1.
                      ymin=make_array(nframes,nsub,/float,value=0.)
                      ymax=make_array(nframes,nsub,/float,value=amp_max)
                      ymin(*,2)=0.4 & ymax(*,2)=1.2
                      ys=make_array(nsub,npts,/float,/nozero) 
                      ys(0,*)=ampg & ys(1,*)=phag & ys(2,*)=cohs

      print,n_elements(ys)

                    end
     'amp'    : begin        ; actually 'amp,coh'
                      nsub=2 
                      y_var=['amp','coh'] & psym=[10,1]
                      sub_fymin=make_array(nsub,/float,value=0.) 
                      sub_fymax=make_array(nsub,/float,value=1.)
                      sub_fymax(1)=0.3 & sub_fymin(0)=0.3  
                      ymin=make_array(nframes,nsub,/float,value=0.)
                      ymax=make_array(nframes,nsub,/float,value=amp_max)
                      ymin(*,1)=0.4 & ymax(*,1)=1.2
                      ys=[ampg,cohs]
                      ys=make_array(nsub,npts,/float,/nozero) 
                      ys(0,*)=ampg & ys(1,*)=cohs
                    end
     'pha'    : begin        ; actually 'pha,coh'
                      nsub=2 
                      y_var=['pha','coh'] & psym=[1,1]
                      sub_fymin=make_array(nsub,/float,value=0.) 
                      sub_fymax=make_array(nsub,/float,value=1.)
                      sub_fymax(1)=0.3 & sub_fymin(0)=0.3  
                      ymin=make_array(nframes,nsub,/float,value=0.)
                      ymax=make_array(nframes,nsub,/float,value=amp_max)
                      ymin(*,1)=0.4 & ymax(*,1)=1.2
                      ys=[phag,cohs]
                      ys=make_array(nsub,npts,/float,/nozero) 
                      ys(0,*)=phag & ys(1,*)=cohs
                    end
     'complex' : begin
                      nsub=3 
                      y_var=['amp','pha','coh'] & psym=[10,1,1]
                      sub_fymin=make_array(nsub,/float,value=0.) 
                      sub_fymax=make_array(nsub,/float,value=1.)
                      sub_fymin(2)=0.0 & sub_fymax(2)=0.2 
                      sub_fymin(1)=0.2 & sub_fymax(1)=0.5 
                      sub_fymin(0)=0.5 & sub_fymax(0)=1.
                      ymin=make_array(nframes,nsub,/float,value=0.)
                      ymax=make_array(nframes,nsub,/float,value=amp_max)
                      ymin(*,2)=0.4 & ymax(*,2)=1.2
                      ys=make_array(nsub,npts,/float,/nozero) 
                      ys(0,*)=amps & ys(1,*)=phas & ys(2,*)=cohs
                    end

     else: begin 
       print,'*** plotting of ',y_vars,' not supported !'
       return,-1
     endelse
endcase
;
; if plotting phases, unwrap them for each frame
;
if y_vars eq 'amp,pha' or y_vars eq 'pha' then begin 
  fir_ind=0 & if y_vars eq 'amp,pha' then fir_ind=1
  for i=0,n_elements(distinct_frames)-1 do begin
    js=where(frames eq distinct_frames(i))
    yt=reform(ys(fir_ind,js),n_elements(js))
    if not keyword_set(no_unwrap) then result=uti_pha_unwrap(yt,smooth=wsmooth,ramp=ramp)
    ys(fir_ind,js)=yt
  endfor
  j=[where(ys[fir_ind,*] ne !BAD_VALUE)]
    uti_range_even,min([ys(fir_ind,j)]),max([ys(fir_ind,j)]),10.,ymi,yma
    ymin(*,fir_ind)=ymi & ymax(*,fir_ind)=yma
endif

  sub_fxmin=make_array(nsub,/float,value=0.) 
  sub_fxmax=make_array(nsub,/float,value=1.)
  xmin=make_array(nsub,/float,value=xmin)
  xmax=make_array(nsub,/float,value=xmax)

;
;  do baseline-based or telescope-based gain fit
;

yfs=make_array(nsub-1,npts,/float)

refit:
if y_vars eq 'amp,pha' or y_vars eq 'pha' then begin 
  fir_ind=0 & if y_vars eq 'amp,pha' then fir_ind=1
  j=[where(ys[fir_ind,*] ne !BAD_VALUE)]
    uti_range_even,min([ys(fir_ind,j)]),max([ys(fir_ind,j)]),10.,ymi,yma
    ymin(*,fir_ind)=ymi & ymax(*,fir_ind)=yma
endif

if (e.debug) then print, 'DT_SMOOTH  ', dt_smooth 

if (connect) then begin

  dt_smooth=-0.5

  result=gain_fit(tel_bsl,'connect',dt_smooth,x_var,y_vars,frames, $
               distinct_frames,xs,ys,wts,yfs)

endif else begin

  if (dt_smooth gt 0) then begin

    result=gain_fit(tel_bsl,'smooth',dt_smooth,x_var,y_vars,frames, $
                 distinct_frames,xs,ys,wts,yfs)

  endif

  if (dt_smooth le 0) then begin

    result=gain_fit(tel_bsl,'poly',dt_smooth,x_var,y_vars,frames, $
                 distinct_frames,xs,ys,wts,yfs)

  endif

endelse

if y_vars eq 'complex' then y_vars='amp,pha'

;
; save the original parameters
;
npts = n_elements(xs)
saved_par={xmin:pan_xmin,xmax:pan_xmax,ymin:pan_ymin,ymax:pan_ymax, $
   pan_dx:pan_dx,pan_dy:pan_dy,row:row,col:col,data_xmin:xmin, $
   data_xmax:xmax,data_ymin:ymin,data_ymax:ymax, $
   nor_xmin:nor_xmin,nor_xmax:nor_xmax,nor_ymin:nor_ymin, $
   nor_ymax:nor_ymax,nframes:nframes,frames_per_page:frames_per_page, $
   nrow:nrow,ncol:ncol}
gai_par={saved_par:saved_par,nor_xmax:nor_xmax,nor_ymax:nor_ymax, $
     nor_dx:nor_dx,nor_dy:nor_dy,frames:frames, $
     distinct_frames:distinct_frames,colors:colors, $
     distinct_colors:distinct_colors,symbols:symbols, $
     distinct_symbols:distinct_symbols,pan_xmin:pan_xmin, $
     pan_xmax:pan_xmax,pan_ymin:pan_ymin,pan_ymax:pan_ymax,pan_dx:pan_dx, $
     pan_dy:pan_dy,sub_fxmin:sub_fxmin,sub_fxmax:sub_fxmax, $
     sub_fymin:sub_fymin,sub_fymax:sub_fymax,xmin:xmin,xmax:xmax,ymin:ymin,$
     ymax:ymax,psym:psym,row:row,col:col,plot_scale:make_array(frames_per_page,nsub,2,2,/float), $
     iframe:0,j_first:0,j_last:0, $
     frames_per_page:frames_per_page,nframes:nframes, $        
     color_index:color_index,symbol_pt_index:symbol_pt_index, $
     symbol_line_index:symbol_line_index,nsub:nsub,xs:xs, $
     x_var:x_var,y_var:y_var,ys:ys,wts:wts,nrow:nrow,ncol:ncol, $
     bottom_label:bottom_label,pt_first:pt_first,pt_npts:pt_npts, $
     multi_pt:multi_pt,initial:0, $
     m_options:'csfwrpne',control:'',$
     j_select:intarr(npts)-1L,i_select:intarr(npts),$
     x_select:dblarr(npts),y_select:dblarr(npts),$
     m_button_select:intarr(npts),n_select:0,yfs:yfs,$
     inhid_beg:inhid_beg,inhid_end:inhid_end,dt_smooth:dt_smooth}
     
gai=replicate(gai_par,1)
pl[plid].num_pages = ceil(float(gai[gindex].nframes) / float(gai[gindex].frames_per_page))
iframe=0

; START PLOTTING
;

;Plot the first page 
if (pl[plid].plot_device ne 'null') then loadct,39,/silent
j_first=iframe
j_last=min([iframe+frames_per_page-1,nframes-1])
gai[gindex].iframe=iframe & gai[gindex].j_first=j_first & gai[gindex].j_last=j_last
gai[gindex].frames_per_page=frames_per_page & gai[gindex].nframes=nframes
gai[gindex].nrow=nrow & gai[gindex].ncol=ncol
if not keyword_set(noplot) then result=plo_page(plid,gindex)
gai[gindex].color_index=color_index+150.
gai[gindex].nsub=n_elements(gai[gindex].y_var)-1
if not keyword_set(noplot) then result=plo_over_page(plid,gindex)
gai[gindex].nsub=n_elements(gai[gindex].y_var)

if not e.java then begin
;   pl[plid].plot_interact=plo_control(plid,gindex)
   if not keyword_set(noplot) then result=plo_control(plid,gindex)
   if result eq 2 then begin
      wts=gai[gindex].wts
      ys=gai[gindex].ys
;      fir_ind=where(gai[gindex].y_var eq 'pha')
;      if not keyword_set(no_unwrap) then result=uti_pha_unwrap(ys[fir_ind,*])
      pl[plid].plot_interact=1
      goto, refit 
   endif else pl[plid].plot_interact=result
   print, "EXITING FROM THE INTERACTIVE MODE"

   result=gain_store_xpol(tel_bsl,x_var,y_vars,plid,gindex,combinations=distinct_combinations)
   plo_plid_rel,plid
endif

if (keyword_set(preavg)) then begin

   pil=pil_bak & pbl=pbl_bak & psl=psl_bak & pcl=pcl_bak & prl=prl_bak

endif

return,pl[plid].num_pages

end
