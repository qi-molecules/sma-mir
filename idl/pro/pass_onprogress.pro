function pass,pas_souids,x_var,y_vars,dch_smooth,frame_vars, $
		frames_per_page,plid,ntrim_max=ntrim_max, $
                funct=funct, npoly=npoly, no_unwrap=no_unwrap
;;
; Passband derivation for amp and phase with interactive fitting.
;
; The spectra on the passband calibrators will be plotted with multi-frames
; (one for each baseline, rec, spectral band,a dn sb). Each panel will have 
; subpanels to plot amp and pha seperately.
;
; Note : The desired plot device should be set in pl[plid].plot_device
;        before calling plo_spec, eg. pl[plid].plot_device='x' 
;
; parameters : pas_souids -- souids for sources to be used for passband
;              x_var      -- variable for x-ccord :'channel', 'fsky' ,'velocity'
;              y_vars     -- variables plotted as y-coord : 
;                            'amp', 'pha', and 'amp,pha'
;              dch_smooth -- number of channels to smooth the spectra
;              frame_vars -- header variable used to separate data between
;                            frames
;              frames_per_page -- max number of frames per page
;              ntrim_max       -- max number of channels to trim spectra by
;                                 when plotting and smoothing
;		plid -- plot id - generated in IDL ONLY routines
;              funct -- default='smooth', can be 'poly' for polynomial fit (KS)
;              npoly -- order of polynomial

;
; result = -1 (error), 0 (no rows) >0 (number of rows)
;
; To do a passband fit for both amp and pha with a maximum of 4 plots 
; per page :  
;  
; eg. : result=dat_filter(s_f,/reset) &  pl[plid].plot_device='x'
; eg. : result=pas_ini(use,pas_souids,all_sources,all_amps)
; eg. : result=pass(pas_souids,'channel','amp,pha',2,'blcd,rec,sb,band',4)

;
; temporary item. (used till npoly ne 0 is allowed. ks)
; 

if funct eq 'poly' then begin
  if n_elements(npoly) eq 1 then begin
    if npoly ne 0 then begin
      print,'Error! npoly ne 0 not yet implemented.'
      return, -1
    endif
  endif else begin
    print,'Error! n_elements(npoly) ne 1'
    return, -1
  endelse
endif

;
common global
common data_set
common plo

if not e.java then begin
plid = plo_plid_gen()
endif

pindex = 0

tel_bsl='baseline'
pl[plid].plot_type='pas'
if not e.java then pl[plid].plot_interact=plo_init(plid,pindex)
color_vars=''
symbol_vars=''
if x_var ne 'channel' and x_var ne 'velocity' and x_var ne 'fsky' then begin 
       print,'*** ',x_var,' not recognized x-coord !'
       return,-1
endif
;
; first set up arrays w/ the desired amp, pha for spectra 
; for the data passing through the filter
;
result=dat_list(s_l,'"band" like "s" and "wt" gt "0"',/reset,/no_notify)
inhid_beg=min([in(pil).inhid]) & inhid_end=max([in(pil).inhid])

distinct_pas_souids=uti_distinct(pas_souids,nsources,/many_repeat)
distinct_sources=strarr(nsources)
for i=0,nsources-1 do begin
   j = where(in.souid eq distinct_pas_souids[i])
   distinct_sources[i] = c.source(in(j[0]).isource)
endfor

;
; segment the data by blcd, receiver, band, and sb
;
bls=c.blcd(bl(pbl).iblcd)
recs=c.rec(bl(pbl).irec)
j=where(recs eq '1')
if max(j) ne -1 then recs(j)= '3mm' 
j=where(recs eq '2')
if max(j) ne -1 then recs(j)= '1mm'
sbs=strupcase(c.sb(bl(pbl).isb))
bands=strupcase(c.band(sp(psl).iband))
combinations=bls+' '+recs+' '+sbs+' '+bands
distinct_combinations=uti_distinct(combinations,ncombinations,/many_repeat)
distinct_bands=uti_distinct(strlowcase(bands),nbands,/many_repeat)
distinct_icombinations=make_array(ncombinations,/int,/nozero)
;
; setup channel ranges for each band
; 
nchs=make_array(ncombinations,/int) & first_chs=make_array(ncombinations,/int) 
pt_first=make_array(ncombinations,/long,value=-1L)
pt_npts=make_array(ncombinations,/long,value=-1L)
wt_tot=make_array(ncombinations,/float,value=0.)
amp_wt=make_array(ncombinations,/float,value=0.)
pha_wt=make_array(ncombinations,/float,value=0.)
rinteg_tot=make_array(ncombinations,/float,value=0.)

;
; loop through all sources which can be seen in filter or
; whatever source was specified in call statement
;
  npt=0L
  cmps=make_array(1,/complex,/nozero) 
  amps=make_array(1,/float,/nozero) 
  phas=make_array(1,/float,/nozero) 
  xs=make_array(1,/float,/nozero)
  colors=make_array(1,/string,value='')
  symbols=make_array(1,/string,value='')

for js=0,n_elements(distinct_pas_souids)-1 do begin
  result=dat_list(s_l,'"souid" eq "'+ $
                 strtrim(string(distinct_pas_souids[js]),2)+'"',/reset,/no_notify)
  result=dat_list(s_l,'"band" like "s" and "wt" gt "0"',/no_notify)
  if result le 0 then begin
    print,'No data for spectrum of ',distinct_sources(js)
    goto, end_sources
  endif
  result=dat_list(s_l,/save,/no_notify)
;
; get the average spectra for each of the combinations
;
;
; in this loop we find and average all spectra in each combination
; of blcd, receiver, sb, and band. to do this, we :
; 1) find the values of each separate parameter going into the combination.
; 2) set the lists to select only those data which satisfy these parameters
; 3) use dat_get_rows to bulk average all spectra in the selected list
; 4) sum the new spectrum into the plotting arrays
; 5) record in the arrays :frames,colors, and symbols the values for each band
; 6) divide data by sum of weights 
;
  for i=0,ncombinations-1 do begin
    result=dat_list(s_l,/restore,/no_notify)
;
;  find the values of each separate parameter going into the combination
;
    result=dat_comb_sep(distinct_combinations[i],['blcd','rec','sb','band'], $
                      codes,icodes,n_components)
    result=dat_list(s_l,'"wt" gt "0" and "iblcd" eq "'+string(icodes(0))+ $
      '" and "irec" eq "'+string(icodes(1))+'"and "isb" eq "'+ $
      string(icodes(2))+'" and "iband" eq "'+string(icodes(3))+'"',/no_notify)
    ib=max(where(distinct_bands eq codes(3)))
;
; set the weight for this spectrum to ampave*wt
; for this combination
;
    wt_source=total(bl(pbl).ampave*sp(psl).wt)
;
; use dat_get_rows to bulk average all spectra in the selected list
;
    dat_get_rows,cmp,amp,pha,x,wts,first,npts,x_var,0,/list, $
                 /normalize,average='all'
    if npts[0] eq 0 then begin
      goto, jump
    endif
    if x_var eq 'channel' then x=x+first_chs(ib)
;
; Trim the edge channels of the spectra by setting them to the edge values.
;
  ntrim = keyword_set(ntrim_max)? (npts[0] gt 2*ntrim_max ? ntrim_max : 0) : 0
  if (ntrim gt 0) then begin
     cmp[0:ntrim-1] = cmp[ntrim]
     cmp[npts[0]-ntrim:npts[0]-1] = cmp[npts[0]-ntrim-1]
  endif

;
; add the spectral band into the plotting arrays
;
    if pt_first[i] eq -1L then begin
      pt_first[i]=1+total(pt_npts > 0)
      npt=npt+npts[0]
      xs=[xs,x]
      cmps=[cmps,cmp*wt_source]
    endif else begin
      cmps[pt_first[i]:pt_first[i]+pt_npts[i]-1L]= $
           cmps[pt_first[i]:pt_first[i]+pt_npts[i]-1L]+cmp*wt_source
    endelse
    wt_tot[i]=wt_tot[i]+wt_source
    if pt_npts[i] eq -1L then pt_npts[i]=npts[0]
    if pt_npts[i] ne npts[0] then begin
      print,'number of channels changed in ',distinct_combinations[i]
      return,-1
    endif
    if xs[pt_first[i]] ne x[0] and xs[pt_first[i]+1] ne x[1] then begin
      print,'channel coordinate changed in ',distinct_combinations[i]
      return,-1
    endif
    jump:
  endfor
  end_sources:
endfor
;
; remove any combinations which had no good data
;
j_good=where(pt_npts ne -1L and wt_tot gt 0.,count)
if count ne ncombinations then begin
  distinct_combinations=distinct_combinations(j_good)
  ncombinations=n_elements(distinct_combinations)
  pt_first=pt_first[j_good]
  pt_npts=pt_npts[j_good]
  xs=xs[j_good]
  wt_tot=wt_tot[j_good]
endif
;
; now remove the first dummy element of the plotting arrays
;
xs=xs[1:npt]
cmps=cmps[1:npt]
amps=make_array(npt,/float,/nozero)
phas=make_array(npt,/float,/nozero)
pt_first=pt_first-1L
for i=0,ncombinations-1L do begin
  cmps[pt_first[i]:pt_first[i]+pt_npts[i]-1L]= $
           cmps[pt_first[i]:pt_first[i]+pt_npts[i]-1L]/wt_tot[i]
  uti_conv_apc,cmps[pt_first[i]:pt_first[i]+pt_npts[i]-1L],amp,pha,/amp_pha
  amps[pt_first[i]:pt_first[i]+pt_npts[i]-1L]=amp
  phas[pt_first[i]:pt_first[i]+pt_npts[i]-1L]=pha
endfor

multi_pt=1
frames=distinct_combinations
distinct_frames=distinct_combinations & nframes=ncombinations
distinct_colors=colors[0] & ncolors=1 
distinct_symbols=symbols[0] & nsymbols=1
color_index=0.6*bytscl((indgen(max([n_elements(distinct_colors),1])) $
            mod 16)+1)+64.
;
; use statement below for grey scale displays
; 
;color_index=make_array(n_elements(distinct_colors),/int,value=223)

symbol_pt_index=2 & symbol_line_index=0
;
; find data ranges, plot sizes and number of col's and rows
; 
bottom_label='track : '+strtrim(string(in(pil(0)).traid),2)
npts=n_elements(xs)
xmin=min(xs) & xmax=max(xs)
dx=xmax-xmin & xmin=xmin-0.05*dx & xmax=xmax+0.05*dx
amp_min=min(amps) & amp_max=max(amps) & dy=amp_max-amp_min 
amp_max=amp_max+0.05*dy & amp_min=amp_min-0.05*dy
nor_xmin=0.1 & nor_xmax=0.95 & nor_ymin=0.1 & nor_ymax=0.94
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
; put amp and pha in ys array
;
nsub=0
case y_vars of
     'amp'        : begin
                      nsub=1 
                      y_var=['amp'] & psym=10
                      sub_fymin=0. & sub_fymax=1.
                      ymin=make_array(nframes,nsub,/float,value=amp_min)
                      ymax=make_array(nframes,nsub,/float,value=amp_max)
                      ys=make_array(nsub,npt,/float,/nozero) 
                      ys(0,*)=amps
                    end
     'amp,pha'    : begin
                      nsub=2 
                      y_var=['amp','pha'] & psym=[10,1]
                      sub_fymin=make_array(nsub,/float,value=0.) 
                      sub_fymax=make_array(nsub,/float,value=1.)
                      sub_fymax(1)=0.3 & sub_fymin(0)=0.3  
                      ymin=make_array(nframes,nsub,/float,value=amp_min)
                      ymax=make_array(nframes,nsub,/float,value=amp_max)
                      ymin(*,1)=-190. & ymax(*,1)=190.
                      ys=make_array(nsub,npt,/float,/nozero) 
                      ys(0,*)=amps & ys(1,*)=phas 
                    end
     'pha'    : begin
                      nsub=1 
                      y_var=['pha'] & psym=[1]
                      sub_fymin=0. & sub_fymax=1.
                      ymin=make_array(nframes,nsub,/float,value=-190.)
                      ymax=make_array(nframes,nsub,/float,value=190.)
                      ys=make_array(nsub,npt,/float,/nozero) 
                      ys(0,*)=phas 
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
    js=max(where(frames eq distinct_frames(i)))
    yt=reform(ys(fir_ind,pt_first[js]:pt_first[js]+pt_npts[js]-1L),pt_npts[js])
; used to be     result=uti_pha_unwrap(yt,smooth=pt_npts[js]/16) 
    if not keyword_set(no_unwrap) then result=uti_pha_unwrap(yt)
    ys(fir_ind,pt_first[js]:pt_first[js]+pt_npts[js]-1L)=yt
    uti_range_even,min([yt]),max([yt]),10.,ymi,yma
    ymin(i,fir_ind)=ymi & ymax(i,fir_ind)=yma
  endfor
endif
  sub_fxmin=make_array(nsub,/float,value=0.) 
  sub_fxmax=make_array(nsub,/float,value=1.)
  xmin=make_array(nsub,/float,value=xmin)
  xmax=make_array(nsub,/float,value=xmax)

;
;  do baseline-based or telescope-based passband fit
;
yfs=make_array(nsub,npt,/float,/nozero)
result=pass_fit(tel_bsl,'smooth',dch_smooth,x_var,'amp,pha',frames, $
          distinct_frames,xs,ys,wts,pt_first,pt_npts,yfs, $
          ntrim_max=ntrim_max, npoly=npoly)


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

pas_par={saved_par:saved_par,nor_xmax:nor_xmax,nor_ymax:nor_ymax, $
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
     m_options:'cspne',control:'',$
     j_select:intarr(npts),i_select:intarr(npts),$
     x_select:dblarr(npts),y_select:dblarr(npts),$
     m_button_select:intarr(npts),n_select:0,yfs:yfs,$
     inhid_beg:inhid_beg,inhid_end:inhid_end,tel_bsl:tel_bsl}


pas=replicate(pas_par,10)
pl[plid].num_pages = ceil(float(pas[pindex].nframes) / float(pas[pindex].frames_per_page))
iframe=0
if (pl[plid].plot_device ne 'null') then loadct,39,/silent
j_first=iframe
j_last=min([iframe+frames_per_page-1,nframes-1])
pas[pindex].iframe=iframe & pas[pindex].j_first=j_first & pas[pindex].j_last=j_last
pas[pindex].frames_per_page=frames_per_page & pas[pindex].nframes=nframes
pas[pindex].nrow=nrow & pas[pindex].ncol=ncol
result=plo_page(plid,pindex)
pas[pindex].color_index=color_index+150.
pas[pindex].nsub=2
result=plo_over_page(plid,pindex)
if not e.java then begin
   pl[plid].plot_interact=plo_control(plid,pindex)
   result=pass_store(x_var,y_vars,dch_smooth,frame_vars,distinct_sources,plid,pindex)
   plo_plid_rel,plid
endif

return,pl[plid].num_pages
end
