function pass_xdelay, pas_souids,x_var,y_vars,dch_smooth,frame_vars, $
		frames_per_page,plid,ntrim_max=ntrim_max, $
                no_unwrap=no_unwrap, funct=funct, npoly=npoly, $
                preavg=preavg, noplot=noplot, ramp=ramp   

common global
common data_set
common plo

if x_var ne 'channel' and x_var ne 'velocity' and x_var ne 'fsky' then begin 
   print,'*** ',x_var,' not recognized x-coord !'
   return,-1
endif
tel_bsl='baseline'


result=dat_list(s_l,'"band" like "s" and "wt" gt "0"',/reset,/no_notify)
inhid_beg=min([in(pil).inhid]) & inhid_end=max([in(pil).inhid])
distinct_pas_souids=uti_distinct(pas_souids,nsources,/many_repeat)
distinct_sources=c.source(distinct_pas_souids)
if nsources gt 1 then begin
   print,'*** This program only works for one passband source!'
   print,'*** Please reselect the passband source.'
   return,-1
endif

bls=c.blcd(bl(pbl).iblcd)
recs=c.rec(bl(pbl).irec)
pols=strupcase(c.pol(bl[pbl].ipol))
sbs=strupcase(c.sb(bl(pbl).isb))
bands=strupcase(c.band(sp(psl).iband))
combinations=bls+' '+recs+' '+sbs+' '+bands
distinct_combinations=uti_distinct(combinations,ncombinations,/many_repeat)
distinct_bls=uti_distinct(bls,nbls,/many_repeat)
distinct_recs=uti_distinct(recs,nrecs,/many_repeat)
distinct_sbs=uti_distinct(sbs,nsbs,/many_repeat)
distinct_bands=uti_distinct(bands,nbands,/many_repeat)
;distinct_combinations=distinct_sbs
;ncombinations=nsbs
distinct_ibands=make_array(nbands,/int)
distinct_ibls=make_array(nbls,/int)
distinct_irecs=make_array(nrecs,/int)
distinct_isbs=make_array(nsbs,/int)
;
; setup channel ranges for each band
; 
;pt_first=make_array(1,/long,/nozero)
;pt_npts=make_array(1,/long,/nozero)
frames=make_array(1,/string,value='')
nchs=make_array(nbands,/long) & first_chs=make_array(nbands,/long)
for i=0,nbands-1 do begin
   distinct_ibands(i)=min(where(strupcase(c.band) eq distinct_bands(i)))
;   first_chs(i)=total(nchs) ; for wideband
   nchs(i)=sp(psl(min(where(sp(psl).iband eq distinct_ibands(i))))).nch
endfor

;
; loop through all sources which can be seen in filter or
; whatever source was specified in call statement
;
npt=0L
cmps=make_array(1,/complex,/nozero) 
xs=make_array(1,/float,/nozero)
pt_first2=make_array(1,/long,/nozero)
pt_npts2=make_array(1,/long,/nozero)
colors=make_array(1,/string,value='')
symbols=make_array(1,/string,value='')

for js=0,n_elements(distinct_pas_souids)-1 do begin
   result=dat_list(s_l,'"souid" eq "'+ $
     strtrim(string(distinct_pas_souids[js]),2)+'"',/reset,/no_notify)
   result=dat_list(s_l,'"band" like "s" and "wt" gt "0" and (("ipol" eq "1") or ("ipol" eq "4"))',/no_notify)
;   result=dat_list(s_l,'"band" like "s" and "wt" gt "0" and "ipol" eq "1"',/no_notify)
   if result le 0 then begin
      print,'No data for spectrum of ',distinct_sources(js)
      goto, end_sources
   endif
      
   a0=pil & a1=pbl & a2=psl & a3=pcl & a4=prl & index=-1L
;   for i=0,nbls-1 do begin
;      blcd=distinct_bls[i]
;      distinct_ibls[i]=min(where(c.blcd eq blcd))
   for j=0, nrecs-1 do begin
      rec=distinct_recs[j]
      distinct_irecs[j]=min(where(c.rec eq rec))
      for k=0, nsbs-1 do begin
         sb=distinct_sbs[k]
         distinct_isbs[k]=min(where(c.sb eq strlowcase(sb)))
         for m=0, nbands-1 do begin
            band=distinct_bands[m]
            distinct_ibands[m]=min(where(c.band eq strlowcase(band)))
            ib=max(where(distinct_bands eq band))
            n=where( (bl[a1].irec eq distinct_irecs[j]) and (bl[a1].isb eq distinct_isbs[k]) and (sp[a2].iband eq distinct_ibands[m]), count)
            if count eq 0 then goto, jump2
            pil=a0[n] & pbl=a1[n] & psl=a2[n] & pcl=a3[n] & prl=a4[n]
;         n=where( (bl[a1].irec eq distinct_irecs[j]) and (bl[a1].isb eq distinct_isbs[k]) and (bl[a1].iblcd eq distinct_ibls[i]), count)
;            if count eq 0 then goto,jump1
;            b0=a0[n] & b1=a1[n] & b2=a2[n] & b3=a3[n] & b4=a4[n]
;            for m=0, nbands-1 do begin
;               band=distinct_bands[m]
;               distinct_ibands[m]=min(where(c.band eq strlowcase(band)))
;               ib=max(where(distinct_bands eq band))
;               n=where( sp[b2].iband eq distinct_ibands[m], count)
;               if count eq 0 then goto, jump2
;               pil=b0[n] & pbl=b1[n] & psl=b2[n] & pcl=b3[n] &
;               prl=b4[n]
            index=index+1

;            wt_source=total(bl(pbl).ampave*bl[pbl].ampave*sp(psl).wt)
            dat_get_rows,cmp,amp,pha,x,wts,first,npts,x_var,0,/list, $
                         average='all', /normalize
            if npts[0] eq 0 then begin
               goto, jump2
            endif
            if x_var eq 'channel' then begin
               x=x+first_chs(ib)
               ntrim = keyword_set(ntrim_max)? (npts[0] gt 2*ntrim_max ? ntrim_max : 0) : 0
            endif else begin
               ntrim = keyword_set(ntrim_max)? (npts[0] gt 2*floor(ntrim_max/abs(sp[psl[0]].fres)) ? floor(ntrim_max/abs(sp[psl[0]].fres)):0) :0
            endelse
            x=x[ntrim:npts[0]-ntrim-1]
            cmp=cmp[ntrim:npts[0]-ntrim-1]
            npts=npts-2*ntrim

            if keyword_set(preavg) then begin
               if x_var eq 'channel' then begin
                  chavg=long(preavg)
               endif else begin
                  chavg=ceil(preavg/abs(sp[psl[0]].fres))
               endelse
               new_npts=floor(npts/float(chavg))
               newx=make_array(new_npts,/float)
               newcmp=make_array(new_npts,/dcomplex)
               chstart=(npts-long(new_npts*chavg))/2
               chend=0
               for chcount=0,new_npts-1 do begin
                  chend=chstart+chavg-1
                  newcmp[chcount]=total(cmp[chstart:chend])/float(chavg)
                  newx[chcount]=total(x[chstart:chend])/float(chavg)
                  chstart=chend+1
               endfor
               cmp=newcmp
               x=newx
               npts=new_npts                  
            endif
            pt_first2=[pt_first2,npt]
            pt_npts2=[pt_npts2,npts[0]]
            cmps=[cmps,cmp]
            xs=[xs,x]
            npt=npt+npts[0]
            frame=sb+' '+band
            frames=[frames,frame]
            jump2:
         endfor                 ; bands
         jump1:
      endfor                    ; sbs
   endfor                       ; recs
;endfor                          ; baselines
   end_sources:
endfor
cmps=cmps[1:npt]
uti_conv_apc,cmps,amps,phas,/amp_pha
xs=xs[1:npt]

frames=frames[1:n_elements(frames)-1]
distinct_frames=uti_distinct(frames,nframes,/many_repeat)

;pt_first2=pt_first2-1L
pt_first2=pt_first2[1:index+1]
pt_npts2=pt_npts2[1:index+1]

pt_first=pt_first2
pt_npts=pt_npts2

pt_npts2=rebin(pt_npts2,index+1,nbls,/sample)
pt_npts2=reform(pt_npts2,ncombinations,/overwrite)
pt_first2=long(total(pt_npts2,/cumulative))
pt_first2=[0L,pt_first2[0:n_elements(pt_first2)-2]]
;colors=colors[1:n_elements(colors)-1]
;symbols=symbols[1:n_elements(symbols)-1]
multi_pt=1
distinct_frames=uti_distinct(frames,nframes,/many_repeat)

;***************************************************************************
; Baseline-Based Solution
;***************************************************************************

pindex = 0
pl[plid].plot_type='pas'
if not e.java then pl[plid].plot_interact=plo_init(plid,pindex)
color_vars=''
symbol_vars=''
;
; the rest of plotting setups
;

multi_pt=1
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
     'complex'    : begin
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
;; used to be     result=uti_pha_unwrap(yt,smooth=pt_npts[js]/16) 
    if not keyword_set(no_unwrap) then result=uti_pha_unwrap(yt,smooth=pt_npts[js]/16,ramp=ramp)
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
start:
ntrim_max=0
result=pass_fit(tel_bsl,funct,dch_smooth,x_var,y_vars,frames, $
          distinct_frames,xs,ys,wts,pt_first,pt_npts,yfs, $
          npoly=npoly, delay=delay, icursor=icursor)
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


pas=replicate(pas_par,1)
pl[plid].num_pages = ceil(float(pas[pindex].nframes) / float(pas[pindex].frames_per_page))
iframe=0
if (pl[plid].plot_device ne 'null') then loadct,39,/silent
j_first=iframe
j_last=min([iframe+frames_per_page-1,nframes-1])
pas[pindex].iframe=iframe & pas[pindex].j_first=j_first & pas[pindex].j_last=j_last
pas[pindex].frames_per_page=frames_per_page & pas[pindex].nframes=nframes
pas[pindex].nrow=nrow & pas[pindex].ncol=ncol
pas[pindex].nsub=2
if not keyword_set(noplot) then result=plo_page(plid,pindex)
pas[pindex].color_index=color_index+150.
if not keyword_set(noplot) then result=plo_over_page(plid,pindex)
if not e.java then begin
   if not keyword_set(noplot) then pl[plid].plot_interact=plo_control(plid,pindex)

;pt_npts=pt_npts2
;pt_first=pt_first2
;temp=npt*nbls
;xs=rebin(xs,npt,nbls,/sample)
;xs=reform(xs,temp,/overwrite)
;yfs2=make_array(nsub,temp,/float,/nozero)
;for i=0,nsub-1 do begin
;   temp2=reform(yfs[i,*])
;   temp2=rebin(temp2,npt,nbls,/sample)
;   temp2=reform(temp2,temp,/overwrite)
;   yfs2[i,*]=temp2
;endfor
;yfs=yfs2
   
;   saved_par={xmin:pan_xmin,xmax:pan_xmax,ymin:pan_ymin,ymax:pan_ymax, $
;     pan_dx:pan_dx,pan_dy:pan_dy,row:row,col:col,data_xmin:xmin, $
;     data_xmax:xmax,data_ymin:ymin,data_ymax:ymax, $
;     nor_xmin:nor_xmin,nor_xmax:nor_xmax,nor_ymin:nor_ymin, $
;     nor_ymax:nor_ymax,nframes:ncombinations,frames_per_page:frames_per_page, $
;     nrow:nrow,ncol:ncol}  

;   pas_par={saved_par:saved_par,nor_xmax:nor_xmax,nor_ymax:nor_ymax, $
;     nor_dx:nor_dx,nor_dy:nor_dy,frames:distinct_combinations, $
;     distinct_frames:distinct_combinations,colors:colors, $
;     distinct_colors:distinct_colors,symbols:symbols, $
;     distinct_symbols:distinct_symbols,pan_xmin:pan_xmin, $
;     pan_xmax:pan_xmax,pan_ymin:pan_ymin,pan_ymax:pan_ymax,pan_dx:pan_dx, $
;     pan_dy:pan_dy,sub_fxmin:sub_fxmin,sub_fxmax:sub_fxmax, $
;     sub_fymin:sub_fymin,sub_fymax:sub_fymax,xmin:xmin,xmax:xmax,ymin:ymin,$
;     ymax:ymax,psym:psym,row:row,col:col,plot_scale:make_array(frames_per_page,nsub,2,2,/float), $
;     iframe:0,j_first:0,j_last:0, $
;     frames_per_page:frames_per_page,nframes:ncombinations, $        
;     color_index:color_index,symbol_pt_index:symbol_pt_index, $
;     symbol_line_index:symbol_line_index,nsub:nsub,xs:xs, $
;     x_var:x_var,y_var:y_var,ys:ys,wts:wts,nrow:nrow,ncol:ncol, $
;     bottom_label:bottom_label,pt_first:pt_first,pt_npts:pt_npts, $
;     multi_pt:multi_pt,initial:0, $
;     m_options:'cspne',control:'',$
;     j_select:intarr(npts),i_select:intarr(npts),$
;     x_select:dblarr(npts),y_select:dblarr(npts),$
;     m_button_select:intarr(npts),n_select:0,yfs:yfs,$
;     inhid_beg:inhid_beg,inhid_end:inhid_end,tel_bsl:tel_bsl}
;   pas=replicate(pas_par,1)
;   pas[pindex].frames=distinct_combinations
;   pas[pindex].distinct_frames=distinct_combinations
;   pas[pindex].nframes=ncombinations
;   pas[pindex].saved_par.nframes=ncombinations
   result=pass_store_xdelay(x_var,y_vars,dch_smooth,frame_vars,distinct_sources,plid,pindex,combinations=distinct_combinations)
   plo_plid_rel,plid
endif

return,pl[plid].num_pages
end



