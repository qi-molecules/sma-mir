function plo_cont,x_var,y_vars,cont_data,frame_vars,color_vars, $
                   symbol_vars,frames_per_page,no_unwrap=no_unwrap,plid, $
                   maxval=maxval, preavg=preavg
;
; Continuum (amp, phase, and coherence) plotting.
;
; The data can be plotted with multi-frames on a page, multi-colors,
; within a frame and multiple symbols for each color. In addition,
; each panel can have subpanels to plot amp, pha and coh seperately.
;
; Note : The desired plot device should be set in pl[plid].plot_device
;        before calling plo_cont, eg. pl[plid].plot_device='x' 
;
; parameters : x_var      -- header variable for x-ccord :'int' or 'hours'
;              y_vars     -- variables plotted as y-coord : 
;                            'amp','amp,pha,coh','amp,pha', 'tssb','wt',...
;              cont_data  -- 'int'     => integration ave 
;                            'records' => record data
;              frame_vars -- header variable used to separate data between
;                            frames
;              color_vars -- header variable separating diff colors                ;              symbol_vars - header variable separating diff symbols
;                            (there are only 2 types of symbols :
;                             0 => (* & *) 1 => (* & solid line)
;              frames_per_page -- max number of frames per page
;		         plid -- plot id - generated in IDL only
;              maxval     -- the maximum y value to plot  ; added by KS
;
; result = -1 (error), 0 (no rows) >0 (number of rows)
;
; To plot continuum int-average amp,pha,coh for the lsb w/ separate
; panels for the 3 and 1 mm receivers and color indicating the baseline
; with a maximum of 4 plots per page :  
; eg. : result=dat_filter(s_f,'"sb" eq "l"',/reset) &  pl[plid].plot_device='x'
; eg. : result=plo_cont('int','amp,pha,coh','int','rec','blcd','sb',4)
; eg. : result=plo_cont('int','amp,pha,coh','int','blcd','sb','sb',4) 
; eg. : result=plo_cont('int','amp,pha,coh','int','blcd,rec,sb,band','','',4) 
;
; Similarly to plot the continuum records,
; eg. : result=plo_cont('int','amp,pha','records','rec','blcd','sb',4) 
; or to plot the baselines on separate panels ;
; eg. : result=plo_cont('int','amp,pha','records','blcd','rec','sb',4) 
; To plot continuum records with hours as the x-axis.
; eg. : result=plo_cont('hours','amp,pha','records','rec','blcd','sb',4)
; eg. : result=plo_cont('hours','amp,pha','records','blcd','rec','sb',4)
;
common global
common data_set
common plo

if not e.java then begin
plid = plo_plid_gen()
endif

cindex = 0		;index into con struct


pl[plid].plot_type='con'
if not e.java then pl[plid].plot_interact=plo_init(plid,cindex)
if color_vars eq frame_vars then color_vars=''
if symbol_vars eq frame_vars or symbol_vars eq color_vars then symbol_vars=''
;
; first set up local arrays w/ the desired amp, pha and coh 
; for the integration average data passing through the filter
;
if dat_list(s_l,'"band" like "c"',/no_notify,/reset) le 0 then begin
  print,'No continuum plot due to lack of data (check filter).'
  return,-1
endif

case cont_data of
     'int'        : begin
          amps=bl(pbl).ampave &  phas=uti_pha_180 (bl(pbl).phaave )
          if y_vars eq 'difif' then begin
             if tag_exist(bl,'tpvar') then begin
                amps=bl[pbl].sigcoh & phas=bl[pbl].tpvar
             endif else begin
                amps=bl[pbl].blhdbl6 & phas=bl[pbl].blhdbl5
             endelse
          endif
          wts=sp(psl).wt
          tssb=sp[psl].tssb  ; %% added by KS %%
;
; plot coherence only for data amp > 0.4
;
          cohs=bl(pbl).coh
;          j=where(amps lt 0.4,count)
;          if count gt 0 then cohs(j) = !BAD_VALUE
          pt_first=0
          pt_npts=0
          multi_pt=0
          end
     'records'    : begin
          dat_get_rows,cmp,amps,phas,xs,wts,pt_first,pt_npts, $
                    x_var,0,/amp_pha,/list
          multi_pt=1
          if y_vars eq 'amp,pha,coh' then y_vars='amp,pha'
       end
     else: begin 
       print,'*** ',cont_data,' invalid selection for data type !' & return,-1
     endelse
endcase
;;;-- addition by KS
if keyword_set(maxval) then begin
   index=where(amps gt maxval,count)
   if count gt 0 then amps[index]=maxval
   index=where(wts gt maxval,count)
   if count gt 0 then wts[index]=maxval
   index=where(tssb gt maxval,count)
   if count gt 0 then tssb[index]=maxval
endif 
;; -- end of change

; remove zero wt pts
;
j=where(wts eq 0.,count)
if count gt 0 then begin
  amps[j]=!BAD_VALUE
  phas[j]=!BAD_VALUE
  if cont_data ne 'records' then cohs[j]=!BAD_VALUE
  tssb[j]=!BAD_VALUE    ; added by KS
endif

if (frame_vars eq 'sb' or frame_vars eq 'rec,sb') then begin
   allints = in[pil].int
   intlist = allints(  uniq(allints,sort(allints) ))
   nint = n_elements(intlist)
   sbs=strupcase(c.sb(bl[pbl].isb))
   frames = sbs
   if strpos(frame_vars,'rec') gt 0 then frames=frames+' '+c.rec(bl[pbl].irec)
   distinct_frames=uti_distinct(frames,nframes,/many_repeat)
   for ia=0,nframes-1L do begin
      for ib=0,nint-1L do begin
         js=where(frames eq distinct_frames(ia) and wts gt 0. and $
           in[pil].int eq intlist[ib], count)
         if (count gt 0) then begin 
            tmp_data=make_array(n_elements(js),/dcomplex)
            uti_conv_apc, tmp_data, amps[js], phas[js], /complex
            tmp_wts=wts[js]
            tmp_avg=total(tmp_data*tmp_wts)/total(tmp_wts)
            uti_conv_apc, tmp_avg, tmp_ampave, tmp_phaave, /amp_pha
            amps[js]=tmp_ampave
            phas[js]=tmp_phaave
         endif
      endfor
   endfor
endif
   

  if (keyword_set(preavg)) then begin

    print,"[PREAVG SELECTED} Calculating visibility averages"

    pil_bak=pil & pbl_bak=pbl & psl_bak=psl & pcl_bak=pcl & prl_bak=prl

;    totalints = in(pil).int
;    intlist = totalints(  uniq(totalints,sort(totalints) ))
;    nofint = n_elements(intlist)

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
     
;    gindex_from=where((intlist - shift(intlist,1)) ne 1)
;    gindex_to=where((intlist - shift(intlist,-1)) ne -1)
    gindex_mid= (gindex_from+gindex_to)/2
;    ngroup = n_elements(gindex_mid)

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
    result=uti_distinct(bl[pbl].ipol,npols,/many_repeat)
    if npols gt 1 then begin
       pols=strupcase(c.pol(bl[pbl].ipol))
       frames=bls+' '+recs+' '+sbs+' '+pols+' '+bands
    endif else begin
    frames=bls+' '+recs+' '+sbs+' '+bands
    endelse
    distinct_frames=uti_distinct(frames,nframes,/many_repeat)
    
    for ia=0,nframes-1 do begin
      for ib = 0, ngroup -1 do begin

        js=where(frames eq distinct_frames(ia) and wts gt 0. and $
                 in(pil).int ge intlist(gindex_from(ib)) and $
                 in(pil).int le intlist(gindex_to(ib)) ,count)
;        if (count le 0) then begin
;          print, "missing data for group ", ib+1, " in ", distinct_frames(ia)
;        endif else begin

;          to_mark = 0
;          ic=0
;          while (to_mark eq 0 and ic lt ngroup) do begin
;             tmp_idx = where(in(pil(js)).int eq intlist(gindex_mid(ic)),tmp_count)
;             stop,ia,ib,to_mark,ic
;             if (tmp_count ne 0) then to_mark = 1
;             ic=ic+1
;          endwhile

        if (count gt 0) then begin 
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

;        endelse
       endif

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
; segment the data by blcd, receiver, sb, and band
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
; setup the variables which will discriminate frames, colors and symbols
;
case frame_vars of
     'blcd'              : frames=bls
     'rec'               : frames=recs
     'sb'                : frames=sbs
     'band'              : frames=bands
     'rec,band'          : frames=recs+' '+bands
     'rec,sb'            : frames=recs+' '+sbs
     'blcd,rec,sb,band'  : frames=bls+' '+recs+' '+sbs+' '+bands
     ''                  : frames=bls+' '+recs+' '+sbs+' '+bands
     'source'            : frames=c.source(in(pil).isource)
     else: begin 
       print,'*** ',frame_vars,' invalid for frame variables !' & return,-1
     endelse
endcase
case color_vars of
     'blcd'         : colors=bls
     'rec'          : colors=recs
     'sb'           : colors=sbs
     'blcd,sb'      : colors=bls+' '+sbs
     'rec,sb'       : colors=recs+' '+sbs
; Qi 01/12/31 add source into color index
     'source'       : colors=c.source(in(pil).isource)
     'pol'          : colors=strupcase(c.pol[bl[pbl].ipol])
; EndOfChange
     ''             : colors=''
     else: begin 
       print,'*** ',color_vars,' invalid for color variables !' & return,-1
     endelse
endcase
case symbol_vars of
     'rec'        : symbols=recs
     'sb'         : symbols=sbs
     '0'	  : symbols=''
     '1'          : symbols=''
     ''           : symbols=''
     else: begin 
       print,'*** ',symbol_vars,' invalid for symbol variables !' & return,-1
     endelse
endcase
distinct_frames =uti_distinct(frames, /many_repeat,nframes)
distinct_colors =uti_distinct(colors, /many_repeat,ncolors)
distinct_symbols=uti_distinct(symbols,/many_repeat,nsymbols)

color_index=0.6*bytscl((indgen(max([n_elements(distinct_colors),1])) $
            mod 16)+1)+64.
;
; use statement below for grey scale displays
; 
;color_index=make_array(n_elements(distinct_colors),/int,value=223)

case 1 of
     n_elements(distinct_symbols) eq 1 : begin
          symbol_pt_index=2 & symbol_line_index=0
         end
     n_elements(distinct_symbols) eq 2 : begin
          symbol_pt_index=[2,1] & symbol_line_index=[0,1]
         end
     else : begin
          symbol_pt_index=make_array(n_elements(distinct_symbols),/int,value=2)
          symbol_line_index=make_array(n_elements(distinct_symbols), $       
                                       /int,value=0)
     endelse
endcase
;
; x-coord
;
case x_var of
     'hours' : begin
         if cont_data eq 'int' then xs=bl(pbl).avedhrs
         bottom_label='reference time :'+c.ref_time(in(pil(0)).iref_time)
         end
     'int'   : begin
         if cont_data eq 'int' then xs=in(pil).int
         bottom_label='track : '+strtrim(string(in(pil(0)).traid),2)
         end
     'ha'    : begin
         if cont_data eq 'int' then xs=in[pil].ha
         bottom_label='track : '+strtrim(string(in(pil(0)).traid),2)
         end
     'el'    : begin
         if cont_data eq 'int' then xs=in[pil].el
         bottom_label='track : '+strtrim(string(in(pil(0)).traid),2)
         end
      'prbl' : begin
         if cont_data eq 'int' then xs=bl(pbl).prbl
         bottom_label='track : '+strtrim(string(in(pil(0)).traid),2)
         end         
     else: begin 
       print,'*** ',x_var,' not recognized x-coord !'
       return,-1
     endelse
endcase
npts=n_elements(xs)
xmin=min(xs) & xmax=max(xs)
dx=xmax-xmin & xmin=xmin-0.05*dx & xmax=xmax+0.05*dx
ymin=0. & amp_max=max(amps[where(amps ne !BAD_VALUE)])
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
;; Added by Neko, 2004/05/04
case symbol_vars of
	''	:sym=1
	'0'	:sym=1
	'1'	:sym=10
endcase
;print, sym
;;
;
nsub=0
case y_vars of
     'amp'        : begin
                      nsub=1 
			y_var=['amp'] & psym=sym
;                      y_var=['amp'] & psym=10
                      sub_fymin=0. & sub_fymax=1.
                      ys=make_array(nsub,npts,/float,/nozero) 
                      ys(0,*)=amps
                      ymin=make_array(nframes,nsub,/float,value=0.)
                      ymax=make_array(nframes,nsub,/float,value=amp_max)
                    end
;; QI plot phase only, 01/12/31
     'pha'        : begin
                      nsub=1 
                      y_var=['pha'] & psym=1
                      sub_fymin=0. & sub_fymax=1.
                      ys=make_array(nsub,npts,/float,/nozero) 
                      ys(0,*)=phas
                      ymin=make_array(nframes,nsub,/float,value=-180.)
                      ymax=make_array(nframes,nsub,/float,value=180.)
                    end
;;EndOfChange
     'amp,pha'    : begin
                      nsub=2 
;
			y_var=['amp','pha'] & psym=[sym,1]
;                      y_var=['amp','pha'] & psym=[10,1]
                      sub_fymin=make_array(nsub,/float,value=0.) 
                      sub_fymax=make_array(nsub,/float,value=1.)
                      sub_fymax(1)=0.3 & sub_fymin(0)=0.3  
                      ys=make_array(nsub,npts,/float,/nozero) 
                      ys(0,*)=amps & ys(1,*)=phas 
                      ymin=make_array(nframes,nsub,/float,value=0.)
                      ymax=make_array(nframes,nsub,/float,value=amp_max)
                   end
     'difif'      : begin
                       nsub=2 
;
			y_var=['amp2/amp1','pha2-pha1'] & psym=[sym,1]
;                      y_var=['amp','pha'] & psym=[10,1]
                      sub_fymin=make_array(nsub,/float,value=0.) 
                      sub_fymax=make_array(nsub,/float,value=1.)
                      sub_fymax(1)=0.3 & sub_fymin(0)=0.3  
                      ys=make_array(nsub,npts,/float,/nozero) 
                      ys(0,*)=amps & ys(1,*)=phas 
                      ymin=make_array(nframes,nsub,/float,value=0.)
                      ymax=make_array(nframes,nsub,/float,value=amp_max)
                   end
     'amp,pha,coh': begin
                      nsub=3 
			y_var=['amp','pha','coh'] & psym=[sym,1,1]
;                      y_var=['amp','pha','coh'] & psym=[10,1,1]
                      sub_fymin=make_array(nsub,/float,value=0.) 
                      sub_fymax=make_array(nsub,/float,value=1.)
                      sub_fymin(2)=0.0 & sub_fymax(2)=0.2 
                      sub_fymin(1)=0.2 & sub_fymax(1)=0.5 
                      sub_fymin(0)=0.5 & sub_fymax(0)=1.
                      ys=make_array(nsub,npts,/float,/nozero) 
                      ys(0,*)=amps & ys(1,*)=phas & ys(2,*)=cohs
                      ymin=make_array(nframes,nsub,/float,value=0.)
                      ymax=make_array(nframes,nsub,/float,value=amp_max)
                      ymin(*,2)=0.4 & ymax(*,2)=1.2
                    end
     'amp,coh'    : begin
                      nsub=2 
			y_var=['amp','coh'] & psym=[sym,1]
;                      y_var=['amp','coh'] & psym=[10,1]
                      sub_fymin=make_array(nsub,/float,value=0.) 
                      sub_fymax=make_array(nsub,/float,value=1.)
                      sub_fymax(1)=0.3 & sub_fymin(0)=0.3
                      ys=make_array(nsub,npts,/float,/nozero) 
                      ys(0,*)=amps & ys(1,*)=cohs
                      ymin=make_array(nframes,nsub,/float,value=0.)
                      ymax=make_array(nframes,nsub,/float,value=amp_max)
                      ymin(*,1)=0.4 & ymax(*,1)=1.2
                    end
     'tssb'       : begin
                      nsub=1 
                      y_var=['Tssb'] & psym=1
                      sub_fymin=0. & sub_fymax=1.
                      ys=make_array(nsub,npts,/float,/nozero) 
                      ys(0,*)=tssb
                      tssb_max=max(tssb)*1.1
                      ymin=make_array(nframes,nsub,/float,value=0.)
                      ymax=make_array(nframes,nsub,/float,value=tssb_max)
                   end
  ;; added by KS to plot wt
     'wt'         : begin
                      nsub=1 
                      y_var=['wt'] & psym=1
                      sub_fymin=0. & sub_fymax=1.
                      ys=make_array(nsub,npts,/float,/nozero) 
                      ys(0,*)=wts
                      wts_max=max(wts)*1.1
                      ymin=make_array(nframes,nsub,/float,value=0.)
                      ymax=make_array(nframes,nsub,/float,value=wts_max)
                    end
  ;;EndOfChange
     else: begin 
       print,'*** plotting of ',y_vars,' not supported !'
       return,-1
     endelse
endcase
;
; if plotting phases, unwrap them for each combination of frame,color,symbol
; but set a single range for all pha plots but don't do for record data
;
if y_vars eq 'amp,pha' or y_vars eq 'amp,pha,coh' or y_vars eq 'pha' or y_vars eq 'difif' then begin 
  combinations=frames+colors+symbols
  distinct_combinations=uti_distinct(combinations,/many_repeat,ncbs)
  ;% End of changes
  if  cont_data eq 'int' and not keyword_set(no_unwrap) then begin 
    for i=0,n_elements(distinct_combinations)-1 do begin
      js=where(combinations eq distinct_combinations(i))
      yt=reform(ys(nsub-1,js),n_elements(js))
      result=uti_pha_unwrap(yt) & ys(nsub-1,js)=yt
    endfor
  endif
  j=[where(ys[nsub-1,*] ne !BAD_VALUE)]
  uti_range_even,min([ys(nsub-1,j)]),max([ys(nsub-1,j)]),10.,ymi,yma
  ymin(*,nsub-1)=ymi & ymax(*,nsub-1)=yma
;
;
endif
  sub_fxmin=make_array(nsub,/float,value=0.) 
  sub_fxmax=make_array(nsub,/float,value=1.)
  xmin=make_array(nsub,/float,value=xmin)
  xmax=make_array(nsub,/float,value=xmax)
;
; save the original parameters
;
saved_par={xmin:pan_xmin,xmax:pan_xmax,ymin:pan_ymin,ymax:pan_ymax, $
   pan_dx:pan_dx,pan_dy:pan_dy,row:row,col:col,data_xmin:xmin, $
   data_xmax:xmax,data_ymin:ymin,data_ymax:ymax, $
   nor_xmin:nor_xmin,nor_xmax:nor_xmax,nor_ymin:nor_ymin, $
   nor_ymax:nor_ymax,nframes:nframes,frames_per_page:frames_per_page, $
   nrow:nrow,ncol:ncol}  

con_par={saved_par:saved_par,nor_xmax:nor_xmax,nor_ymax:nor_ymax, $
     nor_dx:nor_dx,nor_dy:nor_dy,frames:frames, $
     distinct_frames:distinct_frames,colors:colors, $
     distinct_colors:distinct_colors,symbols:symbols, $
     distinct_symbols:distinct_symbols,pan_xmin:pan_xmin, $
     pan_xmax:pan_xmax,pan_ymin:pan_ymin,pan_ymax:pan_ymax,pan_dx:pan_dx, $
     pan_dy:pan_dy,sub_fxmin:sub_fxmin,sub_fxmax:sub_fxmax, $
     sub_fymin:sub_fymin,sub_fymax:sub_fymax,xmin:xmin,xmax:xmax,ymin:ymin,$
     ymax:ymax,psym:psym,row:row,col:col, $
     plot_scale:make_array(frames_per_page,nsub,2,2,/float), $
     iframe:0,j_first:0,j_last:0, $
     frames_per_page:frames_per_page,nframes:nframes, $        
     color_index:color_index,symbol_pt_index:symbol_pt_index, $
     symbol_line_index:symbol_line_index,nsub:nsub,xs:xs, $
     x_var:x_var,y_var:y_var,ys:ys,wts:wts,nrow:nrow,ncol:ncol, $
     bottom_label:bottom_label,pt_first:pt_first,pt_npts:pt_npts, $
     multi_pt:multi_pt,initial:con_par.initial, $
     m_options:'csfpne',control:'', $
     j_select:intarr(npts)-1L,i_select:intarr(npts),$
     x_select:dblarr(npts),y_select:dblarr(npts),$
     m_button_select:intarr(npts),n_select:0}
con=replicate(con_par,1)

pl[plid].num_pages = ceil(float(con[cindex].nframes) / float(con[cindex].frames_per_page))
iframe=0 
loadct,39,/silent
j_first=iframe
j_last=min([iframe+frames_per_page-1,nframes-1])
con[cindex].iframe=iframe & con[cindex].j_first=j_first & con[cindex].j_last=j_last
con[cindex].frames_per_page=frames_per_page & con[cindex].nframes=nframes
con[cindex].nrow=nrow & con[cindex].ncol=ncol
result=plo_page(plid,cindex)
iframe=con[cindex].iframe+con[cindex].frames_per_page

if not e.java then begin
   pl[plid].plot_interact=plo_control(plid,cindex)
   plo_plid_rel,plid	;when plot goes away - make the plid non active
endif

result=dat_list(s_l,/reset)

return,pl[plid].num_pages
end
