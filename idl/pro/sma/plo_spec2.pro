function plo_spec2,x_var,y_vars,frame_vars,color_vars, $
     symbol_vars,frames_per_page,source=source,plid,$
     ntrim_max=ntrim_max,user_pt=user_pt,user_line=user_line, $
     unwrap=unwrap, normalize=normalize, preavg=preavg, smoothing=smoothing, intavg=intavg, pha_max=pha_max, pha_min=pha_min, print=print
;
; Spectrum (amp, phase) plotting for all sources in current filter
;
; The data can be plotted with multi-frames on a page, multi-colors,
; within a frame and multiple symbols for each color. In addition,
; each panel can have subpanels to plot amp, and pha seperately.
;
; Note : The desired plot device should be set in pl[plid].plot_device
;        before calling plo_spec, eg. pl[plid].plot_device='x' 
;
; parameters : x_var      -- variable for x-ccord :'channel', 'fsky' ,'velocity'
;              y_vars     -- variables plotted as y-coord : 
;                            'amp', 'amp,pha'
;              frame_vars -- header variable used to separate data between
;                            frames
;              color_vars -- header variable separating diff colors                
;              symbol_vars - header variable separating diff symbols
;                            (there are only 2 types of symbols :
;                             0 => (* & solid line) 1 => (+ & dash line)
;              frames_per_page -- max number of frames per page
;              source -- keywords to select a specific source
;              ntrim_max -- maximum number of channels to trim from spectra
;                           before plotting data
;		plid -- plot id - generated in IDL only
;
; result = -1 (error), 0 (no rows) >0 (number of rows)
;
; To plot spectrum average of all spectra (amp,pha) w/ separate
; panels for each baseline and color indicating the sideband
; with a maximum of 4 plots per page :
;  
; eg. : result=dat_filter(s_f,/reset) &  pl[plid].plot_device='x'
; eg. : result=plo_spec('channel','amp,pha','blcd','sb','',4,source='3c273') 
; eg. : result=plo_spec('channel','amp,pha','blcd','sb','',4) 
; eg. : result=plo_spec('fsky','amp,pha','blcd','sb','',4) 
; eg. : result=plo_spec('velocity','amp,pha','blcd','sb','',4) 
; eg. : result=plo_spec('channel','amp,pha','blcd','band','sb',4)
;
common global
common data_set
common plo

if keyword_set(print) then openw,lun,'spec.data',/get_lun
   
if not e.java then begin
  plid = plo_plid_gen()
endif

sindex = 0

pl[plid].plot_type='spe'

if not e.java then pl[plid].plot_interact=plo_init(plid,sindex)
if color_vars eq frame_vars then color_vars=''
if symbol_vars eq frame_vars or symbol_vars eq color_vars then symbol_vars=''
if x_var ne 'channel' and x_var ne 'velocity' and x_var ne 'fsky' then begin 
       print,'*** ',x_var,' not recognized x-coord !'
       return,-1
endif
;
; first set up local arrays w/ the desired amp, pha for each source 
; for the data passing through the filter
;
result=dat_list(s_l,'"band" like "s"',/reset,/no)

if keyword_set(source) then distinct_sources=source
if not keyword_set(source) then begin
  sources=c.source(in(pil).isource)
  distinct_sources=uti_distinct(c.source(in(pil).isource),nsources,/many_repeat)
endif
;
; loop through all sources which can be seen in filter or
; whatever source was specified in call statement
;
for js=0,n_elements(distinct_sources)-1 do begin
  result=dat_list(s_l,'"band" like "s" and "source" like "'+ $
                  distinct_sources(js)+'"',/reset,/no_notify)
  if result le 0 then begin
    print,'No data for spectrum of ',distinct_sources(js)
    goto, end_sources
  endif

  intlist = uti_distinct(in[pil].int)
  intmin = min(intlist)
  intmax = max(intlist)
  intflag = intmin
  if  not keyword_set(intavg) then avgint = intmax - intmin + 1 else avgint=intavg



  while (intflag le intmax) do begin

    intflag = intmin + avgint

    intgo = where(intlist ge intmin and intlist lt intflag, nintgo)

    if (nintgo gt 0) then begin

      print,"Plotting integration(s) between ",intmin, " and ",(intflag -1), " on source ",distinct_sources(js)

      result=dat_list(s_l,'"band" like "s" and "source" like "'+distinct_sources(js)+ $
                    '" and "int" ge "'+string(intmin)+'" and "int" lt "'+string(intflag)+'"',/reset,/no_notify)

      result=dat_list(s_l,/save,/no_notify)
      ;
      ; segment the data by blcd, receiver, sb, and band
      ;
      blave=1
      if strpos(frame_vars,'blcd',0) ne -1 or strpos(color_vars,'blcd',0) ne -1 then $
      blave=0
      if not blave then begin
;         bls=c.blcd(bl(pbl).iblcd)
         distinct_ibls=uti_distinct(bl[pbl].iblcd,nbls,/many_repeat)
      endif
;      recs=c.rec(bl(pbl).irec)
      distinct_irecs=uti_distinct(bl[pbl].irec,nrecs,/many_repeat)
      ;
      ; Upper Case sb and band, then find all distinct combinations
      ; of blcd, receiver, sb, and band
      ;
      distinct_isbs=uti_distinct(bl[pbl].isb,nsbs,/many_repeat)
      distinct_ipols=uti_distinct(bl[pbl].ipol,npols,/many_repeat)
      bands=strupcase(c.band(sp(psl).iband))
      distinct_bands=uti_distinct(strlowcase(bands),nbands,/many_repeat)
      distinct_ibands=make_array(nbands,/int,/nozero) 
      nchs=make_array(nbands,/int) & first_chs=make_array(nbands,/int) 
      for i=0,nbands-1 do begin
        distinct_ibands(i)=min(where(c.band eq distinct_bands(i)))
        first_chs(i)=total(nchs)
        nchs(i)=sp(psl(min(where(sp(psl).iband eq distinct_ibands(i))))).nch
      endfor
      ;
      ; get the average spectra for each of the combinations
      ;
      npt=0L
      cmps=make_array(1,/complex,/nozero) 
;      amps=make_array(1,/float,/nozero) 
;      phas=make_array(1,/float,/nozero) 
      xs=make_array(1,/float,/nozero)
      pt_first=make_array(1,/long,/nozero)
      pt_npts=make_array(1,/long,/nozero)
      frames=make_array(1,/string,/nozero)
      colors=make_array(1,/string,/nozero)
      symbols=make_array(1,/string,/nozero)
      ;
      ; in this loop we find and average all spectra in each combination
      ; of blcd, receiver, sb, and band. to do this, we :
      ; 1) find the values of each separate parameter going into the combination.
      ; 2) set the lists to select only those data which satisfy these parameters
      ; 3) use dat_get_rows to bulk average all spectra in the selected list
      ; 4) concatenate the new spectral bands into the plotting arrays
      ; 5) record in the arrays :frames,colors, and symbols the values for each band 
      ;
      a0=pil & a1=pbl & a2=psl & a3=pcl & a4=prl & index=-1L
      for i=0, nbands-1 do begin
         band=distinct_bands[i]
         ib=max(where(distinct_bands eq band))
         for j=0, nsbs-1 do begin
            sb=strupcase(c.sb[distinct_isbs[j]])
         for jp=0, npols-1 do begin
            pol=strupcase(c.pol[distinct_ipols[jp]])
            for k=0, nrecs-1 do begin
               rec=c.rec[distinct_irecs[k]]
               n=where( (bl[a1].irec eq distinct_irecs[k]) and (bl[a1].isb eq distinct_isbs[j]) and (bl[a1].ipol eq distinct_ipols[jp]) and (sp[a2].iband eq distinct_ibands[i]), count)
               if count eq 0 then goto,jump1
               if blave then begin                  
                  pil=a0[n] & pbl=a1[n] & psl=a2[n] & pcl=a3[n] & prl=a4[n]
        ;
        ; use dat_get_rows to bulk average all spectra in the selected list
        ;
        if keyword_set(normalize) then begin
           dat_get_rows,cmp,amp,pha,x,wts,first,npts,x_var,0,/list, $
                        average='all',/normalize,/amp_pha
           if keyword_set(print) then begin
              printf,lun,rec,' ',sb,' ',band
              for iprint=0,npts-1 do begin
                 printf,lun,x[iprint],amp[iprint],pha[iprint],format='(f16.7,2x,E13.4,2x,f8.1)'
              endfor
           endif       
        endif else begin
           dat_get_rows,cmp,amp,pha,x,wts,first,npts,x_var,0,/list, $
                        average='all',/amp_pha
           if keyword_set(print) then begin
              printf,lun,rec,' ',sb,' ',band
              for iprint=0,npts-1 do begin
                 printf,lun,x[iprint],amp[iprint],pha[iprint],format='(f16.7,2x,E13.4,2x,f8.1)'
              endfor
           endif       
        endelse
        index=index+1
        if npts[0] eq 0 then begin
           print,'could not average ', rec, ' ',sb, ' ',band
           goto, jump1
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
           if x_var eq 'fsky' then begin
              chavg=ceil(preavg/abs(sp[psl[0]].fres))
              if preavg lt abs(sp[psl[0]].fres) then begin
                 print,'*** WARNING ***'
                 print,'*** Keyword PREAVG in MHz is smaller than the spectral resolution !'
                 print,'*** No spectral averaging is done.'
                 print,'*** WARNING ***'
              endif
           endif else begin
              chavg=long(preavg)
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
        if keyword_set(smoothing) then begin
           smoothing=long(smoothing)
           cmp=uti_boxcar(x,cmp,smoothing)
        endif
        ;
        ; concatenate the new spectral bands into the plotting arrays
        ;
        pt_first=[pt_first,npt]
        pt_npts=[pt_npts,npts[0]]
        cmps=[cmps,cmp]
        xs=[xs,x]
        npt=npt+npts[0]
        ;
        ; setup the variables which will discriminate frames, colors and symbols
        ;
        case frame_vars of
          'source,pos'        : frame=source+' '+pos
          'blcd'              : frame=blcd
          'blcd,band'         : frame=blcd+' '+strupcase(band)
          'blcd,sb'           : frame=blcd+' '+strupcase(sb)
          'rec'               : frame=rec
          'sb'                : frame=strupcase(sb)
          'rec,sb'            : frame=rec+' '+strupcase(sb)
          'blcd,rec'          : frame=blcd+' '+rec
          'rec,band'          : frame=rec+' '+strupcase(band)
          'sb,band'           : frame=strupcase(sb)+' '+strupcase(band)
          'blcd,rec,sb'       : frame=blcd+' '+rec+' '+strupcase(sb)
          'pol'               : frame=rec+' '+strupcase(pol)
           ''                  : frame=blcd+' '+rec+' '+strupcase(sb)+ $
                                  ' '+strupcase(band)
          else: begin 
            print,'*** ',frame_vars,' invalid for frame variables !' & return,-1
          endelse
        endcase
        case color_vars of
          'blcd'         : color=blcd
          'band'         : color=strupcase(band)
          'rec'          : color=rec
          'sb'           : color=strupcase(sb)
          'rec,sb'       : color=rec+' '+strupcase(sb)
          'pol'          : color=strupcase(pol)
          ''             : color=''
          else: begin 
            print,'*** ',color_vars,' invalid for color variables !' & return,-1
          endelse
        endcase
        case symbol_vars of
          'rec'        : symbol=rec
          'sb'         : symbol=strupcase(sb)
          ''           : symbol=rec+' '+strupcase(sb)+' '+strupcase(band)
          else: begin 
            print,'*** ',symbol_vars,' invalid for symbol variables !' & return,-1
          endelse
        endcase
        frames=[frames,frame]
        colors=[colors,color]
        symbols=[symbols,symbol]
               endif else begin
                  b0=a0[n] & b1=a1[n] & b2=a2[n] & b3=a3[n] & b4=a4[n]
                  for m=0, nbls-1 do begin
                     blcd=c.blcd[distinct_ibls[m]]
                     n=where( bl[b1].iblcd eq distinct_ibls[m], count)
                     if count eq 0 then goto, jump2
                     pil=b0[n] & pbl=b1[n] & psl=b2[n] & pcl=b3[n] & prl=b4[n]
        ;
        ; use dat_get_rows to bulk average all spectra in the selected list
        ;
        if keyword_set(normalize) then begin
           dat_get_rows,cmp,amp,pha,x,wts,first,npts,x_var,0,/list, $
                        average='all',/normalize,/amp_pha
           if keyword_set(print) then begin
              printf,lun,rec,' ',sb,' ',band
              for iprint=0,npts-1 do begin
                 printf,lun,x[iprint],amp[iprint],pha[iprint],format='(f16.7,2x,E13.4,2x,f8.1)'
              endfor
           endif       
        endif else begin
           dat_get_rows,cmp,amp,pha,x,wts,first,npts,x_var,0,/list, $
                        average='all',/amp_pha
           if keyword_set(print) then begin
              printf,lun,rec,' ',sb,' ',band
              for iprint=0,npts-1 do begin
                 printf,lun,x[iprint],amp[iprint],pha[iprint],format='(f16.7,2x,E13.4,2x,f8.1)'
              endfor
           endif       
        endelse
        index=index+1
        if npts[0] eq 0 then begin
           print,'could not average ',blcd,' ',rec, ' ',sb, ' ',band
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
           if x_var eq 'fsky' then begin
              chavg=ceil(preavg/abs(sp[psl[0]].fres))
              if preavg lt abs(sp[psl[0]].fres) then begin
                 print,'*** WARNING ***'
                 print,'*** Keyword PREAVG in MHz is smaller than the spectral resolution !'
                 print,'*** No spectral averaging is done.'
                 print,'*** WARNING ***'
              endif
           endif else begin
              chavg=long(preavg)
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
        if keyword_set(smoothing) then begin
          smoothing=long(smoothing)
          cmp=uti_boxcar(x,cmp,smoothing)
        endif
        ;
        ; concatenate the new spectral bands into the plotting arrays
        ;
        pt_first=[pt_first,npt]
        pt_npts=[pt_npts,npts[0]]
        cmps=[cmps,cmp]
        xs=[xs,x]
        npt=npt+npts[0]
        ;
        ; setup the variables which will discriminate frames, colors and symbols
        ;
        case frame_vars of
          'source,pos'        : frame=source+' '+pos
          'blcd'              : frame=blcd
          'blcd,band'         : frame=blcd+' '+strupcase(band)
          'blcd,sb'           : frame=blcd+' '+strupcase(sb)
          'rec'               : frame=rec
          'sb'                : frame=strupcase(sb)
          'rec,sb'            : frame=rec+' '+strupcase(sb)
          'blcd,rec'          : frame=blcd+' '+rec
          'rec,band'          : frame=rec+' '+strupcase(band)
          'sb,band'           : frame=strupcase(sb)+' '+strupcase(band)
          'blcd,rec,sb'       : frame=blcd+' '+rec+' '+strupcase(sb)
          'blcd,rec,sb,band'  : frame=blcd+' '+rec+' '+strupcase(sb)+ $
                                  ' '+strupcase(band)
          'blcd,pol'          : frame=blcd+' '+rec+' '+strupcase(pol)
           ''                  : frame=blcd+' '+rec+' '+strupcase(sb)+ $
                                  ' '+strupcase(band)
          else: begin 
            print,'*** ',frame_vars,' invalid for frame variables !' & return,-1
          endelse
        endcase
        case color_vars of
          'blcd'         : color=blcd
          'band'         : color=strupcase(band)
          'rec'          : color=rec
          'sb'           : color=strupcase(sb)
          'rec,sb'       : color=rec+' '+strupcase(sb)
          'pol'          : color=strupcase(pol)
          ''             : color=''
          else: begin 
            print,'*** ',color_vars,' invalid for color variables !' & return,-1
          endelse
        endcase
        case symbol_vars of
          'rec'        : symbol=rec
          'sb'         : symbol=strupcase(sb)
          ''           : symbol=rec+' '+strupcase(sb)+' '+strupcase(band)
          else: begin 
            print,'*** ',symbol_vars,' invalid for symbol variables !' & return,-1
          endelse
        endcase
        frames=[frames,frame]
        colors=[colors,color]
        symbols=[symbols,symbol]
        jump2:
     endfor ; baselines
  endelse ; blave
  jump1:
endfor ; recs
endfor ; pols
endfor ; sbs
endfor ; bands
      ;
      ; now remove the first dummy element of the plotting arrays
      ;
      cmps=cmps[1:npt]
      xs=xs[1:npt]
      pt_first=pt_first[1:index+1]
      pt_npts=pt_npts[1:index+1]
      frames=frames[1:n_elements(frames)-1]
      colors=colors[1:n_elements(colors)-1]
      symbols=symbols[1:n_elements(symbols)-1]
      multi_pt=1
      distinct_frames=uti_distinct(frames,nframes,/many_repeat)
      distinct_colors=uti_distinct(colors,ncolors,/many_repeat)
      distinct_symbols=uti_distinct(symbols,nsymbols,/many_repeat)
      color_index=0.6*bytscl((indgen(max([n_elements(distinct_colors),1])) $
                mod 16)+1)+64.
      ;
      ; use statement below for grey scale displays
      ; 
      ;color_index=make_array(n_elements(distinct_colors),/int,value=223)

      case 1 of
        n_elements(distinct_symbols) eq 1 : $
          begin
            symbol_pt_index=1 & symbol_line_index=0
          end
        n_elements(distinct_symbols) eq 2 : $
          begin
            symbol_pt_index = n_elements(user_pt) eq 2 ? user_pt : [2,1]
            symbol_line_index = n_elements(user_line) eq 2 ? user_line : [0,1]
            ; symbol_pt_index=[1,2] & symbol_line_index=[0,1]
          end
        else : begin
                 symbol_pt_index=make_array(n_elements(distinct_symbols),/int,value=2)
                 symbol_line_index=make_array(n_elements(distinct_symbols), $       
                                              /int,value=0)
               endelse
      endcase
      ;
      ; find data ranges, plot sizes and number of col's and rows
      ; 
      bottom_label='track : '+strtrim(string(in(pil(0)).traid),2)+ $
                   ' '+distinct_sources(js)

      if y_vars eq 'complex' then begin
         amps=float(cmps)
         phas=imaginary(cmps)
         pha_max=max(phas)
         pha_min=min(phas)
         amp_min=min(amps)
      endif else begin
         uti_conv_apc,cmps,amps,phas,/amp_pha
      endelse
      npts=n_elements(xs)
      xmin=min(xs) & xmax=max(xs)
      dx=xmax-xmin & xmin=xmin-0.05*dx & xmax=xmax+0.05*dx
      ymin=0. & amp_max=max(amps)
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
             '              colors ',n_elements(distinct_symbols),' symbols '
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
      ;
      ; put amp and pha in ys array
      ;
      nsub=0
      case y_vars of
        'amp'        : begin
                         nsub=1 
                         y_var=['amp'] & psym=10
                         sub_fymin=0. & sub_fymax=1.
                         ymin=make_array(nframes,nsub,/float,value=0.)
                         ymax=make_array(nframes,nsub,/float,value=amp_max)
                         ys=make_array(nsub,npts,/float,/nozero) 
                         ys(0,*)=amps
                       end
        'pha'        : begin
                         nsub=1 
                         y_var=['pha'] & psym=3
                         sub_fymin=0. & sub_fymax=1.
                         ymin=make_array(nframes,nsub,/float,value=-180.)
                         ymax=make_array(nframes,nsub,/float,value=180.)
                         ys=make_array(nsub,npts,/float,/nozero) 
                         ys(0,*)=phas
                       end                         
        'amp,pha'    : begin
                         nsub=2 
                         y_var=['amp','pha'] & psym=[10,1]
                         sub_fymin=make_array(nsub,/float,value=0.) 
                         sub_fymax=make_array(nsub,/float,value=1.)
                         sub_fymax(1)=0.3 & sub_fymin(0)=0.3  
                         ymin=make_array(nframes,nsub,/float,value=0.)
                         ymax=make_array(nframes,nsub,/float,value=amp_max)
                         if not keyword_set(pha_max) then pha_max=190.
                         if not keyword_set(pha_min) then pha_min=-190.
                         ymin(*,1)=pha_min & ymax(*,1)=pha_max
                         ys=make_array(nsub,npts,/float,/nozero) 
                         ys(0,*)=amps & ys(1,*)=phas 
                      end
        'complex'    : begin
                         nsub=2 
                         y_var=['real','imaginary'] & psym=[10,1]
                         sub_fymin=make_array(nsub,/float,value=0.) 
                         sub_fymax=make_array(nsub,/float,value=1.)
                         sub_fymax(1)=0.3 & sub_fymin(0)=0.3  
                         ymin=make_array(nframes,nsub,/float,value=amp_min)
                         ymax=make_array(nframes,nsub,/float,value=amp_max)
                         if not keyword_set(pha_max) then pha_max=190.
                         if not keyword_set(pha_min) then pha_min=-190.
                         ymin(*,1)=pha_min & ymax(*,1)=pha_max
                         ys=make_array(nsub,npts,/float,/nozero) 
                         ys(0,*)=amps & ys(1,*)=phas 
                      end
        else: begin 
                print,'*** plotting of ',y_vars,' not supported !'
               return,-1
        endelse
      endcase

      if keyword_set(unwrap) then begin
        if y_vars eq 'amp,pha' or y_vars eq 'pha' then begin 
          fir_ind=0 & if y_vars eq 'amp,pha' then fir_ind=1
          for i=0,n_elements(distinct_frames)-1 do begin
            is=max(where(frames eq distinct_frames(i)))
            yt=reform(ys(fir_ind,pt_first[is]:pt_first[is]+pt_npts[is]-1L),pt_npts[is])
            result=uti_pha_unwrap(yt,smooth=pt_npts[is]/16) 
            ;    if not keyword_set(no_unwrap) then result=uti_pha_unwrap(yt,smooth=pt_npts[js]/16)
            ys(fir_ind,pt_first[is]:pt_first[is]+pt_npts[is]-1L)=yt
            uti_range_even,min([yt]),max([yt]),10.,ymi,yma
            ymin(i,fir_ind)=ymi & ymax(i,fir_ind)=yma
          endfor
        endif
      endif

      sub_fxmin=make_array(nsub,/float,value=0.) 
      sub_fxmax=make_array(nsub,/float,value=1.)
      xmin=make_array(nsub,/float,value=xmin)
      xmax=make_array(nsub,/float,value=xmax)

      ;
      ; save the original parameters
      ;multi_pt
      npts = n_elements(xs)

      ;Only define these if they haven't already been defined
      saved_par={xmin:pan_xmin,xmax:pan_xmax,ymin:pan_ymin,ymax:pan_ymax, $
                 pan_dx:pan_dx,pan_dy:pan_dy,row:row,col:col,data_xmin:xmin, $
                 data_xmax:xmax,data_ymin:ymin,data_ymax:ymax, $
                 nor_xmin:nor_xmin,nor_xmax:nor_xmax,nor_ymin:nor_ymin, $
                 nor_ymax:nor_ymax, nframes:nframes, frames_per_page:frames_per_page, $
                 nrow:nrow,ncol:ncol}     
      spe_par={saved_par:saved_par,nor_xmax:nor_xmax,nor_ymax:nor_ymax, $
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
               multi_pt:multi_pt,initial:0, $
               m_options:'csfpne',control:'', $
               j_select:intarr(npts),i_select:intarr(npts),$
               x_select:dblarr(npts),y_select:dblarr(npts),$
               m_button_select:intarr(npts),n_select:0}

      spe=replicate(spe_par,1)

      iframe=0 
      if (pl[plid].plot_device ne 'null') then loadct,39,/silent
      j_first=iframe
      j_last=min([iframe+frames_per_page-1,nframes-1])
      spe[sindex].iframe=iframe & spe[sindex].j_first=j_first & spe[sindex].j_last=j_last
      spe[sindex].frames_per_page=frames_per_page & spe[sindex].nframes=nframes
      spe[sindex].nrow=nrow & spe[sindex].ncol=ncol
      result=plo_page(plid,sindex)
      ; if strupcase(!d.name ne 'PS') then result=plo_zbuf(java=e.java)
      spe[sindex].m_options='cspne'
      if not e.java then begin
        pl[plid].plot_interact=plo_control(plid,sindex)
        if not pl[plid].plot_interact then goto, done
      endif else begin
        goto, done
      endelse

    endif

    intmin = intflag

  endwhile

end_sources:
endfor
done:

pl[plid].num_pages = ceil(float(spe[sindex].nframes) / float(spe[sindex].frames_per_page))
if not e.java then plo_plid_rel,plid

if keyword_set(print) then free_lun,lun

result=dat_list(s_l,/reset)
return,pl[plid].num_pages
end
