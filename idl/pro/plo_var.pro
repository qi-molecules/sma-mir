function plo_var ,x_var,y_var,symbol_var=symbol_var,frame_var=frame_var, $
                 color_var=color_var,frames_per_page=frames_per_page
;
; Plots var's in y_var vs x_var group by symbol_var in panels
; with different values of frame_var.
;
; parameters : x_var -- indep. var in plot
;              y_var -- dependent var
;  keyword     symbol_var -- grouping variable for symbols
;  keyword     frame_var -- variable used to separate panels
;  keyword     color_var -- variable used to separate colors
;  keyword     frames_per_page -- max number of frames per page

; result = -1 (error), 1(ok)
; 
; eg. : result=plo_var('int','ampave',symbol_var='blcd',frame_var='rec',color_var='rec'); eg. : result=plo_var('int','ampave',symbol_var='blcd',frame_var='rec',color_var='blcd')
;
common global
common data_set
common plo

if not e.java then begin
plid = plo_plid_gen()
endif

vindex = 0		;index into var struct

if not keyword_set(color_var) then color_var='source'
if not keyword_set(symbol_var) then symbol_var=''
if not keyword_set(frame_var) then frame_var='blcd'
if color_var eq '' then color_var=symbol_var
if symbol_var eq '' then symbol_var=color_var
if not keyword_set(frames_per_page) then frames_per_page=4

pl[plid].plot_type='var'
if not e.java then pl[plid].plot_interact=plo_init(plid,vindex)

;
; find descriptive information on all variables
;
nvar=2
var_list=[x_var,y_var]
if keyword_set(symbol_var) then begin
  var_list=[var_list,symbol_var]
endif
if keyword_set(frame_var) then begin
  var_list=[var_list,frame_var]
endif
if keyword_set(color_var) then begin
  var_list=[var_list,color_var]
endif
nvar=n_elements(var_list)
ss=make_array(nvar,/string)
stags=make_array(nvar,/string)
tags=make_array(nvar,/string)
tagnos=make_array(nvar,/int)
labels=make_array(nvar,/string)
formats=make_array(nvar,/string)
justifys=make_array(nvar,/int)
types=make_array(nvar,/string)
max_lengths=make_array(nvar,/int)
nvar=0
for i=0,n_elements(var_list)-1 do begin
  if not dat_var_info(var_list(i),s,stag,tag,tagno,pointer_str,'f',label,format, $
         justify,type,itype,description,format_long, $
         max_length,condition,c_code,i_code,/all_info) then return,-1

  for j=0L,0L do begin
    ss(nvar+j)=s(j) & stags(nvar+j)=stag(j) & types(nvar+j)=type(j) 
    tags(nvar+j)=tag(j) & tagnos(nvar+j)=tagno(j) & formats(nvar+j)=format(j)
    labels(nvar+j)=label(j) & justifys(nvar+j)=justify(j) 
    max_lengths(nvar+j)=max_length(j)
  endfor
  nvar=nvar+1
endfor
;print,type
if (types[0] ne 'int' and types[0] ne 'long' and types[0] ne 'float'  $
   and types[0] ne 'double' and types[0] ne 'complex') or (types[1] ne 'int'  $
   and types[1] ne 'long' and types[1] ne 'float' and types[1] ne 'double' $
   and types[1] ne 'complex') then begin
   print,'x and y must be numeric :',x_var,' ',type[0],'  ',y_var,' ',type[1]
   return,-1
endif 
;
; assemble complete format
;
var_str=make_array(nvar,/string)
for i=0,nvar-1L do begin
  dec_pos=strpos(formats(i),'.',0)
  if dec_pos eq -1 then dec_pos=strlen(formats(i))
  c_pos=strpos(formats(i),'a',0)
  if c_pos eq -1 then c_pos=strpos(formats(i),'g',0)
  if c_pos eq -1 then c_pos=strpos(formats(i),'f',0)
  if c_pos eq -1 then c_pos=strpos(formats(i),'i',0)
  if c_pos eq -1 then c_pos=0
  pstr='.'
  case 1 of 
    (ss(i) eq 'in') : var_str[i]='in(pil(*)).'+tags(i)
    (ss(i) eq 'bl') : var_str[i]='bl(pbl(*)).'+tags(i)
    (ss(i) eq 'sp') : var_str[i]='sp(psl(*)).'+tags(i)
    (ss(i) eq 're') : var_str[i]='re(prl(*)).'+tags(i)
    (ss(i) eq 'ch') : var_str[i]='ch(pcl(*)).'+tags(i)
    (ss(i) eq 'c')  : begin
      if c.icode_s(tagnos(i)) eq 'in' then pstr='(pil(*)).'
      if c.icode_s(tagnos(i)) eq 'bl' then pstr='(pbl(*)).'
      if c.icode_s(tagnos(i)) eq 'sp' then pstr='(psl(*)).'
      var_str[i]=var_str[i]+ss(i)+'.'+tags(i)+'('+ $
          c.icode_s(tagnos(i))+pstr+c.icode_tag(tagnos(i))+')'      
    end
  else : print,'unrecognized structure :',ss(i)
  endcase
endfor

result=execute('xs='+var_str[0])
result=execute('ys='+var_str[1])
;wts=make_array(n_elements(ys),/float,value=1.)
;help,ys
wts=sp[psl].wt
pt_first=0
pt_npts=0
multi_pt=0

var=2
if keyword_set(symbol_var) then begin
  result=execute('symbols='+var_str[var])
  distinct_symbols=uti_distinct(symbols,nsymbols,/many_repeat)
  var=var+1
endif else distinct_symbols=''
if keyword_set(frame_var) then begin
var_frame=var_str[var]
var=var+1
if n_elements(frame_var) gt 1 then begin
 for ii=1,n_elements(frame_var)-1 do begin
  var_frame=var_frame+"+' '+"+var_str[var]
  var=var+1
 endfor
endif
;  result=execute('frames='+var_str[var])
  result=execute('frames='+var_frame)
  distinct_frames=uti_distinct(frames,nframes,/many_repeat)

;  var=var+1
endif else distinct_frames=''
if keyword_set(color_var) then begin
  result=execute('colors='+var_str[var])
  distinct_colors=uti_distinct(colors,ncolors,/many_repeat)
  var=var+1
endif else distinct_colors=''
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
          symbol_pt_index=make_array(n_elements(distinct_symbols),/int,value=1)
          symbol_line_index=make_array(n_elements(distinct_symbols), $       
                                       /int,value=0)
     endelse
endcase
;
; x-coord
;
case x_var of
     'avedhrs' : begin
         bottom_label='reference time :'+c.ref_time(in(pil(0)).iref_time)
         end
     'int'   : begin
         bottom_label='track : '+strtrim(string(in(pil(0)).traid),2)
         end
     else    : begin
         bottom_label='track : '+strtrim(string(in(pil(0)).traid),2)
         end
endcase
npts=n_elements(xs)
xmin=min(xs) & xmax=max(xs)
dx=xmax-xmin & xmin=xmin-0.05*dx & xmax=xmax+0.05*dx
ymin=min(ys[where(ys ne !BAD_VALUE)]) & ymax=max(ys[where(ys ne !BAD_VALUE)])
dy=ymax-ymin & ymin=ymin-0.05*dy & ymax=ymax+0.05*dy
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
    nsub=1 
    psym=1
    sub_fymin=0. & sub_fymax=1.
    yss=ys
    ys=make_array(nsub,npts,/float,/nozero) 
    ys(0,*)=yss & yss=0.
    ymin=make_array(nframes,nsub,/float,value=ymin)
    ymax=make_array(nframes,nsub,/float,value=ymax)

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


var_par={saved_par:saved_par,nor_xmax:nor_xmax,nor_ymax:nor_ymax, $
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
     multi_pt:multi_pt,initial:var_par.initial, $
     m_options:'csfpne',control:'', $
     j_select:intarr(npts)-1L,i_select:intarr(npts),$
     x_select:dblarr(npts),y_select:dblarr(npts),$
     m_button_select:intarr(npts),n_select:0}
var=replicate(var_par,1)

pl[plid].num_pages = ceil(float(var[vindex].nframes) / float(var[vindex].frames_per_page))
iframe=0 
loadct,39,/silent
j_first=iframe
j_last=min([iframe+frames_per_page-1,nframes-1])
var[vindex].iframe=iframe & var[vindex].j_first=j_first & var[vindex].j_last=j_last
var[vindex].frames_per_page=frames_per_page & var[vindex].nframes=nframes
var[vindex].nrow=nrow & var[vindex].ncol=ncol
result=plo_page(plid,vindex)
iframe=var[vindex].iframe+var[vindex].frames_per_page

if not e.java then begin
   pl[plid].plot_interact=plo_control(plid,vindex)
   plo_plid_rel,plid	;when plot goes away - make the plid non active
endif

return,pl[plid].num_pages

  
return,1
end
