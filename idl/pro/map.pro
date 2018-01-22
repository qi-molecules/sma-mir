; *************************************************************************
; FUNCTION
;      map
;
; WRITTEN
;      ????? by Nick Scoville
;      ????? by Kazushi Sakomoto
;      November 5, 1998 - fixed bugs, added error checking statements,
;                         and converted several loops into vector
;                         operations (JMC)
;
; PURPOSE
;      Map the uv data to get a dirty image
;
; INPUTS
;      nogrid  : Keyword to do only crude gridding
;      pixel_size: Pixel size (arcsec) , if not set, use
;                pixel size of 1/4 of highest ang. res. data
;      npix    : Number of pixels (if not set, use 128 pixels)
;      notaper : Keyword not to use uvtaper
;      uvtaper : uvtaper in kilolambda
;      uniform : Keyword for uniform weighting (otherwiese natural)
;
; OUTPUT
;      -1   Calibration failed.
;       1   Calibrated WLM corrections applied successfully
;
; CALLED BY
;      wlm
;
; EXAMPLES
;      res=map(pix=1.,/nogrid,/notaper)   Simplest & fastest.
;                                         notaper, natural weighting, pillbox 
;                                         convolution. To simulate this in 
;                                         AIPS, use CELLSIZE=1, IMSIZE=128
;                                         UVWTFN='NA', XTYPE=1,YTYPE=1
;      res=map(pix=1.,/nogrid,/notaper,/uniform)
;      res=map(pix=1.,/notaper)           A bit slow.
;                                         notaper, natural weighting, gaussian
;                                         To simulate this in AIPS, use 
;                                         CELLSIZE=1, IMSIZE=128 
;                                         UVWTFN='NA', XTYPE=2,YTYPE=2
;
; *************************************************************************

function map,nogrid=nogrid,pixel_size=pixel_size,npixels=npixels,$
             notaper=notaper,uvtaper=uvtaper,uniform=uniform,$
             image=image,beam=beam,udata=u,vdata=v

   ; Common blocks
     common global
     common data_set
     common plo
     common map_page,nor_xmax,nor_ymax,nor_dx,nor_dy, $
                pan_xmin,pan_xmax,pan_ymin,pan_ymax,pan_dx,pan_dy, $
                sub_fxmin,sub_fxmax,sub_fymin,sub_fymax, $
                xmin,xmax,ymin,ymax,psym,row,col,plot_scale

   ; Initialize plot control
     plid = plo_gen_plid()
     pl[plid].plot_interact=plo_init(plid)

   ; Set contour levels
levels=[-1.-0.6,-0.2,0.2,0.6,1.0]
     levels=[-1.,-0.9,-0.8,-0.7,-0.6,-0.5,-0.4,-0.3,-0.2,-0.1, $
             0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.]
     nlev=n_elements(levels)
     print,' '
     print,"Contour levels      : 10%"

   ; Store u,v,w, wt, and complex vis in vectors. u,v,w are in [lambda]
     u=1000.*bl(pbl).u 
     v=1000.*bl(pbl).v 
     w=1000.*bl(pbl).w
     wt=sp(psl).wt
     uti_conv_apc,vis,bl(pbl).ampave,bl(pbl).phaave,/complex 
     npts=n_elements(u)
     print,"Visibilities read   :" + strcompress(npts)

   ; Conjugate
     u=[u,-u] 
     v=[v,-v] 
     w=[w,-w]
     wt=[wt,wt]/abs(min(wt))
     vis=[vis,conj(vis)]

   ; Print min/max weights
     j=where(wt gt 0., count)
     print,"Minimum weights     :" + strcompress(string(min(wt(j))))
     print,"Maximum weights     :" + strcompress(string(max(wt(j))))
     j=where(wt le 0., count)
     print,"Data pts w/ wt <= 0 :" + strcompress(count)
     print,"Maximum u, v        :" + $
        strcompress(string(max(abs(u))/1000.) + "," + $
                    string(max(abs(v))/1000.) + ' kilo-lambda')

   ; Set image size in pixels
     if keyword_set(npixels) then $
        npix = npixels $
     else $
        npix = 128
     print,"Map size            :" + strcompress(string(npix) + " x " + $
          string(npix) + " pixels")
     if (npix lt 1) then begin
        printf,-1,"Error entering image size"
        return,-1
     endif

   ; Set pixel size in arcseconds
     if not keyword_set(pixel_size) then pixel_size = min(bl(pbl).angres)/4.0
     if (pixel_size le 0.0) then begin
        printf,-1,"Error entering pixel size"
        return,-1
     endif

   ; Set reference pixels in image space
     n21=npix/2-1
     crpix = (npix-1.)/2.
     x = (findgen(npix)-crpix)*pixel_size
     y = x

   ; Set image parameters in UV space
     rad_to_sec = 3600./!dtor
     duv = rad_to_sec/(npix*pixel_size)

   ; Summarize map parameters
     print,"Angular extent      :" + strcompress(string(MIN(x)) + " to " + $
          string(MAX(x)) + " arcseconds")
     print,"Cellsize (image)    :" + strcompress(string(pixel_size) + " arcsec/pixel")
     print,"Cellsize (uv)       :" + $
             strcompress(string( (pixel_size/rad_to_sec)^2) + " steradians")
     print,"UV gridsize         :" + $
          strcompress(string(duv/1000.) + " kilo-lambda")

   ; Allocate memory for the UV grid
     ntot = 2*npts
     num_grid = make_array(npix,npix,/float)

   ; Determine pixel coordinates for each UV data point
     ud = u/duv 
     vd = v/duv
     ip = round(ud)+n21
     jp = round(vd)+n21
     l  = where(ip ge 0 and ip le npix-1L and $
                jp ge 0 and jp le npix-1L and $
                wt gt 0.0, count)
     if (count eq 0) then begin
        printf,-1,"No data points to grid"
        return,-1
     endif
     w  = where(ip ge 0 and ip le npix-1L and $
                jp ge 0 and jp le npix-1L, count_w)
     if (count_w ne ntot) then $
        printf,-1,"WARNING: Dropping ",ntot-count_w," some UV data points from grid"

   ; Grid the UV visibilities
     for i=0L,count-1L do $
       num_grid(ip(l(i)),jp(l(i))) = num_grid(ip(l(i)),jp(l(i))) + 1.

   ; For uniform weighting, set the weights inversely proportional to
   ; the number of data in the cell
     if keyword_set(uniform) then begin
       print,'Uniform weighting'
       wt(l) = wt(l)/num_grid(ip(l),jp(l))
     endif else $
       print,'Natural weighting'

   ; UV taper
     if keyword_set(notaper) then begin
        print, 'No uv-taper'
        gauss = make_array(npix,npix,/float)+1.0
     endif else begin
        if keyword_set(uvtaper) then $
          uvtaper = uvtaper*1000. $
        else $
          uvtaper=max([abs(u),abs(v)])*0.7
        print,"uvtaper sigma       :" + $
           strcompress(string(uvtaper/1000.) + " kilo-lambda")
        print,"                      " + $
          strcompress("(corresponds to UVTAPER =" + string(1.55*uvtaper/1000.)+$
             " in AIPS)")
        wt = wt*exp(-(u^2+v^2)/(2.*uvtaper^2) > (-30.))
        sigma = uvtaper/duv
        g=exp(-(findgen(npix)-crpix)^2/(2.*sigma*sigma) > (-30.))
        gauss=g#g
     endelse

   ; Convolution & resampling
     vis_grid=make_array(npix,npix,/complex)      
     sum_grid=make_array(npix,npix,/complex)
     con_grid=make_array(npix,npix,/double)
     if keyword_set(nogrid) then begin
        print,'uv-convolution with pillbox (XTYPE=YTYPE=1 in AIPS)'
        for p = 0L, count-1L do begin
          lp = l(p)
          vis_grid(ip(lp),jp(lp))=vis_grid(ip(lp),jp(lp)) + vis(lp)*wt(lp)
          sum_grid(ip(lp),jp(lp))=sum_grid(ip(lp),jp(lp)) + wt(lp)
        endfor
        con_grid(n21,n21)=1.0D00 
      endif else begin
       print,'uv-convolution with gaussian (XTYPE=YTYPE=2 in AIPS)'
       base=3         ; base (HWZP) of the convolution fn. int.
       base2=base^2
       fw=base*2+1
       unit=make_array(fw,/int,value=1.)
       imin=(ip(l)-base) > 0L
       imax=(ip(l)+base) < (npix-1L)
       jmin=(jp(l)-base) > 0L
       jmax=(jp(l)+base) < (npix-1L)
       di=ud-round(ud)+base 
       dj=vd-round(vd)+base
       for i=0L,count-1L do begin
         radius2= ((findgen(fw)-di(l(i)))^2)#unit + $
                    unit#((findgen(fw)-dj(l(i)))^2)
         wtgauss=wt(l(i))*exp(-radius2)
         wtgauss(where(radius2 gt base2))=0.0

         vis_grid(imin(i):imax(i),jmin(i):jmax(i))= $
             vis_grid(imin(i):imax(i),jmin(i):jmax(i))+ vis(l(i))*wtgauss
         sum_grid(imin(i):imax(i),jmin(i):jmax(i))= $
            sum_grid(imin(i):imax(i),jmin(i):jmax(i)) + wtgauss
       endfor

       for i=n21-base,n21+base do begin
       for j=n21-base,n21+base do begin
          rad2=(i-n21)^2+(j-n21)^2
          if rad2 lt base^2 then con_grid(i,j)= exp(-rad2)
       endfor
       endfor
     endelse

   ; Shift arrays for FFT routine
     vis_grid=shift(vis_grid,-n21,-n21)
     sum_grid=shift(sum_grid,-n21,-n21)
     con_grid=shift(con_grid,-n21,-n21)

   ; FFT to get dirty map and beam (and shift the results)
     image  =float(fft(vis_grid))
     beam   =float(fft(sum_grid))
     cfactor=float(fft(con_grid))
     cfactor=cfactor/max(cfactor)      ; normalization
     image=image/cfactor
     beam=beam/cfactor
     be_max=max(beam)
     image=shift(image,-n21,-n21)/be_max   ; shift & normalization
     beam=shift(beam,-n21,-n21)/be_max
     cfactor=shift(cfactor,-n21,-n21)

   ; Print image MIN/MAX
     print,strcompress("Image min = " + string(MIN(image)))
     print,strcompress("Image max = " + string(MAX(image)))

   ; image display
     nframes=4 & frames_per_page=4 & ncol=2 & nrow=2
     page=indgen(nframes)/frames_per_page
     frame_on_page=indgen(nframes)-page*frames_per_page
     row=frame_on_page/ncol & col=frame_on_page-row*ncol
     nor_xmin=0.1 & nor_xmax=0.95 & nor_ymin=0.1 & nor_ymax=0.94
     nor_dx=nor_xmax-nor_xmin & nor_dy=nor_ymax-nor_ymin
     separation=0.25
     pan_dx=(1.-separation)*nor_dx/ncol
     pan_dy=(1.-separation)*nor_dy/nrow
     pan_xmin=nor_xmin+col*pan_dx*(1.+separation)
     pan_xmax=nor_xmin+(col+1)*pan_dx*(1.+separation)
     pan_ymax=nor_ymax-row*pan_dy*(1.+separation)
     pan_ymin=nor_ymax-(row+1)*pan_dy*(1.+separation)
     xmin=make_array(nframes,/float,value=-npix/2) & ymin=xmin
     xmax=make_array(nframes,/float,value=npix/2) & ymax=xmax
     uvrange=max([abs(u),abs(v)])
     xmin[0:1]=-uvrange & xmax[0:1]=uvrange
     ymin[0:1]=-uvrange & ymax[0:1]=uvrange
     color_index=[75,125,150,200]
     color_index_save=color_index
     xymax=(npix-1)*pixel_size/2
     xmin(1:3)=-xymax & xmax(1:3)=xymax
     ymin(1:3)=-xymax & ymax(1:3)=xymax

   ; save the original parameters
     s_p={xmin:pan_xmin,xmax:pan_xmax,ymin:pan_ymin,ymax:pan_ymax, $
        pan_dx:pan_dx,pan_dy:pan_dy,row:row,col:col,data_xmin:xmin, $
        data_xmax:xmax,data_ymin:ymin,data_ymax:ymax, $
        nor_xmin:nor_xmin,nor_xmax:nor_xmax,nor_ymin:nor_ymin, $
        nor_ymax:nor_ymax}  

   ; Plot data
     iframe=0 
     while iframe le nframes-1 do begin
       begin_page:
         loadct,39,/silent
         erase
         if not pl[plid].plot_zoom then begin
           i_first=iframe
           i_last=min([iframe+frames_per_page-1,nframes-1])
         endif
         if strlowcase(!d.name) eq 'ps' then begin
           color_index_save=color_index
           color_index=make_array(n_elements(color_index),/float,value=0.)
         endif else begin
           if n_elements(color_index) eq 1 then color_index=70
         endelse
       
         xyouts,nor_xmax-0.1,nor_ymax+0.04,pl[plid].plot_date,alignment=0.5,/norm
         if i_first le 0 and i_last ge 0 then begin
           !p.position=[pan_xmin(0),pan_ymin(0),pan_xmin(0)+pan_dx,pan_ymin(0)+pan_dy]
           plot,[u,-u],[v,-v],linestyle=1,psym=3, /nodata, $
              xrange=[xmin(0),xmax(0)], yrange=[ymin(0),ymax(0)], $
              xstyle=1, ystyle=1, title='uv distribution',/noerase
           oplot,u,v,linestyle=1,psym=3, $
              color=color_index[0]
         endif
         if i_first le 1 and i_last ge 1 then begin
           !p.position=[pan_xmin(1),pan_ymin(1),pan_xmin(1)+pan_dx,pan_ymin(1)+pan_dy]
           contour,float(cfactor)/max(float(cfactor)),x,y,levels=levels, $
              xrange=[xmin(1),xmax(1)],yrange=[ymin(1),ymax(1)], xstyle=1, ystyle=1, $
              c_colors=color_index[1],c_linestyle=(levels lt 0.0),title='cfactor',/noerase 
         endif
         if i_first le 2 and i_last ge 2 then begin
           !p.position=[pan_xmin(2),pan_ymin(2),pan_xmin(2)+pan_dx,pan_ymin(2)+pan_dy]
           contour,beam/max([abs(beam)]),x,-y,levels=levels, $
              xrange=[xmin(2),xmax(2)],yrange=[ymin(2),ymax(2)], xstyle=1, ystyle=1, $
              c_colors=color_index[2],c_linestyle=(levels lt 0.0),title='beam',/noerase  
         endif
         if i_first le 3 and i_last ge 3 then begin
         !p.position=[pan_xmin(3),pan_ymin(3),pan_xmin(3)+pan_dx,pan_ymin(3)+pan_dy]
         contour,image/max([abs(image)]),x,-y,levels=levels, $
              xrange=[xmin(3),xmax(3)],yrange=[ymin(3),ymax(3)], xstyle=1, ystyle=1, $
              c_colors=color_index[3],c_linestyle=(levels lt 0.0),title='dirty image',/noerase
         endif
         if not pl[plid].plot_zoom then begin
           i_first=iframe
           i_last=min([iframe+frames_per_page-1,nframes-1])
         endif
;        pl[plid].plot_interact=map_control(iframe,nframes,frames_per_page, $
;            i_first,i_last,m_options='cspne')
;        if strlowcase(!d.name) eq 'ps' then color_index=color_index_save
;        if pl[plid].plot_key eq 'e' then goto, endplots
;        if pl[plid].plot_copy or pl[plid].plot_zoom then goto, begin_page
         iframe=iframe+frames_per_page
       endwhile
       endplots:

       return,1
end 
