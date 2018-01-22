function map_plot,image,beam,u,v,pixel_size

   ; Common blocks
     common global
     common data_set
     common plo
     common map_page,nor_xmax,nor_ymax,nor_dx,nor_dy, $
                pan_xmin,pan_xmax,pan_ymin,pan_ymax,pan_dx,pan_dy, $
                sub_fxmin,sub_fxmax,sub_fymin,sub_fymax, $
                xmin,xmax,ymin,ymax,psym,row,col,plot_scale

   ; Initialize
     plid = plo_plid_gen()
     pl[plid].plot_interact = plo_init(plid)

   ; Set contour levels
levels=[-1.-0.6,-0.2,0.2,0.6,1.0]
     levels=[-1.,-0.9,-0.8,-0.7,-0.6,-0.5,-0.4,-0.3,-0.2,-0.1, $
             0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.]
     nlev=n_elements(levels)

   ; Pixels
     npix = n_elements(image[0,*])
     n21=npix/2-1
     crpix = (npix-1.)/2.
     x = (findgen(npix)-crpix)*pixel_size
     y = x

   ; image display
     nframes=2 & frames_per_page=2 & ncol=2 & nrow=1
     page=indgen(nframes)/frames_per_page
     frame_on_page=indgen(nframes)-page*frames_per_page
     row=frame_on_page/ncol & col=frame_on_page-row*ncol
     nor_xmin=0.1 & nor_xmax=0.95 & nor_ymin=0.1 & nor_ymax=0.94
     nor_dx=nor_xmax-nor_xmin & nor_dy=nor_ymax-nor_ymin
     separation=0.25
     pan_dx=(1.-separation)*nor_dx/ncol
     pan_dy=(1.-separation)*nor_dy/nrow
pan_dy = pan_dx
     pan_xmin=nor_xmin+col*pan_dx*(1.+separation)
     pan_xmax=nor_xmin+(col+1)*pan_dx*(1.+separation)
     pan_ymax=nor_ymax-row*pan_dy*(1.+separation)
     pan_ymin=nor_ymax-(row+1)*pan_dy*(1.+separation)
     xmin=make_array(nframes,/float,value=-npix/2) & ymin=xmin
     xmax=make_array(nframes,/float,value=npix/2) & ymax=xmax
     color_index=[75,125,150,200]
     color_index_save=color_index
     xymax=(npix-1)*pixel_size/2
     xmin(0:1)=-xymax & xmax(0:1)=xymax
     ymin(0:1)=-xymax & ymax(0:1)=xymax

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
           contour,beam/max([abs(beam)]),x,-y,levels=levels, $
              xrange=[xmin(0),xmax(0)],yrange=[ymin(0),ymax(0)], xstyle=1, ystyle=1, $
              c_colors=color_index[0],c_linestyle=(levels lt 0.0),title='beam',/noerase  
         endif
         if i_first le 1 and i_last ge 1 then begin
         !p.position=[pan_xmin(1),pan_ymin(1),pan_xmin(1)+pan_dx,pan_ymin(1)+pan_dy]
         contour,image/max([abs(image)]),x,-y,levels=levels, $
              xrange=[xmin(1),xmax(1)],yrange=[ymin(1),ymax(1)], xstyle=1, ystyle=1, $
              c_colors=color_index[1],c_linestyle=(levels lt 0.0),title='cleaned image',/noerase
         endif
         if not pl[plid].plot_zoom then begin
           i_first=iframe
           i_last=min([iframe+frames_per_page-1,nframes-1])
         endif
         iframe=iframe+frames_per_page
       endwhile
       endplots:

       return,1
end 
