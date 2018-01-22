; *************************************************************************
; FUNCTION
;      plo_map (originally maps)
;
; WRITTEN
;      ????? by Nick Scoville
;      ????? by Kazushi Sakomoto
;      November 5, 1998 - fixed bugs, added error checking statements,
;                         and converted several loops into vector
;                         operations (JMC)
;      Aug 10, 2000  (use con structure temparorily as map structure)
;                    (map struct is in common block, but no map_par)
;                    to add use-interactive features (syl)
;      Sep 18, 2000  added in deconvolution (clean) and restoring
;                    capabilities (syl)
;      Sep 19, 2000 rename to plo_map. break computing parts into
;                   map_invert, map_clean, and map_restore
;              
; PURPOSE
;      Generate simple maps for plotting in MIR
;
; INPUTS
;      nogrid  : Keyword to do only crude gridding
;      pix     : Pixel size (arcsec) , if not set, use
;                pixel size of 1/4 of highest ang. res. data
;      npix    : Number of pixels (if not set, use 128 pixels)
;      notaper : Keyword not to use uvtaper
;      uvtaper : uvtaper in kilolambda
;      uniform : Keyword for uniform weighting (otherwiese natural)
;      speccube: kepword for mapping a datacube
;
; OUTPUT
;      Number of pages
;
; EXAMPLES
;      res=plo_map(pix=1.,/nogrid,/notaper)   Simplest & fastest.
;                                         notaper, natural weighting, pillbox
;                                         convolution. To simulate this in
;                                         AIPS, use CELLSIZE=1, IMSIZE=128
;                                         UVWTFN='NA', XTYPE=1,YTYPE=1
;      res=plo_map(pix=1.,/nogrid,/notaper,/uniform)
;      res=plo_map(pix=1.,/notaper)           A bit slow.
;                                         notaper, natural weighting, gaussian
;                                         To simulate this in AIPS, use
;                                         CELLSIZE=1, IMSIZE=128
;                                         UVWTFN='NA', XTYPE=2,YTYPE=2
;
function plo_map,plid,nogrid=nogrid,pixel_size=pixel_size,npixels=npixels,$
             notaper=notaper,uvtaper=uvtaper,uniform=uniform,speccube=speccube

common global
common data_set
common plo

if not keyword_set(nogrid) then nogrid=1
if not keyword_set(pixel_size) then pixel_size=1.
if not keyword_set(npixels) then npixels=128
if not keyword_set(notaper) then notaper=1
if not keyword_set(uvtaper) then uvtaper=0
if not keyword_set(uniform) then uniform=0
if not keyword_set(speccube) then speccube=0
;
; initialize id for pl structure
;
if not e.java then begin
  plid = plo_plid_gen()
endif
;
;  index for map sturcture (currently borrowed from con sturcture)
;
mindex = 0
;
; Set contour levels
;
levels=[-1.,-0.9,-0.8,-0.7,-0.6,-0.5,-0.4,-0.3,-0.2,-0.1, $
        0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.]
nlev=n_elements(levels)
print,' '
print,"Contour levels      : 10%"
;
; Set up image size parameters
;
npix=npixels
pix=pixel_size
n21=npix/2-1
;
;  Invert uv data
;
result=map_invert(x,y,u,v,cfactor,beam,map,rms,beamguess, $
                  nogrid=nogrid, pixel_size=pixel_size,npixels=npixels, $
                  notaper=notaper,uvtaper=uvtaper,uniform=uniform, $
                  speccube=speccube)
;
; Print dirty map MIN/MAX
;
print,"Map(dirty)    min = " + string(MIN(map))
print,"Map(dirty)    max = " + string(MAX(map))
;
;  CLEAN (CLEAN) TEST stage
;
map_clean,map,beam,n21,n21,n21/2,deconv,resid, maxiters=10, cutoff=(rms*4)
;
; Print clean component MIN/MAX
;
print,"Deconv        min = " + string(MIN(deconv))
print,"Deconv        max = " + string(MAX(deconv))
;
; Print residual component MIN/MAX
;
print,"Residual      min = " + string(MIN(resid))
print,"Residual      max = " + string(MAX(resid))
;
;  Restore image
;
map_restore,beam,beamguess,pix,deconv,resid,cleanbeam,restore
;
; Print restored map MIN/MAX
;
print,"Restore       min = " + string(MIN(restore))
print,"Restore       max = " + string(MAX(restore))
;
; Make 0 moment map from all channels into moment
;
if (speccube eq 1) then begin
   dims=size(map)
   nch=dims[3]
   moment = total(restore,3)
endif


;
; image display
;
 if (speccube eq 0) then begin
   nframes = 8 & nchans=0
   frametitle = make_array(nframes,/string)
   frametitle(0) = 'uv coverage'
   framedata = make_array(npix,npix,nframes,/float)
   framemax = make_array(nframes,/float)
   framedata(*,*,1)=cfactor   & framemax(1)=max(cfactor)   & frametitle(1)='cfactor'
   framedata(*,*,2)=beam      & framemax(2)=max(beam)      & frametitle(2)='beam'
   framedata(*,*,3)=map       & framemax(3)=max(map)       & frametitle(3)='dirty map'
   framedata(*,*,4)=deconv    & framemax(4)=max(map)       & frametitle(4)='clean comp'
   framedata(*,*,5)=resid     & framemax(5)=max(map)       & frametitle(5)='residual'
   framedata(*,*,6)=cleanbeam & framemax(6)=max(cleanbeam) & frametitle(6)='cleanbeam'
   framedata(*,*,7)=restore   & framemax(7)=max(map)       & frametitle(7)='restore'
   color_index=[75,125,150,200,75,200,150,200]
 endif
 if (speccube eq 1) then begin
   nframes = 4+nch & nchans=nch
   frametitle = make_array(nframes,/string)
   frametitle(0) = 'uv coverage'
   framedata = make_array(npix,npix,nframes,/float)
   framemax = make_array(nframes,/float)
   framedata(*,*,1)=cfactor          & framemax(1)=max(cfactor) & frametitle(1)='cfactor'
   framedata(*,*,2)=beam             & framemax(2)=max(beam)    & frametitle(2)='beam'
   framedata(*,*,3)=moment           & framemax(3)=max(moment)  & frametitle(3)='-1 moment map'
   framedata(*,*,4:nframes-1)=restore & framemax(4:nframes-1)=max(restore)
   nchan = indgen(nch) & frametitle(4:nframes-1) = 'channel ' + string(nchan(0:nch-1) +1)
   color_index= make_array(nframes)
   color_index(0:3)=[75,125,150,200]
   color_index(4:nframes-1) = 200
 endif
 frames_per_page=4 & ncol=2 & nrow=2
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
 xymax=(npix-1)*pix/2
 xmin=make_array(nframes,/float,value=-xymax) & ymin=xmin
 xmax=make_array(nframes,/float,value=xymax) & ymax=xmax
 uvrange=max([abs(u),abs(v)])
 xmin[0]=-uvrange & xmax[0]=uvrange
 ymin[0]=-uvrange & ymax[0]=uvrange
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
         nor_dx:nor_dx,nor_dy:nor_dy,pan_xmin:pan_xmin, $
         pan_xmax:pan_xmax,pan_ymin:pan_ymin,pan_ymax:pan_ymax,pan_dx:pan_dx, $
         pan_dy:pan_dy,xmin:xmin,xmax:xmax,ymin:ymin,$
         ymax:ymax,row:row,col:col, $
         iframe:0,j_first:0,j_last:0, $
         frames_per_page:frames_per_page,nframes:nframes, $        
         color_index:color_index, $
         u:u,v:v, $
         nchans:nchans, framedata:framedata, $
         framemax:framemax, frametitle:frametitle, $
         npix:npix, $
         x:x,y:y, $
         nrow:nrow,ncol:ncol, levels:levels, $
         initial:con_par.initial, $
         m_options:'cspne',control:''}

if (speccube eq 1) then con_par.m_options = 'cspnze'
map=con_par
;
; Initialize plot type and plot mode
;
pl[plid].plot_type ='map' 
pl[plid].num_pages = ceil(float(map[mindex].nframes) / float(map[mindex].frames_per_page))
if not e.java then pl[plid].plot_interact=plo_init(plid,mindex)
;
; Plot data
;
iframe=0 
loadct,39,/silent
j_first=iframe
j_last=min([iframe+frames_per_page-1,nframes-1])
map[mindex].iframe=iframe & map[mindex].j_first=j_first & map[mindex].j_last=j_last
map[mindex].nrow=nrow & map[mindex].ncol=ncol
result=plo_page(plid,mindex)
iframe=map[mindex].iframe+map[mindex].frames_per_page
;
; Start interactive control
;    
if not e.java then begin
       pl[plid].plot_interact=plo_control(plid,mindex)
       plo_plid_rel,plid
endif

return,pl[plid].num_pages

end
