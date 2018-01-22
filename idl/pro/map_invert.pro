; *************************************************************************
; FUNCTION
;      map_invert 
;
; WRITTEN
;      Sep 19, 2000 The invert (FFT) part of mapping routine
;                   extracted from the original "maps.pro". (syl)
;              
; PURPOSE
;      Generate dirty beam and dirty map from uv data.
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
;      x,y       :  x, y (sky plane) coord.
;      u,v       :  u,v coordinates
;      factor    :  beam correction factor
;      beam      :  dirty beam
;      map       :  dirty map
;      noise     :  theoretical noise level
;      beamguess :  guess value for beam size
;
;
; EXAMPLES
;
function map_invert,x,y,u,v,cfactor,beam,map,noise,beamguess, $
                    nogrid=nogrid,pixel_size=pixel_size,npixels=npixels,$
                    notaper=notaper,uvtaper=uvtaper,uniform=uniform, $
                    speccube=speccube

common global
common data_set

if not keyword_set(nogrid) then nogrid=1
if not keyword_set(pixel_size) then pixel_size=1.
if not keyword_set(npixels) then npixels=128
if not keyword_set(notaper) then notaper=1
if not keyword_set(uvtaper) then uvtaper=0
if not keyword_set(uniform) then uniform=0
if not keyword_set(speccube) then speccube=0

;
; Set image size in pixels
;
if keyword_set(npixels) then $
      npix = npixels $
else  npix = 128
print,"Map size            :" + strcompress(string(npix) + " x " + $
       string(npix) + " pixels")
if (npix lt 1) then begin
      printf,-1,"Error entering image size"
      return,-1
endif
;
; Set pixel size in arcseconds
;
if keyword_set(pixel_size) then $
      pix = pixel_size $
else  pix = min(bl(pbl).angres)/4.0
if (pix le 0.0) then begin
      printf,-1,"Error entering pixel size"
      return,-1
endif
;
; initial guess of the beam size in arcsec using the highest resolution
;
beamguess = min(bl(pbl).angres)
;
; Set reference pixels in image space
;
n21=npix/2-1
crpix = (npix-1.)/2.
x = (findgen(npix)-crpix)*pix
y = x
;
; Set image parameters in UV space
;
rad_to_sec = 3600./!dtor
duv = rad_to_sec/(npix*pix)
;
; Summarize map parameters
;
print,"Angular extent      :" + strcompress(string(MIN(x)) + " to " + $
       string(MAX(x)) + " arcseconds")
print,"Cellsize (image)    :" + strcompress(string(pix) + " arcsec/pixel")
print,"Cellsize (uv)       :" + $
             strcompress(string( (pix/rad_to_sec)^2) + " steradians")
print,"UV gridsize         :" + $
       strcompress(string(duv/1000.) + " kilo-lambda")
;
; Allocate memory for the UV grid
;
num_grid = make_array(npix,npix,/float)

if (speccube eq 0) then begin
  ;
  ; Store u,v,w, wt, and complex vis in vectors. u,v,w are in [lambda]
  ;
  u=1000.*bl(pbl).u 
  v=1000.*bl(pbl).v 
  w=1000.*bl(pbl).w
  wt=sp(psl).wt
  uti_conv_apc,vis,bl(pbl).ampave,bl(pbl).phaave,/complex 
  npts=n_elements(u)
  ntot = 2*npts
  print,"Visibilities read   :" + strcompress(npts)
  ;
  ; Conjugate
  ;
  u=[u,-u] 
  v=[v,-v] 
  w=[w,-w]
  wtscale=abs(min(wt))
  wt=[wt,wt]/abs(min(wt))
  vis=[vis,conj(vis)]
  ;
  ; Print min/max weights
  ;
  j=where(wt gt 0., count)
  print,"Minimum weights     :" + strcompress(string(min(wt(j))))
  print,"Maximum weights     :" + strcompress(string(max(wt(j))))
  j=where(wt le 0., count)
  print,"Data pts w/ wt <= 0 :" + strcompress(count)
  print,"Maximum u, v        :" + $
  strcompress(string(max(abs(u))/1000.) + "," + $
              string(max(abs(v))/1000.) + ' kilo-lambda')
  ;
  ; Determine pixel coordinates for each UV data point
  ;
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
  ;
  ; Grid the UV visibilities
  ;
  print,'totol visibilities to grid :',count

  for i=0L,count-1L do $
      num_grid(ip(l(i)),jp(l(i))) = num_grid(ip(l(i)),jp(l(i))) + 1.
  ;
  ; For uniform weighting, set the weights inversely proportional to
  ; the number of data in the cell
  ;
  if keyword_set(uniform) then begin
        print,'Uniform weighting'
        wt(l) = wt(l)/num_grid(ip(l),jp(l))
  endif else $
        print,'Natural weighting'
  ;
  ; UV taper
  ;
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
  ;
  ; Estimate noise level for clean
  ;
  ;print,'number of wt :',n_elements(wt)
  sumwt=total(wt) * wtscale / 2
  fres_dist=uti_distinct(abs(sp(psl).fres),ndistinct,/many_repeat)
  if (ndistinct gt 1) then begin
    print,'*** Variations of channel width in the selected data? ***'
    print,'***  Please check the data or set a correct filter.   ***'
    return, 0
  endif else begin
   fres=fres_dist[0]
   print,"Channel width: ", fres, 'MHz'
   fres = fres * 1e6
  endelse
  noise = 1.0 / sqrt(sumwt*fres)
  rfreq_dist=uti_distinct(sp(psl).rfreq,ndistinct,/many_repeat)
  freq=rfreq_dist[0]
  if (freq gt 60 and freq lt 150) then k = 37.3 else $
    if (freq gt 150 and freq lt 300) then k = 65.3 else $
      begin
        k = 26.1
        print,"Don't know aperture efficiency at this frequency; assuming 100%"
     endelse
  noise = noise * k
  print,'Theoretical noise level: ',noise * 1000,'  mJy/beam'
  ;
  ; Convolution & resampling
  ;     
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
  ;
  ; Shift arrays for FFT routine
  ;
  vis_grid=shift(vis_grid,-n21,-n21)
  sum_grid=shift(sum_grid,-n21,-n21)
  con_grid=shift(con_grid,-n21,-n21)
  ;
  ; FFT to get dirty map and beam (and shift the results)
  ;
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

  map=image

endif



if (speccube eq 1) then begin
  nch = sp(psl(0)).nch
  print,'Total channels to map  :',nch
  map=make_array(npix,npix,nch)
  ; Do everything is channel-wise fashtion
    for ich = 0, nch -1L do begin
    num_grid = make_array(npix,npix,/float)
    ;
    ; Store u,v,w, wt, and complex vis in vectors. u,v,w are in [lambda]
    ;
    u=1000.*bl(pbl).u 
    v=1000.*bl(pbl).v 
    w=1000.*bl(pbl).w
    wt=sp(psl).wt
;    wt=re.wts(prl)
    vis=ch(pcl + ich + 1)
    npts=n_elements(u)
    ntot = 2*npts
    print,"Visibilities read   :" + strcompress(npts) + " for channel " + strcompress(ich+1)
;    print,n_elements(u),n_elements(u),n_elements(w),n_elements(wt),n_elements(vis)
;    print,u,v,w,wt,vis
     ;
    ; Conjugate
    ;
    u=[u,-u] 
    v=[v,-v] 
    w=[w,-w]
    wtscale=abs(min(wt))
    wt=[wt,wt]/abs(min(wt))
    vis=[vis,conj(vis)]
;    print,n_elements(u),n_elements(u),n_elements(w),n_elements(wt),n_elements(vis)
    ;
    ; Print min/max weights
    ;
    j=where(wt gt 0., count)
    if (ich eq 0) then print,"Minimum weights     :" + strcompress(string(min(wt(j))))
    if (ich eq 0) then print,"Maximum weights     :" + strcompress(string(max(wt(j))))
    j=where(wt le 0., count)
;    print,"Data pts w/ wt <= 0 :" + strcompress(count)
    if (ich eq 0) then print,"Maximum u, v        :" + $
                strcompress(string(max(abs(u))/1000.) + "," + $
                string(max(abs(v))/1000.) + ' kilo-lambda')
    ;
    ; Determine pixel coordinates for each UV data point
    ;
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
    ;
    ; Grid the UV visibilities
    ;
    if (ich eq 0) then print,'totol visibilities to grid :',count

    for i=0L,count-1L do $
        num_grid(ip(l(i)),jp(l(i))) = num_grid(ip(l(i)),jp(l(i))) + 1.
    ;
    ; For uniform weighting, set the weights inversely proportional to
    ; the number of data in the cell
    ;
    if keyword_set(uniform) then begin
;          print,'Uniform weighting'
          wt(l) = wt(l)/num_grid(ip(l),jp(l))
    endif else $
;          print,'Natural weighting'
    ;
    ; UV taper
    ;
    if keyword_set(notaper) then begin
;          print, 'No uv-taper'
          gauss = make_array(npix,npix,/float)+1.0
    endif else begin
          if keyword_set(uvtaper) then $
             uvtaper = uvtaper*1000. $
          else $
             uvtaper=max([abs(u),abs(v)])*0.7
;          print,"uvtaper sigma       :" + $
;                 strcompress(string(uvtaper/1000.) + " kilo-lambda")
;          print,"                      " + $
;                 strcompress("(corresponds to UVTAPER =" + string(1.55*uvtaper/1000.)+$
;                " in AIPS)")
          wt = wt*exp(-(u^2+v^2)/(2.*uvtaper^2) > (-30.))
          sigma = uvtaper/duv
          g=exp(-(findgen(npix)-crpix)^2/(2.*sigma*sigma) > (-30.))
          gauss=g#g
    endelse
    ;
    ; Estimate noise level for clean
    ;
    if (ich eq 0) then begin
      sumwt=total(wt) * wtscale / 2
      fres_dist=uti_distinct(sp(psl).fres,ndistinct,/many_repeat)
      if (ndistinct gt 1) then begin
        print,'*** Variations of channel width in the selected data? ***'
        print,'***  Please check the data or set a correct filter.   ***'
        return, 0
      endif else begin
        fres=abs(fres_dist[0])
        print,"Channel width: ", fres, 'MHz'
        fres = fres * 1e6
      endelse
      noise = 1.0 / sqrt(sumwt*fres)
      rfreq_dist=uti_distinct(sp(psl).rfreq,ndistinct,/many_repeat)
      freq=rfreq_dist[0]
      if (freq gt 60 and freq lt 150) then k = 37.3 else $
      if (freq gt 150 and freq lt 300) then k = 65.3 else $
        begin
          k = 26.1
          print,"Don't know aperture efficiency at this frequency; assuming 100%"
        endelse
      noise = noise * k
      print,'Theoretical noise level: ',noise * 1000,'  mJy/beam'
    endif
    ;
    ; Convolution & resampling
    ;     
    vis_grid=make_array(npix,npix,/complex)      
    if (ich eq 0 ) then begin 
        sum_grid=make_array(npix,npix,/complex)
        con_grid=make_array(npix,npix,/double)
    endif
    if keyword_set(nogrid) then begin
;          print,'uv-convolution with pillbox (XTYPE=YTYPE=1 in AIPS)'
          for p = 0L, count-1L do begin
              lp = l(p)
              vis_grid(ip(lp),jp(lp))=vis_grid(ip(lp),jp(lp)) + vis(lp)*wt(lp)
              if (ich eq 0) then sum_grid(ip(lp),jp(lp))=sum_grid(ip(lp),jp(lp)) + wt(lp)
          endfor
          if (ich eq 0) then con_grid(n21,n21)=1.0D00 
    endif else begin
;          print,'uv-convolution with gaussian (XTYPE=YTYPE=2 in AIPS)'
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
              if (ich eq 0) then sum_grid(imin(i):imax(i),jmin(i):jmax(i))= $
                  sum_grid(imin(i):imax(i),jmin(i):jmax(i)) + wtgauss
          endfor

          for i=n21-base,n21+base do begin
              for j=n21-base,n21+base do begin
                  rad2=(i-n21)^2+(j-n21)^2
                 if rad2 lt base^2 then con_grid(i,j)= exp(-rad2)
              endfor
          endfor
    endelse
    ;
    ; Shift arrays for FFT routine
    ;
    if (ich eq 0) then begin
      sum_grid=shift(sum_grid,-n21,-n21)
      con_grid=shift(con_grid,-n21,-n21)
    endif
    vis_grid=shift(vis_grid,-n21,-n21)
    ;
    ; FFT to get dirty map and beam (and shift the results)
    ;
    if (ich eq 0) then begin
      beam   =float(fft(sum_grid))
      cfactor=float(fft(con_grid))
      cfactor=cfactor/max(cfactor)      ; normalization
      beam=beam/cfactor
      be_max=max(beam)
      beam=shift(beam,-n21,-n21)/be_max
      cfactor=shift(cfactor,-n21,-n21)
    endif
    image  =float(fft(vis_grid))
    image=image/cfactor
    image=shift(image,-n21,-n21)/be_max   ; shift & normalization
    ;
    ; Print image MIN/MAX
    ;
    print,'Channel',ich+1,'  min : ',min(image), ' max : ',max(image)
;    print,strcompress("Image min = " + string(MIN(image)))
;    print,strcompress("Image max = " + string(MAX(image)))

    map[0:(npix -1L),0:(npix -1L),ich]=image

  endfor

endif

return,1

end
