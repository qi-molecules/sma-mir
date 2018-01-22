;
; NAME:
;    map_boxmax
; PURPOSE:
;    Find location of a maximum within a sub-array.
;
; CALLING SEQUENCE:
;    map_boxm, image, xcen, ycen, deltay, deltax, xmax, ymax
; INPUTS:
;    image      : Image array.
;    xcen, ycen : Center of sub-array.
;    deltax     : Half-width of sub-array.
;    deltay     : Half-height of sub-array.
;
; KEYWORD PARAMETERS:
;    ABSMAX     : Flag, if set, looks for pixel with greatest absolute value.
;
; OUTPUTS:
;    xmax, ymax : Coordinates, in image, of local maximum.
;
;    sep 18, 2000 modified from Lowell Observatory IDL library 
;                 to be used in map_clean.pro.
;
pro map_boxmax, image, xcen, ycen, deltax, deltay, xmax, ymax, ABSMAX=absmax

   imrank = size(image,/n_dimensions)

s_image = SIZE( image )

x_size = s_image[1]
y_size = s_image[2]

startx = xcen - deltax
stopx = xcen + deltax
starty = ycen - deltay
stopy = ycen + deltay

; Make selected sub-array fit into the large array.
;
startx = 0 OR (startx GE 0 AND startx LT x_size) * startx OR $
                (startx GE x_size) * (x_size - 1)
stopx  = 0 OR (stopx GE 0 AND stopx LT x_size) * stopx OR $
                (stopx GE x_size) * (x_size - 1)
starty = 0 OR (starty GE 0 AND starty LT y_size) * starty OR $
                (starty GE y_size) * (y_size - 1)
stopy  = 0 OR (stopy GE 0 AND stopy LT y_size) * stopy OR $
                (stopy GE y_size) * (y_size - 1)

if imrank eq 2 then begin
   ; Extract the sub-array.
   ;
   t = image[ startx : stopx, starty : stopy ]

   ; Take absolute value (if requested)
   IF KEYWORD_SET( absmax ) THEN t = ABS(t)

   ; Get size info.
   ;
   t_size = SIZE( t )
   t_xsize = t_size[ 1 ]

   ; Locate the local maximum.
   ;
   t1 = WHERE( t EQ MAX( t ), count )

   ; Compute the image array coordinates of the local maximum.
   ;
   IF count NE 0 THEN BEGIN
      w = t1[0] / t_xsize
      xmax = startx + t1[0] - w * t_xsize
      ymax = starty + w
   ENDIF

endif else begin
   nframes = s_image[3]
   xmax = replicate(-10,nframes)
   ymax = replicate(-10,nframes)
   for i=0,nframes-1 do begin
      t = image[ startx : stopx, starty : stopy, i ]
      IF KEYWORD_SET( absmax ) THEN t = ABS(t)
      t_size = SIZE( t )
      t_xsize = t_size[ 1 ]
      t1 = WHERE( t EQ MAX( t ), count )
      t_size = SIZE( t )
      t_xsize = t_size[ 1 ]
      t1 = WHERE( t EQ MAX( t ), count )
	   IF count NE 0 THEN BEGIN
	      w = t1[0] / t_xsize
	      xmax[i] = startx + t1[0] - w * t_xsize
	      ymax[i] = starty + w
	   ENDIF
   endfor
endelse

END



; NAME:
;   map_clean
; PURPOSE: 
;   Remove a PSF from an image via the ``clean'' algorithm.
; CALLING SEQUENCE:
;   map_clean,image,psf,xloc,yloc,maxdist,new_image,resid $
;       MAXITERS=iters,CUTOFF=cutoff,GAIN=gain,VERBOSE=verbose
; INPUTS:
;   image     - Original source image to be cleaned.
;   psf       - PSF image at same sampling resolution as image.
;   xloc      - X location of "object"
;   yloc      - Y location of "object"
;   maxdist   - Maximum distance from xyloc to look for local max.
;   iters     - Number of cleaning iterations to perform.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;   MAXITERS  - Maxinum numbers of iterations to perform. Default = 100
;   CUTOFF    - Cutoff level for terminating clean loop.
;   GAIN      - "Gain" of the clean process, the default value is 0.05 and
;                 is the scaled amount of the psf removed at each step.
;   VERBOSE   - Verbose printout of intermediate steps to the screen.  Just
;                 like display, VERBOSE=0 suppresses output, VERBOSE=n will
;                 print information every nth iteration.
; OUTPUTS:
;   new_image - Clean-ed image result.
;   resid     - Remains of the original image after clean-ed image is removed.
;
;   9/11/00, modified from the IDL library by Buie @ Lowell Observatory (syl)
;    
pro map_clean,image,psf,xloc,yloc,maxdist,deconv_image,resid, $
       MAXITERS=maxiters,CUTOFF=cutoff,GAIN=gain,VERBOSE=verbose

if not keyword_set(maxiters) then maxiters=100
if not keyword_set(cutoff) then cutoff=0
if not keyword_set(verbose) then verbose=0
if not keyword_set(gain) then gain=0.05

print, 'Clean with Hogbom algorithm'

; Set up information on the psf array.
npsf = psf / max(psf)
;   psfnorm = total(npsf)
psfnorm = 1
s=size(psf)
psfw = s[1]
psfh = s[2]
map_boxmax,npsf,psfw/2,psfh/2,psfw/2,psfh/2,psfx,psfy
;
; Set up information on the input image and 
; make a working copy when processing each channel
;
s=size(image)
dim = s[0]
imw = s[1]
imh = s[2]
   
if (dim eq 2) then nch=1
if (dim eq 3) then begin
   nch=s[3]
   deconv_image=make_array(imw,imh,nch)
   resid=make_array(imw,imh,nch)
endif

for ich = 0, nch -1L do begin
  if (dim eq 2) then work=image $
      else work = image(*,*, ich)
   
  ; Set up the new image
  new_image = fltarr(imw,imh)

  i=0L
  map_boxmax,work,xloc,yloc,maxdist,maxdist,xpos,ypos,/ABSMAX
  peak = work[xpos,ypos]

  while (abs(peak) gt cutoff and i lt maxiters) do begin

     i = i + 1
     scale_fac = peak * gain

     ; Increment the output image.
     new_image[xpos,ypos] = new_image[xpos,ypos] + scale_fac*psfnorm

     ; Determine the sub-array for subtraction
     il = max([0,xpos-psfx])
     pl = max([0,psfx-xpos])
     ir = min([imw-1,xpos+(psfw-1-psfx)])
     pr = min([psfw-1,psfx+(imw-1-xpos)])
     ib = max([0,ypos-psfy])
     pb = max([0,psfy-ypos])
     it = min([imh-1,ypos+(psfh-1-psfy)])
     pt = min([psfh-1,psfy+(imh-1-ypos)])

     ; Subtract 5% at the max location
     work[il:ir,ib:it] = work[il:ir,ib:it] - npsf[pl:pr,pb:pt]*scale_fac

     if keyword_set(verbose) then begin
        if i mod verbose eq 0 then begin
           print,'#',i,' x,y ',xpos,ypos,' sf ',scale_fac,' (',il,':',ir,',', $
                 ib,':',it,') (',pl,':',pr,',',pb,',',pt,')', $
                 peak,work[xpos,ypos], $
                 format='(a,i6.6,a,i3,1x,i3,a,f6.1,a,i3.3,a,i3.3,a,' + $
                        'i3.3,a,i3.3,a,i3.3,a,i3.3,a,i3.3,a,i3.3,a,1x,f8.5,1x,f8.5)'
        endif
     endif

  map_boxmax,work,xloc,yloc,maxdist,maxdist,xpos,ypos,/ABSMAX
  peak = work[xpos,ypos]

  endwhile
  print, 'Total number of iterations :', i,' for channel ',ich+1

  if (dim eq 2) then begin
     deconv_image = new_image 
     resid = work
  endif
  if (dim eq 3) then begin
     deconv_image(*,*,ich) = new_image 
     resid(*,*,ich) = work
  endif

endfor

end
