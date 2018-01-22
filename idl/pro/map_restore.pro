function convolve, image, psf, FT_PSF=psf_FT, FT_IMAGE=imFT, NO_FT=noft, $
			CORRELATE=correlate, AUTO_CORRELATION=auto
;+
; NAME:
;	CONVOLVE
; PURPOSE:
;	Convolution of an image with a Point Spread Function (PSF)
; EXPLANATION:
;	The default is to compute the convolution using a product of 
;	Fourier transforms (for speed).
;
; CALLING SEQUENCE:
;
;	imconv = convolve( image1, psf, FT_PSF = psf_FT )
;  or:
;	correl = convolve( image1, image2, /CORREL )
;  or:
;	correl = convolve( image, /AUTO )
;
; INPUTS:
;	image = 2-D array (matrix) to be convolved with psf
;	psf = the Point Spread Function, (size < or = to size of image).
;
; OPTIONAL INPUT KEYWORDS:
;
;	FT_PSF = passes out/in the Fourier transform of the PSF,
;		(so that it can be re-used the next time function is called).
;	FT_IMAGE = passes out/in the Fourier transform of image.
;
;	/CORRELATE uses the conjugate of the Fourier transform of PSF,
;		to compute the cross-correlation of image and PSF,
;		(equivalent to IDL function convol() with NO rotation of PSF)
;
;	/AUTO_CORR computes the auto-correlation function of image using FFT.
;
;	/NO_FT overrides the use of FFT, using IDL function convol() instead.
;		(then PSF is rotated by 180 degrees to give same result)
; METHOD:
;	When using FFT, PSF is centered & expanded to size of image.
; HISTORY:
;	written, Frank Varosi, NASA/GSFC 1992.
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
	sp = size( psf_FT )  &  sif = size( imFT )
	sim = size( image )  &  sc = sim/2  &  npix = N_elements( image )

	if (sim[0] NE 2) OR keyword_set( noft ) then begin
		if keyword_set( auto ) then begin
			message,"auto-correlation only for images with FFT",/INF
			return, image
		  endif else if keyword_set( correlate ) then $
				return, convol( image, psf ) $
			else	return, convol( image, rotate( psf, 2 ) )
	   endif

	if (sif[0] NE 2) OR (sif[sif[0]+1] NE 6) OR $
	   (sif[1] NE sim[1]) OR (sif[2] NE sim[2]) then imFT = FFT( image,-1 )

	if keyword_set( auto ) then $
	 return, shift( npix*float( FFT( imFT*conj( imFT ),1 ) ), sc[1],sc[2] )

	if (sp[0] NE 2) OR (sp[sp[0]+1] NE 6) OR $
	   (sp[1] NE sim[1]) OR (sp[2] NE sim[2]) then begin
		sp = size( psf )
		if (sp[0] NE 2) then begin
			message,"must supply PSF matrix (2nd arg.)",/INFO
			return, image
		   endif
		Loc = ( sc - sp/2 ) > 0		;center PSF in new array,
		s = (sp/2 - sc) > 0	   ;handle all cases: smaller or bigger
		L = (s + sim-1) < (sp-1)
		psf_FT = complexarr( sim[1], sim[2] )
		psf_FT[ Loc[1], Loc[2] ] = psf[ s[1]:L[1], s[2]:L[2] ]
		psf_FT = FFT( psf_FT, -1, /OVERWRITE )
	   endif

	if keyword_set( correlate ) then $
		conv = npix * float( FFT( imFT * conj( psf_FT ), 1 ) ) $
	  else	conv = npix * float( FFT( imFT * psf_FT, 1 ) )

	sc = sc + (sim MOD 2)	;shift correction for odd size images.

return, shift( conv, sc[1], sc[2] )
end


; NAME:
;	map_restore
; PURPOSE:
;	Restore map by forming clean beam, convolve with clean component
;       and add back residual maps
; HISTORY:
;       sep 20, 2000 syl
;
; INPUTS:
;       beam      :  dirty beam,deconv,resid, cleanbeam, restore
;       beamguess :  initial guess for the beam size
;       pix       :  pixel size
;       deconv    :  clean components
;       resid     :  residual from clean procedure
; OUTPUTS:
;       cleanbeam :  clean gaussian beam pattern
;       restore   :  restored map
;
; METHOD:
;	Use NASA convolve.pro for convolution with FFT 
;       
;
; EXAMPLE:
;       map_restore,beam,beamguess,deconv,resid,cleanbeam,restore
;
;***************************************************************************
;
pro map_restore,beam,beamguess,pix,deconv,resid,cleanbeam,restore

;
;  Forming clean beam
;
print,'Forming clean beam'
print,'Guessing clean beam FWHM is ', beamguess, ' arcsecs'
beamguess = beamguess / pix
fitarea = fix (2*beamguess)
cleanbeam = map_beamfit(beam, beamguess, beampar, fitarea=fitarea ) 
beampar(0) = beampar(0) * pix
beampar(1) = beampar(1) * pix
if (beampar(0) gt beampar(1)) then begin
   print, 'Clean beam parameters: ', STRING(beampar(0), FORMAT='(6f10.3)'), $
          '" x ', STRING(beampar(1), FORMAT='(6f10.3)'),'", P.A. =', $
          STRING((beampar(4)+90), FORMAT='(6f5.1)'),' deg'
endif else begin
   print, 'Clean beam parameters: ', STRING(beampar(1), FORMAT='(6f10.3)'), $
          '" x ', STRING(beampar(0), FORMAT='(6f10.3)'),'", P.A. =', $
          STRING(beampar(4), FORMAT='(6f5.1)'),' deg'
endelse

;
;
;  Restore image with clean beam and add back residual map
;

s=size(deconv)
dim = s[0]
imw = s[1]
imh = s[2]

;
;  Default: Only inner quater in the deconv array contains clean components
;
if (dim eq 2) then $
  restore = convolve(cleanbeam,deconv(imw/4:imw*3/4,imh/4:imh*3/4)) + resid

if (dim eq 3) then begin
   nch=s[3]
   restore=make_array(imw,imh,nch)

   for ich = 0, nch -1L do begin
     new_image = convolve(cleanbeam,deconv(imw/4:imw*3/4,imh/4:imh*3/4,ich))
     restore(*,*,ich) = new_image
   endfor

   restore=restore+resid
endif

end


