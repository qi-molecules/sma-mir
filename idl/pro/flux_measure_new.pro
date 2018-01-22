pro flux_measure_new,scalar=scalar,vector=vector,sources=sources,amps=amps,band=band
;
;=Task:FLUX_MEASURE_NEW 
;#Type: utility
;+Use:
;      Compute vector and scalar average fluxes for each selected source 
;      in the data set, pass results through argument list, and store
;      the amplitude in the integration header (in.sflux).
;      This function uses the average amplitude and phase stored in the 
;      baseline headers assuming that these are current values. If not, 
;      update the baseline headers by running uti_avgband before running 
;      flux_measure. uti_avgband applies the current weights which
;      include any calibration and only averages over the central 82 MHz
;      of the 104 MHz s-bands. Therefore, these conditions apply to 
;      the measured fluxes computed here.
;@scalar:
;      If selected, then scalar averaging is used. Scalar is the default.
;      example: flux_measure_new,/scalar
;@vector:
;      If selected, then vector averaging is used. If both vector and
;      scalar are selected, then scalar alone is used.
;      example: flux_measure_new,/vector
;@band:
;	band = "c1" or "c2" or ["c1","c2"]
;	If c2 is not in the data, the results for c2 are not valid
;       example: flux_measure_new,/vector,band=["c1","c2"]
;&history:
;------------------------------------------------------------------------
;  Eric Keto, October 1, 2010
;  Eric Keto, October 26, 2010 Added mean UT to printed table
;------------------------------------------------------------------------

      common global
      common data_set

 if not keyword_set(scalar) then scalar = 0
 if not keyword_set(vector) then vector = 0
 if not keyword_set(band) then band = "c1"

 if not (vector + scalar) then scalar = 1

for iband = 0,n_elements(band) - 1 do begin

; Re-set the local list that is used internally
 result=dat_list(s_l,/reset,/no_notify)

; Check for valid filter. 
 if n_elements(pil) eq 0 then begin
   all_souids=-1 & all_sources='' & all_amps=0.
   print, "No sources selected. Check previous select or dat_filter commands."
   return
;   return,-1
 endif
 
      print,""
      if scalar then print,'Scalar average for band '+band[iband]
      if vector and not scalar then print,'Vector average for band '+band[iband]
      print,'        Source          Flux        StdDev    SNR     Nscans  On source   Mean UT '
      print,'                        (Jy)        (Jy)                      time (min)  (hours) '
; Get a list of unique sources from their id numbers.
 all_souids=uti_distinct(in[pil].souid,ndistinct,/many_repeat)

; Initialize some arrays for this list of sources.
 sources=make_array(ndistinct,/string)
 amps=make_array(ndistinct,/float)

; For each source in the list, calculate vector and scalar averages

 for j=0,n_elements(all_souids)-1 do begin

; Reset the local list
   result=dat_list(s_l,/reset,/no_notify)

; Choose the data for the current source in the loop.
; i is the pointer to all scans for the current source.
   i=where(in[pil].souid eq all_souids[j])

; Get the source name off the first instance. Store in arg list.
   sources[j]=c.source[in[pil[i[0]]].isource]

; Filter the data for the current band. Use only data with valid weights.
   b = band[iband]
; List of scans for this source, this band, with valid weights
   result=dat_list(s_l,'"souid" eq "'+ strtrim(string(all_souids[j]),2) + '"' + $
	' and "wt" gt "0" and "band" eq "'+b+'"',$
	/reset,/no_notify)

; Frequency is here for reference, not used.
;  freq = mean(sp[psl].fsky)
; n is a set of unique (duplicates removed) pointers to the integration headers.
  n = uniq(pil,sort(pil))
; On source integration time in minutes
  onsource = total(in[pil[n]].rinteg) / 60.
; nscans is the number of scans for each source, i.e. the number
; of 30 sec observations executed by the telescope.
  nscans = n_elements(n)

mean_ut = mean(in[pil[n]].dhrs)

; Determine average amplitude for all selected scans of this source.
; Calculate the expected error of the mean which is different from the
; expected error of the data by sqrt(n)
; Phase averages written in for reference, but not used.

; IDL uses n = number of data when computing the std dev. For our
; random samples we should use (n-1) in the stddev. Since we 
; use n in computing the mean of the std dev, we can instead use
; (n-1) in this step and end up with the correct formula.

; The standard deviation of the flux is computed as,
; sigma_flux = sqrt ( sigma_r^2 * (df/dr)^2 + sigma_i^2 * (df/di)^2 )
; where sigma_r is the std dev of the real part of the complex
; ampl and phase, r is the real part itself, ...

; Determine vector average amplitude
   uti_conv_apc,cmp,bl[pbl].ampave,bl[pbl].phaave,/complex
   cmp_avg = mean(cmp)
   ndata = float(n_elements(cmp))
   sqrtn = sqrt(ndata-1)
   real_cmp_dev = stddev(real_part(cmp)) / sqrtn
   imag_cmp_dev = stddev(imaginary(cmp)) / sqrtn
   vec_amp_avg = abs(cmp_avg)
   vec_amp_dev = sqrt(  (real_cmp_dev*real_part(cmp_avg))^2   $
                      + (imag_cmp_dev*imaginary(cmp_avg))^2 ) $
                 / vec_amp_avg
   vec_amp_snr = vec_amp_avg / vec_amp_dev 
;   vec_pha_avg = !radeg*atan(imaginary(cmp_avg),real_part(cmp_avg))
  
; Determine scalar average amplitude.
   sca_amp_avg = mean(bl[pbl].ampave)
;   sca_pha_avg = mean(bl[pbl].phaave)
   sca_amp_dev = stddev(bl[pbl].ampave) / sqrtn
   sca_amp_snr = sca_amp_avg / sca_amp_dev

; Store the amplitude in the arg list. 
  if vector then begin
    amps[j] = vec_amp_avg
    snr = vec_amp_snr 
    dev = vec_amp_dev 
  endif 

  if scalar then begin
    amps[j] = sca_amp_avg
    snr = sca_amp_snr 
    dev = sca_amp_dev 
  endif

; Write the newly determined flux in the integration header.
; Note each source has the same flux in all of its several integration 
; headers rather than a different flux for each individual integration
; of that source.
    in[pil].sflux = amps[j]

         print,format='(A16,4x,2x,F10.4,F10.4,F8.1,I7,F10.1,2x,F10.2)',$
            sources[j],amps[j],dev,snr,nscans,onsource,mean_ut
 endfor

 endfor

; Reset the local list in case the next program run
; by the observer does not do so. This does not affect
; the global filter set by SELECT or DAT_FILTER
result=dat_list(s_l,/reset)

      return
end
