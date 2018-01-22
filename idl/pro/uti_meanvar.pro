; UTI_MEANVAR will compute the mean and standard error in the mean
; from an input data set. The user has the option of including weights.
; The results are returned in a vector:
;    result[0] : weighted mean.
;    result[1] : standard error in the mean based on the observed dispersion 
;    result[2] : observed dispersion
;
; INPUTS:
;    x       = vector containing data
;    weights = weights. Only weights > 0.0 are used.
;    sig     = uncertainty in the individual data points. The weights are
;              computed as 1/sig^2.
;    iset    = indicates which elements in the output vector are set.
;                0 -> entry is not set in output vector
;                1 -> entry is     set in output vector
;    print_results : if set, the results are printed to standard output
;
; Examples:
;     x = [1.0, 1.0, 2.0]
;     sig = [0.1, 0.2, 0.05]
;     wts = [1.0, 0.5, 4.00]
;     result = uti_meanvar(x,sig=sig)
;     result = uti_meanvar(x,weights=sig)

function uti_meanvar,x,weights=weights,sig=sig,iset=iset,print_results=print_results
   ; Allocate memory for results
     result = dblarr(3)
     iset   = intarr(3)

   ; Set number of data points
     if not keyword_set(x) then return,-1
     ndata = n_elements(x)

   ; Set weights
     if keyword_set(weights) then $
        wts = weights > 0 $
     else if keyword_set(sig) then begin
        wts = replicate(0.0,ndata)
        j = where (sig gt 0.0,nj)
        if (nj gt 0) then wts[j] = 1.0/sig[j]^2
     endif else $
        wts = replicate(1.0,ndata)

   ; Compute mean
     sum_weights = total([wts])
     if (sum_weights eq 0.0) then return,-1
     result[0] = total([wts*x]) / sum_weights
     iset[0] = 1

   ; Compute observed dispersion and standard error of the mean
     if (ndata gt 1) then begin
       ; Variance
         rn = 1.0D*ndata
         var = rn/(rn-1) * total([wts*(x-result[0])^2]) / sum_weights

       ; Observed rms
         result[2] = sqrt(var > 0.0)
         iset[2] = 1

       ; Standard error of the mean
         result[1] = result[2] / sqrt(rn)
         iset[1] = 1
     endif

   ; Print results if requested
     if keyword_set(print_results) then begin
        print,"Mean    = ",result[0]
        print,"E(mean) = ",result[1]
        print,"RMS     = ",result[2]
     endif

   ; Done
     return,result
end
