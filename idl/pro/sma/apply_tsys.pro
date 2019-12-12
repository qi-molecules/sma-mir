pro apply_tsys
;yes
;=Task:APPLY_TSYS --- To scale amplititudes by Tsys
;#Type: utility
;+Use:
;      The SMA currently does not automatically scale the data by Tsys
;      as would be done at most other radio telescopes due to occasionally
;      unreliable Tsys measurements. Therefore, to correct the amplitude
;      variation, it is necessary to execute the mir command
;      IDL>apply_tsys
;
;      NOTE: The SMA records Tsys for each baseline and spectral band as 
;      the geometric mean of Tsys from the two antennas of the baseline. 
;      Currently there is only one total power detector for each antenna
;      and it reads only the broadband power from the receiver. Therefore, 
;      Tsys at the SMA is the same for each band(chunk) belonging to an
;      integration and baseline.
; 
;      Major adaptation by Mark Gurwell (SAO, 1 Sep 2004)
;      Update 20 Nov 2014 to reflect change in scaling factors when
;             moving to the new format (changed to includ SWARM).
;      
;&history:
;------------------------------------------------------------------------
;      09feb04 adapting the form of the header
;------------------------------------------------------------------------

common global
common data_set

if tag_exist(in,'inhdbl6') eq 0 then begin
; change Tsys to reflect SSB value, as well as correlator
; normalization factor of (2*pi)**0.5, and 130 Jy/K

   sp[psl].tssb = sp[psl].tssb * 2.00 * sqrt(2.00 * !pi) * 130.00 

; scale data

   uti_tsysamp

; reset system temperature to the "real" SSB temperature

   sp[psl].tssb = sp[psl].tssb / (sqrt(2.00 * !pi) * 130.00) 

endif else begin

; change Tsys is already SSB value, as well as correlator
; normalization factor of 130 Jy/K  (v2)

   tmptsys=sp[psl].tssb
   if tag_exist(c,'filever') then begin
;     if fix(c.filever) ge 2 then sp[psl].tssb = tmptsys * 130.00 else sp[psl].tssb = tmptsys * sqrt(2.00 / !pi) * 130.00
;   endif else sp[psl].tssb = tmptsys * sqrt(2.00 / !pi) * 130.00
     if min([fix(c.filever)]) ge 2 then sp[psl].tssb = tmptsys * 130.00 else sp[psl].tssb = tmptsys * sqrt(0.5) * 130.00
   endif else sp[psl].tssb = tmptsys * sqrt(0.5) * 130.00

; scale data

   uti_tsysamp

; reset system temperature
   sp[psl].tssb = tmptsys

endelse


; calculate 'real' rms noise level as
;      sigma = (2)**0.5 K Tssb / ((bw*t)**0.5*e_c*e_a*A)
; then weight is 1/sigma**2 
;      weight = (bw*t)*(e_c*e_a*A)**2 / (2 *K**2 *Tssb**2)
; 
; note e_c should be 0.988457 instead of 0.88

;    if tag_exist(c,'filever') then begin 
;       if fix(c.filever) ge 2 then e_c=0.988457 else e_c=0.88
;    endif else e_c=0.88
    e_c=0.988457
    sp[psl].wt =(sp[psl].wt/abs(sp[psl].wt)) * $
               ( (abs(sp[psl].fres)*1.e6 * sp[psl].integ ) * $
                 (e_c*0.75*28.274 * e_c*0.75*28.274) ) / $
               (2. * 1380.*sp[psl].tssb * 1380.*sp[psl].tssb )
    re.wts[prl]=sp[psl].wt*re.integs[prl]

end



