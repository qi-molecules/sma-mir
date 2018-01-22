pro uti_sbpass, tel_bsl=tel_bsl, refant=refant, preavg=preavg, ntrim=ntrim, smoothing=smoothing

common global
common data_set

sb=''
read,sb,prompt=' Enter which sideband to use, l or u : ' 
if (sb ne 'u' and sb ne 'l') then begin
   print,'No such sideband: ',sb
   return
endif

if not keyword_set(tel_bsl) then tel_bsl='telescope'
if not keyword_set(refant) then refant=6
if not keyword_set(preavg) then preavg=2
if not keyword_set(ntrim) then ntrim=0
if not keyword_set(smoothing) then smoothing=4


result=dat_list(s_l,'"sb" eq "'+sb+'"',/no_notify,/reset)
phase_conjugate,/force

result=dat_list(s_l,/no_notify,/reset)
pass_cal, sideband=sb, tel_bsl=tel_bsl,refant=refant,preavg=preavg,ntrim=ntrim, smoothing=smoothing

result=dat_list(s_l,'"sb" eq "'+sb+'"',/no_notify,/reset)
phase_conjugate,/force

result=dat_list(s_l,/no_notify,/reset)

end
