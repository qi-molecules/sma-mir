pro uti_subcont, reverse=reverse

common global
common data_set

if keyword_set(reverse) then sign=1. else sign=-1.

distinct_band=uti_distinct(c.band[sp[psl].iband],nband,/many_repeat)
nch=sp[psl].nch
icont=where(nch eq 1, count)
if (count le 0) then begin
   print,' *** NO CONTINUUM BAND IN FILTER ! ***'
   print,' ***   PLEASE RESET THE FILTER !   ***'
   return
endif

acont=icont[0L:count-2L]
bcont=icont[1L:count-1L]
ccont=bcont-acont
i=where((ccont le 1) or (ccont gt nband),temp)
if (temp gt 0) then begin
   int_bad=uti_distinct(in[pil[icont[i]]].int,nint_bad,/many_repeat)
   print, ' No CORRESPONDING SPECTRA BAND in integrations:'
   print, int_bad
   print, ' ***   CHECK/FLAG THE DATA AND RESET FILTER !      ***'
   return
endif


for i=0L, count-2L do begin
   ch[pcl[icont[i]+1L]:(pcl[icont[i+1L]-1L]+nch[icont[i+1]-1L]-1L)]=ch[pcl[icont[i]+1L]:(pcl[icont[i+1L]-1L]+nch[icont[i+1L]-1L]-1L)]+sign*ch[pcl[icont[i]]]
;print,(pcl[icont[i+1]-1L]+nch[icont[i+1]-1L]-1L)-(pcl[icont[i]]+1L)+1L
endfor

ch[pcl[icont[count-1]+1L]:pcl[n_elements(pcl)-1L]]=ch[pcl[icont[count-1]+1L]:pcl[n_elements(pcl)-1L]]+sign*ch[pcl[icont[count-1]]]


end


