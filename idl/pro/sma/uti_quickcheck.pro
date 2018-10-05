pro uti_quickcheck, full=full 

common global
common data_set
common plo

pl[*].plot_device='ps'
set_plot,'ps'
args=command_line_args(count=nparams)
if nparams eq 2 then full=args[1]
dir=args[0]

dir='./'+dir
readdata,dir=dir


; print header
logfile='mircheck.log'
openw,unit,logfile,/get_lun
print,""
printlog,format='(%"                 SMA Data reduction log for Data %s")',datadir,unit=unit
printlog," ",unit=unit

; gather info

list='"wt" gt "0"'
result=dat_filter(s_f,list,/reset,/no_notify)
dist_rx=c.rec[uti_distinct(bl[pbf].irec,nrx,/many)]
dist_sb=c.sb[uti_distinct(bl[pbf].isb,nsb,/many)]
;dist_band=c.band[uti_distinct(sp[psf].iband,nband,/many)]
all_isource=uti_distinct(in[pif].isource,nsources,/many_repeat)



; Print track summary
printlog,"All Sources: ",c.source,unit=unit
printlog,"All Baselines: ",c.blcd,unit=unit
printlog,"All Bands: ",c.band[uti_distinct(sp[psf].iband,nbands,/many)],unit=unit
printlog,"All Recs: ",c.rec[uti_distinct(bl[pbf].irec,nrecs,/many)],unit=unit
printlog,"All Sidebands :",c.sb[uti_distinct(bl[pbf].isb,nsbs,/many)],unit=unit
printlog,"All Integrations: ",strtrim(string(in[0].int),2),'-', $
         strtrim(string(in[n_elements(in)-1].int),2),unit=unit


;printlog,'Check: Found a total of ',nsources,'  different sources in the data set.', unit=unit

;all_sources=make_array(nsources,/string)
;all_amps=make_array(nsources,/float)
;for j=0,n_elements(all_souids)-1 do begin
;   result=dat_list(s_l,/reset,/no_notify)
;   i=where(in[pil].souid eq all_souids[j])
;   all_sources[j]=c.source[in[pil[i[0]]].isource]
;   result=dat_list(s_l,'"souid" eq "'+ $
;                   string(all_souids[j])+'"',/reset,/no_notify)
;   all_amps[j]=mean([bl[pbl].ampave])
;endfor

;tmp=max(all_amps,itmp)
;tmpsource=all_sources[itmp]
;print,tmpsource

tmpdir='./mirplotsdir/'

if not keyword_set(full) then begin

device,filename=tmpdir+'spectra.ps',/color,bits=8
select,/p,/re
for i=0, nsources-1 do begin
   tmpsource=strcompress(string(all_isource[i]),/remove)
;   print,c.source[tmpsource]
   result=dat_filter(s_f,'"isource" eq "'+tmpsource+'" and "wt" gt "0"',/reset,/no_notify)
   if result gt 0 then plot_spectra,ntrim=20
endfor

device,filename=tmpdir+'spectra_norm.ps',/color,bits=8
select,/p,/re
for i=0, nsources-1 do begin
   tmpsource=strcompress(string(all_isource[i]),/remove)
;   print,c.source[tmpsource]
   result=dat_filter(s_f,'"isource" eq "'+tmpsource+'" and "wt" gt "0"',/reset,/no_notify)
   if result gt 0 then plot_spectra,ntrim=20,/norm
endfor

device,filename=tmpdir+'cont.ps',/color,bits=8
select,/p,/re
plot_continuum

device,filename=tmpdir+'tsys.ps',/color,bits=8
select,/p,/re,band='c1'
plot_var

for i=0, nrx-1 do begin
   
   device,filename=tmpdir+'rx'+dist_rx[i]+'_phaseclosure.ps',/color,bits=8
   !P.MULTI=[0,2,2]

   for j=0, nsources-1 do begin
      tmpsource=strcompress(string(all_isource[j]),/remove)
;      device,filename=tmpdir+'rx'+dist_rx[i]+'_'+c.source[all_isource[j]]+'_phaseclosure.ps',/color,bits=8
      result=dat_filter(s_f,'"rec" eq "'+dist_rx[i]+'" and "isource" eq "'+tmpsource+'"',/reset,/no_notify)   
      if result gt 0 then uti_phaseclosure,/auto,/cycle,title=c.source[all_isource[j]]
   endfor
endfor


endif else begin


   device,filename=tmpdir+'tsys-int.ps',/color,bits=8
   select,/p,/re,band='c1'
   plot_var,x='int'

   @source.inc
   for i=0,nrx-1 do begin
      for j=0, nsb -1 do begin
         list=' "wt " gt "0" and "rec" eq "'+dist_rx[i]+'" and "sb" eq "'+dist_sb[j]+'"'
         prtname=tmpdir+'cont_rx'+dist_rx[i]+'_'+dist_sb[j]+'_all_amp.ps'
;         print,list
;         print,prtname
         result=dat_filter(s_f,list,/reset,/no_notify)
         device,filename=prtname,/color,bits=8
         plot_continuum,y_vars='amp'
         prtname=tmpdir+'cont_rx'+dist_rx[i]+'_'+dist_sb[j]+'_all_pha.ps'
         device,filename=prtname,/color,bits=8
         plot_continuum,y_vars='pha'
         
         ; plotting gain sources continuum
         nsources=n_elements(gain_sources)
         for k=0, nsources-1 do begin
            tmpsource=gain_sources[k]
            list2=list+' and "source" eq "'+tmpsource+'"'
            result=dat_filter(s_f,list2,/reset,/no_notify)
            if result gt 0 then begin
               prtname=tmpdir+'cont_rx'+dist_rx[i]+'_'+dist_sb[j]+'_'+tmpsource+'_amp.ps'
               device,filename=prtname,/color,bits=8
               plot_continuum,y_vars='amp'
               prtname=tmpdir+'cont_rx'+dist_rx[i]+'_'+dist_sb[j]+'_'+tmpsource+'_pha.ps'
               plot_continuum,y_vars='pha'
            endif else printlog,"No Source: ",tmpsource,unit=unit
         endfor

         ; plotting flux sources continuum
         nsources=n_elements(flux_sources)
         for k=0, nsources-1 do begin
            tmpsource=flux_sources[k]
            list2=list+' and "source" eq "'+tmpsource+'"'
            result=dat_filter(s_f,list2,/reset,/no_notify)
            if result gt 0 then begin
               prtname=tmpdir+'cont_rx'+dist_rx[i]+'_'+dist_sb[j]+'_'+tmpsource+'_amp.ps'
               device,filename=prtname,/color,bits=8
               plot_continuum,y_vars='amp'
               prtname=tmpdir+'cont_rx'+dist_rx[i]+'_'+dist_sb[j]+'_'+tmpsource+'_pha.ps'
               plot_continuum,y_vars='pha'
            endif else printlog,"No Source: ",tmpsource,unit=unit
         endfor

         ; plotting bandpass spectra
         nsources=n_elements(pass_sources)
         for k=0, nsources-1 do begin
            tmpsource=pass_sources[k]
            list2=list+' and "source" eq "'+tmpsource+'"'
            result=dat_filter(s_f,list2,/reset,/no_notify)
            if result gt 0 then begin
               prtname=tmpdir+'spectra_rx'+dist_rx[i]+'_'+dist_sb[j]+'_'+tmpsource+'_amp.ps'
               device,filename=prtname,/color,bits=8
               plot_spectra,x='channel',y_vars='amp', ntrim=20
               prtname=tmpdir+'spectra_rx'+dist_rx[i]+'_'+dist_sb[j]+'_'+tmpsource+'_pha.ps'
               plot_spectra,x='channel',y_vars='pha',ntrim=20
            endif else printlog,"No Source: ",tmpsource,unit=unit
         endfor               
      endfor
   endfor

endelse

end
