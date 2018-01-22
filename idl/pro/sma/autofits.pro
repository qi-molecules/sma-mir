pro autofits,source=source

common global
common data_set

list='"wt" gt "0" and "source" eq "'+source+'"'
result=dat_list(s_l,list,/reset,/no_notify)
dist_rx=c.rec[uti_distinct(bl[pbl].irec,nrx,/many)]

dist_sb=c.sb[uti_distinct(bl[pbl].isb,nsb,/many)]
dist_band=c.band[uti_distinct(sp[psl].iband,nband,/many)]



for i=0,nrx-1 do begin
   list='"wt" gt "0" and "source" eq "'+source+'"'+' and "rec" eq "'+dist_rx[i]+'"'
   list=list[0]
   result=dat_list(s_l,list,/reset,/no_notify)
   if result gt 0 then begin
      for j=0,nsb-1 do begin
         for k=0,nband-1 do begin
            fitsname=source+'_'+dist_sb[j]+'_'+dist_band[k]+'_rx'+dist_rx[i]
            print,''
            print,'*****'
            print,'Writing uvfits file:',fitsname
;            write_fits,filename=fitsname,source=source,sideband=dist_sb[j],band=dist_band[k]
            result=fits_out(fitsname,'UVFITS',source,'','',dist_band[k],dist_sb[j],'','')
         endfor
      endfor
   endif
endfor

print,'Done!'
end

