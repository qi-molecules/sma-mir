pro autofits,source=source

common global
common data_set

if not keyword_set(source) then begin
   print,'Please set keyword SOURCE to select the target source'
   print,'  to output.'
   print,'Quit !'
   return
endif else begin
   source=strtrim(source,2)
   for ii=0, n_elements(source) -1 do begin         
      list='"wt" gt "0" and "source" eq "'+source[ii]+'"'
      result=dat_filter(s_f,list,/reset,/no_notify)
      dist_rx=c.rec[uti_distinct(bl[pbf].irec,nrx,/many)]
      
      dist_sb=c.sb[uti_distinct(bl[pbf].isb,nsb,/many)]
      dist_band=c.band[uti_distinct(sp[psf].iband,nband,/many)]

      for i=0,nrx-1 do begin
         list='"wt" gt "0" and "source" eq "'+source[ii]+'"'+' and "rec" eq "'+dist_rx[i]+'"'
         list=list[0]
         result=dat_filter(s_f,list,/reset,/no_notify)
         if result gt 0 then begin
            for j=0,nsb-1 do begin
               for k=0,nband-1 do begin
                  fitsname=source[ii]+'.'+dist_sb[j]+'_'+dist_band[k]+'_rx'+dist_rx[i]
                  print,''
                  print,'*****'
                  print,'Writing uvfits file:',fitsname
;            write_fits,filename=fitsname,source=source,sideband=dist_sb[j],band=dist_band[k]
                  result=fits_out(fitsname,'UVFITS',source[ii],'','',dist_band[k],dist_sb[j],'','')
               endfor
            endfor
         endif
      endfor
   endfor
   print,'Done!'
endelse

end

