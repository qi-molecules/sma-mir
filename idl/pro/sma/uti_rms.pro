pro uti_rms

; calculate theoretical noise in map

common global
common data_set

npts=dat_list(s_l,/reset,/no_notify)
distinct_sb=uti_distinct(c.sb[bl[pbl].isb],nsbs,/many_repeat)
distinct_rec=uti_distinct(c.rec[bl[pbl].irec],nrecs,/many_repeat)
distinct_source=uti_distinct(c.source[in[pil].isource],nsources,/many_repeat)
band=uti_distinct(c.band[sp[psl].iband],nbands,/many_repeat)

noise=1.0d0/sqrt(total(sp[psl].wt))

if nbands gt 1 or nsources gt 1 or nrecs gt 1 or nsbs gt 1 then begin
   print, "Only works for single source,sideband,band,receiver, quit!"
   return
endif else begin
   print,npts,noise*1.e3,format='("theoretical rms noise in map for",i5,'+ $
         '" visibilities is ",f8.2," mJy/beam")'
endelse
end

