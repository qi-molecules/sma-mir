pro uti_pos_fix, vra, vdec

common global
common data_set

   vu=bl[pbl].u*1000.
   vx=(vra/3600.)*(!pi/180.)
   vv=bl[pbl].v*1000.
   vy=(vdec/3600.)*(!pi/180.)
   phase=vu*vx+vv*vy
   cphase=complex(cos(phase*!TWOPI),sin(phase*!TWOPI))
   phase=phase*!TWOPI mod !TWOPI
   k = where(phase lt -!DPI,nk)
   if (nk gt 0) then phase(k) = phase(k) + !TWOPI
   k = where(phase gt !DPI,nk)
   if (nk gt 0) then phase(k) = phase(k) - !TWOPI   
   bl[pbl].phaave=bl[pbl].phaave+phase*360./!TWOPI
   nc=sp[psl].nch
   in[pil].decr=in[pil].decr-double(vdec/3600./!radeg)
   in[pil].rar=in[pil].rar-double(vra/3600./cos(in[pil].decr*!radeg*!pi/180.))/!radeg
   for i=0L, n_elements(pcl)-1L do ch[pcl[i]:pcl[i]+nc[i]-1]=ch[pcl[i]:pcl[i]+nc[i]-1]*cphase[i]

end

