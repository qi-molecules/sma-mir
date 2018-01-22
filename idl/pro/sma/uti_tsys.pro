pro uti_tsys, dir=dir, replace=replace

common global
common data_set

if not keyword_set(dir) then begin
   if keyword_set(e.idl_bcp) then dir=e.idl_bcp else dir='.'
endif

readtsys,dir=dir,tsys=tsys
ntssb=size(tsys.tssb)
nband=ntssb[1]
dnband=(nband+1)*2

ints = in[pil].inhid
ii=uti_distinct(in[pil].inhid,nint,/many_repeat)
subset1=bl[pbl].itel1
subset2=bl[pbl].itel2
subset=[subset1,subset2]
ants=subset(uniq(subset, sort(subset)))
nants=n_elements(ants)

if keyword_set(replace) then begin
   print,'Replace Tsys values for antenna x with values from antenna y:'
   read,badant,prompt='x = '
   read,goodant,prompt='y = '
   igood=where(tsys.itel eq goodant, count)
   if count le 0 then begin
      print, "No antenna y=", goodant
      return
   endif   
   goodtssb=tsys[igood].tssb

   ibad=where(tsys.itel eq badant,count)
   if count le 0 then begin
      print, "No antenna x=", badant
      return
   endif
   tsys[ibad].tssb=goodtssb
   
   for i=0L,nint-1L do begin
   
      tmp_idx = where(ints eq ii[i], ncombo)
      tmp_itsys=where(tsys.inhid eq ii[i])
      tmp_tsys=tsys[tmp_itsys]
      tmp=fltarr(nband+1,2)
      for j=0,nants-1 do begin
         if ants[j] ne badant then begin
            if ants[j] gt badant then  loc=where(bl[pbl[tmp_idx]].itel1 eq badant and bl[pbl[tmp_idx]].itel2 eq ants[j], count) else loc=where(bl[pbl[tmp_idx]].itel1 eq ants[j] and bl[pbl[tmp_idx]].itel2 eq badant, count)
            if count gt 0 then begin
               jj=where(tmp_tsys.itel eq ants[j])
               tmpj=tmp_tsys[jj].tssb
               tmp[0,*]=tmpj[0,*]
               tmp[1:nband,*]=tmpj
               tmpj2=reform(tmp,dnband)

               kk=where(tmp_tsys.itel eq badant)
               tmpk=tmp_tsys[kk].tssb
               tmp[0,*]=tmpk[0,*]
               tmp[1:nband,*]=tmpk
               tmpk2=reform(tmp,dnband)
         
               sp[psl[tmp_idx[loc]]].tssb=sqrt(tmpj2*tmpk2)/2.         
            endif
         endif
      endfor
   endfor
            

endif else begin

   for i=0L,nint-1L do begin
   
      tmp_idx = where(ints eq ii[i], ncombo)
      tmp_itsys=where(tsys.inhid eq ii[i])
      tmp_tsys=tsys[tmp_itsys]
      tmp=fltarr(nband+1,2)

      for j=0,nants-2 do begin
         for k=j+1,nants-1 do begin
            loc=where(bl[pbl[tmp_idx]].itel1 eq ants[j] and bl[pbl[tmp_idx]].itel2 eq ants[k], count)
            if count gt 0 then begin
               jj=where(tmp_tsys.itel eq ants[j])
               tmpj=tmp_tsys[jj].tssb
               tmp[0,*]=tmpj[0,*]
               tmp[1:nband,*]=tmpj
               tmpj2=reform(tmp,dnband)

               kk=where(tmp_tsys.itel eq ants[k])
               tmpk=tmp_tsys[kk].tssb
               tmp[0,*]=tmpk[0,*]
               tmp[1:nband,*]=tmpk
               tmpk2=reform(tmp,dnband)
         
               sp[psl[tmp_idx[loc]]].tssb=sqrt(tmpj2*tmpk2)/2.
            endif
         endfor
      endfor
   endfor

endelse

end

