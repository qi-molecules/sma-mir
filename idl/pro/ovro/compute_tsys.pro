function compute_tsys,tel,iscan,sb,band,tsys
   ; Common blocks
     common global
     common data_set

   ; Initialize
     ntel = n_elements(tel)
     tsys = fltarr(ntel)

   ; Set filter
     command = '"band" eq "' + band + '" and "int" eq "' + string(iscan) + '" and "sb" eq "' + sb + '" and "tssb" lt "9000" and "tssb" gt "0"'
     nrows = dat_select(s_l,command,/reset,/no)
     if (nrows lt 3) then return,-1

   ; Set Y array
     y = 2.0 * alog(sp(psl).tssb)

   ; Set array
     a = intarr(ntel,nrows)
     tel1 = c.tel1[bl(pbl).itel1]
     tel2 = c.tel2[bl(pbl).itel2]

     for i=0,nrows-1L do begin
         j1 = where(tel1[i] eq tel)
         j2 = where(tel2[i] eq tel)
         a[[j1[0],j2[0]],i] = 1
     endfor

   ; Solve using singular value decomposition
     svdc,A,W,U,V
     tsys = exp(svsol(u,w,v,y))

   ; Done
     return,1
end
