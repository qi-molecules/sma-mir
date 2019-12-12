pro flag,unity=unity,unflag=unflag,flag=flag,frst=frst,last=last,deficit=deficit, noallpols=noallpols
;yes
;=Task:FLAG --- To flag data
;#Type: utility 
;+Use:
;      In addition to flagging individual data points by mouse clicks in
;      the plot_continuum command, data can be flagged by running the
;      SELECT command followed by the FLAG command. This sets the weights
;      (integration time/tsys) to negative.
;      IDL>flag,/flag
;      Data can be unflagged by
;      IDL>flag,/unflag
;
;      NOTE: Until the SMA tsys hardware is working reliably, it might
;      be better to set all the weights to unity. Positive weights are
;      set to unity and weights which were previously negative are set
;      to negative unity.
;      IDL>flag,/unity
;
;&history:
;------------------------------------------------------------------------
;      cykuo 18feb04 adapting the header
;------------------------------------------------------------------------

common global
common data_set
common plo

  if keyword_set(unity) then begin
;    j = where(sp[psl].wt le 0.,ncount)  
;    if ncount gt 0 then sp[j].wt  = -1.
;    j = where(re.wts[prl] le 0.,ncount)
;    if ncount gt 0 then re.wts[j] = -1.
;    j = where(sp[psl].wt gt 0.,ncount)
;    if ncount gt 0 then sp[j].wt  = 1.
;    j = where(re.wts[prl] gt 0.,ncount)
;    if ncount gt 0 then re.wts[j] = 1.
     sp[psl].wt=sp[psl].wt/abs(sp[psl].wt)
     re.wts[prl]=sp[psl].wt*re.integs[prl]
  endif

if (keyword_set(flag)) then begin
  sp[psl].wt = -abs(sp[psl].wt)
  re.wts[prl] = -abs(re.wts[prl] )
endif
      
if (keyword_set(unflag)) then begin
  sp[psl].wt = abs(sp[psl].wt)
  re.wts[prl] = abs(re.wts[prl] )
endif

if (keyword_set(frst)) then begin

distinct_blcd=uti_distinct(c.blcd[bl[pbl].iblcd],nblcds,/many_repeat)

for ib = 0, nblcds-1 do begin

result=dat_list(s_l,'"blcd" eq " '+distinct_blcd[ib]+ '" and "band" eq "c1" and "wt" gt "0"',/reset,/no_notify)
j0=strcompress(string(in[pil[0]].int),/remove_all)
j=in[pil[1:(n_elements(pil)-1)]].isource-in[pil[0:(n_elements(pil)-2)]].isource
jj=where(j ne 0, count)+1
jj=strcompress(string(in[pil[jj]].int),/remove_all)
if (count eq 0) then jj=j0 else jj=[j0,jj]
count=count+1
command=' "blcd" eq" ' +distinct_blcd[ib]+ '" and ("int" eq "' + strtrim(jj[0],2)+'"'
if count gt 1 then for i=1, count-1 do command=command+' or "int" eq "'+strtrim(jj[i],2)+'"'
command=command+')'
;print,command
print,'flagging first integration points for each source on baseline ', distinct_blcd[ib]
print,jj
result=dat_list(s_l,command,/reset,/no_notify)
flag,/flag
result=dat_list(s_l,/reset,/no_notify)
endfor

endif

if (keyword_set(last)) then begin

distinct_blcd=uti_distinct(c.blcd[bl[pbl].iblcd],nblcds,/many_repeat)

for ib = 0, nblcds-1 do begin
result=dat_list(s_l,'"blcd" eq "'+distinct_blcd[ib]+ '" and "band" eq "c1" and "wt" gt "0"',/reset,/no_notify)
j0=strcompress(string(in[pil[n_elements(pil)-1]].int),/remove_all)
j=in[pil[0:(n_elements(pil)-2)]].isource-in[pil[1:(n_elements(pil)-1)]].isource
jj=where(j ne 0, count)
jj=strcompress(string(in[pil[jj]].int),/remove_all)
if (count eq 0) then jj=j0 else jj=[jj,j0]
count=count+1
command=' "blcd" eq "' +distinct_blcd[ib]+ '" and ("int" eq "' + strtrim(jj[0],2)+'"'
if count gt 1 then for i=1, count-1 do command=command+' or "int" eq "'+strtrim(jj[i],2)+'"'
command=command+')'
;print,command
print,'flagging last integration points for each source on baseline ', distinct_blcd[ib]
print,jj
result=dat_list(s_l,command,/reset,/no_notify)
flag,/flag
result=dat_list(s_l,/reset,/no_notify)

endfor
endif

if (keyword_set(deficit)) then begin

command='"sb" eq "'+strcompress(c.sb[bl[pbl[0]].isb],/remove_all) $
 +'" and "blcd" eq "'+strcompress(c.blcd[bl[pbl[0]].iblcd],/remove_all) $
 +'" and "band" eq "c1"'
result=dat_list(s_l,command,/no_notify, /reset)
nc=n_elements(pil)
command='"sb" eq "'+strcompress(c.sb[bl[pbl[0]].isb],/remove_all) $
 +'" and "blcd" eq "'+strcompress(c.blcd[bl[pbl[0]].iblcd],/remove_all) $
 +'" and "band" eq "s6"'
result=dat_list(s_l,command,/no_notify, /reset)
nb2=n_elements(pil)
if (nb2 lt nc) then begin
 nb=nc-nb2
 j=indgen(nc)+1
 jj=where((in[pil].int-j) gt 0)
 jjj=strcompress(string(in[pil[jj[0]]].int-1),/remove_all)
 command=' ("int" eq "' + strtrim(jjj,2)+'"'
 if nb gt 1 then begin
  for i=1, nb-1 do begin
   jj=where((in[pil].int-j) gt i)
   j0=strcompress(string(in[pil[jj[0]]].int-1),/remove_all)
   command=command+' or "int" eq "'+j0+'"'
   jjj=[jjj,j0]
  endfor
 endif
 command=command+')'
 print,'flagging integration points where points in blocks are different:'
 print,jjj
; print,command
 result=dat_list(s_l,command,/reset,/no_notify)
 flag,/flag
endif else begin
 print, 'no deficit points, nothing flagged !'
 return
endelse
endif

if keyword_set(noallpols) then begin
   ints = in[pil].int
   ibls = bl[pbl].iblcd
   psls = psl
   ii=uti_distinct(ints,nint,/many_repeat)
   jj=uti_distinct(ibls,nbls,/many_repeat)
   temp=uti_distinct(sp[psl].iband,nbands,/many_repeat)
   temp=uti_distinct(bl[pbl].isb,nsbs,/many_repeat)
   for i=0L,nint-1L do begin
      for j=0,nbls-1L do begin
         tmp_idx = where((ints eq ii[i]) and (ibls eq jj[j]), ncombo)
         if ncombo ne 0 and ncombo ne 4*nbands*nsbs then begin
            print,"Flagging integration #",strcompress(string(ii[i]),/remove), " Baseline ",c.blcd[jj[j]], " which doesn't have all 4 pol states."
            sp[psls[tmp_idx]].wt=-1
         endif
      endfor
   endfor
endif


end
