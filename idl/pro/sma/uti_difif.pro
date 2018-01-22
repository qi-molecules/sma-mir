pro uti_difif, dif=dif, swmch1=swmch1, swmch2=swmch2, no_notify=no_notify

common global
common data_set

if tag_exist(bl,'tpvar') then newformat=0 else newformat=1

swmonly=0
if newformat eq 1 then begin
   result=uti_distinct(sp.sphint1,/many,ntmp)
   if ntmp eq 1 and result eq 1 then begin
      print,'SWARM only track ...'
      swmonly=1
   endif
endif

if not keyword_set(dif) then dif=[1,2]
listall=''
dif=dif[sort(dif)]
for i=0,1 do begin
   case dif[i] of
      '1': begin
         if swmonly then begin
            listall=listall+' ("iband" eq "1") or '
            if i eq 0 then chunk1='1'
         endif else begin
            listall=listall+' ("iband" le "24") or '
            if i eq 0 then chunk1='24'
         endelse
      end
      '2': begin
         if swmonly then begin
            listall=listall+' ("iband" eq "2") or '
            if i eq 0 then chunk1='2'
         endif else begin
            listall=listall+' ("iband" ge "25" and "iband" le "48") or '
            if i eq 0 then chunk1='48'
         endelse
      end
      '3': begin
         if swmonly then begin
            listall=listall+' ("iband" eq "3") or '
            if i eq 0 then chunk1='3'
         endif else begin
            listall=listall+' ("iband" eq "49") or '
            if i eq 0 then chunk1='49'
         endelse
      end
      '4': begin
         if swmonly then begin
            listall=listall+' ("iband" eq "4") or '
            if i eq 0 then chunk1='4'
         endif else begin
            listall=listall+' ("iband" eq "50") or '
            if i eq 0 then chunk1='50'
         endelse
      end
      '5': begin
         if swmonly then begin
            listall=listall+' ("iband" eq "5") or '
            if i eq 0 then chunk1='5'
         endif else begin
            listall=listall+' ("iband" eq "51") or '
            if i eq 0 then chunk1='51'
         endelse
      end
      '6': begin
         if swmonly then begin
            listall=listall+' ("iband" eq "6") or '
            if i eq 0 then chunk1='6'
         endif else begin
            listall=listall+' ("iband" eq "52") or '
            if i eq 0 then chunk1='52'
         endelse
      end         
      else: begin
         print,'No such IF:',dif[i]
         return
      endelse
   endcase
endfor

listall=listall+'("iband" eq "0")'
;print,listall

if swmonly eq 1 then begin
   print, 'Calculating differences between two Chunks ...', dif
endif else begin
   print, 'Calculating differences between two IFs ...', dif
endelse

result=dat_list(s_l,listall,/reset,/no_notify)

int0=in[pil[0]].int
result=dat_list(s_l,'"int" eq "'+strcompress(string(int0),/remove_all)+'" and "iband" gt "0" and "iband" le "'+chunk1+'"',/no_notify)
result=uti_distinct(sp[psl].iband,nb0)
;print,nb0

result=dat_list(s_l,listall,/reset,/no_notify)
nch=sp[psl].nch
j=where(nch eq 1, count)
if (count le 0) then begin
   print,' *** NO CONTINUUM BAND IN FILTER ! ***'
   print,' *** PLEASE RESET FILTER TO INCLUDE CONTINUUM BANDS ***'
   return
endif

j=where(nch gt 1,count)
if (count le 0) then begin
   print,' *** NO SPECTRUM BAND IN FILTER ! ***'
   print,' ***   PLEASE RESET THE FILTER !   ***'
   return
endif

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

for i=0L, count-1L do begin
   if i eq count-1L then nb=n_elements(pcl)-icont[count-1L]-1 else nb=icont[i+1]-icont[i]-1
   npts=nch[icont[i]+1:icont[i]+nb]
   avgwt=dblarr(nb)

   skip1 = fix(  (1 - 82./104.)*npts/2.  )
   skip2 = fix(  (1 - 82./104.)*npts/2.  )
   first = pcl[icont[i]+1:icont[i]+nb]
   
   if newformat then k=where((sp[psl[icont[i]+1:icont[i]+nb]].sphint1 eq 1) or (sp[psl[icont[i]+1:icont[i]+nb]].iband ge 49),nswp) else k=where(sp[psl[icont[i]+1:icont[i]+nb]].iband ge 49,nswp)
   bw=0.
   if nswp gt 0 then begin
      if keyword_set(swmch1) and keyword_set(swmch2) then begin
         skip1[k]=swmch1-1
         skip2[k]=npts[k]-swmch2
      endif
      swmfres=abs(sp[psl[k[0]+1]].fres)
      bw=total([npts[k]-skip1[k]-skip2[k]])*swmfres + (nb-nswp)*82.
      avgwt[*] = 82./bw
      avgwt[k] = (npts[k]-skip1[k]-skip2[k])*swmfres/bw
   endif else begin
      bw=n_elements(npts)*82.
      avgwt[*] = 82./bw
   endelse      
   
   cmp=complex(0,0)
   for j=0,nb0-1 do begin
      cmp=cmp+avgwt[j]*total(ch[first[j]+skip1[j]:first[j]+npts[j]-skip2[j]-1])/(npts[j]-skip2[j]-skip1[j])
   endfor
   uti_conv_apc,cmp,amp1,pha1,/amp_pha
;        if nb0 eq nb then begin
;           if tag_exist(bl,'tpvar') then bl[pbl[icont[i]]].tpvar=uti_pha_180(pha1) else bl[pbl[icont[i]]].blhdbl5=uti_pha_180(pha1)
;        endif else begin
   cmp=complex(0,0)
   for j=nb0,nb-1 do begin
      cmp=cmp+avgwt[j]*total(ch[first[j]+skip1[j]:first[j]+npts[j]-skip2[j]-1])/(npts[j]-skip2[j]-skip1[j])
   endfor
   uti_conv_apc,cmp,amp2,pha2,/amp_pha
   if newformat then begin
      bl[pbl[icont[i]]].blhdbl5=uti_pha_180(pha2-pha1)
      bl[pbl[icont[i]]].blhdbl6=amp2/amp1
   endif else begin
      bl[pbl[icont[i]]].tpvar=uti_pha_180(pha2-pha1)
      bl[pbl[icont[i]]].sigcoh=amp2/amp1
   endelse
   
endfor



if not keyword_set(no_notify) then begin
   
  if tag_exist(bl,'tpvar') then begin
   print, ''
   print, "Phase differences (IF2-IF1) stored in variable 'tpvar'. "
   print, "Amplitude ratios (IF2/IF1) stored in variable 'sigcoh'. "
   print, 'To plot the variable vs time or integration number:'
   print, "    IDL> plot_var,x='dhrs', y='tpvar', frame_v='blcd,rec,sb'"
   print, " or IDL> plot_var,x='int',  y='sigcoh', frame_v='blcd,rec,sb'"
   print, ''
   print, 'To plot both amp ratios and phase differences:'
   print, "    IDL> plot_continuum,y='difif'" 
   print, " /preavg keyword can be used in this context."
   print, ''
 endif else begin
   print, ''
   print, "Phase differences (IF2-IF1) stored in variable 'blhdbl5'. "
   print, "Amplitude ratios (IF2/IF1) stored in variable 'blhdbl6'. "
   print, 'To plot the variable vs time or integration number:'
   print, "    IDL> plot_var,x='dhrs', y='blhdbl5', frame_v='blcd,rec,sb'"
   print, " or IDL> plot_var,x='int',  y='blhdbl6', frame_v='blcd,rec,sb'"
   print, ''
   print, 'To plot both amp ratios and phase differences:'
   print, "    IDL> plot_continuum,y='difif'" 
   print, " /preavg keyword can be used in this context."
   print, ''   
 endelse
endif


result=dat_list(s_l,/reset)

end


