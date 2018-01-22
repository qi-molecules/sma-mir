pro uti_phaseclosure,autocycle=autocycle
;yes
;=Task:UTI_PHASECLOSURE --- To plot the phase closure of data
;#Type: utility
;+Use:
;      To check the phase closure of the data, one can use the commnad
;      UTI_PHASECLOSURE. To use it, just type
;      >uti_phaseclosure
;      Following the questions in this program, the plot of the phase
;      closure vs. time will be shown.
;&history:
;------------------------------------------------------------------------
;      cykuo 19feb04 adapting the header
;------------------------------------------------------------------------

common global
common data_set

red = [0,1,1,0,0,1]
green = [0,1,0,1,0,1]
blue = [0,1,0,0,1,0]
; color 2 = red
;       3 = green
;       4 = blue
;       5 = yellow
tvlct, 255*red, 255*green, 255*blue

 alltels = [bl[pbf].itel1,bl[pbf].itel2]
 distinct_tels=alltels(  uniq(alltels,sort(alltels) ))
 ntels = n_elements(distinct_tels)
 ac1=0
 ac2=ac1+1
 ac3=ac2+1

 if (ntels lt 3) then begin
   print,'Antenna Number Less Then 3, No Closure Measurement Possible. Check Filter!'
   return
 endif

 j=0

 while(j eq 0) do begin

   if (not keyword_set(autocycle)) then begin
     temp=''
     start:
     print,'Enter 3 antenna numbers seperated by comma, eg: 2,3,4'
     read,temp
     if temp eq '' then goto,finish
     parts=strtrim(strsplit(temp,',',/extract),2)
     parts=parts(sort(parts))
     if n_elements(parts) ne 3 then begin
       print, 'Please input 3 antenna numbers !'
       goto, start
     endif
   endif else begin
     parts = make_array(3,/string)
     parts[0] = strtrim(distinct_tels[ac1],2)
     parts[1] = strtrim(distinct_tels[ac2],2)
     parts[2] = strtrim(distinct_tels[ac3],2)
   endelse


   titlename='Phase Closure: '+parts(0)+'-'+parts(1)+'-'+parts(2)
   bsl=strarr(3)
   bsl=[parts(0)+'-'+parts(1),parts(1)+'-'+parts(2),parts(0)+'-'+parts(2)]
   print,'getting phase closure of ',parts(0)+'-'+parts(1)+'-'+parts(2)

   ii=where(bsl[0] eq c.blcd, count)
   if (count gt 0) then begin
     cmd='"band" eq "c1" and "sb" eq "l" and "blcd" eq "'+c.blcd(ii)+'"'
     nl01=dat_list(s_l,cmd[0],/reset,/no_notify)
     l01=bl[pbl].phaave
     wl01=sp[psl].wt
     cmd='"band" eq "c1" and "sb" eq "u" and "blcd" eq "'+c.blcd(ii)+'"'
     nu01=dat_list(s_l,cmd[0],/reset,/no_notify)
     u01=bl[pbl].phaave
     wu01=sp[psl].wt
   endif else begin
     print, 'Baseline ',bsl[i],' not found !'
     goto, finish
   endelse

   ii=where(bsl[1] eq c.blcd, count)
   if (count gt 0) then begin
     cmd='"band" eq "c1" and "sb" eq "l" and "blcd" eq "'+c.blcd(ii)+'"'
     nl12=dat_list(s_l,cmd[0],/reset,/no_notify)
     l12=bl[pbl].phaave
     wl12=sp[psl].wt
     cmd='"band" eq "c1" and "sb" eq "u" and "blcd" eq "'+c.blcd(ii)+'"'
     nu12=dat_list(s_l,cmd[0],/reset,/no_notify)
     u12=bl[pbl].phaave
     wu12=sp[psl].wt
   endif else begin
     print, 'Baseline ',bsl[i],' not found !'
     goto, finish
   endelse

   ii=where(bsl[2] eq c.blcd, count)
   if (count gt 0) then begin
     cmd='"band" eq "c1" and "sb" eq "l" and "blcd" eq "'+c.blcd(ii)+'"'
     nl02=dat_list(s_l,cmd[0],/reset,/no_notify)
     l02=bl[pbl].phaave
     wl02=sp[psl].wt
     cmd='"band" eq "c1" and "sb" eq "u" and "blcd" eq "'+c.blcd(ii)+'"'
     nu02=dat_list(s_l,cmd[0],/reset,/no_notify)
     u02=bl[pbl].phaave
     wu02=sp[psl].wt
   endif else begin
     print, 'Baseline ',bsl[i],' not found !'
     goto, finish
   endelse

   lplot=1
   if (nl01 ne nl02 or nl01 ne nl12 or nl02 ne nl12 or nl01*nl02*nl12 eq 0) then begin
     lplot=0
     print,'Mismatch in number of phase data points or missing phase data points in LSB'
   endif
   uplot=1
   if (nu01 ne nu02 or nu01 ne nu12 or nu02 ne nu12 or nu01*nu02*nu12 eq 0) then begin
     uplot=0
     print,'Mismatch in number of phase data points or missing phase data points in USB'
   endif

   if (lplot+uplot ne 0) then begin
     l012=l01+l12-l02
     i_wl=where(wl01 lt 0 or wl12 lt 0 or wl02 lt 0, lcount)
     l012=uti_pha_180(l012)
     u012=u01+u12-u02
     i_wu=where(wu01 lt 0 or wu12 lt 0 or wu02 lt 0, ucount)
     u012=uti_pha_180(u012)
     ;x=indgen(n_elements(pbl))
     x=in[pil].int
     plot,x,l012,/nodata,yrange=[-180,180],/ystyle,ytitle='P H A S E',title=titlename,xtitle='I N T E G R A T I O N'
     if (lplot) then begin
        oplot,x,l012,psym=4,color=2
        if lcount gt 0 then oplot,x[i_wl],l012[i_wl],psym=7,color=1,symsize=2
     endif
     if (uplot) then begin
        oplot,x,u012,psym=4,color=3
        if ucount gt 0 then oplot,x[i_wu],u012[i_wu],psym=7,color=1,symsize=2
     endif
;     xyouts,max(x)/3,160,'Sideband L',color=2,charsize=8
;     xyouts,max(x)/1.5,160,'Sideband U',color=3,charsize=8
;     xyouts,max(x)/1.2,160,'Flagged Data',color=1,charsize=8
     xyouts,max(x)/1.1,160,'Sideband L',color=2,charsize=8
     xyouts,max(x)/1.1,140,'Sideband U',color=3,charsize=8
     xyouts,max(x)/1.1,120,'Flagged Data',color=1,charsize=8
   endif

   finish:

   if (not keyword_set(autocycle)) then begin
     temp1=''
     read,temp1,prompt='Another sets of antenna ? Y/[N]:'
     if (temp1 eq 'N' or temp1 eq 'n' or temp1 eq 'no' or temp1 eq 'NO') then j=1
   endif else begin
     print,'PRESS ANY KEY TO CONTINUE'
     keyin = get_kbrd(1)
     ac3=ac3+1
     if (ac3 eq ntels) then begin
       ac2=ac2+1
       ac3=ac2+1
       if (ac2 eq (ntels-1)) then begin
         ac1=ac1+1
         ac2=ac1+1
         ac3=ac2+1
         if (ac1 eq (ntels-2)) then j=1
       endif
     endif
   endelse

endwhile

end
