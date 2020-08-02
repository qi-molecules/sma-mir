pro uti_respike, dir=dir, channel=channel, verbose=verbose

common global
common data_set


file=dir+'/spike_read'

nlines=file_lines(file)
temp=fltarr(9,nlines)
openr, unit, file,/get_lun
readf,unit,temp
close, unit & free_lun,unit
scan=reform(strcompress(string(fix(temp[0,*])),/remove))
a1=reform(strcompress(string(fix(temp[1,*])),/remove))
a2=reform(strcompress(string(fix(temp[2,*])),/remove))
rx=reform(strcompress(string(fix(temp[3,*])),/remove))
iband=reform(fix(temp[4,*]))
sb=reform(strcompress(string(fix(temp[5,*])),/remove))
ispike=reform(fix(temp[6,*]))
f1=reform(temp[7,*])
f2=reform(temp[8,*])

;FMT='A,A,A,A,I,A,I,F,F'
;readcol,file, F=FMT, scan,a1,a2,rx,iband,sb,ispike,f1,f2
; scan: scans
; a1-a2: baselines
; rx: receiver code
; iband: chunk
; sb: sideband
; ispike: spike channels
; f1,f2 spike real and imaginary

; chanlist [int,rec,sb,baseline,band,channel] 
; chanlist=[0,0,0,0,0,0]

irec=uti_distinct(bl.irec,nrec,/many)

if (nrec le 2) and (nrec gt 0) then begin
   if nrec eq 2 then begin
      itmp=where(rx eq '0', count)
      if count gt 0 then rx[itmp]=strcompress(string(irec[0]),/remove)
      itmp=where(rx eq '3', count)
      if count gt 0 then rx[itmp]=strcompress(string(irec[1]),/remove)
   endif else begin
      if irec[0] ge 2 then itmp=where(rx eq '3', count) else itmp=where(rx eq '0',count)
      if count gt 0 then rx[itmp]=strcompress(string(irec[0]),/remove)         
   endelse
endif else begin
   print,"Number of receivers is", nrec
   print,"Quit !!!"
   return
endelse


band=strcompress(string(iband+1),/remove)


bsl=a1+'-'+a2
nchan=n_elements(channel)

for i=0, nchan-1 do begin
   ichan=where(ispike eq channel[i],count)
   if count gt 0 then begin
      for j=0, count - 1 do begin
         ibsl=where(c.blcd eq bsl[ichan[j]],nbsl) ; array of 1
         blcd=strcompress(string(ibsl[0]),/remove)
;         print,'scan,',scan[ichan[j]]
;         print,'receiver id,', rx[ichan[j]]
;         print,'sideband id,', sb[ichan[j]]
;         print,'baseline id, ', blcd
;         print,'band id,', band[ichan[j]]
;         print,'channel', channel[i]
         
         command='"int" eq "'+scan[ichan[j]]+'" and "irec" eq "'+rx[ichan[j]]+'" and "isb" eq "'+sb[ichan[j]]+'" and "iband" eq "'+band[ichan[j]]+'" and "iblcd" eq "'+blcd+'"'
;         print,command
         result=dat_list(s_l,command,/no_notify,/reset)
         if (result gt 0) and finite(f1[ichan[j]]) then begin
;            ptr=pcl+channel[i]-1L 
            ptr=pcl+channel[i] ; spike counting from 0
            ch[ptr]=complex(f1[ichan[j]],f2[ichan[j]])
            if keyword_set(verbose) then print,'Spike restored for scan ', scan[ichan[j]], ', receiver ',c.rec[rx[ichan[j]]], ', sideband ',c.sb[sb[ichan[j]]], ', baseline ',c.blcd[blcd], ', chunk ',c.band[band[ichan[j]]], ', channel ',channel[i]
         endif
      endfor
   endif
endfor

end
