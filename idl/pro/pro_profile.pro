pro pro_profile ,file=file,number=number,start_timer=start_timer,$
                   stop_timer=stop_timer,start_memory=start_memory, $
                   stop_memory=stop_memory,show=show
;
; Generates timing and memory profile for .pro file. 
;
; eg. : pro_profile ,file='dbi_head_read'
; eg. : then run the program
; eg. : pro_profile ,/show,number=10  ; to print 10 largest time lines
;

; read in .pro file
; set up timer array
; insert pro_profile calls
;
common timer_com, dts,n_timer,t1,m1,m2
if keyword_set(file) then begin
  if not fil_read(file+'.pro',lines,nlines) then goto, en
;
; find first non-comment line and truncate file to last 'end'
;
  out_lines=make_array(3*nlines,/string)
  for i=0,nlines-1 do begin
    if strpos(strlowcase(strtrim(lines(i),1)),';',0) ne 0 then begin
      if (strpos(strlowcase(strtrim(lines(i),1)),'fun',0) ne -1 or $
         strpos(strlowcase(strtrim(lines(i),1)),'pro',0) ne -1)  then goto, begfound
    endif
  endfor
  begfound: beg=i
  if beg ne 0 then out_lines[0:beg-1]=lines[0:beg-1]
  n_out_line=beg-1L
  for i=nlines-1,beg,-1 do begin
    if strpos(strlowcase(lines(i)),'end',0) ne -1 then goto, endfound
  endfor
  endfound: nlines=i-beg+1
  lines=lines(beg:i)
  n_timer=-1L
  start_active=0
  start_routine=0L
  begin_case=0
  for i=0,nlines-2 do begin
    lines(i)=strlowcase(lines(i))
    if strtrim(lines(i),2) eq '' then goto, blank_line
    before=1
    after=1 
    cont_this=strpos(lines(i),'$',0) & cont_prev=-1
    if i gt 0 then cont_prev=strpos(lines(i-1),'$',0)
    if cont_this ne -1 and cont_this eq -1 then begin
      goto, write_line  
    endif
    if (cont_this eq -1 and start_routine ne 0L) then begin
      if (cont_prev eq -1)then start_routine=start_routine+1
    endif
    if start_routine eq 2 then begin
      n_out_line=n_out_line+1L
      out_lines(n_out_line)='pro_profile ,/start_memory'
      start_routine=start_routine+1
    endif
    if strpos(strtrim(lines(i),1),';',0) eq 0 then before=0
    if (strpos(lines(i),'case ',0) ne -1) and $
       (strpos(lines(i),'endcase',0) eq -1) then begin_case=1
    if begin_case and (strpos(lines(i),'endcase',0) ne -1) then begin_case=0
    if strpos(lines(i),'return',0) ne -1 then begin
      if strpos(lines(i),'return',0) eq 0 then before=0
      n_out_line=n_out_line+1L
      out_lines(n_out_line)='pro_profile ,/stop_memory'
    endif
    if strpos(lines(i),'stop',0) ne -1 then before=0
    if (strpos(lines(i),':',0) ne -1 and begin_case eq 1) then before=0
;    if (strpos(lines(i),'if',0) ne -1 and $
;        strpos(lines(i),'begin',0) ne -1) then before=0
    if strpos(lines(i),'endif',0) ne -1 then before=0
    if strpos(lines(i),'end',0) ne -1 then before=0
    if strpos(lines(i),'else',0) ne -1 then before=0
    if (strpos(lines(i),'for',0) ne -1 and $
        strpos(lines(i),'begin',0) ne -1) then before=0
    if strpos(lines(i),'endfor',0) ne -1 then before=0
    if (strpos(lines(i),'while',0) ne -1 and $
        strpos(lines(i),'begin',0) ne -1) then before=0
    if strpos(lines(i),'endwhile',0) ne -1 then before=0
    if cont_prev ne -1 then before=0
    if (strpos(lines(i),'pro',0) ne -1 or strpos(lines(i),'fun',0) ne -1) then before=0
    if ((strpos(lines(i),'pro',0) ne -1 or strpos(lines(i),'fun',0) ne -1) $
         and start_routine eq 0L) then start_routine=1
    if strpos(lines(i),'$',0) gt 0 then after=0
    if begin_case then after=0
    if start_routine le 1 then before=0
    if before and not start_active then begin
     n_timer=n_timer+1L
     start_active=1
     n_out_line=n_out_line+1L
     out_lines(n_out_line)='pro_profile ,/start_timer,number='+ $
                            strtrim(string(n_timer),2L)
    endif

    write_line: n_out_line=n_out_line+1L
    out_lines(n_out_line)=lines(i)
    if not start_active then after=0
    if after then begin
     n_out_line=n_out_line+1L
     out_lines(n_out_line)='pro_profile ,/stop_timer,number='+ $
                            strtrim(string(n_timer),2L)
     start_active=0
    endif
    blank_line: ;
  endfor
  n_out_line=n_out_line+1L
  out_lines(n_out_line)='pro_profile ,/stop_memory'
  n_out_line=n_out_line+1L
  out_lines(n_out_line)=lines(nlines-1L)
  out_lines=out_lines(0L:n_out_line)
;
; now compile routine
;
if not fil_write('temp.pro',out_lines) then goto, en
print,' '
print,'*** to recompile the modified ',file+'.pro, enter at idl prompt:'
print,'.compile temp.pro'
print,'*** if there are compilation errors, you can edit temp.pro directly'
print,'*** then run the program'
print,'*** pro_profile ,/show,number=10  ; to print 10 largest time lines'

dts=make_array(n_timer+1L,/double)
endif
;
; start and stop timers and memory
; 
if keyword_set(start_timer) then begin
 t1=systime(1)  
endif
if keyword_set(stop_timer) then begin
 dts(number)=dts(number)+systime(1)-t1
endif
if keyword_set(start_memory) then begin
 help,/memory,output=m1
endif
if keyword_set(stop_memory) then begin
 help,/memory,output=m2
endif
;
; display results
;
if keyword_set(show) then begin
  if not keyword_set(number) then number=n_elements(dts)
  if not fil_read('temp.pro',lines,nlines) then return
  reads,m1,mr1,format='(19x,f10.0)' & reads,m2,mr2,format='(19x,f10.0)'
  per_cent=100.*dts/total(dts)  
  j=reverse(sort(dts))
  print,' '
  print,'   total time  (sec) :',total(dts)
  print,'   memory used (Mb)  :   ',(mr2-mr1)/1000000., $
        ' (includes called routines)'
  print,' '
  for i=0,min([number-1,n_elements(dts)-1]) do begin
    timer_no=j(i)
    istart = -1 & istop = -1
    for l=0,nlines-1 do begin
      if (istart ne -1 and istop ne -1) then goto, pr
        if (strpos(lines(l),'/start_timer,number='+ $
            strtrim(string(timer_no),2L),0) ne -1) then istart=l+1 
        if (strpos(lines(l),'/stop_timer,number='+ $
            strtrim(string(timer_no),2L),0) ne -1) then istop=l-1
    endfor
    pr: print,format='("** ",f5.2," %,",f6.2," sec => ",a,:/,(25x,a))', $
              per_cent(timer_no),dts(timer_no),lines(istart:istop)
  endfor
endif
en:

end

