pro dbi_tab_for,tab_n,fmt_n,fmt_ns,com,tab_c,fmt_c,fmt_cs,com_row, $
    header_n,header_c,fl,nline,label=label,out_str
;
; Takes output of dbi_tab_lis and formats it w/ compute by included
;
; output is a string array with each row with pagination
;
; header_n   : column labels for tab_n
; com        : lodical signifying if there are compute results
; header_c   : column labels for compute by columns
; fmt_n      : outut format for normal data
; fmt_c      : output format for compute data
; out_st     : output string array
; fl         : line number of first line
; nline      : line number at bottom of page
; label      : keyword  : overall label header for output
;
; eg. sql='declare @tra int select @tra='+tr_str+' exec fluxes'+str_mm+';1 @tra'
;dbi_sql_tab,sql,tab_n,fmt_n,widths_n,tab_c,fmt_c,widths_c,com_row
;fl=1 & nline=70 
;label=['                                 Fluxes',' ']
;fmt_ns=['4x','4x','a2','i4','i5','f5.2','f5.1','f4.2','i3']
;fmt_n='(a12,1x,a11,1x,a2,2x,a4,2x,a5,3x,a5,2x,a5,1x,"+-",a4,2x,a3)' 
;header_n="   source        pos     sb int's  bsl's   <coh>  <amp>  sig<phase>"
;header_c="                                           ------------------------"
;fmt_c='(43x,a5,2x,a5,9x,a3)'
;fmt_cs=['f5.2','f5.1','i3']
;dbi_tab_for,tab_n,fmt_n,fmt_ns,1,tab_c,fmt_c,fmt_cs,com_row, $
;    header_n,header_c,fl,nline,label=label,report
;

; normal results
;
sizes_n=size(tab_n)
n_nor=sizes_n[1]
if sizes_n[0] eq 1 then n_row=1 else n_row=sizes_n[2]
;
; now read and format the strings uniformly
; f => allign decimals
; i => remove decimals
; x => pad on right w/ blanks
;
for i=0,n_nor-1 do begin
  if ((strpos(fmt_ns[i],'f') ne -1) or (strpos(fmt_ns[i],'e') ne -1) or $
      (strpos(fmt_ns[i],'g') ne -1)) then begin
    tab_n[i,*]=string(double(tab_n[i,*]),format='('+fmt_ns[i]+')')
  endif
  if strpos(fmt_ns[i],'i') ne -1 then begin
    tab_n[i,*]=string(long(tab_n[i,*]),format='('+fmt_ns[i]+')')
  endif
  if strpos(fmt_ns[i],'x') ne -1 then begin
    right_pad=string(' ',format='('+fmt_ns[i]+',a0)')
    right_pad=strmid(right_pad,0,strlen(right_pad)-1)
    tab_n[i,*]=tab_n[i,*]+right_pad
  endif
endfor
;
; compute_by results
;
if com then begin
  sizes_c=size(tab_c)
  if sizes_c[0] eq 1 then n_row_com=1 else n_row_com=sizes_c[2]
  n_com=sizes_c[1]
 j_com=make_array(n_row,/int,value=-1)
 j_com[com_row]=indgen(n_row_com)
;
; now read and format the strings uniformly
; f => allign decimals
; i => remove decimals
; x => pad on right w/ blanks
;
  for i=0,n_com-1 do begin
    if ((strpos(fmt_cs[i],'f') ne -1) or (strpos(fmt_cs[i],'e') ne -1) or $
        (strpos(fmt_cs[i],'g') ne -1)) then begin
      tab_c[i,*]=string(double(tab_c[i,*]),format='('+fmt_cs[i]+')')
    endif
    if strpos(fmt_cs[i],'i') ne -1 then begin
      tab_c[i,*]=string(long(tab_c[i,*]),format='('+fmt_cs[i]+')')
    endif
    if strpos(fmt_cs[i],'x') ne -1 then begin
      right_pad=string(' ',format='('+fmt_cs[i]+',a0)')
      right_pad=strmid(right_pad,0,strlen(right_pad)-1)
      tab_c[i,*]=tab_c[i,*]+right_pad
    endif
  endfor

endif

fl=(fl mod nline)
line=fl+1
if keyword_set(label)  then begin
   if (line+4) gt nline then begin 
     line=1
     out_str=[out_str,string(' ',format='(1h0,a)')]
   endif
  out_str=[out_str,string(label,format='(a)')]
  line=line+n_elements(label)
endif
dashes=make_array(n_nor,/string,value='--------------------------'+ $
       '-----------------------------------------------------------')
nor_h=1
j=0
while j le n_row-1 do begin
   if com then begin
     if j_com[j] ne -1 and (line+6) gt nline then goto, page_break 
   endif
   if (line+1) gt nline then goto, page_break 
   if line eq 1 then nor_h=1
   if nor_h then begin
     out_str=[out_str,header_n]
     out_str=[out_str,string(dashes,format=fmt_n)]
     line=line+2  
     nor_h=0
   endif
   if n_row gt 1 then begin
     out_str=[out_str,string(tab_n[*,j],format=fmt_n)]
   endif else begin
     out_str=[out_str,string(tab_n[*],format=fmt_n)]
   endelse
   line=line+1   
   if com then begin
     if j_com[j] ne -1 then begin
       out_str=[out_str,header_c]
       if n_row_com gt 1 then begin
         out_str=[out_str,string(tab_c[*,j_com[j]],format=fmt_c)]
       endif else begin
         out_str=[out_str,string(tab_c[*],format=fmt_c)]
       endelse
       out_str=[out_str,string(' ',format='(1h ,a)')]
       line=line+2
     endif 
   endif
   if line gt nline then begin
   page_break : 
     line=1
     out_str=[out_str,string('',format='(a)')]
     j=((j-1) > 0)
   endif
   j=j+1  
endwhile

return
end
