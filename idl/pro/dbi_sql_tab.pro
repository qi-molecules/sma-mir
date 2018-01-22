pro dbi_sql_tab ,sql_statement,ct,tab_n,fmt_n,widths_n,tab_c,fmt_c, $
      widths_c,com_row
;
; Submits a sql statement to get a 2-d table of string output
; 
; the output string table will have string columns left-justified
; and numeric columns right justified with both cut donw to their
; minimum width yet still alligned vertically.
;
; tab_n,fmt_n,widths_n : ouput table, fmt, width for normal rows
; tab_c,fmt_c,widths_c : ouput table, fmt, width for compute rows
; com_row : row number of normal row before teh compute rows
;
; eg. : sql='declare @tra int select @tra=6002 exec fluxes0001;1 @tra'
;       sql='declare @tra int select @tra=6002 exec sources;1 @tra'
;       sql='declare @tra int select @tra=6002 exec blanks0001;1 @tra'
;       sql='declare @tra int select @tra=6002 exec weather;1 @tra'
;       sql='declare @tra int select @tra=6002 exec config0001;1 @tra'
;       sql='declare @tra int select @tra=6002 exec bands0001;1 @tra'
;       sql='declare @tra int select @tra=6007 exec tralog0001;1 @tra'
;       sql='declare @tra int select @tra=6002 exec trasum;1 @tra'

;       dbi_sql_tab,sql,tab_n,fmt_n,widths_n,tab_c,fmt_c,widths_c,com_row
; to print the output :
;
common global
res=dbi_sql_submit(sql_statement)

if n_elements(res) lt 5 then begin
  if e.debug then print,sql_statement
  print,'no results for query  :',res
  tab=make_array(1,1,/string) & fmt=''
  ct=0
  return
endif
;
; take out blank, 'Command has been aborted' and 'row(s) affected' lines
;
j_bl=where(strtrim(res) ne '',ct_non_bl)
if ct_non_bl gt 0 then res=res[j_bl]
j_ca=where(strpos(res,'Command has been aborted') eq -1,ct_non_ca)
if ct_non_ca gt 0 then res=res[j_ca]
j_ra=where(strpos(res,'affected') eq -1,ct_non_ra)
if ct_non_ra gt 0 then res=res[j_ra]
if strpos(res[n_elements(res)-1],'return status') ne -1 then res=res[0:n_elements(res)-2]
;
; a little complex because we try to handle compute clause results
; separate the normal rows from the compute results
;
;
;  'Compute Result:'
;
j_cr=where(strpos(res,'Compute Result:') ne -1,ct_comp)
if ct_comp gt 0 then begin
  com_dashed_line=res[j_cr[0]+1]
  js=lindgen(n_elements(res))
  jst=make_array(n_elements(res),/int)
  js[j_cr]=-10L    ; rows with 'Compute results:'        : js = -10   
  js[j_cr+1L]=-20L ; rows w/ dashed line                 : js = -20
  js[j_cr+2L]=-30L ; actual compute reuslts              : js = -30
  jst[j_cr-1L]=1
  com_res=res[j_cr+2L]   
  j=where(js gt -1)
  res=res[js[j]]
  jst=jst[js[j]]
  j=where(jst eq 1) 
  com_row=j  
endif

js=lindgen(n_elements(res))
j_dash=where(strpos(res,'----') ne -1)
nor_dashed_line=res[j_dash[0]]
js[j_dash]=-10L    ; rows with dashes 
js[j_dash-1]=-10L  ; rows w/ labels
if ct_comp gt 0 then jst=make_array(n_elements(res),/int)
j=where(js gt -1,ct_nor)
nor_res=res[js[j]]
if ct_comp gt 0 then begin
  jst[com_row]=1
  jst=jst[js[j]]
  j=where(jst eq 1) 
  com_row=j
endif  


nloop=1
if ct_comp ge 1 then nloop = 2
for il=0,nloop-1 do begin
  if il eq 0 then begin
    res=nor_res &   ir=0 & dashed_line=nor_dashed_line
  endif else begin
    res=com_res & ir=0 & dashed_line=com_dashed_line
  endelse
; 
; trigger the fields based on the -----'s separated by blanks
; cf is a pointer to first character in each filed
;
r=str_sep(dashed_line,' ')
lengs=strlen(r)
j=where(lengs gt 0,ct)
cf=indgen(n_elements(r)) 
for i=1,n_elements(lengs)-1 do cf[i]=cf[i-1]+lengs[i-1]+1 
cf=cf[j] & lengs=lengs[j]
tab=make_array(n_elements(cf),n_elements(res),/string)
js=indgen(n_elements(lengs))
;
; now make the string array
;
for i=0,n_elements(res)-1 do begin  
  for j=0,n_elements(lengs)-1 do tab[j,i]=strmid(res[i],cf[j],lengs[j]) 
endfor
; now cut each string column back to the max length in each column
; and do justifying
;
col=make_array(n_elements(res),/string)
fmt='(1x,'
widths=make_array(n_elements(lengs),/int)
for i=0,n_elements(lengs)-1 do begin
  col=reform(tab[i,*])
  col_tr=strtrim(col,2)
  max_width=max(strlen(col_tr))
  widths[i]=max_width
  fmt_i='a'+strtrim(string(max_width),2)
  col_tr=strtrim(col,1)
  reads,col_tr,col,format='('+fmt_i+')'
  tab[i,*]=col
  fmt=fmt+fmt_i
  if i ne (n_elements(lengs)-1) then fmt=fmt+',2x,'
endfor
fmt=fmt+')'
  if il eq 0 then begin
    fmt_n=fmt
    tab_n=tab
    widths_n=widths
  endif else begin
    fmt_c=fmt
    tab_c=tab
    widths_c=widths
  endelse
endfor
sizes_n=size(tab_n)
sizes_c=size(tab_c)
ct=sizes_n[1]

if e.debug then begin
  ny_n=1 
  if sizes_n[0] eq 2 then ny_n=sizes_n[2]
  print,'normal table ...'
  for i=0,ny_n-1 do print,i,tab_n[*,i]
  if nloop eq 2 then begin
    print,'compute_by table ...'
    ny_c=1 
   if sizes_c[0] eq 2 then ny_c=sizes_c[2]
    for i=0,ny_c-1 do print,i,tab_c[*,i]
    print,'compute table links to normal ...'
    print,com_row
  endif
endif

return
end
