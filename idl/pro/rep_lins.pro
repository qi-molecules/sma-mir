function rep_lins,report,fn=fn,list=list,nline=nline
;
; generates a report with summary information on spectral lines 
;
;        report : string array with output
;        list : keyword if set does a print command on output
;        fn   : keyword if set dump output to file fn
;
; eg. res=rep_lins()
; eg. res=rep_lins(report,fn='rpt',/list,nline=1000)
; eg. res=rep_lins(report,/list,nline=1000)
; eg. res=rep_lins(/list,nline=1000)
;
;
common global

dashed_line='---------------------------------------------------------' + $
         '------------------'
fl=1
line_skips=[' ']
report=[' ']
if not keyword_set(nline) then nline=59
;
; line list 
;
if e.debug then print,'multiple track log ...'
sql='select lin#,trans,rfreq,mol,levels' + $
    '  from mmdb..LIN d where rfreq > 0. and lin# > 0 order by rfreq asc '
dbi_sql_tab,sql,ct_res,db_tab,fmt_n,widths_n,tab_c,fmt_c,widths_c,com_row
;
; db_tab has a list of the  lin#,trans,refreq,mol,levels
;
if ct_res lt 1 then begin
  print,'no lines found'
  return,-1
endif
      label=['                         Spectral Lines    ',' ', $
      '   lin#   transition          rest freq. molecule          levels     ']
      fmt_ns=['','1x','4x', '', '', '','']
      fmt_n='(i7,2x,a'+string(widths_n[1])+',2x,f10.6,2x,a15'+ $
	',2x,a10)'
      header_n=" "
      dbi_tab_for,db_tab,fmt_n,fmt_ns,0,tab_c,fmt_c,fmt_cs,com_row, $
        header_n,header_c,fl,nline,label=label,report
      report=[report,line_skips]

if keyword_set(list) then for i=0,n_elements(report)-1 do print,report[i]
if keyword_set(fn) then begin
  result=fil_write(fn,report)
endif

return,1
end

