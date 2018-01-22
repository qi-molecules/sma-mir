function rep_cons,report,fn=fn,list=list,nline=nline
;
; generates a report with summary information on configurations 
;
;        report : string array with output
;        list : keyword if set does a print command on output
;        fn   : keyword if set dump output to file fn
;
; eg. res=rep_cons()
; eg. res=rep_cons(report,fn='rpt',/list,nline=1000)
; eg. res=rep_cons(report,/list,nline=1000)
; eg. res=rep_cons(/list,nline=1000)
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
; configurations 
;
if e.debug then print,'multiple track log ...'
sql='select con#,cocd,ut,ses#,utstop' + $
    '  from mmdb..CON d order by con# asc '
dbi_sql_tab,sql,ct_res,db_tab,fmt_n,widths_n,tab_c,fmt_c,widths_c,com_row
;
; db_tab has a list of the con#,cocd,ut,ses#,utstop 
;
if ct_res lt 1 then begin
  print,'no configurations found'
  return,-1
endif
      label=['             Configurations    ',' ', $
      ' con#  config.      UT            ses#      UT stop']
      fmt_ns=['','1x','4x', '', '2x', '','']
      fmt_n='(i4,2x,a5,2x,a19,2x,i3,2x,a19)'
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

