function rep_users,report,fn=fn,list=list,nline=nline
;
; generates a report with summary information on user accounts 
;
;        report : string array with output
;        list : keyword if set does a print command on output
;        fn   : keyword if set dump output to file fn
;
; eg. res=rep_users()
; eg. res=rep_users(report,fn='rpt',/list,nline=1000)
; eg. res=rep_users(report,/list,nline=1000)
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
; database sizes 
;
if e.debug then print,'multiple track log ...'
sql='select use#,init,lname+", "+fname,email' + $
    '  from mmdb..USERS d order by lname asc '
dbi_sql_tab,sql,ct_res,db_tab,fmt_n,widths_n,tab_c,fmt_c,widths_c,com_row
;
; db_tab has a list of the use#, init, lname, fname and email 
;
if ct_res lt 1 then begin
  print,'no users found'
  return,-1
endif
      label=['                  Users    ',' ', $
      'use# init      name                        email']
      fmt_ns=['','1x','4x', '', '', '','']
      fmt_n='(i7,a6,2x,a'+string(widths_n[2])+',2x,a'+string(widths_n[3])+')'
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

