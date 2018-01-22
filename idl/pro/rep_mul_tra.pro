function rep_mul_tra,traid_start=traid_start,traid_stop=traid_stop, $
     proid=proid,source=source,report,fn=fn,list=list
;
; generates a report with summaries for all tracks 
; satisfying criteria w.r.t. project and sources
; 
; paramters : traid_start = starting track number
;             traid_stop  = last track number
;             proid       = project id 
;             source      = source name
;
;        report : string array with output
;        list : keyword if set does a print command on output
;        fn   : keyword if set dump output to file fn
;
; eg. res=rep_mul_tra(traid_start=5900,traid_stop=6010,report,/list)
; eg. res=rep_mul_tra(traid_start=5942,traid_stop=5947,report,/list)
; eg. res=rep_mul_tra(proid=687,fn=rpt687,report,/list)
;
;
common global


dashed_line='---------------------------------------------------------' + $
         '---------------'

line_skips=[' ']
report=[' ']
nline=59
;
; track log
;
if e.debug then print,'multiple track log ...'
sql='SELECT con#=t.con#,pro#=t.pro#,tra#=t.tra# from TRACKS t,TRACKS t1 ' + $
'where t.tra#=t1.tra# ' 
if keyword_set(source) then sql = sql + 'and t1.source like "'+ source +'"'
if keyword_set(traid_start) then sql = sql + $
         ' and t.tra# >=' + string(traid_start)
if keyword_set(traid_stop) then sql = sql + $
         ' and t.tra# <=' + string(traid_stop)
if keyword_set(proid) then sql = sql + $
         ' and t.pro#  =' + string(proid)
sql = sql + ' group by t.con#,t.pro#,t.tra# order by t.con#,t.tra#,t.pro#'

dbi_sql_tab,sql,ct_res,track_tab,fmt_n,widths_n,tab_c,fmt_c,widths_c,com_row
;
; track_tab has a list of the conid, proid, and traid 
;
if ct_res lt 1 then begin
  print,'no tracks found'
  return,-1
endif
sizes=size(track_tab)
if sizes[0] gt 1 then begin 
  conids=reform(track_tab[0,*])
  proids=reform(track_tab[1,*])
  traids=reform(track_tab[2,*])
  ntrack=sizes[2]
endif else begin
  conids=track_tab[0]
  proids=track_tab[1]
  traids=track_tab[2]
  ntrack=1
endelse

cur_conid=0 
for i=0,ntrack-1 do begin
;
; if the configuration changed, then list it out
;
  if cur_conid ne conids[i] then begin
    cur_conid=conids[i]
    if e.debug then print,'configuration ...'
    sql='declare @con int select @con='+string(cur_conid)+' exec config;1 @con'
    dbi_sql_tab,sql,ct_res,tab_n,fmt_n,widths_n,tab_c,fmt_c,widths_c,com_row
    if ct_res gt 0 then begin
      fl=n_elements(report)+1
      col9=tab_n[9,*] & col10=tab_n[10,*]
      tab_n[9,*]=tab_n[7,*] & tab_n[10,*]=tab_n[8,*]
      tab_n[8,*]=col10 & tab_n[7,*]=tab_n[6,*]
      tab_n[6,*]=tab_n[5,*] & tab_n[5,*]=col9
      label=['                            Configuration',' ', $
       '                              vector           location'+ $
       '        location', $
       '                           ------------        '+ $
       '--------        --------']
      fmt_ns=['3x','3x','f6.1','i5','i5','a3','i4','i4','a3','i4','i4']
      fmt_n='(a7,2x,a8,2x,a6,2x,a5,2x,a5,2x,a3,2x,a4,1x,a4,2x,a3,2x,a4,' + $
        '1x,a4)' 
      header_n="config.  baseline  length   east  north  tel     e    n  " + $
           "tel     e    n "
      dbi_tab_for,tab_n,fmt_n,fmt_ns,0,tab_c,fmt_c,fmt_cs,com_row, $
        header_n,header_c,fl,nline,label=label,report
      report=[report,line_skips,dashed_line,line_skips]
    endif    
  endif
;
; get weather and mean values for track
;
  sql='SELECT tra#=t.tra#,quality=tr.quality,status=tr.status, ' + $
      'comments=max(tr.comments),coh=t.coh,h2o=t.h2o,tssb=min(t.tssb)' + $
      ',sbr=t.sbr,fsky=avg(fsky),date=convert(char(12),tr.ut,107) ' + $
      'FROM TRA tr,TRACKS t where tr.tra#=t.tra# and ' + $
      ' t.tra#='+string(traids[i])+ $
      ' group by t.tra#,tr.quality,tr.status,t.coh,t.h2o, '+ $
      ' t.sbr,tr.ut order by t.tra#
  dbi_sql_tab,sql,ct_res,tab_n,fmt_n,widths_n,tab_c,fmt_c,widths_c,com_row
  report=[report,line_skips]
  j=where(tab_n eq 'NULL',ct)
  if ct gt 0 then tab_n[j]='0'
  if ct_res gt 0 then begin
    report=[report,string(traids[i],tab_n[9,0],tab_n[1,0], $
      proids[i],format='("track: ",i6,2x,a12,2x,a8,2x,"pro#:  ",i4)')]
    report=[report,string(tab_n[5,0],tab_n[4,0],tab_n[6,0], $
      format='(15x,"<h2o>: ",f5.2,2x," <coh>: ",f5.2,2x,"<tssb>:",i6)')]
    report=[report,string(tab_n[7,0],tab_n[8,0], $
      format='(15x,"<sbr>: ",f5.2,2x,"<fsky>: ",f5.1," GHz")')]
  endif else print,traids[i],format='("track: ",i6,2x," no weather")'
;
; get source listings 
;  
    sql='SELECT soupos=t.source+t.pos,nint=t.nint,fbase=t.fbase,' + $
      'hastart=t.hastart,hastop=t.hastop FROM  TRACKS t where ' + $
      'tra#='+string(traids[i])+' group by t.source,t.pos,' + $
      't.nint,t.fbase,t.hastart,t.hastop order by t.nint desc'
    dbi_sql_tab,sql,ct_res,tab_n,fmt_n,widths_n,tab_c,fmt_c,widths_c,com_row
  if ct_res gt 0 then begin
    label=['               --------------------------------------------']
    fmt_ns=['7x','i4','i4','f6.2','f6.2']
    fmt_n='(15x,a15,2x,a5,2x,a8,2x,a5," to ",a5)' 
    header_n="                 source         # int  <# base> "+ $
           "    HA range "
    dbi_tab_for,tab_n,fmt_n,fmt_ns,0,tab_c,fmt_c,fmt_cs,com_row, $
      header_n,header_c,fl,nline,label=label,report
    report=[report,line_skips,dashed_line,line_skips]
  endif 
endfor

if keyword_set(list) then for i=0,n_elements(report)-1 do print,report[i]
if keyword_set(fn) then begin
  result=fil_write(fn,report)
endif

return,1
end

