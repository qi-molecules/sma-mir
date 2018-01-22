function rep_tra,traid,report,fn=fn,list=list
;
; generates a log for the specified track
; 
; paramters : traid = track number
;        report : string array with output
;        list : keyword if set does a print command on output
;        ff   : keyword if set dump output to file fn
;
; eg. res=rep_tra(6007,report,/list)
;     res=rep_tra(6002,report,fn='rpt',/list)
;
;
common global

tr_str=string(traid)

case 1 of
  (traid le 460L): str_mm='9192'
  (traid gt 460L) and (traid le 896L): str_mm='9293'
  (traid gt 896L) and (traid le 1644L): str_mm='9394'
  (traid gt 1644L) and (traid le 2262L): str_mm='9495'
  (traid gt 2262L) and (traid le 2920L): str_mm='9596'
  (traid gt 2920L) and (traid le 3682L): str_mm='9697'
  (traid gt 3683L) and (traid le 4457L): str_mm='9798'
  (traid gt 4457L) and (traid le 5171L): str_mm='9899'
  (traid gt 5171L) and (traid le 5956L): str_mm='9900'
  (traid gt 5956L) and (traid le 6911L): str_mm='0001'
  (traid gt 6911L) and (traid le 7865L): str_mm='0102'
else: str_mm='mm'
endcase

dashed_line='---------------------------------------------------------' + $
         '--------------------'

line_skips=[' ']
report=[' ']
nline=59
;
; track log
;
if e.debug then print,'track log ...'
sql='declare @tra int select @tra='+tr_str+' exec trasum;1 @tra'
res=dbi_sql_submit(sql)
traid=0 & ut_start='' & ut_stop='' & proid=0 & proposal='' & nint=0 
inhid_start=0L & inhid_stop=0L & quality='' & comments ='' 
reads,res[3],traid,ut_start,ut_stop,proid,proposal,nint,inhid_start,inhid_stop,quality,format='(1x,i12,1x,a25,2x,a25,i12,1x,a40,1x,i11,1x,i11,1x,i11,1x,a12,1x,a180)'
ut_start=strtrim(ut_start,2)
ut_stop=strtrim(ut_stop,2)
proposal1=strmid(proposal,0,20) & proposal2=strmid(proposal,20,20)
label=['                             Track Log',' ',systime()]
report=[report,string(label,format='(a)')]
report=[report,line_skips]
header_n="  tra#       ut range         pro#    title           int's  inh#   quality"
report=[report,string(header_n,format='(a)')]
report=[report,line_skips]
report=[report,string(traid,ut_start,proid,proposal1,nint,inhid_start,quality,format='(i6,2x,a19,2x,i4,2x,a20,i3,i8,1x,a6)')]
report=[report,string(ut_stop,proposal2,inhid_stop,format='(8x,a19,2x,4x,2x,a20,3x,i8,1x,6x)')]
comments=strtrim(comments,2)
leng=strlen(comments)
if leng gt 0 then begin
  report=[report,line_skips]
  for i=0,leng,60 do begin
    report=[report,strmid(comments,i*60,60)]
  endfor 
endif
report=[report,line_skips,dashed_line,line_skips]

;
; weather
;
if e.debug then print,'weather ...'
sql='declare @tra int select @tra='+tr_str+' exec weather;1 @tra'
res=dbi_sql_submit(sql)
if strpos(res[3],'NULL') eq -1 then begin
  r=str_sep(strcompress(res[3]),' ')
  tab_n=make_array(3,3,/string)
  tab_n[0,*]=r[1:3]
  tab_n[1,*]=r[4:6]
  tab_n[2,*]=r[7:9]
  fl=n_elements(report)+1
  label=['                                  Weather',' ']
  fmt_ns=['f5.2','i5','f5.2']
  fmt_n='(20x,a5,10x,a5,10x,a5)' 
  header_n="                   h2o (mm)        T (C)       wind (mph)"
  dbi_tab_for,tab_n,fmt_n,fmt_ns,0,tab_c,fmt_c,fmt_cs,com_row, $
    header_n,header_c,fl,nline,label=label,report
  report=[report,line_skips,dashed_line,line_skips]
endif

;
; configuration
;
if e.debug then print,'configuration ...'
sql='declare @tra int select @tra='+tr_str+' exec config'+str_mm+';1 @tra'
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
  fmt_ns=['3x','3x','f6.2','f6.2','f6.2','a3','i4','i4','a3','i4','i4','i4']
  fmt_n='(a7,2x,a8,2x,a6,2x,a5,2x,a5,2x,a3,2x,a4,1x,a4,2x,a3,2x,a4,' + $
        '1x,a4,2x,a4)' 
  header_n="config.  baseline  length   east  north  tel     e    n  " + $
           "tel     e    n  #int"
  dbi_tab_for,tab_n,fmt_n,fmt_ns,0,tab_c,fmt_c,fmt_cs,com_row, $
      header_n,header_c,fl,nline,label=label,report
  report=[report,line_skips,dashed_line,line_skips]
endif

;
; sources
;
if e.debug then print,'sources ...'
sql='declare @tra int select @tra='+tr_str+' exec sources;1 @tra'
dbi_sql_tab,sql,ct_res,tab_n,fmt_n,widths_n,tab_c,fmt_c,widths_c,com_row
if ct_res gt 0 then begin
  fl=n_elements(report)+1
  label=['                                 Sources',' ']
  fmt_ns=['4x','a11','a11','a11','i3','i3','a2','i8','i5']
  fmt_n='(a12,1x,a11,2x,a11,2x,a11,3x,a3,2x,a3,1x,a2,2x,a8,2x,a5)' 
  header_n="   source        pos           ra          dec        " + $
           "offsets       sou#    size  "
  dbi_tab_for,tab_n,fmt_n,fmt_ns,0,tab_c,fmt_c,fmt_cs,com_row, $
      header_n,header_c,fl,nline,label=label,report
  report=[report,line_skips,dashed_line,line_skips]
endif

;
; fluxes
;
if e.debug then print,'fluxes ...'
sql='declare @tra int select @tra='+tr_str+' exec fluxes'+str_mm+';1 @tra'
dbi_sql_tab,sql,ct_res,tab_n,fmt_n,widths_n,tab_c,fmt_c,widths_c,com_row
if ct_res gt 0 then begin
  fl=n_elements(report)+1 
  label=['                                 Fluxes',' ']
  fmt_ns=['4x','4x','a2','i4','i5','f5.2','f5.2','f4.2','i3']
  fmt_n='(a12,1x,a11,1x,a2,2x,a4,2x,a5,3x,a5,2x,a5,1x,"+-",a4,2x,a3)' 
  header_n="   source        pos     sb int's  bsl's   <coh>  <amp>" + $
           "   sig <phase>"
  header_c="                                           ------------------------"
  fmt_c='(43x,a5,2x,a5,9x,a3)'
  fmt_cs=['f5.2','f5.2','i3']
  dbi_tab_for,tab_n,fmt_n,fmt_ns,1,tab_c,fmt_c,fmt_cs,com_row, $
    header_n,header_c,fl,nline,label=label,report
  report=[report,line_skips,dashed_line,line_skips]
endif


;
; bands
;
if e.debug then print,'bands ...'
sql='declare @tra int select @tra='+tr_str+' exec bands'+str_mm+';1 @tra'
dbi_sql_tab,sql,ct_res,tab_n,fmt_n,widths_n,tab_c,fmt_c,widths_c,com_row
if ct_res gt 0 then begin
  fl=n_elements(report)+1
  label=['                         Spectrometer bands',' ']
  fmt_ns=['a2','a2','13x','a3','f7.3','f11.3','f6.1','i3','f6.2', $
          'i6','i3','i3'] 
  fmt_n='(a2,1x,a2,1x,a13,1x,a3,1x,a7,1x,a11,1x,a6,1x,a3,1x,a6,' + $
        '1x,a6,1x,a3," to ",a3)' 
  header_n="sb bd transition   tq     freq      vel      vres  nch" + $
           "  fsky  <tssb>     int "
  dbi_tab_for,tab_n,fmt_n,fmt_ns,0,tab_c,fmt_c,fmt_cs,com_row, $
    header_n,header_c,fl,nline,label=label,report
  report=[report,line_skips,dashed_line,line_skips]
endif

;
; integrations
;
if e.debug then print,'integrations ...'
sql='declare @tra int select @tra='+tr_str+' exec tralog'+str_mm+';1 @tra'
dbi_sql_tab,sql,ct_res,tab_n,fmt_n,widths_n,tab_c,fmt_c,widths_c,com_row
     label='integrations'
if ct_res gt 0 then begin
      fl=n_elements(report)+1 
      label=['                            Integrations',' ']
  fmt_ns=['i3','a12','a6','a3','a1','a1','a1','a1','a1','a1', $
          'i2','f5.2','i4','f4.2','f4.2','f6.2','a4','a2']
  fmt_n='(a4,1x,a12,1x,a6,1x,a3,1x,6a1,1x,a2,1x,a4,1x,a4,'+ $
        '2x,a4,"-",a4,2x,a4,1x,a4,1x,a2,1x)' 
  header_n=" int     source     pos  tq  qual's el  ha  tssb  "+ $
           "coherence <amp> bds bsl"
  dbi_tab_for,tab_n,fmt_n,fmt_ns,0,tab_c,fmt_c,fmt_cs,com_row, $
    header_n,header_c,fl,nline,label=label,report
  report=[report,line_skips,dashed_line,line_skips]
endif

;
; blanked data
;
if e.debug then print,'blanked data ...'
sql='declare @tra int select @tra='+tr_str+' exec blanks'+str_mm+';1 @tra'
dbi_sql_tab,sql,ct_res,tab_n,fmt_n,widths_n,tab_c,fmt_c,widths_c,com_row
if ct_res gt 0 then begin
  fl=n_elements(report)+1
  label=['                             Blanked Data',' ']
  fmt_ns=['4x','a5','a3','a3','a5','3x']
  fmt_n='(15x,a8,4x,a5,4x,a3," to ",a3,3x,a5,3x,a11)' 
  header_n="               baseline    bands     int range   "+ $
           "# int   # int-bands"
  dbi_tab_for,tab_n,fmt_n,fmt_ns,0,tab_c,fmt_c,fmt_cs,com_row, $
    header_n,header_c,fl,nline,label=label,report
  report=[report,line_skips]

  com=["Comments:   bnd's is the number of non-blanked bands averaged "+$
       "over all", $
  '                  baselines. (if this number changes during the track,', $
  '                  it may indicate intermittent problems.)']
  report=[report,com]
endif

if keyword_set(list) then for i=0,n_elements(report)-1 do print,report[i]
if keyword_set(fn) then begin
  result=fil_write(fn,report)
endif
 
return,1
end

