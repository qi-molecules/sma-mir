function rep_db,report,fn=fn,list=list
;
; generates a report with summary information on database
; devices and databases 
;
;        report : string array with output
;        list : keyword if set does a print command on output
;        fn   : keyword if set dump output to file fn
;
; eg. res=rep_db()
; eg. res=rep_db(report,fn='rpt',/list)
;
;
common global

dashed_line='---------------------------------------------------------' + $
         '------------------'
fl=1
line_skips=[' ']
report=[' ']
nline=59
;
; database sizes 
;
if e.debug then print,'multiple track log ...'
sql='select distinct dbname  = d.name,' + $
    ' dbdate = d.crdate , dbid = d.dbid ,' + $
    'status = d.status  from master..sysdatabases d, ' + $
     'master..sysusages u ' + $
     ' where  ' + $ 
     ' d.dbid=u.dbid                order by d.name asc '
user_name=e.user_name & password=e.password
e.user_name='sa' & e.password='sasasa'
dbi_sql_tab,sql,ct_res,db_tab,fmt_n,widths_n,tab_c,fmt_c,widths_c,com_row
;
; db_tab has a list of the name,date,dbid,status and comments 
;
if ct_res lt 1 then begin
  print,'no databases found'
  e.user_name=user_name & e.password=password
  return,-1
endif
sizes=size(db_tab)
nrows=sizes[2]
db_names=db_tab[0,*] & dbdates=db_tab[1,*]
dbids=db_tab[2,*] & dbstatuss=db_tab[3,*]
db_data=make_array(1,nrows,/string) & db_log=make_array(1,nrows,/string)
   sql_used=' select @dbrsize=sum(reserved_pgs(id,doampg)+ ' + $
       ' reserved_pgs(id,ioampg)) from sysindexes where indid!=255 ' + $
       ' and id in (select id from sysobjects where ' + $
       ' name != "syslogs") ' + $
       ' select @dbrisize=sum(reserved_pgs(id,doampg)+ ' + $
       ' reserved_pgs(id,ioampg)) from sysindexes where indid=255 ' + $
       ' and id in (select id from sysobjects where ' + $
       ' name != "syslogs") ' + $ 
       ' select @dblogsize=sum(reserved_pgs(id,doampg)+ ' + $
       ' reserved_pgs(id,ioampg)) from sysindexes where id in ' + $ 
       ' (select id from sysobjects where name = "syslogs") ' + $
       ' select @dalloc,@lalloc,@dbrsize/512.0,@dbrisize/512.0,' + $
       ' @dblogsize/512.0'
db_tab1=make_array(11,nrows,/string)
db_tab1[0,0]=db_tab
for i=0,nrows-1 do begin
  sql=' declare @dalloc int,@lalloc int, @dbrsize int, ' + $
    ' @dbrisize int, @dblogsize int ' + $ 
    ' select @dalloc = convert(int,sum(size)/512.0+0.5) from ' + $
    ' master..sysusages u where u.dbid='+dbids[i] + $
    ' and (u.segmap = 3  or  u.segmap = 7) ' + $  
    ' select @lalloc = convert(int,sum(size)/512.0+0.5) from ' + $
    ' master..sysusages u where u.dbid='+dbids[i] + $
    ' and u.segmap = 4 '
  sql =['use '+db_names[i],'go', sql + sql_used]
  dbi_sql_tab,sql,ct_res,db_tab2,fmt_n,widths_n,tab_c,fmt_c,widths_c,com_row
  j=where(db_tab1 eq 'NULL',ct)
  if ct gt 0 then db_tab1[j]='0'
  j=where(db_tab2 eq 'NULL',ct)
  if ct gt 0 then db_tab2[j]='0'
  db_tab1[4:8,i]=db_tab2
  db_tab1[9,i]=string(total(long([db_tab2[2:4]])))
  db_tab1[10,i]=string(total(long(db_tab2[0:1]))-total(long([db_tab2[2:4]])))
  db_data[i]=db_tab[0] & db_log[i]=db_tab[1] 
endfor


tab_n=[db_names,db_data,db_log]
tab_n=db_tab1[*,0:nrows-1]
sizes=size(tab_n)
      label=['                              Databases    ',' ', $
      '                                      alloc.(Mb)    ' + $
      '      used (Mb) ', $
      '                                      ----------' + $
      '   ----------------------',$
      ' name                 date   id stat   data  log   ' + $
      'data  image logs total    free']
      fmt_ns=['3x',  '3x', 'i2', 'i3', 'i5', 'i3', $
              'i6', 'i6', 'i3',  'i6','i6']
      fmt_n='(a14,2x,a11,2x,a2,2x,a3,2x,a5,2x,a3,1x,' + $
              'a6,1x,a6,1x,a3,1x,a6,2x,a6)' 
      header_n=" "
      dbi_tab_for,tab_n,fmt_n,fmt_ns,0,tab_c,fmt_c,fmt_cs,com_row, $
        header_n,header_c,fl,nline,label=label,report
      report=[report,line_skips,'    status : 4 -- bcp, 8 -- trunc log, 32 db load, 256 -- suspect',dashed_line,line_skips]
;
; database devices 
;d.cntrltype != 3 and 
sql='select devname  = d.name ,devpname=d.phyname,' + $
    ' devtype  = d.cntrltype,' + $
    ' devsize=convert(int,(d.high-d.low)/512.0+0.5),' + $
    ' devused=convert(int,sum(u.size)/512.0+0.5)' + $
    ' from master..sysdevices d, master..sysusages u ' + $
    ' where   ' + $
    ' u.vstart <= d.high and u.vstart >= d.low group by d.name' + $
    ' order by d.name,d.cntrltype '  
dbi_sql_tab,sql,ct_res,db_tab,fmt_n,widths_n,tab_c,fmt_c,widths_c,com_row
if ct_res lt 1 then begin
  print,'no devices found'
  e.user_name=user_name & e.password=password
  return,-1
endif
db_tab=[db_tab,string([(long(db_tab[3,*])-long(db_tab[4,*]) > 0)])]
;
; db_tab has a list of the device name, physical name, type, size and used 
;
      label=['                                Devices    ',' ', $
      'logical             physical               type    size  allocated    free']
      fmt_ns=['8x','4x', '', 'i7', 'i7','i7']
      fmt_n='(a18,2x,a22,2x,a3,2x,a7,2x,a7,2x,a7)' 
      header_n=" "
      dbi_tab_for,db_tab,fmt_n,fmt_ns,0,tab_c,fmt_c,fmt_cs,com_row, $
        header_n,header_c,fl,nline,label=label,report
      report=[report,line_skips, $
        '    types : 0 -- disk, 2 -- streaming tape, 3 -- tape dump device', $
        dashed_line,line_skips]

if keyword_set(list) then for i=0,n_elements(report)-1 do print,report[i]
if keyword_set(fn) then begin
  result=fil_write(fn,report)
endif
e.user_name=user_name & e.password=password

return,1
end

