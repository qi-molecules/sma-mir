function tra,local_server,remote_server
; 
;    Find ranges of rows in critical database tables
;    for transfer of data between two data servers
;    from remote_server to local_server
;
;    Note : this routine will only run for user rdx
;
;    parameters : local_server -- the server into which data is transferred
;                 remote_server -- the server from which data comes
;
; result=-1 (error), 1(ok)
;
; parameters : none
;
; e.g. : result=tra('SYBASE1','SYBASE')
; e.g. : result=tra('SYBASE2','SYBASE2')
;

common global
common data_set
e.debug=1
log_file='/logs/dbtra'
;
; fill in upd structure with rows from UPD on local server
;
sql_statement='select upd#,table_name,var_name,ser_name,min_allowed, ' + $
              'max_allowed from UPD'
result=dbi_sql_submit(sql_statement)
nrows=n_elements(result) - 4
if nrows le 0 then begin
  print,'read ',nrows, ' from UPD table, aborting data transfer !'
  return,-1
endif
;
; set up empty upd structure and then read UPD table into it
;
upd_temp={updid:lonarr(1),table_name:strarr(1),var:strarr(1), $
  ser_name:strarr(1),min_allowed:lonarr(1),max_allowed:lonarr(1), $
  low:lonarr(1),high:lonarr(1),rem_low:lonarr(1),rem_high:lonarr(1)}
upd = replicate(upd_temp,nrows)
updid=0L & table_name='' & var='' & ser_name='' & min_allowed=0L 
max_allowed=0L
for i = 0,nrows-1 do begin
  reads,result(2+i),updid,table_name,var,ser_name,min_allowed,max_allowed, $
        format='(i12,1x,a20,1x,a20,1x,a10,1x,i12,1x,i12)
  upd[i].updid=updid & upd[i].table_name=strcompress(table_name,/remove_all)
  upd[i].var=strcompress(var,/remove_all) 
  upd[i].ser_name=strcompress(ser_name,/remove_all) 
  upd[i].min_allowed=min_allowed & upd[i].max_allowed=max_allowed
endfor
j=where(upd.ser_name eq remote_server,nrows)
if nrows le 0 then return,1
upd=upd[j]
low=0L & high=0L
for i = 0,nrows-1 do begin
  if upd[i].ser_name ne e.server then begin 
      sql_statement='set nocount on ' + $
              ' declare @minv int,@maxv int ' + $
              ' select @minv = min(' + upd[i].var + ') ' + $
              ' from ' + upd[i].table_name + $
              ' select @maxv = max(' + upd[i].var + ') ' + $
              ' from ' + upd[i].table_name + $
      ' if @minv < ' + string(upd[i].min_allowed) + ' or @maxv > ' + $
      string(upd[i].max_allowed) + $
      ' or @minv > ' + string(upd[i].max_allowed) + ' or @maxv < ' + $
      string(upd[i].min_allowed) + $
      '  begin ' + $
      '   select @minv = min(' + upd[i].var + '),@maxv=max(' + $
      upd[i].var + ') ' + $
      '       from ' + upd[i].table_name + $ 
      '          where ' + upd[i].var + ' >= ' + string(upd[i].min_allowed) + $
      ' and ' + upd[i].var + ' <= ' + string(upd[i].max_allowed) + $
      '  end ' + $
      ' select isnull(@minv,-1),isnull(@maxv,-1) '
     
      result=dbi_sql_submit(sql_statement)
      reads,result[2],low,high
      upd[i].low=low & upd[i].high=high
      result=dbi_sql_submit(sql_statement,server=upd[i].ser_name)
      if strpos(result[0],'error') eq -1 then begin
        reads,result[2],low,high
        upd[i].rem_low=low & upd[i].rem_high=high
      endif else begin
        upd[i].rem_low=0 & upd[i].rem_high=0
      endelse
    endif
endfor
if e.debug then begin
  for i = 0,nrows-1 do begin
    print,upd[i].updid,upd[i].table_name,upd[i].var, $
       upd[i].ser_name,upd[i].min_allowed,upd[i].max_allowed, $
       upd[i].low,upd[i].high,upd[i].rem_low,upd[i].rem_high, $
       format='(i3,a12,a12,a10,6i12)
  endfor
endif
;
; write out data
;
maxrows=400000L

for i = 0,nrows-1 do begin
  if e.server ne upd[i].ser_name then begin
    if upd[i].rem_high gt upd[i].high then begin 
      act_max_rows=maxrows
      if upd[i].table_name eq 'mm..SCH' and act_max_rows gt 800 then $
        act_max_rows = 800
      sql_statement='use mmdb set nocount on ' + $
         ' if ((select count(*) from sysobjects ' + $ 
         ' where name = "temp_table" and uid=user_id()) > 0 ) ' + $
         ' begin drop table temp_table end '
      result=dbi_sql_submit(sql_statement,server=upd[i].ser_name)
;
; for tables which are primary on more than 1 server
; 
     j=where(upd.table_name eq upd[1].table_name,ser_count)
     if ser_count eq 1 then begin
      sql_statement='use mmdb declare @minv int,@maxv int ' + $
         ' set nocount on select @minv = min('+ upd[i].var + ') from ' + $
         upd[i].table_name + ' select @maxv = max('+ upd[i].var + ')' + $
         ' from ' + upd[i].table_name + ' set rowcount ' + $
         string(act_max_rows) + ' if (@minv < ' + $
         string(upd[i].min_allowed) + ' or @maxv > ' + $
         string(upd[i].max_allowed) + ' or @minv > ' + $
         string(upd[i].max_allowed) + ' or @maxv < ' + $
         string(upd[i].min_allowed) + ') begin select "error" end ' + $
         ' else begin select * into temp_table from ' + upd[i].table_name + $
         ' where '+ upd[i].var + ' > ' + string(upd[i].high) + $
         ' end set rowcount 0 checkpoint '
      endif else begin
;
; for tables which are primary on only one server
; 

      sql_statement=' use mmdb declare @minv int,@maxv int ' + $
         ' set nocount on select * into temp_table from ' + $
         upd[i].table_name + ' where '+ upd[i].var + ' > ' + $
         string(upd[i].high) + ' and   '+ upd[i].var + ' >= ' + $
         string(upd[i].min_allowed) + ' and '+ upd[i].var + ' <= ' + $
         string(upd[i].max_allowed) + $
         ' set rowcount 0 checkpoint  '
      endelse
      result=dbi_sql_submit(sql_statement,server=upd[i].ser_name)

      outname = 'in/' + upd[i].ser_name + '.'+ upd[i].table_name
      bcp_statement='bcp  temp_table out $RDXSCR/' + outname + $
        ' -n -T 900000 -Urdx -Prdxrdx -S ' + upd[i].ser_name + $
        ' ; chmod g+w $RDXSCR/' + outname 
     if e.debug then print,'bcp :',bcp_statement
     spawn,bcp_statement,res
     num_rows_out=0L
     reads,strmid(res[3],0,strpos(res(3),'rows',0)-1),num_rows_out
     if (strpos(res(3),'0 rows',0) eq 0) then begin
       print,'no data bcped out for table :',upd[i].table_name
       print,res
       return,0
     endif
     message='.....:output '+upd[i].table_name+' '+' ['+upd[i].var+  $
           ' > '+strcompress(string(upd[i].high),/remove_all)+ $  
           ' '+strcompress(string(num_rows_out),/remove_all)+' rows]'

     print,message
     spawn,'echo '+message+' >> $RDXDIR/'+log_file,res

    endif
  endif                 
endfor
return,0
end
