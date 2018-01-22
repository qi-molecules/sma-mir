function dbi_dat_tra,local_server,remote_server,input=input
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
; e.g. : result=dbi_dat_tra('SYBASE1','SYBASE')
; e.g. : result=dbi_dat_tra('SYBASE1','SYBASE',/input)
; e.g. : result=dbi_dat_tra('SYBASE','SYBASE1')
; e.g. : result=dbi_dat_tra('SYBASE','SYBASE1',/input)
;

common global
common data_set
log_file='/logs/dbtrans'
e.debug=0
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

message = '**********:database transfer: ' + systime(0)
print,message
spawn,'echo "'+message+'" >> $RDXDIR/'+log_file,res
message=strcompress('.....:evaluating ranges of data on ' +local_server+' and '+remote_server)
print,message
spawn,'echo '+message+' >> $RDXDIR/'+log_file,res

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
low=0L & high=0L
for i = 0,n_elements(upd)-1 do begin
  if upd[i].ser_name ne local_server then begin 
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
     
      result=dbi_sql_submit(sql_statement,server=local_server)
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
  for i = 0,n_elements(upd)-1 do begin
    print,upd[i].updid,upd[i].table_name,upd[i].var, $
       upd[i].ser_name,upd[i].min_allowed,upd[i].max_allowed, $
       upd[i].low,upd[i].high,upd[i].rem_low,upd[i].rem_high, $
       format='(i3,a12,a12,a10,6i12)
  endfor
endif
;
; write out data
;
maxrows=200000L

for i = 0,n_elements(upd)-1 do begin
  if local_server ne upd[i].ser_name then begin
    if upd[i].rem_high gt upd[i].high then begin 
      act_max_rows=maxrows
      if upd[i].table_name eq 'mm..SCH' and act_max_rows gt 800 then $
        act_max_rows = 800
      sql_statement='use mmdb set nocount on ' + $
         ' if ((select count(*) from sysobjects ' + $ 
         ' where name = "temp_table" and uid=user_id()) > 0 ) ' + $
         ' begin drop table temp_table end '
      result=dbi_sql_submit(sql_statement,server=upd[i].ser_name)
     j=where(upd.table_name eq upd[i].table_name,ser_count)
;
; for tables which are primary on only one server
; 
if e.debug then print,'server count :',ser_count
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
; for tables which are primary on more than 1 server
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
     for ir=0,n_elements(res)-1 do begin
       if strpos(res(ir),'rows copied') ne -1 then il=ir
     endfor
     reads,strmid(res[il],0,strpos(res(il),'rows',0)-1),num_rows_out
     if (strpos(res(il),'0 rows',0) eq 0) then begin
       message=strcompress('no data bcped out for table :'+upd[i].table_name)
       print,message
       spawn,'echo '+message+' >> $RDXDIR/'+log_file,res
       print,res
       return,0
     endif
     message=strcompress('.....:output '+upd[i].table_name+' '+' ['+upd[i].var+  $
           ' > '+strcompress(string(upd[i].high),/remove_all)+ $  
           ' '+strcompress(string(num_rows_out),/remove_all)+' rows]')

     print,message
     spawn,'echo "'+message+'" >> $RDXDIR/'+log_file,res

    endif
  endif                 
endfor

if keyword_set(input) then begin
for i = 0,n_elements(upd)-1 do begin
  if local_server ne upd[i].ser_name then begin
    if upd[i].rem_high gt upd[i].high then begin 
;
; now input table to local dataserver
;
; 
; first drop temp_table, then create a new temp_table with same
; columns as one we will insert into
;
      sql_statement='use mmdb set nocount on ' + $
         ' if ((select count(*) from sysobjects ' + $ 
         ' where name = "temp_table" and uid=user_id()) > 0 ) ' + $
         ' begin drop table temp_table end '
      result=dbi_sql_submit(sql_statement,server=local_server)
      sql_statement='use mmdb set nocount on ' + $
         ' select * into temp_table from '+upd[i].table_name + $ 
         ' where 1=2 '
      result=dbi_sql_submit(sql_statement,server=local_server)
;
; now bcp in to temp_table and rm host file
;
      inname = 'in/' + upd[i].ser_name + '.'+ upd[i].table_name
      bcp_statement='bcp  temp_table in $RDXSCR/' + inname + $
        ' -n -T 900000 -Urdx -Prdxrdx -S ' + local_server + $
        ' ; rm $RDXSCR/'+ inname
      if e.debug then print,'bcp :',bcp_statement
      spawn,bcp_statement,res
      num_rows_out=0L
      for ir=0,n_elements(res)-1 do begin
       if strpos(res(ir),'rows copied') ne -1 then il=ir
      endfor

      reads,strmid(res[il],0,strpos(res(il),'rows',0)-1),num_rows_in
      if (strpos(res(il),'0 rows',0) eq 0) then begin
        print,'no data bcped in for table :',upd[i].table_name
        print,res
        return,0
      endif
      sql_statement='use mmdb set nocount on ' + $
         ' select row_count=count(*) from temp_table '
      result=dbi_sql_submit(sql_statement,server=local_server)
      reads,result[2],row_count
      if row_count gt 0 then begin
        sql_statement='use mmdb set nocount on ' + $
          ' create index tind on temp_table('+upd[i].var+')'
        result=dbi_sql_submit(sql_statement,server=local_server)
        loc_high=0L 
        j=where(upd.table_name eq upd[i].table_name,ser_count)
;
; for tables which are primary on only one server
; 
        if ser_count eq 1 then begin
          sql_statement='use mmdb set nocount on ' + $
          ' select loc_high=max('+ upd[i].var +') from ' + upd[i].table_name
          result=dbi_sql_submit(sql_statement,server=local_server)
          reads,result[2],loc_high
        endif else begin
;
; for tables which are primary on more than 1 server
; 
;
          sql_statement='use mmdb set nocount on ' + $
           ' select loc_high=max('+ upd[i].var +') from '+ upd[i].table_name + $
           ' where '+ upd[i].var +' >= '+ string(upd[i].min_allowed) + $
           ' and '+ upd[i].var +' <= '+ string(upd[i].max_allowed)
           result=dbi_sql_submit(sql_statement,server=local_server)
           reads,result[2],loc_high
         endelse
         if (loc_high lt upd[i].min_allowed or loc_high gt upd[i].max_allowed) $
         then begin
           message=strcompress('error in data ranges on read for '+  $
              upd[i].table_name + $   
             ' local high ='+string(loc_high)+' min allowed='+ $
             string(upd[i].min_allowed) +', max allowed='+ $
             string(upd[i].max_allowed))
           print,message
           spawn,'echo '+message+' >> $RDXDIR/'+log_file,res
           return,-1
         endif

         if (upd[i].table_name eq 'INV' or upd[i].table_name eq 'USERS') $
         then begin
            sql_statement='delete '+ upd[i].table_name + $
              ' where init in (select init from temp_table) '
            result=dbi_sql_submit(sql_statement,server=local_server)
         endif
;
; now read rows into the archive table
;
         maxrow=5000
         if upd[i].table_name eq 'mm..SCH' then maxrow=50
         sql_statement='use mmdb set nocount on ' + $
            ' select ins_count=count(*),ins_low=min('+ upd[i].var +')'+ $
            ' ,ins_high=max('+ upd[i].var +') from temp_table ' + $
            ' where '+ upd[i].var +' > '+ string(loc_high) + $
            ' and '+ upd[i].var +' >= '+ string(upd[i].min_allowed) + $
            ' and '+ upd[i].var +' <= '+ string(upd[i].max_allowed)
         result=dbi_sql_submit(sql_statement,server=local_server)
         ins_count=0L & ins_low=0L & ins_high=0L
         reads,result[2],ins_count,ins_low,ins_high
         startrow = ins_low
         skip = 'no'
         while startrow le ins_high and skip eq 'no' do begin
           endrow = ((startrow + maxrow -1) > ins_high)
           nrow = endrow - startrow +1           
           sql_statement='declare @nr int,@strow int ' + $
             ' insert '+ upd[i].table_name + $
             ' select * from temp_table where '+ upd[i].var + $
             ' >= '+ string(startrow) +' and '+ upd[i].var + $
             ' >= '+ string(upd[i].min_allowed) +' and '+ upd[i].var + $
             ' <= '+ string(upd[i].max_allowed) +' and '+ upd[i].var + $
             ' <= '+ string(endrow) +' select @nr=@@rowcount ' + $
             ' select @strow=min('+ upd[i].var +') from temp_table ' + $
             ' where '+ upd[i].var +' >= '+ string(startrow) + $
             ' and '+ upd[i].var +' >= '+ string(upd[i].min_allowed) + $
             ' and '+ upd[i].var +' <= '+ string(upd[i].max_allowed) + $
             ' select "insertresults ",@nr,@strow '
           result=dbi_sql_submit(sql_statement,server=local_server)
           nr=0L & strow=0L
           for ir=0,n_elements(result)-1 do begin
             if strpos(result(ir),'insertresults') ne -1 then il=ir
           endfor
           result[il]=strmid(result[il],strpos(result[il],'insertresults')+13)
           reads,result[il],nr,strow
           if nr le 0 then begin
             skip = 'yes'
             message=strcompress('..........:error  ' + upd[i].table_name + $
                     '['+upd[i].var+'='+string(nr) + $
                          ' rows starting at '+string(startrow)+']')
             print,message
             spawn,'echo '+message+' >> $RDXDIR/'+log_file,res
             message=strcompress('..........:error  -- skipping the '+ $
                     'rest of ' + upd[i].table_name)
             print,message
             spawn,'echo '+message+' >> $RDXDIR/'+log_file,res
           endif else begin
             message=strcompress('.....:input  ' + upd[i].table_name + $
                          '['+upd[i].var+'='+string(nr) + $
                          ' rows starting at '+string(startrow)+']')
             print,message
             spawn,'echo '+message+' >> $RDXDIR/'+log_file,res
             startrow=strow
           endelse
           cmd='isql -U rdx -P rdxrdx < $RDXDIR/dbms/scripts/' + $
              'dump_transaction  '
           sp_str=''
           if e.java then sp_str='source '+e.rdx_dir+'/.cshrc; '
;           spawn,sp_str+cmd,result
           spawn,cmd,result
           startrow = endrow +1
         endwhile
         if skip eq 'no' then begin
           message=strcompress('.....:input  ' + upd[i].table_name + $
                   '['+upd[i].var+'='+string(ins_low) + $
                   '-'+string(ins_high)+' :'+string(ins_count)+' rows]')
           print,message
           spawn,'echo '+message+' >> $RDXDIR/'+log_file,res
         endif else begin
           message=strcompress('no data bcped in for table :'+upd[i].table_name)
           print,message
           spawn,'echo '+message+' >> $RDXDIR/'+log_file,res
         endelse
      endif
  endif
  endif                 
endfor

endif


if (keyword_set(input) and local_server ne 'SYBASE') then begin
;
;  recreate v_last,v_present,v_previous and TRACKS summary table
;
  message=strcompress('..........:recreating v_last,v_present,v_previous')
  print,message
  spawn,'echo '+message+' >> $RDXDIR/'+log_file,res
  cmd='isql -U rdx -P rdxrdx < $RDXDIR/dbms/proc/c_views_1.proc '
  if e.java then sp_str='source '+e.rdx_dir+'/.cshrc; '
;  spawn,sp_str+cmd,result
  spawn,cmd,result
  sql_statement='declare @tr int,@tr1 int,@tr2 int,@incr int ' + $
         ' select @tr1=max(tra#) from TRACKS ' + $
         ' select @tr2=max(tra#) from TRA ' + $
         ' delete TRACKS where tra# = @tr1 ' + $
         ' select @incr=1 set nocount on select @tr=@tr1 ' + $
         ' while (@tr <= @tr2) begin exec trackmm @tr ' + $
         ' select @tr=@tr+@incr end '
  result=dbi_sql_submit(sql_statement,server=local_server)
endif

return,0
end

