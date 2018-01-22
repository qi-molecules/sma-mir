function dbi_sql_submit ,sql_statement,output_file=output_file, $
                      no_notify=no_notify,server=server, $
                      user_name=user_name,password=password
;
; Submits a single isql statement.
; result = server output as a string array
; keyword : output_file = file in which to put output
; keyword : no_notify = do not notify that output exists
; eg. : result=dbi_sql_submit(sql_statement,output_file='junk')
;
common global
fn = uti_filename(e.user_name,suffix='.sql');

if not keyword_set(server) then server=e.server
if not keyword_set(user_name) then user_name=e.user_name
if not keyword_set(password) then password=e.password

sybpath = getenv('SYBASE')
nl=n_elements(sql_statement)
lines=replicate('',nl+2) & lines(0:nl-1)=sql_statement & lines(nl)='go' & lines(nl+1)='quit'
if e.debug then begin
  for i=0,n_elements(lines)-1 do begin
    print,'> ',i,' ',lines[i]
  endfor
endif
if fil_write(e.dataset_dir + '/' + fn,lines) then begin
  cmd=replicate('',7)
  cmd(0)=sybpath + '/OCS-12_5/bin/isql'
  cmd(1)='-U'+user_name
  cmd(2)='-P'+password
  cmd(3)='-S'+server
  cmd(4)='-i' + e.dataset_dir+ '/' + fn
  cmd(5)='-w4096'
  if keyword_set(output_file) then begin
    cmd(6)='-o'+strtrim(output_file,2)
    if not keyword_set(no_notify) then print,'*** output in file '+output_file
  endif
cmds=''
for i=0,n_elements(cmd)-1 do cmds =cmds + ' '+cmd[i]
if e.debug then print,'command : xx',cmds,'xx'
spawn,cmds,result
if e.debug then print,result[0:min([20,n_elements(result)-1])]
if e.debug then print,e.dataset_dir + '/' + fn
result1=fil_remove(e.dataset_dir + '/' + fn)
endif
return,result
end
