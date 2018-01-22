pro pro_mir_execute_dbrecord
;
; executes a db_record dump of database info
; to record sizes and configs
;

common global

;
; first dump sql output to old dbrecord
;

cmd='isql  -Usa -Psasasa < '+e.idl_sql+'dbrecord >> '+e.rdx_dir+'/logs/dbrecord
;spawn,'source '+e.rdx_dir+'/.cshrc; '+cmd,result
spawn,cmd,result
;
; then cut the file so it doesn't get too big
;
cmd='head -400 '+e.rdx_dir+'/logs/dbrecord > '+e.rdx_dir+'/dbtemp; ' + $
    'echo "******** deleted lines **********" >> '+e.rdx_dir+'/dbtemp; ' + $
    'tail -4000 '+e.rdx_dir+'/logs/dbrecord >> '+e.rdx_dir+'/dbtemp; ' + $
    'mv '+e.rdx_dir+'/dbtemp '+e.rdx_dir+'/logs/dbrecord '

;spawn,'source '+e.rdx_dir+'/.cshrc; '+cmd,result
spawn,cmd,result

end
