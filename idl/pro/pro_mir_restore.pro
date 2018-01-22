pro pro_mir_restore,fn
;
; Restores the ms data structure and the rest if we are restoring a ds
;
; eg. pro_mir_restore,'merge'
;
common global
common data_set
common wlm

if e.debug then print,'the save file is ',e.idl_sav+'ms.save'
restore,e.idl_sav+'ms.save'

e.save_fn=fn
if(e.save_fn eq '') then return ;On a new dataset there is nothing to restore

if e.debug then	print,e.dataset_dir
;test=CALL_EXTERNAL('db.so', 'db_check_dataset_dir', e.dataset_dir)
;if(test EQ -1) then begin
;    print, "Unable to create dataset directory"
;    return
;endif
dbprocess=e.dbprocess
;Save the dataset file before continuing
count=0 & result=findfile(e.dataset_dir+'/dataset/'+e.save_fn,count=count)
if count ne 0 then restore,e.dataset_dir+'/dataset/'+e.save_fn
e.dbprocess=dbprocess
filename=e.dataset_dir+'/dataset/'+e.save_fn
print,'Restored data set ',filename
end
