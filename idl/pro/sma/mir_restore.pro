pro mir_restore,fn,filename=filename, nowait=nowait, newformat=newformat
;yes
;=Task:MIR_RESTORE --- To restore the "mir_saved" data structure 
;#Type: i/o
;+Use:
;      MIR_RESTORE restore the data which was previously saved
;      by the MIR_SAVE command. 
;@filename:
;      The filename of the dataset to be restored. For example,
;      if you want to restore the file 'mysaveset', you can type
;      >mir_restore,'mysaveset'   or
;      >mir_save,filename='mysaveset'
;
;
;&history:
;--------------------------------------------------------------------
;      cykuo 25dec03 adapting the header
;--------------------------------------------------------------------      

common global
common data_set
common wlm


;restore,e.idl_sav+'ms.save'

if n_params() ne 0 then e.save_fn=fn
if (keyword_set(filename) ne 0) then e.save_fn = filename 

;On a new dataset there is nothing to restore
if(e.save_fn eq '') then return

if (e.campuslogin ne 'sma' and e.campuslogin ne 'cfa') then begin

  if e.debug then	print,e.dataset_dir
  test=CALL_EXTERNAL('db.so', 'db_check_dataset_dir', e.dataset_dir)
  if(test EQ -1) then begin
      print, "Unable to create dataset directory"
      return
  endif
  dbprocess=e.dbprocess
;Save the dataset file before continuing
  count=0 & result=findfile(e.dataset_dir+'/dataset/'+e.save_fn,count=count)
  if count ne 0 then restore,e.dataset_dir+'/dataset/'+e.save_fn
  e.dbprocess=dbprocess
  filename=e.dataset_dir+'/dataset/'+e.save_fn

endif else begin

if (strmid(e.save_fn,0,1) eq '/') then begin
  filename=e.save_fn
endif else begin
  filename=e.dataset_dir+e.save_fn
endelse


  newfilename='a'
  print,'Will restore data and environment from file: ',filename
  if keyword_set(nowait) then newfilename='' else read,newfilename,prompt='Return for OK, or enter another file name: '


  if newfilename ne '' then begin
; if the name was entered with single quotes, strip them off
    length = strlen(newfilename)
    if (strmid(newfilename,0,1) eq "'") then  $
      newfilename = strmid(newfilename,1,length-1)
    if (strmid(newfilename,length-2,1) eq "'") then  $
      newfilename = strmid(newfilename,0,length-2)

    filename = newfilename
    e.save_fn = filename
  endif

  count=0 & result=findfile(filename,count=count)
  if count ne 0 then begin
    restore,filename
;    print,'Restored data set ',filename
;    return
  endif else begin
    print,'Could not find file: ',filename
    print,'Failed. No data restored'
    return
  endelse
;  e.dbprocess=dbprocess

endelse

if tag_exist(in,'conid') eq 0 then begin
   msfile=e.idl_sav+'ms_newformat.save'
   print,'Restored new data format data set ',filename
endif else begin
   msfile=e.idl_sav+'ms.save'
   print,'Restored old data format data set ',filename
endelse

restore,msfile

if e.debug then print,'the save file is ',msfile

end
