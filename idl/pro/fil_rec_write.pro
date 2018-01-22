function fil_rec_write,baselines,file
;
; Writes an ascii file of record data.  Assumes that
; the dat_filter settings have been setup to only pass a single 
; sideband and receiver.
;
; INPUTS = >
;       baselines   array of baseline strings
;       file        string name of file
;
; result = -1 (error) , 1 (ok)
;
; eg. : result=fil_rec_write(['1-2','3-4'],'myfile')
;       To get all record data for baseline 1-2 and 3-4 and write a file
;       called myfile.
; eg. : result=fil_rec_write(c.blcd,'output')
;       To write out all record data for all baselines.
; 
;
openw,unit,file,/get_lun,error=err
if err ne 0 then begin
  print,!err_string
  return,-1
endif
;
; Print out baselines present header
;
printf,unit,baselines
;
; Start loop over baselines
;
FOR i = 0,n_elements(baselines)-1L DO BEGIN
; 
; build teststring to create list pointers of data for bsln i
; then build list pointers
;
   teststring = ' "' + baselines[i] + '" '
   myresult = dat_list(s_l,'"blcd" eq ' + teststring,/reset)
;
; Get all data for bsln i
; 
 dat_get_rows,cmp,amp,pha,x,wt,pt_first,pt_npts,'hours',0,/amp_pha,/list
;
; Make output array based on first pass parameters
;
IF i EQ 0  THEN BEGIN
   outputarray = make_array(n_elements(baselines)+1L,n_elements(x),/float)
END
;
; Start loop over number of elements of time stamp variable
;
FOR j = 0,long(n_elements(x))-1L DO BEGIN
;
; If this is the first pass, then define the first column to be the time.
;
 IF i EQ 0 then outputarray[0,j] = x[j]
; 
; Define rest of columns to be the values of the phases
;
outputarray[i+1,j] = pha[j]
;
; END loop over number of elements of time stamp variable
;
END
; 
; END loop over number of baselines
END
;
; Write out the output array in one fell swoop and clean up.
;
rep = strtrim(string(n_elements(baselines)),2)
frmt =' ( F7.4," ", ' + rep +'(F8.3," "))'
FOR i=0,n_elements(x)-1L DO BEGIN
printf,unit,outputarray[*,i],format=frmt
END
close,unit & free_lun,unit
return,1
end
