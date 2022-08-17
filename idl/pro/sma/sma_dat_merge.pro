pro sma_dat_merge, newformat=newformat, sametrack=sametrack
;yes
;=Task:SMA_DAT_MERGE --- To merge datasets (SMA wrapper)
;#Type: utility
;+Use:
;      One can use the command SMA_DAT_MERGE to merge datasets
;      in different epochs. To use the program, you have to
;      first separately MIR_SAVE dataset files, then use this
;      wrapper program SMA_DAT_MERGE by giving the dataset
;      filenames following the instructions at runtime.
;
;&history:
;------------------------------------------------------------------------
;      cykuo 18feb04 adapting the header
;------------------------------------------------------------------------
;     

common global
common data_set

if not keyword_set(newformat) then begin
   aa=''
   read,aa,prompt='Merging new format file? [default is yes]:  '
   if (aa eq 'NO' or aa eq 'no' or aa eq 'No' or aa eq 'N' or aa eq 'n') then newformat=0 else newformat=1
endif

print, ' Input dataset filenames for merging: '
i=0
newfilename='a'
WHILE(newfilename ne '') DO BEGIN

  read,newfilename,prompt='Enter file name or return for OK: '
  IF newfilename ne '' THEN BEGIN
; if the name was entered with single quotes, strip them off
    newfilename=strcompress(newfilename,/remove_all) 
    length = strlen(newfilename)
    IF (strmid(newfilename,0,1) eq "'") THEN  $
      newfilename = strmid(newfilename,1,length-1)
    IF (strmid(newfilename,length-2,1) eq "'") THEN  $
      newfilename = strmid(newfilename,0,length-2)

   count=0 & result=findfile(newfilename,count=count)
   IF count eq 0 THEN BEGIN
     print,'Could not find file: ',newfilename
     print,'Failed. No data merged'
     RETURN
   ENDIF

   i=i+1
   IF (i eq 1) THEN filenames=newfilename ELSE filenames=[filenames,newfilename]

  ENDIF
ENDWHILE

IF (i eq 1) THEN BEGIN
  print, 'Failed. Need no less than 2 files to merge'
  RETURN
ENDIF

savefilename=''
read,savefilename,prompt='Enter merge file name: '
IF savefilename eq '' THEN savefilename='mergedsaveset'
result=dat_merge_sma(filenames, savefilename, newformat=newformat, sametrack=sametrack)

print,'Merging done with dataset: ',savefilename
print,'Note that the scan numbers will not be contiguous.'
END


