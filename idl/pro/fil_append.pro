function fil_append ,input_file,output_file
;
; Appends input_file to end of output_file.
; If output_file does not exist, it is created and 
; the input file is deleted.
; result = -1 (error) , 1 (ok)
; eg. : result=fil_append('junk','junk1')
;
count=0 & result=findfile(output_file,count=count)
if count ne 0 then begin
  result=fil_remove('plot_temp')
;
; can't use /noshell because of redirection
;
  cmd='cat '+output_file+' '+input_file+' > plot_temp'
  spawn,cmd
  result=fil_remove(output_file)
  cmd=strarr(3) & cmd(0)='mv' & cmd(1)='plot_temp' & cmd(2)=output_file   
  spawn,cmd,/noshell
  result=fil_remove('plot_temp')
  result=fil_remove(input_file)
endif
if count eq 0 then begin
  cmd=strarr(4) & cmd(0)='mv' & cmd(1)='-f' & cmd(2)=input_file
  cmd(3)=output_file & spawn,cmd,/noshell  
  result=fil_remove(input_file)
endif
return,1
end
