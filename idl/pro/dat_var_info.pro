function dat_var_info ,var,s,stag,tag,tagno,pointer_str,fsl,label,format, $
                   justify,type,itype,description,format_long, $
                   max_length,get_icode=get_icode,condition,c_code, $
                   i_code,all_info=all_info

;
; Looks up info and integer code for variable in the data structures.
; For character code variables, it will also return the integer
; code (i_code) corresponding to the character code c_code
;
; parameters : var   -- variable name in mir structures
; keyword : all_info -- get formating info also
; keyword : get_icode -- get i_code corresponding to c_code
;         : condition -- condition to be satisfied by c_code  
;         : c_code -- value of var for which you want to lookup i_code
;         : fsl -- called from 'f'=filter, 's'=select or 'l'=list
; returns : s -- structure name in data_set
;         : stag -- full name (ie. structure.tag)
;         : tag -- variable name w/i structure
;         : tagno -- location of variable w/i structure
;         : pointer_str -- string w/ pointer to be used command constructs
;         : label -- heading label to be used for print
;         : format -- idl format string for printing
;         : itype -- idl integer code for the variable type
;         : description -- full description of variable
;         : format_long -- long format for complete significance
;         : max_length -- max string length of output
;         : i_code -- integer code correponding to c_code 
;                     (i_code=-1 if no matches)  
; result = -1 (error), 1(ok)
; eg. : var='int'
; eg. : result=dat_var_info(var,s,stag,tag,tagno,pointer_str,'f',label,format, $
; eg. :                  justify,type,itype,description,format_long, $
; eg. :                  max_length)
; to find the integer code corresponding to blcd = '1-2' :
; eg. : result=dat_var_info('blcd',s,stag,tag,tagno,pointer_str,'f',label,format, $
; eg. :                  justify,type,itype,description,format_long, $
; eg. :                  max_length,/get_icode,'eq','1-2',i_code)
; to find all blcd i_codes corresponding baselines w/ tel 1 
; eg. : result=dat_var_info('blcd',s,stag,tag,tagno,pointer_str,'f',label,format, $
; eg. :                  justify,type,itype,description,format_long, $
; eg. :                  max_length,/get_icode,'like','1',i_code)

common global
common data_set

;
; find which structure var is in (skip ca which repeats some var names)
;
j=where(ms.tag eq var and ms.s ne 'ca')
if max(j) eq -1 then begin
  print,'*** ',var,' not found !'
  return,-1
endif
s=ms.s(j)
stag=ms.stag(j)
tag=ms.tag(j)
tagno=ms.tagno(j)
;
; construct pointer string used in execute commands (see se_list.pro)
;
pointer_str=s+'(p'+strmid(s,0,1)+fsl+').'+tag
if s(0) eq 'c'  then pointer_str=c.icode_s(tagno)+ $
          '(p'+strmid(c.icode_s(tagno),0,1)+fsl+').'+c.icode_tag(tagno)
;
; lookup integer code for character variables
; (don't need to loop through multiple stag's since each
;  character variable appears only once.)
;
if n_elements(s) eq 1 then begin
  if s eq 'c' and keyword_set(get_icode) then begin
    case 1 of
      (condition ne 'like' and condition ne 'in'): begin
       cmd='i_code=where('+stag+' '+condition+" '"+c_code(0)+"')"
      end
      (condition eq 'like'): begin
        cmd="i_code=where(strpos("+stag+ $
           ",'"+c_code(0)+"',0) ne -1)"
      end
      (condition eq 'in'): begin
        cmd='i_code=where('
        for ij=0,n_elements(c_code)-1 do begin
          if ij gt 0 then cmd = cmd + ' or '
          t_value="'"+c_code(ij)+"'"
          cmd=cmd+stag+" eq "+t_value
        endfor
        cmd = cmd + ')'
      end
    else: begin
      print,condition,' not recognized by dat_var_info routine'
      return,-1
      end
    endcase
    if e.debug then print,cmd
    result=execute(cmd[0])
  endif
endif
;
; get output formating information
;
if keyword_set(all_info) then begin
  label=ms.label(j)
  format=ms.format(j)
  justify=ms.justify(j)
  type=ms.type(j)
  itype=ms.itype(j)
  description=ms.description(j)
  format_long=ms.format_long(j)
  max_length=ms.max_length(j)
endif

return,1
end
