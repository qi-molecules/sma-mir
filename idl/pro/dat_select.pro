function dat_select ,s_s,selection,reset=reset,save=save,restore=restore, $
                   no_notify=no_notify
;
; Creates pointer arrays to select the data set with restrictions.
; pis,pbs,pss,pcs,prs are the restricted list pointer arrays. The 
; complete restricted data set be constructed with one call with
; a compound selection clause or by successive calls (once for 
; each selection restriction) w/o /reset. 
;
; Note : There is a parallel routine dat_filter.pro which produces the
;        pointer list to be used globally. The filter pointers should
;        be created first since the lists built here start from those
;        produced by dat_filter.
;
; parameters : s_s -- save structure for lists (must always include)
;              selection -- string with the variables, conditions and
;                           values to be selected out. The varaible 
;                           names and their values are translated into
;                           structure.variable names and string values
;                           which idl can process in a where clause.
;                           The basic rules are that any field to be 
;                           translated is enclosed in double quotes (")
;                           and for any varaible there must follow a 
;                           value field, except in the case of the
;                           'in' condition which can take multiple 
;                           values (which must be enclosed in square  
;                           brackets with each value in quotes).
; keywords : reset -- first, reset pointers to the filter values
;            save  -- save the current pointers before selection
;            restore -- restore saved pointers before selection
;            no_notify -- don't print the number of rows which pass
; 
; Conditions supported include : 'eq' , 'ne' , 'le' , 'ge' , 
;                           'like' as though wildcards on either side (see eg.)
;                           'in' used with string list of values (see eg.)
;                           'not' may be used for the opposite condition
;                           eg. ' not in ' or 'not like'
; Compound selection with joins using 'and', 'or' and '(' ')' are possible.
;            : reset -- 1 => initialize to look at all the filter data
; result = -1 (error), 0 (no rows) >0 (number of rows)
;
; eg.: result=dat_select(s_s,/reset) ; simply reset and pass all data 
; eg.: result=dat_select(s_s,'"band" eq "c1"',/reset) ; pick out just c1 data
; eg.: result=dat_select(s_s,'"band" like "c"',/reset) ; pick out all c1 & c2 data
; eg.: result=dat_select(s_s,'"blcd" like "1"',/reset) ; pick out all bsl w/ tel 1
; eg.: result=dat_select(s_s,'"band" in ["c1","c2"]',/reset) ; pick out c1 & c2
; compound example: to pick out bsl 1-2, 1-3, & 1-6 could use :
; eg.: result=dat_select(s_s,'"blcd" in ["1-2","1-3"] or "blcd" eq "1-6"',/reset) 
; eg.; result=dat_select(s_s,'"blcd" in ["1-2","1-3"]',/reset)
; eg.: result=dat_select(s_s,'"blcd" in ["1-2","1-3"] or "blcd" in ["4-5"]',/reset)
;
common global
common data_set

if keyword_set(reset) then begin
  pis=pif
  pbs=pbf
  pss=psf
  pcs=pcf
  prs=prf
  s_s={a:0} ; remove s_s save structure if not needed
endif
if keyword_set(save) then begin
  s_s={pis:pis,pbs:pbs,pss:pss,pcs:pcs,prs:prs}
endif
if keyword_set(restore) then begin
  if n_tags(s_s) le 1 then begin
    print,'no save structure or lists, can not restore !'
    return,-1
  endif
  pis=s_s.pis & pbs=s_s.pbs & pss=s_s.pss & pcs=s_s.pcs & prs=s_s.prs
endif
  nrows=n_elements(pss)
  if max(pss) eq -1 then nrows=0
  if n_params() le 1 then goto, update_list
;
; proceed through selection string replacing variables and values
; with forms recognized by idl
;
pos=0
leng=strlen(selection)
cmd_final='j=where('
while strpos(selection,'"',pos) ne -1 do begin
;
; positions here refer to character position of the '"'
;
  pos_var_beg=strpos(selection,'"',pos)
  pos_var_end=strpos(selection,'"',pos_var_beg+1)
  var=strtrim(strmid(selection,pos_var_beg+1, $
                     pos_var_end-pos_var_beg-1),2L)
  cmd_final=cmd_final+' '+strmid(selection,pos,pos_var_beg-pos)
  pos=pos_var_end+1
  pos_val_beg=strpos(selection,'"',pos_var_end+1)
  pos_val_end=strpos(selection,'"',pos_val_beg+1)
  if pos_val_beg eq -1 or pos_val_end eq -1 then begin
    print,'*** Unmatched quotes in list selection clause ! :'
    print,selection
    return,-1 
  endif
   value=strtrim(strmid(selection,pos_val_beg+1, $
                       pos_val_end-pos_val_beg-1),2L)
  if strpos(selection,'[',pos_var_end+1) ne -1 then pos_val_beg=min( $
    [strpos(selection,'"',pos_var_end+1),strpos(selection,'[',pos_var_end+1)])
  condition=strtrim(strmid(selection,pos_var_end+1, $
                           pos_val_beg-pos_var_end-1),2L)
  negate=0 & if strpos(condition,'not') ne -1 then negate=1
  nots=" " & nots_end=" "
  ors= ' or '
  if negate then begin
    condition=strtrim(strmid(condition,strpos(condition,'not')+3,20),2L)
    nots=' abs(254-(not(' & nots_end=')))'
    ors=' and '
  endif
  pos=pos_val_end+1
;
; for 'in' we need to get complete list of values (terminated by ']')
; each value is still enclosed in '"'
;
  if condition eq 'in' then begin
    pos_bra_end=strpos(selection,']',pos)
    if pos_bra_end eq -1 then begin
      print,'"in" condition requires matched "[]"'
      return,-1 
    endif
      while pos_val_beg lt pos_bra_end do begin
        pos_val_beg=strpos(selection,'"',pos_val_end+1)
        if pos_val_beg ne -1 and pos_val_beg lt pos_bra_end then begin
          pos_val_end=strpos(selection,'"',pos_val_beg+1)
          value=[value,strtrim(strmid(selection,pos_val_beg+1, $
                             pos_val_end-pos_val_beg-1),2L)]
        endif else  pos_val_beg = pos_bra_end + 1
      endwhile    
    pos=pos_bra_end+1
  endif
;
; find which structure var is in
;
if not dat_var_info(var,s,stag,tag,tagno,pointer_str,'s',label,format, $
            justify,type,itype,description,format_long, $
            max_length,/get_icode,condition,value,i_value) then return,-1
nvar=n_elements(stag)
i=0
;
; since there may be multiple structures with the same var name we have 
; to do a loop over all possible variable matchs.
;
while i le (nvar-1) do begin
  cmd=''
;
; if condition is 'like' or 'in', the var must be a character code
;
  if (condition eq 'like' or condition eq 'in') and s(i) ne 'c'  then begin
    print,'error in list : "',condition,'" works only with string variables !'
    return,-1
  endif
;
; if a character code, must use int code equiv, and insert it into the search
;
  if s(i) eq 'c'  then begin
    value=strtrim(string(i_value),2L)
  endif
;
; assemble final command string
;
  nv=n_elements(value)
  case 1 of
    (condition ne 'like' and condition ne 'in' and condition ne 'ne' and nv eq 1): begin
       cmd=nots+pointer_str(i)+' '+condition+' '+string(value(0))+nots_end
    end
    (condition eq 'in' or condition eq 'like' or condition eq 'ne' or nv gt 1): begin
     if s(i) ne 'c' then begin
      cmd = cmd + '('
      for ij=0,n_elements(value)-1 do begin
        if ij gt 0 then cmd = cmd + ' and '
        cmd=cmd+pointer_str(i)+" "+condition+" "+"'"+value(ij)+"'"+" "
      endfor
      cmd = cmd + ')'  
     endif else begin
      cmd = cmd + '('
      for ij=0,n_elements(value)-1 do begin
        if ij gt 0 then cmd = cmd + ors
        cmd=cmd+nots+pointer_str(i)+" eq "+"'"+value(ij)+"'"+nots_end
      endfor
      cmd = cmd + ')'
     endelse
    end
  else: begin
    print,condition,' not recognized by dat_select routine'
    return,-1
    end
  endcase
if i gt 0 then cmd_final=cmd_final + ' and '
cmd_final=cmd_final+cmd
    i=i+1
en:
endwhile
endwhile
  cmd_final = cmd_final +strtrim(strmid(selection,pos,leng-pos+1),2L)+' )'
  if e.debug then print,cmd_final
  result=execute(cmd_final)
  nrows=n_elements(j)

  if max(j) ne -1 then begin
    if not keyword_set(no_notify) then print,nrows,' passed in list'
    pis=pis(j)
    pbs=pbs(j)
    pss=pss(j)
    pcs=pcs(j)
    prs=prs(j)
  endif else begin
    nrows=0
    if (not keyword_set(no_notify)) or e.debug then begin
      print,'0 rows passed'
      print, '******************************************************'
      print, '******************************************************'
      print, '**** DANGER ! DANGER ! DANGER ! DANGER ! DANGER ! ****'
      print, '**** NO ROWS SELECTED ...                         ****'
      print, '****                 MEANS ALL ROWS PASSED !!!    ****'
      print, '**** DANGER ! DANGER ! DANGER ! DANGER ! DANGER ! ****'
      print, 'This is dat_select'
      print, 'selection=',selection
      print, '******************************************************'
      print, '******************************************************'
    endif
  endelse


if e.prog_help then begin
 print,' '
 print,'  list pointer arrays :'   
 print,'    pis, pbs, pss, pcs, and prs should now be used in place of '$
      ,'    pif, pbf, psf, pcf, and prf.'$
      ,'    pcs is pointer array from sp rows to the first data pt in ch'$
      ,'    prs is pointer array from sp rows to the first record of header'  $
      ,'    ... data in re'
endif
update_list:
  result=dat_list(s_l,/reset)
return,nrows
end
