pro readdata_long,directory=directory, full=full, _extra=extra_keywords

common global
common data_set
common wlm

;print,directory
;print,keyword_set(directory) 

if keyword_set(directory) then begin
   nchar = strlen(directory)
   checkslash = strmid(directory,nchar-1,1)
   if ( strmatch(checkslash,'/') le 0 ) then directory = directory + '/'
   e.idl_bcp=directory
   
   count = 0
   result = findfile(e.idl_bcp+'in_read',count=count)
   if (count eq 0) then begin
      print,'could not find file ',e.idl_bcp+'in_read'
      return
   endif
   count = 0
   result = findfile(e.idl_bcp+'bl_read',count=count)
   if (count eq 0) then begin
      print,'could not find file ',e.idl_bcp+'bl_read'
      return
   endif
   count = 0
   result = findfile(e.idl_bcp+'sp_read',count=count)
   if (count eq 0) then begin
      print,'could not find file ',e.idl_bcp+'sp_read'
      return
   endif
   count = 0
   result = findfile(e.idl_bcp+'codes_read',count=count)
   if (count eq 0) then begin
      print,'could not find file ',e.idl_bcp+'codes_read'
      return
   endif
   count = 0
   result = findfile(e.idl_bcp+'sch_read',count=count)
   if (count eq 0) then begin
      print,'could not find file ',e.idl_bcp+'sch_read'
      return
   endif
endif
result=dbi_head_read_long(_extra=extra_keywords)
if result lt 0 then return
if keyword_set(full) then begin
   result=dbi_chan_read_long(_extra=extra_keywords)
endif else begin
   result=dbi_chan_read_short(_extra=extra_keywords)
   if result lt 0 then result=dbi_chan_read_long(_extra=extra_keywords)
endelse
res=dat_list(s_l,'"band" eq "c1"',/reset,/no_notify)
cont_bw=(n_elements(c.band)-1.)*82.
if sp[psl[0]].fres ne cont_bw then sp[psl].fres = (sp[psl].fres/abs(sp[psl].fres)) * cont_bw
res=dat_list(s_l,/reset)

end
