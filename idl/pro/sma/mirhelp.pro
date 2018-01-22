pro mirhelp,str_par,type=type, keyword=keyword
;yes
;+Use:
;     MIRHELP gets the on-line help for the tasks and keywords
;     in MIR/IDL. 
;
;     There are several types of MIR/IDL tasks:
;      1. calib : calibration tasks.
;      2. utility   : utility tasks.
;      3. conversion: those tasks to convert the data type.
;      4. plotting: plotting tasks.
;      5. i/o   : input and output task
;     To show all tasks beloning to the 'calib' catagory, for example, use
;     IDL>mirhelp, type='calib'
;
;     To get help for a certain task, e.g. 'GAIN_CAL', use
;     IDL>mirhelp,'gain_cal'. 
;     If the task name is imcomplete, for example, you only type 'gai', 
;     then all task names containing 'gai' will be printed out.
;    
;     IF you type >mirhelp, keyword='x_var', then the explanations
;     of the keyword x_var will be printed out with the names of the
;     tasks which contain it.
;
;
;&history:
;--------------------------------------------------------------------
;       cykuo 19dec03 firt version
;---------------------------------------------------------------------

common global

if n_params() eq 0 then begin
   if not keyword_set(type) and not keyword_set(keyword) then begin
     mirhelp,'mirhelp'
     return
   endif
endif

if n_params() ne 0 then begin

   file=str_par
   pro_list=findfile(e.idl_pro+'sma/'+'*'+strlowcase(file)+'*.pro')
   list=pro_list
   uti_stri_replace,list,'.pro',''
   uti_stri_replace,list,e.idl_pro+'sma/',''
 
   for i=0,n_elements(pro_list)-1 do begin

     openr,unit,pro_list(i),/get_lun,error=err
     if (err eq 0) then begin
        ; file can be successfully opened; close and re-open for reading
        close,unit & free_lun,unit
        result=fil_read(pro_list(i),lines,nlines)

        istart=-1
        istop=-1
        lcount = 0
        tostop = 0

        while (tostop ne 1 and lcount lt nlines) do begin

           if (istart eq -1 and strpos(lines(lcount),';',0) eq 0) then istart=lcount 
           if (istart ne -1 and strpos(lines(lcount),';',0) eq 0) then istop=lcount 
           if (istart ne -1 and istop ne -1 and $
               strpos(lines(lcount),';',0) ne 0) then tostop = 1
           lcount = lcount + 1

        endwhile
        line=''  
        cmdlist=['=','+','#','&','@']

        if (istart ne -1) then begin
          if (strmid(lines(istart),1,3) eq 'yes') then begin
            for l=istart+1,istop do begin
              reads,lines(l),line,format='(1x,a)'
              index = where( strmid(line,0,1) eq cmdlist)

              if (index[0] eq 3) then break
              case index[0] of
                0 : print,format='(/,a,T10, a )', 'task:',strupcase(strtrim(list[i],2L))
                1 : print,format='(/,a)','use:'
                2 : print,format='(/,a,T10,a)','type: ', strmid(line,7,10) 
                4 : begin 
                     print,''
                     print,'keyword: ', strmid(line,1,100)
                   end
                else: print,format='(T10,a)', strtrim(line,2)
              endcase
            endfor
          endif
        endif
     endif
   endfor
endif

if keyword_set(type) then begin

  pro_list=findfile(e.idl_pro+'sma/*.pro')

  for i=0,n_elements(pro_list)-1 do begin

    openr,unit,pro_list(i),/get_lun,error=err

    if (err eq 0) then begin
       ; file can be successfully opened; close and re-open for reading
       close,unit & free_lun,unit
       result=fil_read(pro_list(i),lines,nlines)

       istart=-1
       istop=-1
       lcount = 0
       tostop = 0

       while (tostop ne 1 and lcount lt nlines) do begin

         if (istart eq -1 and strpos(lines(lcount),';',0) eq 0) then istart=lcount 
         if (istart ne -1 and strpos(lines(lcount),';',0) eq 0) then istop=lcount 
         if (istart ne -1 and istop ne -1 and $
               strpos(lines(lcount),';',0) ne 0) then tostop = 1
         lcount = lcount + 1

       endwhile

       if (istart ne -1) then begin
         line=''
         for l=istart,istop do begin
           reads, lines(l), line, format='(1x,a)'
           if strmid(line,0,1) eq '=' then begin
               task=strmid(line,1,100)
               x=strpos(task,' ')
               y=strmid(task,5,x-4)
               z=strmid(task,x,100) 
           endif
           if strmid(line,0,1) eq '#' then begin
              if strtrim(strmid(line,7,100),2) eq type then begin
                print,format='(a,T18,a)', y, z  
              endif
           endif
         endfor
       endif
    endif 
  endfor
endif    
 

if keyword_set(keyword) then begin

  pro_list=findfile(e.idl_pro+'sma/*.pro')

  for i=0,n_elements(pro_list)-1 do begin

    openr,unit,pro_list(i),/get_lun,error=err

    if (err eq 0) then begin
       ; file can be successfully opened; close and re-open for reading
       close,unit & free_lun,unit
       result=fil_read(pro_list(i),lines,nlines)

       istart=-1
       istop=-1
       lcount = 0
       tostop = 0

       while (tostop ne 1 and lcount lt nlines) do begin

         if (istart eq -1 and strpos(lines(lcount),';',0) eq 0) then istart=lcount
         if (istart ne -1 and strpos(lines(lcount),';',0) eq 0) then istop=lcount
         if (istart ne -1 and istop ne -1 and $
               strpos(lines(lcount),';',0) ne 0) then tostop = 1
         lcount = lcount + 1

       endwhile

       if (istart ne -1) then begin
         line=''
         key = 0
         for l=istart,istop do begin
           reads, lines(l), line, format='(1x,a)'
           a=strpos(line,':')
           b=strmid(line,1,a-1)
           if ((strmid(line,0,1) eq '@') and (b ne keyword)) then key=0
           if key eq 1 then print, format='(T14,a)',strmid(line,2,100) 
           if strmid(line,0,1) eq '=' then begin
               task=strmid(line,1,100)
               x=strpos(task,' ')
               y=strmid(task,5,x-4)
               z=strmid(task,x,100)
           endif     
           if strmid(line,0,1) eq '@' then begin
              if b eq keyword then begin
                key = 1
                print,format='(/,a,a,T18,a)','Task:',y,z
                print,'keyword: ',b,':'
              endif              
           endif         
         endfor
       endif
    endif
  endfor
endif
       
                                                                           


end
