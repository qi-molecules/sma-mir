; Produces a track summary in terms of sources, track length, etc...

function reduce_summary,project=project,title=title,obsdate=obsdate,$
           wvp=wvp,config=config,piproject=piproject,$
           unit=unit,defaults=defaults
   ; Common blocks
     common global
     common data_set

   ; Reset filter
     if dat_select(s_s,/reset) eq 0 then return,-1

   ; Get project number
     project = uti_distinct(in.proid,nproj)
     if (nproj gt 1) then begin
        j = where(project ne 100,nproj)
        p = project[j]
        nmax = 0
        project = (nproj eq 1) ? project(j[0]) : 0
        if nproj gt 1 then begin
           for i=0, nproj-1 do begin
              n = where(in.proid eq p[i],np)
              project = (np gt nmax ? p[i] : project)
              nmax = max([nmax,np])
           endfor
        endif
     endif

   ; Get project title
     command =  'select title from PRO where PRO.pro#=' + string(project)
     result = dbi_sql_submit(command)
     k = where (strmatch(result,'*-----------*') eq 1)
     title = strtrim(result[k+1],2)

   ; Prompt user for project PI
     com = 'select fname,lname from INV,PR_IN where PR_IN.inv#=INV.inv# and PR_IN.pro#=' + string(project)
     dbi_sql_tab,com,ct,pi_list
     nlist = (ct gt 0) ? n_elements(pi_list[0,*]) : 0

   ; Print possible email address
     if (nlist eq 0) then begin
        piproject = ' '
     endif else begin
        ; Print table of investigators and let the user pick one
          irepeat = 1
          max_name  = max(strlen(pi_list[0,*])) + max(strlen(pi_list[1,*])) + 1
          while (irepeat) do begin
             ; Print table
               print,""
               print,format='("Project number :",I0)',project
               print,format='("Project title  : ",A)',title
               print,"Enter the ID number of the project PI"
               print,"ID  Name"
               s1 = '' & for i=1,max_name  do s1 = s1 + '-'
               print,"--  ",s1
               for i=0,nlist-1 do print,format='(%"%2d  %s %s")',$
                    i+1,pi_list[0,i],pi_list[1,i]

             ; Let user select IDS
               print,format='("Choice ? ",$)'
               input = ' '
               read,input
               x = 0.0
               if not valid_num(input,x) or 1.0*fix(x) ne x or $
                  x lt 1 or x gt nlist then begin
                  print,""
                  print,"*** Error entering project PI ***" 
                  print,""
               endif else begin
                  irepeat = 0
                  result = strsplit(pi_list[1,fix(x)-1],' ',/extract)
                  piproject = result[0]
               endelse
           endwhile
     endelse

   ; Obs date
     obsdate = ' '
     reads,c.ut[0],obsdate,format='(A11)'

   ; Get observer
     observer = e.user_name
;    if not keyword_set (defaults) then begin
;       s = 'Enter observers (default: ' + e.user_name + ') '
;       input = ' '
;       print,format='(%"%s",$)',s
;       read,input
;       input = strtrim(input)
;       if (input ne "") then observer = input
;    endif

   ; Get configuration
     config = strupcase(c.cocd)

   ; Soid
     soid = uti_distinct(bl.soid,nsoid)

   ; Antenna offline
     tel_online = uti_distinct([c.tel1,c.tel2])
     ant_offline = ""
     noff = 0
     for i = 1, !NTEL do begin
        j = where(i eq tel_online,nj)
        if (nj eq 0) then begin
           if (noff ne 0) then ant_offline = ant_offline + ","
           ant_offline = ant_offline + string(i)
           noff = noff + 1
        endif
     endfor
     ant_offline = strcompress(ant_offline,/remove)
     if ant_offline eq "" then ant_offline = "None"

   ; Number of integrations
     nint = max(in.int)

   ; Track length
     ut_start = c.ut[in(0).iut]
     ut_stop  = c.ut[in(n_elements(in)-1).iut]
     command = 'select datediff(ss,"' + ut_start + '","' + ut_stop + '")'
     result = dbi_sql_submit(command)
     k = where (result eq ' ----------- ')
     s = result[k+1]
     if valid_num(s[0]) then begin
       tracklength = 0.0
       reads,s[0],tracklength
       tracklength = strtrim(string(format='(%"%8.1f h")',tracklength/3600.0),2)
     endif else $
       tracklength = "?"

   ; Weather
     tamb = "?"
     h2o  = "?"
     wvp  = "null"
     wind = "?"
     command = 'select avg(tamb),avg(h2o),avg(wind) from WEA ' + $
               'where utstart >= "' + ut_start + $
               '" and utstop <= "' + ut_stop + '"'
     result = dbi_sql_submit(command)
     k = where (strmatch(result,'*-----------*') eq 1,nk)
     if nk eq 1 and k[0] ne -1 and k[0] lt (n_elements(result)-1) then begin
        s = result[k+1]
        result = strtok(s[0]," ",/extract)
        if valid_num(result[0]) then $
            tamb = strtrim(string(format='(%"%8.1f C")',result[0]),2)
        if valid_num(result[1]) then begin
            wvp = strtrim(string(format='(%"%8.1f")',result[1]),2)
            h2o = strtrim(string(format='(%"%8.1f mm")',result[1]),2)
        endif
        if valid_num(result[2]) then $
            wind = strtrim(string(format='(%"%8.1f mph")',result[2]),2)
     endif

   ; Phase
     phase = "?"
     sql = string(format='(%"select avg(rms) from PMN where rms > 0 and ut >= \"%s\" and ut <= \"%s\"")',ut_start,ut_stop)
     result = dbi_sql_submit(sql)
     k = where (strmatch(result,'*-----------*') eq 1,nk)
     if nk eq 1 and k[0] ne -1 and k[0] lt (n_elements(result)-1) then begin
        s = result[k+1]
        avephase = 0.0
        if valid_num(s[0],avephase) then begin
           deg_to_mm = 299792458.0/11.8e9/360.0*1000.0
           avephase = avephase * deg_to_mm
           phase = strcompress(string(format='(%"%10.2f mm")',avephase))
        endif
     endif

   ; Hour angle converage for source that was observed the most
     sources = uti_distinct(in(pis).isource,nsources)
     nmax = 0
     ha_min = 100.0
     ha_max = -100.0
     for i = 0, nsources-1 do begin
        j = where(in.isource eq sources[i],nj)
        if (nj gt nmax) then begin
           nmax = nj
           ha_min = min([in[j].ha])
           ha_max = max([in[j].ha])
        endif
     endfor
     ha = (nmax eq 0) ? "?"  : string(format='(%"%4.1f,%4.1f")',ha_min,ha_max)
     ha = strcompress(ha,/remove)
     
   ; Print results
     printlog,"Title  :",strtrim(title,2),unit=unit
     if piproject ne ' ' then printlog,"PI     : ",piproject,unit=unit
     printlog,format='(%"Project: %7d    Date: %11s    Observer    : %s")',$
         project,obsdate,observer,unit=unit
     printlog,format='(%"Config : %7s    Nint: %11d    Ant offline : %s")',$
         config[0],nint,ant_offline,unit=unit1
     printlog,format='(%"Tamb   : %7s    HA  : %11s    Track length: %s")',$
         tamb,ha,tracklength,unit=unit
     printlog,format='(%"Phase  : %7s    H2O : %11s    Wind        : %s")',$
         phase,h2o,wind,unit=unit

   ; Print historical weather conditions
     s = strsplit(obsdate,/extract)
     sut = strsplit(c.ut(n_elements(in)/2),/extract)
     stime = strsplit(sut[3],':',/extract)
     ut = float(stime[0]) + float(stime[1])/60.0 + float(stime[2])/3600.0
     if (ut ge 12.0) then ut = ut - 12.0
     if (strpos(stime[3],'PM') ne -1) then ut = ut + 12.0
;    weather,month=s[0],day=fix(s[1]),ut=ut,unit=unit,phase=phase,h2o=h2o

   ; Print any warning messages
     iwarning = 0;
     if nproj gt 1 then begin
        iwarning = 1
        printlog,"WARNING: Track contains multiple project numbers"
     endif
     if nsoid gt 1 then begin
        iwarning = 1
        printlog,"WARNING: Track contains multiple SOIDs"
     endif
     if n_elements(config) gt 1 then begin
        iwarning = 1
        printlog,"WARNING: Track contains multiple configurations"
        config = config[0]
     endif
end
