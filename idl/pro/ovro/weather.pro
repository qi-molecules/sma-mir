; Search range
;    month : 3 letter character or integer (1-12) indicating month to search
;    day   : day of of the month
;    ut    : ut time
; Inputs
;     value    : Find data exceeding value
;     percent  : Find data exceeding percent
;     his_per  : optional, list of percentiles in history file
;     his_data : optional, list of data in history file
; Output
;    xval  : actual data value exceeding value or percent
;    xper  : actual data value exceeding value or percent

function get_weather_data,type,imonth,db_day,db_ut,$
                          xval=xval,xper=xper,$
                          value=value,percent=percent,$
                          his_per=his_per,his_data=his_data
   ; Set root names
     if (type eq 'h2o') then begin
        root = 'h2o'
     endif else if (type eq 'phase') then begin
        root = 'phase'
     endif else begin
        print,"Error entering root name"
        stop
     endelse

   ; Open input file
     if not (keyword_set(his_per) and keyword_set(his_data)) then begin
        dir = ['/home/kokadjo/jmc/idl/pro/ovro',$
               '/home/rdx/idl/pro/ovro']
        input_files = dir + '/history_' + root + '.dat'
        j = where(file_test(input_files) eq 1,nj)
        if (nj eq 0) then return,-1
        input = input_files[j(0)]

      ; Read the file line in the file - this must be the header line
        result = read_weather_data(input,num_records=1)
        field = tag_names(result)
        result = (field[0] eq 'FIELD1') ? result.field1 : result.field001

      ; Read the header line to determine the percentiles
        i1 = 4
        i2 = n_elements(result[*,0])-1
        his_per = result[i1:i2]

      ; Now read the results
        result = read_weather_data(input,comment='#')
        field = tag_names(result)
        his_data = (field[0] eq 'FIELD1') ? result.field1 : result.field001
     endif
     i1 = 4
     i2 = n_elements(his_data[*,0])-1
     if not keyword_set(imonth) then return,-1

   ; Find appropriate month
     jmonth = where(fix(his_data[0,*]) eq imonth+1,nmonths)
     if (nmonths eq 0) then return,-1

   ; Days
     dday = fix(abs(his_data[2,jmonth] - db_day))
     j = where (dday eq min(dday))
     good_days = his_data[*,jmonth(j)]

   ; Get closest UT time
     dut = min(abs(good_days[3,*] - db_ut),imin_ut)
     xval = good_days[i1:i2,imin_ut]

   ; If values is set, then determine where historical values exceed value
   ; If values is not set, then determine where percentages exceed percent
     if (keyword_set(value)) then begin
        x = min([abs(xval-value)],j)
     endif else if keyword_set(percent) then begin
        x = min([abs(his_per-percent)],j)
     endif else begin
        x = min([abs(his_per-50.0)],j)
     endelse
     imin_per = j(0)
     xper = his_per[imin_per]
     xval = xval[imin_per]

   ; Done
     return,1
end

pro weather,month=month,day=day,ut=ut,h2o=h2o,phase=phase,percent=percent,$
            unit=unit
   ; Month vectors
     months_abr  = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", $
                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
     months_full = ["January", "February", "March", "April", "May", "June",$
               "July", "August", "September", "October", "November", "December"]
     days_per_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

   ; Conversion factor from deg phase to mm phase
     deg_to_mm = 299792458.0/11.8e9/360.0*1000.0

   ; Read current time using UNIX in order to get PST or PDT
     spawn,"date",result

   ; Split up time
     db_month = ''
     db_day   = 0
     db_year  = 0
     timezone = ' '
     s = strsplit(result[0],/extract)
     db_month = s[1]
     reads,s[2],db_day
     reads,s[5],db_year

   ; Reset day
     if keyword_set(day) then db_day = day

   ; Reset month
     if keyword_set(month) then begin
        imonth = 0
        if valid_num(month,imonth) then begin
           if (imonth lt 1 or imonth gt 12) then begin
              printf,"Error entering month"
              return
           endif
           db_month = months_abr[imonth-1]
        endif else $
           db_month = month
     endif

   ; Get integer month value
     imonth = where(strlowcase(db_month) eq strlowcase(months_abr),nj)
     if (nj eq 0) then begin
        print,"Error reading month"
        print,"Month = ",db_month
     endif
     imonth = imonth[0]

   ; Read local time
     s = strsplit(s[3],':',/extract)
     localtime = float(s[0]) + float(s[1])/60.0 + float(s[2])/3600.0

   ; Convert local time to UT time if needed. If the user entered
   ; a UT time, then set that here.
     lt_to_ut = (timezone='PST') ? 8.0 : 7.0
     db_ut = keyword_set(ut) ? ut : localtime + lt_to_ut

   ; Round UT time to the nearest hour
     db_ut = fix(db_ut + 0.5)  ; Round to nearest hour
     if db_ut ge 24 then db_ut = db_ut - 24

   ; Initialize
     ierr = [0,0]

   ; Set nominal percentile
     if not keyword_set(percent) then percent = 50.0

   ; If the H2O and phase have not been entered, and it is the current
   ; date, then get the average phase and H2O over the past DT hours
     dt = 3.0
     iset_current = 0
     if not (keyword_set(month) or keyword_set(day) or keyword_set(ut) or $
             keyword_set(phase) or keyword_set(h2o)) then begin
        ; Flag to indicate current conditions should be set
          iset_current = 1

        ; Get H2O
          sql = 'select avg(h2o) from WEA where h2o > 0 and utstart >= dateadd(hh,' + string(lt_to_ut-dt) + ',getdate())'
          result = dbi_sql_submit(sql)
          k = where (strmatch(result,'*-----------*') eq 1,nk)
          if nk eq 1 and k[0] ne -1 and k[0] lt (n_elements(result)-1) then begin
             s = result[k+1]
             aveh2o = 0.0
             if not valid_num(s[0],aveh2o) then aveh2o = 0
          endif

        ; Get Phase
          sql = 'select avg(rms) from PMN where rms > 0 and ut > dateadd(hh,' + string(lt_to_ut-dt) + ',getdate())'
          result = dbi_sql_submit(sql)
          k = where (strmatch(result,'*-----------*') eq 1,nk)
          if nk eq 1 and k[0] ne -1 and k[0] lt (n_elements(result)-1) then begin
             s = result[k+1]
             avephase = 0.0
             if not valid_num(s[0],avephase) then begin
                avephase = 0
             endif else begin
                avephase = avephase * deg_to_mm
             endelse
          endif
     endif

   ; Print historical results
     printlog,"",unit=unit
     printlog,format='(%"Historical weather conditions for %s %d at UT=%d hours")',$
            months_full[imonth],db_day,db_ut,unit=unit
     ; H2O
       result = get_weather_data('h2o',imonth,db_day,db_ut,$
                                 value=h2o,xper=xper,xval=xval,percent=percent,$
                                 his_per=his_per_h2o,his_data=his_data_h2o)
       if (result eq -1) then begin
          printlog,'   --- Error reading H2O   data file'
       endif else if not keyword_set(h2o) and xper eq 50.0 then begin
          printlog,format='(%"   --- Average %5s = %5.2f mm")',$
                   'H2O',xval,unit=unit
       endif else begin
          printlog,format='(%"   --- %4.1f%% of days have %5s < %5.2f mm")',$
                   xper,'H2O',xval,unit=unit
       endelse
     ; Phase
       result = get_weather_data('phase',imonth,db_day,db_ut,$
                   value=phase,xper=xper,xval=xval,percent=percent,$
                   his_per=his_per_pha,his_data=his_data_pha)
       if (result eq -1) then begin
          printlog,'   --- Error reading phase data file'
       endif else if not keyword_set(phase) and xper eq 50.0 then begin
          printlog,format='(%"   --- Average %5s = %5.2f mm")',$
                   'phase',xval,unit=unit
       endif else begin
          printlog,format='(%"   --- %4.1f%% of days have %5s < %5.2f mm")',$
                   xper,'phase',xval,unit=unit
       endelse

   ; If current values are set, then print how indicate how current conditions
   ; stack up historyically.
     if iset_current then begin
        print,""
        print,format='(%"Average conditions over the past %3.1f hours")',dt
        if keyword_set(aveh2o) then $
           print,format='(%"   --- average H2O   = %5.2f mm")',aveh2o else $
           print,'   --- Error reading H2O   from database'
        if keyword_set(avephase) then $
           print,format='(%"   --- average phase = %5.2f mm")',avephase else $
           print,'   --- Error reading phase from database'
        if keyword_set(aveh2o) or keyword_set(avephase) then begin
           print,""
           print,"Approximate percentages for current conditions"
        endif
        ; H2O
          if keyword_set(aveh2o) then begin
             result = get_weather_data('h2o',imonth,db_day,db_ut,$
                           value=aveh2o,xper=xper,xval=xval,percent=percent,$
                           his_per=his_per_h2o,his_data=his_data_h2o)
             if (result eq -1) then begin
             endif else begin
                print,format='(%"   --- %4.1f%% of days have %5s < %5.2f mm")',$
                   xper,'H2O',xval
             endelse
          endif
        ; Phase
          if keyword_set(avephase) then begin
             result = get_weather_data('phase',imonth,db_day,db_ut,$
                           value=avephase,xper=xper,xval=xval,percent=percent,$
                           his_per=his_per_pha,his_data=his_data_pha)
             if (result eq -1) then begin
                print,'   --- Error reading phase data file'
             endif else begin
                print,format='(%"   --- %4.1f%% of days have %5s < %5.2f mm")',$
                   xper,'phase',xval
             endelse
          endif
     endif
end
