; Search range of days in a month
;    month : 3 letter character or integer (1-12) indicating month to search
;    year  : Nominal year
;    day1  : starting day of the month
;    day2  : ending   day of the month
;
; Search +/- ndays around current day
;    date  : Specific day in the format "Mon dd, yyy".
;    ndays : number of days to search are found date.
;    year  : If set, search only for the year specified in date
;
; Specify UT hours
;    ut1   : beginning UT hour
;    ut2   : ending    UT hour
;    dut   : UT interval to search
;
; Options
;    noh2o : Do not print H2O   results
;    nophase : Do not print phase results
;    percentiles : Output percentiles
;    xh2o  : Output percentiles for H2O  results
;    xpha  : Output percentiles for phase results
;
;    noprint   : Do not print results
;    nighttime : Set UT times to night.
;    daytime   : Set UT times to day.
;
; Defaults:
;    date  = current date
;    ndays = 7

pro phase,month=month,day1=day1,day2=day2,$
          date=date,ndays=ndays,year=year,$
          noh2o=noh2o,nophase=nophase,$
          xpha=xpha,xh2o=xh2o,percentiles=percentiles,$
          noprint=noprint,$
          nighttime=nighttime,daytime=daytime,ut1=ut1,ut2=ut2,dut=dut
   ; Common blocks
     common global
     common data_set

   ; Month vectors
     months_of_year = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", $
                       "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
     days_per_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
                       
   ; Error checking. Can only have one set of parameters set
     if (keyword_set(month) or keyword_set(day1) or keyword_set(day2) and $
        keyword_set(date)) or $
        (keyword_set(day2) and keyword_set(ndays)) then begin
        print,"Error entering arguments"
        print,"Must choose either month/day1/ndays , month/day1/day2, or date/ndays"
        return
     endif
     if (keyword_set(dut1) and not keyword_set(ut1)) or $
        (keyword_set(dut1) and keyword_set(ut1) and keyword_set(ut2)) or $
        (keyword_set(ut1) and not keyword_set(ut2) and not keyword_set(dut)) then begin
        print,"Invalid combination of ut1/ut2/dut"
        return
     endif
     if (keyword_set(dut) and (keyword_set(nighttime) or keyword_set(daytime))) or $
        (keyword_set(ut1) and (keyword_set(nighttime) or keyword_set(daytime))) or $
        (keyword_set(nighttime) and keyword_set(daytime)) then begin
        print,"Invalid combination of ut1/ut2/dut and/or nighttime/daytime"
        return
     endif

   ; Read current month, day, year
   ; These are the default search dates
     db_date  = keyword_set(date) ? date : systime(0)
     ifield   = keyword_set(date) ? [0,1,2] : [1,2,4]
     db_day1  = 0
     db_year1 = 1994
     db_year2 = 0
     s = strsplit(db_date,/extract)
     db_month = s[ifield(0)]
     reads,s[ifield(1)],db_day1
     reads,s[ifield(2)],db_year2
     db_day2 = db_day1

   ; Get number of days to average over. This can be overwritten by 
   ; day1 and day2
     if not keyword_set(ndays) then ndays = 14
     if (ndays lt 0) then ndays = 14

   ; Get year
     search_all_years = (keyword_set(year) or keyword_set(date)) ? 0 : 1

   ; Evaluate input criteria in order of precedence. IFIELD is used to 
   ; parse the data string to mm/dd/year
     if keyword_set(month) then begin
        ; See if it is a character or an interger
          imonth = 0
          if valid_num(month,imonth) then begin
             if (imonth lt 1 or imonth gt 12) then begin
                print,"Error entering month. Must be between 1 and 12"
                return
             endif
             month = months_of_year[imonth-1]
          endif

        ; Check that month is valid
          if strlen(month) gt 3 then month = strmid(month,0,3);
          j = where (strlowcase(month) eq strlowcase(months_of_year),nj)
          if (nj eq 0) then begin
             print,"Error entering month on command line"
             print,"Please use 3 letter character abbreviations"
             return
          endif
          month = months_of_year[j(0)]

        ; Set month and day
          db_month = month
          db_day1 = 1
          db_day2 = days_per_month[j(0)]
     endif 

   ; Set first and last day if day1 and day2 are set
   ; NDAYS, if set, takes precedence over db_day2
     if keyword_set(day1) then begin
       db_day1 = day1
       db_day2 = keyword_set(day2) ? day2 : day1
     endif

   ; Set year
     if not search_all_years then begin
        if keyword_set(date) then begin
           db_year1 = db_year2
        endif else begin
           db_year1 = year
           db_year2 = year
        endelse
     endif

   ; Set UT times
     db_ut1 = keyword_set(ut1) ? 1.0*ut1 : 0.0
     db_ut2 = keyword_set(ut2) ? 1.0*ut2 : db_ut1
     if keyword_set(dut) then begin
        db_ut1 = db_ut1 - dut
        if db_ut1 lt  0.0 then db_ut1 = db_ut1 + 24.0

        db_ut2 = db_ut2 + dut
        if db_ut2 gt 24.0 then db_ut2 = db_ut2 - 24.0
     endif else if keyword_set(nighttime) then begin
        db_ut1 = 8.0
        db_ut2 = 14.0
     endif else if keyword_set(daytime) then begin
        db_ut1 = 20.0
        db_ut2 =  2.0
     endif

   ; Set vector of percentiles
     if not keyword_set(percentiles) then begin
        percentiles = 1.0 * (lindgen(99) + 1)
     endif

   ; Set which variables to search for
     ierr = [0,0]
     isearch = [1,1]
     if keyword_set(noh2o)   then isearch[0] = 0
     if keyword_set(nophase) then isearch[1] = 0
     xh2o = dblarr(n_elements(percentiles))
     xpha = xh2o

   ; Make 2 loops. On the first loop, get H2O, and the second loop, get phases
     for j = 0, 1 do begin
       if (isearch[j]) then begin
        ; Set variable names
          utvar = (j eq 0) ? 'utstart' : 'ut'
          var   = (j eq 0) ? 'h2o'     : 'rms'
          table = (j eq 0) ? 'WEA'     : 'PMN'
          var_print   = (j eq 0) ? 'H2O' : 'Phase'
          var_unit    = (j eq 0) ? 'mm' : 'mm'
          conversion  = (j eq 0) ? 1.0 : 299792468.0/11.8e9/360.0*1000.0

        ; Set times
          com = ''
          for i=db_year1,db_year2 do begin
             if not keyword_set(ndays) then begin
                s = string(format='(%"(%s >= \"%s %d %d\" and %s <= \"%s %d %d\")")',$
                    utvar,db_month,db_day1,i,utvar,db_month,db_day2,i)
             endif else begin
                s = string(format='(%"abs(datediff(dd,%s,\"%s %d %d\")) < %d")',utvar,db_month,db_day1,i,ndays)
             endelse
             if (i gt db_year1) then com = com + ' or '
             com = com + s
          endfor

        ; Construct SQL clause
          sql = string(format='(%"select %s from %s where %s > 0 and (%s)")',$
             var,table,var,com)
          s = string(format='(%"datepart(hh,convert(char(11),%s,108))")',utvar)
          if db_ut1 ne db_ut2 then begin
             if (db_ut1 lt db_ut2) then begin
                sql = sql + string(format='(%" and %s >= %5.1f and %s <= %5.1f")',s,db_ut1,s,db_ut2)
             endif else begin
                sql = sql + string(format='(%" and (%s >= %5.1f or %s <= %5.1f)")',s,db_ut1,s,db_ut2)
             endelse
          endif

        ; Submit sql command
          result = dbi_sql_submit(sql)

        ; Read results
          if (n_elements(result) le 10) then begin
             ierr[j] = 1
             print,"Too few data point for ",var_print
          endif else begin
             ; Remove sql headers/trailers and multiply by conversion factor
             ; needed to convert to mm
               result = result[3:n_elements(result)-3] * conversion

             ; Sort
               result = result(sort(result))

             ; Find percentile values
               n = long(percentiles/100.0 * n_elements(result))
               x = result[n]
               if j eq 0 then xh2o = x else xpha = x
          endelse

        ; Print results
          if not keyword_set(noprint) and not ierr[j] then begin
             ; Print date
               print,'# Month  : ',db_month

             ; Days
               if db_day1 eq db_day2 then begin
                  print,format='(%"# Days   : %d +/- %d")',db_day1,ndays
               endif else begin
                  print,format='(%"# Days   : %d to %d")',db_day1,db_day2
               endelse

             ; Year
               if db_year1 eq db_year2 then begin
                  print,format='(%"# Year   : %d")',db_year1
               endif else begin
                  print,format='(%"# Years  : %d to %d")',db_year1,db_year2
               endelse

             ; UT time
               if (db_ut1 ne db_ut2) then begin
                  if keyword_set(night) then begin
                     print,format='(%"# UT     : night time (%4.1f to %4.1f hours)")',db_ut1,db_ut2
                  endif else if keyword_set(day) then begin
                     print,format='(%"# UT     : day time (%4.1f to %4.1f hours)")',db_ut1,db_ut2
                  endif else begin
                     print,format='(%"# UT     : %4.1f to %4.1f hours")',db_ut1,db_ut2
                  endelse
               endif

             ; Print table
               print,'# Percentile      ',var_print
               print,'                 (',var_unit,')'
               print,'# ----------   --------'
               for i=0,n_elements(x)-1 do begin
                   print,format='(%"     %5.1f    %7.3f")',percentiles[i],x[i]
               endfor
          endif
       endif
     endfor
end
