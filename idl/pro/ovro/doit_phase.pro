pro doit_phase,day=day,night=night,nophase=nophase,noh2o=noh2o,$
               percentile=percentile

   ; Set months
     month = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
     days_per_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

   ; Set day and UT interval
     dut   = 3
     ndays = 4

   ; Make two passes. On first pass, run H2O, and second pass, run phase
     for itype=0,1 do begin
        ; Indicate if we should run this type
          if (itype eq 0) then begin
             irun = keyword_set(noh2o) ? 0 : 1
             type = 'H2O'
             no_h2o = 0
             no_pha = 1
             output = "history_h2o.dat"
          endif else begin
             irun = keyword_set(nophase) ? 0 : 1
             type = 'Phase'
             no_h2o = 1
             no_pha = 0
             output = "history_phase.dat"
          endelse

        ; Open output file
          unit_out = -1
          if (irun) then openw,unit_out,output,/get_lun 
          print_header = 1
          unit = unit_out

        ; Loop over months
          for imonth=0,n_elements(month)-1 do begin
             for iday=1,days_per_month[imonth],3 do begin
              ; Set date
                date = month[imonth] + ' ' + string(iday) + ' 2002'

              ; Loop over UT
                for ut=0.0,23.0,2.0 do begin
                   ; Run program
                     if (irun) then begin
                       ; Run program
                         phase,month=month[imonth],xh2o=xh2o,xpha=xpha,$
                           day1=iday,ndays=ndays,percentile=percentile,$
                           ut1=ut,dut=dut,nopha=no_pha,noh2o=no_h2o,/noprint

                       ; Print header
                         if (print_header) then begin
                            printf,unit,format='(%"#MO Mon Da UT ",$)'
                            for j=0,n_elements(percentile)-1 do $
                              printf,unit,format='(%" %6.1f",$)',percentile[j]
                            printf,unit,""
                         endif
                         print_header = 0
                            
                       ; Print results
                         printf,unit,format='(%" %2d %s %2d %2d ",$)',$
                            imonth+1,month[imonth],iday,ut
                         x = no_h2o ? xpha : xh2o
                         x = x < 99.99
                         for j=0,n_elements(x)-1 do $
                              printf,unit,format='(%" %6.3f",$)',x[j]
                         printf,unit,""
                     endif
                endfor
             endfor
          endfor

        ; Close output file
          if (irun) then begin
             close,unit_out
             free_lun,unit_out
          endif
     endfor
end
