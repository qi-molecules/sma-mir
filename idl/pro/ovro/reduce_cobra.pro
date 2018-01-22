function plot_cobra_pb,at_ovro,noreset=noreset,plotpdf=plotpdf,pdfonly=pdfonly
   common global
   common data_set
   common plo

   ; Message
     print,""
     print,"                     ******************************"
     print,"                     *      PLOTTING PASSBAND     *"
     print,"                     ******************************"
     print,""

   ; Set filter for passband plots. Include bright sources.
   ; The user can overwride this by using the /noreset keyword.
     if not keyword_set(noreset) then begin
        com = '"band" in ["w0","w2","w3","w4","w5","w6","w7"] and ' + $
            '"wt" gt "0" and ' + $
            '"source" in ["3c345","3c84","3c273","mars","mercury","3c454.3"]'
        result = dat_filter(s_f,com,/reset,/no)
        if (result eq 0) then begin
           print,"No suitable sources for COBRA passband plots"
           return,0
        endif
     endif

   ; Loop over sources. Make two passes. On the first pass, plot to
   ; screen. On the second pass, print postscript.
     distinct_sources = uti_distinct(c.source(in(pil).isource),nsources)
     j1 = keyword_set(pdfonly) ? 2 : 1
     j2 = (keyword_set(plotpdf) or keyword_set(pdfonly) ? 2 : 1)
     for j=j1,j2 do begin
        if (j eq 2) then begin 
           print,"                 *******************************************"
           print,"                 * GENERATING PDF IMAGES OF COBRA PASSBAND *"
           print,"                 *******************************************"
        endif
        for i=0,nsources-1 do begin
           ; Set output plot device
             if j eq 2 then begin
                ; Set output postscript file
                  root = string(in(0).traid) + '_' + distinct_sources[i]
                  output_ps  = getenv('HOME') + "/" + root + '.ps'
                  output_ps  = strcompress(output_ps,/remove)
                  file_delete,output_ps,/quiet

                ; Set output pdf file
                  path = (at_ovro) ? '/mm/html/logs/cobra/pb/' : $
                         (getenv('HOME') + "/")
                  ; output_gif = path + root + '.gif'
                  ; output_gif = strcompress(output_gif,/remove)
                  output_pdf = path + root + '.pdf'
                  output_pdf = strcompress(output_pdf,/remove)
                  file_delete,output_pdf,/quiet

                ; Set plot device
                  plid = 0
                  plot_device_old = pl.plot_device
                  pl.plot_device = 'ps'
                  set_plot,'ps'
                  device,filename=output_ps,/color,/landscape
             endif

           ; Set filter
             com = '"source" eq "' + distinct_sources[i] + '"'
             result = dat_select(s_s,com,/reset,/no)

           ; Make plot
             result = reduce_plo_spec('fsky','amp,pha','blcd','band','sb',$
                        15,plid,user_pt=[3,3],user_line=[0,0])

           ; Close device
             if j eq 2 then begin
                ; Close postscript file and reset plot device
                  device,/close
                  set_plot,'x'
                  pl.plot_device = plot_device_old

                ; Convert postscript file to gif image
                  ; path = at_ovro ? $
                  ;          '/home/jmc/bin/' : '/opt/SUNWspro/bin/'
                  ; command = path + "pstogif -flip rotate90 -density 120 -out "$
                  ;   + output_gif + " " + output_ps + " 2>&1 /dev/null"
                  command = "ps2pdf " + output_ps + " " + output_pdf
                  spawn,command,result
                  file_delete,output_ps,/quiet
             endif
        endfor
     endfor
end

function plot_cobra_int,at_ovro,noreset=noreset,plotpdf=plotpdf,pdfonly=pdfonly,$
            unit=unit,doplots=doplots
   common global
   common data_set
   common plo

   ; Reset filter to include all data
     if not keyword_set(noreset) then $
        result = dat_filter(s_f,'"wt" gt "0"',/reset,/no)

   ; Print header
     printlog,"",unit=unit
     printlog,"Following table lists (actual integration time)/(requested integration time)",unit=unit
     printlog,"   1.00 -> No  data have been blanked",unit=unit
     printlog,"   0.00 -> All data have been blanked",unit=unit
     printlog,"    Band  F(tint)",unit=unit
     printlog,"    ----- -------",unit=unit

   ; Loop over combinations
     bands = ["c1","c2","s1","s2","s3","s4","w0","w1","w2",$
              "w3","w4","w5","w6","w7","w8"]
     xdata = [0]
     ydata = [0.0]
     frames = [""]
     colors = [""]
     tint = in.rinteg
     nscans  = max(in.int)
     nscans1 = nscans - 1
     for iband=0,n_elements(bands)-1 do begin
        ; Initialize
          total_int_requested = 0.0;
          total_int_actual = 0.0;
          fraction = 0.0
          xadd = 0
          yadd = 0.0
          cadd = ""
          iset_zero  = replicate(0,nscans)
          iset_unity = replicate(0,nscans)

        ; Loop over baselines/sidebands
          for iblcd=0,n_elements(c.blcd)-1 do begin
          for isb=0,n_elements(c.sb)-1 do begin
              ; Initialize
                int_blcd = in.int
                col_blcd = replicate(c.blcd(iblcd),nscans)
                fra_blcd = make_array(nscans)

              ; Get data; compute fraction of time integrating per integration
                com = '"band" eq "' + bands[iband] + '" and ' + $
                      '"blcd" eq "' + c.blcd[iblcd] + '" and ' + $
                      '"sb" eq "' + c.sb[isb] + '"'
                if (dat_select(s_s,com,/reset,/no) gt 0) then begin
                   for i=0,nscans1 do begin
                      j = where(in(pil).int eq i+1,nj)
                      if (nj gt 0) then begin
                         fra_blcd[i] = $
                              total(re.integs(prl(j[0]):prl(j[0])+sp(psl(j[0])).nrec-1))/tint[i]
                      endif
                   endfor
                endif
                fraction = [fraction,fra_blcd]
                
              ; Remove any duplicate zero or entries to save space
                j = where(fra_blcd eq 0.0 and iset_zero eq 0,nj)
                if (nj gt 0) then begin
                   iset_zero[j] = 1
                   xadd = [xadd,int_blcd(j)]
                   cadd = [cadd,col_blcd(j)]
                   yadd = [yadd,fra_blcd(j)]
                endif

              ; Remove any duplicate unity entries to save space
                j = where(fra_blcd eq 1.0 and iset_unity eq 0,nj)
                if (nj gt 0) then begin
                   iset_unity[j] = 1
                   xadd = [xadd,int_blcd(j)]
                   cadd = [cadd,col_blcd(j)]
                   yadd = [yadd,fra_blcd(j)]
                endif

              ; Add non-zero/unity elements
                j = where(fra_blcd ne 1.0 and fra_blcd ne 0.0,nj)
                if (nj gt 0) then begin
                   iset_unity[j] = 1
                   xadd = [xadd,int_blcd(j)]
                   cadd = [cadd,col_blcd(j)]
                   yadd = [yadd,fra_blcd(j)]
                endif
          endfor
          endfor

        ; Compute amount of data lost
          fraction = fraction[1:n_elements(fraction)-1]
          fraction_lost = total(fraction) / n_elements(fraction)
          bandlabel = string(format='(A,": ",F4.2)',bands[iband],fraction_lost)

        ; Print table
          printlog,format='("     ",A,"    ",F4.2)',bands[iband],fraction_lost,unit=unit

        ; Fraction
          if (n_elements(yadd) gt 1) then begin
             xdata  = [xdata,xadd[1:n_elements(xadd)-1]]
             ydata  = [ydata,yadd[1:n_elements(yadd)-1]]
             colors = [colors,cadd[1:n_elements(cadd)-1]]
             frames = [frames,replicate(bandlabel,n_elements(yadd)-1)]
          endif
     endfor

   ; Blank space
     printlog,unit=unit," "

   ; Return if not plotting data
     if not keyword_set(doplots) then return,1
     print,""
     print,"  *** PLOTTING TIME OBSERVED PER INTEGRATION ***"
     print,""

   ; Remove dummy elements
     xdata  = xdata[1:n_elements(xdata)-1]
     ydata  = ydata[1:n_elements(ydata)-1]
     frames = frames[1:n_elements(frames)-1]
     colors = colors[1:n_elements(colors)-1]

   ; Plot data
     y = fltarr(1,n_elements(ydata))
     y[0,*] = ydata
     a = findgen(16) * (!DPI*2/16.)
     usersym,0.20*cos(a),0.20*sin(a),/fill
     blabel = strcompress("Track " + string(in(0).traid))

   ; Plot data
     j1 = keyword_set(pdfonly) ? 2 : 1
     j2 = (keyword_set(plotpdf) or keyword_set(pdfonly) ? 2 : 1)
     for j=j1,j2 do begin
        ; Message
          if (j eq 2) then begin 
             print,"                   ***************************************"
             print,"                   * GENERATING PDF IMAGES OF TIME PLOTS *"
             print,"                   ***************************************"
          endif

        ; Set output plot device
          if j eq 2 then begin
             ; Set output postscript file
               root = string(in(0).traid)
               output_ps  = getenv('HOME') + "/" + root + '.ps'
               output_ps  = strcompress(output_ps,/remove)
               file_delete,output_ps,/quiet

             ; Set output pdf file
               path = (at_ovro) ? '/mm/html/logs/cobra/time/' : $
                         (getenv('HOME') + "/")
               output_pdf = path + root + '.pdf'
               output_pdf = strcompress(output_pdf,/remove)
               file_delete,output_pdf,/quiet

             ; Set plot device
               plid = 0
               plot_device_old = pl.plot_device
               pl.plot_device = 'ps'
               set_plot,'ps'
               device,filename=output_ps,/color,/landscape
          endif

        ; Make plot
          result = wlm_plot_data(xdata,y,xdata,$
             frames,'Integration','f(tint)',plid,nframes=15,$
             psym=[8],plot=1,blabel=blabel,colors=colors);

        ; Close device
          if j eq 2 then begin
             ; Close postscript file and reset plot device
               device,/close
               set_plot,'x'
               pl.plot_device = plot_device_old

             ; Convert postscript file to gif image
               command = "ps2pdf " + output_ps + " " + output_pdf
               spawn,command,result
               file_delete,output_ps,/quiet
          endif
     endfor

   ; Release memory
     xdata  = 0
     ydata  = 0
     frames = 0
     colors = 0
     y      = 0
end

function generate_web_page
   common global
   common data_set
   common plo

   dir = '/mm/html/logs/cobra/'
   tmpfile  = dir + 'temp.html'
   htmlfile = dir + 'index.html'
   file_delete,tmpfile,/quiet
   spawn,'/home/jmc/ovro/cgi-bin/cobralogs '+e.user_name+' > '+tmpfile,result
   spawn,'/usr/bin/mv -f '+tmpfile+" "+htmlfile,result
end

function reduce_cobra,noreset=noreset,plotpdf=plotpdf,pdfonly=pdfonly,$
           noweb=noweb,unit=unit,doplots=doplots
   common global
   common data_set
   common plo

   ; At OVRO???
     loadct,39,/silent
     at_ovro = (getenv('HOST') ne 'radio')

   ; Plot data
     if keyword_set(doplots) then $
        result = plot_cobra_pb(at_ovro,noreset=noreset,plotpdf=plotpdf,pdfonly=pdfonly)
     result = plot_cobra_int(at_ovro,noreset=noreset,plotpdf=plotpdf,pdfonly=pdfonly,doplots=doplots,unit=unit)

   ; Generate web page
     if (at_ovro and not keyword_set(noweb)) then result = generate_web_page()

   ; Reset filter
     result = dat_filter(s_f,/reset,/no)
end
