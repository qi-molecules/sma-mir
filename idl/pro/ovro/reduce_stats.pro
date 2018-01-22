function reduce_stats,track,config,piproject,project,obsdate,$
          quality,status,avefreq,wvp,avetsys,ampsig,phasig
   ; Root file name
     root = 'ovro_sumlog_'
     dir = '/mm/logs/sumlog/'

   ; If track is set, then create a new summary file
     if keyword_set(track) then begin
        ; Open summary file for this track
          unit = 0
          output = strcompress(dir + root + string(track),/rem)
          openw,unit,output,/get_lun

        ; Print info
          printf,unit,format='(%"%5d | %20s | %20s | %3s | %4d | %8s | %8s | %5.1f | %6s | %6d | %6s | %6s")',$
                track,obsdate,piproject,config,project,quality,status,avefreq,$
                wvp,fix(avetsys),ampsig,phasig

        ; Close file
          close,unit
          free_lun,unit

     endif

   ; Now make the summary web page
     print,""
     print,"Generating observing log web page..."
     print,""
     com = 'ls -1 ' + dir + root + '*'
     spawn,com,result,error
     if (n_elements(result) eq 0 or error[0] ne '') then begin
        print,"Error listing summary files"
        return,-1
     endif

   ; Read track number in each of these files and sort by inverse number
     tracknum = strmid(result,strlen(dir)+strlen(root))
     tracknum = reverse(tracknum(sort(tracknum)))

   ; If someone has edited the files in this directory, there could be
   ; file names with emacs extensions, so I need to make sure each 
   ; file is a valid number. Valid num only works on scalars.
     use = intarr(n_elements(tracknum))
     for i=0,n_elements(tracknum)-1 do begin
         x = 0.0
         if valid_num(tracknum[i],x) then $
             use[i] = (x eq 1.0*fix(x) and x gt 0.0) ? 1 : 0
     endfor
     j = where(use eq 1,nj)
     if (nj eq 0) then begin
        print,"No summary files available"
        return,-1
     endif
     tracknum = tracknum(j)

   ; Create temporary new file
     tempfile = '/mm/html/logs/temp.html'
     com = 'cp -f /mm/logs/logs_template.html ' + tempfile
     spawn,com,result,err
     if (err[0] ne '') then begin
        print,"Error creating temporary file"
        return,-1
     endif

   ; Open temporary file
     unit_html = 0
     openw,unit_html,tempfile,/get_lun,/append

   ; Start table
     printf,unit_html,'<center><table border=2>'
     printf,unit_html,'<caption><b>Track History</b></caption>'
     printf,unit_html,'<TR valign=top bgcolor=#ffcccc><TH>Track<TH>Date<TH>PI<TH>Config<TH>Project<TH>Quality<TH>Status<TH>Freq<BR>[GHz]<TH>H<sub>2</sub>O<BR>[mm]<TH>Tssb@zen<BR>[K]<TH>Sig(amp)<TH>Sig(pha)<BR>[mm]'
     align =[' align=center',' align=center',' align=left',' align=center',$
             ' align=center',' align=center','','','','','','']

   ; Loop over files, read info, and print html file
     for i=0, n_elements(tracknum)-1 do begin
        ; Open input file
          input = dir + root + tracknum[i]
          openr,unit,input,/get_lun

        ; Read info
          readf,unit,input
          s = strtok(input,'|',/extract)

        ; Close file
          close,unit
          free_lun,unit

        ; Reset track to be the link to the observing log
          s[0] = strtrim(s[0],2)
          s[0] = string(format='(%"<a href=\"http://www.ovro.caltech.edu/cgi-bin/jmc/ovrologs?send_email=0&start_log=1&track=%s&log=redlog_ascii\">%s</a>")',s[0],s[0])

        ; Print web stuff
          htmlcode = '<TD' + align + '>' + s
          printf,unit_html,format='("<TR align=right>",$)'
          for j=0,n_elements(htmlcode)-1 do $
             printf,unit_html,format='(A,$)',htmlcode(j)
          printf,unit_html,""
     endfor

   ; End table
     printf,unit_html,'</table></center>'

   ; Close file
     close,unit_html
     free_lun,unit_html

   ; Copy file to new logfile
     spawn,'mv -f ' + tempfile + ' /mm/html/logs/ovrologs.html',result,err
     if (err[0] ne '') then begin
        print,"Error moving temporary file"
        return,-1
     endif

   ; Done
     return,1
end
