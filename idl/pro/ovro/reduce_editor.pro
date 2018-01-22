function reduce_editor
     if not keyword_set(defaults) and not keyword_set(nologs) and $
        getenv("EDITOR") eq '' then begin
        print,""
        print,'Choose an editor. To avoid this message in the future, '
        print,'set the environment variable "EDITOR" in the .cshrc file.'
        print,'e.g. setenv EDITOR vi'
        editors = ["vi", "emacs", "textedit"]
        editor_default = 0
        ieditor = editor_default
        ierr = 1
        print,"   (1) vi  (default)"
        print,"   (2) emacs"
        print,"   (3) textedit"
        input = ''
        read,prompt="Choice? ",input
        if (strtrim(input,2) eq '') then begin
           ieditor = editor_default
           ierr = 0
        endif else if valid_num(input) then begin
           x = 0.0
           reads,input,x
           if 1.0*fix(x) eq x and fix(x) ge 1 and fix(x) le n_elements(editors) then begin
              ierr = 0
              ieditor = fix(x)-1
           endif
        endif
        if (ierr) then begin
           print,"Error choosing editor."
           print,"Using the ",editors[ieditor]," editor by default"
        endif else $
           print,"Using the ",editors[ieditor]," editor"
        setenv,"EDITOR="+editors[ieditor]
     endif
end
