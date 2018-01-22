pro autoRead,directory=directory,sideband=sideband,dataselect=dataselect

common global
common data_set
common wlm
common eng, en

; print,keyword_set(sideband) 

if not keyword_set(sideband) then sideband = 'l'

if not keyword_set(dataselect) then dataselect = 'a'

;print,sideband

if strmid(sideband,0,1) eq 'l' or  strmid(sideband,0,1) eq 'L' then sideband = 'l'
if strmid(sideband,0,1) eq 'u' or  strmid(sideband,0,1) eq 'U' then sideband = 'u'
if strmid(dataselect,0,1) eq 'a' or  strmid(dataselect,0,1) eq 'A' then dataselect = 'a'
if strmid(dataselect,0,1) eq 'v' or  strmid(dataselect,0,1) eq 'V' then dataselect = 'v'

;print,sideband


if keyword_set(directory) then begin
    nchar = strlen(directory)
;    print,'nchar =',nchar
    checkslash = strmid(directory,nchar-1,1)
;    print,'checkslash ',checkslash
;    print, 'return ', strmatch(checkslash,'/') 
    if ( strmatch(checkslash,'/') le 0 ) then directory = directory + '/'
;    print,directory
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
    if (e.campuslogin eq 'sma' or e.campuslogin eq 'cfa') then begin
        count = 0
        result = findfile(e.idl_bcp+'eng_read',count=count)
        if (count eq 0) then begin
            print,'could not find file ',e.idl_bcp+'eng_read'
            return
        endif
        print,'found eng_read'
    endif

endif 

widjetz,sideband,dataselect

end
