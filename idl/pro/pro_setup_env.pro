pro pro_setup_env

;
; Makes sure that the environment variables that
; are used by pro_mir_init are set before they are accessed
;
; Relies on campus login for how to set the variables
;
; NOTE: This does not set any IDL environment variables because
; 	by the time we reach here IDL is already started - IDL environment
;	variables are set by ION when the iond is started
;
common global
tmpstring=''

tmpstring = getenv('DSQUERY')
if(tmpstring eq '') then setenv,'DSQUERY=SYBASE'

tmpstring = getenv('RDXDIR')
if(tmpstring eq '') then begin
    if(e.campuslogin eq 'caltech') then begin
	setenv,'RDXDIR=/home/radio/rdx'
    endif
    if(e.campuslogin eq 'ovro') then begin
	setenv,'RDXDIR=/home/rdx'
    endif
    if(e.campuslogin eq 'nick') then begin
	setenv,'RDXDIR=/home/nick/rdx'
    endif
endif

tmpstring = getenv('SYBASE')
if(tmpstring eq '') then begin
    if(e.campuslogin eq 'caltech') then begin
	setenv,'SYBASE=/home/radio/sybase/sybase-12_5'
    endif 
    if(e.campuslogin eq 'ovro') then begin
	setenv,'SYBASE=/home/sybase'
    endif 
    if(e.campuslogin eq 'nick') then begin
	setenv,'SYBASE=/home/nick/sybase/sybase-12_5'
    endif
endif

end
