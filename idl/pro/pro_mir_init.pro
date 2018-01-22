pro pro_mir_init,uname,upass,camplog,jv,db
;
; Sets up initial state of idl for mir 
;
; Input Parameters
;	uname - Unix/DB user login - String
;	camplog - String: ovro,caltech,cfa,sma
; no longer 	camplog - Ovro login (0), Campus login (1) or  nick login (2)
;	jv - Operating from java (1) or from a command line (0) - Bool
;	db - Debug mode (1) or Not (0)
;
; Replacement routine for pro_mir_start
; Calls a routine which sets the proper environment variables if not set
; Sets up the global data structure e
;

;
; DEFINE COMMON BLOCKS
;
common global,e,ms
common plo,pl,pl_par,pl_index,s_page,gai,pas,con,spe,wlm,map,var, $
	gai_par,con_par,wlm_par,spe_par,pas_par,var_par
common data_set,in,bl,sp,c,ch,re,ca,pi,pb,ps,pc, $
		pr,pif,pbf,psf,pcf,prf,          $
                pis,pbs,pss,pcs,prs,             $
                pil,pbl,psl,pcl,prl

common wlm,wlm_ref_time,wlm_ntel,wlm_itel,       $
           wlm_times,wlm_id,wlm_raw,wlm_wts,     $
           wlm_start,wlm_stop,wlm_cal,wlm_gains, $
           wlm_egains, wlm_xfactor,wlm_filter,   $
           wlm_icoherence

;
; DEFINE environment STRUCTURE
; db_user removed because it will be the same as user_name
; dbprocess added so that the db connection can be preserved 
; campuslogin added so that we know if we're logged in on campus or ovro
;
e={user_name:'abc',password:'mmmmmm',save_fn:'default',dbprocess:0L,server:'SYBASE',      $
	rdx_dir:'a',idl_dir:'a',dataset_dir:'/tmp',start_date:'a',			   $
	date_tag:'a',journal:'a',debug:0,xsize:0,ysize:0,			   $
	prog_help:0,idl:'a',idl_sql:'a',idl_src:'a',campuslogin:'a',java:0,          $
	login_str:'aa',printer:'a',editor:'a',idl_sav:'a',idl_pro:'a',idl_bcp:'a'}

;global plot parameters
pl_par={active:0,plot_device:'x',plot_interact:1,plot_file:'a',plot_date:'a', $
	plot_zoom:0, mouse_x:0.0,mouse_y:0.0,mouse_button:'l', $
	plot_copy:0,plot_panel:-1,plot_key:'',plot_done:0, $
	num_pages:0,plot_type:''}
pl=replicate(pl_par,15)

gai_par={initial:0} & pas_par={initial:0} & con_par={initial:0}
spe_par={initial:0} & wlm_par={initial:0} & var_par={initial:0}
con=0 & gai=0 & pas=0 & spe=0 & spe=0 & wlm=0 & var=0	
s_page=0 & map=0
wlm_ref_time=0 & wlm_ntel=0 & wlm_itel=0 & wlm_times=0 & wlm_id=0 & wlm_raw=0 
wlm_wts=0 & wlm_start=0 & wlm_stop=0 & wlm_cal=0 & wlm_gains=0 & wlm_egains=0
wlm_xfactor=0 & wlm_filter = '' & wlm_icoherence=0
;
; set up e structure variables which determine environement
; and operational mode for mir
;
!more=0				; OK FOR JAVA?
e.start_date=systime(0)
e.debug=db
e.server=getenv('DSQUERY')
host=getenv('HOST')
case camplog of
      0 : camplog='ovro' 
      1 : camplog='caltech'
      2 : camplog='nick'
      3 : camplog='sma'
      4 : camplog='cfa'
else : camplog=camplog
endcase

if host eq 'nick' then camplog='nick'
e.campuslogin = camplog
e.java = jv
if e.java then begin
  case camplog of
      'ovro'    : e.server='SYBASE' 
      'caltech' : e.server='SYBASE1'
      'nick'    : e.server='SYBASE2'
      'sma'     : e.server='unknown'
      'cfa'     : e.server='unknown'
  else : e.server='SYBASE'
  endcase  
endif

!except=2   ; report over and underflow error locations - OK FOR JAVA?

;
; set up journal and plot files with date tag
;
parts=strtrim(strsplit(e.start_date,' ',/extract),2L)
parts=parts(where (parts ne ''))
parts(3)=strmid(parts(3),0,strpos(parts(3),':',/reverse_search))
e.date_tag=''
for i=0L,n_elements(parts)-2 do begin
 e.date_tag=e.date_tag+'_'+parts(i) 
endfor
e.journal='journal'+e.date_tag
;journal,e.journal

;
; Set environment variables
  pro_setup_env

; Prompt and verify username/password. This loop is executed until the 
; login is correct.
; Allow passworrd/username to be set as environment variables
  e.user_name=uname
  if (uname eq 'mmvis') then e.user_name = 'mm'
  if (getenv('MIR_NAME') ne "") then e.user_name = getenv('MIR_NAME')
  ipassword_env = (getenv('MIR_PASS') ne "");
  if (ipassword_env) then e.password  = getenv('MIR_PASS')
  if (e.campuslogin eq 'ovro' or e.campuslogin eq 'caltech' or $
      e.campuslogin eq 'nick' or e.campuslogin eq 'cfa') then begin
     if e.java then begin
        e.password=upass
     endif else begin
        irepeat = 1
        while (irepeat) do begin
           ; Initialize
             user_name = ''
             password=''

           ; Get user name
             suser = strcompress('enter db username (default: ' + e.user_name + ') : ')
             if (not ipassword_env) then read,user_name,prompt=suser
             if (user_name eq '') then user_name = e.user_name

           ; Get password
             spass = 'enter db password for ' + user_name
             for i = 1, 33-strlen(spass) do spass = spass + ' '
             spass = spass + ': '
             if (ipassword_env) then $
               password = e.password $
             else $
               read,password,prompt=spass

           ; Verify username/password is correct by submitting test sql query
             teststring = 'LoginSuccessful'
             com = 'print "' + teststring + '"'
             result = dbi_sql_submit(com,user_name=user_name,password=password)
             if (result[0] eq teststring) then begin
                irepeat = 0
                e.user_name = user_name
                e.password  = password
             endif else begin
                print,""
                print,"*****************************"
                print,"    Database login FAILED    "
                print,"*****************************"
                print,""
                if (ipassword_env) then stop
             endelse
        endwhile
     endelse
  endif

;
; unix environment
; First call the routine to make sure all the environment vars are there
;
e.login_str=' -U'+e.user_name+' -P'+e.password+' -S'+e.server+' '
e.rdx_dir=getenv('RDXDIR')
e.idl=e.rdx_dir+'/idl/'
e.idl_sav=e.idl+'sav/'
e.idl_sql=e.idl+'sql/'
e.idl_pro=e.idl+'pro/'
e.idl_bcp=e.idl+'bcp/'
e.idl_src=e.idl+'src/'
e.idl_dir=getenv('IDL_DIR')
e.printer=getenv('PRINTER')
e.editor=getenv('EDITOR')
if e.debug then print,'   IDL src dir: ',e.idl_src
if e.debug then print,'db server: ',e.server, '   user name: ',e.user_name, '  password: ',e.password
if e.debug then print,'   printer: ',e.printer, '   editor: ',e.editor
if e.debug then print,'idl dir: ',e.idl, '   idl_pro dir: ',e.idl_pro
if e.debug then print,'idl_sav dir: ',e.idl_sav, '   idl_sql dir: ',e.idl_sql
if e.debug then print,'idl_bcp dir: ',e.idl_bcp

case e.campuslogin of
      'caltech' : e.dataset_dir='/scr/rdx' 
      'ovro'    : e.dataset_dir='/scr/rdx'
      'nick'    : e.dataset_dir='/scr/rdx'
      'cfa'     : begin
                     e.dataset_dir='./'
                     e.idl_bcp='./'
                     e.prog_help = 0
                  end
      'sma'     : begin
                     e.dataset_dir='./'
                     e.idl_bcp='./'
                     e.prog_help = 0
                  end
endcase
pl.plot_file=getenv('HOME')+'/plot_'+e.user_name+'_'+e.date_tag
if e.debug then print, 'Dataset directory',e.dataset_dir

if e.prog_help then begin
 help,/structure,e
endif
; set plot font 
!P.font=0
if e.java then begin
   !P.font=1 
   e.xsize=500
   e.ysize=550
endif else begin
  e.xsize =650
  e.ysize=500
endelse
; Set constants as system environment variables
pro_constants
end
