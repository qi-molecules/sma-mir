function dbi_eng_read

;  Reads the engineering data that is recorded in eng_read

common global
common data_set

common eng, en

enh_temp = {			$
	antenna_number:0L,	$
	pad_number:0L,		$
	antenna_status:0L,	$
	track_status:0L,	$; track program status
	comm_status:0L,		$; status of real time SMA communications
	inhid:0L,		$; integration header id number
	ints:0L,		$; integration number
	dhrs:0D,		$; decimal hours since midnight
	ha:0D,			$
	lst:0D,			$
	pmdaz:0D,		$
	pmdel:0D,		$
	tiltx:0D,		$
	tilty:0D,		$
	actual_az:0D,		$
	actual_el:0D,		$
	azoff:0D,		$
	eloff:0D,		$
	az_tracking_error:0D,	$
	el_tracking_error:0D,	$
	refraction:0D,		$
	chopper_x:0D,		$
	chopper_y:0D,		$
	chopper_z:0D,		$
	chopper_angle:0D,	$
	tsys:0D,		$
	ambient_load_temperature:0D$
}

openr,unit,e.idl_bcp+'eng_read',/get_lun,error=err
if err ne 0 then begin
  print,!err_string
  return,1
endif

; There is one structures worth of data for each antenna 
; for each integration.
; 2 integrations with 3 antennas each will have been
; stored as six "rows" of 188 bytes each.

status = fstat(unit) & nrows = status.size/188
if e.debug then print,nrows,' enh rows'

en = replicate(enh_temp,nrows)
point_lun,unit,0L
for i=0L,(nrows-1L) do  begin
  readu,unit,enh_temp
  if (strpos(!VERSION.ARCH,'x86') ge 0) or (strpos(!VERSION.ARCH,'alpha') ge 0) then $
     enh_temp = swap_endian(enh_temp)
  en(i)=enh_temp
  if e.debug then print, 'eng data: ', $
  'int id # '+strtrim(string(en(i).inhid),2)+'  ant # ' $
   + strtrim(string(en(i).antenna_number),2)+ $
  '  time ',en(i).dhrs,' status '+strtrim(string(en(i).antenna_status),2)
endfor

close,unit & free_lun,unit

;Use these to get the first and last sets of each integration.
; The difference (last-first)+1 is the number of antennas in each integration
;last = uniq(en.ints)
;first = [0,last[0:n_elements(last)-2]+1]

end
