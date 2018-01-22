function pri_data,band=band,sideband=sideband,blcode=blcode,data,time,filname=filname
;
; Creates an ascii file which contains amplitudes, phases, 
; times and baseline ids for the selected band and sideband
; note that this routine writes out RECORD DATA
;
; parameters : band     => which band, 0 = 3mm, 1 = 1mm
;            : sideband => which sideband, 0 = lower, 1 = upper
;            : filname  => String for name of ascii file
; RETURNS    : ASCII file containing baseline, time, amp and phase
;
common global
common data_set
;
; Define variables
;
mylun=0
time = dblarr(9000) 
;
; Get LUN for writing out file if three parms present
;
nparm = n_params()
if nparm eq 6 then begin
	GET_LUN,mylun
	OPENW,mylun,filname ,ERROR=err
	if (err ne 0) then print,!ERR_STRING
end
;
; first determine which baselines are present in the dataset
;
baselines = c.tel1(bl.itel1)+c.tel2(bl.itel2)
baselines_sorted = baselines(uniq(baselines,sort(baselines)))
baselines_nice = c.tel1(bl.itel1)+'-'+c.tel2(bl.itel2)
baselines_nice_sorted = baselines_nice(uniq(baselines,sort(baselines)))
print,''
print,'Unique baselines present in this track are:'
print,''
print,baselines_nice_sorted
print,''
print,'You have selected data from baseline =>',baselines_nice_sorted[where(baselines_sorted eq blcode)]
;
; Define search index values
;  These values are the indices of the sp structure which have the data we want.
;
spindex = ps[where(bl.irec eq band and bl.isb eq sideband and (c.tel1[bl.itel1]+c.tel2[bl.itel2] eq blcode ) ) ]
blindex = pb[where(bl.irec eq band and bl.isb eq sideband and (c.tel1[bl.itel1]+c.tel2[bl.itel2] eq blcode ) ) ]
print,''
print,'Now getting baseline data.'
temp_size = size(spindex)
count1 = 0
count2 = 0
total  = 0
data = complexarr(temp_size[1]*sp[spindex[0]].nrec * sp[spindex[0]].nch )
time = dblarr(temp_size[1]*sp[spindex[0]].nrec * sp[spindex[0]].nch )
;
; Determine Julian Day of Reference Date for Track
;
temp_date = c.ref_time
num_date = uti_sd_nd(temp_date)
ref_date_jul = julday(num_date[1],num_date[2],num_date[0]) -0.5
;
;
;
for count1 = 0, temp_size[1]-1 DO BEGIN
	;
	; Define Start and Stop points in ch structure
	;
	start = pc[spindex[count1]]
	stop = start + (sp[spindex[count1]].nrec * sp[spindex[count1]].nch)-1
	;
	; Get the reference avedhrs for this baseline and int
	;
	ref_time = bl[blindex[count1]].avedhrs
	for count2 = start, stop DO BEGIN
		data[total] = ch[count2]
		time[total] = re.toffs[count2-start]/3600.+ ref_time
                total = total+1
	ENDFOR
ENDFOR

return,1

END
