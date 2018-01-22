function uti_sd_nd,str_date
num_date = make_array(3,/integer)
;
; Break up the Date string
;
str_broken = strsplit(str_date,' ',/extract)
;
; Define month
;
str_month = strtrim(str_broken[0])
; 
; take care of extra comma on day
;
temp_day = str_broken[1]
comma_pos = strpos(temp_day,',')
if comma_pos eq 2 then mylen = 2
if comma_pos eq 1 then mylen = 1
str_day = strtrim(strmid(temp_day,0,mylen))
;
; define year
;
str_year = strtrim(str_broken[2])
;
; Switch str_month to numeric month
;
months = 'JanFebMarAprMayJunJulAugSepOctNovDec'
num_month = (strpos(months,str_month))/3 + 1
;
; Switch str_day to numeric day
;
num_day = long(str_day)
;
; Switch str_year to numeric year
;
num_year = long(str_year)

num_date[0] = num_year
num_date[1] = num_month
num_date[2] = num_day

return,num_date
end

