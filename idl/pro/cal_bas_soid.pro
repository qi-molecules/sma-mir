; *************************************************************************
; FUNCTION
;      CAL_BAS_SOID
;
; WRITTEN 
;      February 13, 1998 by JMC
;
; PURPOSE
;      Read a baseline solution (soid) from the database into memory
;
; INPUTS 
;      soid_num : ID number for baseline solution to be read into memory
;      soid     : Structure that will be used to store the soid information 
;
; OUTPUT
;     -1 : SOID not successfully read from database
;      1 : SOID successfully read from database and stored in soid
;
; EXAMPLE
;      result = cal_bas_soid(743,soid)
; *************************************************************************
function cal_bas_soid, soid_num, soid
   ; Define structure for soid
     soid_str = {tel:0,dte:0.0D,dtn:0.0D,dtu:0.0D,dta:0.0D,doff:0.0D,loff:0.0D}

   ; Allocate memory based on the total number of telescopes in the array
     soid = replicate(soid_str,!NTEL)

   ; Create/submit SQL command to read the SOID from the database.
   ; The database output/soid solution is stored in "result"
     com = 'select tel,dte,dtn,dtu,dta,doff,loff from TPO where soid=' $
              + string(soid_num)
     result=dbi_sql_submit(com)

   ; Determine the number of antennas in the baseline solution
   ; The first two lines of "result" are header information from SQL,
   ; and the last two lines are SQL messages.
     ntel = n_elements(result) - 4
     if ntel le 0 then begin
        com = 'Error reading baseline solution for SOID = ' + string(soid_num)
        printf,-1,strcompres(com)
        return,-1
     endif

   ; Allocate memory for the SOID
     soid_temp = replicate(soid_str,ntel)

   ; Read the soid into memory.
     reads,result(2:2+ntel-1),soid_temp

   ; Store the baseline solution such that telescope 1 is array element 0, 
   ; telescope 2 in array element 1, etc... I do this since it is possible
   ; that a telescope is missing from the configuration
     soid(soid_temp.tel-1) = soid_temp

   ; Return 1 to indicate successful completion
     return,1
end
