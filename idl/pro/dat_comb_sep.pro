function dat_comb_sep,combinations,vars,codes,icodes,n_components 
;
; Separates a list of combined codes into components
;
; Returns separate lists of the components character codes
; and their integer code equivalents. The separaors between
; component character strings are single spaces.
;
; eg. : result=dat_comb_sep(combinations,['blcd','rec'],codes, $
;     :                     icodes,n_components)
;
; inputs:
;   combinations : array of strings like ['3-4 3mm','3-5 1mm', ...]
;   vars         : contents of combinations like ['blcd','rec'] in the
;                  above case.
; outputs:
;   codes        : string array of size [n_components,n_combinations],
;                  containing character codes of vars for each combination.
;                  note that the array is 1D if n_combinations=1.
;   icodes       : int array of size [n_components,n_combinations],
;                  containing integer codes of vars for each combination.
;                  each integer code is a pointer to the c structure.
;                  i.e., to c.blcd and c.rec in the case above. 
;   n_components : n_elements(vars)
;
common global
common data_set

;
; find out number of separate codes and set up output arrays
;
n_components=n_elements(vars)
if n_components eq 0 then begin
  codes='' & icodes=-1 & return,0
endif
n_combinations = n_elements(combinations)
if n_combinations eq 0 then begin
  codes='' & icodes=-1 & return,0
endif else begin
  codes =make_array(n_components,n_combinations,/string,value='')
  icodes=make_array(n_components,n_combinations,/int,value=-1)
endelse

;
; lookup codes and icodes & treat a few aliases 
;
for i=0,n_combinations-1 do begin
  codes[*,i]=strlowcase(strsplit(combinations[i],' ',/extract))
  js=where(codes[*,i] eq '3mm',count) & if count ne 0 then codes[js,i]='1'
  js=where(codes[*,i] eq '1mm',count) & if count ne 0 then codes[js,i]='2'
  for j=0,n_components-1 do begin
    case vars[j] of
      'blcd'   : icodes[j,i]=min(where(codes[j,i] eq c.blcd))
      'rec'    : icodes[j,i]=min(where(codes[j,i] eq c.rec))
      'band'   : icodes[j,i]=min(where(codes[j,i] eq c.band))
      'sb'     : icodes[j,i]=min(where(codes[j,i] eq c.sb))
      'pol'    : icodes[j,i]=min(where(codes[j,i] eq c.pol))
      'pstate' : icodes[j,i]=min(where(codes[j,i] eq c.pstate))
      'aq'     : icodes[j,i]=min(where(codes[j,i] eq c.aq))
      else: begin 
        print,'*** ',vars[j],' not recognized in uti_comb_sep.pro !'
        return,-1
      endelse
    endcase
  endfor
endfor

return,1
end

; 2002-10-02 KS 
;   removed redundant lines, added comments. no functional change.

