function dat_what_is_it

; Looks at the data set and writes out some basic information
; on the contents.
; There are no parameters. Usage:  result=dat_what_is_it()

common global
common data_set





; Get the number of integrations. The idl function size
; returns a vector with 
;	1	the number of dimensions
;	2	the size of the first dimension
;	other stuff
; We know in.int is 1D, so the 2nd element is the number of
; integrations

sizeOfArray = size(in.int)
numberOfIntegrations = sizeOfArray(1)
print,'There are ',sizeOfArray(1),' integrations in the data set'

;print out the integrations, time, source, date in a format
print,' '
print,'Here are the integrations:'
print, format='("Int. No.",T15,"Minutes",T25,"Source",T40,"Date and Time")'
for i = 0,numberOfIntegrations-1 DO BEGIN
print, format='(I5,T15,F6.1,T25,A,T40,A)', $
	in(i).int,in(i).rinteg/60., $
	c.source(in(i).isource), c.ut(in(i).iut)
endfor

; Move down to the baseline level of the tree. Check how
; many baselines and sidebands.

sizeOfArray = size(bl.iblcd)
numberOfblRows = sizeOfArray(1)
;print,numberOfblRows,' rows in bl'

print,' '
; Find out how many sidebands there are in the whole data set
array = bl.isb
uniqueElements = array(UNIQ(array, SORT(array)))
sideBands = uniqueElements
sizeOfArray = size(uniqueElements)
numberOfSidebands = sizeOfArray(1)
print,'There are ',numberOfSidebands,' sidebands in the whole data set'

; Find out how many baselines there are in the whole data set
array = bl.iblcd
uniqueElements = array(UNIQ(array, SORT(array)))
baselines = uniqueElements
;print,uniqueElements
sizeOfArray = size(uniqueElements)
numberOfBaselines = sizeOfArray(1)
print,'There are ',numberOfBaselines,' baselines in the whole data set'

; If  the 
; number of entries (rows) = number of ints * number of sb * number bl
; probably means that each int has the same number of sidebands and bl
; Maybe this is always the case, so don't worry too much about
; the alternative.
if (numberOfblRows EQ numberOfIntegrations * numberOfSidebands $
	* numberOfBaselines ) then begin
  print,"    There are probably the same number of sidebands and baselines"
  print,"    in each integration. (bl rows = int * sb * bl)"
endif else begin
  print,"    There must be different numbers of sidebands or baselines"
  print,"    in the different integrations." 
  print,"    number of integration ",numberOfIntegrations
  print,"    number of baselines   ",numberOfBaselines   
  print,"    number of sidebands   ",numberOfSidebands   
  print,"    number of bl rows     ",numberOfblRows
  print,"    Use the following commands:"
  print,"        print,bl.isb"
  print,"        print,bl.iblcd"
endelse
print,' '

; Move down to the SP level of the data

sizeOfArray = size(sp.sphid)
numberOfspRows = sizeOfArray(1)


; Find out how many spectral bands there are in the whole data set
array = sp.iband
uniqueElements = array(UNIQ(array, SORT(array)))
sizeOfArray = size(uniqueElements)
numberOfBands = sizeOfArray(1)
;print,'There are ',numberOfBands,' spectral bands in the whole data set'
bands = uniqueElements

;FOR i = 0,numberOfBands-1 DO BEGIN
;  print, 'Code number ',bands(i),'  Band ',c.band(bands(i))
;ENDFOR
;print, 'sp rows ',numberOfspRows

; 1 is upper sideband
; 0 is lower sideband

; Find out whether the bands are in the upper sideband or lower 
; sideband.

bandIsUpper = intarr(5)
bandIsLower = intarr(5)

FOR i = 0,numberOfspRows-1 DO BEGIN
  CASE bl(pb(i)).isb OF
    0: BEGIN
      CASE sp(i).iband OF
        0: bandIsUpper(sp(i).iband) = 1
        1: bandIsUpper(sp(i).iband) = 1
        2: bandIsUpper(sp(i).iband) = 1
        3: bandIsUpper(sp(i).iband) = 1
        4: bandIsUpper(sp(i).iband) = 1
      ENDCASE
      END
    1: BEGIN
      CASE sp(i).iband OF
        0: bandIsLower(sp(i).iband) = 1
        1: bandIsLower(sp(i).iband) = 1
        2: bandIsLower(sp(i).iband) = 1
        3: bandIsLower(sp(i).iband) = 1
        4: bandIsLower(sp(i).iband) = 1
      ENDCASE
    END
    ENDCASE
ENDFOR

;print,'Upper  ',bandIsUpper
;print,'Lower  ',bandIsLower

junk = WHERE(bandIsUpper,numberUpper)
junk = WHERE(bandIsLower,numberLower)

print,'There are ',numberUpper,' spectral bands in the Upper sideband'
print,'There are ',numberLower,' spectral bands in the Lower sideband'

print,'     Upper Sideband     Lower Sideband'
FOR i = 0,numberOfBands-1 DO BEGIN
  print,c.band(i),bandIsUpper(i),'          ',bandIsLower(i)
ENDFOR

; Find out how many sp rows are in each of the integrations.

numberOfspInt = INTARR(numberOfIntegrations)

FOR i = 0,numberOfIntegrations-1 DO BEGIN
  junk = WHERE(pi eq i,count)
  numberOfspInt(i) = count
;  print,'i count ',i,count,numberOfspInt(i)
ENDFOR

; Find out how many channels there are in each band and in
; each integration.

channelsPerBand = INTARR(max([5,numberOfBands]))

i1 = 0
i2 = numberOfspInt(0)
FOR i = i1,i2-1 DO BEGIN
  CASE sp(i).iband OF
    0: channelsPerBand(0) = sp(i).nch
    1: channelsPerBand(1) = sp(i).nch
    2: channelsPerBand(2) = sp(i).nch
    3: channelsPerBand(3) = sp(i).nch
    4: channelsPerBand(4) = sp(i).nch
  ENDCASE
ENDFOR

print,' '
print,'Channels per band in the first integration. Source ', $
  c.source(in(1).isource)
FOR i = 0,numberOfBands-1 DO BEGIN
  print,'Band ',c.band(bands(i)),'  Channels ',channelsPerBand(i)
ENDFOR


i1 = i1 + numberOfspInt(0)
i2 = i2 + numberOfspInt(0)

FOR j = 1,numberOfIntegrations-1 DO BEGIN
  same = 1
;print,'i1 i2',i1,i2
  FOR i = i1,i2-1 DO BEGIN
    if (sp(i).nch NE channelsPerBand(sp(i).iband)) then begin
      same = 0
    endif
  ENDFOR 

  if (same EQ 1) then begin
    print, 'Same channels per bands for integration ',j+1,' Source ', $
  c.source(in(j).isource)
  endif else begin
 FOR i = i1,i2-1 DO BEGIN
  CASE sp(i).iband OF
    0: channelsPerBand(0) = sp(i).nch
    1: channelsPerBand(1) = sp(i).nch
    2: channelsPerBand(2) = sp(i).nch
    3: channelsPerBand(3) = sp(i).nch
    4: channelsPerBand(4) = sp(i).nch
  ENDCASE
 ENDFOR
 print,'Channels per band in integration',j+1,' Source ', $
  c.source(in(j).isource)
 FOR i = 0,numberOfBands-1 DO BEGIN
  print,'Band ',c.band(bands(i)),'  Channels ',channelsPerBand(i)
 ENDFOR
  endelse

 i1 = i1 + numberOfspInt(j) 
 i2 = i2 + numberOfspInt(j)

ENDFOR

;FOR i = 0,numberOfspRows-1 DO BEGIN
;print,'i nrec band sb int ',i,sp(i).nrec,sp(i).iband,bl(pb(i)).isb $
;,in(pi(i)).int
;ENDFOR

; Below is the same for number of records per band instead of the
; the number of channels per band.

recordsPerBand = INTARR(max([5,numberOfBands]))

i1 = 0
i2 = numberOfspInt(0)
FOR i = i1,i2-1 DO BEGIN
  CASE sp(i).iband OF
    0: recordsPerBand(0) = sp(i).nrec
    1: recordsPerBand(1) = sp(i).nrec
    2: recordsPerBand(2) = sp(i).nrec
    3: recordsPerBand(3) = sp(i).nrec
    4: recordsPerBand(4) = sp(i).nrec
  ENDCASE
ENDFOR

print,' '
print,'Records per band in the first integration. Source ', $
  c.source(in(1).isource)
FOR i = 0,numberOfBands-1 DO BEGIN
  print,'Band: ',c.band(bands(i)),'  Records: ',recordsPerBand(i)
ENDFOR


i1 = i1 + numberOfspInt(0)
i2 = i2 + numberOfspInt(0)

FOR j = 1,numberOfIntegrations-1 DO BEGIN
  same = 1
;print,'i1 i2',i1,i2
  FOR i = i1,i2-1 DO BEGIN
    if (sp(i).nrec NE recordsPerBand(sp(i).iband)) then begin
      same = 0
    endif
  ENDFOR 

  if (same EQ 1) then begin
    print, 'Same records per bands for integration ',j+1,' source ', $
  c.source(in(j).isource)
  endif else begin
 FOR i = i1,i2-1 DO BEGIN
  CASE sp(i).iband OF
    0: recordsPerBand(0) = sp(i).nrec
    1: recordsPerBand(1) = sp(i).nrec
    2: recordsPerBand(2) = sp(i).nrec
    3: recordsPerBand(3) = sp(i).nrec
    4: recordsPerBand(4) = sp(i).nrec
  ENDCASE
 ENDFOR
 print,'Records per band in integration',j+1,' Source ', $
  c.source(in(j).isource)
 FOR i = 0,numberOfBands-1 DO BEGIN
  print,'Band: ',c.band(bands(i)),'  Records: ',recordsPerBand(i)
 ENDFOR
  endelse

 i1 = i1 + numberOfspInt(j) 
 i2 = i2 + numberOfspInt(j)

ENDFOR

print,' '
print,' '
print,'That summarizes the structure of the data.'
print,'Could also print out more astronomical information such as '
print,'the band frequencies, the spectral line codes, etc'




end
