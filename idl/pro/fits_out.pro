; NAME: FITS_OUT:
; Write out fits uv file for specified source
;      fits_file -- name of output fits file
;      fit_ext   -- file extension
;      source    -- source name
;      pos       -- source position
;      trans     -- transition
;      band      -- spectral band
;      sb        -- sideband
;      tq        -- tuning qualifier
;      pol       -- polarization state
; keyword : shadow -- % shadowing allowed (default = 0)
;           records -- write out record data (0 no, 1 yes)
;
;
; parameters : fits_file   -- output filename
;
; result = -1 (error), 1 (ok)
;
;  
; eg. : result=fits_out('fits_test','UVF','3c454.3','','13CO(2-1)','c1','l','v01','vv')
; eg. : result=fits_out('fits_test','UVF','3c273','','HCN(3-2)','c1','l','v01','vv')
; eg. : result=fits_out('fits_test','UVF','0336-019','','HCN(1-0)','c1','l','v01','hh')
; eg. : result=fits_out('fits_test','UVF','0336-019','','HCN(1-0)','c1','l','v01','hh',/records)

; multiple sources 
; multiple bands and sidebands are permitted, but all the bands must have
; the same number of channels because of limitations in the FITS format.
; Different frequencies, channel widths, and velocities are ok.
; Only one velocity definition is permitted in FITS.
; To request multiple bands, enter a list of bands ['s0','s1','s2'], or
; use '' to get all the bands in the data set (not likely to work because
; the c bands have 1 channel whereas the s bands have more). 
; same syntax for sources and sidebands
;eg. : result=fits_out('fits_test','UVF',['3c454.3','mars'],'','', ['s0','s1'],'u','','')
; only the source, band, and sideband parameters will take a list, the other
; parameters can be used to select one choice, or all using ''

function gettok,st,char
;+
; NAME:
;       GETTOK                                    
; PURPOSE:
;       Retrieve the first part of the string up to a specified character
; EXPLANATION:
;       GET TOKen - Retrieve first part of string until the character char 
;       is encountered.
;
; CALLING SEQUENCE:
;       token = gettok( st, char )
;
; INPUT:
;       char - character separating tokens, scalar string
;
; INPUT-OUTPUT:
;       st - (scalar) string to get token from (on output token is removed)
;
; OUTPUT:
;       token - scalar string value is returned 
;
; EXAMPLE:
;       If ST is 'abc=999' then gettok(ST,'=') would return
;       'abc' and ST would be left as '999' 
;
; NOTES:
;       A version of GETTOK that accepts vector strings is available for users 
;       of IDL V5.3 or later from  http://idlastro.gsfc.nasa.gov/ftp/v53/
; HISTORY
;       version 1  by D. Lindler APR,86
;       Remove leading blanks    W. Landsman (from JKF)    Aug. 1991
;       Converted to IDL V5.0   W. Landsman   September 1997
;-
;----------------------------------------------------------------------
  On_error,2                           ;Return to caller

; if char is a blank treat tabs as blanks

  tab = string(9b)
  while strpos(st,tab) GE 0 do begin    ;Search for tabs
        pos = strpos(st,tab)
        strput,st,' ',pos
  endwhile

  st = strtrim(st,1)              ;Remove leading blanks

; find character in string

  pos = strpos(st,char)
  if pos EQ -1 then begin         ;char not found?
        token = st
        st = ''
        return, token
  endif

; extract token

 token = strmid(st,0,pos)
 len = strlen(st)
 if pos EQ (len-1) then st = '' else st = strmid(st,pos+1,len-pos-1)

;  Return the result.

 return,token
 end

;+
; Project     : SOHO - CDS     
;                   
; Name        : VALID_NUM()
;               
; Purpose     : Check if a string is a valid number representation.
;               
; Explanation : The input string is parsed for characters that may possibly
;               form a valid number.  It is more robust than simply checking
;               for an IDL conversion error because that allows strings such
;               as '22.3qwert' to be returned as the valid number 22.3
;               See also the original NUM_CHK which returns the status in 
;               the opposite sense.
;
; Use         : IDL> status = valid_num(string  [,value]  [,/integer])
;    
; Inputs      : string  -  the string to be tested
;               
; Opt. Inputs : None
;               
; Outputs     : The function returns 1 for valid, 0 for invalid number
;               
; Opt. Outputs: value   - The value the string decodes to.  This will be
;                         returned as a double precision number unless /INTEGER
;                         is present, in which case a long integer is returned.
;               
; Keywords    : Integer   -  if present code checks specfically for an integer.
;
; Calls       : None
;               
; Restrictions: None
;               
; Side effects: None
;               
; Category    : Utilities, Numerical
;               
; Prev. Hist. : Small changes from NUM_CHK by Andrew Bowen, 
;                                             Tessella Support Services, 8/3/93
;
; Written     : CDS version by C D Pike, RAL, 24-May-93
;               
; Modified    : Version 1, C D Pike, RAL, 24-May-93
;               Version 2, William Thompson, GSFC, 14 October 1994
;                       Added optional output parameter VALUE to allow
;                       VALID_NUM to replace STRNUMBER in FITS routines.
;
; Version     : Version 1  24-May-93
;       Converted to IDL V5.0   W. Landsman   September 1997
;-            

FUNCTION valid_num, string, value, INTEGER=integer

                ;**** Set defaults for keyword ****
  IF NOT (KEYWORD_SET(integer)) THEN integer=0

                ;**** arrays of legal characters ****
  numbers       = '0123456789'
  signs         = '+-'
  decimal       = '.'
  exponents     = 'ED'

                ;**** trim leading and trailing blanks/compress white ****
                ;**** space and convert any exponents to uppercase.   ****
  numstr = strupcase(strtrim(strcompress(string),2))

                ;**** length of input string ****
  len = strlen(numstr)

  ok = 1

  if integer eq 0 then stage = 1 else stage = 6

  for i = 0, len-1 do begin

    char = strmid(numstr,i,1)

                ;**** the parsing steps 1 to 8 are for floating   ****
                ;**** point, steps 6 to 8, which test for a legal ****
                ;**** exponent, can be used to check for integers ****

;**** The parsing structure is as follows.  Each character in the ****
;**** string is checked against the valid list at the current     ****
;**** stage.  If no match is found an error is reported.  When a  ****
;**** match is found the stage number is updated as indicated     ****
;**** ready for the next character.  The valid end points are     ****
;**** indicated in the diagram.                                   ****
;
;Stage  1               2               3               4
;
;Valid  sign    --> 2   dec-pt  --> 3   digit   --> 5   dec-pt  --> 5
;  "    dec-pt  --> 3   digit   --> 4                   digit   --> 4
;  "    digit   --> 4                                   exp't   --> 6
;  "                                                    END
;
;Stage  5               6               7               8
;
;Valid  digit   --> 5   sign    --> 7   digit   --> 8   digit   -->8
;  "    exp't   --> 6   digit   --> 8                   END
;  "    END
;

    CASE stage OF

      1 : begin
        if              strpos(signs,char) ge 0         then stage = 2 $
        else if         decimal eq char                 then stage = 3 $
        else if         strpos(numbers,char) ge 0       then stage = 4 $
        else            ok = 0
      end

      2 : begin
        if              decimal eq char                 then stage = 3 $
        else if         strpos(numbers,char) ge 0       then stage = 4 $
        else            ok = 0
      end

      3 : begin
        if              strpos(numbers,char) ge 0       then stage = 5 $
        else            ok = 0
      end

      4 : begin
        if              decimal eq char                 then stage = 5 $
        else if         strpos(numbers,char) ge 0       then stage = 4 $
        else if         strpos(exponents,char) ge 0     then stage = 6 $
        else            ok = 0
      end

      5 : begin
        if              strpos(numbers,char) ge 0       then stage = 5 $
        else if         strpos(exponents,char) ge 0     then stage = 6 $
        else            ok = 0
      end

      6 : begin
        if              strpos(signs,char) ge 0         then stage = 7 $
        else if         strpos(numbers,char) ge 0       then stage = 8 $
        else            ok = 0
      end

      7 : begin
        if              strpos(numbers,char) ge 0       then stage = 8 $
        else            ok = 0
      end

      8 : begin
        if              strpos(numbers,char) ge 0       then stage = 8 $
        else            ok = 0
      end

    ENDCASE

  end

                ;**** check that the string terminated legally ****
                ;**** i.e in stages 4, 5 or 8                  ****
  if (stage ne 4) and (stage ne 5) and (stage ne 8) then ok = 0

                ;**** If requested, then form the value. ****

  if (n_params() eq 2) and ok then begin
        if keyword_set(integer) then value = long(string) else  $
                value = double(string)
  endif

                ;**** return error status to the caller ****
  RETURN, ok


END


        FUNCTION DETABIFY, CHAR_STR
;+
; NAME:
;       DETABIFY
; PURPOSE:
;       Replaces tabs in character strings with appropriate number of spaces
; EXPLANATION:
;       The number of space characters inserted is calculated to space
;       out to the next effective tab stop, each of which is eight characters
;       apart.
;
; CALLING SEQUENCE:
;       Result = DETABIFY( CHAR_STR )
;
; INPUT PARAMETERS:
;       CHAR_STR = Character string variable (or array) to remove tabs from.
;
; OUTPUT:
;       Result of function is CHAR_STR with tabs replaced by spaces.
;
; RESTRICTIONS:
;       CHAR_STR must be a character string variable.
;
; MODIFICATION HISTORY:
;       William Thompson, Feb. 1992.
;       Converted to IDL V5.0   W. Landsman   September 1997
;-
;
        ON_ERROR, 2
;
;  Check the number of parameters.
;
        IF N_PARAMS() NE 1 THEN MESSAGE,'Syntax:  Result = DETABIFY(CHAR_STR)'
;
;  Make sure CHAR_STR is of type string.
;
        SZ = SIZE(CHAR_STR)
        IF SZ[SZ[0]+1] NE 7 THEN BEGIN
                MESSAGE,/INFORMATIONAL,'CHAR_STR must be of type string'
                RETURN, CHAR_STR
        ENDIF
;
;  Step through each element of CHAR_STR.
;
        STR = CHAR_STR
        FOR I = 0,N_ELEMENTS(STR)-1 DO BEGIN
;
;  Keep looking for tabs until there aren't any more.
;
                REPEAT BEGIN
                        TAB = STRPOS(STR[I],STRING(9B))
                        IF TAB GE 0 THEN BEGIN
                                NBLANK = 8 - (TAB MOD 8)
                                STR[I] = STRMID(STR[I],0,TAB) +         $
                                        STRING(REPLICATE(32B,NBLANK)) + $
                                        STRMID(STR[I],TAB+1,STRLEN(STR[I])-TAB-1)
                        ENDIF
                ENDREP UNTIL TAB LT 0
        ENDFOR
;
        RETURN, STR
        END



        FUNCTION FXPAR, HDR, NAME, ABORT, COUNT=MATCHES, COMMENT=COMMENTS, $
                        START=START, PRECHECK=PRECHECK, POSTCHECK=POSTCHECK, $
                                          NOCONTINUE = NOCONTINUE
;+
; NAME: 
;        FXPAR()
; PURPOSE: 
;       Obtain the value of a parameter in a FITS header.
; EXPLANATION: 
;       The first 8 chacters of each element of HDR are searched for a match to
;       NAME.  If the keyword is one of those allowed to take multiple values
;       ("HISTORY", "COMMENT", or "        " (blank)), then the value is taken
;       as the next 72 characters.  Otherwise, it is assumed that the next
;       character is "=", and the value (and optional comment) is then parsed
;       from the last 71 characters.  An error occurs if there is no parameter
;       with the given name.
;      
;       If the value is too long for one line, it may be continued on to the
;       the next input card, using the OGIP CONTINUE convention.  For more info,
;       http://heasarc.gsfc.nasa.gov/docs/heasarc/ofwg/docs/ofwg_recomm/r13.html
;
;       Complex numbers are recognized as two numbers separated by one or more
;       space characters.
;
;       If a numeric value has no decimal point (or E or D) it is returned as
;       type LONG.  If it contains more than 8 numerals, or contains the
;       character 'D', then it is returned as type DOUBLE.  Otherwise it is
;       returned as type FLOAT.    If an integer is too large to be stored as
;       type LONG, then it is returned as DOUBLE.
;
; CALLING SEQUENCE: 
;       Result = FXPAR( HDR, NAME  [, ABORT, COUNT=, COMMENT=, /NOCONTINUE ] )
;
;       Result = FXPAR(HEADER,'DATE')           ;Finds the value of DATE
;       Result = FXPAR(HEADER,'NAXIS*')         ;Returns array dimensions as
;                                               ;vector
; REQUIRED INPUTS: 
;       HDR     = FITS header string array (e.g. as returned by FXREAD).  Each
;                 element should have a length of 80 characters
;       NAME    = String name of the parameter to return.  If NAME is of the
;                 form 'keyword*' then an array is returned containing values
;                 of keywordN where N is an integer.  The value of keywordN
;                 will be placed in RESULT(N-1).  The data type of RESULT will
;                 be the type of the first valid match of keywordN found.
; OPTIONAL INPUT: 
;       ABORT   = String specifying that FXPAR should do a RETALL if a
;                 parameter is not found.  ABORT should contain a string to be
;                 printed if the keyword parameter is not found.  If not
;                 supplied, FXPAR will return with a negative !err if a keyword
;                 is not found.
;       START   = A best-guess starting position of the sought-after
;                 keyword in the header.  If specified, then FXPAR
;                 first searches for scalar keywords in the header in
;                 the index range bounded by START-PRECHECK and
;                 START+POSTCHECK.  This can speed up keyword searches
;                 in large headers.  If the keyword is not found, then
;                 FXPAR searches the entire header.  
;
;                 If not specified then the entire header is searched.
;                 Searches of the form 'keyword*' also search the
;                 entire header and ignore START.
;
;                 Upon return START is changed to be the position of
;                 the newly found keyword.  Thus the best way to
;                 search for a series of keywords is to search for
;                 them in the order they appear in the header like
;                 this:
;
;                       START = 0L
;                       P1 = FXPAR('P1', START=START)
;                       P2 = FXPAR('P2', START=START)
;       PRECHECK = If START is specified, then PRECHECK is the number
;                  of keywords preceding START to be searched.
;                  Default: 5
;       POSTCHECK = If START is specified, then POSTCHECK is the number
;                   of keywords after START to be searched.
;                   Default: 20
; OUTPUT: 
;       The returned value of the function is the value(s) associated with the
;       requested keyword in the header array.
;
;       If the parameter is complex, double precision, floating point, long or
;       string, then the result is of that type.  Apostrophes are stripped from
;       strings.  If the parameter is logical, 1 is returned for T, and 0 is
;       returned for F.
;
;       If NAME was of form 'keyword*' then a vector of values are returned.
;
; OPTIONAL INPUT KEYWORDS: 
;       /NOCONTINUE = If set, then continuation lines will not be read, even
;                 if present in the header
; OPTIONAL OUTPUT KEYWORD:
;       COUNT   = Optional keyword to return a value equal to the number of
;                 parameters found by FXPAR.
;       COMMENTS= Array of comments associated with the returned values.
;
; PROCEDURE CALLS: 
;       GETTOK(), VALID_NUM
; SIDE EFFECTS: 
;
;       The system variable !err is set to -1 if parameter not found, 0 for a
;       scalar value returned.  If a vector is returned it is set to the number
;       of keyword matches found.
;
;       If a keyword occurs more than once in a header, a warning is given,
;       and the first occurence is used.  However, if the keyword is "HISTORY",
;       "COMMENT", or "        " (blank), then multiple values are returned.
;
; NOTES:
;       The functions SXPAR() and FXPAR() are nearly identical, although
;       FXPAR() has slightly more sophisticated parsing.   There is no
;       particular reason for having two nearly identical procedures, but
;       both are too widely used to drop either one.
;
; REVISION HISTORY: 
;       Version 1, William Thompson, GSFC, 12 April 1993.
;               Adapted from SXPAR
;       Version 2, William Thompson, GSFC, 14 October 1994
;               Modified to use VALID_NUM instead of STRNUMBER.  Inserted
;               additional call to VALID_NUM to trap cases where character
;               strings did not contain quotation marks.
;       Version 3, William Thompson, GSFC, 22 December 1994
;               Fixed bug with blank keywords, following suggestion by Wayne
;               Landsman.
;       Version 4, Mons Morrison, LMSAL, 9-Jan-98
;               Made non-trailing ' for string tag just be a warning (not
;               a fatal error).  It was needed because "sxaddpar" had an
;               error which did not write tags properly for long strings
;               (over 68 characters)
;       Version 5, Wayne Landsman GSFC, 29 May 1998
;               Fixed potential problem with overflow of LONG values
;       Version 6, Craig Markwardt, GSFC, 28 Jan 1998, 
;               Added CONTINUE parsing         
;       Version 7, Craig Markwardt, GSFC, 18 Nov 1999,
;               Added START, PRE/POSTCHECK keywords for better performance
;-
;------------------------------------------------------------------------------
;
;  Check the number of parameters.
;
        IF N_PARAMS() LT 2 THEN BEGIN
            PRINT,'Syntax:  result =  FXPAR( HDR, NAME  [, ABORT ])'
            RETURN, -1
        ENDIF
;
;  Determine the abort condition.
;
        VALUE = 0
        IF N_PARAMS() LE 2 THEN BEGIN
            ABORT_RETURN = 0
            ABORT = 'FITS Header'
        END ELSE ABORT_RETURN = 1
        IF ABORT_RETURN THEN ON_ERROR,1 ELSE ON_ERROR,2
;
;  Check for valid header.  Check header for proper attributes.
;
        S = SIZE(HDR)
        IF ( S[0] NE 1 ) OR ( S[2] NE 7 ) THEN $
            MESSAGE,'FITS Header (first parameter) must be a string array'
;
;  Convert the selected keyword NAME to uppercase.
;
        NAM = STRTRIM( STRUPCASE(NAME) )
;
;  Determine if NAME is of form 'keyword*'.  If so, then strip off the '*', and
;  set the VECTOR flag.  One must consider the possibility that NAM is an empty
;  string.
;
        NAMELENGTH1 = (STRLEN(NAM) - 1) > 1
        IF STRPOS( NAM, '*' ) EQ NAMELENGTH1 THEN BEGIN    
            NAM = STRMID( NAM, 0, NAMELENGTH1)  
            VECTOR = 1                          ;Flag for vector output  
            NAME_LENGTH = STRLEN(NAM)           ;Length of name 
            NUM_LENGTH = 8 - NAME_LENGTH        ;Max length of number portion  
            IF NUM_LENGTH LE 0 THEN MESSAGE,    $
                'Keyword length must be 8 characters or less'
;
;  Otherwise, extend NAME with blanks to eight characters.
;
        ENDIF ELSE BEGIN
            WHILE STRLEN(NAM) LT 8 DO NAM = NAM + ' '
            VECTOR = 0
        ENDELSE
;
;  If of the form 'keyword*', then find all instances of 'keyword' followed by
;  a number.  Store the positions of the located keywords in NFOUND, and the
;  value of the number field in NUMBER.
;
        IF N_ELEMENTS(START)     EQ 0 THEN START = -1L
        START = LONG(START[0])
        IF NOT VECTOR AND START GE 0 THEN BEGIN
            IF N_ELEMENTS(PRECHECK)  EQ 0 THEN PRECHECK = 5
            IF N_ELEMENTS(POSTCHECK) EQ 0 THEN POSTCHECK = 20
            NHEADER = N_ELEMENTS(HDR)
            MN = (START - PRECHECK)  > 0
            MX = (START + POSTCHECK) < NHEADER-1
            KEYWORD = STRMID(HDR[MN:MX], 0, 8)
        ENDIF ELSE BEGIN
            RESTART:
            START   = -1L
            KEYWORD = STRMID( HDR, 0, 8)
        ENDELSE

        IF VECTOR THEN BEGIN
            NFOUND = WHERE(STRPOS(KEYWORD,NAM) GE 0, MATCHES)
            IF ( MATCHES GT 0 ) THEN BEGIN
                NUMST= STRMID(HDR[NFOUND], NAME_LENGTH, NUM_LENGTH)
                NUMBER = INTARR(MATCHES)-1
                FOR I = 0, MATCHES-1 DO         $
                    IF VALID_NUM( NUMST[I], NUM) THEN NUMBER[I] = NUM
                IGOOD = WHERE(NUMBER GE 0, MATCHES)
                IF MATCHES GT 0 THEN BEGIN
                    NFOUND = NFOUND[IGOOD]
                    NUMBER = NUMBER[IGOOD]
                ENDIF
            ENDIF
;
;  Otherwise, find all the instances of the requested keyword.  If more than
;  one is found, and NAME is not one of the special cases, then print an error
;  message.
;
        ENDIF ELSE BEGIN
            NFOUND = WHERE(KEYWORD EQ NAM, MATCHES)
            IF MATCHES EQ 0 AND START GE 0 THEN GOTO, RESTART
            IF START GE 0 THEN NFOUND = NFOUND + MN
            IF (MATCHES GT 1) AND (NAM NE 'HISTORY ') AND               $
                (NAM NE 'COMMENT ') AND (NAM NE '') THEN        $
                MESSAGE,/INFORMATIONAL, 'WARNING- Keyword ' +   $
                NAM + 'located more than once in ' + ABORT
            IF (MATCHES GT 0) THEN START = NFOUND[MATCHES-1]
        ENDELSE
;
;  Extract the parameter field from the specified header lines.  If one of the
;  special cases, then done.
;
        IF MATCHES GT 0 THEN BEGIN
            LINE = HDR[NFOUND]
            SVALUE = STRTRIM( STRMID(LINE,9,71),2)
            IF (NAM EQ 'HISTORY ') OR (NAM EQ 'COMMENT ') OR    $
                    (NAM EQ '        ') THEN BEGIN
                VALUE = STRTRIM( STRMID(LINE,8,72),2)
                COMMENTS = STRARR(N_ELEMENTS(VALUE))
;
;  Otherwise, test to see if the parameter contains a string, signalled by
;  beginning with a single quote character (') (apostrophe).
;
            END ELSE FOR I = 0,MATCHES-1 DO BEGIN
                IF ( STRMID(SVALUE[I],0,1) EQ "'" ) THEN BEGIN
                    TEST = STRMID( SVALUE[I],1,STRLEN( SVALUE[I] )-1)
                    NEXT_CHAR = 0
                    OFF = 0
                    VALUE = ''
;
;  Find the next apostrophe.
;
NEXT_APOST:
                    ENDAP = STRPOS(TEST, "'", NEXT_CHAR)
                    IF ENDAP LT 0 THEN MESSAGE,         $
                        'WARNING: Value of '+NAME+' invalid in '+ABORT+ " (no trailing ')", /info
                    VALUE = VALUE + STRMID( TEST, NEXT_CHAR, ENDAP-NEXT_CHAR )
;
;  Test to see if the next character is also an apostrophe.  If so, then the
;  string isn't completed yet.  Apostrophes in the text string are signalled as
;  two apostrophes in a row.
;
                    IF STRMID( TEST, ENDAP+1, 1) EQ "'" THEN BEGIN    
                        VALUE = VALUE + "'"
                        NEXT_CHAR = ENDAP+2      
                        GOTO, NEXT_APOST
                    ENDIF
;
;  Extract the comment, if any.
;
                    SLASH = STRPOS(TEST, "/", ENDAP)
                    IF SLASH LT 0 THEN COMMENT = '' ELSE        $
                        COMMENT = STRMID(TEST, SLASH+1, STRLEN(TEST)-SLASH-1)

;
; CM 19 Sep 1997
; This is a string that could be continued on the next line.  Check this
; possibility with the following four criteria: *1) Ends with '&'
; (2) Next line is CONTINUE  (3) LONGSTRN keyword is present (recursive call to
;  FXPAR) 4. /NOCONTINE is not set

    IF NOT KEYWORD_SET(NOCONTINUE) THEN BEGIN
                    OFF = OFF + 1
                    VAL = STRTRIM(VALUE,2)

                    IF (STRLEN(VAL) GT 0) AND $
                      (STRMID(VAL, STRLEN(VAL)-1, 1) EQ '&') AND $
                      (STRMID(HDR[NFOUND[I]+OFF],0,8) EQ 'CONTINUE') THEN BEGIN
                       IF (SIZE(FXPAR(HDR, 'LONGSTRN',/NOCONTINUE)))[1] EQ 7 THEN BEGIN                    
                      VALUE = STRMID(VAL, 0, STRLEN(VAL)-1)
                      TEST = HDR[NFOUND[I]+OFF]
                      TEST = STRMID(TEST, 8, STRLEN(TEST)-8)
                      TEST = STRTRIM(TEST, 2)
                      IF STRMID(TEST, 0, 1) NE "'" THEN MESSAGE, $
                        'ERROR: Invalidly CONTINUEd string in '+ABORT
                      NEXT_CHAR = 1
                      GOTO, NEXT_APOST
                    ENDIF
                   ENDIF
    ENDIF

;
;  If not a string, then separate the parameter field from the comment field.
;
                ENDIF ELSE BEGIN
                    TEST = SVALUE[I]
                    SLASH = STRPOS(TEST, "/")
                    IF SLASH GT 0 THEN BEGIN
                        COMMENT = STRMID(TEST, SLASH+1, STRLEN(TEST)-SLASH-1)
                        TEST = STRMID(TEST, 0, SLASH)
                    END ELSE COMMENT = ''
;
;  Find the first word in TEST.  Is it a logical value ('T' or 'F')?
;
                    TEST2 = TEST
                    VALUE = GETTOK(TEST2,' ')
                    TEST2 = STRTRIM(TEST2,2)
                    IF ( VALUE EQ 'T' ) THEN BEGIN
                        VALUE = 1
                    END ELSE IF ( VALUE EQ 'F' ) THEN BEGIN
                        VALUE = 0
                    END ELSE BEGIN
;
;  Test to see if a complex number.  It's a complex number if the value and the
;  next word, if any, both are valid numbers.
;
                        IF STRLEN(TEST2) EQ 0 THEN GOTO, NOT_COMPLEX
                        VALUE2 = GETTOK(TEST2,' ')
                        IF VALID_NUM(VALUE,VAL1) AND VALID_NUM(VALUE2,VAL2) $
                                THEN BEGIN
                            VALUE = COMPLEX(VAL1,VAL2)
                            GOTO, GOT_VALUE
                        ENDIF
;
;  Not a complex number.  Decide if it is a floating point, double precision,
;  or integer number.  If an error occurs, then a string value is returned.
;  If the integer is not within the range of a valid long value, then it will 
;  be converted to a double.  
;
NOT_COMPLEX:
                        ON_IOERROR, GOT_VALUE
                        VALUE = TEST
                        IF NOT VALID_NUM(VALUE) THEN GOTO, GOT_VALUE
                        IF (STRPOS(VALUE,'.') GE 0) OR (STRPOS(VALUE,'E') $
                                GE 0) OR (STRPOS(VALUE,'D') GE 0) THEN BEGIN
                            IF ( STRPOS(VALUE,'D') GT 0 ) OR $
                                    ( STRLEN(VALUE) GE 8 ) THEN BEGIN
                                VALUE = DOUBLE(VALUE)
                                END ELSE VALUE = FLOAT(VALUE)
                        ENDIF ELSE BEGIN
                            LMAX = 2.0D^31 - 1.0D
                            LMIN = -2.0D31
                            VALUE = DOUBLE(VALUE)
                            if (VALUE GE LMIN) and (VALUE LE LMAX) THEN $
                                VALUE = LONG(VALUE)
                        ENDELSE
                            
;
GOT_VALUE:
                        ON_IOERROR, NULL
                    ENDELSE
                ENDELSE         ; if string
;
;  Add to vector if required.
;
                IF VECTOR THEN BEGIN
                    MAXNUM = MAX(NUMBER)
                    IF ( I EQ 0 ) THEN BEGIN
                        SZ_VALUE = SIZE(VALUE)
                        RESULT = MAKE_ARRAY( MAXNUM, TYPE=SZ_VALUE[1])
                        COMMENTS = STRARR(MAXNUM)
                    ENDIF 
                    RESULT[   NUMBER[I]-1 ] =  VALUE
                    COMMENTS[ NUMBER[I]-1 ] =  COMMENT
                ENDIF ELSE BEGIN
                    COMMENTS = COMMENT
                ENDELSE
            ENDFOR
;
;  Set the value of !ERR for the number of matches for vectors, or simply 0
;  otherwise.
;
            IF VECTOR THEN BEGIN
                !ERR = MATCHES
                RETURN, RESULT
            ENDIF ELSE !ERR = 0
;
;  Error point for keyword not found.
;
        ENDIF ELSE BEGIN
            IF ABORT_RETURN THEN MESSAGE,'Keyword '+NAM+' not found in '+ABORT
            !ERR = -1
        ENDELSE
;
        RETURN, VALUE
        END


        FUNCTION FXPARPOS, KEYWRD, IEND, BEFORE=BEFORE, AFTER=AFTER
;+
; Project     : SOHO - CDS
;
; Name        : 
;       FXPARPOS()
; Purpose     : 
;       Finds position to insert record into FITS header.
; Explanation : 
;       Finds the position to insert a record into a FITS header.  Called from
;       FXADDPAR.
; Use         : 
;       Result = FXPARPOS(KEYWRD, IEND  [, BEFORE=BEFORE ]  [, AFTER=AFTER ])
; Inputs      : 
;       KEYWRD  = Array of eight-character keywords in header.
;       IEND    = Position of END keyword.
; Opt. Inputs : 
;       None.
; Outputs     : 
;       Result of function is position to insert record.
; Opt. Outputs: 
;       None.
; Keywords    : 
;       BEFORE  = Keyword string name.  The parameter will be placed before the
;                 location of this keyword.  For example, if BEFORE='HISTORY'
;                 then the parameter will be placed before the first history
;                 location.  This applies only when adding a new keyword;
;                 keywords already in the header are kept in the same position.
;
;       AFTER   = Same as BEFORE, but the parameter will be placed after the
;                 location of this keyword.  This keyword takes precedence over
;                 BEFORE.
;
;       If neither BEFORE or AFTER keywords are passed, then IEND is returned.
;
; Calls       : 
;       None.
; Common      : 
;       None.
; Restrictions: 
;       KEYWRD and IEND must be consistent with the relevant FITS header.
; Side effects: 
;       None.
; Category    : 
;       Data Handling, I/O, FITS, Generic.
; Prev. Hist. : 
;       William Thompson, Jan 1992.
; Written     : 
;       William Thompson, GSFC, January 1992.
; Modified    : 
;       Version 1, William Thompson, GSFC, 12 April 1993.
;               Incorporated into CDS library.
; Version     : 
;       Version 1, 12 April 1993.
;       Converted to IDL V5.0   W. Landsman   September 1997
;-
;
        ON_ERROR,2                              ;Return to caller
;
;  Check the number of parameters.
;
        IF N_PARAMS() NE 2 THEN MESSAGE,        $
                'Required parameters are KEYWRD and IEND'
;
;  If the AFTER keyword has been entered, then find the location.
;
        IF N_ELEMENTS(AFTER) EQ 1 THEN BEGIN
                KEY_AFTER = STRING(REPLICATE(32B,8))
                STRPUT,KEY_AFTER,STRUPCASE(STRTRIM(AFTER,2)),0
                ILOC = WHERE(KEYWRD EQ KEY_AFTER,NLOC)
                IF NLOC GT 0 THEN RETURN, (MAX(ILOC)+1) < IEND
        ENDIF
;
;  If AFTER wasn't entered or found, and if the BEFORE keyword has been
;  entered, then find the location.
;
        IF N_ELEMENTS(BEFORE) EQ 1 THEN BEGIN
                KEY_BEFORE = STRING(REPLICATE(32B,8))
                STRPUT,KEY_BEFORE,STRUPCASE(STRTRIM(BEFORE,2)),0
                ILOC = WHERE(KEYWRD EQ KEY_BEFORE,NLOC)
                IF NLOC GT 0 THEN RETURN,ILOC[0]
        ENDIF
;
;  Otherwise, simply return IEND.
;
        RETURN,IEND
        END

function is_ieee_big
;+
; NAME:
;       IS_IEEE_BIG
; PURPOSE:
;       Determine if the current machine is use IEEE, big-endian numbers.
; EXPLANATION:
;       (Big endian implies that byteorder XDR conversions are no-ops).
; CALLING SEQUENCE:
;       flag = is_ieee_big()
; INPUT PARAMETERS:
;       None
; RETURNS:
;       1 if the machine appears to be IEEE-compliant, 0 if not.
; COMMON BLOCKS:
;       None.
; SIDE EFFECTS:
;       None
; RESTRICTIONS:
; PROCEDURE:
;       A sample int, long, float and double are converted using
;       byteorder and compared with the original.  If there is no
;       change, the machine is assumed to be IEEE compliant and
;       big-endian.
; MODIFICATION HISTORY:
;       Written 15-April-1996 by T. McGlynn for use in MRDFITS.
;       13-jul-1997     jkf/acc - added calls to check_math to avoid
;                                 underflow messages in V5.0 on Win32 (NT).
;       Converted to IDL V5.0   W. Landsman   September 1997
;-

    itest = 512
    ltest = 102580L
    ftest = 1.23e10
    dtest = 1.23d10
                
                
    it2 = itest
    lt2 = ltest
    ft2 = ftest
    dt2 = dtest
                                
    byteorder, it2, /htons
    byteorder, lt2, /htonl
    byteorder, ft2, /ftoxdr
    byteorder, dt2, /dtoxdr

    if itest eq it2  and  ltest eq lt2   and ftest eq ft2  and dtest eq dt2  $
    then begin
        dum = check_math()
        return, 1
    endif else begin
        dum = check_math()
        return, 0
    endelse
    end                                                             



; STANDARD FITS WRITING LIBARY INCLUDE UPDATED MWRUVFITS.PRO
;+
; NAME:
;       MWRUVFITS
; PURPOSE:
;       Write all standard FITS data types from input arrays or structures.
;
; CALLING SEQUENCE:
;       MWRUVFITS, Input, Filename, [Header],
;                       /LSCALE , /ISCALE, /BSCALE, 
;                       /USE_COLNUM, /Silent, /Create, /No_comment, /Version, $
;                       Alias=, /ASCII, Separator=, Terminator=, Null=,
;                       /Logical_cols, /Bit_cols, /Nbit_cols, 
;                       Group=, Pscale=, Pzero=
;
; INPUTS:
;       Input = Array or structure to be written to FITS file.
;
;               -When writing FITS primary data or image extensions
;                input should be an array.
;               --If data is to be grouped
;                 the Group keyword should be specified to point to
;                 a two dimensional array.  The first dimension of the
;                 Group array will be PCOUNT while the second dimension
;                 should be the same as the last dimension of Input.
;               --If Input is undefined, then a dummy primary dataset
;                 or Image extension is created [This might be done, e.g.,
;                 to put appropriate keywords in a dummy primary
;                 HDU].
;
;               -When writing an ASCII table extension, Input should
;                be a structure array where no element of the structure
;                is a structure or array (except see below).
;               --A byte array will be written as A field.  No checking
;                 is done to ensure that the values in the byte field
;                 are valid ASCII.
;               --Complex numbers are written to two columns with '_R' and
;                 '_I' appended to the TTYPE fields (if present).  The
;                 complex number is enclosed in square brackets in the output.
;               --Strings are written to fields with the length adjusted
;                 to accommodate the largest string.  Shorter strings are
;                 blank padded to the right.
;
;               -When writing a binary table extension, the input should
;                be a structure array with no element of the structure
;                being a substructure.
;
;               If a structure is specified on input and the output
;               file does not exist or the /CREATE keyword is specified
;               a dummy primary HDU is created.
;
;       Filename = String containing the name of the file to be written.
;                By default MWRFITS appends a new extension to existing
;                files which are assumed to be valid FITS.  The /CREATE
;                keyword can be used to ensure that a new FITS file
;                is created even if the file already exists.
;
; OUTPUTS:
;
; OPTIONAL INPUTS:
;       Header = Header should be a string array.  Each element of the
;                array is added as a row in the FITS  header.  No
;                parsing is done of this data.  MWRFITS will prepend
;                required structural (and, if specified, scaling)
;                keywords before the rows specified in Header.
;                Rows describing columns in the table will be appended
;                to the contents of Header.
;                Header lines will be extended or truncated to
;                80 characters as necessary.
;                If Header is specified then on return Header will have
;                the header generated for the specified extension.
;
; OPTIONAL INPUT KEYWORDS:
;       ALias=   Set up aliases to convert from the IDL structure
;                to the FITS column name.  The value should be
;                a STRARR(2,*) value where the first element of
;                each pair of values corresponds to a column
;                in the structure and the second is the name
;                to be used in the FITS file.
;                The order of the alias keyword is compatible with
;                use in MRDFITS.
;       ASCII  - Creates an ASCII table rather than a binary table.
;                This keyword may be specified as:
;                /ASCII - Use default formats for columns.
;                ASCII='format_string' allows the user to specify
;                  the format of various data types such using the following
;                  syntax 'column_type:format, column_type:format'.  E.g.,
;                ASCII='A:A1,I:I6,L:I10,B:I4,F:G15.9,D:G23.17,C:G15.9,M:G23.17'
;                gives the default formats used for each type.  The TFORM
;                fields for the real and complex types indicate will use corresponding
;                E and D formats when a G format is specified.
;                Note that the length of the field for ASCII strings and
;                byte arrays is automatically determined for each column.
;       BIT_COLS=   An array of indices of the bit columns.   The data should
;                comprise a byte array with the appropriate dimensions.
;                If the number of bits per row (see NBIT_COLS)
;                is greater than 8, then the first dimension of the array 
;                should match the number of input bytes per row.
;       BSCALE   Scale floats, longs, or shorts to unsigned bytes (see LSCALE)
;       CREATE   If this keyword is non-zero, then a new FITS file will
;                be created regardless of whether the file currently
;                exists.  Otherwise when the file already exists,
;                a FITS extension will be appended to the existing file
;                which is assumed to be a valid FITS file.
;       GROUP=   This keyword indicates that GROUPed FITS data is to
;                be generated.
;                Group should be a 2-D array of the appropriate output type.
;                The first dimension will set the number of group parameters.
;                The second dimension must agree with the last dimension
;                of the Input array.
;       ISCALE   Scale floats or longs to short integer (see LSCALE)
;       LOGICAL_COLS=  An array of indices of the logical column numbers.
;                These should start with the first column having index 0.
;                The structure element should be an array of characters
;                with the values 'T' or 'F'.  This is not checked.
;       LSCALE   Scale floating point numbers to long integers.
;                This keyword may be specified in three ways.
;                /LSCALE (or LSCALE=1) asks for scaling to be automatically
;                determined. LSCALE=value divides the input by value.
;                I.e., BSCALE=value, BZERO=0.  Numbers out of range are 
;                given the value of NULL if specified, otherwise they are given
;                the appropriate extremum value.  LSCALE=(value,value)
;                uses the first value as BSCALE and the second as BZERO
;                (or TSCALE and TZERO for tables).
;       NBIT_COLS=  The number of bits actually used in the bit array.
;                This argument must point to an array of the same dimension
;                as BIT_COLS.
;       NO_TYPES  If the NO_TYPES keyword is specified, then no TTYPE
;                keywords will be created for ASCII and BINARY tables.
;       No_comment Do not write comment keywords in the header
;       NULL=    Value to be written for integers/strings which are
;                undefined or unwritable.
;       PSCALE=  An array giving scaling parameters for the group keywords.
;                It should have the same dimension as the first dimension
;                of Group.
;       PZERO=   An array giving offset parameters for the group keywords.
;                It should have the same dimension as the first dimension
;                of Group.
;       Separator= This keyword can be specified as a string which will
;                be used to separate fields in ASCII tables.  By default
;                fields are separated by a blank.
;       SILENT   Suppress informative messages.  Errors will still
;                be reported.
;       Terminator= This keyword can be specified to provide a string which
;                will be placed at the end of each row of an ASCII table.
;                No terminator is used when not specified.
;                If a non-string terminator is specified (including
;                when the /terminator form is used), a new line terminator
;                is appended.
;       USE_COLNUM  When creating column names for binary and ASCII tables
;                MWRFITS attempts to use structure field name
;                values.  If USE_COLNUM is specified and non-zero then
;                column names will be generated as 'C1, C2, ... 'Cn'
;                for the number of columns in the table.
;       Version   Print the version number of MWRFITS.
;
; EXAMPLE:
;       Write a simple array:
;            a=fltarr(20,20)
;            mwrfits,a,'test.fits'
;
;       Append a 3 column, 2 row, binary table extension to file just created.
;            a={name:'M31', coords:(30., 20.), distance:2}
;            a=replicate(a, 2);
;            mwrfits,a,'test.fits'
;
;       Now add on an image extension:
;            a=lonarr(10,10,10)
;            hdr=("COMMENT  This is a comment line to put in the header", $
;                 "MYKEY    = "Some desired keyword value")
;            mwrfits,a,'test.fits',hdr
;
; RESTRICTIONS:
;       (1)     Variable length columns are not supported for anything
;               other than simple types (byte, int, long, float, double).
;       (2)     String columns with all columns of zero length crash the
;               program
; NOTES:
;       This multiple format FITS writer is designed to provide a
;       single, simple interface to writing all common types of FITS data.
;       Given the number of options within the program and the
;       variety of IDL systems available it is likely that a number
;       of bugs are yet to be uncovered.  If you find an anomaly
;       please send a report to:
;           Tom McGlynn
;           NASA/GSFC Code 660.2
;           tam@silk.gsfc.nasa.gov (or 301-286-7743)
;
; PROCEDURES USED:
;	FXPAR(), FXADDPAR, IS_IEEE_BIG(), HOST_TO_IEEE
; MODIfICATION HISTORY:
;	Version 0.9: By T. McGlynn   1997-07-23
;		Initial beta release.
;	Dec 1, 1997, Lindler, Modified to work under VMS.
;	Version 0.91: T. McGlynn  1998-03-09
;	        Fixed problem in handling null primary arrays.
;       Reconverted to IDL 5.0 format using IDLv4_to_v5
;       Version 0.92: T. McGlynn 1998-09-09
;               Add no_comment flag and keep user comments on fields.
;               Fix handling of bit fields.
;       Version 0.93: T. McGlynn 1999-03-10
;               Fix table appends on VMS.
;       Version 0.93a  W. Landsman/D. Schlegel
;               Update keyword values in chk_and_upd if data type has changed 
;       Version 0.94: T. McGlynn 2000-02-02
;               Efficient processing of ASCII tables.
;               Use G rather than E formats as defaults for ASCII tables
;                and make the default precision long enough that transformations
;                binary to/from ASCII are invertible.
;               Some loop indices made long.
;               Fixed some ends to match block beginnings.
;       Version 0.95: T. McGlynn 2000-11-06
;               Several fixes to scaling.  Thanks to David Sahnow for
;               documenting the problems.
;               Added PCOUNT,GCOUNT keywords to Image extensions.
;               Version numbers shown in SIMPLE/XTENSION comments
;       Version 0.96: T. McGlynn 2001-04-06
;               Changed how files are opened to handle ~ consistently
;       Version 1.0: T. McGlynn 2001-12-04
;               Unsigned integers,
;               64 bit integers.
;               Aliases
;               Variable length arrays
;               Some code cleanup
;       Version 1.001: Qi 2002-02-12
;               add features for uvfits file. change name to mwruvfits.pro       
;-

; What is the current version of this program.
function mwr_version
    return, '1.001'
end
    

; Add a keyword as non-destructively as possible to a FITS header
pro chk_and_upd, header, key, value, comment


    xcomm = ""
    if n_elements(comment) gt 0 then xcomm = comment
    if n_elements(header) eq 0 then begin
      
        fxaddpar, header, key, value, xcomm
	
    endif else begin
	
        oldvalue = fxpar(header, key, count=count, comment=oldcomment)
   
        if (count eq 1) then begin

	    qchange = 0 ; Set to 1 if either the type of variable or its
	                ; value changes.
            size1 = size(oldvalue) & size2 = size(value)
            if size1[size1[0]+1] NE size2[size2[0]+1] then qchange = 1 $
	     else if (oldvalue ne value) then qchange = 1

	     if (qchange) then begin

	        if n_elements(oldcomment) gt 0 then xcomm = oldcomment[0]
	        fxaddpar, header, key, value, xcomm
		
	    endif
	    
	endif else begin
	    
            fxaddpar, header, key, value, xcomm
        endelse
	
    endelse
end

; Get the column name appropriate for a given tag
function mwr_checktype, tag, alias=alias

    if not keyword_set(alias) then return, tag

    sz = size(alias)
    ; 1 or 2 D string array with first dimension of 2
    if (sz[0] eq 1 or sz[1] eq 2) and sz[1] eq 2 and sz[sz[0]+1] eq 7 then begin
	w = where(tag eq alias[0,*])
	if (w[0] eq -1) then begin
	    return, tag
	endif else begin
	    return, alias[1,w[0]]
	endelse
    endif else begin
	print,'MWRFITS: Warning: Alias values not strarr(2) or strarr(2,*)'
    endelse
    return, tag
end

; Create an ASCII table
pro mwr_ascii, input, siz, lun, bof, header,     $
        ascii=ascii,                             $
	null=null,                               $
	use_colnum = use_colnum,                 $
	lscale=lscale, iscale=iscale,		 $
	bscale=bscale,                           $
	no_types=no_types,			 $
	separator=separator,                     $
	terminator=terminator,                   $
        no_comment=no_comment,                   $
	silent=silent,                           $
	alias=alias
	
    ; Write the header and data for a FITS ASCII table extension.
  
    types=  ['A',   'I',   'L',   'B',   'F',    'D',      'C',     'M',     'K']
    formats=['A1',  'I6',  'I10', 'I4',  'G15.9','G23.17', 'G15.9', 'G23.17','I20']
    lengths=[1,     6,     10,     4,    15,     23,       15,      23,      20]

    ; Check if the user is overriding any default formats.
    sz = size(ascii)

    if sz[0] eq 0 and sz[1] eq 7 then begin
        ascii = strupcase(strcompress(ascii,/remo))
        for i=0, n_elements(types)-1  do begin
            p = strpos(ascii,types[i]+':')
            if p ge 0 then begin

	        q = strpos(ascii, ',', p+1)
	        if q lt p then q = strlen(ascii)+1
	        formats[i] = strmid(ascii, p+2, (q-p)-2)
	        len = 0
	    
	        reads, formats[i], len, format='(1X,I)'
	        lengths[i] = len
            endif
        endfor
    endif

    i0      = input[0]
    ntag    = n_tags(i0)
    tags    = tag_names(i0)
    ctypes  = lonarr(ntag)
    strmaxs = lonarr(ntag)

    if not keyword_set(separator) then separator=' '

    slen = strlen(separator)

    offsets = 0
    tforms = ''
    ttypes = ''
    offset = 0

    totalFormat = ""
    xsep = "";

    for i=0, ntag-1 do begin

        totalFormat = totalFormat + xsep;
    
        sz = size(i0.(i))
        if sz[0] ne 0 and (sz[sz[0]+1] ne 1) then begin
            print, 'MWRFITS Error: ASCII table cannot contain arrays'
	    return
        endif

        ctypes[i] = sz[1]

        xtype = mwr_checktype(tags[i], alias=alias)
    
        ttypes = [ttypes, xtype+' ']

        if sz[0] gt 0 then begin
            ; Byte array to be handled as a string.
	    nelem = sz[sz[0]+2]
	    ctypes[i] = sz[sz[0]+1]
            tf = 'A'+strcompress(string(nelem))
            tforms = [tforms, tf]
	    offsets = [offsets, offset]
            totalFormat = totalFormat + tf
	    offset = offset + nelem
	
        endif else if sz[1] eq 7 then begin
            ; Use longest string to get appropriate size.
	    strmax = max(strlen(input.(i)))
	    strmaxs[i] = strmax
	    tf = 'A'+strcompress(string(strmax), /remo)
	    tforms = [tforms, tf]
	    offsets = [offsets, offset]
            totalFormat = totalFormat + tf
	    ctypes[i] = 7
	    offset = offset + strmax
	
        endif else if sz[1] eq 6  or sz[1] eq 9 then begin
            ; Complexes handled as two floats.
	    offset = offset + 1
	
	    if sz[1] eq 6 then indx = where(types eq 'C')
	    if sz[1] eq 9 then indx = where(types eq 'M')
	    indx = indx[0]
	    fx = formats[indx]
	    if (strmid(fx, 0, 1) eq "G"  or strmid(fx, 0, 1) eq "g") then begin
	        if (sz[1] eq 6) then begin
	            fx = "E"+strmid(fx,1, 99)
	        endif else begin
		    fx = "D"+strmid(fx,1, 99)
	        endelse
	    endif
	    tforms = [tforms, fx, fx]
            offsets = [offsets, offset, offset+lengths[indx]+1]
	    nel = n_elements(ttypes)
	    ttypes = [ttypes[0:nel-2], xtype+'_R', xtype+'_I']
	    offset = offset + 2*lengths[indx] + 1

            totalFormat = totalFormat + '"[",'+formats[indx]+',1x,'+formats[indx]+',"]"'
            offset = offset+1
	
        endif else begin
	  
            if sz[1] eq 1 then indx = where(types eq 'B')                      $
	    else if sz[1] eq 2 or sz[1] eq 12 then indx = where(types eq 'I')  $
	    else if sz[1] eq 3 or sz[1] eq 13 then indx = where(types eq 'L')  $
	    else if sz[1] eq 4 then indx = where(types eq 'F')                 $
	    else if sz[1] eq 5 then indx = where(types eq 'D')                 $
	    else if sz[1] eq 14 or sz[1] eq 15 then indx = where(types eq 'K') $
	    else begin
	        print, 'MWRFITS Error: Invalid type in ASCII table'
	        return
	    endelse
	
	    indx = indx[0]
	    fx = formats[indx]
	    if (strmid(fx, 0, 1) eq 'G' or strmid(fx, 0, 1) eq 'g') then begin
	        if sz[1] eq 4 then begin
	            fx = 'E'+strmid(fx, 1, 99)
	        endif else begin
	            fx = 'D'+strmid(fx, 1, 99)
	        endelse
	    endif
	
	    tforms = [tforms, fx]
	    offsets = [offsets, offset]
            totalFormat = totalFormat + formats[indx]
	    offset = offset + lengths[indx]
        endelse
        if i ne ntag-1 then begin
            offset = offset + slen
        endif

        xsep = ", '"+separator+"', "
    
    endfor

    if  keyword_set(terminator) then begin
        sz = size(terminator);
        if sz[0] ne 0 or sz[1] ne 7 then begin
            terminator= string(10B)
        endif
    endif


    if keyword_set(terminator) then offset = offset+strlen(terminator)
    ; Write required FITS keywords.

    chk_and_upd, header, 'XTENSION', 'TABLE', 'ASCII table extension written by MWRFITS '+mwr_version()
    chk_and_upd, header, 'BITPIX', 8,'Required Value: ASCII characters'
    chk_and_upd, header, 'NAXIS', 2,'Required Value'
    chk_and_upd, header, 'NAXIS1', offset, 'Number of characters in a row'
    chk_and_upd, header, 'NAXIS2', n_elements(input), 'Number of rows'
    chk_and_upd, header, 'PCOUNT', 0, 'Required value'
    chk_and_upd, header, 'GCOUNT', 1, 'Required value'
    chk_and_upd, header, 'TFIELDS', n_elements(ttypes)-1, 'Number of fields'

    ; Recall that the TTYPES, TFORMS, and OFFSETS arrays have an
    ; initial dummy element.

    ; Write the TTYPE keywords.
    if not keyword_set(no_types) then begin
        for i=1, n_elements(ttypes)-1 do begin
            key = 'TTYPE'+ strcompress(string(i),/remo)
            if keyword_set(use_colnum) then begin
	        value = 'C'+strcompress(string(i),/remo)
	    endif else begin
	        value = ttypes[i]+' '
	    endelse
	    chk_and_upd, header, key, value
        endfor

        if (not keyword_set(no_comment)) then begin
            fxaddpar, header, 'COMMENT', ' ', before='TTYPE1'
            fxaddpar, header, 'COMMENT', ' *** Column names ***', before='TTYPE1'
            fxaddpar, header, 'COMMENT', ' ', before='TTYPE1'
        endif
    
    endif

    ; Write the TBCOL keywords.

    for i=1, n_elements(ttypes)-1 do begin
        key= 'TBCOL'+strcompress(string(i),/remo)
        chk_and_upd, header, key, offsets[i]+1
    endfor

    if (not keyword_set(no_comment)) then begin
        fxaddpar, header, 'COMMENT', ' ', before='TBCOL1'
        fxaddpar, header, 'COMMENT', ' *** Column offsets ***', before='TBCOL1'
        fxaddpar, header, 'COMMENT', ' ', before='TBCOL1'
    endif

    ; Write the TFORM keywords

    for i=1, n_elements(ttypes)-1 do begin
        key= 'TFORM'+strcompress(string(i),/remo)
        chk_and_upd, header, key, tforms[i]
    endfor

    if (not keyword_set(no_comment)) then begin
        fxaddpar, header, 'COMMENT', ' ', before='TFORM1'
        fxaddpar, header, 'COMMENT', ' *** Column formats ***', before='TFORM1'
        fxaddpar, header, 'COMMENT', ' ', before='TFORM1'
    endif

    ; Write the header.

    mwr_header, lun, header

    ; Now loop over the structure and write out the data.

    totalFormat = "("+totalFormat+")";
    
    start = 0L
    last  = 1023L
    while (start lt n_elements(input)) do begin
        if (last ge n_elements(input)) then begin
            last = n_elements(input) - 1
        endif

        strings = string(input[start:last], format=totalFormat)
        if keyword_set(terminator) then begin
            strings = strings+terminator
        endif
        writeu, lun, strings
        start = last + 1
        last  = last + 1024
    endwhile

    ; Check to see if any padding is required.

    nbytes = n_elements(input)*offset
    padding = 2880 - nbytes mod 2880

    if padding ne 0 then begin
        pad = replicate(32b, padding)
    endif
    writeu, lun, pad

   return
end

; Write a dummy primary header-data unit.
pro mwr_dummy, lun

    fxaddpar, header, 'SIMPLE', 'T','Dummy Created by MWRFITS v'+mwr_version()
    fxaddpar, header, 'BITPIX', 8, 'Dummy primary header created by MWRFITS'
    fxaddpar, header, 'NAXIS', 0, 'No data is associated with this header'
    fxaddpar, header, 'EXTEND', 'T', 'Extensions may (will!) be present'

    mwr_header, lun, header
end

; Check if this is a valid pointer array for variable length data.
function mwr_validptr, vtypes, nfld, index, array
    
    type = -1
    offset = 0L
    for i=0, n_elements(array)-1 do begin
	if ptr_valid(array[i]) then begin
	    
	    sz = size(*array[i])
	    if sz[0] gt 1 then begin
		print,'MWRFITS: Error: Multidimensional Pointer array'
		return, 0
	    endif
	    if type eq -1 then begin
		type = sz[sz[0] + 1]
	    endif else begin
		if sz[sz[0] + 1] ne type then begin
		    print,'MWRFITS: Error: Inconsistent type in pointer array'
		    return, 0
		endif
	    endelse
	    xsz = sz[1]
	    if sz[0] eq 0 then xsz = 1
	    offset = offset + xsz
	endif
    endfor
    if type eq -1 then begin
        ; If there is no data assume an I*2 type
	type = 2
    endif

    if  (type lt 1 or type gt 5) and (type lt 12 or type gt 15) then begin
	print,'MWRFITS: Error: Unsupported type for variable length array'
    endif

    types = 'BIJED      IJKK'
    sizes = [1,2,4,4,8,0,0,0,0,0,0,2,4,8,8]
    
    if n_elements(vtypes) eq 0 then begin
	
        vtype = {status:0, data:array,           $
	     type: strmid(types, type-1, 1),            $
	     itype: type, ilen: sizes[type-1],          $
	     offset:offset }
    
	vtypes = replicate(vtype, nfld)
	
    endif else begin
	; This ensures compatible structures without
	; having to used named structures.
	
	vtype = vtypes[0]
	vtype.status = 0
	vtype.data   = array
	vtype.type   = strmid(types, type-1, 1)
	vtype.itype  = type
	vtype.ilen   = sizes[type-1]
	vtype.offset = offset
	vtypes[index] = vtype
	
	
    endelse
    vtypes[index].status = 1;

    return, 1
end
	
; Handle the header for a binary table.
pro mwr_tablehdr, lun, input, header, vtypes,     $
		no_types=no_types,                $
		logical_cols = logical_cols,	  $
		bit_cols = bit_cols,		  $
		nbit_cols= nbit_cols,             $
                no_comment=no_comment,            $
		alias=alias,                      $
		silent=silent, extname=extname, unit=unit

    if not keyword_set(no_types) then no_types = 0

    nfld = n_tags(input[0])
    if nfld le 0 then begin
	print, 'MWRFITS Error: Input contains no structure fields.'
	return
    endif

    tags = tag_names(input)

    ; Get the number of rows in the table.

    nrow = n_elements(input)

    dims    = lonarr(nfld)
    tdims   = strarr(nfld)
    types   = strarr(nfld)
    pointers= lonarr(nfld)

    ; offsets = null...  Don't want to define this
    ; in advance since reference to ulon64 won't word with IDL < 5.2
    ;
    ; Get the type and length of each column.  We do this
    ; by examining the contents of the first row of the structure.
    ;

    nbyte = 0

    for i=0, nfld-1 do begin

	a = input[0].(i)

	sz = size(a)
	
	nelem    = sz[sz[0]+2]
	type_ele = sz[sz[0]+1]
	
	if type_ele eq 7 then begin
	    maxstr = max(strlen(input.(i)))
	endif 		
	
	dims[i] = nelem
	
        if (sz[0] lt 1) or (sz[0] eq 1 and type_ele ne 7) then begin
	    tdims[i] = ''
	endif else begin
	    tdims[i] = '('
	    
	    if type_ele eq 7 then begin
	        tdims[i] = tdims[i] + strcompress(string(maxstr), /remo) + ','
	    endif
	    
	    for j=1, sz[0] do begin
	        tdims[i] = tdims[i] + strcompress(sz[j])
	        if j ne sz[0] then tdims[i] = tdims[i] + ','
	    endfor
	    
	    tdims[i] = tdims[i] + ')'
	endelse
	
	
	case type_ele of
	   1: 	begin
			types[i] = 'B'
			nbyte = nbyte + nelem
		end
	   2:	begin
	    		types[i] = 'I'
			nbyte = nbyte + 2*nelem
		end
	   3:	begin
			types[i] = 'J'
			nbyte = nbyte + 4*nelem
		end
	   4:	begin
	   		types[i] = 'E'
			nbyte = nbyte + 4*nelem
	        end
	   5:	begin
			types[i] = 'D'
			nbyte = nbyte + 8*nelem
		end
	   6:	begin
	   		types[i] = 'C'
			nbyte = nbyte + 8*nelem
		end
	   7:	begin
			types[i] = 'A'
			nbyte = nbyte + maxstr*nelem
			dims[i] = maxstr*nelem
		end
	   9:   begin
	                types[i] = 'M'
			nbyte = nbyte + 16*nelem
		end

	  10:   begin
	                if not mwr_validptr(vtypes, nfld, i, input.(i)) then begin
			    return
			endif
			
	                types[i] = 'P'+vtypes[i].type
			nbyte = nbyte + 8
			dims[i] = 1
			
			if vtypes[i].itype eq 12 then begin
			    if (n_elements(offsets) lt 1) then begin
			        offsets = ulon64arr(nfld)
			    endif
			    offsets[i] = 32768L
			endif else if vtypes[i].itype eq 13 then begin
			    if (n_elements(offsets) lt 1) then begin
			        offsets = ulon64arr(nfld)
			    endif
			    offsets[i] = ulong('2147483648')
			endif else if vtypes[i].itype eq 15 then begin
			    if (n_elements(offsets) lt 1) then begin
			        offsets = ulon64arr(nfld)
			    endif
			    offsets[i] = ulong64('9223372036854775808')
			endif
			
	        end

	  12:   begin
		        types[i] = 'I'
			if (n_elements(offsets) lt 1) then begin
			    offsets = ulon64arr(nfld)
			endif
			offsets[i] = 32768L
			nbyte = nbyte + 2*nelem
		end

	  13:   begin
		        types[i] = 'J'
			if (n_elements(offsets) lt 1) then begin
			    offsets = ulon64arr(nfld)
			endif
			offsets[i] = ulong('2147483648')
			nbyte = nbyte + 4*nelem
		end
		
                ; 8 byte integers are not standard fits
	  14:   begin
	                if not keyword_set(silent) then begin
		            print, "MWRFITS: Warning: 8 byte integers are non-standard (column "+strtrim(i+1,2)+')'
			endif
		        types[i] = 'K'
			nbyte = nbyte + 8*nelem
	        end

	  15:   begin
	                if not keyword_set(silent) then begin
		            print, "MWRFITS: Warning: 8 byte integers are non-standard (column "+strtrim(i+1,2)+')'
			endif
		        types[i] = 'K'
			nbyte = nbyte + 8*nelem
			if (n_elements(offsets) lt 1) then begin
			    offsets = ulon64arr(nfld)
			endif
			offsets[i] = ulong64('9223372036854775808')
	        end
		    
	   0:   begin
	   		print,'MWRFITS Error: Undefined structure element??'
			return
		end
		
	   8:   begin
	   		print, 'MWRFITS Error: Nested structures'
			return
		end
		
	   else:begin
	   		print, 'MWRFITS Error: Cannot parse structure'
			return
		end
	endcase
    endfor

    ; Put in the required FITS keywords.
    chk_and_upd, header, 'XTENSION', 'BINTABLE', 'Binary table written by MWRFITS v'+mwr_version()
    chk_and_upd, header, 'BITPIX', 8, 'Required value'
    chk_and_upd, header, 'NAXIS', 2, 'Required value'
    chk_and_upd, header, 'NAXIS1', nbyte, 'Number of bytes per row'
    chk_and_upd, header, 'NAXIS2', n_elements(input), 'Number of rows'
    chk_and_upd, header, 'PCOUNT', 0, 'Normally 0 (no varying arrays)'
    chk_and_upd, header, 'GCOUNT', 1, 'Required value'
    chk_and_upd, header, 'TFIELDS', nfld, 'Number of columns in table'
    chk_and_upd, header, 'EXTNAME', extname, 'AIPS table file'
    chk_and_upd, header, 'EXTVER',1,'Version number of table'
    ;
    ; Handle the special cases.
    ;
    if keyword_set(logical_cols) then begin
	nl = n_elements(logical_cols)
	for i = 0, nl-1 do begin
	    icol = logical_cols[i]
	    if types[icol-1] ne 'A'  then begin
		print,'WARNING: Invalid attempt to create Logical column:',icol
	      	goto, next_logical
	    endif
	    types[icol-1] = 'L'
  next_logical:
	endfor
    endif
	
    if keyword_set(bit_cols) then begin
	nb = n_elements(bit_cols)
	if nb ne n_elements(nbit_cols) then begin
	    print,'WARNING: Bit_cols and Nbit_cols not same size'
	    print,'         No bit columns generated.'
	   goto, after_bits
	endif
	for i = 0, nb-1 do begin
	    nbyte = (nbit_cols[i]+7)/8
	    icol = bit_cols[i]
	    if types[icol-1] ne 'B'  or (dims[icol-1] ne nbyte) then begin
		print,'WARNING: Invalid attempt to create bit column:',icol
	      	goto, next_bit
	    endif
	    types[icol-1] = 'X'
	    tdims[icol-1] = ''
	    dims[icol-1] = nbit_cols[i]
  next_bit:
	endfor
  after_bits:
    endif



    ; Write scaling info as needed.
    if n_elements(offsets) gt 0 then begin
        w = where(offsets gt 0)

        for i=0, n_elements(w) - 1 do begin
            key = 'TSCAL'+strcompress(string(w[i])+1,/remo)
	    chk_and_upd, header, key, 1
        endfor
    
        for i=0, n_elements(w) - 1 do begin
	    key = 'TZERO'+strcompress(string(w[i]+1),/remo)
	    chk_and_upd, header, key, offsets[w[i]]
        endfor
    
        if not keyword_set(no_comment) then begin
            key = 'TSCAL'+strcompress(string(w[0])+1,/remo)
	    fxaddpar, header, 'COMMENT', ' ', before=key
	    fxaddpar, header, 'COMMENT', ' *** Unsigned integer column scalings ***', before=key
	    fxaddpar, header, 'COMMENT', ' ', before=key
        endif
    endif

    ; Now add in the TFORM keywords
    for i=0, nfld-1 do begin
	if dims[i] eq 1 then begin
	    form = types[i]
	endif else begin
	    form=strcompress(string(dims[i]),/remove) + types[i]
        endelse
	
	tfld = 'TFORM'+strcompress(string(i+1),/remove)
	
	; Check to see if there is an existing value for this keyword.
	; If it has the proper value we will not modify it.
	; This can matter if there is optional information coded
	; beyond required TFORM information.
		
	oval = fxpar(header, tfld)
	oval = strcompress(string(oval),/remove_all)
	if (oval eq '0')  or  (strmid(oval, 0, strlen(form)) ne form) then begin
	    chk_and_upd, header, tfld, form
	endif
    endfor

    if (not keyword_set(no_comment)) then begin
        fxaddpar, header, 'COMMENT', ' ', before='TFORM1'
        fxaddpar, header, 'COMMENT', ' *** Column formats ***', before='TFORM1'
        fxaddpar, header, 'COMMENT', ' ', before='TFORM1'
    endif

    ; Now write TDIM info as needed.
    for i=nfld-1, 0,-1 do begin
        if tdims[i] ne '' then begin
            fxaddpar, header, 'TDIM'+strcompress(string(i+1),/remo), tdims[i],after=tfld
        endif
    endfor

    w=where(tdims ne '')
    if w[0] ne -1 and not keyword_set(no_comment) then begin
        fxaddpar, header, 'COMMENT', ' ', after=tfld
        fxaddpar, header, 'COMMENT', ' *** Column dimensions (2 D or greater) ***', after=tfld
        fxaddpar, header, 'COMMENT', ' ', after=tfld
    endif

    for i=0, nfld-1 do begin
        if tdims[i] ne '' then begin
            chk_and_upd, header, 'TDIM'+strcompress(string(i+1),/remo), tdims[i]
        endif
    endfor

    if n_elements(vtypes) gt 0 then begin
        fxaddpar, header, 'THEAP', nbyte*n_elements(input), 'Offset of start of heap'
        offset = 0L
        for i=0,n_elements(vtypes)-1 do begin
	    if vtypes[i].status then offset = offset + vtypes[i].offset*vtypes[i].ilen
        endfor
        fxaddpar, header, 'PCOUNT', offset, 'Size of heap'
    endif

    ;
    ; Last add in the TTYPE keywords if desired.
    ;
    if not no_types then begin
	for i=0, nfld - 1 do begin
	    key = 'TTYPE'+strcompress(string(i+1),/remove)
	    if not keyword_set(use_colnums) then begin
	        value= mwr_checktype(tags[i],alias=alias)+' '
	    endif else begin
	        value = 'C'+strmid(key,5,2)
	    endelse
	    chk_and_upd, header, key, value
	endfor
	
        if (not keyword_set(no_comment)) then begin
	    fxaddpar, header, 'COMMENT', ' ', before='TTYPE1'
	    fxaddpar, header, 'COMMENT', ' *** Column names *** ',before='TTYPE1'
	    fxaddpar, header, 'COMMENT', ' ',before='TTYPE1'
	endif
    endif

    if (not keyword_set(no_comment)) then begin
        fxaddpar, header, 'COMMENT', ' ', after='TFIELDS'
        fxaddpar, header, 'COMMENT', ' *** End of mandatory fields ***', after='TFIELDS'
        fxaddpar, header, 'COMMENT', ' ', after='TFIELDS'
    endif

    for i=0, nfld - 1 do begin
        key = 'TUNIT'+strcompress(string(i+1),/remove)
        chk_and_upd, header, key, unit(i)
    endfor

    if extname eq 'AIPS AN' then begin
       chk_and_upd, header, 'ARRAYX',-5.46242840757d6
       chk_and_upd, header, 'ARRAYY',-2.49196045618d6
       chk_and_upd, header, 'ARRAYZ', 2.28652665343d6
       chk_and_upd, header, 'GSTIAO', 3.5402577500155d2
       chk_and_upd, header, 'DEGPDY', 3.6098564497557d2
       chk_and_upd, header, 'FREQ', 2.30538d11
       chk_and_upd, header, 'RDATE', '2002-02-12'
       chk_and_upd, header, 'POLARX', 0.d0
       chk_and_upd, header, 'POLARY', 0.d0
       chk_and_upd, header, 'UT1UTC', 0.d0
       chk_and_upd, header, 'DATUTC', 0.d0
       chk_and_upd, header, 'TIMSYS', 'IAT     '
       chk_and_upd, header, 'ARRNAM', 'SMA     '
       chk_and_upd, header, 'NUMORB', 1
       chk_and_upd, header, 'NOPCAL', 2
       chk_and_upd, header, 'FREQID', -1
       chk_and_upd, header, 'IATUTC', 0.d0
    endif

    ; Write to the output device.
    mwr_header, lun, header

end

; Modify the structure to put the pointer column in.
function mwr_retable, input, vtypes

    offset = 0L
    str = "output=replicate({";
    comma =""
    tags = tag_names(input);
    for i=0, n_elements(tags) -1 do begin
	if vtypes[i].status then begin
	    str = str + comma +tags[i] + ":lonarr(2)"
	endif else begin
	    str = str + comma + tags[i]+ ":input[0].("+strtrim(i,2)+")"
	endelse
	comma= ","
    endfor
    str = str + "},"+strtrim(n_elements(input),2)+")"
    stat = execute(str)
    if stat eq 0 then begin
        print,'MWRFITS: Error: Unable to create temporary structure for heap'
	return, 0
    endif

    for i=0, n_elements(tags)-1 do begin
	if vtypes[i].status then begin
	    for j=0, n_elements(input)-1 do begin
		ptr = input[j].(i)
		if ptr_valid(ptr) then begin
		    sz = size(*ptr)
		    if sz[0] eq 0 then xsz = 1 else xsz= sz[1]

		    output[j].(i)[0] = xsz
		    output[j].(i)[1] = offset
		    
		    offset = offset + vtypes[i].ilen*xsz
		endif
	    endfor
	endif
    endfor
    return,output
end

; Write the heap data.
function mwr_writeheap, lun, vtypes

    offset = 0L
    flip = not is_ieee_big()
    
    for i=0, n_elements(vtypes)-1 do begin
	if vtypes[i].status then begin
	    itype = vtypes[i].itype
	    unsigned = 0
	    if (itype eq 12 or itype eq 13 or itype eq 15) then unsigned=1
	    ptrs = vtypes[i].data
	    for j=0,n_elements(ptrs)-1 do begin
		if ptr_valid(ptrs[j]) then begin
		    if (unsigned) then begin
			*ptrs[j] = not *ptrs[j]
		    endif
		    if flip then begin
			x = *ptrs[j]
                        host_to_ieee, x
			writeu,lun,x
		    endif else begin
		        writeu, lun, *ptrs[j]
		    endelse
		    
		    sz = size(*ptrs[j])
		    xsz = 1 > sz[0]
		    offset = offset + xsz * vtypes[i].ilen
		endif
	    endfor
	endif
    endfor
    return, offset
    
end
	

; Write the brinary table.
pro mwr_tabledat, lun, input, header, vtypes
    ;
    ; file		-- unit to which data is to be written.
    ; Input		-- IDL structure
    ; Header	-- Filled header

    nfld = n_tags(input)

    ; Any special processing?

    for i=0, nfld-1 do begin
        
        sz = size(input.(i))
	nsz = n_elements(sz)
	typ = sz[nsz-2]
	if (typ eq 7) then begin

            siz = max(strlen(input.(i)))

	    if siz gt 0 then begin
	        blanks = string(bytarr(siz) + 32b)
	        input.(i) = strmid(input.(i)+blanks, 0, siz)
	    endif

	endif
	
	if (typ eq 12 or typ eq 13 or typ eq 15) then begin
	    input.(i) = not(input.(i))
	endif
    endfor

    if n_elements(vtypes) gt 0 then begin
        input = mwr_retable(input, vtypes)
    endif

    ; Use Astron library routine to convert to IEEE (since byteorder
    ; may be buggy).
    if not is_ieee_big() then host_to_ieee, input

    ; Write the data segment.
    ;
    writeu, lun, input

    nbyte = long(fxpar(header, 'NAXIS1'))
    nrow  = n_elements(input)

    heap = 0
    if n_elements(vtypes) gt 0 then begin
        heap = mwr_writeheap(lun, vtypes)
    endif

    siz   = nbyte*nrow + heap

    padding = 2880 - (siz mod 2880)
    if padding eq 2880 then padding = 0

    ;
    ; If necessary write the padding.
    ;
    if padding gt 0 then begin
        pad = bytarr(padding)  ; Should be null-filled by default.
        writeu, lun, pad
    endif

end


; Scale parameters for GROUPed data.
pro mwr_pscale, grp, header, pscale=pscale, pzero=pzero

; This function assumes group is a 2-d array.

    if not keyword_set(pscale) and not keyword_set(pzero) then return

    if not keyword_set(pscale) then begin
        pscale = dblarr(sizg[1])
        pscale[*] = 1.
    endif

    w = where(pzero eq 0.d0)

    if w[0] ne 0 then begin
        print, 'MWRFITS  Warning: PSCALE value of 0 found, set to 1.'
        pscale[w] = 1.d0
    endif

    if keyword_set(pscale) then begin
        for i=0L, sizg[1]-1 do begin
            key= 'PSCAL' + strcompress(string(i+1),/remo)
            chk_and_upd, header, key, pscale[i]
        endfor
    endif

    if not keyword_set(pzero) then begin
        pzero = dblarr(sizg[1])
        pzero[*] = 0.
    endif else begin
        for i=0L, sizg[1]-1 do begin
            key= 'PZERO' + strcompress(string(i+1),/remo)
            chk_and_upd, header, key, pscale[i]
        endfor
    endelse

    for i=0L, sizg[1]-1 do begin
        grp[i,*] = grp[i,*]/pscale[i] - pzero[i]
    endfor

end


; Find the appropriate scaling parameters.
pro mwr_findscale, flag, array, nbits, scale, offset, error


    error = 0
    if n_elements(flag) eq 2 then begin
         scale  = double(flag[0])
	 offset = double(flag[1])
    endif else if n_elements(flag) eq 1 and flag[0] ne 1 then begin
         minmum = min(array, max=maxmum)
	 offset = 0.d0
	 scale  = double(flag[0])
    endif else if n_elements(flag) ne 1 then begin
         print, 'MWRFITS Error: Invalid scaling parameters.'
	 error  = 1
	 return
    endif else begin
	 
         minmum = min(array, max=maxmum)
	 scale  = (maxmum-minmum)/(2.d0^nbits)
	 amin   = -(2.d0^(nbits-1))
	 if (amin gt -130) then amin = 0  ; looking for -128
	 offset = minmum - scale*amin
	 
    endelse
    return
end

; Scale and possibly convert array according to information
; in flags.
pro mwr_scale, array, scale, offset, lscale=lscale, iscale=iscale,  $
   bscale=bscale, null=null


    ; First deallocate scale and offset
    if n_elements(scale)  gt 0 then xx = temporary(scale)
    if n_elements(offset) gt 0 then xx = temporary(offset)

    if not keyword_set(lscale) and not keyword_set(iscale) and  $
       not keyword_set(bscale) then return

    siz = size(array)
    if keyword_set(lscale) then begin

        ; Doesn't make sense to scale data that can be stored exactly.
        if siz[siz[0]+1] lt 4 then return
        amin = -2.d0^31
        amax = -(amin + 1)
    
        mwr_findscale, lscale, array, 32, scale, offset, error

    endif else if keyword_set(iscale) then begin
        if siz[siz[0]+1] lt 3 then return
        amin = -2.d0^15
        amax = -(amin + 1)
    
        mwr_findscale, iscale, array, 16, scale, offset, error

    endif else begin
        if siz[siz[0]+1] lt 2 then return
    
        amin = 0
        amax = 255
    
        mwr_findscale, bscale, array, 8, scale, offset, error
    endelse

    ; Check that there was no error in mwr_findscale
    if error gt 0 then return

    if scale le 0.d0 then begin
        print, 'MWRFITS Error: BSCALE/TSCAL=0'
        return
    endif

    array = round((array-offset)/scale)

    w=where(array lt 0)
    w = where(array gt amax)
    if w[0] ne -1 then begin
        if keyword_set(null) then array[w] = null else array[w]=amax
    endif

    w = where(array lt amin)
    if w[0] ne -1 then begin
        if keyword_set(null) then array[w] = null else array[w] = amin
    endif

    if keyword_set(lscale) then      array = long(array) $
    else if keyword_set(iscale) then array = fix(array)  $
    else                             array = byte(array)
    
end

; Write a header
pro mwr_header, lun, header

    ; Fill strings to at least 80 characters and then truncate.

    space = string(replicate(32b, 80))
    header = strmid(header+space, 0, 80)

    w = where(strmid(header,0,8) eq "END     ")

    if w[0] eq -1 then begin

	header = [header, strmid("END"+space,0,80)]
	
    endif else begin
        if (n_elements(w) gt 1) then begin 
	    ; Get rid of extra end keywords;
	    print,"MWRFITS Warning: multiple END keywords found."
	    for irec=0L, n_elements(w)-2 do begin
		header[w[irec]] = strmid('COMMENT INVALID END REPLACED'+  $
		  space, 0, 80)
	    endfor
	endif

	; Truncate header array at END keyword.
	header = header[0:w[n_elements(w)-1]]
    endelse

    nrec = n_elements(header)
    if nrec mod 36 ne 0 then header = [header, replicate(space,36 - nrec mod 36)]

    writeu, lun, byte(header)
end


; Move the group information within the data.
pro mwr_groupinfix, data, group, hdr

    siz = size(data)
    sizg = size(group)

    ; Check if group info is same type as data 

    if siz[siz[0]+1] ne sizg[3] then begin
        case siz[siz[0]+1] of
         1: begin
	        mwr_groupscale, 127.d0, group, hdr
	        group = byte(group)
	    end
         2: begin
	        mwr_groupscale, 32767.d0, group, hdr
	        group = fix(group)
	    end
         3: begin
	        mwr_groupscale, 2147483647.d0, group, hdr
	        group = long(group)
	    end
         4: group = float(group)
         5: group = double(group)
      else: begin
                print,'MWRFITS Internal error: Conversion of group data'
	        return
            end
        endcase
    endif

    nrow = 1
    for i=1, siz[0]-1 do begin
        nrow = nrow*siz[i]
    endfor

    data = reform(data, siz[siz[0]+2])
    for i=0L, siz[siz[0]] - 1 do begin
        if i eq 0 then begin
            gdata = group[*,0]
	    gdata = reform(gdata)
            tdata = [ gdata , data[0:nrow-1]]
        endif else begin
            start = nrow*i
	    fin = start+nrow-1
	    gdata = group[*,i]
            tdata = [tdata, gdata ,data[start:fin]]
       endelse
    endfor

    data = temporary(tdata)
end

; If an array is being scaled to integer type, then
; check to see if the group parameters will exceed the maximum
; values allowed.  If so scale them and update the header.
pro mwr_groupscale, maxval, group, hdr

    sz = size(group)
    for i=0L, sz[1]-1 do begin
         pmax = max(abs(group[i,*]))
         if (pmax gt maxval) then begin
             ratio = pmax/maxval
	     psc = 'PSCAL'+strcompress(string(i+1),/remo)
	     currat = fxpar(hdr, psc)
	     if (currat ne 0) then begin
	         fxaddpar, hdr, psc, currat*ratio, 'Scaling overriden by MWRFITS'
	     endif else begin
	         fxaddpar, hdr, psc, ratio, ' Scaling added by MWRFITS'
	     endelse
             group[i,*] = group[i,*]/ratio
         endif
    endfor
end
	 
	 
; Write out header and image for IMAGE extensions and primary arrays.
pro mwr_image, input, siz, lun, bof, hdr,       $
	null=null,                              $
	group=group,                            $
	pscale=pscale, pzero=pzero,             $
	lscale=lscale, iscale=iscale,		$
	bscale=bscale,                          $
        no_comment=no_comment,                  $
	silent=silent


    type = siz[siz[0] + 1]

    bitpixes=[8,8,16,32,-32,-64,-32,0,0,-64,0,0,16,32,64,64]

    ; Convert complexes to two element real array.

    if type eq 6 or type eq 9 then begin
 
        if not keyword_set(silent) then begin
            print, "MWRFITS Note: Complex numbers treated as arrays"
        endif
    
        array_dimen=(2)
        if siz[0] gt 0 then array_dimen=[array_dimen, siz[1:siz[0]]] 
        if siz[siz[0]+1] eq 6 then data = float(input,0,array_dimen)  $
        else data = double(input,0,array_dimen)

    ; Convert strings to bytes.
    endif else if type eq 7 then begin
        data = input
        len = max(strlen(input))
        if len eq 0 then begin
            print, 'MWRFITS Error: strings all have zero length'
	    return
        endif
	
        for i=0L, n_elements(input)-1 do begin
            t = len - strlen(input[i])
	    if t gt 0 then input[i] = input[i] + string(replicate(32B, len))
        endfor
    
        ; Note that byte operation works on strings in a special way
        ; so we don't go through the subterfuge we tried above.
    
        data = byte(data)
    
    endif else if n_elements(input) gt 0 then data = input

    ; Convert scalar to 1-d array.
    if siz[0] eq 0 and siz[1] ne 0 then data=(data)

    ; Do any scaling of the data.
    mwr_scale, data, scalval, offsetval, lscale=lscale, $
      iscale=iscale, bscale=bscale, null=null

    ; This may have changed the type.
    siz  = size(data)
    type = siz[siz[0]+1]


    ; If grouped data scale the group parameters.
    if keyword_set(group) then mwr_pscale, group, hdr, pscale=pscale, pzero=pzero

    if bof then begin
        chk_and_upd, hdr, 'SIMPLE', 'T','Primary Header created by MWRFITS v'+mwr_version()
        chk_and_upd, hdr, 'EXTEND', 'T', 'Extensions may be present'
    endif else begin
        chk_and_upd, hdr, 'XTENSION', 'IMAGE','Image Extension created by MWRFITS v'+mwr_version()
        chk_and_upd, hdr, 'PCOUNT', 0
        chk_and_upd, hdr, 'GCOUNT', 1
    endelse


    if keyword_set(group) then begin
        group_offset = 1
    endif else group_offset = 0

    chk_and_upd, hdr, 'BITPIX', bitpixes[type]
    chk_and_upd, hdr, 'NAXIS', siz[0]
    if keyword_set(group) then begin
       chk_and_upd, hdr, 'NAXIS1', 0
    endif

    for i=1L, siz[0]-group_offset do begin
        chk_and_upd, hdr, 'NAXIS'+strcompress(string(i+group_offset),/remo), siz[i]
    endfor


    if keyword_set(group) then begin
        chk_and_upd, hdr, 'GROUPS', 'T'
        sizg = size(group)
        if sizg[0] ne 2 then begin
            print,'MWRFITS Error: Group data is not 2-d array'
	    return
        endif
        if sizg[2] ne siz[siz[0]] then begin
            print,'MWRFITS Error: Group data has wrong number of rows'
	    return
        endif
        chk_and_upd,hdr,  'PCOUNT', sizg[1]
        chk_and_upd, hdr, 'GCOUNT', siz[siz[0]]
    endif
    
    if n_elements(scalval) gt 0 then begin
    
        chk_and_upd, hdr, 'BSCALE', scalval
        chk_and_upd, hdr, 'BZERO', offsetval
    
    endif else begin
        if type eq 12 or type eq 13 or type eq 15 then begin
	    chk_and_upd,hdr,'BSCALE', 1
            if type eq 12 then begin
	        chk_and_upd, hdr, 'BZERO', 32768
	    endif else if type eq 13 then begin
	        chk_and_upd, hdr, 'BZERO', ulong64('2147483648')
	    endif else if type eq 15 then begin
	        chk_and_upd, hdr, 'BZERO', ulong64('9223372036854775808')
	    endif
	    data = not(data)
        endif
    endelse

    if keyword_set(group) then begin
        if keyword_set(pscale) then begin
            if n_elements(pscale) ne sizg[1] then begin
	        print, 'MWRFITS Warning: wrong number of PSCALE values'
	    endif else begin
                for i=1L, sizg[1] do begin
                    chk_and_upd, hdr, 'PSCALE'+strcompress(string(i),/remo)
	        endfor
	    endelse
        endif
        if keyword_set(pzero) then begin
            if n_elements(pscale) ne sizg[1] then begin
	        print, 'MWRFITS Warning: Wrong number of PSCALE values'
	    endif else begin
                for i=1L, sizg[1] do begin
                    chk_and_upd, hdr, 'PZERO'+strcompress(string(i),/remo)
	        endfor
	    endelse
        endif
    endif

    bytpix=abs(bitpixes[siz[siz[0]+1]])/8             ; Number of bytes per pixel.
    npixel = n_elements(data) + n_elements(group)     ; Number of pixels.

    if keyword_set(group) then mwr_groupinfix, data, group, hdr

    ; Write the FITS header
    mwr_header, lun, hdr

    ; This is all we need to do if input is undefined.
    if (n_elements(input) eq 0) or (siz[0] eq 0) then return

    ; Write the data.
    host_to_ieee, data
    writeu, lun, data

    nbytes = bytpix*npixel
    filler = 2880 - nbytes mod 2880
    if filler eq 2880 then filler = 0
  
    ; Write any needed filler.
    if filler gt 0 then writeu, lun, replicate(0B,filler)
end


; Main routine -- see documentation at start
pro mwruvfits, xinput, file, header,              $
        ascii=ascii,                            $
	separator=separator,                    $
	terminator=terminator,                  $
	create=create,                          $
	null=null,                              $
	group=group,                            $
	pscale=pscale, pzero=pzero,             $
	alias=alias,                            $
	use_colnum = use_colnum,                $
	lscale=lscale, iscale=iscale,		$
	bscale=bscale,                          $
	no_types=no_types,                      $
	silent=silent,                          $
	no_comment=no_comment,                  $
	logical_cols=logical_cols,              $
	bit_cols=bit_cols,                      $
	nbit_cols=nbit_cols,                    $
	version=version,extname=extname,unit=unit


    ; Check required keywords.

    if (keyword_set(Version)) then begin
        print, "MWRFITS V"+mwr_version()+":  2002-02-12"
    endif

    if n_elements(file) eq 0 then begin
        if (not keyword_set(Version)) then begin
            print, 'MWRFITS: Usage:'
            print, '    MWRFITS, struct_name, file, [header,] '
            print, '             /CREATE, /SILENT, /NO_TYPES, /NO_COMMENT, '
            print, '             GROUP=, PSCALE=, PZERO=,'
            print, '             LSCALE=, ISCALE=, BSCALE=,'
            print, '             LOGICAL_COLS=, BIT_COLS=, NBIT_COLS=,'
            print, '             ASCII=, SEPARATOR=, TERMINATOR=, NULL='
	    print, '             /USE_COLNUM, ALIAS='
        endif
        return
    endif


    ; Save the data into an array/structure that we can modify.
 
    if n_elements(xinput) gt 0 then input = xinput

    on_ioerror, open_error

    ; Open the input file.
    ; If the create keyword is not specified we
    ; try to open the file readonly to see if it
    ; already exists and if so we append to it.
    ; An error implies the file does not exist.
;
    ; We use this rather circuitous route to handle
    ; the unix ~ construction consistently -- findfile
    ; doesn't reliably understand that.
    ;

    if  not keyword_set(create) then begin

        on_ioerror, not_found
        openr, lun, file, /get_lun
        free_lun, lun
        on_ioerror, null
        if !version.os eq 'vms' then openu, lun, file, 2880, /block, /none, /get_lun, /append $
                                else openu, lun, file, /get_lun, /append
        bof = 0
        goto, finished_open
    endif

  not_found:
    on_ioerror, null
    if !version.os eq 'vms' then openw, lun, file, 2880, /block, /none, /get_lun $
                            else openw, lun, file, /get_lun
    bof = 1

  finished_open:

    siz = size(input)
    if siz[siz[0]+1] ne 8 then begin

        ; If input is not a structure then call image writing utilities.
        mwr_image, input, siz, lun, bof, header,    $
	  null=null,                              $
	  group=group,                            $
	  pscale=pscale, pzero=pzero,             $
	  lscale=lscale, iscale=iscale,		$
	  bscale=bscale,                          $
          no_comment=no_comment,                  $
	  silent=silent

    endif else if keyword_set(ascii) then begin

        if bof then mwr_dummy, lun
        ; Create an ASCII table.
        mwr_ascii, input, siz, lun, bof, header,     $
          ascii=ascii,                             $
	  null=null,                               $
	  use_colnum = use_colnum,                 $
	  lscale=lscale, iscale=iscale,		 $
	  bscale=bscale,                           $
	  no_types=no_types,			 $
	  separator=separator,                     $
	  terminator=terminator,                   $
          no_comment=no_comment,                   $
	  alias=alias,                             $
	  silent=silent

    endif else begin

        if bof then mwr_dummy, lun

        ; Create a binary table.
        mwr_tablehdr, lun, input, header, vtypes,    $
	   no_types=no_types,                        $
	   logical_cols = logical_cols,	             $
	   bit_cols = bit_cols,		             $
	   nbit_cols= nbit_cols,                     $
	   alias=alias,                              $
           no_comment=no_comment, extname=extname, unit=unit
	
        mwr_tabledat, lun, input, header, vtypes

    endelse

    free_lun, lun
    return
    
    ; Handle error in opening file.
  open_error:
    on_ioerror, null
    print, 'MWRFITS Error: Cannot open output: ', file
    if n_elements(lun) gt 0 then free_lun, lun
    
    return
end


;+
; Name        : 
;       FXADDPAR
; Purpose     : 
;       Add or modify a parameter in a FITS header array.
; Explanation : 
;       This version of FXADDPAR will write string values longer than 68 
;       characters using the FITS continuation convention described at 
;       http://heasarc.gsfc.nasa.gov/docs/heasarc/ofwg/docs/ofwg_recomm/r13.html
; Use         : 
;       FXADDPAR, HEADER, NAME, VALUE, COMMENT
; Inputs      : 
;       HEADER  = String array containing FITS header.  The maximum string
;                 length must be equal to 80.  If not defined, then FXADDPAR
;                 will create an empty FITS header array.
;
;       NAME    = Name of parameter.  If NAME is already in the header the
;                 value and possibly comment fields are modified. Otherwise a
;                 new record is added to the header.  If NAME is equal to
;                 either "COMMENT" or "HISTORY" then the value will be added to
;                 the record without replacement.  In this case the comment
;                 parameter is ignored.
;
;       VALUE   = Value for parameter.  The value expression must be of the
;                 correct type, e.g. integer, floating or string.
;                 String values of 'T' or 'F' are considered logical
;                 values.  If the value is a string and is "long"
;                 (more than 69 characters), then it may be continued
;                 over more than one line using the OGIP CONTINUE
;                 standard.
;
; Opt. Inputs : 
;       COMMENT = String field.  The '/' is added by this routine.  Added
;                 starting in position 31.  If not supplied, or set equal to ''
;                 (the null string), then any previous comment field in the
;                 header for that keyword is retained (when found).
; Outputs     : 
;       HEADER  = Updated header array.
; Opt. Outputs: 
;       None.
; Keywords    : 
;       BEFORE  = Keyword string name.  The parameter will be placed before the
;                 location of this keyword.  For example, if BEFORE='HISTORY'
;                 then the parameter will be placed before the first history
;                 location.  This applies only when adding a new keyword;
;                 keywords already in the header are kept in the same position.
;
;       AFTER   = Same as BEFORE, but the parameter will be placed after the
;                 location of this keyword.  This keyword takes precedence over
;                 BEFORE.
;
;       FORMAT  = Specifies FORTRAN-like format for parameter, e.g. "F7.3".  A
;                 scalar string should be used.  For complex numbers the format
;                 should be defined so that it can be applied separately to the
;                 real and imaginary parts.
;
;       /NOCONTINUE = By default, FXADDPAR will break strings longer than 68 
;                characters into multiple lines using the continuation
;                convention.    If this keyword is set, then the line will
;                instead be truncated to 68 characters.    This was the default
;                behaviour of FXADDPAR prior to December 1999.  
; Calls       : 
;       FXPAR(), FXPARPOS()
; Common      : 
;       None.
; Restrictions: 
;       Warning -- Parameters and names are not checked against valid FITS
;       parameter names, values and types.
;
;       The required FITS keywords SIMPLE (or XTENSION), BITPIX, NAXIS, NAXIS1,
;       NAXIS2, etc., must be entered in order.  The actual values of these
;       keywords are not checked for legality and consistency, however.
;
; Side effects: 
;       All HISTORY records are inserted in order at the end of the header.
;
;       All COMMENT records are also inserted in order at the end of the
;       header, but before the HISTORY records.  The BEFORE and AFTER keywords
;       can override this.
;
;       All records with no keyword (blank) are inserted in order at the end of
;       the header, but before the COMMENT and HISTORY records.  The BEFORE and
;       AFTER keywords can override this.
;
;       All other records are inserted before any of the HISTORY, COMMENT, or
;       "blank" records.  The BEFORE and AFTER keywords can override this.
;
;       String values longer than 68 characters will be split into multiple
;       lines using the OGIP CONTINUE convention, unless the /NOCONTINUE keyword
;       is set.    For a description of the CONTINUE convention see    
;       http://heasarc.gsfc.nasa.gov/docs/heasarc/ofwg/docs/ofwg_recomm/r13.htm
; Category    : 
;       Data Handling, I/O, FITS, Generic.
; Prev. Hist. : 
;       William Thompson, Jan 1992, from SXADDPAR by D. Lindler and J. Isensee.
;       Differences include:
;
;               * LOCATION parameter replaced with keywords BEFORE and AFTER.
;               * Support for COMMENT and "blank" FITS keywords.
;               * Better support for standard FITS formatting of string and
;                 complex values.
;               * Built-in knowledge of the proper position of required
;                 keywords in FITS (although not necessarily SDAS/Geis) primary
;                 headers, and in TABLE and BINTABLE extension headers.
;
;       William Thompson, May 1992, fixed bug when extending length of header,
;       and new record is COMMENT, HISTORY, or blank.
; Written     : 
;       William Thompson, GSFC, January 1992.
; Modified    : 
;       Version 1, William Thompson, GSFC, 12 April 1993.
;               Incorporated into CDS library.
;       Version 2, William Thompson, GSFC, 5 September 1997
;               Fixed bug replacing strings that contain "/" character--it
;               interpreted the following characters as a comment.
;       Version 3, Craig Markwardt, GSFC,  December 1997
;               Allow long values to extend over multiple lines
;       Version 4, D. Lindler, March 2000, modified to use capital E instead
;               of a lower case e for exponential format.
;       Version 4.1 W. Landsman April 2000, make user-supplied format uppercase
; Version     : 
;       Version 4.1, April 2000
;-
;

; This is a utility routine, which splits a parameter into several
; continuation bits.
PRO FXADDPAR_CONTPAR, VALUE, CONTINUED
  
  APOST = "'"
  BLANK = STRING(REPLICATE(32B,80)) ;BLANK line

  ;; The value may not need to be CONTINUEd.  If it does, then split
  ;; out the first value now.  The first value does not have a
  ;; CONTINUE keyword, because it will be grafted onto the proper
  ;; keyword in the calling routine.

  IF (STRLEN(VALUE) GT 68) THEN BEGIN
      CONTINUED = [ STRMID(VALUE, 0, 67)+'&' ]
      VALUE = STRMID(VALUE, 67, STRLEN(VALUE)-67)
  ENDIF ELSE BEGIN
      CONTINUED = [ VALUE ]
      RETURN
  ENDELSE

  ;; Split out the remaining values.
  WHILE( STRLEN(VALUE) GT 0 ) DO BEGIN
      H = BLANK

      ;; Add CONTINUE keyword
      STRPUT, H, 'CONTINUE  '+APOST
      ;; Add the next split
      IF(STRLEN(VALUE) GT 68) THEN BEGIN
          STRPUT, H, STRMID(VALUE, 0, 67)+'&'+APOST, 11
          VALUE = STRMID(VALUE, 67, STRLEN(VALUE)-67)
      ENDIF ELSE BEGIN
          STRPUT, H, VALUE+APOST, 11
          VALUE = ''
      ENDELSE

      CONTINUED = [ CONTINUED, H ]
  ENDWHILE

  RETURN
END

; Utility routine to add a warning to the file.  The calling routine
; must ensure that the header is in a consistent state before calling
; FXADDPAR_CONTWARN because the header will be subsequently modified
; by calls to FXADDPAR.
PRO FXADDPAR_CONTWARN, HEADER, NAME

;  By OGIP convention, the keyword LONGSTRN is added to the header as
;  well.  It should appear before the first occurrence of a long
;  string encoded with the CONTINUE convention.

  CONTKEY = FXPAR(HEADER, 'LONGSTRN', COUNT = N_LONGSTRN)

;  Calling FXADDPAR here is okay since the state of the header is
;  clean now.
  IF N_LONGSTRN GT 0 THEN $
    RETURN

  FXADDPAR, HEADER, 'LONGSTRN', 'OGIP 1.0', $
    ' The OGIP long string convention may be used.', $
    BEFORE=NAME

  FXADDPAR, HEADER, 'COMMENT', $
    ' This FITS file may contain long string keyword values that are', $
    BEFORE=NAME

  FXADDPAR, HEADER, 'COMMENT', $
    " continued over multiple keywords.  This convention uses the  '&'", $
    BEFORE=NAME

  FXADDPAR, HEADER, 'COMMENT', $
    ' character at the end of a string which is then continued', $
    BEFORE=NAME

  FXADDPAR, HEADER, 'COMMENT', $
    " on subsequent keywords whose name = 'CONTINUE'.", $
    BEFORE=NAME

  RETURN
END


PRO FXADDPAR, HEADER, NAME, VALUE, COMMENT, BEFORE=BEFORE,      $
              AFTER=AFTER, FORMAT=FORMAT, NOCONTINUE = NOCONTINUE

        ON_ERROR,2                              ;Return to caller
;
;  Check the number of parameters.
;
        IF N_PARAMS() LT 3 THEN MESSAGE, $      ;Need at least 3 parameters
                'Syntax:  FXADDPAR, HEADER, NAME, VALUE [, COMMENT ]'
;
; Define a blank line and the END line
;
        ENDLINE = 'END' + STRING(REPLICATE(32B,77))     ;END line
        BLANK = STRING(REPLICATE(32B,80))               ;BLANK line
;
;  If no comment was passed, then use a null string.
;
        IF N_PARAMS() LT 4 THEN COMMENT = ''
;
;  Check the HEADER array.
;
        N = N_ELEMENTS(HEADER)          ;# of lines in FITS header
        IF N EQ 0 THEN BEGIN            ;header defined?
                HEADER=STRARR(36)       ;no, make it.
                HEADER[0]=ENDLINE
                N=36
        ENDIF ELSE BEGIN
                S = SIZE(HEADER)        ;check for string type
                IF (S[0] NE 1) OR (S[2] NE 7) THEN MESSAGE, $
                        'FITS Header (first parameter) must be a string array'
        ENDELSE
;
;  Make sure NAME is 8 characters long
;
        NN = STRING(REPLICATE(32B,8))   ;8 char name
        STRPUT,NN,STRUPCASE(NAME)       ;Insert name
;
;  Check VALUE.
;
        S = SIZE(VALUE)         ;get type of value parameter
        STYPE = S[S[0]+1]
        IF S[0] NE 0 THEN BEGIN
                MESSAGE,'Keyword Value (third parameter) must be scalar'
        END ELSE IF STYPE EQ 0 THEN BEGIN
                MESSAGE,'Keyword Value (third parameter) is not defined'
        END ELSE IF STYPE EQ 8 THEN BEGIN
                MESSAGE,'Keyword Value (third parameter) cannot be structure'
        ENDIF
;
;  Extract first 8 characters of each line of header, and locate END line
;
        KEYWRD = STRMID(HEADER,0,8)                     ;Header keywords
        IEND = WHERE(KEYWRD EQ 'END     ',NFOUND)
;
;  If no END, then add it.  Either put it after the last non-null string, or
;  append it to the end.
;
        IF NFOUND EQ 0 THEN BEGIN
                II = WHERE(STRTRIM(HEADER) NE '',NFOUND)
                II = MAX(II) + 1
                IF (NFOUND EQ 0) OR (II EQ N_ELEMENTS(HEADER)) THEN     $
                        HEADER = [HEADER,ENDLINE] ELSE HEADER[II] = ENDLINE
                KEYWRD = STRMID(HEADER,0,8)
                IEND = WHERE(KEYWRD EQ 'END     ',NFOUND)
        ENDIF
;
        IEND = IEND[0] > 0                      ;Make scalar
;
;  History, comment and "blank" records are treated differently from the
;  others.  They are simply added to the header array whether there are any
;  already there or not.
;
        IF (NN EQ 'COMMENT ') OR (NN EQ 'HISTORY ') OR          $
                        (NN EQ '        ') THEN BEGIN
;
;  If the header array needs to grow, then expand it in increments of 36 lines.
;
                IF IEND GE (N-1) THEN BEGIN
                        HEADER = [HEADER,REPLICATE(BLANK,36)]
                        N = N_ELEMENTS(HEADER)
                ENDIF
;
;  Format the record.
;
                NEWLINE = BLANK
                STRPUT,NEWLINE,NN+STRING(VALUE),0
;
;  If a history record, then append to the record just before the end.
;
                IF NN EQ 'HISTORY ' THEN BEGIN
                        HEADER[IEND] = NEWLINE          ;add history rec.
                        HEADER[IEND+1]=ENDLINE          ;move end up
;
;  The comment record is placed immediately after the last previous comment
;  record, or immediately before the first history record, unless overridden by
;  either the BEFORE or AFTER keywords.
;
                END ELSE IF NN EQ 'COMMENT ' THEN BEGIN
                        I = FXPARPOS(KEYWRD,IEND,AFTER=AFTER,BEFORE=BEFORE)
                        IF I EQ IEND THEN I =   $
                            FXPARPOS(KEYWRD,IEND,AFTER='COMMENT',$
                                     BEFORE='HISTORY')
                        HEADER[I+1] = HEADER[I:N-2]     ;move rest up
                        HEADER[I] = NEWLINE             ;insert comment
;
;  The "blank" record is placed immediately after the last previous "blank"
;  record, or immediately before the first comment or history record, unless
;  overridden by either the BEFORE or AFTER keywords.
;
                END ELSE BEGIN
                        I = FXPARPOS(KEYWRD,IEND,AFTER=AFTER,BEFORE=BEFORE)
                        IF I EQ IEND THEN I =   $
                            FXPARPOS(KEYWRD,IEND,AFTER='',BEFORE='COMMENT')<$
                            FXPARPOS(KEYWRD,IEND,AFTER='',BEFORE='HISTORY')
                        HEADER[I+1] = HEADER[I:N-2]     ;move rest up
                        HEADER[I] = NEWLINE             ;insert "blank"
                ENDELSE
                RETURN
        ENDIF                           ;history/comment/blank
;
;  Find location to insert keyword.  If the keyword is already in the header,
;  then simply replace it.  If no new comment is passed, then retain the old
;  one.
;
        IPOS  = WHERE(KEYWRD EQ NN,NFOUND)
        IF NFOUND GT 0 THEN BEGIN
                I = IPOS[0]
                IF COMMENT EQ '' THEN BEGIN
                        SLASH = STRPOS(HEADER[I],'/')
                        QUOTE = STRPOS(HEADER[I],"'")
                        IF (QUOTE GT 0) AND (QUOTE LT SLASH) THEN BEGIN
                                QUOTE = STRPOS(HEADER[I],"'",QUOTE+1)
                                IF QUOTE LT 0 THEN SLASH = -1 ELSE      $
                                        SLASH = STRPOS(HEADER[I],'/',QUOTE+1)
                        ENDIF
                        IF SLASH NE -1 THEN     $
                                COMMENT = STRMID(HEADER[I],SLASH+1,80) ELSE $
                                COMMENT = STRING(REPLICATE(32B,80))
                ENDIF
                GOTO, REPLACE
        ENDIF
;
;  Start of section dealing with the positioning of required FITS keywords.  If
;  the keyword is SIMPLE, then it must be at the beginning.
;
        IF NN EQ 'SIMPLE  ' THEN BEGIN
                I = 0
                GOTO, INSERT
        ENDIF
;
;  In conforming extensions, if the keyword is XTENSION, then it must be at the
;  beginning. 
;
        IF NN EQ 'XTENSION' THEN BEGIN
                I = 0
                GOTO, INSERT
        ENDIF
;
;  If the keyword is BITPIX, then it must follow the either SIMPLE or XTENSION
;  keyword.
;
        IF NN EQ 'BITPIX  ' THEN BEGIN
                IF (KEYWRD[0] NE 'SIMPLE  ') AND                $
                        (KEYWRD[0] NE 'XTENSION') THEN MESSAGE, $
                        'Header must start with either SIMPLE or XTENSION'
                I = 1
                GOTO, INSERT
        ENDIF
;
;  If the keyword is NAXIS, then it must follow the BITPIX keyword.
;
        IF NN EQ 'NAXIS   ' THEN BEGIN
                IF KEYWRD[1] NE 'BITPIX  ' THEN MESSAGE,        $
                        'Required BITPIX keyword not found'
                I = 2
                GOTO, INSERT
        ENDIF
;
;  If the keyword is NAXIS1, then it must follow the NAXIS keyword.
;
        IF NN EQ 'NAXIS1  ' THEN BEGIN
                IF KEYWRD[2] NE 'NAXIS   ' THEN MESSAGE,        $
                        'Required NAXIS keyword not found'
                I = 3
                GOTO, INSERT
        ENDIF
;
;  If the keyword is NAXIS<n>, then it must follow the NAXIS<n-1> keyword.
;
        IF STRMID(NN,0,5) EQ 'NAXIS' THEN BEGIN
                NUM_AXIS = FIX(STRMID(NN,5,3))
                PREV = STRING(REPLICATE(32B,8))         ;Format NAXIS<n-1>
                STRPUT,PREV,'NAXIS',0                   ;Insert NAXIS
                STRPUT,PREV,STRTRIM(NUM_AXIS-1,2),5     ;Insert <n-1>
                IF KEYWRD[NUM_AXIS+1] NE PREV THEN MESSAGE,     $
                        'Required '+PREV+' keyword not found'
                I = NUM_AXIS + 2
                GOTO, INSERT
        ENDIF
;
;  If the first keyword is XTENSION, and has the value of either 'TABLE' or
;  'BINTABLE', then there are some additional required keywords.
;
        IF KEYWRD[0] EQ 'XTENSION' THEN BEGIN
                XTEN = FXPAR(HEADER,'XTENSION')
                IF (XTEN EQ 'TABLE   ') OR (XTEN EQ 'BINTABLE') THEN BEGIN
;
;  If the keyword is PCOUNT, then it must follow the NAXIS2 keyword.
;
                        IF NN EQ 'PCOUNT  ' THEN BEGIN
                                IF KEYWRD[4] NE 'NAXIS2  ' THEN MESSAGE, $
                                        'Required NAXIS2 keyword not found'
                                I = 5
                                GOTO, INSERT
                        ENDIF
;
;  If the keyword is GCOUNT, then it must follow the PCOUNT keyword.
;
                        IF NN EQ 'GCOUNT  ' THEN BEGIN
                                IF KEYWRD[5] NE 'PCOUNT  ' THEN MESSAGE, $
                                        'Required PCOUNT keyword not found'
                                I = 6
                                GOTO, INSERT
                        ENDIF
;
;  If the keyword is TFIELDS, then it must follow the GCOUNT keyword.
;
                        IF NN EQ 'TFIELDS ' THEN BEGIN
                                IF KEYWRD[6] NE 'GCOUNT  ' THEN MESSAGE, $
                                        'Required GCOUNT keyword not found'
                                I = 7
                                GOTO, INSERT
                        ENDIF
                ENDIF
        ENDIF
;
;  At this point the location has not been determined, so a new line is added
;  at the end of the FITS header, but before any blank, COMMENT, or HISTORY
;  keywords, unless overridden by the BEFORE or AFTER keywords.
;
        I = FXPARPOS(KEYWRD,IEND,AFTER=AFTER,BEFORE=BEFORE)
        IF I EQ IEND THEN I =                                     $
            FXPARPOS(KEYWRD,IEND,AFTER=AFTER,BEFORE='')         < $
            FXPARPOS(KEYWRD,IEND,AFTER=AFTER,BEFORE='COMMENT')  < $
            FXPARPOS(KEYWRD,IEND,AFTER=AFTER,BEFORE='HISTORY')
;
;  A new line needs to be added.  First check to see if the length of the
;  header array needs to be extended.  Then insert a blank record at the proper
;  place.
;
INSERT:
        IF IEND EQ (N-1) THEN BEGIN
                HEADER = [HEADER,REPLICATE(BLANK,36)]
                N = N_ELEMENTS(HEADER)
        ENDIF
        HEADER[I+1] = HEADER[I:N-2]
        HEADER[I] = BLANK
        IEND = IEND + 1        ; CM 24 Sep 1997
;
;  Now put value into keyword at line I.
;
REPLACE: 
        H=BLANK                 ;80 blanks
        STRPUT,H,NN+'= '        ;insert name and =.
        APOST = "'"             ;quote (apostrophe) character
        TYPE = SIZE(VALUE)      ;get type of value parameter
;
;  Store the value depending on the data type.  If a character string, first
;  check to see if it is one of the logical values "T" (true) or "F" (false).
;

        IF TYPE[1] EQ 7 THEN BEGIN              ;which type?
                UPVAL = STRUPCASE(VALUE)        ;force upper case.
                IF (UPVAL EQ 'T') OR (UPVAL EQ 'F') THEN BEGIN
                        STRPUT,H,UPVAL,29       ;insert logical value.
;
;  Otherwise, remove any tabs, and check for any apostrophes in the string.
;
                END ELSE BEGIN
                        VAL = DETABIFY(VALUE)
                        NEXT_CHAR = 0
                        REPEAT BEGIN
                                AP = STRPOS(VAL,"'",NEXT_CHAR)
                                IF AP GE 66 THEN BEGIN
                                        VAL = STRMID(VAL,0,66)
                                END ELSE IF AP GE 0 THEN BEGIN
                                        VAL = STRMID(VAL,0,AP+1) + APOST + $
                                          STRMID(VAL,AP+1,80)
                                        NEXT_CHAR = AP + 2
                                ENDIF
                        ENDREP UNTIL AP LT 0

;
;  If a long string, then add the comment as soon as possible.
;
; CM 24 Sep 1997
;  Separate parameter if it needs to be CONTINUEd.
;
                        IF NOT KEYWORD_SET(NOCONTINUE) THEN $
                             FXADDPAR_CONTPAR, VAL, CVAL  ELSE $
                             CVAL = STRMID(VAL,0,68)
                        K = I + 1
                        ;; See how many CONTINUE lines there already are
                        WHILE K LT IEND DO BEGIN
                            IF STRMID(HEADER[K],0,8) NE 'CONTINUE' THEN $
                              GOTO, DONE_CHECK_CONT
                            K = K + 1
                        ENDWHILE
                        
                        DONE_CHECK_CONT:
                        NOLDCONT = K - I - 1
                        NNEWCONT = N_ELEMENTS(CVAL) - 1

                        ;; Insert new lines if needed
                        IF NNEWCONT GT NOLDCONT THEN BEGIN
                            INS = NNEWCONT - NOLDCONT
                            WHILE IEND+INS GT N DO BEGIN
                                HEADER = [HEADER, REPLICATE(BLANK,36)]
                                N = N_ELEMENTS(HEADER)
                            ENDWHILE
                        ENDIF 

                        ;; Shift the old lines properly
                        IF NNEWCONT NE NOLDCONT THEN $
                          HEADER[I+NNEWCONT+1] = HEADER[I+NOLDCONT+1:IEND]
                        IEND = IEND + NNEWCONT - NOLDCONT

                        ;; Blank out any lines at the end if needed
                        IF NNEWCONT LT NOLDCONT THEN BEGIN
                            DEL = NOLDCONT - NNEWCONT
                            HEADER[IEND+1:IEND+DEL] = REPLICATE('', DEL)
                        ENDIF

                        IF STRLEN(CVAL[0]) GT 18 THEN BEGIN
                            STRPUT,H,APOST+STRMID(CVAL[0],0,68)+APOST+ $
                              ' /'+COMMENT,10
                            HEADER[I]=H
                                
;  There might be a continuation of this string.  CVAL would contain
;  more than one element if that is so.
                            
                            ;; Add new continuation lines
                            IF N_ELEMENTS(CVAL) GT 1 THEN BEGIN
                              HEADER[I+1] = CVAL[1:*]
                            
                            ;; Header state is now clean, so add
                            ;; warning to header

                               FXADDPAR_CONTWARN, HEADER, NAME
                            ENDIF
                            DONE_CONT:
                            RETURN
;
;  If a short string, then pad out to at least eight characters.
;
                        END ELSE BEGIN
                                STRPUT,H,APOST+CVAL[0],10
                                STRPUT,H,APOST,11+(STRLEN(CVAL[0])>8)
                        ENDELSE

                    ENDELSE
;
;  If complex, then format the real and imaginary parts, and add the comment
;  beginning in column 51.
;
        END ELSE IF TYPE[1] EQ 6 THEN BEGIN
                IF N_ELEMENTS(FORMAT) EQ 1 THEN BEGIN   ;use format keyword
                        VR = STRING(FLOAT(VALUE),    '('+STRUPCASE(FORMAT)+')')
                        VI = STRING(IMAGINARY(VALUE),'('+STRUPCASE(FORMAT)+')')
                END ELSE BEGIN
                        VR = STRTRIM(FLOAT(VALUE),2)
                        VI = STRTRIM(IMAGINARY(VALUE),2)
                ENDELSE
                SR = STRLEN(VR)  &  STRPUT,H,VR,(30-SR)>10
                SI = STRLEN(VI)  &  STRPUT,H,VI,(50-SI)>30
                STRPUT,H,' /'+COMMENT,50
                HEADER[I] = H
                RETURN
;
;  If not complex or a string, then format according to either the FORMAT
;  keyword, or the default for that datatype.
;
        END ELSE BEGIN
                IF (N_ELEMENTS(FORMAT) EQ 1) THEN $ ;use format keyword
                        V = STRING(VALUE,'('+STRUPCASE(FORMAT)+')' ) ELSE $
                        V = STRTRIM(strupcase(VALUE),2)    ;default format
                S = STRLEN(V)                 ;right justify
                STRPUT,H,V,(30-S)>10          ;insert
        ENDELSE
;
;  Add the comment, and store the completed line in the header.
;
        STRPUT,H,' /',30        ;add ' /'
        STRPUT,H,COMMENT,32     ;add comment
        HEADER[I]=H             ;save line
;
        RETURN
        END




function fits_wlog,name,value,comment
;
; write fits log
;
common fits,str_buf,unit
res=fits_buff(string(name,value,comment,format='(a,T9,"=",T30,a,T32,"/",T34,a,T81)'))
return,1
end



function fits_init,filename
;
; initialize fits file 
;
; paramters : fits_file -- fits file name
;             fits_ext -- fits extension
;
; eg result=fits_init(fits_file,fits_ext,unit)
;
common global
common fits,str_buf,unit

;print,'try to open fits file ',filename

ier=0
openw,unit,filename,2880,error=ier,/get_lun

if (ier eq 0) then begin
   res=fits_buff('',/init)
   return,1
endif else begin
  print,!err_string
  return,0
endelse
end





function fits_wint,name,value,comment
;
; write fits integer
;
common fits,str_buf,unit
res=fits_buff(string(name,value,comment,format='(a,T9,"=",T11,i20,T32,"/",T35,a,T81)'))
return,1
end



function fits_wstr,name,value,comment
;
; write fits string
;
common fits,str_buf,unit
apost="'"
res=fits_buff(string(name,apost,value,apost,comment,format='(a,T9,"=",T11,a,a,a,T32,"/",T35,a,T81)'))
return,1
end





function fits_wreal,name,value,comment
;
; write fits real
;
common fits,str_buf,unit
res=fits_buff(string(name,value,comment,format='(a,T9,"=",T11,e20.6,T32,"/",T35,a,T81)'))
return,1
end






function fits_wdble,name,value,comment
;
; write fits real
;
common fits,str_buf,unit
res=fits_buff(string(name,value,comment,format='(a,T9,"=",T11,E20.12,T32,"/",T35,a,T81)'))
return,1
end




function fits_wcmt,name,comment
;
; write fits comment
;
common fits,str_buf,unit
res=fits_buff(string(name,comment,format='(a,T9,a,T81)'))
return,1
end





function fits_wend
;
; write fits end
;
common fits,str_buf,unit
print,'fits end: str_buf = ',strlen(str_buf)
res=fits_buff(string('END',format='(a,T81)'),/flush)
return,1
end







function fits_wrayi,idata,flush=flush
;
;     Write out as signed 32 bit integers.  
;
; eg. result=fits_wrayi,idata
;
common fits,str_buf,unit
if keyword_set(init) then begin
  idata[*]=0
  return,1
endif
npt=n_elements(idata)
;print,'npt in idata ',npt
nwrite=long(ceil(1.*npt/720))
; print,'nwrite ',nwrite
ntot=nwrite*720
;print,'ntot ',ntot
if ntot gt npt then idata=[idata,make_array(ntot-npt,/int)]
;print,'idata now ',n_elements(idata)
if keyword_set(flush) then begin
  for i=1L,nwrite do begin
;print,'i start end ',i,(i-1L)*720L,i*720L-1L
    odata = idata[(i-1L)*720L:i*720L-1L]
;    stop, odata
    if (strpos(!VERSION.ARCH,'x86') ge 0) or (strpos(!VERSION.ARCH,'alpha') ge 0) then $
        byteorder, odata, /LSWAP
;    stop, odata
    writeu,unit, odata

;    if (i eq 1) then print,'idata ',idata[(i-1L)*720L:i*720L-1L]
  endfor
endif
return,1
end


function fits_want_ascii,ns,ver,stname,stx,sty,stz,site
;-----------------------------------------------------------------------
; Append AIPS antenna table as a FITS table extension to the FITS file.
;
; Arguments:
;  ns      (input, I*4)       Number of antennas.
;  ver     (input, i*4)       version number of table.
;  stname  (input, C*8 array) Names of antennas.
;  stx     (input, R*8 array) Antenna x-coordinates (m).
;  sty     (input, R*8 array) Antenna y-coordinates (m).
;  stz     (input, R*8 array) Antenna z-coordinates (m).
;-----------------------------------------------------------------------
;
common fits,str_buf,unit
res=fits_buff('',/init)
result=fits_wstr ('XTENSION', 'TABLE   ', 'EXTENSION TYPE')
result=fits_wint ('BITPIX', 8, 'PRINTABLE ASCII CODES')
result=fits_wint ('NAXIS',  2, 'TABLE IS A MATRIX')
result=fits_wint ('NAXIS1', 80, 'WIDTH OF TABLE IN CHARACTERS')
result=fits_wint ('NAXIS2', ns, 'NUMBER OF ENTRIES IN TABLE')
result=fits_wint ('PCOUNT', 0, 'NO RANDOM PARAMETERS')
result=fits_wint ('GCOUNT', 1, 'GROUP COUNT')
result=fits_wint ('TFIELDS', 5, 'NUMBER OF FIELDS IN EACH ROW')
result=fits_wstr ('EXTNAME', 'AIPS AN', 'AIPS ANTENNA TABLE')
result=fits_wint ('EXTVER',ver, 'VERSION NUMBER OF TABLE')
;
result=fits_wint ('TBCOL1', 1, 'STARTING COLUMN')
result=fits_wstr ('TFORM1', 'I3', 'FORTRAN FORMAT')
result=fits_wstr ('TTYPE1', 'ANT NO. ', 'ANTENNA NUMBER')
;
result=fits_wint ('TBCOL2', 7, 'STARTING COLUMN')
result=fits_wstr ('TFORM2', 'A8', 'FORTRAN FORMAT')
result=fits_wstr ('TTYPE2', 'STATION', 'ANTENNA NAME')
;
result=fits_wint ('TBCOL3', 15, 'STARTING COLUMN')
result=fits_wstr ('TFORM3', 'D20.10', 'FORTRAN FORMAT')
result=fits_wstr ('TTYPE3', 'LX', 'ANTENNA X COORDINATE')
result=fits_wstr ('TUNIT3', 'METERS', 'PHYSICAL UNITS')
result=fits_wreal('TSCAL3', 1.0, ' ')
result=fits_wreal('TZERO3', 0.0, ' ')
;
result=fits_wint ('TBCOL4', 35,  'STARTING COLUMN')
result=fits_wstr ('TFORM4', 'D20.10', 'FORTRAN FORMAT')
result=fits_wstr ('TTYPE4', 'LY', 'ANTENNA Y COORDINATE')
result=fits_wstr ('TUNIT4', 'METERS', 'PHYSICAL UNITS')
result=fits_wreal('TSCAL4', 1.0, ' ')
result=fits_wreal('TZERO4', 0.0, ' ')
;
result=fits_wint ('TBCOL5', 55, 'STARTING COLUMN')
result=fits_wstr ('TFORM5', 'D20.10', 'FORTRAN FORMAT')
result=fits_wstr ('TTYPE5', 'LZ', 'ANTENNA Z COORDINATE')
result=fits_wstr ('TUNIT5', 'METERS', 'PHYSICAL UNITS')
result=fits_wreal('TSCAL5', 1.0, ' ')
result=fits_wreal('TZERO5', 0.0, ' ')
;
if (site eq 'saosma') then begin
; This is the SMA Pad 1 location.
  result=fits_wdble('ARRAYX',-5.46242840757d6,'')
  result=fits_wdble('ARRAYY',-2.49196045618d6,'')
  result=fits_wdble('ARRAYZ', 2.28652665343d6,'')
endif 
if (site eq 'ovromma') then begin
; This is the OVRO VLBA site which is not quite the
; correct location for the reference point of the 
; millimeter array.
  result=fits_wdble('ARRAYX',-2.40612582165d6,'')
  result=fits_wdble('ARRAYY',-4.47312856557d6,'')
  result=fits_wdble('ARRAYZ', 3.85876003303d6,'')
endif

;result=fits_wdble('FREQ  ',freq0,'')

result=fits_wend ()

for is=0,ns-2 do begin
  res=fits_buff(string(is+1,stname(is),stx(is),  $
             sty(is),stz(is),format='(i3,3X,a8,3d20.10,T81)'))
endfor

  is = ns-1
  res=fits_buff(string(is+1,stname(is),stx(is),  $
             sty(is),stz(is),format='(i3,3X,a8,3d20.10,T81)'),/flush)

end



function fits_want,ns,ver,stnum,stname,stx,sty,stz,freq0,datobs,site
;-----------------------------------------------------------------------
; Append AIPS antenna table as a FITS table extension to the FITS file.
;
; Arguments:
;  ns      (input, I*4)       Number of antennas.
;  ver     (input, i*4)       version number of table.
;  stnum   (input, I*4 array) Numbers of antennas
;  stname  (input, C*8 array) Names of antennas.
;  stx     (input, R*8 array) Antenna x-coordinates (m).
;  sty     (input, R*8 array) Antenna y-coordinates (m).
;  stz     (input, R*8 array) Antenna z-coordinates (m).
;-----------------------------------------------------------------------
;
common fits,str_buf,unit

obs_date=fix(strsplit(datobs,'-',/extract))
mjd=uti_date2mjd(obs_date[0],obs_date[1],obs_date[2])
Tu = (mjd - 51544.5d)/36525.
gst=(24110.54841d + 8640184.812866d*Tu + .093104d*Tu^2 - 6.2e-6*Tu^3) mod 86400.d
gst=gst >0 ? gst:gst+86400.
gstia0=gst/240.d

wbytes = 70

res=fits_buff('',/init)
result=fits_wstr('XTENSION','BINTABLE','Extension type')
result=fits_wint('BITPIX  ',8,'Binary data')
result=fits_wint('NAXIS   ',2,'Table is a matrix ')  
result=fits_wint ('NAXIS1', wbytes, 'WIDTH OF TABLE IN BYTES')
result=fits_wint ('NAXIS2', ns, 'NUMBER OF ENTRIES IN TABLE')
result=fits_wint ('PCOUNT', 0, 'NO RANDOM PARAMETERS')
result=fits_wint ('GCOUNT', 1, 'GROUP COUNT')
result=fits_wint ('TFIELDS', 12, 'NUMBER OF FIELDS IN EACH ROW')
result=fits_wstr ('EXTNAME', 'AIPS AN', 'AIPS ANTENNA TABLE')
;result=fits_wstr ('EXTNAME', 'ARRAY_GEOMETRY', 'AIPS ANTENNA TABLE')
result=fits_wint ('EXTVER',ver, 'VERSION NUMBER OF TABLE')

result=fits_wstr('TFORM1','8A      ',        'Fortran format of field 1')
result=fits_wstr('TTYPE1','ANNAME          ','Type (heading) of field 1')
result=fits_wstr('TUNIT1','        ',        'Physical units of field 1') 

result=fits_wstr('TFORM2','3D      ',        'Fortran format of field 2')
result=fits_wstr('TTYPE2','STABXYZ         ','Type (heading) of field 2')
result=fits_wstr('TUNIT2','METERS  ',        'Physical units of field 2') 

result=fits_wstr('TFORM3','0D      ',        'Fortran format of field 3')
result=fits_wstr('TTYPE3','ORBPARM         ','Type (heading) of field 3')
result=fits_wstr('TUNIT3','        ',        'Physical units of field 3')

result=fits_wstr('TFORM4','1J      ',        'Fortran format of field 4')
result=fits_wstr('TTYPE4','NOSTA           ','Type (heading) of field 4')
result=fits_wstr('TUNIT4','        ',        'Physical units of field 4')
 
result=fits_wstr('TFORM5','1J      ',        'Fortran format of field 5')
result=fits_wstr('TTYPE5','MNTSTA           ','Type (heading) of field 5')
result=fits_wstr('TUNIT5','        ',        'Physical units of field 5')
 
result=fits_wstr('TFORM6','1E      ',        'Fortran format of field 6')
result=fits_wstr('TTYPE6','STAXOF          ','Type (heading) of field 6')
result=fits_wstr('TUNIT6','METERS  ',        'Physical units of field 6')
 
result=fits_wstr('TFORM7','1A      ',        'Fortran format of field 7')
result=fits_wstr('TTYPE7','POLTYA          ','Type (heading) of field 7')
result=fits_wstr('TUNIT7','DEGREES ',        'Physical units of field 7')
 
result=fits_wstr('TFORM8','1E      ',        'Fortran format of field 8')
result=fits_wstr('TTYPE8','POLAA           ','Type (heading) of field 8')
result=fits_wstr('TUNIT8','DEGREES ',        'Physical units of field 8')
 
result=fits_wstr('TFORM9','2E      ',        'Fortran format of field 9')
result=fits_wstr('TTYPE9','POLCALA         ','Type (heading) of field 9')
result=fits_wstr('TUNIT9','        ',        'Physical units of field 9')
 
result=fits_wstr('TFORM10','1A      ',        'Fortran format of field 10')
result=fits_wstr('TTYPE10','POLTYB          ','Type (heading) of field 10')
result=fits_wstr('TUNIT10','        ',        'Physical units of field 10')
 
result=fits_wstr('TFORM11','1E      ',        'Fortran format of field 11')
result=fits_wstr('TTYPE11','POLAB           ','Type (heading) of field 11')
result=fits_wstr('TUNIT11','DEGREES ',        'Physical units of field 11')
 
result=fits_wstr('TFORM12','2E      ',        'Fortran format of field 12')
result=fits_wstr('TTYPE12','POLCALB         ','Type (heading) of field 12')
result=fits_wstr('TUNIT12','        ',        'Physical units of field 12') 


; The array center coordinates have the X axis pointing along
; longitude 0.0 (Greenwich and East). This is different from the
; convention used in STABXYZ above where the X axis points along
; 0 hour angle and the Y axis points along -6 HA (East). 
; The difference between the 2 systems is that X and Y are interchanged
; and the sign of Y is reversed.

if (site eq 'saosma') then begin
; This is the SMA Pad 1 location.
  result=fits_wdble('ARRAYX',-5.46242840757d6,'')
  result=fits_wdble('ARRAYY',-2.49196045618d6,'')
  result=fits_wdble('ARRAYZ', 2.28652665343d6,'')
endif 
if (site eq 'ovromma') then begin
; This is the OVRO VLBA site which is not quite the
; correct location for the reference point of the 
; millimeter array.
  result=fits_wdble('ARRAYX',-2.40612582165d6,'')
  result=fits_wdble('ARRAYY',-4.47312856557d6,'')
  result=fits_wdble('ARRAYZ', 3.85876003303d6,'')
endif
result=fits_wstr ('XYZHAND', 'RIGHT', 'HANDNESS OF STATION COORD')
result=fits_wdble('GSTIA0',gstia0,'')
result=fits_wdble('DEGPDY',3.6098564497557D+02,'')
result=fits_wdble('FREQ  ',freq0,'')
result=fits_wstr ('RDATE ',datobs,'')
result=fits_wdble('POLARX',0.d0,'')
result=fits_wdble('POLARY',0.d0,'')
result=fits_wdble('UT1UTC',0.d0,'')
result=fits_wdble('DATUTC',0.d0,'')
result=fits_wstr ('TIMSYS','UTC     ','')
;;result=fits_wstr ('ARRNAM','SMA     ','')  
result=fits_wint ('NUMORB',0,'')
result=fits_wint ('NOPCAL',0,'')
;result=fits_wint ('NUMORB',0,'')
result=fits_wint ('FREQID',-1,'')
result=fits_wdble('IATUTC',0.d0,'')


if (site eq 'saosma') then  result=fits_wstr('ARRNAM ','SMA','')
if (site eq 'ovromma') then result=fits_wstr('ARRNAM ','OVRO MMA','')

result=fits_wend ()

;;print,'stnum  ',stnum
;;print,'stname ',stname

m = ' ' 

for i=0,ns-1 do begin

;ANNAME
  nchars = strlen(stname[i]) 
  for k = 0,nchars-1 do begin
      writeu,unit,strupcase(strmid(stname[i],k,1))
  endfor
  if (nchars lt 8) then begin
    for k = nchars,7 do begin
      writeu,unit,m
    endfor
  endif   

;STABXYZ
    d =  double(stx[i])  & byteorder,d,/DTOXDR  & writeu,unit,d
    d =  double(sty[i])  & byteorder,d,/DTOXDR  & writeu,unit,d
    d =  double(stz[i])  & byteorder,d,/DTOXDR  & writeu,unit,d

;NOSTA
;  j  = long(stnum[i])  & byteorder,j,/HTONL &  writeu,unit,j
   j  = i+1L  & byteorder,j,/HTONL &  writeu,unit,j

;MNTSTA
  j  = 0L              & byteorder,j,/HTONL &  writeu,unit,j

;ORBPARM
;    d = 0.d0 & byteorder,d,/DTOXDR  & writeu,unit,d

;STXOF
    f  = 0.0          & byteorder,f,/FTOXDR  & writeu,unit,f

;POLTYA
    writeu,unit,'R'  

;POLAA
    f  = 0.0          & byteorder,f,/FTOXDR  & writeu,unit,f

;POLCALA
    f  = 0.0          & byteorder,f,/FTOXDR  & writeu,unit,f
    f  = 0.0          & byteorder,f,/FTOXDR  & writeu,unit,f

;POLTYB
    writeu,unit,'L'

;POLAB
    f  = 0.0          & byteorder,f,/FTOXDR  & writeu,unit,f

;POLCALB
    f  = 0.0          & byteorder,f,/FTOXDR  & writeu,unit,f
    f  = 0.0          & byteorder,f,/FTOXDR  & writeu,unit,f


endfor



for i = 1,2880 - wbytes*ns do begin
  j = byte(0) & writeu,unit,j
endfor  



end





function fits_buff,str,init=init,flush=flush
;
; add str to buffer str_buf and write out when 2880 bytes
;
common fits,str_buf,unit
if keyword_set(init) then begin
  str_buf=''
  return,1
endif
;;print,str
;print,'new size of buffer ',strlen(str_buf+str)
if keyword_set(flush) then begin
  while strlen(str_buf+str) lt 2880 do begin
    str=str+string('',format='(a,T81)')
  endwhile
endif
str_buf=str_buf+str
if strlen(str_buf) eq 2880 then begin
;print,'Now writing out the 2880 byte str_buf'
  writeu,unit,str_buf
  str_buf=''
endif
return,1
end





function fits_su,no_if,nsources,source,suid,qual,calcode,iflux, $
    raepo,decepo,epoch,lsrvel,restfreq,veldef,veltyp

common global
common data_set
common plo
common fits,str_buf,unit

wbytes = 92 + no_if*40
nt = strtrim(string(no_if),2)
blanks = '      '
if (no_if gt 9) then blanks = '     '

res=fits_buff('',/init)
result=fits_wstr('XTENSION','BINTABLE','Extension type')
result=fits_wint('BITPIX  ',8,'Binary data') 
result=fits_wint('NAXIS   ',2,'Table is a matrix ')
result=fits_wint('NAXIS1  ',wbytes,'Width of table in bytes')
result=fits_wint('NAXIS2  ',nsources,'Number of entries in table ')
result=fits_wint('PCOUNT  ',0,'Random parameter count')
result=fits_wlog('GCOUNT  ','1','Group count')
result=fits_wint('TFIELDS ',19,'Number of fields in each row')
result=fits_wstr('EXTNAME ','AIPS SU','AIPS table file')
result=fits_wint('EXTVER  ',1,'Version number of table')


result=fits_wstr('TFORM1','1J      ',        'Fortran format of field 1')
result=fits_wstr('TTYPE1','ID. NO.         ','Type (heading) of field 1')
result=fits_wstr('TUNIT1','        ',        'Physical units of field 1')

result=fits_wstr('TFORM2','16A     ',        'Fortran format of field 2')
result=fits_wstr('TTYPE2','SOURCE          ','Type (heading) of field 2')
result=fits_wstr('TUNIT2','        ',        'Physical units of field 2')

result=fits_wstr('TFORM3','1J      ',        'Fortran format of field 3')
result=fits_wstr('TTYPE3','QUAL            ','Type (heading) of field 3')
result=fits_wstr('TUNIT3','        ',        'Physical units of field 3')

result=fits_wstr('TFORM4','4A      ',        'Fortran format of field 4')
result=fits_wstr('TTYPE4','CALCODE         ','Type (heading) of field 4')
result=fits_wstr('TUNIT4','        ',        'Physical units of field 4')

result=fits_wstr('TFORM5',nt+'E'+blanks,     'Fortran format of field 5')
result=fits_wstr('TTYPE5','IFLUX           ','Type (heading) of field 5')
result=fits_wstr('TUNIT5','JY      ',        'Physical units of field 5')

result=fits_wstr('TFORM6',nt+'E'+blanks,     'Fortran format of field 6')
result=fits_wstr('TTYPE6','QFLUX           ','Type (heading) of field 6')
result=fits_wstr('TUNIT6','JY      ',        'Physical units of field 6')

result=fits_wstr('TFORM7',nt+'E'+blanks,     'Fortran format of field 7')
result=fits_wstr('TTYPE7','UFLUX           ','Type (heading) of field 7')
result=fits_wstr('TUNIT7','JY      ',        'Physical units of field 7')

result=fits_wstr('TFORM8',nt+'E'+blanks,     'Fortran format of field 8')
result=fits_wstr('TTYPE8','VFLUX           ','Type (heading) of field 8')
result=fits_wstr('TUNIT8','JY      ',        'Physical units of field 8')

result=fits_wstr('TFORM9',nt+'D'+blanks,     'Fortran format of field 9')
result=fits_wstr('TTYPE9','FREQOFF         ','Type (heading) of field 9')
result=fits_wstr('TUNIT9','HZ      ',        'Physical units of field 9')

result=fits_wstr('TFORM10','1D      ',        'Fortran format of field 10')
result=fits_wstr('TTYPE10','BANDWIDTH       ','Type (heading) of field 10')
result=fits_wstr('TUNIT10','HZ      ',        'Physical units of field 10')

result=fits_wstr('TFORM11','1D      ',        'Fortran format of field 11')
result=fits_wstr('TTYPE11','RAEPO           ','Type (heading) of field 11')
result=fits_wstr('TUNIT11','DEGREES ',        'Physical units of field 11')

result=fits_wstr('TFORM12','1D      ',        'Fortran format of field 12')
result=fits_wstr('TTYPE12','DECEPO          ','Type (heading) of field 12')
result=fits_wstr('TUNIT12','DEGREES ',        'Physical units of field 12')

result=fits_wstr('TFORM13','1D      ',        'Fortran format of field 13')
result=fits_wstr('TTYPE13','EPOCH           ','Type (heading) of field 13')
result=fits_wstr('TUNIT13','YEARS   ',        'Physical units of field 13')

result=fits_wstr('TFORM14','1D      ',        'Fortran format of field 14')
result=fits_wstr('TTYPE14','RAAPP           ','Type (heading) of field 14')
result=fits_wstr('TUNIT14','DEGREES ',        'Physical units of field 14')

result=fits_wstr('TFORM15','1D      ',        'Fortran format of field 15')
result=fits_wstr('TTYPE15','DECAPP          ','Type (heading) of field 15')
result=fits_wstr('TUNIT15','DEGREES ',        'Physical units of field 15')

result=fits_wstr('TFORM16',nt+'D'+blanks,     'Fortran format of field 16')
result=fits_wstr('TTYPE16','LSRVEL          ','Type (heading) of field 16')
result=fits_wstr('TUNIT16','M/SEC   ',        'Physical units of field 16')

result=fits_wstr('TFORM17',nt+'D'+blanks,     'Fortran format of field 17')
result=fits_wstr('TTYPE17','RESTFREQ        ','Type (heading) of field 17')
result=fits_wstr('TUNIT17','HZ      ',        'Physical units of field 17')

result=fits_wstr('TFORM18','1D      ',        'Fortran format of field 18')
result=fits_wstr('TTYPE18','PMRA            ','Type (heading) of field 18')
result=fits_wstr('TUNIT18','DEG/DAY ',        'Physical units of field 18')

result=fits_wstr('TFORM19','1D      ',        'Fortran format of field 19')
result=fits_wstr('TTYPE19','PMDEC           ','Type (heading) of field 19')
result=fits_wstr('TUNIT19','DEG/DAY ',        'Physical units of field 19')

result=fits_wint('NO_IF   ',no_if,' ')
result=fits_wstr('VELTYP  ',veltyp,' ')
result=fits_wstr('VELDEF  ',veldef,' ')
result=fits_wint('FREQID  ',-1,' ')

result=fits_wend()

m = ' '

print,nsources
for i = 0, nsources-1 do begin

; ID_NO
;  j  = long(suid[i])    & byteorder,j,/HTONL &  writeu,unit,j
  j  = long(1) & byteorder,j,/HTONL &  writeu,unit,j

; SOURCE
  nchars = strlen(source[i])
  for k = 0,nchars-1 do begin
      writeu,unit,strupcase(strmid(source[i],k,1))
  endfor
  if (nchars lt 16) then begin
    for k = nchars,15 do begin
      writeu,unit,m
    endfor
  endif

; QUAL
  j  = long(qual(i))  & byteorder,j,/HTONL &  writeu,unit,j

; CALCODE
  nchars = strlen(calcode[i])
  for k = 0,nchars-1 do begin
      writeu,unit,strmid(calcode[i],k,1)
  endfor
  if (nchars lt 4) then begin
    for k = nchars,3 do begin
      writeu,unit,m
    endfor
  endif

; IFLUX
  for n = 0,no_if-1 do begin
    f  = iflux[i]     & byteorder,f,/FTOXDR  & writeu,unit,f
  endfor
; QFLUX
  for n = 0,no_if-1 do begin
    f  = 0.0          & byteorder,f,/FTOXDR  & writeu,unit,f
  endfor
; UFLUX
  for n = 0,no_if-1 do begin
    f  = 0.0          & byteorder,f,/FTOXDR  & writeu,unit,f
  endfor
; VFLUX
  for n = 0,no_if-1 do begin
    f  = 0.0          & byteorder,f,/FTOXDR  & writeu,unit,f
  endfor

; FREQOFF
  for n = 0,no_if-1 do begin
    d =  double(0.0)  & byteorder,d,/DTOXDR  & writeu,unit,d
  endfor

; BANDWIDTH
  d =  double(0.0)  & byteorder,d,/DTOXDR  & writeu,unit,d

  d =  raepo[i]     & byteorder,d,/DTOXDR  & writeu,unit,d
  d =  decepo[i]    & byteorder,d,/DTOXDR  & writeu,unit,d
  d =  epoch[i]     & byteorder,d,/DTOXDR  & writeu,unit,d
  d =  raepo[i]     & byteorder,d,/DTOXDR  & writeu,unit,d
  d =  decepo[i]    & byteorder,d,/DTOXDR  & writeu,unit,d
  for n = 0,no_if-1 do begin
    d =  lsrvel[i]    & byteorder,d,/DTOXDR  & writeu,unit,d
  endfor
  for n = 0,no_if-1 do begin
    d =  restfreq[i]    & byteorder,d,/DTOXDR & writeu,unit,d
  endfor

; PMRA
  d =  double(0.0)   & byteorder,d,/DTOXDR  & writeu,unit,d
; PMDEC
  d =  double(0.0)   & byteorder,d,/DTOXDR  & writeu,unit,d
endfor

for i = 1,2880 - wbytes*nsources do begin
  j = byte(0) & writeu,unit,j
endfor


return,1
end






function fits_fq,no_frqid,no_if,freqoffset,chwidth,bandwidth,sideband

common global
common data_set
common plo
common fits,str_buf,unit

nt = strtrim(string(no_if),2)
blanks = '      '
if (no_if gt 9) then blanks = '     '
wbytes = 4 + no_if*20

res=fits_buff('',/init)
result=fits_wstr('XTENSION','BINTABLE','Extension type')
result=fits_wint('BITPIX  ',8,'Binary data') 
result=fits_wint('NAXIS   ',2,'Table is a matrix ')
result=fits_wint('NAXIS1  ',wbytes,'Width of table in bytes')
result=fits_wint('NAXIS2  ',no_frqid,'Number of entries in table ')
result=fits_wint('PCOUNT  ',0,'Random parameter count')
result=fits_wlog('GCOUNT  ','1','Group count')
result=fits_wint('TFIELDS ',5,'Number of fields in each row')
result=fits_wstr('EXTNAME ','AIPS FQ','AIPS table file')
result=fits_wint('EXTVER  ',1,'Version number of table')


result=fits_wstr('TFORM1','1J      ',        'Fortran format of field 1')
result=fits_wstr('TTYPE1','FRQSEL          ','Type (heading) of field 1')
result=fits_wstr('TUNIT1','        ',        'Physical units of field 1')

result=fits_wstr('TFORM2',nt+'D'+blanks,     'Fortran format of field 2')
result=fits_wstr('TTYPE2','IF FREQ         ','Type (heading) of field 2')
result=fits_wstr('TUNIT2','HZ      ',        'Physical units of field 2')

result=fits_wstr('TFORM3',nt+'E'+blanks,     'Fortran format of field 3')
result=fits_wstr('TTYPE3','CH WIDTH        ','Type (heading) of field 3')
result=fits_wstr('TUNIT3','HZ      ',        'Physical units of field 3')

result=fits_wstr('TFORM4',nt+'E'+blanks,     'Fortran format of field 4')
result=fits_wstr('TTYPE4','TOTAL BANDWIDTH ','Type (heading) of field 4')
result=fits_wstr('TUNIT4','HZ      ',        'Physical units of field 4')

result=fits_wstr('TFORM5',nt+'J'+blanks,     'Fortran format of field 5')
result=fits_wstr('TTYPE5','SIDEBAND        ','Type (heading) of field 5')
result=fits_wstr('TUNIT5','        ',        'Physical units of field 5')

result=fits_wint ('NO_IF ',no_if,' ')

result=fits_wend()

;print,'sideband array',sideband

for i = 0, no_frqid-1 do begin
  j  = long(i+1)  & byteorder,j,/HTONL &  writeu,unit,j
  for k = 0, no_if-1 do begin 
    d = freqoffset[i,k] & byteorder,d,/DTOXDR  & writeu,unit,d
  endfor
  for k = 0, no_if-1 do begin 
    f  = chwidth[i,k] & byteorder,f,/FTOXDR & writeu,unit,f
  endfor
  for k = 0, no_if-1 do begin 
    f  = bandwidth[i,k]  & byteorder,f,/FTOXDR & writeu,unit,f
  endfor
  for k = 0, no_if-1 do begin 
    j  = long(sideband[i,k])  & byteorder,j,/HTONL &  writeu,unit,j
;    print,'sideband j',j
  endfor
endfor

for i = 1, 2880 - no_frqid*wbytes do begin
  j = byte(0) & writeu,unit,j
endfor


return,1
end



function fits_out,fits_file,fits_ext,source,pos,trans,band,sb,tq,pol, $
        shadow=shadow,records=records

; Shadowing needs to be fixed. 

; Write out fits uv file for specified source
;      fits_file -- name of output fits file
;      fit_ext   -- file extension
;      source    -- source name
;      pos       -- source position
;      trans     -- transition
;      band      -- spectral band
;      sb        -- sideband
;      tq        -- tuning qualifier
;      pol       -- polarization state
; keyword : shadow -- % shadowing allowed (default = 0)
;           records -- write out record data (0 no, 1 yes)
;
;
; parameters : fits_file   -- output filename
;
; result = -1 (error), 1 (ok)
;
;  
; eg. : result=fits_out('fits_test','UVF','3c454.3','','13CO(2-1)','c1','l','v01','vv')
; eg. : result=fits_out('fits_test','UVF','3c273','','HCN(3-2)','c1','l','v01','vv')
; eg. : result=fits_out('fits_test','UVF','0336-019','','HCN(1-0)','c1','l','v01','hh')
; eg. : result=fits_out('fits_test','UVF','0336-019','','HCN(1-0)','c1','l','v01','hh',/records)

; multiple sources 
; multiple bands and sidebands are permitted, but all the bands must have
; the same number of channels because of limitations in the FITS format.
; Different frequencies, channel widths, and velocities are ok.
; Only one velocity definition is permitted in FITS.
; To request multiple bands, enter a list of bands ['s0','s1','s2'], or
; use '' to get all the bands in the data set (not likely to work because
; the c bands have 1 channel whereas the s bands have more). 
; same syntax for sources and sidebands
;eg. : result=fits_out('fits_test','UVF',['3c454.3','mars'],'','', ['s0','s1'],'u','','')
; only the source, band, and sideband parameters will take a list, the other
; parameters can be used to select one choice, or all using ''

;
common global
common data_set
common plo
common fits,str_buf,unit
;
; remove embedded spaces in file and extension and capitalize
;
fits_file=strupcase(strcompress(fits_file,/remove_all))
fits_ext=strupcase(strcompress(fits_ext,/remove_all))
filename=fits_file+'.'+fits_ext
print,'Output file name ',filename

;
; calculate shortest projected baseline for requested shadowing
;
; first set up local arrays w/ the desired amp, pha for each source 
; for the data passing through the filter
;
if not keyword_set(shadow) then shadow=0.
if not keyword_set(records) then records=0
  if (records eq 1) then begin
    print,'This fits_out program does not output individual records.'
    print,'The records for each integration must be averaged together.'
    print,'You have to re-run this program without the records keyword.'
    return,-1
  endif

shadow=(shadow gt 0.0001) lt 100.

if e.campuslogin eq 'cfa' or e.campuslogin eq 'sma' or $
    e.campuslogin eq 'asiaa' then site = 'saosma'

if e.campuslogin eq 'ovro' or e.campuslogin eq 'caltech' $
    then site = 'ovromma'

case site of
	'ovromma': begin
			telescope = 'OVRO MMA'
			instrument = 'OVRO MMA'
			diameter  = 10.4
  			lat = 3.723405556d1
                        fc1=1.
		   end
	'saosma': begin
			telescope = 'SMA'
			instrument = 'SMA'
			diameter = 6.0
			lat = 19.82420526391d0 ; Pad #1
                        result=dat_list(s_l,'("band" eq "c1") and ("sb" eq "'+strtrim(sb[0],2)+'")',/reset,/no_notify)
                        if result gt 0 then begin
                           if sp[psl[0]].nch eq 1 then  fc1=sp[psl[0]].fsky*1.d9 else fc1=bl[pbl[0]].fave*1.d9
                        endif else begin
                           print,'Should include continuum band in the data selection command !'
                           print,'No data output. Quit !'
                           return,-1
                        endelse
		   end
	else:	   begin
                     print,'dont know which telescope, ovromma or saosma'
                     print,'need to include site=ovromma or site=saosma in arguments'
                     return,-1
		   end
endcase
;bmin=uti_bmin_calc(shadow,diameter)
bmin=0.

list='"wt" gt "0.0" and "nrec" ge "1"'
if (source[0] ne '') then begin
    list=list+' and ("source" eq "'+strtrim(source[0],2)+'"'
  if n_elements(source) gt 1 then begin
    for i = 1, n_elements(source)-1 do begin
        list=list+' or "source" eq "'+strtrim(source[i],2)+'"'
    endfor
  endif
  list = list + ')'
endif


if (pos ne '') then list=list+' and "pos" eq "'+strtrim(pos,2)+'"'
if (trans ne '') then list=list+' and "trans" eq "'+strtrim(trans,2)+'"'

if (band[0] ne '') then begin
  list=list+' and ("band" eq "'+strtrim(band[0],2)+'"'
  if n_elements(band) gt 1 then begin
    for i = 1, n_elements(band)-1 do begin
        list=list+' or "band" eq "'+strtrim(band[i],2)+'"'
    endfor
  endif
  list = list + ')'
endif

if (sb[0] ne '') then begin
  list=list+' and ("sb" eq "'+strtrim(sb[0],2)+'"'
   if n_elements(sb) gt 1 then begin
      for i = 1, n_elements(sb)-1 do begin
            list=list+' or  "sb" eq "'+strtrim(sb[i],2)+'"'
      endfor
   endif
  list = list + ')'
endif                                                                                  

if (tq ne '') then list=list+' and "tq" eq "'+strtrim(tq,2)+'"'
if (pol ne '') then list=list+' and "pol" eq "'+strtrim(pol,2)+'"'

if e.debug then print,'Here are the data selection criteria '
if e.debug then print,list

npts=dat_list(s_l,list,/reset,/no_notify)

if e.debug then print,npts,' data points'
if npts le 0 then begin
  print,'no data found for :',source,' ',pos,' ',trans,' ', $
        band,' ',sb,' ',tq,' ',pol
  return,-1
endif
;
; now get data for fits header
;


; See how many bands and sidebands sources antennas are in the filtered data.
junk = sp[psl].iband
distinct_iband=junk(  uniq(junk,sort(junk) ))
nbands = n_elements(distinct_iband)
print,nbands,'  bands requested:  ',c.band[distinct_iband]
  for k=0,nbands-1 do begin
    jnow = where( sp[psl].iband eq distinct_iband[k])
    print,'Band = ',c.band[distinct_iband[k]],'  # channels ',sp[psl[jnow[0]]].nch
    i = where( sp[psl[jnow]].nch ne sp[psl[jnow[0]]].nch, countm )
    if (countm gt 0) then begin
      print,'    And in addition, different spectra marked as belonging to this band'
      print,'    have different numbers of channels. Correlator setup changed?'
    endif
  endfor

; See how many receivers
junk = bl[pbl].irec
distinct_irec = junk(  uniq(junk,sort(junk) ))
no_recv = n_elements(distinct_irec)
if (no_recv gt 1) then begin
  print,'This code will not yet handle multiple receivers in the same data set.'
  print,'Filter the data to select only one receiver and make a separate'
  print,'FITS file for each receiver.'
endif

junk = bl[pbl].isb
distinct_isb=junk(  uniq(junk,sort(junk) ))
nsidebands = n_elements(distinct_isb)
print,'sidebands requested  ',c.sb[distinct_isb]

no_frqid = nsidebands
no_if = nbands
print,'There will be ',no_if,'  bands (aips IFs) in the output file'
print,'There will be ',no_frqid,' sidebands (aips FRQIDs) in the output file'

junk = in[pil].isource
distinct_isource=junk(  uniq(junk,sort(junk) ))
nsources = n_elements(distinct_isource)
print,nsources,'  sources requested:  ',c.source[distinct_isource] 

junk = [bl[pbl].itel1,bl[pbl].itel2]
distinct_itel = junk( uniq(junk,sort(junk)) )
ntels = n_elements(distinct_itel)
print,'Antennas requested  ',distinct_itel

; temporary until bug can be fixed at ovro
if (site eq 'ovromma') then ntels = 6


;arrays
rfreq=sp[psl].rfreq
fres=sp[psl].fres 
vel=sp[psl].vel 

if e.debug then $
print,'number of elements of rfreq,fres,vel,psl  ', $
n_elements(rfreq),n_elements(fres),n_elements(vel),n_elements(psl)

;scalars
nch=sp[psl[0]].nch
if nch eq 1 then rfreq=sp[psl].fsky

;vtype=c.vtype[sp[psl[0]].ivtype]
vtype='vlsr' ; turn off vtype reading for sma new data format.

if e.debug then print,'number of channels ',nch
if e.debug then print,'vtype ',vtype

rar=in[pil[0]].rar & decr=in[pil[0]].decr & souid=in(pil[0]).souid
umax=double(max(bl[pbl].u)) & vmax=double(max(bl[pbl].v)) 
wmax=double(max(bl[pbl].w)) & umin=double(min(bl[pbl].u)) 
vmin=double(min(bl[pbl].v)) & wmin=double(min(bl[pbl].w))

if e.debug then print,'rfreq,fres,nch,vel,vtype,rar,decr'
if e.debug then print,rfreq,fres,nch,vel,vtype,rar,decr
;
; check for multiple sources, nch
;
j=where(sp(psl).nch ne nch,count_nch)
if count_nch gt 0 then begin
  print,'Found different numbers of channels in the requested bands'
  print,'Cannot write a FITS file because '
  print,'each band must have the same number of channels.'
  print,'Use a filter to select a consistent set of bands.'
  print,'Check the bands and channels listed above.'
endif

;; turn off vtype reading for new sma data format.
;j=where(c.vtype[sp[psl].ivtype] ne vtype,count_vtype)
;if count_vtype gt 0 then begin
;  print,'Found more than one velocity definition in the data.'
;  print,'Cannot write a FITS file because'
;  print,'each integration must have same vel definition.'
;  print,'Use a filter to select a consistent set of integrations.'
;  junk = c.vtype[sp[psl].ivtype]
;  distinct_vtypes = junk(  uniq(junk,sort(junk) ))
;  print,'Here are the velocity types found', distinct_vtypes
;endif 

;if (count_nch gt 0 or count_vtype gt 0 or no_recv gt 1) then begin
;  print,'No FITS file written'
;  return,-1
;endif

groupid = make_array(n_elements(psl),/long)
ifid = make_array(n_elements(psl),/long)

junk = sp[psl].inhid
distinct_inhid=junk(  uniq(junk,sort(junk) ))
no_inhid = n_elements(distinct_inhid)
print,'There are ',no_inhid,' integrations to write'

no_baselines = make_array(no_inhid,/long)
for i = 0,no_inhid-1 do begin
j = where(bl[pbl].inhid eq distinct_inhid[i],count_base)
junk = bl[pbl[j]].blhid
;print,'blhids ',junk
distinct_baselines = junk(  uniq(junk,sort(junk) ))
no_baselines[i] = n_elements(distinct_baselines)
endfor

junk = bl[pbl].blhid
distinct_baselines = junk(  uniq(junk,sort(junk) ))
no_groups = n_elements(distinct_baselines)

;print,'Number of groups should be' , no_groups



for i=0L,n_elements(psl)-1 do begin
  j = where( bl[pbl[i]].blhid eq distinct_baselines,count_groups)
  if (count_groups gt 1) then begin
     print,'Mistake, should be only one match'
     print,'j ',j
     print,'bl[pbl[i]].blhid ',bl[pbl[i]].blhid
  endif
  groupid[i] = j[0]
  j = where( sp[psl[i]].iband eq distinct_iband,count_groups)
  if (count_groups gt 1) then begin
     print,'Mistake, should be only one match'
     print,'j ',j
     print,'sp[psl[i]].iband ',sp[psl[i]].iband
  endif
  ifid[i] = j[0]
  if e.debug then print,'i groupid ifid',i,groupid[i],ifid[i]
endfor



;
; dates of observations and this file in format : dd/mm/yy
; dates must be written with 2 digits for each day, month or year.
; 09/05/00 ok, but 9/5/00 will not be read correctly by aips
months = ['Jan','Feb','Mar','Apr','May','Jun','Jul', $
          'Aug','Sep','Oct','Nov','Dec']
datobs=c.ref_time[in[pil[0]].iref_time]
mon=strmid(datobs,0,3)
j=where(mon eq months,count)
if count le 0 then begin
  print,"couldn't decode UT date in data (",c.ref_time[in[pil[0]].iref_time],") !"
  return,-1
endif
num_day_obs=[fix(strtrim(strmid(datobs,8,4),2)), $
     fix(strtrim(string(j[0]+1),2)),(strtrim(strmid(datobs,4,2),2))]
day=strtrim(string(num_day_obs[2]),2) 
yr =strtrim(string(num_day_obs[0]),2)
mo =strtrim(string(num_day_obs[1]),2)
if (strlen(day) eq 1) then day = '0'+day
if (strlen(mo) eq 1) then mo = '0'+mo
;print,'mo day yr ',mo,day,yr
datobs=strtrim(yr,2)+'-'+strtrim(mo,2)+'-'+strtrim(day,2)
numdate=strtrim(string(bin_date(systime(0))),2)
;datmap=numdate[2]+'/'+numdate[1]+'/'+strmid(numdate[0],2,2)
day=strtrim(string(numdate[2]),2) 
yr =strtrim(string(numdate[0]),2)
mo =strtrim(string(numdate[1]),2)
if (strlen(day) eq 1) then day = '0'+day
if (strlen(mo) eq 1) then mo = '0'+mo
datmap=strtrim(yr,2)+'-'+strtrim(mo,2)+'-'+strtrim(day,2)
if e.debug then print,'obs and output dates    ',datobs,' ',datmap
print,'obs and output dates    ',datobs,' ',datmap
i = 0
repeat begin
    i = i + 1 & epoch = strtrim(string(in[i].epoch),2) 
endrep until (epoch gt 1900.0 or epoch lt 3000 or i ge n_elements(in.epoch)-1)

; aips allows for one reference day per file. this is chosen to be
; the first reference time in the data, same as the obsdate above.
; aips expects the reference day to start
; at 0h, but the julian day starts at 12h of the current ut day. Subtract
; 0.5 days. 
num_jdref=num_day_obs
;print,'num_jdref ',num_jdref
juldat_day0 =  uti_jul_day(num_jdref[1],num_jdref[2],num_jdref[0]) + 0.d0
;print,'juldat_day0 ',juldat_day0

vel=vel*1.d3 & rfreq=rfreq*1.d9 & fres=fres*1.d6
if (nch eq 1) then fres=abs(fres)

case 1 of
 (strmid(vtype,0,2) eq 'cz') : freq=rfreq/(1.d0+(vel/!cvel))
else : freq=rfreq*(1.d0-(vel/!cvel))
endcase
if site eq 'saosma' then begin
 freq = sp[psl].fsky*1.d9
endif
if e.debug then print,'rfreq,vel,freq=',rfreq[0],vel[0],freq[0]
;vabs=(1- (sp[psl[j[0]]].fsky/sp[psl[j[0]]].rfreq)^2)/(1+(sp[psl[j[0]]].fsky/sp[psl[j[0]]].rfreq)^2)*!cvel; absolute vel in m/s
;spvdop=vabs-vel[0]
mu0=sp[psl[j[0]]].rfreq
mur=sp[psl[j[0]]].fsky
vel0=vel[0]
vabs=(1- (mur/mu0)^2)/(1+(mur/mu0)^2)*!cvel; absolute vel in m/s
spvdop=vabs-vel0 ; doppler velocity from the observer to the LSR
vref=(-!cvel/mu0*(mur-mu0)-spvdop)/(1-spvdop/!cvel)
;print,'Reference velocity in m/s:',vref
;print,'Absolute velocity in m/s:',vabs
;print,'Doppler velocity in m/s:',spvdop
;print,'Vlsr in m/s:',vel[0]

;print,'number of elements of rfreq,fres,vel,psl  ', $
;n_elements(rfreq),n_elements(fres),n_elements(vel),n_elements(psl)

case 1 of
 (vtype eq 'vhel') : fitvel=258
 (vtype eq 'cz')   : fitvel=2
else : fitvel=257
endcase

ra50=rar*180./!DPI & dec50=decr*180./!DPI

if (nch eq 1) then begin
   refch=1.0
endif else begin
   refch=(float(nch)+1.0)/2.0
endelse

if (abs(umin) gt abs(umax)) then umax = double(abs(umin))
if (abs(vmin) gt abs(vmax)) then vmax = double(abs(vmin))
if (abs(wmin) gt abs(wmax)) then wmax = double(abs(wmin))
if e.debug then print,'umax,vmax,wmax :',umax,vmax,wmax

; calculate theoretical noise in map:

case site of
    'ovromma' : begin
        noise=1.0d0/sqrt(total(sp[psl].wt*abs(fres[psl])))
        case 1 of
                (freq[0] gt 60.d9 and freq[0] lt 150.d9)  : eff_con=37.3d0
                (freq[0] gt 150.d9 and freq[0] lt 300.d9) : eff_con=65.3d0
        else  : begin
                 eff_con=26.1d0
                 print," don't know aperture efficiency at this", $
                 " frequency; assume 100%"
        endelse
        endcase

        if e.debug then print,'telescope efficiency constant :',eff_con
        noise=noise*eff_con
        print,npts,noise*1.e3,format='("theoretical rms noise in map for",i5,'+ $
            '" visibilities is ",f8.2," mJy/beam")'

        wt=sp[psl].wt*abs(fres[psl])/eff_con^2
    end
	'saosma' : wt=sp[psl].wt 
endcase

wtmax = max(wt)

; now find max data values for scaling
;
; worry about magic values later

dat_get_rows,cmp,amp,pha,x,wts,first,npts,'channel',0,/list, $
             order='chan',average='int'
if ((band[0] eq 'c1' or band[0] eq 'c2') and (not records)) then begin
   amax = double(max([bl[pbl].ampave*cos(bl[pbl].phaave*!DPI/180.), $
                      bl[pbl].ampave*sin(bl[pbl].phaave*!DPI/180.)]))
endif else begin
   amax = double(max([float(cmp),imaginary(cmp)]))
endelse

if n_elements(first) ne n_elements(psl) then begin
   print,'Mistake. Should have the same number of spectra from dat_get_rows'
   print,'as from the list'
   print,'dat_get_rows ',n_elements(first)
   print,'list         ',n_elements(psl)
   return,-1
endif


;for i = 0,nch-1 do begin
;print,'i real imag ',i,float(cmp[i]),imaginary(cmp[i])
;endfor

if e.debug then print,'max data value =',amax
if fits_init(filename) ne 1 then return,-1


; calculate scaling factors to fit into I*4

      uvwscl = double(max([umax,vmax,wmax])) * (freq[0]/fc1) * 1.d3 / 1.d9
      bscale = double(amax) / 1.d9
      wtscal = wtmax / amax

;print,'umax,vmax,wmax ',umax,vmax,wmax
;print,'wtmax,amax ',wtmax,amax
;print,' scale factors for uv b and wt ',uvwscl,bscale,wtscal
;
; write FITS header:
;
; added a bands axis for multiple IF's or spectral bands.
; at the moment, stokes is set to 1
nstokes = 1
pcount = 8
;print,'source ', source,nch

result=fits_wlog ('SIMPLE','T','Standard UVFITS')
result=fits_wint ('BITPIX',32,' ')
result=fits_wint ('NAXIS',7,' ')
result=fits_wint ('NAXIS1',0,'No standard image just group')
result=fits_wint ('NAXIS2',3,' ')
result=fits_wint ('NAXIS3',nstokes,' ')
result=fits_wint ('NAXIS4',nch ,' ')
result=fits_wint ('NAXIS5',no_if,' ')
result=fits_wint ('NAXIS6',1,' ')
result=fits_wint ('NAXIS7',1,' ')
result=fits_wlog ('EXTEND','T','This is the antenna file')
result=fits_wlog ('GROUPS','T','Groups format')
result=fits_wint ('PCOUNT',pcount,'The number of parameters in each group')

nvisibilities=long(total(sp(psl).nch))
nrec_total=n_elements(psl)
nrec_total = nrec_total/no_if/nstokes
result=fits_wint ('GCOUNT',nrec_total,'Number of visibility groups')
if e.debug then $
print,'The number of groups and the number of visibilities  ',nrec_total,nvisibilities

if no_groups ne nrec_total then begin
  print,'Mistake no_groups ne nrec_total', no_groups,nrec_total
  return,-1
endif

result=fits_wstr ('OBJECT',source[0],'Source name')
result=fits_wstr ('TELESCOP',telescope,' ')
result=fits_wstr ('INSTRUME',instrument,' ')
result=fits_wstr ('OBSERVER',strupcase(e.user_name),' ')
;print,'date obs ',datobs
result=fits_wstr ('DATE-OBS',datobs,' ')
;print,'date map ',datmap
result=fits_wstr ('DATE-MAP',datmap,' ')
result=fits_wdble('EPOCH',epoch,'Epoch of RA, Dec')
result=fits_wstr ('BUNIT','JY',' ')
result=fits_wdble('BSCALE',float(bscale),'REAL = TAPE * BSCALE + BZERO')
result=fits_wdble('BZERO',0.0,' ')
result=fits_wint ('VELREF',fitvel,'>256 RADIO, 1 LSR 2 HEL 3 OBS')
result=fits_wdble('ALTRPIX',refch,'ALTERNATE FREQ/VEL REF PIXEL')
;result=fits_wdble('ALTRVAL',vel[0],'ALTERNATE FREQ/VEL REF VALUE')
;result=fits_wdble('ALTRVAL',vabs,'ALTERNATE FREQ/VEL REF VALUE')
result=fits_wdble('ALTRVAL',vref,'ALTERNATE FREQ/VEL REF VALUE')
;result=fits_wdble('ALTRPIX',1.0,'ALTERNATE FREQ/VEL REF PIXEL')
;result=fits_wdble('ALTRVAL',1.2818501e7,'ALTERNATE FREQ/VEL REF VALUE')
result=fits_wdble('OBSRA',ra50,'ANTENNA POINTING RA')
result=fits_wdble('OBSDEC',dec50,'ANTENNA POINTING DEC')
result=fits_wdble('RESTFREQ',rfreq[0],'REST FREQUENCY')
;result=fits_wstr ('SPECSYS','LSRK',' ')
;result=fits_wreal('BLANK',MAGIC_COM,' ')

; FITS coordinate parameters:

result=fits_wstr ('CTYPE2','COMPLEX','1=real, 2=imag, 3=weight')
result=fits_wdble('CRVAL2',1.0,' ')
result=fits_wdble('CDELT2',1.0,' ')
result=fits_wdble('CRPIX2',1.0,' ')
result=fits_wdble('CROTA2',0.0,' ')
result=fits_wstr ('CTYPE3','STOKES',' ') ; -1=RR, -2=LL, -3=RL, -4=LR
result=fits_wdble('CRVAL3',-1.0,' ')
result=fits_wdble('CDELT3',-1.0,' ')
result=fits_wdble('CRPIX3',1.0,' ')
result=fits_wdble('CROTA3',0.0,' ')
result=fits_wstr ('CTYPE4','FREQ','Frequency, Hz')
result=fits_wdble('CRVAL4',freq[0],' ')
result=fits_wdble('CDELT4',fres[0],' ')
result=fits_wdble('CRPIX4',refch,' ')
result=fits_wdble('CROTA4',0.0,' ')
result=fits_wstr ('CTYPE5','IF','IF band or spectrum')
result=fits_wdble('CRVAL5',1.0,' ')
result=fits_wdble('CDELT5',1.0,' ')
result=fits_wdble('CRPIX5',1.0,' ')
result=fits_wdble('CROTA5',0.0,' ')
result=fits_wstr ('CTYPE6','RA','Right ascension, degrees')
result=fits_wdble('CRVAL6',ra50,' ')
;result=fits_wdble('CDVAL6',0.0,' ')
result=fits_wdble('CDELT6',1.0,' ')
result=fits_wdble('CRPIX6',1.0,' ')
result=fits_wdble('CROTA6',0.0,' ')
result=fits_wstr ('CTYPE7','DEC','Declination, degrees')
result=fits_wdble('CRVAL7',dec50,' ')
;result=fits_wdble('CRVAL7',0.0,' ')
result=fits_wdble('CDELT7',1.,' ')
result=fits_wdble('CRPIX7',1.0,' ')
result=fits_wdble('CROTA7',0.0,' ') 
;
;
result=fits_wstr ('PTYPE1','UU---SIN','baseline u projection, seconds')
result=fits_wdble('PSCAL1',float(uvwscl/fc1),' ')
result=fits_wdble('PZERO1',0.0,' ')
result=fits_wstr ('PTYPE2','VV---SIN','baseline v projection, seconds')
result=fits_wdble('PSCAL2',float(uvwscl/fc1),' ')
result=fits_wdble('PZERO2',0.0,' ')
result=fits_wstr ('PTYPE3','WW---SIN','baseline w projection, seconds')
result=fits_wdble('PSCAL3',float(uvwscl/fc1),' ')
result=fits_wdble('PZERO3',0.0,' ')
result=fits_wstr ('PTYPE4','BASELINE','256*ANT1 + ANT2 + (array-1)/100')
result=fits_wdble('PSCAL4',0.01,' ')
result=fits_wdble('PZERO4',0.0,' ')
result=fits_wstr ('PTYPE5','DATE','Julian date part 1')
result=fits_wdble('PSCAL5',1.0d0/24.d0,'days/24 (hr)')
result=fits_wdble('PZERO5',juldat_day0-0.5,' ')
result=fits_wstr ('PTYPE6','DATE','Julian date part 2')
result=fits_wdble('PSCAL6',1.d0/8640000.d0,'days/8640000 (.001 sec)')
result=fits_wdble('PZERO6',0.d0,' ')
result=fits_wstr ('PTYPE7','SOURCE','Integer code for source')
result=fits_wdble('PSCAL7',1.0,' ')
result=fits_wdble('PZERO7',0.0,' ')
result=fits_wstr ('PTYPE8','FREQSEL','Integer code for frequency setup')
result=fits_wdble('PSCAL8',1.0,' ')
result=fits_wdble('PZERO8',0.0,' ')
;
; additional FITS history:
;
dash='/---------------------------------------------'
result=fits_wcmt ('HISTORY',dash)
result=fits_wstr ('DATE',datmap,'Fits file creation date ')
result=fits_wcmt ('HISTORY',"AIPS   SORT ORDER = 'TB'")
result=fits_wcmt ('HISTORY','- Output FITS file: '+filename)
str1=string(umax,format='("- Umax = ",f10.3)')
str2=string(vmax,format='(" Vmax = ",f10.3)')
str3=string(wmax,format='(" Wmax = ",f10.3," klambda")')
result=fits_wcmt ('HISTORY',str1+str2+str3)
str1=string(amax,format='("- Maximum real or imaginary record = ",e12.6)')
result=fits_wcmt ('HISTORY',str1)
str1=string(nvisibilities,format='("- Number of visibilities:",i8)')
result=fits_wcmt ('HISTORY',str1)
str1=string(shadow,format='("- Allowed shadowing (%) = ",f5.1)')
result=fits_wcmt ('HISTORY',str1)
str1=string(wtscal,format='("AIPS WTSCAL = ",e17.10)')
result=fits_wcmt ('HISTORY',str1)
result=fits_wcmt ('HISTORY',dash)
result=fits_wend ()
;
; now get the calibrated data to write out to FITS:
;

; there might be different reference days in the same mir data. this
; section decodes the reference date for each integration and calculates
; the number of hours since the first reference day.

refdat=c.ref_time[in[pil].iref_time] 
hrdiff = make_array(n_elements(refdat),/float)
for i=0L,n_elements(refdat)-1 do begin
  refmn = where(strmid(refdat[i],0,3) eq months,count) + 1
  if (count ne 1) then begin
    print,"couldn't decode UT date in data (",c.ref_time[in[pil[0]].iref_time],") !"
    return,-1
  endif
  refyr = fix(strtrim(strmid(refdat[i],8,4),2))
  refdy = fix(strtrim(strmid(refdat[i],4,2),2))
;print,'ref ',refyr,refmn[0],refdy
  hrdiff[i] = 24.0 * (uti_jul_day(refmn[0],refdy,refyr) - juldat_day0)
endfor

;daydiff=uti_jul_day(num_day_obs[1],num_day_obs[2],num_day_obs[0])- $
;        uti_jul_day(num_jdref[1],num_jdref[2],num_jdref[0])
;hrdiff=24.d0*daydiff

;print,'hrdiff ',hrdiff

; hrdiff is the number of hours from the current reference time to the first
; reference time. add to this the number of hours from the current reference
; time to the beginning of the integration, and the number of seconds to the
; beginning of the current record. convert this to days.
;
;
;juldat_sec=3600.d0*(bl[pbl].avedhrs+hrdiff)-in[pil].rinteg/2. 
juldat_days=(bl[pbl].avedhrs-in[pil].rinteg/7200.d0) /24.d0

; scale jd1 by 2 and jd2 by 86400
jd1 = long(hrdiff)
jd2 = long(8640000. * juldat_days)
;print,'juldat ',juldat_days

bands=c.band(sp[psl].iband)

; Turn off SOID reading for new sma data format
;junk = bl[pbl].soid
;distinct_soids=junk(  uniq(junk,sort(junk) ))
;ndistinct = n_elements(distinct_soids)
;if e.debug then print,ndistinct,' configurations'
distinct_soids=0
ndistinct=1

posx=make_array(ndistinct,ntels,/double)
posy=make_array(ndistinct,ntels,/double)
posz=make_array(ndistinct,ntels,/double)
te=make_array(ndistinct,ntels,/double)
tn=make_array(ndistinct,ntels,/double)
tu=make_array(ndistinct,ntels,/double)
dte=make_array(ndistinct,ntels,/double)
dtn=make_array(ndistinct,ntels,/double)
dtu=make_array(ndistinct,ntels,/double)
telname=make_array(ndistinct,ntels,/string)
telnum =make_array(ndistinct,ntels,/long)
;
; if the data are shadowed more than that specified by fit_shadow,
; the weight is set to negative -- this is complicated because
; one must look for baselines which involve telescopes blocked
; on shorter baselines
;
shad_tel=-1
shad_inhid=0
j=where(bl[pbl].prbl lt bmin,count_shadow)
if count_shadow gt 0 then begin
  count_shadow=0
  jw_neg=where(bl[pbl[j]].w lt 0.,count_neg)
  jw_pos=where(bl[pbl[j]].w gt 0.,count_pos)
  if count_neg gt 0 then begin
     shad_tel=[shad_tel,bl[j[jw_neg]].itel1]
     shad_inhid=[shad_inhid,in[pil[[j[jw_neg]]]].inhid]
  endif 
  if count_pos gt 0 then begin
     shad_tel=[shad_tel,bl[j[jw_pos]].itel1]
     shad_inhid=[shad_inhid,in[pil[[j[jw_pos]]]].inhid]
  endif
  if n_elements(shad_tel) gt 1 then begin
    for i=1,n_elements(shad_tel)-1 do begin
      j=where(in[pil].inhid eq shad_inhid[i])
      js=j(where(bl[pbl[j]].itel1 eq shad_tel[i] or  $
                 bl[pbl[j]].itel2 eq shad_tel[i],count))
      wt[js]=-abs(wt[js])
      count_shadow=count_shadow+count
    endfor
  endif 
endif
print,count_shadow,' spectrometer bands removed due to shadowing'
print,'The following can be very slow if there is a lot of data. Patience...'

; in db the time variables are :
;
; toff_rec = sec from beginning (i think) ut to beginning of record
; integ_rec = sec in record
; ut = beginning ut of integration
; utave = ave ut of data on baseline
; integ = total sec on the baseline
; rinteg = max sec obtained for any data in this integration
;
; in mir (idl) the variables are :
;
; avedhrs=convert(float,datediff(ms,th.ref_time,utave))/3.6e+6
; toff = sec from beginning ut to middle of record (ie. toff+integ/2)
; integ = sec in record
; ut = beginning ut of integration
; utave = ave ut of baseline
; rinteg = max integ time obtained on this integration
;
idata=make_array(no_groups*(pcount + 3L*nch*no_if *nstokes),/long)
if e.debug then print,'size of idata ',n_elements(idata)
;
; set up index array
;  p_0 such that p_0[i] is the index in idata for the 
;         first element of random group data for the i-1 'th record (group)
;
; 
p_0=make_array(nrec_total,/long)
soids=make_array(nrec_total,/long)

p_0 = (3L*nch*no_if *nstokes+pcount) * lindgen(nrec_total)
if e.debug then print,'p_0 ',p_0

for i=0L,n_elements(psl)-1 do begin

; fill in u,v,w,jd1 and jd2 at least once per group

idata[p_0[groupid[i]]+0L]    = long(bl[pbl[i]].u * 1.e3 / uvwscl)
idata[p_0[groupid[i]]+1L] = long(bl[pbl[i]].v * 1.e3 / uvwscl)
idata[p_0[groupid[i]]+2L] = long(bl[pbl[i]].w * 1.e3 / uvwscl)
; 4th random group parameter for antennas is filled in later
idata[p_0[groupid[i]]+4L] = jd1[i]
idata[p_0[groupid[i]]+5L] = jd2[i]+in[pil[i]].rinteg/2.
;idata[p_0[groupid[i]]+6L] = long(in[pil[i]].isource)
idata[p_0[groupid[i]]+6L] = long(1)
if n_elements(sb) eq 1 then idata[p_0[groupid[i]]+7L] =1L else $
 idata[p_0[groupid[i]]+7L] = long(bl[pbl[i]].isb+1)

; stokes is always 1 at the moment so there is no stokes
; stuff in the following
 
for k=0,nch-1 do begin
   idata[p_0[groupid[i]]+pcount +3L*(k+ ifid[i]*nch)] = float(cmp[first[i]+k])/bscale
   idata[p_0[groupid[i]]+pcount+1L +3L*(k+ ifid[i]*nch)] = imaginary(cmp[first[i]+k])/bscale
   idata[p_0[groupid[i]]+pcount+2L+3L*(k+ ifid[i]*nch)] = long(wt[i] / (wtscal*bscale))
;print,'i,k,g,if ',i,k,groupid[i],ifid[i],p_0[groupid[i]]+pcount +3L*(k+ ifid[i]*nch)
endfor
 
; replace ch 0 w/ integration average continuum unless continuum records
;if nch gt 1 or not records then begin
;i=0
;   idata[p_0+pcount +3L*(i+ j*nch + k*no_if *nch)] = $
;        bl[pbl[js]].ampave*cos(bl[pbl[js]].phaave/!radeg)/bscale
;   idata[p_0+pcount+1L +3L*(i+ j*nch + k*no_if *nch)] = $
;        bl[pbl[js]].ampave*sin(bl[pbl[js]].phaave/!radeg)/bscale
;   idata[p_0+pcount+2L+3L*(i+ j*nch + k*no_if *nch)] = $
;        long(wt[js] / (wtscal*bscale))
;endif
 
;endfor
;endfor 

endfor

defd = intarr(ntels)
;print,'n_elements(defd)',n_elements(defd)

; get telescope positions for all configurations
itel1=fix(c.tel1(bl[pbl].itel1))
itel2=fix(c.tel2(bl[pbl].itel2))
;itel1 is now the integer version of the string c.tel1

  for icon=0,n_elements(distinct_soids)-1 do begin
;    jss=where(bl[pbl].soid eq distinct_soids[icon],ndat)
     jss=lindgen(n_elements(pbl)) ;SOID no long exist in new sma data format 	
;print,'distinct_soids ',distinct_soids[icon]
;print,'jss  ',jss

case site of
'ovromma' : begin
    if dbi_soid_read(distinct_soids[icon],solution) ne 1 then return,-1
    jtel=indgen(n_elements(solution))
    te[icon,jtel]=solution[jtel].te
    tn[icon,jtel]=solution[jtel].tn
    tu[icon,jtel]=solution[jtel].tu
    dte[icon,jtel]=solution[jtel].dte
    dtn[icon,jtel]=solution[jtel].dtn
    dtu[icon,jtel]=solution[jtel].dtu
    telname(icon,jtel)='AN'+strtrim(string(jtel+1),2)
    telnum (icon,jtel)=jtel+1

end

'saosma' : begin
; alltels is a long list of each antenna in each baseline this configuration
    alltels = [    bl[pbl].itel1,    bl[pbl].itel2    ]
;    print,'alltels ',alltels,' in array ',icon
; make a short list of the distinct antennas in this configuration
    distinct_tels=alltels(  uniq(alltels,sort(alltels) ))
    ndistinct_tels = n_elements(distinct_tels)
    jtel=indgen(n_elements(distinct_tels))
;    print,'ndistinct_tels ',ndistinct_tels
;    print,'distinct_tels ',distinct_tels
; The postion of the antennas is undefined except for the first in the list
    defd(*) = 0
    defd[0] = 1
    te[icon,0] = 0.;
    tn[icon,0] = 0.;
    tu[icon,0] = 0.;
    dte[icon,*] = 0.;
    dtn[icon,*] = 0.;
    dtu[icon,*] = 0.;

; loop over pairs of antennas and define the positions using a baseline and
; a known antenna position
    for ii=1,ndistinct_tels-1 do begin
    for jj=0,ii-1 do begin

;      print,'loop indices ',ii,jj,'  defined ',defd[ii],'  ',defd[jj]
      if (  (defd[ii] + defd[jj])  eq 1) then begin

;	print,'look for ', distinct_tels[ii],' with   ',distinct_tels[jj]

        jd1 = where( ((c.tel1[bl[pbl].itel1] eq distinct_tels[ii] $
		and c.tel2[bl[pbl].itel2] eq distinct_tels[jj]) or $
		(c.tel1[bl[pbl].itel1] eq distinct_tels[jj] $
		and c.tel2[bl[pbl].itel2] eq distinct_tels[ii]))  $
		,count)
;        print,'count1 ',count
	if (count ne 0) then begin

;	    print,'jd1 ',jd1
;	    print,'jd1[0],j[jd1[0]] ',jd1[0],j[jd1[0]]
;            print,'blcd ',c.blcd[bl[j[jd1[0]]].iblcd]
;	    print,'c.tel1 ',c.tel1[bl[j[jd1[0]]].itel1] 
;	    print,'c.tel2 ',c.tel2[bl[j[jd1[0]]].itel2] 
;            print,'ble,bln,blu ',bl[j[jd1[0]]].ble,bl[j[jd1[0]]].bln,bl[j[jd1[0]]].blu 

            if (defd(jj) eq 0) then begin
		te[icon,jj] = te[icon,ii] + bl[pbl[jd1[0]]].ble
		tn[icon,jj] = tn[icon,ii] + bl[pbl[jd1[0]]].bln
		tu[icon,jj] = tu[icon,ii] + bl[pbl[jd1[0]]].blu
                defd[jj] = 1
	    endif else begin
		te[icon,ii] = te[icon,jj] - bl[pbl[jd1[0]]].ble
		tn[icon,ii] = tn[icon,jj] - bl[pbl[jd1[0]]].bln
		tu[icon,ii] = tu[icon,jj] - bl[pbl[jd1[0]]].blu
                defd[ii] = 1
            endelse

;	    print,'east ',ii,'  ',te[icon,ii],'  ',jj,'  ',te[icon,jj]

	endif 

      endif

    endfor
    endfor

    for jj=0,ndistinct_tels-1 do begin
    if (distinct_tels[jj] lt 10) then begin
        telname(icon,[jj])='AN0'+strtrim(string(distinct_tels[jj]),2)
    endif else begin
        telname(icon,jj)='AN'+strtrim(string(distinct_tels[jj]),2)
    endelse
    endfor
    telnum(icon,jtel)=distinct_tels(jtel)

end

endcase


; compute geocentric coordinates from local coordinates
    posx(icon,jtel) = (tu[icon,jtel] + dtu[icon,jtel]*1.d-3) * cos(lat*!DPI/180.) $
       - (tn[icon,jtel] + dtn[icon,jtel]*1.d-3) * sin(lat*!DPI/180.)
    posy(icon,jtel) = te[icon,jtel] + dte[icon,jtel]*1.d-3
    posz(icon,jtel) = (tu[icon,jtel] + dtu[icon,jtel]*1.d-3) * sin(lat*!DPI/180.) $
       + (tn[icon,jtel] + dtn[icon,jtel]*1.d-3) * cos(lat*!DPI/180.)


; get telescope numbers for the FITS baseline id

    idata[p_0[groupid[jss]]+3L]=(256L*itel1[jss] + itel2[jss])*100L + icon

  endfor
; this is the end of the for icon=0,n_elements(distinct_soids)-1 do begin loop



; Write the fits data
result=fits_wrayi(idata,/flush) & idata=0

print,'distinct_tels  ',distinct_tels

; write FITS-AIPS antenna tables:
;
for icon=0,n_elements(distinct_soids)-1 do begin
   stx0=reform(posx[icon,*]) & sty0=reform(posy[icon,*]) & stz0=reform(posz[icon,*])
   stname0=reform(telname[icon,*])
   stnum0 =reform( telnum[icon,*])
   jj=where(fix(stnum0) eq 1) 
   if jj lt 0 then begin
      stname='AN01' 
      stnum=['1']
      stx=9999
      sty=9999
      stz=9999
   endif else begin
      stname=stname0[0]
      stnum=stnum0[0]
      stx=stx0[0]
      sty=sty0[0]
      stz=stz0[0]
   endelse
   for ii=1l, 8l do begin
      jj=where(fix(stnum0) eq (ii+1))
      if jj[0] lt 0 then begin
         stname=[stname,'AN0'+strcompress(string(ii+1),/remove_all)]
         stnum=[stnum, strcompress(string(ii+1),/remove_all)]
         stx=[stx,9999]
         sty=[sty,9999]
         stz=[stz,9999]
      endif else begin
         stname=[stname,stname0[jj]]
         stnum=[stnum,stnum0[jj]]
         stx=[stx,stx0[jj]]
         sty=[sty,sty0[jj]]
         stz=[stz,stz0[jj]]
      endelse        
   endfor
;  result=fits_want_ascii(n_elements(stx),icon,stname,stx,sty,stz,site)
   result=fits_want(n_elements(stx),icon,stnum,stname,stx,sty,stz,freq[0],datobs,site)
endfor

; finished with antenna table
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; this section for the frequency table

;there are nbands*nsidebands different frequency setups to be
;recorded. Find one example of
;each of, get its index, use that
;index to point to the header variables needed for the frequency
;table. 

freqoffset = make_array(no_frqid,no_if ,/DOUBLE)
chwidth =  make_array(no_frqid,no_if ,/FLOAT)
bandwidth =  make_array(no_frqid,no_if ,/FLOAT)
sideband =  make_array(no_frqid,no_if ,/INT)


if (no_frqid gt 99) then begin
    print,'cannot write more than 99 FRQID with this program'
    print,'exiting without completing FITS file'
    return, -1
endif
if (no_if gt 99) then begin
    print,'cannot write more than 99 IF with this program'
    print,'exiting without completing FITS file'
    return, -1
endif

for i = 0,nsidebands-1 do begin
  m = where(distinct_isb[i] eq bl[pbl].isb)
for j = 0,nbands-1 do begin
;  k = where(distinct_iband[j] eq sp[psl].iband and bl[m[0]].blhid eq sp[psl].blhid)  
k = where(distinct_iband[j] eq sp[psl].iband and distinct_isb[i] eq bl[pbl].isb)
;  print,'m0 sb requested sb found  ',m[0],'   ', $
;    c.sb[distinct_isb[i]],'  ', c.sb[bl[pbl[m[0]]].isb]
;  print,'m0 band requested band found ', m[0], '    ' ,$
;    c.band[distinct_iband[j]],'  ', c.band[sp[psl[k[0]]].iband]
  freqoffset[i,j] = freq[k[0]] - freq[0]
;  print,'freqoffset= ',freqoffset[i,j]
  chwidth[i,j] =  fres[m[0]]
  bandwidth[i,j] = nch * chwidth[i,j] 
  sideband[i,j] = -1
  if c.sb[  bl[pbl[m[0]]].isb   ] eq 'u' then sideband[i,j] = 1
;  print,'sideband ',c.sb[  bl[pbl[m[0]]].isb   ],sideband[i,j]
endfor
endfor

if e.debug then begin
print, 'freqoffset ',freqoffset 
print, 'chwidth    ',chwidth
print, 'bandwidth  ',bandwidth
print, 'sideband   ',sideband 
endif

result = fits_fq(no_frqid,no_if,freqoffset,chwidth,bandwidth,sideband)

; finished with frequency table
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; this section for source table. There are more entries in the
; source table than are filled in here. 

; The frequency information in the source table is filled in
; the same for each IF band, and only the minimum required is filled in.
; For example, there can be different velocities for each source, but 
; all IF bands for each source will have the same velocity.


source = make_array(nsources,/STRING)
suid = make_array(nsources,/INT)
qual = make_array(nsources,/INT)
calcode = make_array(nsources,/STRING)
iflux = make_array(nsources,/FLOAT)
raepo = make_array(nsources,/DOUBLE)
decepo = make_array(nsources,/DOUBLE)
epoch = make_array(nsources,/DOUBLE)
lsrvel = make_array(nsources,/DOUBLE)
restfreq = make_array(nsources,/DOUBLE)

calcode[*] = '   '

for i = 0,nsources-1 do begin

  j = where(in[pil].isource eq distinct_isource[i])
  source[i]  = strmid(c.source[ in[pil[j[0]]].isource ],0,16)
;  suid[i] = in[pil[j[0]]].isource
  suid[i]= 1

;;print,suid[i],source[i]

;  qual[i] = in[pil[j[0]]].itq
  qual[i] = 0 ; No itq in new sma data format

;  if (  c.aq[ bl[pbl[j[0]]].iaq] ne ' ' or $
  if ( c.pq[ sp[psl[j[0]]].ipq] ne ' ' or $
        c.gq[ sp[psl[j[0]]].igq] ne ' ' )  then calcode[i] = 'A'

  iflux[i] = in[pil[j[0]]].sflux
  raepo[i] = in[pil[j[0]]].rar * 180./!DPI
  decepo[i] = in[pil[j[0]]].decr * 180./!DPI
  epoch[i] = in[pil[j[0]]].epoch
  lsrvel[i] = sp[psl[j[0]]].vel * 1.e3
  restfreq[i] = sp[psl[j[0]]].rfreq * 1.e9

if e.debug then print,source[i],'  ',calcode[i],'  ', $
    iflux[i],raepo[i],decepo[i],epoch[i], $
    lsrvel[i],restfreq[i]

endfor

veldef = 'RADIO'
if (strmid(vtype,0,2) eq 'cz') then veldef = 'OPTICAL'

;;print,veldef,'  ',veldef

;veltyp = 'TOPOCENT'
;if (c.vtype[sp[psl[0]].ivtype] eq 'vlsr') then veltyp = 'LSR'
;if (c.vtype[sp[psl[0]].ivtype] eq 'vhel') then veltyp = 'BARYCENT'
;if (c.vtype[sp[psl[0]].ivtype] eq 'vrad') then veltyp = 'GEOCENTR'
veltyp = 'LSR' ; new data format

;;print,veltyp,'  ',veltyp

result = fits_su(no_if,nsources,source,suid,qual,calcode,iflux, $
    raepo,decepo,epoch,lsrvel,restfreq,veldef,veltyp)


; close output file:

close,unit
free_lun,unit

;aa={TIME:float(1),TIME_INTERVAL:float(1),SOURCE_ID:long(1),SUBARRAY:long(1),START_VIS:long(1),END_VIS:long(1),FREQ_ID:long(1)}
;aa=replicate(aa,2)
;mwruvfits,aa,filename,extname='AIPS NX ',unit=['DAYS','DAYS',' ',' ',' ',' ',' ']

aa={TIME:float(1),TIME_INTERVAL:float(1),SOURCE_ID:long(1),ANTENNA_NO:long(1),SUBARRAY:long(1),FREQ_ID:long(1),TSYS_1:float(1),TANT_1:float(1)}
;mwruvfits,aa,filename,extname='AIPS TY ',unit=['DAYS','DAYS',' ',' ',' ',' ','KELVINS ','KELVINS']

aa={TIME:double(1),TIME_INTERVAL:float(1),ANTENNA_NUMBER:long(1),TEMPERATURE:float(1),PRESSURE:float(1),DEWPOINT:float(1),WIND_VELOCITY:float(1),WIND_DIRECTION:float(1),H2O_COLUMN:float(1),ELECTRON_COLUMN:float(1)}
;mwruvfits,aa,filename,extname='AIPS WX ',unit=['DAYS','DAYS',' ','CENTIGRA','MILLIBAR','CENTIGRA','M/SEC ','DEGREES',' ',' ']

return,1
end


