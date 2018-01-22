pro pro_idl
;
; idl programming notes and guidelines:
; ***************************************************************
; Programming guidelines :
;
; 1) functions vs procedures :
;    make most routines functions rather than procedures
;    (can then return an error code in return)
;    eg. return,1 (ok) or return,-1 (error)
;
; 2) routine names :
;    try to make routine names 3letters_4letters.pro
;
;    Routines are named with prefixes describing broad categories :
; cal_ : Calibration
; dat_ : Data manipulation for idl mir structures  
; dbi_ : Database interaction
; fil_ : Host file manipulation
; plo_ : Plotting 
; pri_ : Printing 
; pro_ : Programming 
; uti_ : Small utility programs 
; wlm_ : Water Line Monitor programs
;
; 3) comments at beginning (used by pro_hlp and pro_lkf routines) :
;    ;
;    ; brief 1 line description
;    ; follow with other comments which will appear when pro_hlp is run.
;    ; eg. : example call to routine
;    ; end w/ blank comment line followed by blank line
;
; 4) parameter passing :
;    There are 2 types of idl parameters : normal and keyword
;
;    normal : specified in .pro file simply as : ,par
;             must be specified in calling
;    keyword : specified in .pro file as : ,par=par1
;              within the given .pro file refer to the variable as par1
;              keyword parameters are optional when calling the function
;              within the given .pro file use 'if keyword_set(par1)' to
;              check if it was specified.
;
; 5) When you call a routine, the parameters for functions
;    with return results must be in parenthesis --
;    result=function(par1,/par2) or 
;    result=function(par1,par2='ddd')
; 
; 6) Global parameters and the current data are usually passed
;    in the common blocks : global and data_set (see start_mir.pro).
;    A routine can access the variables in the structures in 
;    each common if the statements below are included (after
;    removing the column 1 ';') --
;
;    common global
;    common data_set 
;    (idl matches common blocks by variable order and it is
;     in principle possible to use different variable names
;     in different routines, but this is NOT advised if we
;     are to be able to follow the code throughout many .pro's)
; 
;  7) IDL will not eventually not support () as a way to 
;      address variables, use [].
; 
; ****************************************************************
; Help and Lookfor routines
; 
; pro_hlp prints help comments from beginning of .pro files 
; pro_lkf looks for a routine with a keyword in its first 
;     comment line
; help on a variable  : pro_hlp,variable='variable_name'
; help on a structure : pro_hlp,structure='structure_name'
; help on program (print this file) : pro_hlp,/program
; 
; *****************************************************************
; Profile
;
;  idl routine pro_profile will generate a timing profile for all 
;  statements in an idl .pro file
; 
; *****************************************************************
; Common blocks
;
; Common blocks are used to pass large quantities of data or
; shared variables between program units. They are also needed in
; instances in which you need to have a variable value retained
; between separate calls to a routine.
; The major common blocks in mir are : 
;   global   -- includes environment and mir operational parameters
;   data_set -- structures and arrays which access a particular data
;               set (usually a track)
;   wlm      -- Arrays and variables with wlm data. 
; 
; In order that the variables in these commons be accessible to
; the idl main level, common references must be put in the 
; idl_startup file.
; 
; ******************************************************************
; Data structure
;
; The astronomical data sets are arranged in a tree structure. For
; a given track, there are three levels to this tree, corresponding to
; header information specific to integrations, baselines, and spectra.
; The three levels are contained in strucutres : in, bl, and sp.
; This tree structure makes for compact data storage (repeating non-
; changing header entries as few times as possible, yet retaining
; programming ease to link the in, bl and sp headers for a complete
; description of the headers for a specific spectral band. At each 
; level (in, bl, or sp), the number of rows increases by a factor 
; which depends on the number of active telescopes for each integration 
; and the number of spectral bands observed on each baseline.
;
; e.g. the in is the smallest, sp the largest
;
; A second feature of the idl_mir data structure is that all character
; header data (eg. source name or gq the gain qualifier) are translated 
; to 2 byte integer codes (eg. isource or igq) in the structures 
; in, bl, and sp. The actual character strings corresponding to each
; of these integer codes are strored in structure c (eg. c.source).
; This was done to avoid the storage of repeating character strings
; which require a lot of memory.
;
; Each structure has multiple rows for each variable, eg in(0).int
; is the first integration number and in(9).int is the 10'th. The 
; complete list of integrations is simply in.int.
;
; You may quickly see a list of variables in each structure w/
; the idl help command, eg. > help,in,/structure
;
; ******************************************************************
; Data Selection
;
; On top of the complete data set (generally a single track) one  
; will generally want to apply a selection criteria for a particular
; data reduction operation (eg. all gain cal observations as  
; indicated by variable gq='g'). We call this filtering the data set 
; and in order implement it, one should almost
; always access the data structures though index vectors : pi, pb,
; ps, pif, pbf,psf, pil, pbl, and psl -- eg in(pi), rather than
; in or in(*).
; pi, pb, ps -- the complete set of rows numbers in structures in, bl, and sp
;               except that bad data rows (eg. wt le 0) have been deleted.)
; pif, pbf, psf -- a list of row numbers from pi, pb, ps which
;                  passed through the filter criteria (eg. 
;                  source='3c273')
; pil, pbl, psl -- a working list subset of pif, pbf, and psf used for 
;                  a specific idl routine (eg. only continuum bands on 
;                  3c273)
; Each of these is a successive narrowing of the data set. The 
; essential difference between the f and l lists is that the f 
; lists are sticky and are reset only by the filter whereas
; the l lists are local to each function and can be reset at will
; by each function.
;
; Now a critical point : In order to provide linking between the
; three main structures (in, bl, and sp), the entries in the 2
; lesser vectors repeat themselves many times. eg. if there are 4
; bands on a particular baseline, the arrays pb, pbf and pbl will
; repeat 4 times and similarly for pi, pif and pil. Specifically,
; pb[i] is the row index in the bl arrays corresponding to the 
; i'th spectral band, i.e. sp.xxx[ps[i]].
; 
; ******************************************************************
; Referencing or Linking of Data Structures
; 
; To reference data within the structures, there are several techniques
; depending on what you want to get --
;
; 1) complete linked list of header info :
;    for i=0,n_elements(psl)-1 do begin
;      in[pil[i]].conid,...,bl[pbl[i]].blhid,...,sp[psl[i]].sphid,...
;    endfor
;    (For any string variables, you must you indirect addressing,
;     eg. c.gq[sp.igq[psl[i]]] to get the actual string rather than
;     the integer code. This would seem like an instance to use
;     idl pointer variables; unfortunately dereferencing of pointers
;     only operates on scalars and therefore can not do vector
;     operations.)

; 2) list of all values of a single variable : 
;    in[pil].int
;    or if it is a string, c.gq[sp.igq[psl]]
;
; 3) for only the distinct (different) values of the variable, use :
;    uti_distinct[in[pil].int]
;
; 4) the routine dat_var_info.pro can actively get much of the 
;    the variable information needed for dynamic programming. 
;    (look at its useage in dat_list.pro)
;
; *******************************************************************
; If a routine has aborted and you want to recompile it, sometimes
; one gets :
; 
; IDL> .run file
; % Procedure was compiled while active: FILE. Returning.
; % Compiled module: FILE.
; IDL> 
; 
; hit ctrl/c to terminate the routine, then try again
; % Interrupt encountered.
; 
; IDL> .run file
; 
; Alternately one can type
;
; IDL> retall
; IDL> .run file
; 
; ***************************************
; When running in interactive mode (as opposed to a .pro
; file, statement blocks need & $ , eg.
; 
; for i=0,(n_outputs-1) do begin $
; statement & $
; statement & $
; endfor
;
;  The dollar sign tells idl more command line typing is coming.
;
; *****************************************
; The directory structure for this software is :
; 
; ~rdx/idl     : idl_startup (startup operations related to mir)
;         /bcp : sybase bulkcopy format files
;         /pro : .pro files
;         /sav : idl save_format files + misc. important prog. files
;         /sql : sql script files 
;
; In each of these subdir there are README files describing
; the contents of the directory and use of the files.
;
; *****************************************
; Timing on some common array processes
;
; 45.62 %,  1.58 sec => i2[j]=i1[j]
; 16.00 %,  0.56 sec => i2=i1[j]
;  7.15 %,  0.25 sec => i1=make_array(1000000,/long,value=0)
;  6.89 %,  0.24 sec => i1=make_array(1000000,/int,value=0)
;  6.28 %,  0.22 sec => i1=make_array(1000,1000,/long,value=0)
;  5.83 %,  0.20 sec => i1=make_array(1000000,/long,value=0)
;  5.77 %,  0.20 sec => i2=make_array(1000000,/long,value=0)
;  3.72 %,  0.13 sec => j=lindgen(1000000)
;  0.81 %,  0.03 sec => i1=intarr(1000000)
;  0.64 %,  0.02 sec => i2=i1(0:n_elements[i1]-1L)
;  0.62 %,  0.02 sec => i2=i1
;  0.62 %,  0.02 sec => i2=i1(0:n_elements[i1]-1L)
;  0.02 %,  0.00 sec => i2=i1(1,0:999)
;  0.01 %,  0.00 sec => i1=intarr(1000000,/nozero)
;  0.00 %,  0.00 sec => i2=i1(0:999,1)
;  0.00 %,  0.00 sec => i1=make_array(1000000,/int,/nozero)
;
; Conclusions : 
; 1) use /nozero whenever possible in array setup.
; 2) int and long arrays take same time
; 3) indexing with an array of indexes is much slower than an implicit
;    loop of indexes
; 4) not allowed to use implicit loop on index (ie. ':') with vectors
;    therefore one must often use the ptr arrays given below
;
; *****************************************
; Useful array indexing tricks :
;
; useful pointer arrays 
;
; ny=4 & nx=3 & first=100 & yoff=1000 & xoff=10000 
;
; print,lindgen(ny,nx)/ny
;           0           0           0           0
;           1           1           1           1
;           2           2           2           2
;
; print,transpose(xoff*( lindgen(ny,nx)/ny))
;           0       10000       20000
;           0       10000       20000
;           0       10000       20000
;           0       10000       20000
;
; print,xoff*( lindgen(ny,nx)/ny)
;           0           0           0           0
;       10000       10000       10000       10000
;       20000       20000       20000       20000
;
; print,yoff*(lindgen(ny,nx)/ny)
;           0           0           0           0
;        1000        1000        1000        1000
;        2000        2000        2000        2000
; 
; print,xoff*(transpose(lindgen(nx,ny)/nx))
;           0       10000       20000       30000
;           0       10000       20000       30000
;           0       10000       20000       30000
;
; print,yoff*(lindgen(ny,nx)/ny) + xoff*(transpose(lindgen(nx,ny)/nx))
;           0       10000       20000       30000
;        1000       11000       21000       31000
;        2000       12000       22000       32000
;
; to access all records of chan, ich in array ch 
; form the pointer array ptr and then get ch[ptr]
;
; ptr=pcl[j]+(ich-1L)+lindgen(sp[psl[j]].nrec)
;  
; to access all channels of rec, irec in array ch 
; form the pointer array ptr and then get ch(ptr)
;
; ptr=pcl[j]+(irec-1L)+sp[psl[j]].nrec*lindgen(sp[psl[j]].nch)
;  
; to access all channels of rec, irec in all spectra
; in the list psl, form the pointer array ptr and then get 
; ch[ptr]
;
; ptr=make_array(total(sp[psl].nrec*sp[psl].nch),/long,/nozero)
; iptr_end=-1L & nch_prev=-1L
; for j=0,n_elements(psl)-1L do begin 
;   iptr=iptr_end+1L & iptr_end=iptr+sp[psl[j]].nrec*sp[psl[j]].nch-1L 
;   if sp[psl[j]].nch ne nch_prev then lin_nch=lindgen(sp[psl[j]].nch) 
;   nch_prev=sp[psl[j]].nch 
;   ptr[iptr:iptr_end]=pcl[j]+(irec-1L)+sp[psl[j]].nrec*lin_nch 
; endfor
;
; to make a 2-d array 1-d use 
; a=reform(a,n_elements[a],/overwrite)
;
end
