pro pro_test
;
; Test idl statements for timing and results
;
; eg. : pro_profile ,file='pro_test'
; eg. : pro_test
;
common global
common data_set

i1=intarr(1000000)
i1=intarr(1000000,/nozero)
i1=make_array(1000000,/int,/nozero)
i1=make_array(1000000,/int,value=0)
i1=make_array(1000000,/long,value=0)
i2=i1(0:n_elements(i1)-1L)
i2=i1
j=lindgen(1000000)
i2=i1(j)
i2=make_array(1000000,/long,value=0)
i2(j)=i1(j)
i1=make_array(1000000,/long,value=0)
i2=i1(0:n_elements(i1)-1L)
i1=make_array(1000,1000,/long,value=0)
i2=i1(0:999,1)
i2=i1(1,0:999)

irec=3
ptr=make_array(total(sp(psl).nrec*sp(psl).nch),/long,/nozero)
iptr_end=-1L & nch_prev=-1L
for j=0,n_elements(psl)-1L do begin 
  iptr=iptr_end+1L & iptr_end=iptr+sp[psl[j]].nrec*sp[psl[j]].nch-1L 
  if sp[psl[j]].nch ne nch_prev then lin_nch=lindgen(sp[psl[j]].nch) 
  nch_prev=sp[psl[j]].nch 
  ptr[iptr:iptr_end]=pcl[j]+(irec-1L)+sp[psl[j]].nrec*lin_nch 
endfor



 irec=3
 ptr=make_array(total(sp(psl).nrec*sp(psl).nch),/long,/nozero)
 iptr_end=-1L
 for j=0,n_elements(psl)-1L do begin 
   iptr=iptr_end+1L & iptr_end=iptr+sp[psl[j]].nrec*sp[psl[j]].nch-1L 
   ptr[iptr:iptr_end]=pcl[j]+(irec-1L)+sp[psl[j]].nrec*lindgen(sp[psl[j]].nch)
 endfor

end


