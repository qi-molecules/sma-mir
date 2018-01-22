;yes
;=Task:UTI_TSYS_COR --- To correct bad tsys values by interpolation
;#type: utility
;+Use:
;         Tsys values are important in SMA data calibration since they 
;      are directly related to the visibility weights and the amplitude 
;      calibration after apply_tsys. Unfortunately currently our continum 
;      detectors are not stable enough to track the tsys changes in the 
;      whole track. Sometimes no tsys values which mir records as 99999L 
;      to minimize the visibility weight, and sometimes some bad tsys 
;      values have been recorded which are shown in the plot of tsys versus 
;      elelation. The program UTI_TSYS_COR can actually correct those tsys v
;      alues according to the correlation between the tsys and el. This 
;      program fit the tsys vs. el, giving the atmospheric tau is very 
;      constant. Of couse we need to ignore the  obvious outliers(e.g. those 
;      no-tsys points) in order to get reasonable fit.
;         The program determine the bad points if the differnce of the  
;      tsys with the fit function is larger than a fixed value( the loose 
;      value), and then replace the bad tsys points with the fitted results.
;         To call this program: 
;      IDL>uti_tsys_cor, highlimit=400, lowlimit=150, loose=20
;      highlimit and lowlimit are the range you decide for obvious outliers,
;      and the  loose value is the fixed value you make the judgement for 
;      bad tsys values compared with the fitting result. The default values
;      for these  three keywords are 1000K, 50K, and 30K respectively. 
;      After the program plot out the fitted function and the real tsys
;      values, it will ask the users whether they'd like to fix the tsys 
;      using the function plotted. One can just use it, or abort it and
;      use different parameters to get better fit.   
;         If users would like to use the program in scripts without inquiring
;      about fixing tsys, they can use
;      IDL>uti_tsys_cor, highlimit=500,lowlimit=300,/no_display.
;&history:
;----------------------------------------------------------------------------
;      cykuo 03mar04 adapting the header
;---------------------------------------------------------------------------

PRO tsys_gfunct, X, A, F, pder
  bx = EXP(A[1] * X)
  F = A[0] * bx + A[2]
;If the procedure is called with four parameters, calculate the
;partial derivatives.
  IF N_PARAMS() GE 4 THEN $
    pder = [[bx], [A[0] * X * bx], [replicate(1.0, N_ELEMENTS(X))]]
END

pro uti_tsys_cor, highlimit=highlimit, lowlimit=lowlimit, loose=loose, no_display=no_display

common global
common data_set

if not keyword_set(loose) then loose=30
if not keyword_set(highlimit) then highlimit=1000.
if not keyword_set(lowlimit) then lowlimit=50.

distinct_source=uti_distinct(c.source[in[pil].isource],nsources,/many_repeat)
distinct_blcd=uti_distinct(c.blcd[bl[pbl].iblcd],nblcds,/many_repeat)

for ib=0,nblcds-1 do begin
   for is=0,nsources-1 do begin
      result=dat_list(s_l,'"blcd" eq "'+distinct_blcd[ib]+'" and "source" eq "'+distinct_source[is]+'"',/no_notify,/reset)
;print,distinct_blcd[ib],distinct_source[is]
      if result le 0 then goto, no_source
      x=1./sin(in[pil].el*!pi/180.)
      y=sp[psl].tssb
      i_good=where((sp[psl].tssb gt lowlimit) and (sp[psl].tssb lt highlimit),count)
      if count le 0 then begin
         print, 'No fitting. Please check the tsys values for source '+distinct_source[is]+' on baseline '+distinct_blcd[ib]
         return
      endif
      xx=x[i_good]
      yy=y[i_good]
      weights=fltarr(count)+1.
;      weights=1/y^2
      c0=yy[0]-400.*exp(0.2*xx[0])
      A=[400.,0.2,c0]
      yfit=curvefit(xx,yy,weights,A,sigma,itmax=200,function_name='tsys_gfunct')
;      print,A
      yfit=A[0]*exp(A[1]*x)+A[2]
      i_int=where(abs(y-yfit) gt loose, count)
      if count gt 0 then begin
         aa='YES'
         if not keyword_set(no_display) then begin
            xline=(max(in[pil].el)-min(in[pil].el))/100.*indgen(100)+min(in[pil].el)
            yline=A[0]*exp(A[1]/(sin(xline*!pi/180.)))+A[2]
            print, 'Fitting tsys values for source '+distinct_source[is]+' on baseline '+distinct_blcd[ib]
            plot,xline,yline,/ynoz,title='Source '+distinct_source[is]+' on baseline '+distinct_blcd[ib],xtitle='EL',ytitle='TSYS'
            oplot,in[pil].el,sp[psl].tssb,psym=4
            read,aa,prompt='Fixing tsys ? [NO <YES>]:  '
         endif
         if (aa eq 'YES' or aa eq 'yes' or aa eq 'Yes' or aa eq 'Y' or aa eq 'y') then sp[psl[i_int]].tssb=yfit[i_int]
      endif
      no_source:
   endfor
endfor


result=dat_list(s_l,/reset)
end
