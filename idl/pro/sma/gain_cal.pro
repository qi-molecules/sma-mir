pro gain_cal,x_var=x_var,cal_type=cal_type,smoothing=smoothing, $
  frames_per_page=frames_per_page, tel_bsl=tel_bsl, refant=refant, $
  loose=loose, poly=poly, connect=connect, preavg=preavg, difif=difif, $
  no_unwrap=no_unwrap, non_point=non_point, apply_all=apply_all, $
  dual=dual, offonly=offonly, plotps=plotps,defaults=defaults,_extra=extra_keywords
;yes
;=Task:GAIN_CAL --- To excute gain calibration (SMA wrapper)
;#Type: calib 
;+Use:
;      GAIN_CAL determines the gain solutions and apply, if needed,
;      the solutions to the visibility data. In GAIN_CAL, the first
;      job is to specify the gain calibrators and set their fluxes.
;      This must be done by hand because the SMA has no calibrator
;      database. Both antenna-based and baseline-based solution can
;      be derived.
;         There is a flagging option to pick out individual points.
;      By doing this, it is necessary to re-run  gain_cal in order
;      to re-fit the gains to the data ignoring the recently flagged
;      points.
;@x_var:   
;      header variable for x-ccord:
;         'int'   : integrations --- the default 
;         'hours' : obvering time in hours
;         'el'    : elevation angle
;         'ha'    : hour angle
;@cal_type: 
;      gain solution to be derived. 
;        'amp'    : amplitude only 
;        'pha'    : phase only
;        'amp,pha': both amplitude and phase
;      For the telescope(antenna)-based calibration, 
;      cal_type='amp' implies cal_type='amp,pha'
;@smoothing: 
;      time interval over which data is smoothed to derive
;      gain curves. 
;        Defulat:
;             smoothing = 20 for x_var='int'
;          or smoothing = 2  for x_var='hours'
;@frames_per_page: 
;      number of frames per page
;@tel_bsl:  
;      'baseline' :baseline-based solution and
;      'telescope':antenna-based solution
;      'baseline' will be the default, if not set
;@refant: 
;      reference antenna needed in the ant-based
;      solution derivation, NO default
;@loose:  
;      loose restriction for ant/baseline number
;      matching, see gain.pro for details
;@poly:   
;      order of polynomial gain curve fitting;
;      integer 0 or higher. The default gain fit
;      will be boxcar smoothing if poly is not set.
;@connect:
;      deriving gain curves simply by connecting
;      data points. This is equivalent to NO smoothing.
;@preavg:
;      to pre-(vector)-average data points in consecutive
;      integrations in order to get better S/N for solving
;      (in particular antenna-based) gain curves.
;@apply_all:
;      to apply the solution to the full dataset, ignoring
;      the filter set-up. By detault, the gain solution is
;      only applied to filtered data. This option is useful
;      when one use partial baselines to obtain antenna-based
;      gain solutions but would like to apply the solution
;      to the full dataset.
;@no_unwrap:
;      by default phases will be unwrapped. Set this flag
;      to deactivate phase unwrapping.
;@non_point: 
;      deriving gain curves using planets/satellites as calibrator.
;&history:
;--------------------------------------------------------------------
;       cykuo 19dec03 adapting the form of the header
;---------------------------------------------------------------------

common global
common data_set
common plo

if (keyword_set(frames_per_page) eq 0) then frames_per_page = 1
if (keyword_set(x_var) eq 0) then x_var = 'int'
if (keyword_set(cal_type) eq 0) then cal_type = 'amp,pha'
if (n_elements(smoothing) eq 0) and (n_elements(poly) eq 0) then begin
  if x_var eq 'int' then smoothing = 20.
  if x_var eq 'hours' then smoothing = 2.
  if x_var eq 'el' then poly=2
endif
plid = 1


if not keyword_set(tel_bsl) then tel_bsl='baseline'
if (tel_bsl ne 'baseline' and tel_bsl ne 'telescope') then tel_bsl='baseline'
if (not keyword_set(connect)) then connect = 0

if (keyword_set(dual)) then begin

  irecs=bl(pbl).irec
  distinct_irecs = uti_distinct(irecs, nrecs)

  if (nrecs lt 2) then begin
    print, "Only One Frequency Band Data Exist"
    return
  endif else begin

    print, '-------------------------------------------'
    print, '       Dual Band Calibration Mode          '
    print, ' "cal_type" will be assigned to "pha" only '
    print, '-------------------------------------------'

    cal_type='pha'

    print, '       Existing Bands Include :            '
    print, c.rec[distinct_irecs]

    print, '-------------------------------------------'
    print, '     Select Reference Gain Calibrator      '
    print, '-------------------------------------------'

    result=dual_cal_ini(30.,use,all_souids,all_sources)

    j = where(use eq 1,count)
    if count eq 0 then begin
        print, 'Found no sources in dataset to be selected as gain calibrators'
        print, 'Quit !'
        return
    endif

    ncals = n_elements(j)

    gai_souids = make_array(ncals,/float)
    gai_fluxes_3mm = make_array(ncals,/float)
    gai_fluxes_1mm = make_array(ncals,/float)

    for i = 0, ncals-1 do begin
      gai_souids[i] = all_souids[j[i]]
      gai_fluxes_3mm[i] = 1.0
      gai_fluxes_1mm[i] = 1.0
    endfor
     
    print, '-------------------------------------------'
    print, '     Deriving Low Freq Band Ref Gain       '
    print, '-------------------------------------------'

    freqs=make_array(2,/float)
    lorec=c.rec[distinct_irecs[0]]
    j=where(bl(pbl).irec eq distinct_irecs[0], jcount)
    sub_sbs=uti_distinct(bl(pbl(j)).isb, nsubsbs,/many_repeat)

    aa = ''
    read,aa,prompt='Low Frequency Band Calibration Mode : [C]ontinuum (default) or [L]ine '

    if (aa ne 'L' and aa ne 'l') then begin

      ab = ''
      print,'Continuum Mode Calibration for Low Frequency Band'
      print,'Sidebands include: '
      for i = 0, (nsubsbs-1) do begin
        print, '[',i,']:', c.sb(sub_sbs[i])
      endfor
      read,ab,prompt='Select Sideband of Line [0] - ['+strtrim(string((nsubsbs-1)),2)+'] : '
      ab=long(ab)
      losb=c.sb(sub_sbs[ab])

      j=where(bl(pbl).irec eq distinct_irecs[0] and bl(pbl).isb eq sub_sbs[ab], jcount)
      sub_bands=uti_distinct(sp(psl(j)).iband, nsubbands,/many_repeat)

      ac = 0
      lobd = c.band(sub_bands[ac])

      j=where(bl(pbl).irec eq distinct_irecs[0] and bl(pbl).isb eq sub_sbs[ab] $
                 and sp(psl).iband eq sub_bands[ac], jcount)
      freqs[0] = total(sp(psl(j)).fsky)/jcount

      result=gain(gai_souids,gai_fluxes_3mm,gai_fluxes_1mm, $
                  tel_bsl,'int',cal_type,frames_per_page, $
                  plid,refant=refant,loose=loose, $
                  /connect, $
                  non_point=non_point, rec=c.rec[distinct_irecs[0]], /line, $
                  calsb=losb,calband=lobd, calbchan=0, calechan=0)

    endif else begin

      ab = ''
      print,'Line Mode Calibration for Low Frequency Band'
      print,'Sidebands include: '
      for i = 0, (nsubsbs-1) do begin
        print, '[',i,']:', c.sb(sub_sbs[i])
      endfor
      read,ab,prompt='Select Sideband of Line [0] - ['+strtrim(string((nsubsbs-1)),2)+'] : '
      ab=long(ab)
      losb=c.sb(sub_sbs[ab])

      j=where(bl(pbl).irec eq distinct_irecs[0] and bl(pbl).isb eq sub_sbs[ab], jcount)
      sub_bands=uti_distinct(sp(psl(j)).iband, nsubbands,/many_repeat)

      ac = ''
      print,'Bands include: '
      for i = 0, (nsubbands-1) do begin
        print, '[',i,']:', strupcase(c.band[sub_bands[i]])
      endfor
      read,ac,prompt='Select Band of Line [0] - ['+strtrim(string((nsubbands-1)),2)+'] : '
      ac=long(ac)
      lobd=c.band(sub_bands[ac])

      j=where(bl(pbl).irec eq distinct_irecs[0] and bl(pbl).isb eq sub_sbs[ab] $
              and sp(psl).iband eq sub_bands[ac], jcount)
      freqs[0] = total(sp(psl(j)).fsky)/jcount

      sub_chs=uti_distinct(sp(psl(j)).nch, nsubchs,/many_repeat)
         
      ad = ''
      read,ad,prompt='Specify Beginning Channels of the Line [0] - ['+strtrim(string(sub_chs[0]-1),2)+']'
      ad=long(ad)

      ae = ''
      read,ae,prompt='Specify Ending Channels of the Line ['+string(ad)+'] - ['+string(sub_chs[0]-1)+']'
      ae=long(ae)

      result=gain(gai_souids,gai_fluxes_3mm,gai_fluxes_1mm, $
                  tel_bsl,'int',cal_type,frames_per_page, $
                  plid,refant=refant,loose=loose, $
                  /connect, $
                  non_point=non_point, rec=c.rec[distinct_irecs[0]], /line, $
                  calsb=losb,calband=lobd, calbchan=ad, calechan=ae)
    endelse

    print, '-------------------------------------------'
    print, '     Deriving High Freq Band Ref Gain       '
    print, '-------------------------------------------'

    hirec=c.rec[distinct_irecs[1]]
    j=where(bl(pbl).irec eq distinct_irecs[1], jcount)
    sub_sbs=uti_distinct(bl(pbl(j)).isb, nsubsbs,/many_repeat)

    aa = ''
    read,aa,prompt='High Frequency Band Calibration Mode : [C]ontinuum (default) or [L]ine '

    if (aa ne 'L' and aa ne 'l') then begin

     ab = ''
      print,'Continuum Mode Calibration for High Frequency Band'
      print,'Sidebands include: '
      for i = 0, (nsubsbs-1) do begin
        print, '[',i,']:', c.sb(sub_sbs[i])
      endfor
      read,ab,prompt='Select Sideband of Line [0] - ['+strtrim(string((nsubsbs-1)),2)+'] : '
      ab=long(ab)
      hisb=c.sb(sub_sbs[ab])

      j=where(bl(pbl).irec eq distinct_irecs[1] and bl(pbl).isb eq sub_sbs[ab], jcount)
      sub_bands=uti_distinct(sp(psl(j)).iband, nsubbands,/many_repeat)

      ac = 0
      hibd = c.band(sub_bands[ac])

      j=where(bl(pbl).irec eq distinct_irecs[1] and bl(pbl).isb eq sub_sbs[ab] $
                 and sp(psl).iband eq sub_bands[ac], jcount)
      freqs[1] = total(sp(psl(j)).fsky)/jcount

      result=gain(gai_souids,gai_fluxes_3mm,gai_fluxes_1mm, $
                  tel_bsl,'int',cal_type,frames_per_page, $
                  plid,refant=refant,loose=loose, $
                  /connect, $
                  non_point=non_point, rec=c.rec[distinct_irecs[1]], /line, $
                  calsb=hisb,calband=hibd, calbchan=0, calechan=0)

    endif else begin

      ab = ''
      print,'Line Mode Calibration for Low Frequency Band'
      print,'Sidebands include: '
      for i = 0, (nsubsbs-1) do begin
        print, '[',i,']:', c.sb(sub_sbs[i])
      endfor
      read,ab,prompt='Select Sideband of Line [0] - ['+strtrim(string((nsubsbs-1)),2)+'] : '
      ab=long(ab)
      hisb=c.sb(sub_sbs[ab])

      j=where(bl(pbl).irec eq distinct_irecs[1] and bl(pbl).isb eq sub_sbs[ab], jcount)
      sub_bands=uti_distinct(sp(psl(j)).iband, nsubbands,/many_repeat)

      ac = ''
      print,'Bands include: '
      for i = 0, (nsubbands-1) do begin
        print, '[',i,']:', strupcase(c.band[sub_bands[i]])
      endfor
      read,ac,prompt='Select Band of Line [0] - ['+strtrim(string((nsubbands-1)),2)+'] : '
      ac=long(ac)
      hibd=c.band(sub_bands[ac])

      j=where(bl(pbl).irec eq distinct_irecs[1] and bl(pbl).isb eq sub_sbs[ab] $
              and sp(psl).iband eq sub_bands[ac], jcount)
      freqs[1] = total(sp(psl(j)).fsky)/jcount

      sub_chs=uti_distinct(sp(psl(j)).nch, nsubchs,/many_repeat)
         
      ad = ''
      read,ad,prompt='Specify Beginning Channels of the Line [0] - ['+strtrim(string(sub_chs[0]-1),2)+']'
      ad=long(ad)

      ae = ''
      read,ae,prompt='Specify Ending Channels of the Line ['+string(ad)+'] - ['+string(sub_chs[0]-1)+']'
      ae=long(ae)

      result=gain(gai_souids,gai_fluxes_3mm,gai_fluxes_1mm, $
                  tel_bsl,'int',cal_type,frames_per_page, $
                  plid,refant=refant,loose=loose, $
                  /connect, $
                  non_point=non_point, rec=c.rec[distinct_irecs[1]], /line, $
                  calsb=hisb,calband=hibd, calbchan=ad, calechan=ae)
    endelse

    print, '-------------------------------------------'
    print, '     Deriving Band Offset and Scaling      '
    print, '-------------------------------------------'

    dualfactor=cal_dual_scale(x_var=x_var,tel_bsl=tel_bsl,refant=refant,freqs=freqs,losbs=losb,hisbs=hisb,plotps=plotps)

    print, '-------------------------------------------'
    print, ' Phase Offset and Scaling between Bands are'
    print, ' listed above. Notice the values are derived '
    print, ' from and applicable to : '
    print, ' Low Frequency ',losb,' Sideband.'
    print, ' High Frequency ',hisb,' Sideband.' 
    print, '-------------------------------------------'

  endelse

  if (keyword_set(offonly)) then return

endif

if (keyword_set(dual)) then begin
    print, '-------------------------------------------'
    print, '        Dual Band Calibration Mode         '
    print, '        Reference Band Calibration         '
    print, '-------------------------------------------'
endif else begin
    print, '-------------------------------------------'
    print, '      Single Band Calibration Mode         '
    print, '          Main Band Calibration            '
    print, '-------------------------------------------'
endelse

result=gain_ini(30.,use,all_souids,all_sources,all_amps, $
   numbs_3mm,numbs_1mm,fluxes_3mm,fluxes_1mm,defaults=defaults)
if result eq 0 then begin
    print, 'Found no sources in dataset to be selected as gain calibrators'
    print, 'Quit !'
    return
endif


if (e.debug) then begin
  print,'Array  all_souids: ',all_souids
  print,'Array all_sources: ',all_sources
  print,'Array flag of use: ',use
  print,'Array  fluxes_3mm: ',fluxes_3mm
  print,'Array  fluxes_1mm: ',fluxes_1mm
endif

j = where(use eq 1,count)
if count eq 0 then begin
    print, 'Found no sources in dataset to be selected as gain calibrators'
    print, 'Quit !'
    return
endif

ncals = n_elements(j)
print,'number of gain cals ',ncals
;print,'gain cal source(s): ',all_sources[j]
;print,'gain cal 3mm fluxes:',fluxes_3mm[j]
;print,'gain cal 1mm fluxes:',fluxes_1mm[j]


gai_souids = make_array(ncals,/float)
gai_fluxes_3mm = make_array(ncals,/float)
gai_fluxes_1mm = make_array(ncals,/float)

for i = 0, ncals-1 do begin
  gai_souids[i] = all_souids[j[i]]
  gai_fluxes_3mm[i] = fluxes_3mm[j[i]]
  gai_fluxes_1mm[i] = fluxes_1mm[j[i]]
  if gai_fluxes_3mm[i] eq 0 or gai_fluxes_1mm[i] eq 0 then begin
      print, 'Calibrator fluxes should be nonezero !'
      return
  endif
endfor

if (e.debug) then print,'----------CALLING GAIN----------'

if (not keyword_set(dual)) then begin

  if keyword_set(difif) then uti_difif,/no_notify

  if (keyword_set(connect)) then $
      result=gain(gai_souids,gai_fluxes_3mm,gai_fluxes_1mm, $
                  tel_bsl,x_var,cal_type,frames_per_page, $
                  dt_smooth=smoothing,plid,refant=refant,loose=loose, $
                  connect=connect, preavg=preavg, no_unwrap=no_unwrap, $
                  non_point=non_point, difif=difif, _extra=extra_keywords) $
  else $
    if (keyword_set(smoothing) gt 0) then $
      result=gain(gai_souids,gai_fluxes_3mm,gai_fluxes_1mm, $
                  tel_bsl,x_var,cal_type,frames_per_page, $
                  dt_smooth=smoothing,plid,refant=refant,loose=loose, $
                  preavg=preavg, no_unwrap=no_unwrap, non_point=non_point, $
                  difif=difif, _extra=extra_keywords) $
    else $
      result=gain(gai_souids,gai_fluxes_3mm,gai_fluxes_1mm, $
                  tel_bsl,x_var,cal_type,frames_per_page, $
                  dt_smooth=-poly,plid, refant=refant,loose=loose, $
                  preavg=preavg, no_unwrap=no_unwrap, non_point=non_point, $
                   difif=difif, _extra=extra_keywords)
endif else begin
  if (keyword_set(connect)) then $
      result=gain(gai_souids,gai_fluxes_3mm,gai_fluxes_1mm, $
                  tel_bsl,x_var,cal_type,frames_per_page, $
                  dt_smooth=smoothing,plid,refant=refant,loose=loose, $
                  connect=connect, preavg=preavg, no_unwrap=no_unwrap, $
                  non_point=non_point, rec=c.rec[distinct_irecs[0]], _extra=extra_keywords) $
  else $
    if (keyword_set(smoothing) gt 0) then $
      result=gain(gai_souids,gai_fluxes_3mm,gai_fluxes_1mm, $
                  tel_bsl,x_var,cal_type,frames_per_page, $
                  dt_smooth=smoothing,plid,refant=refant,loose=loose, $
                  preavg=preavg, no_unwrap=no_unwrap, non_point=non_point, $
                  rec=c.rec[distinct_irecs[0]], _extra=extra_keywords) $
    else $
      result=gain(gai_souids,gai_fluxes_3mm,gai_fluxes_1mm, $
                  tel_bsl,x_var,cal_type,frames_per_page, $
                  dt_smooth=-poly,plid, refant=refant,loose=loose, $
                  preavg=preavg, no_unwrap=no_unwrap, non_point=non_point, $
                  rec=c.rec[distinct_irecs[0]], _extra=extra_keywords)

endelse

if (result lt 0) then begin
   print,'**** No gain solution was derived ****'
endif else begin

if not keyword_set(defaults) then begin
  aa = ''
  read,aa,prompt='Apply gain solution? [NO <YES>]:  '
  if (aa eq 'YES' or aa eq 'yes' or aa eq 'Yes' or aa eq 'Y' or aa eq 'y') then begin
    print,'YES: apply gains'

    if (keyword_set(dual)) then begin

      if (keyword_set(apply_all)) then begin
        print, ' -- apply_all flag is currently not effective in the dual band calibration mode ---'
      endif

      aa = ''      
      read,aa,prompt='Applying Solutions to the Low Freq Band? [NO <YES>] :'
      if (aa eq 'YES' or aa eq 'yes' or aa eq 'Yes' or aa eq 'Y' or aa eq 'y') then begin
        result=cal_apply(gain=cal_type,x_var=x_var,tel_bsl=tel_bsl,refant=refant,/dualmode)
      endif

        print, '-------------------  NOTE -- -------------------'
        print, '   Dual Band Phase Offset and Scaling are     '
        print, '   derived from and applicable to : '
        print, '     Low Frequency ',losb,' Sideband.'
        print, '     High Frequency ',hisb,' Sideband.'
        print, '   Low frequency opposite sideband solution   '
        print, '   and high frequency opposite sideband data, '
        print, '   if exist, will be modified and applied with'
        print, '   the same factors. Use with caution.        '
        print, '------------------------------------------------'

      aa = ''
      read,aa,prompt='Applying (Scaled) Solutions to the High Freq Band? [NO <YES>] :'
      if (aa eq 'YES' or aa eq 'yes' or aa eq 'Yes' or aa eq 'Y' or aa eq 'y') then begin

        result=cal_apply(gain=cal_type,x_var=x_var,tel_bsl=tel_bsl,refant=refant,/dualmode,dualfactor=dualfactor)
      endif

    endif

    if (not keyword_set(dual)) then begin
      if (keyword_set(apply_all)) then begin
        print, ' --- Flag apply_all is set. Set filter temperarily to the full dataset ---'
        result=dat_filter(s_s,/save)
        result=dat_filter(s_f,/reset,/no_notify)
      endif

      if keyword_set(difif) then begin
         print,'Applying DIFIF term'
         result=dat_filter(s_s,/save)
         result=dat_filter(s_f,'"iband" gt "24"',/no_notify)
      endif else print,'Applying Gains'

      result=cal_apply(gain=cal_type,x_var=x_var,tel_bsl=tel_bsl,refant=refant)

      if (keyword_set(apply_all)) then begin
        print,' --- Restoring filter ---'
        result=dat_filter(s_s,/restore)
      endif
    endif

  endif else begin
    print,'NO: nothing done'
 endelse
endif else begin
      print,'Applying Gains'

      result=cal_apply(gain=cal_type,x_var=x_var,tel_bsl=tel_bsl,refant=refant)
   endelse

endelse

if keyword_set(difif) then begin
   result=dat_filter(s_s,/restore)
   uti_avgband
   print,'***'
   print,'DIFIF term fixed on IF2 and continuum regrenerated !'
   print,'***'
endif

end
