pro uti_ave_int,dt_max=dt_max
; 
; averages integrations up to a time interval dt_max (min)
; 
;

; Common blocks
  common global
  common data_set

; set up structures for header table
;
  inh_temp={conid:0L,icocd:0,traid:0L,inhid:0L,$
         int:0L,itq:0,az:0e,el:0e,ha:0e,$
         iut:0,$
         iref_time:0,$
         dhrs:0d,vc:0e,ivctype:0,sx:0d,sy:0d,sz:0d,rinteg:0e,proid:0L,$
         souid:0L,isource:0,ipos:0,offx:0e,offy:0e,$
         iofftype:0,ira:0,$
         idec:0,rar:0d,decr:0d,epoch:0e,sflux:0e,size:0e}
  blh_temp={blhid:0L,inhid:0L,isb:0,ipol:0,pa:0e,$
         iaq:0,ibq:0,icq:0,ioq:0,irec:0,$
         iifc:0,u:0e,v:0e,w:0e,prbl:0e,angres:0e,vis:0e,coh:0e,$
         sigcoh:0e,csnr:0e,vflux:0e,cnoise:0e,avedhrs:0d,$
         ampave:0e,phaave:0.e,$
         tpvar:0.e,blsid:0L,itel1:0,itel2:0,iblcd:0,ble:0.e,$
         bln:0e,blu:0e,soid:0L}
  sph_temp={sphid:0L,blhid:0L,inhid:0L,igq:0,ipq:0,iband:0,$
         ipstate:0,tau0:0e,vel:0d,vres:0e,ivtype:0,fsky:0d,$
         fres:0e,tssb:0e,integ:0e,wt:0e,itaper:0,snoise:0e,$
         nch:0,nrec:0,dataOFF:0L,linid:0L,itrans:0,$
         rfreq:0d,pasid:0,gaiidamp:0,gaiidpha:0,$
         flcid:0,atmid:0}
  code_temp={v_name:'123456789012',icode:0, $
           code:'12345678901234567890123456',ncode:0}

  bad=complex(-1001.00,-1001.00)
  if not keyword_set(dt_max) then dt_max=2.
  print,'average integrations up to ',dt_max,' minutes'
  dt_max=dt_max/60.
  redo_cont=1
  cont_band='C1'

  in_new=in
  bl_new=bl
  sp_new=sp
  ch_new=ch
  pi_new=pi
  pb_new=pb
  ps_new=ps
  pc_new=pc

  in_ind=0L
  bl_ind=0L
  sp_ind=0L
  ch_ind=0L

  i=0L
  while i lt n_elements(in)-1L do begin
     start_time=in[i].dhrs
     j=where(in.isource eq in[i].isource and (in.dhrs-start_time) ge 0.  $
       and (in.dhrs-start_time) le dt_max,ct)
     in_temp=in[i]
     bl_temp=bl[i]
     sp_temp=sp[i]
;  print,in[i].inhid,'  ',c.source[in[i].isource],'  ',start_time,ct,' integrations to average'
     if ct gt 0 then begin
        in_temp.az=uti_avg([in[j].az])
        in_temp.el=uti_avg(in[j].el)
        in_temp.ha=uti_avg(in[j].ha)
        in_temp.vc=uti_avg(in[j].vc)
        in_temp.sx=uti_avg(in[j].sx)
        in_temp.sy=uti_avg(in[j].sy)
        in_temp.sz=uti_avg(in[j].sz)
        in_temp.rinteg=total(in[j].rinteg)
        in_temp.inhid=in_ind+1L
        in_new[in_ind]=in_temp & in_ind=in_ind+1L
        rows=where(sp[ps].inhid ge in[i].inhid and sp[ps].inhid le max([in[j].inhid]),ctb)
        if ctb gt 0 then begin
           sbs=strupcase(c.sb(bl(pb[rows]).isb))
           bands=strupcase(c.band(sp(ps[rows]).iband))
           bls=strupcase(c.blcd(bl(pb[rows]).iblcd))
           bl_sbs=bls+' '+sbs
           distinct_bl_sbs=uti_distinct(bl_sbs,n_bl_sbs,/many_repeat)
           bl_sb_bands=bls+' '+sbs+' '+bands
           distinct_bl_sb_bands=uti_distinct(bl_sb_bands,n_bl_sb_bands,/many_repeat)
           distinct_bands=uti_distinct(bands,n_bands,/many_repeat)

           for ic=0,n_bl_sbs-1 do begin
              jc=where(bl_sbs eq distinct_bl_sbs[ic],ctc)
              bl_temp=bl[pb[rows[jc[0]]]]
              bl_temp.inhid=in_temp.inhid
              bl_temp.blhid=bl_ind+1L
              if ctc gt 1 then begin
                 bl_temp.u=uti_avg(bl[pb[rows[jc]]].u)
                 bl_temp.v=uti_avg(bl[pb[rows[jc]]].v)
                 bl_temp.w=uti_avg(bl[pb[rows[jc]]].w)
                 bl_temp.prbl=uti_avg(bl[pb[rows[jc]]].prbl)
                 bl_temp.angres=uti_avg(bl[pb[rows[jc]]].angres)
                 bl_temp.coh=uti_avg(bl[pb[rows[jc]]].coh)
                 bl_temp.avedhrs=uti_avg(bl[pb[rows[jc]]].avedhrs)
                 bl_temp.ampave=uti_avg(bl[pb[rows[jc]]].ampave)
                 bl_temp.phaave=uti_avg(bl[pb[rows[jc]]].phaave)              
                 bl_temp.ble=uti_avg(bl[pb[rows[jc]]].ble)
                 bl_temp.bln=uti_avg(bl[pb[rows[jc]]].bln)
                 bl_temp.blu=uti_avg(bl[pb[rows[jc]]].blu)
              endif
              tot_cmp=complex(0.,0.) & nb=0L 
              for ib=0,n_bands-1 do begin
                 jcs=where(bands[jc] eq distinct_bands[ib],ctcc)
                 if ctcc gt 0 then begin
                    first_ch=pc[rows[jc[jcs[0]]]]
                    if ch[first_ch] eq bad then begin
                       goto, skip_spec
                    endif
                    first_ch=pc[rows[jc[jcs[0]]]]
                    ps_new[sp_ind]=sp_ind
                    pi_new[sp_ind]=in_ind-1L
                    pb_new[sp_ind]=bl_ind
                    pc_new[sp_ind]=ch_ind
                    sp_temp=sp[ps[rows[jc[jcs[0]]]]]
                    sp_temp.inhid=in_temp.inhid
                    sp_temp.blhid=bl_temp.blhid
                    sp_temp.sphid=sp_ind+1L
                    sp_temp.tau0=uti_avg(sp[ps[rows[jc[jcs]]]].tau0)
                    sp_temp.vel=uti_avg(sp[ps[rows[jc[jcs]]]].vel)
                    sp_temp.fsky=uti_avg(sp[ps[rows[jc[jcs]]]].fsky)
                    sp_temp.tssb=uti_avg(sp[ps[rows[jc[jcs]]]].tssb)
                    sp_temp.integ=total(sp[ps[rows[jc[jcs]]]].integ)
                    sp_temp.wt=total(sp[ps[rows[jc[jcs]]]].wt)
                    sp_temp.dataoff=ch_ind
                    icc=lindgen(sp_temp.nch)
                    ch_new[ch_ind+icc]=ch[first_ch+icc]
                    ispec=1L
                    while ispec lt ctcc do begin
                       first_ch=pc[rows[jc[jcs[ispec]]]]
                       ch_new[ch_ind+icc]=ch_new[ch_ind+icc]+ch[first_ch+icc]
                       ispec=ispec+1L
                    endwhile
                    ch_new[ch_ind+icc]=ch_new[ch_ind+icc]/float(ctcc)
                    if redo_cont eq 1 then begin
                       for icb=0,n_elements(cont_band)-1 do begin
                          if n_elements(icc) gt 2 then icc=icc[1:n_elements(icc)-2] ; cut out first and last channel   
                          if distinct_bands[ib] eq cont_band[icb] then begin
                             tot_cmp=tot_cmp+total(ch_new[ch_ind+icc])/float(n_elements(icc))
                             nb=nb+1L
                          endif
                       endfor
                    endif
                    
                    ch_ind=ch_ind+sp_temp.nch
                    sp_new[sp_ind]=sp_temp & sp_ind=sp_ind+1L
                    
                 endif
                 skip_spec:
                 
              endfor
              if nb eq 0 then begin
                 goto, skip_base
              endif
              if redo_cont and nb gt 0 then begin
                 mean_cmp=tot_cmp/float(nb)
                 uti_conv_apc,mean_cmp,vec_amp_avg,vec_pha_avg,/amp_pha
                 vec_pha_avg = uti_pha_180(vec_pha_avg) + 360.*(vec_pha_avg lt 0)
                 bl_temp.ampave=vec_amp_avg
                 bl_temp.phaave=vec_pha_avg
;          print,distinct_bl_sbs[ic],nb,vec_amp_avg,vec_pha_avg
              endif
              bl_new[bl_ind]=bl_temp & bl_ind=bl_ind+1L
              skip_base:

           endfor


        endif


     endif
     i=j[ct-1]+1L 
  endwhile


  in=in_new[0:in_ind-1L]
  bl=bl_new[0:bl_ind-1L]
  sp=sp_new[0:sp_ind-1L]
  ch=ch_new[0:ch_ind-1L]


; set up pointer arrays for correspondence from sp to in and bl
; set up a similar one for sp even though it is not needed


  pi=pi_new[0:sp_ind-1L]
  pb=pb_new[0:sp_ind-1L]
  ps=ps_new[0:sp_ind-1L]
  pc=pc_new[0:sp_ind-1L]

  pif=pi
  pbf=pb
  psf=ps
  pis=pi
  pbs=pb
  pss=ps
  pil=pi
  pbl=pb
  psl=ps

  nrec=long(total(sp.nrec))
  pr=lindgen(nrec)
  pcf=pc
  prf=pr
  pcs=pc
  prs=pr
  pcl=pc
  prl=pr
  



  

; Done
  print,'Done !'
;  return,!MIR_OK
end
