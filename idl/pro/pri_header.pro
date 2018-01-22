function pri_header ,file_name
; 
; Print header data to host file in flat table ascii format 
; result=-1 (error), 1(ok)
;
; parameters : file_name : output file name
;
; eg. : table_name='t_3500_1' 
;       result=pri_header(file_name) 
;

common global
common data_set

openw,unit,file_name,/get_lun,width=1042
for i=0L,n_elements(ps)-1L do begin
inh_temp={i1:in(pi(i)).conid,i2:c.cocd(in(pi(i)).icocd),i3:in(pi(i)).traid, $
i4:in(pi(i)).inhid,i5:c.ut(in(pi(i)).iut),i6:in(pi(i)).int, $
i7:c.tq(in(pi(i)).itq),i8:in(pi(i)).az,i9:in(pi(i)).el,i10:in(pi(i)).ha, $
i11:in(pi(i)).vc,i12:c.vctype(in(pi(i)).ivctype),i13:in(pi(i)).sx, $
i14:in(pi(i)).sy,i15:in(pi(i)).sz,i16:in(pi(i)).rinteg,i17:bl(pb(i)).blhid, $
i18:c.sb(bl(pb(i)).isb),i19:c.pol(bl(pb(i)).ipol),i20:bl(pb(i)).pa, $
i21:c.aq(bl(pb(i)).iaq),i22:c.bq(bl(pb(i)).ibq), $
i23:c.cq(bl(pb(i)).icq),i24:c.oq(bl(pb(i)).ioq),i25:c.rec(bl(pb(i)).irec), $
i26:c.ifc(bl(pb(i)).iifc),i27:bl(pb(i)).u,i28:bl(pb(i)).v,i29:bl(pb(i)).w, $
i30:bl(pb(i)).prbl,i31:bl(pb(i)).angres,i32:bl(pb(i)).vis,i33:bl(pb(i)).coh, $
i34:bl(pb(i)).sigcoh,i35:bl(pb(i)).csnr,i36:bl(pb(i)).vflux, $
i37:bl(pb(i)).cnoise,i38:c.ut(in(pi(i)).iut),i39:bl(pb(i)).ampave, $
i40:bl(pb(i)).phaave,i41:bl(pb(i)).tpvar,i42:bl(pb(i)).blsid, $
i43:c.tel1(bl(pb(i)).itel1),i44:c.tel2(bl(pb(i)).itel2), $
i45:c.blcd(bl(pb(i)).iblcd),i95:bl(pb(i)).ble,i46:bl(pb(i)).bln, $
i47:bl(pb(i)).blu,i48:bl(pb(i)).soid,i49:sp(ps(i)).sphid, $
i50:c.gq(sp(ps(i)).igq),i51:c.pq(sp(ps(i)).ipq), $
i52:c.band(sp(ps(i)).iband),i53:'',i54:c.pstate(sp(ps(i)).ipstate), $
i55:sp(ps(i)).tau0,i56:0.0,i57:sp(ps(i)).vel,i58:sp(ps(i)).vres, $
i59:c.vtype(sp(ps(i)).ivtype),i60:sp(ps(i)).fsky,i61:sp(ps(i)).fres, $
i62:sp(ps(i)).tssb,i63:sp(ps(i)).integ,i64:sp(ps(i)).wt, $
i65:c.taper(sp(ps(i)).itaper),i66:0.0,i67:sp(ps(i)).snoise, $
i68:sp(ps(i)).nch,i69:sp(ps(i)).nrec,i70:sp(ps(i)).dataOFF, $
i71:sp(ps(i)).linid,i72:c.trans(sp(ps(i)).itrans),i73:sp(ps(i)).rfreq, $
i74:in(pi(i)).souid,i75:'',i76:c.source(in(pi(i)).isource), $
i77:c.pos(in(pi(i)).ipos),i78:in(pi(i)).offx,i79:in(pi(i)).offy, $
i80:c.offtype(in(pi(i)).iofftype),i81:c.ra(in(pi(i)).ira), $
i82:c.dec(in(pi(i)).idec),i83:in(pi(i)).rar,i84:in(pi(i)).decr, $
i85:in(pi(i)).epoch,i86:in(pi(i)).sflux,i87:in(pi(i)).size, $
i88:in(pi(i)).proid,i89:sp(ps(i)).pasid,i90:sp(ps(i)).pasid, $
i91:sp(ps(i)).gaiidamp,i92:sp(ps(i)).gaiidpha,i93:sp(ps(i)).flcid, $
i94:sp(ps(i)).atmid,i96:''}
printf,unit,inh_temp
endfor
return,0
end

