pro plo_finish_flagging,plid,sindex
; This routine finishes up the flagging process by finalizing all the 
; selections and redrawing the plot
;
; parameters 
;	     : plid - plot index - index into pl array structure
;	     : sindex - index into con,gai,wlm, etc array
;
; 
; Written by:  Kim Drongesen
; Date:  December 16, 1998

common global
common plo
common data_set

; This routine owes its awkwardness to the fact that I don't want
; to copy in the entire structure into s_page for one instruction
case pl[plid].plot_type of
      'con':  begin
		if con[sindex].n_select gt 0 then begin
                j=where(con[sindex].j_select ne -1,count)
                con[sindex].wts(con[sindex].j_select[j])=float(con[sindex].m_button_select[j])* $
                  abs(con[sindex].wts(con[sindex].j_select[j]))
                for i=0L, count-1L do begin
                   spindex=psl[con[sindex].j_select[j[i]]]+indgen(n_elements(c.band))
                   spband=sp[spindex].iband
                   ii=where(spband eq 0, count2)
                   if (count2 ge 2) then spindex=spindex[0:ii[1]-1]
                   sp[spindex].wt=float(con[sindex].m_button_select[j[i]])*abs(sp[spindex].wt)
                endfor
;                sp[psl[con[sindex].j_select[j]]].wt=float(con[sindex].m_button_select[j])*abs(sp[psl[con[sindex].j_select[j]]].wt)
                endif
               end
      'spe':  begin
		if spe[sindex].n_select gt 0 then begin
   	      	spe[sindex].wts(spe[sindex].j_select)=float(spe[sindex].m_button_select)* $
                  abs(spe[sindex].wts(spe[sindex].j_select))
	   	endif
	      end
      'gai':  begin
		if gai[sindex].n_select gt 0 then begin
                j=where(gai[sindex].j_select ne -1,count)
                gai[sindex].wts(gai[sindex].j_select[j])=float(gai[sindex].m_button_select[j])* $
                  abs(gai[sindex].wts(gai[sindex].j_select[j]))

;                sp[psl[gai[sindex].j_select[j]]].wt=float(gai[sindex].m_button_select[j])*abs(sp[psl[gai[sindex].j_select[j]]].wt)
	   	endif
	      end
      'pas':  begin
		if pas[sindex].n_select gt 0 then begin
   	      	pas[sindex].wts(pas[sindex].j_select)=float(pas[sindex].m_button_select)* $
                  abs(pas[sindex].wts(pas[sindex].j_select))
	   	endif
	      end
      'wlm':  begin
		if wlm[sindex].n_select gt 0 then begin
   	      	wlm[sindex].wts(wlm[sindex].j_select)=float(wlm[sindex].m_button_select)* $
                  abs(wlm[sindex].wts(wlm[sindex].j_select))
	   	endif
             end
      'var':  begin
                if var[sindex].n_select gt 0 then begin
                   j=where(var[sindex].j_select ne -1)
                   sp[psl[var[sindex].j_select[j]]].wt=float(var[sindex].m_button_select[j])*abs(sp[psl[var[sindex].j_select[j]]].wt) 
                   var[sindex].wts(var[sindex].j_select[j])=float(var[sindex].m_button_select[j])* $
                     abs(var[sindex].wts(var[sindex].j_select[j]))               
                endif
             end
endcase

result=plo_page(plid,sindex)

case pl[plid].plot_type of
    'gai':  begin
      	       gai[sindex].nsub=gai[sindex].nsub-1
               result=plo_over_page(plid,sindex)
               gai[sindex].nsub=gai[sindex].nsub+1
            end
    'pas':  begin
               pas[sindex].nsub=2
       	       result=plo_over_page(plid,sindex)
       	    end
else:
endcase

return
end
