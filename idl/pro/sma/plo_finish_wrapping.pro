pro plo_finish_wrapping,plid,sindex

common global
common plo
common data_set

case pl[plid].plot_type of
      'gai':  begin
		if gai[sindex].n_select gt 0 then begin
                fir_ind=where(gai[sindex].y_var eq 'pha')
                plot_panel=gai[sindex].i_select[0]
                js=where(gai[sindex].frames eq gai[sindex].distinct_frames[plot_panel])
                jindex=where(js ge gai[sindex].j_select[0])
                gai[sindex].ys(fir_ind[0],js[jindex])=float(gai[sindex].m_button_select[0])*360.+gai[sindex].ys(fir_ind[0],js[jindex])
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
