function wlm_unwrap,time,phases,labels=labels,linear=linear,$
                    frames=frames
    ; Find distinct labels
      if keyword_set(labels) then $
         ids = labels $
      else $
         ids = replicate(0,n_elements(phases))
      frames = uti_distinct(ids,nframes)

    ; Remove linear fit? 
      ilinear = keyword_set(linear)

    ; Loop over distinct frames
      for i = 0, nframes-1L do begin
        ; Find data
          j = where(ids eq frames[i])

        ; Unwrap
          x = phases(j)
          x = uti_pha_180(x)
          result = uti_pha_unwrap(x)

        ; Remove any drift
          if (ilinear) then begin
             uti_fit_lin,time(j),x,a,siga,b,sigb
             x = x - (b(0)*time(j)+ a(0))
          endif else begin
             x = x - total(x)/n_elements(x)
          endelse

        ; Save phases
          phases(j) = x
      endfor

    ; Done
      return,1
end
