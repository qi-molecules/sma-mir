pro plo_plid_rel,plid
; After the plot goes away - this sets the plid structure used to non active

common global
common plo

pl[plid].active = 0

return
end
