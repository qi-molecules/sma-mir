pro uti_ha2el
common global
common data_set

lat=double(19.82420526391d*!pi/180.d)
dec=in[pil].decr
haa=in[pil].ha*!pi/12.0d

temp=sin(lat)*sin(dec)+cos(lat)*cos(dec)*cos(haa)

in[pil].el=asin(temp)*180.d/!pi

end
