function uti_vrad, ra, dec, lst, tdb
; input:
;         ra, dec in radians
;         tdb, Barycentric Dynamical Time in seconds
; output: radial velocity respect to SMA, no catalog velocity.
;         vRad, in m/s

ASTRONOMICAL_UNIT=double(1.4959787e11) ;meter 
M_PER_KM=1000.d
globalRefLat=19.82420526391d*!dpi/180.d
globalRA=ra
globalDec=dec
sladcs2c,globalRA,globalDec,unitVector
; Get Heliocentric vector velocity of earth
slaEvp, tDB, double(2000.0), dummy1, dummy2, dVH, dummy3
 
; Now form the dot product of the unit vector and the velocity vector 
earthOrbitalVelocity = slaDvdv(unitVector, dVH) * ASTRONOMICAL_UNIT
;;print,'earthOrbitalVelocity = ',earthOrbitalVelocity ; m/s 

;-----the SUN's LSR velocity in the direction of the source
kinematicLSRCorrection=slaRvlsrk(globalRA, globalDec)*M_PER_KM
;;print,'kinematicLSRCorrection = ',kinematicLSRCorrection

;-----the earth rotation
; LST also in radian
lstRad=lst*15.d*!dpi/180.d
globalRoto = slaRverot(globalRefLat, globalRA, globalDec, lstRad) * 1.0e3
;;print,'globalRoto = ',globalRoto

; Radial velocity
vRad = -earthOrbitalvelocity+kinematicLSRCorrection+globalRoto
return,vrad
end

