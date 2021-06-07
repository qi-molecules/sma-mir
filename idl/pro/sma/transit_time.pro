function transit_time, rarad=rarad, longitude=longitude, unixtime=unixtime

; finds the unixtime of transit after the specified unixtime
; longitude in degrees east of Greenwich (-155.477522 for SMA)

JD = (unixtime - double(1.0e9))/86400.D + 2452161.5740740742D
T = (JD - 2451545.0D) / 36525.0D
lstRadians = rarad
mjd = JD - 2400000.5D
SOLAR_TO_SIDEREAL = 1.002737909350795D
lstHours = lstRadians*12/!dpi
siderealDegrees = 280.46061837D + 360.98564736629D*(JD - 2451545.0D) + 0.000387933D*T*T - T*T*T/38710000.0D
sidereal = (siderealDegrees + longitude)/360.0D
lstAtMidnight = 24D * (sidereal - floor(sidereal))
delay = (lstHours - lstAtMidnight) / 24.0D
if (delay < 0.0) then begin
   delay += 1.0
endif
mjd = mjd + delay/SOLAR_TO_SIDEREAL
unixtime = double(1e9) + (mjd-52161.07407407416D)*86400D
return, unixtime
end
