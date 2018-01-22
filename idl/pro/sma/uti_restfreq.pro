pro uti_restfreq, vel=vel

common global
common data_set

;print,'This program is used only for line identification!'
;print,'Only headers are changed for spectra plotting in MIR.'
;print,'   To use the program:
;print,'     IDL> uti_restfreq,vel=xxx '
;print,'       in which xxx is the radial velocity at source transit 
;print,'       tracked by DopplerTrack command, in unit of meter/second.'
;print,'     Then use plot_spectra to check restfreq of the lines, e.g.
;print,"     IDL> plot_spectra,frame_v='sb',x='fsky'"


beta=vel/!cvel
sp[psf].fsky=sp[psf].fsky*sqrt((1+beta)/(1-beta))

print,'FSKY header has been changed !'
print,'Now you can use plot_spectra to check restfreq of the lines, e.g.
print,"     IDL> plot_spectra,frame_v='sb',x='fsky'"
print,'To reverse the action:
print,'     IDL> uti_restfreq,vel=-xxx '
print,'       in which -xxx is the opposite value of the velocity'
print,'       used previously.'


end
