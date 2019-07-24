# sma-mir
a software package to reduce data taken with the Submillimeter Array (SMA)

see

https://www.cfa.harvard.edu/~cqi/mircook.html

###################################################

Jul. 2019
fits_out.pro (modified)
-- u,v,w calculation based on continuum freq, 
             not chunk freq. Also a bug fixed for sideband selection.

readdata.pro (modified)
-- added polarization correction


Apr. 2019
uti_respike.pro (added)
-- use spike_read file to restore spikes at given channel. 

uti_hayshft_fix.pro (added)
-- fix the diurnal dopplerTracking error 
                     found by Mark 
idl2miriad.pro, uti_pos_fix.pro, fits_out.pro (modified)
-- !DPI double floats

dbi_head2_read2.pro (modified) 
-- additional warning message about wrong receiver
                           header.

Feb. 2019

uti_avgbandx.pro (added)
-- testing, excludefreq keyword

autofits.pro, uti_tsys_fix.pro, uti_doppler_fix.pro (modified)
-- autofits to allow more sources output
-- uti_tsys_fix.pro for noninteractive tsys fixing
-- uti_doppler_fix.pro, disabling fft correction, replace it 
                    with cubic convolution

Jan. 2019:
uti_doppler_fix.pro  adding a line to fix vres 

Oct. 2018: 
plo_print_page.pro removing xs=s_page.xs
plo_spec.pro about source=fsource
plo_cont.pro for preavg usage (new data format, 
	  	       	      no flagging between source)
gain.pro  for preavg usage (new data format from attila)

sma/dbi_head_rad2.pro adding defaults without uvw checking
sma/gain_sideband.pro
sma/gain_xpol.pro
sma/gain_xpol1.pro
sma/gain_xpol4.pro
sma/uti_phaseclosure.pro
sma/uti_quickcheck.pro
sma/dat_merge_sma.pro (for second track has more baselines)
sma/readdata.pro (double data fix)
sma/readtsys2.pro (small bug about irec eq -1)
sma/dbi_head2_read2.pro (correct for the inh headers, uvw checking)
sma/uti_checkspike.pro (bug fixed in smoothing amplitude)

Mar. 2018: updated fits_out.pro for correct continuum freq header
                   uti_checkspike.pro for mis. message
                   readdata.pro, dbi_head2_read2.pro, dbi_head_read2.pro
                      for batch processing without checking ant file.

Jan. 2018: added uti_checkspike.pro, autocal.pro, autofits.pro
