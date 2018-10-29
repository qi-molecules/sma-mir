# sma-mir
a software package to reduce data taken with the Submillimeter Array (SMA)

see

https://www.cfa.harvard.edu/~cqi/mircook.html

###################################################

Jan. 2018: added uti_checkspike.pro, autocal.pro, autofits.pro

Mar. 2018: updated fits_out.pro for correct continuum freq header
                   uti_checkspike.pro for mis. message
                   readdata.pro, dbi_head2_read2.pro, dbi_head_read2.pro
                      for batch processing without checking ant file.

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
