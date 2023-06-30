# sma-mir
a software package to reduce data taken with the Submillimeter Array (SMA)

see

https://www.cfa.harvard.edu/~cqi/mircook.html

###################################################

Jul 2023: MIR update:

setup.sh (modified) 
-- update for bash setup file

sma.py (modified)
-- load uvfits with correct weight scaling for CASA output.

readdata.pro, dbi_head2_read2.pro, dat_merge_sma.pro, ms_newformat.save (modified)
-- update for data v5 

flux_casa.pro (modified)
-- Bug fixed. The bug was initiated on February 2nd, 2021 which affected the 
   flux models of Ceres, Lutetia, Mars, Pallas, and Vesta. Specifically, the 
   old routine only read in the model flux up to a frequency of 330 GHz. 
   As a result, flux values from these sources at frequency above 330 GHz 
   would approximate the value at 330 GHz at the time stamp of the measurement.

mir2ms.pro (modified)
-- New keywords SIDEBAND, NOMS added. 
 

Nov 2022: MIR update:

mir2ms.pro and sma.py modified to allow for CASA6+ output and the polarization
state of the data set to be XX instead of RR.


Sep 2022: MIR update:

uti_hayshft_fix.pro, uti_redoppler.pro, uti_doppler_fix.pro, mir2ms.pro (modified)
-- double keyword not allowed for INTERPOLATE routine for IDL before version 8.2.3, removed.

Aug 2022: MIR update:

uti_hayshft_fix.pro, uti_redoppler.pro, uti_doppler_fix.pro, mir2ms.pro (modified)
-- double precision

uti_uvw_fix.pro, sma_dat_merge.pro, uti_avgband.pro, autofits.pro, phase_conjugate (modified)
-- bug fixes

Jun 2021: MIR update:

CASA MS output support:
-- CASA analysis utility package (added)
-- sma.py (added)
-- mir2ms.pro, transit_time.pro, ymdut_to_unixtime.pro (added)

uti_redoppler.pro (added)
-- to apply Doppler tracking corrections for the individual sources
   offline. 

ms_newformat.save, readdata.pro, uti_addhdr.pro (modified)
-- update for mir v4 data

Feb 2021: MIR update:

readdata.pro
-- remove signum function (not available before idl 8.3)

dbi_head2_read2.pro, dat_merge_sma.pro, uti_addhdr.pro (modified)
-- updated for mir v4 data 

dbi_doppler_fix.pro (modified)
-- updated for mir v3 data

flux_casa.pro, Ceres_fd_time.dat, Lutetia_fd_time.dat,
   Pallas_fd_time.dat, Vesta_fd_time.dat (modified)
-- updated for CASA 6.1 flux models

readtsys2.pro, uti_tsys_fix (modified), 
   readtsys_eng2.pro, uti_spectsys_fix.pro (added)
-- added spectsys and enginnering tsys reading option and spectsys fix

plo_spec.pro, plo_spec2.pro (modified)
-- updated for warning on keyword preavg in MHz


Oct 2020: MIR update:

readdata.pro (modified)
-- off by one channel fix, adding spectsys keyword, adding 
   uti_avgband at end.

readtsys_eng.pro, readeng.pro (added)
-- reading in eng tsys information.

plo_cont.pro, flux_cal.pro, fits_out.pro, gain.pro,
   uti_xgain.pro, flux_scale.pro, gain_sideband.pro (modified)
-- mir v3 update

uti_c1_fix.pro (added)
-- fix spectral continuum c1 if necessary.

Aug 2020: MIR update
readdata.pro, dbi_head2_read2.pro, dbi_chan2_read2.pro,
   dbi_chan2_read2int.pro, uti_respike.pro (modified)
-- updated the header information for mir v3. data. 
   fixed the "off by one" issue for all swarm data 
      taken after 2016 June 2nd.
   fixed the sky frequency problem caused by the
      "off by one" issue.

uti_avgband.pro, sma_cal_bas.pro, tsys_ant.pro, 
   tsys_bsl.pro, tsys_replace.pro, uti_uvw_fix.pro (modified)
-- updated with the new headers introduced for mir v3.data.

plo_spec.pro (modified)
-- adding c1 keyword to plot the spectral continuum data.

apply_we.pro (modified)
-- updated for refractivity calculation.


May 2020: MIR update

plo_page.pro, plo_spec.pro
-- remove boxy lines in spectra plotting for different sb/rx

dat_merge_sma.pro, uti_addhdr.pro
-- update data merging and header adding with mir v3 information.

Mar. 2020: MIR v3 release

readata.pro, dbi_head2_read2.pro (modified)
-- add new headers stype, svtype for sources and supply values
   for vrra, vrdec, lst, mjd, ara, adec for IN structures.
   uvw converted from meter to kilolambda.

uti_vel_fix.pro uti_uvw_fix.pro (modified)
-- fixed a few bugs.


Feb. 2019: MIR update
gain.pro, sma_flux_cal.pro, gain_sideband.pro, sma_flux_cal_ini.pro,
   gain_cal.pro, gain_ini.pro (modified)
-- CASA flux model used as default. keyword /orig can be used to 
   to load original flux model

readdata.pro, dbi_chan2_read2.pro (modified)
-- reduce the memory needed for data loading by a factor of two.
   increase the speed by a factor of 0.2 -- 2.

Dec. 2019: MIR v2 release 

SolarSystemModels (directory added from CASA 5.6.0 flux models)
flux_casa.pro  (added)
gain.pro, sma_flux_cal.pro, gain_sideband.pro, sma_flux_cal_ini.pro (modified)
-- CASA flux model added

plo_spec.pro (modified)
-- non-interactive 

flag.pro (modified)
-- added flagging option 'noallpols' in 'flag' routine to allow 
   better data output by flagging the scans without all four 
   polarization states.

uti_uvw_fix.pro (modified)
-- use apparent dec for uvw recalculation.

readdata.pro, mir_restore.pro, dbi_head2_read2.pro, dbi_chan2_read2.pro, 
    dat_merge_sma.pro (modified)
-- mir v2 change including flag marking (-32768), new header 'filever'

apply_tsys.pro (modified)
-- updated the normalization factor change due to mir v2 data change

dbi_chan2_read2int.pro (modified)
-- fixed a bug.

uti_avgband.pro (modified)
-- added exclude keyword.


Aug. 2019

readdata.pro (modified)
-- polarization correction debugged

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
