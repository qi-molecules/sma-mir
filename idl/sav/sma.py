import glob
import shutil
import os

# Casa tasks used: this is the CASA 5 version of importing (it is different in CASA 6)
from taskinit import *
from concat_cli import concat_cli as concat
from importuvfits_cli import importuvfits_cli as importuvfits

def concatSMAdataset(targetlist='allsources', 
                     finalvis='allsources', offset=100, 
                     ignoreContinuumSpws=True, timesort=True, overwrite=True, skipFinalConcat=False,
                     relabelAsTopo=True, importdata=True):
    """
    This function is meant to be run after you have run auto_fits in MIR/IDL for each source in 
    an SMA dataset that has been despiked and had tsys applied, and then imported each of the 
    resulting FITS files into measurement sets.  It will produce a fully concatenated ms, by
    first concatenating each spw of a target into a target ms, then concatenating all targets.
    The offset parameter is used to avoid collisions in the scan numbers between targets.
    targetlist: list of sourcenames (i.e. the initial characters of each per-source/per-spw input ms); either a python list or a comma-delimited list
    finalvis: name of final concatenated ms
    offset: first offset will be zero, subsequent will be 1*offset, 2*offset, etc.
    timesort: passed to concat task for final measurement set
    relabelAsTopo: default is True
    Todd Hunter   Nov-2019
    """
    if importdata:
        uvfits = glob.glob('*.UVFITS')
        for f in uvfits:
            if ignoreContinuumSpws and f.find('_C1_')>0:
                continue
            dataset = f.lower().replace('.uvfits','.ms.part')
            importuvfits(f, dataset)
            setMeasFreqRef(dataset)
    if type(targetlist) == str:
        targetlist = targetlist.split(',')
    outputvislist = []
    for i,target in enumerate(targetlist):
        concatvis = target + '_concat.ms.part'
        outputvis = target + '_concat_offset.ms.part'
        if not os.path.exists(concatvis) or overwrite:
            if os.path.exists(concatvis):
                shutil.rmtree(concatvis)
            if ignoreContinuumSpws:
                vislist = sorted(glob.glob(target+'*_s*.ms.part'))
            else:
                vislist = sorted(glob.glob(target+'*.ms.part'))
            print("Concatenating %d ms: %s" % (len(vislist), vislist))
            concat(vislist, concatvis=concatvis, timesort=False)
            setMeasFreqRef(concatvis) # relabels all spws as TOPO
        if not os.path.exists(outputvis) or overwrite:
            if os.path.exists(outputvis):
                shutil.rmtree(outputvis)
            myoffset = i*offset
            print("Finished %s now offsetting by %d" % (concatvis,myoffset))
            offsetScanNumbers(concatvis, offset=myoffset, outputvis=outputvis)
            print("Finished ", outputvis)
        outputvislist.append(outputvis)
    if not skipFinalConcat:
        if finalvis == '':
            finalvis = 'sma.ms'
        print("Doing final concatenation")
        concat(outputvislist, concatvis=finalvis, timesort=timesort)
        listobslist(finalvis, overwrite=True)
        return finalvis
    else:
        return concatvis

def setMeasFreqRef(vis, newcode=5, spw=''):
    """
    Uses the tb tool to change the MEAS_REF_CODE entries in the SPECTRAL_WINDOW_TABLE.
    newcode:  1 = LSRK, 4 = GEO, 5 = TOPO, etc.   integer or string integer
    spw: default=all, or integer or list or comma-delimited string
    """
    if not os.path.exists(vis):
        print("Could not find dataset.")
        return
    mytb = tbtool()
    newcode = int(newcode)
    mytb.open(os.path.join(vis,'SPECTRAL_WINDOW'), nomodify=False)
    if spw == '':
        spws = range(mytb.nrows())
    elif type(spw) == str:
        spws = [int(i) for i in spw.split(',')]
    elif type(spw) == list or type(spw) == np.ndarray:
        spws = spw
    else:
        spws = [spw]
    for spw in spws:
        oldcode = mytb.getcell('MEAS_FREQ_REF',spw)
        if oldcode != newcode:
            print("Changing spw %d from %d to %d" % (spw, oldcode, newcode))
            mytb.putcell('MEAS_FREQ_REF', spw, newcode)
    mytb.close()

def offsetScanNumbers(vis, offset=100, outputvis=''):
    """
    Adds a constant integer offset to the SCAN_NUMBER column of the main table.
    """
    if outputvis == '':
        outputvis = vis
    else:
        print("Copying %s to %s" % (vis,outputvis))
        shutil.copytree(vis, outputvis)
    mytb = createCasaTool(tbtool)
    mytb.open(outputvis, nomodify=False)
    scan = mytb.getcol('SCAN_NUMBER')
    scan += offset    
    mytb.putcol('SCAN_NUMBER', scan)
    mytb.close()
    return outputvis

def listobslist(vislist, suffix='.listobs', outpath='', overwrite=False, verbose=True, field=''):
    """
    Run listobs on a list of measurement sets
    vislist: either a list ['a.ms','b.ms'], a comma-delimited string, or a wildcard string e.g. '*.ms'
    outpath: if blank, write to same directory as measurement set, otherwise 
             write to specified directory, using basename of measurement set
    -Todd Hunter
    """
    if (type(vislist) == str):
        if vislist.find('*') >= 0:
            vislist = glob.glob(vislist)
        else:
            vislist = vislist.split(',')
        if len(vislist) == 0:
            print("No qualifying measurement sets found.")
    for vis in vislist:
        vis = vis.rstrip('/')
        if len(outpath) > 0:
            listfile = os.path.join(outpath,os.path.basename(vis)+suffix)
        else:
            listfile = vis+suffix
        if (not os.path.exists(listfile) or overwrite):
            if (os.path.exists(listfile)):
                os.remove(listfile)
            print("Running listobs('%s', listfile='%s', field='%s')" % (vis, listfile, field))
            listobs(vis, listfile=listfile, field=field)

