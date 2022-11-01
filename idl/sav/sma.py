import glob
import shutil
import os
import numpy as np

casaVersion = None
try: # Check if this is CASA6  CASA 6
    import casalith
    casaVersion = casalith.version_string()
except:
    # either we are importing into python, modular casa, or CASA < 6
    try:
        import casashell  # modular casa
        casaVersion = casashell.version_string()
    except: # either we are importing into python, or CASA < 6
        if (os.getenv('CASAPATH') is not None):
            import casadef
            if casadef.casa_version >= '5.0.0':
                import casa as mycasa
                if 'cutool' in dir(mycasa):
                    cu = mycasa.cutool()
                    casaVersion = '.'.join([str(i) for i in cu.version()[:-1]]) + '-' + str(cu.version()[-1])
                else:
                    casaVersion = mycasa.casa['build']['version'].split()[0]
            else:
                casaVersion = casadef.casa_version
        else: # this should never happen
            casaVersion = None
if casaVersion is not None:
    try:  # CASA 5.x
        from taskinit import *
        from concat_cli import concat_cli as concat
        from listobs_cli import listobs_cli as listobs
        from importuvfits_cli import importuvfits_cli as importuvfits
    except:  # casa 6
        if casaVersion >= '5.9.9': # 5.9.9 was the first attempt at 6.0
            from casatasks import concat
            from casatasks import listobs
            from casatasks import importuvfits
            print("Importing tbtool")
            from casatools import table as tbtool
            print("Done")

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
    mytb = tbtool()
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


def modifyCorrType(vis, oldCorrType=[5], newCorrType=[9]):
     """
     Set the CORR_TYPE column of the POLARIZATION table
     oldCorrType: determines which entries to change
     newCorrType: new value to write
     ptypes=['Undefined','I','Q','U','V','RR','RL','LR','LL','XX','XY','YX','YY']
     """
     if (not os.path.exists(vis)):
         print("Could not find measurement set.")
         return
     mytb = tbtool()
     mytb.open(vis+'/POLARIZATION', nomodify=False)
     corrType = mytb.getcol('CORR_TYPE')
     i = np.where(corrType == oldCorrType)
     corrType[i] = newCorrType
     print("Writing ", corrType)
     mytb.putcol('CORR_TYPE', corrType)
     mytb.close()
     listobslist(vis, overwrite=True)

