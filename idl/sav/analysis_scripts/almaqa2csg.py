# ALMA Quality Assurance Software
# QA2 Calibration Script Generator
# Eric Villard (JAO)
# Todd Hunter (NRAO)
# Dirk Petry (ESO)
# $Id: almaqa2csg.py,v 1.15 2021/04/06 16:13:12 dpetry Exp $
#
"""
The ALMA QA2 Calibration Script Generator
"""

from __future__ import print_function

import os
import sys
import numpy as np
import glob
import re
import time as timeUtilities
import pprint
import analysisUtils as aU
sfsdr = aU.stuffForScienceDataReduction()

try:  # Python 3
    import inspect
    from casatasks import importasdm
    from casatasks import gencal
    from casatasks import casalog
    from casatools import table as tbtool
    from casatools import msmetadata as msmdtool

    from urllib.parse import urlparse, urlencode
    from urllib.request import urlopen, Request, HTTPPasswordMgrWithDefaultRealm, HTTPBasicAuthHandler, build_opener
    
    from urllib.error import HTTPError
    from subprocess import getstatusoutput, getoutput
    import XmlObjectifier_python3 as XmlObjectifier
except ImportError:  # Python 2
    from taskinit import *
    from importasdm_cli import importasdm_cli as importasdm
    from gencal_cli import gencal_cli as gencal

    import exceptions

    import XmlObjectifier
    from urlparse import urlparse
    from urllib import urlencode
    from urllib2 import urlopen, Request, HTTPError, HTTPPasswordMgrWithDefaultRealm, HTTPBasicAuthHandler, build_opener
    from commands import getstatusoutput, getoutput


def version(short=False):
    """
    Returns the CVS revision number.
    """
    myversion = "$Id: almaqa2csg.py,v 1.15 2021/04/06 16:13:12 dpetry Exp $"
    if (short):
        myversion = myversion.split()[2]
    return myversion


def generateReducScript(msNames='', step='calib', corrAntPos=True, timeBinForFinalData=0., 
                        refant='', bpassCalId='', chanWid=1, angScale=0, run=False, lowSNR=False, 
                        projectCode='', schedblockName='', schedblockUid='', queue='', state='', 
                        upToTimeForState=2, useLocalAlmaHelper=False, tsysChanTol=1, sdQSOflux=1, 
                        runPhaseClosure=False, skipSyscalChecks=False, lazy=False, lbc=False, 
                        phaseDiff=False, remcloud=False, bdfflags=True, phaseDiffPerSpwSetup=False, 
                        tsysPerField=False, splitMyScienceSpw=True, bpassCalTableName='', 
                        reindexMyScienceSpw=False, useCalibratorService=False):
    """
    The ALMA QA2 calibration script generator

    msNames: a string or a list of strings of UIDs (either ASDM or MS) to process
              default=''
    step:    calib, fluxcal, wvr, calsurvey, SDeff, SDcalibLine, SDcalibCont, SDscience, SDampcal
              default='calib'
    corrAntPos: if True, then run correctMyAntennaPositions
              default=True
    timeBinForFinalData: a value in seconds (string, int, or float), passed to split
              default=0.
    refant:  the reference antenna to use (instead of automatic selection), must be a string
              default='', i.e. determine automatically
    bpassCalId: use the specified source for bandpass (rather than determine from the intents)
              default='', i.e. determine from the intents
    chanWid: integer, used by runCleanOnSource and searchForLines
              default=1
    angScale: deprecated

    run:     deprecated

    lowSNR:  Boolean passed to doBandpassCalibration to use whole spw for pre-bandpass phase-up
              default=False
    projectCode, schedblockName, queue, state, upToTimeForState: deprecated

    useLocalAlmaHelper: if True, run tsysspwmap inside generator, rather than in the resulting script
              default=False
    tsysChanTol: integer argument passed to tsysspwmap
              default=1
    sdQSOflux: flux density to use for quasar in single dish case (step='SDeff')
              default=1
    runPhaseClosure: deprecated

    skipSyscalChecks: if True, then don't check for negative Tsys problems
              default=False
    lazy: value of the 'lazy' parameter in importasdm. If True, reference the ASDM instead
            of copying the visibilities into the DATA column of the MS. Saves disk space.
              default=False
    lbc:    if True, invoke long-baseline campaign usage in correctMyAntennaPositions, i.e.
              search='both_latest' and  maxSearchDays=30;
            secondly, in bandpass calibration, use solint='inf,8MHz' instead of 'inf,20ch'
              default=False
    remcloud: runs the recipe remove_cloud prior to running wvrgcal
              default=False
    bdfflags: passed to importasdm to invoke the application of BDF flags
              default=True
    bpassCalTableName: to use instead of default name
              default='', i.e. use the bp table created for bpassCalId with the standard naming
    phaseDiff: force the combination of spw. If it is not enabled, and a DGC scan is detected, then it will 
              automatically enable it.
              default=False
    phaseDiffPerSpwSetup:  controls the iteration loop for the calculation of gaincal table with spw combined. 
              If False, the iteration will be done over the fields; if True, the iteration will be done over 
              the groups of spw from the same setup.
              default=False
    tsysPerField: passed to the perField parameter of tsysspwmap
              default=False
    splitMyScienceSpw: In the final split-out, only include the SPWs corresponding to intent OBSERVE_TARGET
              and BANDPASS.
              default=True
    reindexMyScienceSpw: perform reindexing in the split out after the apriori calibration.
              default=False
    useCalibratorService: if True, then, in the call to aU.getALMAFluxForMS in the setjy step, use
              aU.calibratorService(), otherwise use aU.getALMAFlux()
              default=False
    """

    print("The ALMA QA2 calibration script generator")
    print(version())
    print('using '+aU.version())
    casalog.post(version(),'INFO')
    casalog.post('using '+aU.version(),'INFO')

    mycasaversion = aU.getCasaVersion()

    if mycasaversion < '4.7.2':
        casalog.post('CASA versions < 4.7.2 are no longer supported.', 'SEVERE')

    latestValidatedVersion = '6.1.1' # 2020-02-01
    if re.search('^'+latestValidatedVersion, mycasaversion) == None:
        print('WARNING: You are currently running CASA %s rather than CASA %s.' % (mycasaversion, latestValidatedVersion))
        print('WARNING: If you observe any issue, please send an email to Dirk Petry (dpetry@eso.org) or file an ALMA PRTSPR ticket.')


    mytb = aU.createCasaTool(tbtool)


    ######################################
    # parse input parameters

    # useLocalAlmaHelper
    if (useLocalAlmaHelper):
        try:
            from almahelpers_localcopy import tsysspwmap
        except:
            if mycasaversion < '5.9.9':
                from recipes.almahelpers import tsysspwmap
            else:
                from casarecipes.almahelpers import tsysspwmap
    else:
        if mycasaversion < '5.9.9':
            from recipes.almahelpers import tsysspwmap
        else:
            from casarecipes.almahelpers import tsysspwmap

    # step
    availableSteps = ['calib', 
                      'fluxcal', 
                      'wvr', 
                      'calsurvey', 
                      'SDeff', 
                      'SDcalibLine', 
                      'SDcalibCont', 
                      'SDscience', 
                      'SDampcal']
    if step not in availableSteps:
        casalog.post("Step "+str(step)+" not valid.  Available values: "+str(availableSteps), 'SEVERE')
        return False

    if step in ['SDcalibLine', 'SDcalibCont', 'SDampcal', 'SDscience']:
        with_pointing_correction = True
    else:
        with_pointing_correction = False

    # refant
    if (type(refant) != str):
        casalog.post("refant must be a string", 'SEVERE')
        return False

    # phaseDiffPerSpwSetup
    if phaseDiffPerSpwSetup:
        casalog.post("phaseDiffPerSpwSetup is enabled.", 'INFO')
        
    # splitMyScienceSpw, reindexMyScienceSpw
    if splitMyScienceSpw and (not reindexMyScienceSpw) and mycasaversion < '5.4':
        casalog.post("splitMyScienceSpw = True and reindexMyScienceSpw = False is not supported in CASA versions < 5.4", 'SEVERE')
        return False

    # state
    if state != '':
        casalog.post('The parameter "state" is no longer supported. Please set it to empty string.', 'SEVERE')
        return False

    # queue
    if queue != '':
        casalog.post('The parameter "queue" is no longer supported. Please set it to empty string.', 'SEVERE')
        return False

    # msNames
    if type(msNames) == str: 
        msNames = [msNames]

    # projectCode, schedblockName, schedblockUid, step
    if ((projectCode != '' and schedblockName != '') or schedblockUid != '') and step in ['calib', 'SDscience']:
        casalog.post('Automatic EB determination based on projectCode and schedblockName or schedblockUid is no longer supported.\n'\
                     +'Please set these parameters to empty string.', 'SEVERE')
        return False

    # end initial parameter parsing
    ##################################

    currDir = os.getcwd()

    asis1 = 'Antenna Station Receiver Source CalAtmosphere CalWVR CorrelatorMode SBSummary'

    asdmNames = []
    for msName in msNames:
        myname = msName.rstrip(os.path.sep)
        if re.search('^uid\:\/\/[0-9a-z]+\/[0-9a-z]+\/[0-9a-z]+$', myname, re.IGNORECASE) is not None: # this is an ASDM name with slashes
            myname = re.sub(':|/', '_', myname) # convert to underscore notation

        if myname[-3:] == '.ms': # this has an ms extension    
            myname = myname[0:-3] # remove it
        
        if re.search('^uid___[0-9a-z]+_[0-9a-z]+_[0-9a-z]+$', myname, re.IGNORECASE) is not None: # this is now an ASDM name with underscores
            if not os.path.exists(myname) and not os.path.exists(myname+'.ms') and not os.path.exists('../'+myname):
                casalog.post('ERROR: '+myname+': Neither the asdm nor the ms exists. Please retrieve the ASDM before starting generateReducScript.', 'SEVERE')
                return False
            else:
                asdmNames.append(myname)
        else:
            casalog.post('ERROR: '+msName+' does not follow the ASDM naming convention.', 'SEVERE')
            return False

    print('Identified ASDMs '+str(asdmNames))

    msNames=[]
    for asdmName in asdmNames: # create the MS for each ASDM if it doesn't exist, yet

        msName = asdmName+'.ms'
        msNames.append(msName)

        corrModes1 = ''

        if not os.path.exists(msName): # create the MS

            if not os.path.exists(asdmName):
                asdmName='../'+asdmName
                if not os.path.exists(asdmName):
                    casalog.post('ERROR: '+msName+': Neither the asdm nor the ms exists. Please retrieve the ASDM before starting generateReducScript.', 'SEVERE')
                    return False

            print('NOTE: The asdm '+asdmName+' exists, but the ms does not exist, running importasdm.')

            f1 = open(asdmName+'/CorrelatorMode.xml')
            fc1 = f1.read()
            f1.close()

            corrModes = re.findall('<correlatorName>.*?</correlatorName>', fc1, re.IGNORECASE)
            if len(corrModes) == 0: return False
            corrModes1 = []
            for j in range(len(corrModes)):
                corrModes1.append(corrModes[j].replace('<correlatorName>', '').replace('</correlatorName>', ''))
            corrModes1 = np.unique(corrModes1).tolist()
            if len(corrModes1) != 1: return False
            corrModes1 = corrModes1[0]
            if corrModes1 not in ['ALMA_ACA', 'ALMA_BASELINE']:
                casalog.post('ERROR: Correlator name in CorrelatorMode table must be ALMA_ACA or ALMA_BASELINE.', 'SEVERE')
                return False

            if corrModes1 == 'ALMA_ACA' and mycasaversion < '5.9.9':
                # I don't think this was ever necessary since CAS-8995 was fixed by 4.7.1, but bdflags2MS is no longer in CASA6. - TRH
                importasdm(asdmName, vis=msName, asis=asis1, bdfflags=False, lazy=lazy, process_caldevice=False, with_pointing_correction=with_pointing_correction)
                if bdfflags == True:
                    casalog.post('This is an ACA dataset: applying the BDF flags separately.', 'WARN')
                    os.system(os.environ['CASAPATH'].split()[0]+'/bin/bdflags2MS -f "COR DELA INT MIS SIG SYN TFB WVR ZER" '+asdmName+' '+msName+'.ms')
            else:
                importasdm(asdmName, vis=msName, asis=asis1, bdfflags=bdfflags, lazy=lazy, process_caldevice=False, with_pointing_correction=with_pointing_correction)

        if corrModes1 == '':
            mytb.open(msName)
            keywordnames1 = mytb.keywordnames()
            mytb.close()

            if 'ASDM_CORRELATORMODE' not in keywordnames1:
            
                print('\n\n\nWARNING: You have not imported the CorrelatorMode table from the ASDM.')
                print('WARNING: If this is a 12m array dataset, then it is Ok. Otherwise, please regenerate the MS '+msName+' using the script generator.\n\n\n')

            else:

                mytb.open(msName+'/ASDM_CORRELATORMODE')
                corrModes1 = np.unique(mytb.getcol('correlatorName'))
                mytb.close()

                if len(corrModes1) != 1: 
                    return False
                if corrModes1 == 'ALMA_ACA':
                    print('\n\n\nWARNING: This is a 7m/TP array dataset. It seems you have not used the script generator to generate the MS.')
                    print('WARNING: Please note that the application of the BDF flags is particular. It is recommended that you use the script generator to generate the MS.\n\n\n')


    msNames = sorted(msNames)
    valueMaps = {}

    for msName in msNames:

        if step not in ['fluxcal']: sfsdr.fixForCSV2555(msName)

        if os.getlogin == 'aod':
            sfsdr.listOfIntentsWithSources(msName)
            sfsdr.listobs3(msName, figfile=msName+'.listobs3.png')
            sfsdr.plotAntennas(msName)

        spwInfo = sfsdr.getSpwInfo(msName, caching=True)
        spwIds = sorted(spwInfo.keys())
        print('Value mapping for MS '+msName+' ...')
        vm = aU.ValueMapping(msName)
        valueMaps[msName] = vm
        spwScans = vm.getScansForSpw(spwIds[0]).tolist()
        for j in spwIds:
            if vm.getScansForSpw(j).tolist() != spwScans:
                print('WARNING: The scans are not the same for all science spws.')
                print('WARNING: The script generator is not compatible with this, it will very likely fail.')
                print('WARNING: If it does not fail, do not expect the reduction script to be good. Please check it carefully.')


        ### determine if B2B or BWSW dataset or none of the above

        isB2B, isBWSW = isB2BorBWSW(msName, valueMaps)


    ##### step 'wvr' #####
    if step == 'wvr':

        for msName in msNames:

            f1 = open(msName+'.scriptForWVRCalibration.py', 'w')
            print("import re\n", file=f1)
            print("es = aU.stuffForScienceDataReduction() \n\n", file=f1)
            if mycasaversion < '5.1':
                print("if re.search('^"+re.findall('^[0-9]+.[0-9]+.[0-9]+', mycasaversion)[0]+"', casaVersion) == None:", file=f1)
            elif mycasaversion < '5.9':
                print("if re.search('^"+re.findall('^[0-9]+.[0-9]+.[0-9]+', mycasaversion)[0]+"', '.'.join([str(i) for i in cu.version().tolist()[:-1]])) == None:", file=f1)
            else:
                print("if re.search('^"+re.findall('^[0-9]+.[0-9]+.[0-9]+', mycasaversion)[0]+"', '.'.join([str(i) for i in casalith.version()[:-1]])) == None:", file=f1)
            print(" sys.exit('ERROR: PLEASE USE THE SAME VERSION OF CASA THAT YOU USED FOR GENERATING THE SCRIPT: "+re.findall('^[0-9]+.[0-9]+.[0-9]+', mycasaversion)[0]+"')\n\n", file=f1)

            myRefAnt = refant
            if myRefAnt == '': myRefAnt = sfsdr.getRefAntenna(msName)
            print("# Using reference antenna = "+myRefAnt+"\n", file=f1)

            print(doAprioriFlagging(msName, valueMaps=valueMaps), file=f1)
            print(doGenerateWVRCalTable(msName, valueMaps=valueMaps), file=f1)
            print("es.wvr_stat(ms1='"+msName+"', refAnt='"+myRefAnt+"', qa2_output_dir='./')\n", file=f1)

            f1.close()

    ##### step 'calib' #####
    if step in ['calib']:

        for msName in msNames:

            mystepdict = {}
            mystepindent = "  "

            tsysmap = ''
            if re.search('^3.3', mycasaversion) == None and skipSyscalChecks == False: # do Syscall checks

                print("\n*** ANALYSIS OF TSYS TABLE ***")

                print("\n*** SEARCH FOR NEGATIVE TSYS ***")
                aU.detectNegativeTsys(vis = msName, edge = 8, showfield = True)

                print("\n*** SEARCH FOR NEGATIVE TREC ***")
                aU.detectNegativeTrx(vis = msName, edge = 8, showfield = True)

                os.system('rm -Rf '+msName+'.tsys.temp')
                gencal(vis = msName, caltable = msName+'.tsys.temp', caltype = 'tsys')

                print("\n*** SEARCH FOR MISSING SCANS IN SYSCAL TABLE ***")
                mymsmd = msmdtool()
                mymsmd.open(msName)

                scans1 = sorted(mymsmd.scansforintent('CALIBRATE_ATMOSPHERE#*').tolist()) # T. Hunter 2014-08-14
                # There are cases, e.g. uid___A002_X7ea111_Xc03.ms where the OFF_SOURCE intent is present
                # in the ms but the ON_SOURCE is not, but the corresponding Tsys value is present.
                # As of 4.2.0, the forintent methods of msmd accept the wildcard character.

                mymsmd.close()

                mytb.open(msName+'.tsys.temp')                    
                scans2 = sorted(np.unique(mytb.getcol('SCAN_NUMBER')).tolist())
                mytb.close()

                if len(scans1) == 0 or len(scans2) == 0 or scans1 != scans2:
                    print("len(scans1)=%d, len(scans2)=%d" % (len(scans1), len(scans2)))
                    casalog.post('ERROR: THE SYSCAL TABLE IS MISSING ONE (OR MORE) SCAN(S). IT MAY BE NECESSARY TO RE-GENERATE IT.', 'SEVERE')
                    return False
                else:
                    print("-> OK")

                if useLocalAlmaHelper == True:
                    tsysmap = tsysspwmap(vis = msName, tsystable = msName+'.tsys.temp', tsysChanTol=tsysChanTol)
                    if msName in valueMaps.keys():
                        vm = valueMaps[msName]
                        print('Using canned ValueMap.')
                    else:
                        vm = aU.ValueMapping(msName)
                        valueMaps[msName] = vm

                    spwInfo = sfsdr.getSpwInfo(msName, caching=True)
                    spwIds = sorted(spwInfo.keys())
                    for j in spwIds:
                        spwIntents = vm.getIntentsForSpw(tsysmap[j])
                        if 'CALIBRATE_ATMOSPHERE#ON_SOURCE' not in spwIntents:
                            casalog.post('ERROR: INCOMPLETE TSYS SPW MAPPING!', 'SEVERE')
                            return False

                os.system('rm -Rf '+msName+'.tsys.temp')

            ##############################
            # Start the calibration script
            f1 = open(msName+'.scriptForCalibration.py', 'w')

            print("import re\n", file=f1)
            print("import os\n", file=f1)
            if (mycasaversion < '5.9'):
                print("import casadef\n", file=f1)
            else:
                print("import casalith\n", file=f1)
            print("if applyonly != True: es = aU.stuffForScienceDataReduction() \n\n", file=f1)
            if mycasaversion < '5.1':
                print("if re.search('^"+re.findall('^[0-9]+.[0-9]+.[0-9]+', mycasaversion)[0]+"', casaVersion) is None:", file=f1)
            elif mycasaversion < '5.9':
                print("if re.search('^"+re.findall('^[0-9]+.[0-9]+.[0-9]+', mycasaversion)[0]+"', '.'.join([str(i) for i in cu.version().tolist()[:-1]])) == None:", file=f1)
            else:
                print("if re.search('^"+re.findall('^[0-9]+.[0-9]+.[0-9]+', mycasaversion)[0]+"', '.'.join([str(i) for i in casalith.version()[:-1]])) == None:", file=f1)
            print(" sys.exit('ERROR: PLEASE USE THE SAME VERSION OF CASA THAT YOU USED FOR GENERATING THE SCRIPT: "+re.findall('^[0-9]+.[0-9]+.[0-9]+', mycasaversion)[0]+"')\n\n", file=f1)

            # add the list of intents as comments
            tempStdout = sys.stdout
            sys.stdout = f1
            sfsdr.listOfIntentsWithSources(msName)
            sys.stdout = tempStdout

            # determine reference antenna
            myRefAnt = refant
            if myRefAnt == '': myRefAnt = sfsdr.getRefAntenna(msName, lbc=lbc)
            print("\n# Using reference antenna = "+myRefAnt, file=f1)

            isFullP = isFullPol(msName, valueMaps)

            if isFullP:
                print("NOTE: This is a full polarisation dataset.")
                print("#     (will use it strictly since this is a full polarisation dataset)\n", file=f1)
            else:
                print("", file=f1)
                

            ### add importasdm step
            stext = "if os.path.exists('"+msName+"') == False:\n"

            if corrModes1 == 'ALMA_ACA' and mycasaversion < '5.9.9':
                # I don't think this was ever necessary since CAS-8995 was fixed by 4.7.1, but bdflags2MS is no longer in CASA6. - TRH
                stext += "  importasdm('"+re.findall('.+(?=\.ms)', msName, re.IGNORECASE)[0]+"', asis='"+asis1+"', bdfflags=False, lazy="+str(lazy)+", process_caldevice=False)"
                if bdfflags == True:
                    stext += "\n  os.system(os.environ['CASAPATH'].split()[0]+'/bin/bdflags2MS -f \"COR DELA INT MIS SIG SYN TFB WVR ZER\" "+re.findall('.+(?=\.ms)', msName, re.IGNORECASE)[0]+" "+msName+"')"
            else:
                stext += "  importasdm('"+re.findall('.+(?=\.ms)', msName, re.IGNORECASE)[0]+"', asis='"+asis1+"', bdfflags="+str(bdfflags)+", lazy="+str(lazy)+", process_caldevice=False)"


            stext += "\n  if not os.path.exists('"+msName+".flagversions'):\n    print('ERROR in importasdm. Output MS is probably not useful. Will stop here.')\n    thesteps = []"

            stext += "\nif applyonly != True: es.fixForCSV2555('"+msName+"')"
            sfsdr.addReducScriptStep(f1, mystepdict, "Import of the ASDM", stext, mystepindent, applyonly=True)

            ### add fixsyscaltimes step: see CAS-4981, CSV-2841, ICT-3642; sometimes needed for cycle 0-2.
            if mycasaversion < '5.9.9':
                stext = "from recipes.almahelpers import fixsyscaltimes\nfixsyscaltimes(vis = '"+msName+"')"
            else:
                stext = "from casarecipes.almahelpers import fixsyscaltimes\nfixsyscaltimes(vis = '"+msName+"')"
            sfsdr.addReducScriptStep(f1, mystepdict, "Fix of SYSCAL table times", stext, mystepindent, applyonly=True)

            print('print("# A priori calibration")\n', file=f1)

            ### add fixplanets step if needed 
            stext = doRunFixPlanets(msName)
            if stext is not None: 
                sfsdr.addReducScriptStep(f1, mystepdict, "Running fixplanets on fields with 0,0 coordinates", stext, mystepindent, applyonly=True)
            else:
                print('No (0,0) coordinates found. fixplanets step not needed.')

            ### add listobs step
            stext = "os.system('rm -rf %s.listobs')\n" %(msName) # Added by CLB
            stext += "listobs(vis = '"+msName+"',\n  listfile = '"+msName+".listobs')\n\n" # Modified by CLB
            sfsdr.addReducScriptStep(f1, mystepdict, "listobs", stext, mystepindent)

            ### add apriori flagging step
            stext = doAprioriFlagging(msName, valueMaps=valueMaps)
            sfsdr.addReducScriptStep(f1, mystepdict, "A priori flagging", stext, mystepindent)

            ### add wvrgcal step
            wvrCalTableName = []
            stext = doGenerateWVRCalTable(msName, wvrCalTableName, refant=myRefAnt, remcloud=remcloud, valueMaps=valueMaps, isB2B=isB2B)
            sfsdr.addReducScriptStep(f1, mystepdict, "Generation and time averaging of the WVR cal table", stext, mystepindent)

            ### add Tsys step
            tsysCalTableName = []
            stext = doGenerateTsysCalTable(msName, tsysCalTableName, isB2B=isB2B)
            sfsdr.addReducScriptStep(f1, mystepdict, "Generation of the Tsys cal table", stext, mystepindent)

            ### add antpos (if needed) and first applycal step
            if corrAntPos == True:
                stext = sfsdr.correctMyAntennaPositions(msName, lbc=lbc)
                if stext is not None:
                    sfsdr.addReducScriptStep(f1, mystepdict, "Generation of the antenna position cal table", stext, mystepindent)
                    stext = doApplyAprioriCalTables(msName, tsys=tsysCalTableName[0], wvr=wvrCalTableName[0], antpos=msName+'.antpos', tsysmap=tsysmap, tsysChanTol=tsysChanTol, tsysPerField=tsysPerField, valueMaps=valueMaps)
                    sfsdr.addReducScriptStep(f1, mystepdict, "Application of the WVR, Tsys and antpos cal tables", stext, mystepindent, applyonly=True)
                else:
                    stext = doApplyAprioriCalTables(msName, tsys=tsysCalTableName[0], wvr=wvrCalTableName[0], tsysmap=tsysmap, tsysChanTol=tsysChanTol, tsysPerField=tsysPerField, valueMaps=valueMaps)
                    sfsdr.addReducScriptStep(f1, mystepdict, "Application of the WVR and Tsys cal tables", stext, mystepindent, applyonly=True)
            else:
                stext = doApplyAprioriCalTables(msName, tsys=tsysCalTableName[0], wvr=wvrCalTableName[0], tsysmap=tsysmap, tsysChanTol=tsysChanTol, tsysPerField=tsysPerField, valueMaps=valueMaps)
                sfsdr.addReducScriptStep(f1, mystepdict, "Application of the WVR and Tsys cal tables", stext, mystepindent, applyonly=True)

            ### add splitout step
            stext = doSplitOut(msName, splitMyScienceSpw=splitMyScienceSpw, timebin=timeBinForFinalData, reindexMyScienceSpw=reindexMyScienceSpw)
            sfsdr.addReducScriptStep(f1, mystepdict, "Split out science SPWs and time average", stext, mystepindent, applyonly=True)


            #### end of apriori calibration steps generation ####

            print('print("# Calibration")\n', file=f1)

            ### add listobs step
            stext = "os.system('rm -rf %s.split.listobs')\n" % (msName) # Added by CLB
            stext += "listobs(vis = '"+msName+".split',\n  listfile = '"+msName+".split.listobs')\n\n"
            stext += doSaveFlags(msName+'.split', name='Original')
            sfsdr.addReducScriptStep(f1, mystepdict, "Listobs, and save original flags", stext, mystepindent)

            ### add initial flagging step
            stext = doInitialFlagging(msName, msName1=msName+'.split', iHaveSplitMyScienceSpw=reindexMyScienceSpw)
            sfsdr.addReducScriptStep(f1, mystepdict, "Initial flagging", stext, mystepindent)

            ### add setjy step
            stext = doRunSetjy(msName, msName1=msName+'.split', iHaveSplitMyScienceSpw=reindexMyScienceSpw, useCalibratorService=useCalibratorService, valueMaps=valueMaps)
            sfsdr.addReducScriptStep(f1, mystepdict, "Putting a model for the flux calibrator(s)", stext, mystepindent)

            ### add bandpass step
            if bpassCalTableName == '':
                stext = doSaveFlags(msName+'.split', name='BeforeBandpassCalibration')
                sfsdr.addReducScriptStep(f1, mystepdict, "Save flags before bandpass cal", stext, mystepindent, applyonly=True)
                bpassCalTableName = []
                stext = doBandpassCalibration(msName, msName1=msName+'.split', iHaveSplitMyScienceSpw=reindexMyScienceSpw, 
                                              refant=myRefAnt, calTableName=bpassCalTableName, lowSNR=lowSNR, lbc=lbc, 
                                              phaseDiff=phaseDiff, isB2B=isB2B, isBWSW=isBWSW, isFullP=isFullP, 
                                              valueMaps=valueMaps)
                sfsdr.addReducScriptStep(f1, mystepdict, "Bandpass calibration", stext, mystepindent)
            else:
                bpassCalTableName = [bpassCalTableName]

            ### add saveflags step
            stext = doSaveFlags(msName+'.split', name='BeforeGainCalibration')
            sfsdr.addReducScriptStep(f1, mystepdict, "Save flags before gain cal", stext, mystepindent, applyonly=True)

            ### add gain calibration step
            phaseDiffCalTableName = []
            ampForSci = []
            if isB2B:
                stext = doB2BGainCalibrationPartI(msName, msName1=msName+'.split', iHaveSplitMyScienceSpw=reindexMyScienceSpw, refant=myRefAnt, 
                                                  bandpass=bpassCalTableName[0], valueMaps=valueMaps)
                sfsdr.addReducScriptStep(f1, mystepdict, "B2B Gain calibration Part I", stext, mystepindent)

                stext = doB2BGainCalibrationPartII(msName, msName1=msName+'.split', iHaveSplitMyScienceSpw=reindexMyScienceSpw, refant=myRefAnt, 
                                                   bandpass=bpassCalTableName[0], ampForSci=ampForSci, 
                                                   valueMaps=valueMaps)
                sfsdr.addReducScriptStep(f1, mystepdict, "B2B Gain calibration Part II", stext, mystepindent)

            else:
                stext = doGainCalibration(msName, msName1=msName+'.split', iHaveSplitMyScienceSpw=reindexMyScienceSpw, refant=myRefAnt, 
                                          bandpass=bpassCalTableName[0], phaseDiffCalTableName=phaseDiffCalTableName, ampForSci=ampForSci, 
                                          phaseDiff=phaseDiff, phaseDiffPerSpwSetup=phaseDiffPerSpwSetup, 
                                          isBWSW=isBWSW,
                                          valueMaps=valueMaps)
                sfsdr.addReducScriptStep(f1, mystepdict, "Gain calibration", stext, mystepindent)

            ### add safeflags step
            stext = doSaveFlags(msName+'.split', name='BeforeApplycal')
            sfsdr.addReducScriptStep(f1, mystepdict, "Save flags before applycal", stext, mystepindent, applyonly=True)

            ### add final applycal step
            if isB2B:
                stext = doApplyB2BBandpassAndGainCalTables(msName, msName1=msName+'.split', iHaveSplitMyScienceSpw=reindexMyScienceSpw, 
                                                           bandpass=bpassCalTableName[0], valueMaps=valueMaps)
            else:
                stext = doApplyBandpassAndGainCalTables(msName, msName1=msName+'.split', iHaveSplitMyScienceSpw=reindexMyScienceSpw, 
                                                        bandpass=bpassCalTableName[0], phaseForCal=msName+'.split.phase_int', 
                                                        phaseForSci=msName+'.split.phase_inf', flux=msName+'.split.flux_inf', 
                                                        phaseDiffCalTableName=phaseDiffCalTableName, ampForSci=ampForSci, 
                                                        phaseDiffPerSpwSetup=phaseDiffPerSpwSetup, valueMaps=valueMaps)

            sfsdr.addReducScriptStep(f1, mystepdict, "Application of the bandpass and gain cal tables", stext, mystepindent, applyonly=True)

            ### add final splitout step 
            stext = doSplitOut(msName, msName1=msName+'.split', outMsName=msName+'.split.cal', allowHybrid=False, intentsToDiscard='ATMOSPHERE|POINTING', iHaveSplitMyScienceSpw=reindexMyScienceSpw)
            sfsdr.addReducScriptStep(f1, mystepdict, "Split out corrected column", stext, mystepindent, applyonly=True)

            ### add final flagsave step
            stext = doSaveFlags(msName+'.split.cal', name='AfterApplycal')
            sfsdr.addReducScriptStep(f1, mystepdict, "Save flags after applycal", stext, mystepindent, applyonly=True)

            ### finish script by adding header
            sfsdr.prependReducScriptHeader(f1, mystepdict, "Created using "+version(), mystepindent)

            f1.close()


    ########################################

    if step == 'fluxcal':

        if os.path.exists('allFluxes.txt') == False:
            sfsdr.generateFluxFile(msNames)
        else:
            print('File allFluxes.txt already exists, it will be loaded.')

        myRefAnt = refant # note: no need to run getRefAntenna because it will be called in doFluxCalibration if refant=''
        f1 = open('scriptForFluxCalibration.py', 'w')
        print("import re\n", file=f1)
        if mycasaversion < '5.1':
            print("if re.search('^"+re.findall('^[0-9]+.[0-9]+.[0-9]+', mycasaversion)[0]+"', casaVersion) == None:", file=f1)
        elif mycasaversion < '5.9':
            print("if re.search('^"+re.findall('^[0-9]+.[0-9]+.[0-9]+', mycasaversion)[0]+"', '.'.join([str(i) for i in cu.version().tolist()[:-1]])) == None:", file=f1)
        else:
            print("if re.search('^"+re.findall('^[0-9]+.[0-9]+.[0-9]+', mycasaversion)[0]+"', '.'.join([str(i) for i in casalith.version()[:-1]])) == None:", file=f1)
        print(" sys.exit('ERROR: PLEASE USE THE SAME VERSION OF CASA THAT YOU USED FOR GENERATING THE SCRIPT: "+re.findall('^[0-9]+.[0-9]+.[0-9]+', mycasaversion)[0]+"')\n\n", file=f1)
        print(doFluxCalibration(msNames, refant=myRefAnt), file=f1)
        f1.close()

    ######################################


    if step == 'calsurvey':
        mystepdict = {}
        mystepindent = "  "

        for msName in msNames:

            tsysmap = ''
            if re.search('^3.3', mycasaversion) == None and skipSyscalChecks == False:

                print("\n*** ANALYSIS OF TSYS TABLE ***")

                print("\n*** SEARCH FOR NEGATIVE TSYS ***")
                aU.detectNegativeTsys(vis = msName, edge = 8, showfield = True)

                print("\n*** SEARCH FOR NEGATIVE TREC ***")
                aU.detectNegativeTrx(vis = msName, edge = 8, showfield = True)

                os.system('rm -Rf '+msName+'.tsys.temp')
                gencal(vis = msName, caltable = msName+'.tsys.temp', caltype = 'tsys')

                print("\n*** SEARCH FOR MISSING SCANS IN SYSCAL TABLE ***")

                mymsmd = msmdtool()
                mymsmd.open(msName)

                scans1 = mymsmd.scansforintent('CALIBRATE_ATMOSPHERE*')

                mymsmd.close()

                mytb.open(msName+'.tsys.temp')                    
                scans2 = np.unique(mytb.getcol('SCAN_NUMBER'))
                mytb.close()

                if (scans1 == scans2).all():
                    print("-> OK")
                else:
                    casalog.post('ERROR: THE SYSCAL TABLE IS MISSING ONE (OR MORE) SCAN(S). IT MAY BE NECESSARY TO RE-GENERATE IT.', 'SEVERE')
                    return False

                if useLocalAlmaHelper == True:
                    tsysmap = tsysspwmap(vis = msName, tsystable = msName+'.tsys.temp', tsysChanTol=tsysChanTol)
                    if msName in valueMaps.keys():
                        vm = valueMaps[msName]
                        print('Using canned ValueMap.')
                    else:
                        vm = aU.ValueMapping(msName)
                        valueMaps[msName] = vm

                    spwInfo = sfsdr.getSpwInfo(msName, caching=True)
                    spwIds = sorted(spwInfo.keys())
                    for j in spwIds:
                        spwIntents = vm.getIntentsForSpw(tsysmap[j])
                        if 'CALIBRATE_ATMOSPHERE#ON_SOURCE' not in spwIntents:
                            casalog.post('ERROR: INCOMPLETE TSYS SPW MAPPING!', 'SEVERE')
                            return False

                os.system('rm -Rf '+msName+'.tsys.temp')

            f1 = open(msName+'.scriptForCalibration.py', 'w')
            print("import re\n", file=f1)
            print("es = aU.stuffForScienceDataReduction() \n\n", file=f1)
            if mycasaversion < '5.1':
                print("if re.search('^"+re.findall('^[0-9]+.[0-9]+.[0-9]+', mycasaversion)[0]+"', casaVersion) == None:", file=f1)
            elif mycasaversion < '5.9':
                print("if re.search('^"+re.findall('^[0-9]+.[0-9]+.[0-9]+', mycasaversion)[0]+"', '.'.join([str(i) for i in cu.version().tolist()[:-1]])) == None:", file=f1)
            else:
                print("if re.search('^"+re.findall('^[0-9]+.[0-9]+.[0-9]+', mycasaversion)[0]+"', '.'.join([str(i) for i in casalith.version()[:-1]])) == None:", file=f1)
            print(" sys.exit('ERROR: PLEASE USE THE SAME VERSION OF CASA THAT YOU USED FOR GENERATING THE SCRIPT: "+re.findall('^[0-9]+.[0-9]+.[0-9]+', mycasaversion)[0]+"')\n\n", file=f1)

            myRefAnt = refant
            if myRefAnt == '': myRefAnt = sfsdr.getRefAntenna(msName)
            print("# Using reference antenna = "+myRefAnt+"\n", file=f1)

            print('print("# A priori calibration")\n', file=f1)
            stext = doRunFixPlanets(msName)
            if stext is not None: sfsdr.addReducScriptStep(f1, mystepdict, "Running fixplanets on fields with 0,0 coordinates", stext, mystepindent)

            stext = "os.system('rm -rf %s.listobs')\n" %(msName) # Added by CLB
            stext += "listobs(vis = '"+msName+"',\n  listfile = '"+msName+".listobs')\n\n" # Modified by CLB
            sfsdr.addReducScriptStep(f1, mystepdict, "listobs", stext, mystepindent)
            stext = doAprioriFlagging(msName, valueMaps=valueMaps)
            sfsdr.addReducScriptStep(f1, mystepdict, "A priori flagging", stext, mystepindent)
            wvrCalTableName = []
            stext = doGenerateWVRCalTable(msName, wvrCalTableName, valueMaps=valueMaps, isB2B=isB2B)
            sfsdr.addReducScriptStep(f1, mystepdict, "Generation and time averaging of the WVR cal table", stext, mystepindent)
            tsysCalTableName = []
            stext = doGenerateTsysCalTable(msName, tsysCalTableName, isB2B=isB2B)
            sfsdr.addReducScriptStep(f1, mystepdict, "Generation of the Tsys cal table", stext, mystepindent)

            if corrAntPos == True:
                stext = sfsdr.correctMyAntennaPositions(msName)
            if corrAntPos == True and stext is not None:
                sfsdr.addReducScriptStep(f1, mystepdict, "Generation of the antenna position cal table", stext, mystepindent)
                stext = doApplyAprioriCalTables(msName, tsys=tsysCalTableName[0], wvr=wvrCalTableName[0], antpos=msName+'.antpos', tsysmap=tsysmap, valueMaps=valueMaps)
                sfsdr.addReducScriptStep(f1, mystepdict, "Application of the WVR, Tsys and antpos cal tables", stext, mystepindent)
            else:
                stext = doApplyAprioriCalTables(msName, tsys=tsysCalTableName[0], wvr=wvrCalTableName[0], valueMaps=valueMaps)
                sfsdr.addReducScriptStep(f1, mystepdict, "Application of the WVR and Tsys cal tables", stext, mystepindent)

            stext = doSplitOut(msName, splitMyScienceSpw=splitMyScienceSpw, timebin=timeBinForFinalData, reindexMyScienceSpw=reindexMyScienceSpw)
            sfsdr.addReducScriptStep(f1, mystepdict, "Split out science SPWs and time average", stext, mystepindent)

            print('print("# Calibration")\n', file=f1)
            stext = "os.system('rm -rf %s.split.listobs')\n" % (msName) # Added by CLB
            # following line changed to += by CLB
            stext += "listobs(vis = '"+msName+".split',\n  listfile = '"+msName+".split.listobs')\n\n" \
                + doClearPointingTable(msName+'.split') \
                + doSaveFlags(msName+'.split', name='Original')
            sfsdr.addReducScriptStep(f1, mystepdict, "Listobs, clear pointing table, and save original flags", stext, mystepindent)
            stext = doInitialFlagging(msName, msName1=msName+'.split', iHaveSplitMyScienceSpw=reindexMyScienceSpw)
            sfsdr.addReducScriptStep(f1, mystepdict, "Initial flagging", stext, mystepindent)
            stext = doRunSetjy(msName, msName1=msName+'.split', iHaveSplitMyScienceSpw=reindexMyScienceSpw, useCalibratorService=useCalibratorService, valueMaps=valueMaps)
            sfsdr.addReducScriptStep(f1, mystepdict, "Putting a model for the flux calibrator(s)", stext, mystepindent)
            stext = doSaveFlags(msName+'.split', name='BeforeBandpassCalibration')
            sfsdr.addReducScriptStep(f1, mystepdict, "Save flags before bandpass cal", stext, mystepindent)
            bpassCalTableName = []
            stext = doBandpassCalibration(msName, msName1=msName+'.split', bpassCalId=bpassCalId, iHaveSplitMyScienceSpw=reindexMyScienceSpw, refant=myRefAnt, 
                                          calTableName=bpassCalTableName, valueMaps=valueMaps)
            sfsdr.addReducScriptStep(f1, mystepdict, "Bandpass calibration", stext, mystepindent)
            stext = doSaveFlags(msName+'.split', name='BeforeGainCalibration')
            sfsdr.addReducScriptStep(f1, mystepdict, "Save flags before gain cal", stext, mystepindent)
            stext = doGainCalibration(msName, msName1=msName+'.split', iHaveSplitMyScienceSpw=reindexMyScienceSpw, refant=myRefAnt, bandpass=bpassCalTableName[0], gaintypeForAmp='T', valueMaps=valueMaps)
            sfsdr.addReducScriptStep(f1, mystepdict, "Gain calibration", stext, mystepindent)

            sfsdr.prependReducScriptHeader(f1, mystepdict, "Created using "+version(), mystepindent)

            f1.close()


    if step == 'SDeff':

        for msName in msNames:

            fieldNames = sfsdr.getIntentsAndSourceNames(msName)['OBSERVE_TARGET']['name']
            fieldNames = sorted(dict.fromkeys(fieldNames).keys())
            if len(fieldNames) != 1: 
                casalog.post('ERROR: Unexpected number of fields.', 'SEVERE')
                return False
            fieldNames = fieldNames[0]
            if fieldNames.upper() in ['VENUS', 'MARS', 'JUPITER', 'URANUS', 'NEPTUNE', 'IO', 'EUROPA', 'GANYMEDE', 'CALLISTO', 'TITAN', 'CERES', 'JUNO', 'PALLAS', 'VESTA', 'HYGEIA']:
                print('Observation type = SSO')
                sdEffType = 'SSO'
            else:
                print('Observation type = QSO')
                sdEffType = 'QSO'

            mytb.open(msName+'/OBSERVATION')
            obsTimeRange = mytb.getcol('TIME_RANGE')
            obsTime = (obsTimeRange[0]+obsTimeRange[1])/2.0
            obsTime = ((obsTime/86400.0)+2400000.5-2440587.5)*86400.0
            obsTime = timeUtilities.strftime('%Y-%m-%dT%H:%M:%S', timeUtilities.gmtime(obsTime))
            mytb.close()

            mystepdict = {}
            mystepindent = "  "

            f1 = open(msName+'.scriptForSDefficiencies.py', 'w')
            print("import re\n", file=f1)
            print("es = aU.stuffForScienceDataReduction()\n", file=f1)
            print("import analysisUtilsForSD as aUsd\n\n", file=f1)
            if mycasaversion < '5.1':
                print("if re.search('^"+re.findall('^[0-9]+.[0-9]+.[0-9]+', mycasaversion)[0]+"', casaVersion) == None:", file=f1)
            elif mycasaversion < '5.9':
                print("if re.search('^"+re.findall('^[0-9]+.[0-9]+.[0-9]+', mycasaversion)[0]+"', '.'.join([str(i) for i in cu.version().tolist()[:-1]])) == None:", file=f1)
            else:
                print("if re.search('^"+re.findall('^[0-9]+.[0-9]+.[0-9]+', mycasaversion)[0]+"', '.'.join([str(i) for i in casalith.version()[:-1]])) == None:", file=f1)
            print(" sys.exit('ERROR: PLEASE USE THE SAME VERSION OF CASA THAT YOU USED FOR GENERATING THE SCRIPT: "+re.findall('^[0-9]+.[0-9]+.[0-9]+', mycasaversion)[0]+"')\n\n", file=f1)

            asdmName1 = re.findall('^uid___[0-9a-z]+_[0-9a-z]+_[0-9a-z]+', msName, re.IGNORECASE)[0]

            stext = "aUsd.continuumReducer2('"+asdmName1+"')"
            sfsdr.addReducScriptStep(f1, mystepdict, "Make continuum images", stext, mystepindent)

            mytb.open(msName+'/ANTENNA')
            antNames = mytb.getcol('NAME')
            mytb.close()

            antNames1 = []
            for i in antNames:
                if re.search('^CM[0-9]+', i) == None: antNames1.append(i)
            if len(antNames1) == 0: 
                casalog.post('ERROR: No antenna to process.', 'SEVERE')
                return False
                
            if msName in valueMaps.keys():
                vm = valueMaps[msName]
                print('Using canned ValueMap.')
            else:
                vm = aU.ValueMapping(msName)
                valueMaps[msName] = vm

            sciScans = vm.getScansForIntent('OBSERVE_TARGET#ON_SOURCE').tolist()
            sciScans = [str(i) for i in sciScans]
            sciScans = ','.join(sciScans)

            weatherInfo = {}
            for i in antNames1:
                weatherInfo[i] = aU.getWeather(msName, antenna=i, scan=sciScans, getSolarDirection=False)[0]

            spwInfo = sfsdr.getSpwInfo(msName, caching=True)
            spwIds = sorted(spwInfo.keys())

            stext = ''

            for i in antNames1:
                for j in spwIds:

                    msName1 = asdmName1 + '.' + i + '.cal.ms'
                    imgName1 = asdmName1 + '.' + i + '.SPW' + str(j) + '.SF.im'

                    stext += "aUsd.SdImDWtApp(ms_SD = '"+msName1+"',\n  im_SD = '"+imgName1+"',\n  outimage = '"+imgName1+".wt',\n  spwid = "+str(j)+",\n  gridfunc = 'SF')\n"

                stext += '\n'

            sfsdr.addReducScriptStep(f1, mystepdict, "Mitigating noisy effect at edge of SD image", stext, mystepindent)

            if sdEffType == 'SSO':

                stext = 'ssoParams = {}\n\n'

                for i in antNames1:

                    msName1 = asdmName1 + '.' + i + '.cal.ms'

                    stext += "ssoParams['"+i+"'] = {}\n"
                    stext += "s = aUsd.sso_params('"+msName1+"')\n\n"

                    stext += "for i in "+str(spwIds)+":\n\n"
                    stext += "  s.setSpwId(int(i))\n"
                    stext += "  s.doCalc()\n"
                    stext += "  ssoResults = s.getResults()\n\n"
                    stext += "  ssoParams['"+i+"'][i] = {}\n"
                    stext += "  ssoParams['"+i+"'][i]['eqsize'] = ( ssoResults['Apparent Size'][0][0] + ssoResults['Apparent Size'][1][0] ) / 2.\n"
                    stext += "  ssoParams['"+i+"'][i]['psize'] = ( ssoResults['Apparent Size'][0][1] + ssoResults['Apparent Size'][1][1] ) / 2.\n"
                    stext += "  ssoParams['"+i+"'][i]['pnang'] = ( ssoResults['Apparent Size'][0][2] + ssoResults['Apparent Size'][1][2] ) / 2.\n"
                    stext += "  ssoParams['"+i+"'][i]['btemp'] = ( ssoResults['Brightness Temperature'][0] + ssoResults['Brightness Temperature'][1] ) / 2.\n\n"

                stext += "f = open('"+msName+".ssoParams.txt', 'w')\n"
                stext += "f.write(ssoParams+'\\n')\n"
                stext += "f.close()\n\n"

                sfsdr.addReducScriptStep(f1, mystepdict, "Obtain apparent size and brightness temperature for Solar system object", stext, mystepindent)

            stext = 'sdEffs = {}\n\n'

            for i in antNames1:

                msName1 = asdmName1 + '.' + i + '.cal.ms'

                stext += "for i in "+str(spwIds)+":\n\n"
                stext += "  a = aUsd.analysis_sdim('"+asdmName1+"."+i+".SPW'+str(i)+'.SF.im.wt')\n"

                if sdEffType == 'SSO':
                    stext += "  a.setSSOParams(eqsize = ssoParams['"+i+"'][i]['eqsize'],\n    psize = ssoParams['"+i+"'][i]['psize'],\n    pnang = ssoParams['"+i+"'][i]['pnang'],\n    btemp = ssoParams['"+i+"'][i]['btemp'])\n"
                    stext += "  a.doSSOAnalysis(antbeam = True)\n"
                    stext += "  results = a.getSSOResults()\n"
                    stext += "  #a.showSSOModel()\n"
                    stext += "  #a.showSSOResults()\n\n"
                else:
                    stext += "  a.setQSOFlux("+str(sdQSOflux)+")\n"
                    stext += "  a.doQSOAnalysis()\n"
                    stext += "  results = a.getQSOResults()\n"
                    stext += "  #a.showSdBeam()\n\n"

                stext += "  ij = len(sdEffs)\n"
                stext += "  sdEffs[ij] = {}\n"
                stext += "  sdEffs[ij]['execBlockUid'] = '"+asdmName1+"'\n"
                stext += "  sdEffs[ij]['obsTime'] = '"+obsTime+"'\n"
                stext += "  sdEffs[ij]['antennaName'] = '"+i+"'\n"
                stext += "  sdEffs[ij]['spwId'] = i\n"
                stext += "  sdEffs[ij]['frequency'] = results['Frequency']/1e9\n"
                stext += "  sdEffs[ij]['meanElevation'] = "+str(weatherInfo[i]['elevation'])+"\n"
                stext += "  sdEffs[ij]['meanTemp'] = "+str(weatherInfo[i]['temperature'])+"\n"
                stext += "  sdEffs[ij]['meanWindSpeed'] = "+str(weatherInfo[i]['windspeed'])+"\n"
                stext += "  sdEffs[ij]['effectiveBeamSize'] = results['Effective Beam Size'].tolist()\n"
                stext += "  sdEffs[ij]['mainBeamEfficiency'] = results['Main Beam Efficiency']\n\n"

            stext += "f = open('"+msName+".sdEfficiencies.txt', 'w')\n"
            stext += "f.write(sdEffs+'\\n')\n"
            stext += "f.close()\n\n"

            sfsdr.addReducScriptStep(f1, mystepdict, "Obtain efficiencies and (effective) beam size", stext, mystepindent)

            sfsdr.prependReducScriptHeader(f1, mystepdict, "Calculation of SD efficiencies\n# Created using "+version(), mystepindent)

            f1.close()

    if step in ['SDcalibLine', 'SDcalibCont', 'SDampcal', 'SDscience']:

        if step == 'SDscience' and len(msNames) > 1:

            spwInfo = sfsdr.getSpwInfo(msNames[0], caching=True)
            spwIds = sorted(spwInfo.keys())

            for j in range(1, len(msNames)):

                spwInfo1 = sfsdr.getSpwInfo(msNames[j], caching=True)
                spwIds1 = sorted(spwInfo1.keys())
                if spwIds1 != spwIds: 
                    print('WARNING: THE SCIENCE SPWS ARE NOT THE SAME FOR ALL EXECUTIONS.')

        imagingParams = {}

        for msName in msNames:

            mystepdict = {}
            mystepindent = "  "

            tsysmap = ''
            if re.search('^3.3', mycasaversion) is None:

                print("\n*** ANALYSIS OF TSYS TABLE ***")

                print("\n*** SEARCH FOR NEGATIVE TSYS ***")
                aU.detectNegativeTsys(vis = msName, edge = 8, showfield = True)

                print("\n*** SEARCH FOR NEGATIVE TREC ***")
                aU.detectNegativeTrx(vis = msName, edge = 8, showfield = True)

                os.system('rm -Rf '+msName+'.tsys.temp')
                gencal(vis = msName, caltable = msName+'.tsys.temp', caltype = 'tsys')

                print("\n*** SEARCH FOR MISSING SCANS IN SYSCAL TABLE ***")
                mymsmd = msmdtool()
                mymsmd.open(msName)

                scans1 = mymsmd.scansforintent('CALIBRATE_ATMOSPHERE*')

                if ('CALIBRATE_ATMOSPHERE#REFERENCE' in mymsmd.intents()):
                    # The presence of extra AtmCals with a REFERENCE intent will cause scans1 != scans2
                    scansWithZeroLevel = mymsmd.scansforintent('CALIBRATE_ATMOSPHERE#REFERENCE')
                    scans1 = np.array(sorted(list(set(scans1)-set(scansWithZeroLevel))))
                mymsmd.close()

                mytb.open(msName+'.tsys.temp')                    
                scans2 = np.unique(mytb.getcol('SCAN_NUMBER'))
                mytb.close()

                if (scans1 == scans2).all():
                    print("-> OK")
                else:
                    print("scans1 = ", scans1)
                    print("scans2 = ", scans2)
                    casalog.post('ERROR: THE SYSCAL TABLE IS MISSING ONE (OR MORE) SCAN(S). IT MAY BE NECESSARY TO RE-GENERATE IT.', 'SEVERE')
                    return False

                if useLocalAlmaHelper == True:
                    tsysmap = tsysspwmap(vis = msName, tsystable = msName+'.tsys.temp', tsysChanTol=tsysChanTol)
                    if msName in valueMaps.keys():
                        vm = valueMaps[msName]
                        print('Using canned ValueMap.')
                    else:
                        vm = aU.ValueMapping(msName)
                        valueMaps[msName] = vm

                    spwInfo = sfsdr.getSpwInfo(msName, caching=True)
                    spwIds = sorted(spwInfo.keys())
                    for j in spwIds:
                        spwIntents = vm.getIntentsForSpw(tsysmap[j])
                        if 'CALIBRATE_ATMOSPHERE#ON_SOURCE' not in spwIntents:
                            if 'CALIBRATE_ATMOSPHERE#HOT' not in spwIntents:
                                casalog.post('ERROR: INCOMPLETE TSYS SPW MAPPING!', 'SEVERE')
                                return False

                os.system('rm -Rf '+msName+'.tsys.temp')

            if step == 'SDampcal':
                f1name = msName+'.scriptForSDampcalReduction.py'
            else:
                f1name = msName+'.scriptForSDCalibration.py'
            f1 = open(f1name, 'w')

            print("import os", file=f1)
            print("import re\n", file=f1)

            sourcelines1 = ''.join(inspect.getsourcelines(aU.createCasaTool)[0])
            if re.search('""".*?"""', sourcelines1, re.DOTALL) is not None:
                sourcelines2 = re.findall('""".*?"""', sourcelines1, re.DOTALL)[0]
                sourcelines1 = sourcelines1.replace(sourcelines2, '')
            print(sourcelines1, file=f1)

            sourcelines1 = ''.join(inspect.getsourcelines(aU.getDataColumnName)[0])
            if re.search('""".*?"""', sourcelines1, re.DOTALL) is not None:
                sourcelines2 = re.findall('""".*?"""', sourcelines1, re.DOTALL)[0]
                sourcelines1 = sourcelines1.replace(sourcelines2, '')
            print(sourcelines1, file=f1)

            sourcelines1 = ''.join(inspect.getsourcelines(aU.scaleAutocorr)[0])
            if re.search('""".*?"""', sourcelines1, re.DOTALL) is not None:
                sourcelines2 = re.findall('""".*?"""', sourcelines1, re.DOTALL)[0]
                sourcelines1 = sourcelines1.replace(sourcelines2, '')
            print(sourcelines1, file=f1)

            print("if applyonly != True: es = aU.stuffForScienceDataReduction()\n", file=f1)
            if mycasaversion < '5.1':
                print("if re.search('^"+re.findall('^[0-9]+.[0-9]+.[0-9]+', mycasaversion)[0]+"', casaVersion) == None:", file=f1)
            elif mycasaversion < '5.9':
                print("if re.search('^"+re.findall('^[0-9]+.[0-9]+.[0-9]+', mycasaversion)[0]+"', '.'.join([str(i) for i in cu.version().tolist()[:-1]])) == None:", file=f1)
            else:
                print("if re.search('^"+re.findall('^[0-9]+.[0-9]+.[0-9]+', mycasaversion)[0]+"', '.'.join([str(i) for i in casalith.version()[:-1]])) == None:", file=f1)
            print("  sys.exit('ERROR: PLEASE USE THE SAME VERSION OF CASA THAT YOU USED FOR GENERATING THE SCRIPT: "+re.findall('^[0-9]+.[0-9]+.[0-9]+', mycasaversion)[0]+"')\n\n", file=f1)

            tempStdout = sys.stdout
            sys.stdout = f1
            sfsdr.listOfIntentsWithSources(msName)
            sys.stdout = tempStdout

            print("\n", file=f1)

            stext = "if os.path.exists('"+msName+"') == False:\n"

            if corrModes1 == 'ALMA_ACA' and mycasaversion < '5.9.9':
                # I don't think this was ever necessary since CAS-8995 was fixed by 4.7.1, but bdflags2MS is no longer in CASA6. - TRH
                stext += "  importasdm('"+re.findall('.+(?=\.ms)', msName, re.IGNORECASE)[0]+"', asis='"+asis1+"', bdfflags=False, lazy="+str(lazy)+", process_caldevice=False, with_pointing_correction="+str(with_pointing_correction)+")"
                if bdfflags == True:
                    stext += "\n  os.system(os.environ['CASAPATH'].split()[0]+'/bin/bdflags2MS -f \"COR DELA INT MIS SIG SYN TFB WVR ZER\" "+re.findall('.+(?=\.ms)', msName, re.IGNORECASE)[0]+" "+msName+"')"
            else:
                stext += "  importasdm('"+re.findall('.+(?=\.ms)', msName, re.IGNORECASE)[0]+"', asis='"+asis1+"', bdfflags="+str(bdfflags)+", lazy="+str(lazy)+", process_caldevice=False, with_pointing_correction="+str(with_pointing_correction)+")"


            stext += "\nif applyonly != True: es.fixForCSV2555('"+msName+"')"
            sfsdr.addReducScriptStep(f1, mystepdict, "Import of the ASDM", stext, mystepindent, applyonly=True)

            stext = doRunFixPlanets(msName)
            if stext is not None: sfsdr.addReducScriptStep(f1, mystepdict, "Running fixplanets on fields with 0,0 coordinates", stext, mystepindent)

            mytb.open(msName+'/ANTENNA')
            antNames = mytb.getcol('NAME').tolist()
            mytb.close()

            stext = "os.system('rm -rf %s.listobs')\n" %(msName) # Added by CLB
            stext += "listobs(vis = '"+msName+"',\n  listfile = '"+msName+".listobs')\n\n" # Modified by CLB

            stext += "if applyonly != True:\n"
            stext += "  aU.getTPSampling(vis = '"+msName+"', showplot = True, plotfile = '"+msName+".sampling.png')\n"
            stext += "  for i in "+str(antNames)+":\n    aU.getTPSampling(vis = '"+msName+"', antenna = i, showplot = True, plotfile = '"+msName+".sampling.'+i+'.png')"

            sfsdr.addReducScriptStep(f1, mystepdict, "listobs", stext, mystepindent)

            stext = doAprioriFlagging(msName, flagAutoCorr=False, flagCalIntents=False, valueMaps=valueMaps)
            sfsdr.addReducScriptStep(f1, mystepdict, "A priori flagging", stext, mystepindent)

            if mycasaversion < '5.0':

                stext = "for i in "+str(antNames)+":\n  os.system('rm -Rf "+msName+".'+i+'*')\n\n"
                stext += "sdsave(infile = '"+msName+"',\n  splitant = True,\n  outfile = '"+msName+".asap',\n  overwrite = True)\n\n"
                sfsdr.addReducScriptStep(f1, mystepdict, "Split by antenna", stext, mystepindent)

                asapNames = [msName+'.'+i+'.asap' for i in antNames]
                asapNames = sorted(asapNames)

                stext = ''
                for i in asapNames:
                    stext += "os.system('rm -Rf "+i+".sdlist')\n"
                    stext += "sdlist(infile = '"+i+"',\n  outfile = '"+i+".sdlist')\n\n"
                sfsdr.addReducScriptStep(f1, mystepdict, "sdlist", stext, mystepindent)

            else:

                asapNames = [msName]

            tsysCalTableName = []
            stext = SDdoFillTsysSolutions(asapNames, msName=msName, doplot=True, tsysCalTableName=tsysCalTableName)

            sfsdr.addReducScriptStep(f1, mystepdict, "Generation of the Tsys cal table", stext, mystepindent)

            skyCalTableName = []

            if mycasaversion >= '5.0':

                if step in ['SDcalibLine', 'SDscience']:
                    stext = SDdoFillTsysSolutions(asapNames, msName=msName, doplot=True, tsysCalTableName=skyCalTableName, sky=True, calmode='ps')
                else:
                    stext = SDdoFillTsysSolutions(asapNames, msName=msName, doplot=True, tsysCalTableName=skyCalTableName, sky=True, calmode='otfraster')
                sfsdr.addReducScriptStep(f1, mystepdict, "Generation of the Sky cal table", stext, mystepindent)

            spwInfo = sfsdr.getSpwInfo(msName, caching=True)
            if msName in valueMaps.keys():
                vm = valueMaps[msName]
                print('Using canned ValueMap.')
            else:
                vm = aU.ValueMapping(msName)
                valueMaps[msName] = vm

            chanFlags = {}

            for i in sorted(spwInfo.keys()):
                if vm.spwInfo[i]['bandwidth'] > 1875000000:
                    chanEdge = int((vm.spwInfo[i]['numChannels'] - (1875000000. / vm.spwInfo[i]['bandwidth']) * vm.spwInfo[i]['numChannels']) / 2.)
                    maskflag = str([[0, chanEdge-1], [vm.spwInfo[i]['numChannels']-chanEdge, vm.spwInfo[i]['numChannels']-1]])
                    if maskflag not in list(chanFlags.keys()):
                        chanFlags[maskflag] = []
                    chanFlags[maskflag].append(i)

            stext = ''

            for i in asapNames:
                for maskflag in list(chanFlags.keys()):
                    maskflag1 = eval(maskflag)
                    maskflag2 = []
                    for j in range(len(maskflag1)):
                        maskflag2.append('~'.join([str(k) for k in maskflag1[j]]))
                    maskflag2 = ';'.join(maskflag2)
                    spwmaskflag = []
                    for j in chanFlags[maskflag]:
                        spwmaskflag.append(str(j)+':'+maskflag2)
                    spwmaskflag = ','.join(spwmaskflag)
                    if mycasaversion >= '5.0':
                        stext += "flagdata(vis = '"+i+"',\n  mode = 'manual',\n  spw = '"+spwmaskflag+"')\n\n"
                    else:
                        stext += "sdflag(infile = '"+i+"',\n  mode = 'manual',\n  spw = '"+spwmaskflag+"',\n  overwrite = True)\n\n"

            sfsdr.addReducScriptStep(f1, mystepdict, "Do initial flagging", stext, mystepindent)


            mytb.open(msName+'/OBSERVATION')
            obsTimeRange = mytb.getcol('TIME_RANGE')
            mytb.close()
            obsTimeStart = ((obsTimeRange[0]/86400.0)+2400000.5-2440587.5)*86400.0
            obsTimeStart = timeUtilities.strftime('%Y-%m-%dT%H:%M:%S', timeUtilities.gmtime(obsTimeStart))

            if step in ['SDcalibLine', 'SDscience']:

                if mycasaversion < '5.0':
                    stext = SDdoCalibration(asapNames, msName=msName, calmode='ps', tsysCalTableName=tsysCalTableName[0])
                else:
                    stext = SDdoCalibration(asapNames, msName=msName, tsysCalTableName=tsysCalTableName[0], skyCalTableName=skyCalTableName[0])
                sfsdr.addReducScriptStep(f1, mystepdict, "Calibration of the data into Kelvins", stext, mystepindent)

                if mycasaversion < '5.0':

                    asapNames = [i+'.cal' for i in asapNames]

                    if obsTimeStart < '2015-10-01T00:00:00':

                        stext = ''
                        for i in asapNames:
                            stext += "os.system('rm -Rf "+i+".nlc')\n\n"
                            stext += "sdscale(infile = '"+i+"',\n  outfile = '"+i+".nlc',\n  factor = 1.25)\n\n"
                        sfsdr.addReducScriptStep(f1, mystepdict, "Application of non-linearity correction factor", stext, mystepindent)

                        asapNames = [i+'.nlc' for i in asapNames]

                stext = SDdoBaselineSubtraction(asapNames, msName=msName)
                sfsdr.addReducScriptStep(f1, mystepdict, "Subtracting the baseline", stext, mystepindent)

                asapNames = [i+'.bl' for i in asapNames]

            else:

                if mycasaversion < '5.0':
                    stext = SDdoCalibration(asapNames, msName=msName, calmode='otfraster', tsysCalTableName=tsysCalTableName[0])
                else:
                    stext = SDdoCalibration(asapNames, msName=msName, tsysCalTableName=tsysCalTableName[0], skyCalTableName=skyCalTableName[0])
                sfsdr.addReducScriptStep(f1, mystepdict, "Calibration of the data into Kelvins", stext, mystepindent)

                if mycasaversion < '5.0':

                    asapNames = [i+'.cal' for i in asapNames]

                    if obsTimeStart < '2015-10-01T00:00:00':

                        stext = ''
                        for i in asapNames:
                            stext += "os.system('rm -Rf "+i+".nlc')\n\n"
                            stext += "sdscale(infile = '"+i+"',\n  outfile = '"+i+".nlc',\n  factor = 1.25)\n\n"
                        sfsdr.addReducScriptStep(f1, mystepdict, "Application of non-linearity correction factor", stext, mystepindent)

                        asapNames = [i+'.nlc' for i in asapNames]

            spwInfo = sfsdr.getSpwInfo(msName, caching=True)
            spwIds = sorted(spwInfo.keys())
            spwIds = [str(i) for i in spwIds]

            if mycasaversion < '5.0':

                stext = ''
                for i in asapNames:
                    stext += "os.system('rm -Rf "+i+".ms')\n\n"
                    if mycasaversion >= '4.2.2':
                        stext += "sdsave(infile = '"+i+"',\n  outfile = '"+i+".ms',\n  spw = '"+','.join(spwIds)+"',\n  outform = 'MS2')\n\n"
                    else:
                        stext += "sdsave(infile = '"+i+"',\n  outfile = '"+i+".ms',\n  outform = 'MS2')\n\n"
                sfsdr.addReducScriptStep(f1, mystepdict, "Converting ASAP -> MS", stext, mystepindent)

                asapNames = [i+'.ms' for i in asapNames]


            stext = ''

            if len(asapNames) > 1:
                stext += "os.system('rm -Rf "+msName+".cal')\n\n"
                stext += "concat(vis = [ \\\n    '"+"', \\\n    '".join(asapNames)+"' ], \\\n  concatvis = '"+msName+".cal')\n\n"
            else:
                stext += "os.system('rm -Rf "+msName+".cal')\n\n"
                stext += "os.system('cp -Rf "+asapNames[0]+" "+msName+".cal')\n\n"

            sfsdr.addReducScriptStep(f1, mystepdict, "Split and concatenation", stext, mystepindent)

            if step == 'SDscience':

                jyperk = sfsdr.getJyPerK(msName, interactive=True)

                f2 = open('jyperk.txt', 'w')
                pprint.pprint(jyperk, stream=f2, indent=2)
                f2.close()

                f2 = open('jyperk.txt')
                jyperk = f2.read()
                f2.close()

                stext = "jyperk = \\\n" + jyperk + "\n"

                if mycasaversion >= '5.0':
                    blspwmap = {}
                    for i in spwIds: blspwmap[str(i)] = str(spwIds.index(i))
                    stext += "blspwmap = "+str(blspwmap)+"\n"

                stext += "os.system('rm -Rf "+msName+".cal.jy')\n"
                stext += "os.system('cp -Rf "+msName+".cal "+msName+".cal.jy')\n\n"

                stext += "for ant in jyperk.keys():\n"
                stext += "  for spw in jyperk[ant].keys():\n"

                if mycasaversion < '5.0':
                    stext += "    scaleAutocorr(vis='"+msName+".cal.jy', scale=jyperk[ant][spw]['mean'], antenna=ant, spw=spw)\n"
                else:
                    stext += "    scaleAutocorr(vis='"+msName+".cal.jy', scale=jyperk[ant][spw]['mean'], antenna=ant, spw=int(blspwmap[str(spw)]))\n"

                sfsdr.addReducScriptStep(f1, mystepdict, "Convert the Science Target Units from Kelvin to Jansky", stext, mystepindent)

            if step in ['SDscience', 'SDampcal']:

                imagingParams[msName] = {}

                imagingParams[msName]['spwIds'] = spwIds
                print("running au.getTPSampling('%s', showplot=False, pickFirstRaster=True)" % (msName))
                xSampling, ySampling, maxsize = aU.getTPSampling(msName, showplot=False, pickFirstRaster=True)
                imagingParams[msName]['maxsize'] = maxsize
                mymsmd = msmdtool()
                mymsmd.open(msName)

                for i in sorted(spwInfo.keys()):

                    imagingParams[msName][i] = {}

                    freq = mymsmd.meanfreq(i)
                    imagingParams[msName][i]['freq'] = freq

                    theorybeam = aU.primaryBeamArcsec(frequency=freq*1e-9, fwhmfactor=1.13, diameter=12)
                    imagingParams[msName][i]['theorybeam'] = theorybeam

                    minor, major, fwhmsfBeam, sfbeam = aU.sfBeam(frequency=freq*1e-9, pixelsize=theorybeam/9.0, convsupport=6, img=None, stokes='both', xSamplingArcsec=xSampling, ySamplingArcsec=ySampling, fwhmfactor=1.13, diameter=12)
                    imagingParams[msName][i]['sfbeam'] = sfbeam

                fieldId = mymsmd.fieldsforintent('OBSERVE_TARGET#ON_SOURCE')
                if len(fieldId) != 1: 
                    casalog.post('ERROR: UNEXPECTED NUMBER OF FIELDS.', 'SEVERE')
                    return False
                fieldId = fieldId[0]
                imagingParams[msName]['fieldId'] = fieldId

                fieldName = mymsmd.namesforfields(fieldId)
                if len(fieldName) != 1: 
                    casalog.post('ERROR: UNEXPECTED NUMBER OF FIELDS.', 'SEVERE')
                    return False
                fieldName = fieldName[0]
                imagingParams[msName]['fieldName'] = fieldName

                mymsmd.close()

            if step == 'SDampcal':

                stext = "# the values below were calculated assuming fwhmfactor = 1.13\n\n"
                stext += "maxsize = "+str(imagingParams[msName]['maxsize'])+"\n\n"

                stext += "theorybeam = {}\n"
                for i in sorted(spwInfo.keys()):
                    stext += "theorybeam['"+str(i)+"'] = "+str(imagingParams[msName][i]['theorybeam'])+" # mean freq = "+str(imagingParams[msName][i]['freq']*1e-9)+"\n"

                setjyModels = ['Venus', 'Mars', 'Jupiter', 'Uranus', 'Neptune', 'Pluto', 'Io', 'Europa', 'Ganymede', 'Callisto', 'Titan', 'Triton', 'Ceres', 'Pallas', 'Vesta', 'Juno', 'Victoria', 'Davida']

                doPlanet = 0
                for j in range(len(setjyModels)):
                    if re.search(setjyModels[j], fieldName, re.IGNORECASE) is not None:
                        doPlanet = 1
                        break

                if doPlanet == 1:
                    supportedSSOTPampCals = ['mercury', 'venus', 'mars', 'jupiter', 'saturn', 'uranus', 'neptune']
                    if fieldName.lower() not in supportedSSOTPampCals: 
                        casalog.post('ERROR: SSO NOT SUPPORTED AS TP AMP CAL.', 'SEVERE')

                stext += "\nfor spw in "+str(spwIds)+":\n\n"
                stext += "  cell = theorybeam[spw]/9.0\n"
                stext += "  imsize = int(round(maxsize/cell)*2)\n\n"
                stext += "  for ant in "+str(antNames)+":\n\n"
                stext += "    sdimaging(infiles = '"+msName+".cal',\n"
                stext += "      field = '"+fieldName+"',\n"
                stext += "      spw = spw,\n"
                stext += "      antenna = ant,\n"
                stext += "      nchan = 1,\n"
                stext += "      mode = 'channel',\n"
                stext += "      width = '4080',\n"
                stext += "      gridfunction = 'SF',\n"
                stext += "      convsupport = 6,\n"

                if doPlanet == 1:
                    stext += "      ephemsrcname = '"+fieldName+"',\n"
                else:
                    stext += "      phasecenter = "+str(fieldId)+",\n"

                stext += "      imsize = imsize,\n"
                stext += "      cell = str(cell)+'arcsec',\n"
                stext += "      overwrite = True,\n"
                stext += "      outfile = '"+msName+".cal.%s.spw%s.image' % (ant, spw))\n"

                sfsdr.addReducScriptStep(f1, mystepdict, "Imaging", stext, mystepindent)

                stext = "srcflux = {}\n"

                for i in sorted(spwInfo.keys()):

                    if doPlanet == 1:
                        planetInfo = aU.planetFlux(vis=msName, spw=i)
                        srcflux = planetInfo['fluxDensity']
                        srcsize = (planetInfo['majorAxis']*planetInfo['minorAxis'])**0.5
                        spwfreq = planetInfo['meanFrequency']
                    else:
                        qsoInfo = aU.getALMAFluxForMS(msName, field=fieldName, spw=str(i), useCalibratorService=useCalibratorService)
                        srcflux = qsoInfo[fieldName]['fluxDensity']
                        spwfreq = qsoInfo[fieldName]['frequency']

                    stext += "srcflux['"+str(i)+"'] = "+str(srcflux)+" # mean freq = "+str(spwfreq*1e-9)+"\n"

                if doPlanet == 1:
                    stext += "\nsrcsize = "+str(srcsize)+"\n"

                stext += "\njyperk = {}\n"

                stext += "\nfor ant in "+str(antNames)+":\n\n"
                stext += "  jyperk[ant] = {}\n\n"
                stext += "  for spw in "+str(spwIds)+":\n\n"
                stext += "    if os.path.exists('"+msName+".cal.%s.spw%s.image' % (ant, spw)):\n\n"
                stext += "      peak = imstat('"+msName+".cal.%s.spw%s.image' % (ant, spw))['max'][0]\n\n"

                if doPlanet == 1:

                    stext += "      fwhm = aU.getfwhm2('"+msName+".cal.%s.spw%s.image' % (ant, spw))\n"
                    stext += "      print('Apparent FWHM (inc. gridding convolution) is %.2f arcsec' % fwhm)\n\n"
                    stext += "      deconvfwhm = aU.deconvolveDiskFromBeam(fwhm, srcsize)\n"
                    stext += "      print('FWHM deconvolved for planet size is %.2f arcsec' % deconvfwhm)\n\n"
                    stext += "      # correction factor for dilution due to planet size\n"
                    stext += "      srcsizecorr = (deconvfwhm/fwhm)**2\n\n"
                    stext += "      jyperk[ant][spw] = srcflux[spw] / peak * srcsizecorr"

                else:

                    stext += "      jyperk[ant][spw] = srcflux[spw] / peak"

                sfsdr.addReducScriptStep(f1, mystepdict, "Determination of the Jy/K factors", stext, mystepindent)

                stext = "asdm = '"+re.findall('uid___[a-zA-Z0-9]+_[a-zA-Z0-9]+_[a-zA-Z0-9]+', msName)[0]+"'\n"

                mytb.open(msName+'/OBSERVATION')
                obsTimeRange = mytb.getcol('TIME_RANGE')
                obsTime = (obsTimeRange[0]+obsTimeRange[1])/2.0
                obsTime = ((obsTime/86400.0)+2400000.5-2440587.5)*86400.0
                obsTime = timeUtilities.strftime('%Y-%m-%dT%H:%M:%S', timeUtilities.gmtime(obsTime))
                mytb.close()

                stext += "date = '"+obsTime+"'\n"

                stext += "ampcal = '"+fieldName+"'\n"
                mymsmd = msmdtool()
                mymsmd.open(msName)

                spwbb = {}
                spwfreq = {}
                spwbw = {}

                for i in sorted(spwInfo.keys()):
                    spwbb[str(i)] = str(mymsmd.baseband(int(i)))
                    spwfreq[str(i)] = str(mymsmd.meanfreq(int(i)))
                    spwbw[str(i)] = str(mymsmd.bandwidths(int(i)))

                stext += "band = '"+str(aU.getBand(spwfreq[list(spwfreq.keys())[0]]))+"'\n"
                stext += "bb = "+str(spwbb)+" # spw baseband number\n"
                stext += "freq = "+str(spwfreq)+" # spw mean frequency\n"
                stext += "bw = "+str(spwbw)+" # spw bandwidth\n"

                scanList = mymsmd.scansforintent('OBSERVE_TARGET#ON_SOURCE')

                mymsmd.close()

                weatherInfo = aU.getWeather(msName, scan = scanList.tolist(), getSolarDirection=False)

                stext += "elev = '"+str(weatherInfo[0]['elevation'])+"' # mean elevation\n"
                stext += "temp = '"+str(weatherInfo[0]['temperature'])+"' # mean temperature\n\n"

                stext += "f = open('"+msName+".cal.jyperk.txt', 'w')\n\n"

                stext += "for ant in "+str(antNames)+":\n"
                stext += "  for spw in "+str(spwIds)+":\n\n"
                stext += "    if ant in jyperk.keys():\n"
                stext += "      if spw in jyperk[ant].keys():\n"
                stext += "        f.write('\\t'.join([asdm, ant, spw, str(jyperk[ant][spw]), date, ampcal, band, bb[spw], freq[spw], bw[spw], elev, temp])+'\\n')\n"

                stext += "\nf.close()\n"

                sfsdr.addReducScriptStep(f1, mystepdict, "Writing out the Jy/K factors", stext, mystepindent)

            sfsdr.prependReducScriptHeader(f1, mystepdict, "Created using "+version(), mystepindent)
            print("Wrote ", f1name)
            f1.close()

        ###################################
        
        if step == 'SDscience':

            os.chdir(currDir)

            mystepdict = {}
            mystepindent = "  "
            f1name = 'scriptForSDimaging.py'
            f1 = open(f1name, 'w')

            if mycasaversion < '5.1':
                print("if re.search('^"+re.findall('^[0-9]+.[0-9]+.[0-9]+', mycasaversion)[0]+"', casaVersion) == None:", file=f1)
            elif mycasaversion < '5.9':
                print("if re.search('^"+re.findall('^[0-9]+.[0-9]+.[0-9]+', mycasaversion)[0]+"', '.'.join([str(i) for i in cu.version().tolist()[:-1]])) == None:", file=f1)
            else:
                print("if re.search('^"+re.findall('^[0-9]+.[0-9]+.[0-9]+', mycasaversion)[0]+"', '.'.join([str(i) for i in casalith.version()[:-1]])) == None:", file=f1)
            print("  sys.exit('ERROR: PLEASE USE THE SAME VERSION OF CASA THAT YOU USED FOR GENERATING THE SCRIPT: "+re.findall('^[0-9]+.[0-9]+.[0-9]+', mycasaversion)[0]+"')\n\n", file=f1)

            stext = 'msNames = [ \\\n'
            for i in range(len(msNames)):
                stext += "'"+msNames[i]+".cal.jy', \\\n"
            stext += ']\n\n'

            tos = []
            stext += "# Time on source\n"
            for i in range(len(msNames)):
                tos.append(aU.timeOnSource(msNames[i])['minutes_on_science'])
                stext += "# "+msNames[i]+" = "+str(round(tos[i], 1))+" min\n"
            stext += "# Total = "+str(round(np.sum(tos), 1))+" min"

            sfsdr.addReducScriptStep(f1, mystepdict, "Define the calibrated datasets", stext, mystepindent)

            splitMS = False
            for i in range(len(msNames)):
                if sorted(imagingParams[msNames[i]]['spwIds']) != sorted(imagingParams[msNames[0]]['spwIds']):
                    print('WARNING: the science spw indices are not the same across all EBs, I will need to run split.')
                    splitMS = True
                    break

            if splitMS == True:

                stext = ''
                for i in range(len(msNames)):

                    stext += "os.system('rm -Rf "+msNames[i]+".cal.jy.split')\n\n"
                    stext += "split(vis = '"+msNames[i]+".cal.jy',\n"
                    stext += "  outputvis = '"+msNames[i]+".cal.jy.split',\n"
                    stext += "  datacolumn = 'all',\n"
                    stext += "  spw = '"+','.join(sorted(imagingParams[msNames[i]]['spwIds']))+"')\n\n"

                    spwIds1 = sorted(imagingParams[msNames[i]]['spwIds'])
                    spwIds1 = [int(j) for j in spwIds1]
                    for j in range(len(spwIds1)):
                        imagingParams[msNames[i]][j] = imagingParams[msNames[i]][spwIds1[j]]
                        imagingParams[msNames[i]].pop(spwIds1[j])

                    spwIds1 = list(range(len(imagingParams[msNames[i]]['spwIds'])))
                    spwIds1 = [str(j) for j in spwIds1]
                    imagingParams[msNames[i]]['spwIds'] = spwIds1

                stext += 'msNames = [ \\\n'
                for i in range(len(msNames)):
                    stext += "'"+msNames[i]+".cal.jy.split', \\\n"
                stext += ']\n\n'

                sfsdr.addReducScriptStep(f1, mystepdict, "Split the science spectral windows", stext, mystepindent)

            stext = "# only the spectral windows listed below will be imaged\n"
            stext += "spwIds = "+str(imagingParams[msNames[0]]['spwIds'])+"\n\n"

            if mycasaversion >= '5.0':

                blspwmap = {}
                for i in imagingParams[msNames[0]]['spwIds']: blspwmap[i] = str(imagingParams[msNames[0]]['spwIds'].index(i))
                stext += "blspwmap = "+str(blspwmap)+"\n"


            sfsdr.addReducScriptStep(f1, mystepdict, "Define the imaging parameters", stext, mystepindent)

            fieldId = imagingParams[msNames[0]]['fieldId']
            fieldName = imagingParams[msNames[0]]['fieldName']

            stext = "# the values below were calculated assuming fwhmfactor = 1.13\n\n"
            stext += "maxsize = "+str(imagingParams[msNames[0]]['maxsize'])+"\n\n"

            stext += "theorybeam = {}\n"

            myspwids = sorted(imagingParams[msNames[0]]['spwIds'])

            for i in myspwids:
                stext += "theorybeam['"+str(i)+"'] = "+str(imagingParams[msNames[0]][int(i)]['theorybeam'])+" # mean freq = "+str(imagingParams[msNames[0]][int(i)]['freq']*1e-9)+"\n"

            stext += "\n# the values below were calculated assuming cell = theorybeam[spw]/9.0\n"
            stext += "sfbeam = {}\n"

            myimagename = {}            
            for i in myspwids:
                stext += "sfbeam['"+str(i)+"'] = "+str(imagingParams[msNames[0]][int(i)]['sfbeam'])+" # mean freq = "+str(imagingParams[msNames[0]][int(i)]['freq']*1e-9)+"\n"
                myimagename[i] = aU.genImageName(vis=msNames[0], spw=int(i), field=fieldId, imtype='cube', targettype='sci', stokes='I', modtext='manual')

            stext += "\nmyimagenames = {"
            for i in myspwids:
                stext += "'"+str(i)+"': '"+myimagename[i]+"',\n                "
            stext += "}\n"

            stext += "\nfor spw in spwIds:\n\n"

            stext += "  cell = theorybeam[spw]/9.0\n"
            stext += "  imsize = int(round(maxsize/cell)*2)\n\n"


            stext += "  sdimaging(infiles = msNames,\n"
            stext += "    field = '"+fieldName+"',\n"

            if mycasaversion < '5.0':
                stext += "    spw = spw,\n"
            else:
                stext += "    spw = blspwmap[spw],\n"

            stext += "    mode = 'channel',\n"
            stext += "    outframe = 'lsrk',\n"   # requires lower-case prior to fix for CAS-12820
            stext += "    gridfunction = 'SF',\n"
            stext += "    convsupport = 6,\n"
            stext += "    phasecenter = "+str(fieldId)+",\n"
            stext += "    imsize = imsize,\n"
            stext += "    cell = str(cell)+'arcsec',\n"
            stext += "    overwrite = True,\n"
            stext += "    outfile = myimagenames[spw])\n\n"

            sfsdr.addReducScriptStep(f1, mystepdict, "Image the Science Target", stext, mystepindent)

            stext = "\nfor spw in spwIds:\n\n"
            stext += "  imhead(imagename = myimagenames[spw],\n"
            stext += "    mode = 'put',\n"
            stext += "    hdkey = 'bunit',\n"
            stext += "    hdvalue = 'Jy/beam')\n\n"

            sfsdr.addReducScriptStep(f1, mystepdict, "Correct the brightness unit in the image header", stext, mystepindent)

            stext = "\nfor spw in spwIds:\n\n"
            stext += "  myia = iatool()\n"
            stext += "  myia.open(myimagenames[spw])\n"
            stext += "  myia.setrestoringbeam(major = str(sfbeam[spw])+'arcsec', minor = str(sfbeam[spw])+'arcsec', pa = '0deg')\n"
            stext += "  myia.done()\n"

            sfsdr.addReducScriptStep(f1, mystepdict, "Add Restoring Beam Header Information to the Science Image", stext, mystepindent)

            stext = "\nfor spw in spwIds:\n\n"
            stext += "  exportfits(imagename = myimagenames[spw],\n"
            stext += "    fitsimage = myimagenames[spw]+'.fits')\n\n"

            sfsdr.addReducScriptStep(f1, mystepdict, "Export images to fits", stext, mystepindent)

            sfsdr.prependReducScriptHeader(f1, mystepdict, "SD Imaging\n# Created using "+version(), mystepindent)

            print("Wrote ", f1name)
            f1.close()

    return True

    # end of generateReducScript()

############################

def doAprioriFlagging(msName, flagAutoCorr=True, flagCalIntents=True, valueMaps={}):
    """Generate code for the Apriori Flagging step of a calibration script."""

    print('\n*** doAprioriFlagging ***')

    casaCmd = ''

    if flagCalIntents == True:

        intentsToFlag = ['POINTING', 'FOCUS', 'SIDEBAND_RATIO', 'ATMOSPHERE']

        if msName in valueMaps.keys():
            vm = valueMaps[msName]
            print('Using canned ValueMap.')
        else:
            vm = aU.ValueMapping(msName)
            valueMaps[msName] = vm

        fullIntentList = vm.uniqueIntents

        scanIntentList = []
        for i in intentsToFlag:
            for j in fullIntentList:
                if re.search(i, j) is not None:
                    scanIntentList.append('*'+i+'*')
                    break

        scanIntentList = ','.join(scanIntentList)

    mytb = aU.createCasaTool(tbtool)
    if flagAutoCorr == True:

        mytb.open(msName+'/DATA_DESCRIPTION')
        spwIds = mytb.getcol('SPECTRAL_WINDOW_ID')
        mytb.close()

        mytb.open(msName+'/PROCESSOR')
        procType = mytb.getcol('TYPE')
        mytb.close()

        procType1 = np.where(procType == 'RADIOMETER')[0]

        spwIds1 = []

        mytb.open(msName)

        for i in procType1:
            tb1 = mytb.query('PROCESSOR_ID == '+str(i))
            dataDescIds1 = tb1.getcol('DATA_DESC_ID')
            dataDescIds1 = np.unique(dataDescIds1)
            for j in dataDescIds1:
                spwIds1.append(spwIds[j])
            tb1.close() 

        mytb.close()

        spwIds1 = [i for i in range(len(spwIds)) if i not in spwIds1]

        if len(spwIds1) > 1:
            j0 = 0
            spwIds2 = str(spwIds1[j0])
            for j in range(len(spwIds1)-1):
                if spwIds1[j+1] == spwIds1[j]+1: continue
                spwIds2 = spwIds2 + '~' + str(spwIds1[j])
                j0 = j+1
                spwIds2 = spwIds2 + ',' + str(spwIds1[j0])
            spwIds2 = spwIds2 + '~' + str(spwIds1[j+1])
        else:
            spwIds2 = str(spwIds1[0])

    if flagAutoCorr == True:
        if re.search('^3.3', aU.getCasaVersion()) is not None:
            casaCmd = casaCmd + "flagdata(vis = '"+msName+"',\n"
            casaCmd = casaCmd + "  mode = 'manualflag',\n"
            casaCmd = casaCmd + "  spw = '"+spwIds2+"',\n"
            casaCmd = casaCmd + "  autocorr = True,\n"
            casaCmd = casaCmd + "  flagbackup = False)\n\n"
        elif aU.getCasaVersion() >= '4.1.0':
            casaCmd = casaCmd + "flagdata(vis = '"+msName+"',\n"
            casaCmd = casaCmd + "  mode = 'manual',\n"
            casaCmd = casaCmd + "  spw = '"+spwIds2+"',\n"
            casaCmd = casaCmd + "  autocorr = True,\n"
            casaCmd = casaCmd + "  flagbackup = False)\n\n"
        else:
            casaCmd = casaCmd + "tflagdata(vis = '"+msName+"',\n"
            casaCmd = casaCmd + "  mode = 'manual',\n"
            casaCmd = casaCmd + "  spw = '"+spwIds2+"',\n"
            casaCmd = casaCmd + "  autocorr = True,\n"
            casaCmd = casaCmd + "  flagbackup = False)\n\n"

    if flagCalIntents == True:

        if re.search('^3.3', aU.getCasaVersion()) is not None:
            casaCmd = casaCmd + "flagdata(vis = '"+msName+"',\n"
            casaCmd = casaCmd + "  mode = 'manualflag',\n"
            casaCmd = casaCmd + "  intent = '"+scanIntentList+"',\n"
            casaCmd = casaCmd + "  flagbackup = False)\n"
        elif aU.getCasaVersion() >= '4.1.0':
            casaCmd = casaCmd + "flagdata(vis = '"+msName+"',\n"
            casaCmd = casaCmd + "  mode = 'manual',\n"
            casaCmd = casaCmd + "  intent = '"+scanIntentList+"',\n"
            casaCmd = casaCmd + "  flagbackup = False)\n"
        else:
            casaCmd = casaCmd + "tflagdata(vis = '"+msName+"',\n"
            casaCmd = casaCmd + "  mode = 'manual',\n"
            casaCmd = casaCmd + "  intent = '"+scanIntentList+"',\n"
            casaCmd = casaCmd + "  flagbackup = False)\n"

    mytb.open(msName)
    tableNames = mytb.keywordnames()
    mytb.close()

    if 'FLAG_CMD' in tableNames:

        mytb.open(msName+'/FLAG_CMD')
        nFlagRows = mytb.nrows()
        mytb.close()

        if nFlagRows != 0:

            if re.search('^3.3', aU.getCasaVersion()) == None:
                casaCmd = casaCmd + "\nflagcmd(vis = '"+msName+"',\n"
                casaCmd = casaCmd + "  inpmode = 'table',\n"
                casaCmd = casaCmd + "  useapplied = True,\n"
                casaCmd = casaCmd + "  action = 'plot',\n"
                casaCmd = casaCmd + "  plotfile = '"+msName+".flagcmd.png')\n\n"
                casaCmd = casaCmd + "flagcmd(vis = '"+msName+"',\n"
                casaCmd = casaCmd + "  inpmode = 'table',\n"
                casaCmd = casaCmd + "  useapplied = True,\n"
                casaCmd = casaCmd + "  action = 'apply')\n"
            else:
                casaCmd = casaCmd + "\nflagcmd(vis = '"+msName+"',\n"
                casaCmd = casaCmd + "  flagmode = 'table',\n"
                casaCmd = casaCmd + "  optype = 'plot')\n\n"
                casaCmd = casaCmd + "flagcmd(vis = '"+msName+"',\n"
                casaCmd = casaCmd + "  flagmode = 'table',\n"
                casaCmd = casaCmd + "  optype = 'apply')\n"

    return casaCmd

####################

def doGenerateWVRCalTable(msName, calTableName=[], refant='', smooth=True, doplot=True, remcloud=False, valueMaps={}, isB2B=False):
    """Generate code for the wvrgcal step of a calibration script."""

    print('\n*** doGenerateWVRCalTable ***')

    if remcloud == True and aU.getCasaSubversionRevision() < '35187':
        casalog.post('ERROR: remcloud option is only supported for CASA >= r35187','SEVERE')
        return False

    casaCmd = ''
    mytb = aU.createCasaTool(tbtool)
    mytb.open(msName+'/OBSERVATION')
    obsTimeRange = mytb.getcol('TIME_RANGE')
    mytb.close()
    obsTimeStart = ((obsTimeRange[0]/86400.0)+2400000.5-2440587.5)*86400.0
    obsTimeStart = timeUtilities.strftime('%Y-%m-%dT%H:%M:%S', timeUtilities.gmtime(obsTimeStart))
    if obsTimeStart > '2013-01-21T00:00:00':
        wvrTimeOffset = 0
    else:
        wvrTimeOffset = -1

    mytb.open(msName+'/ANTENNA')
    antNames = mytb.getcol('NAME')
    mytb.close()

    found = 0
    for i in antNames:
        if re.search('^[CP]M[0-9]+', i) == None: found = 1
    if found == 0:
        calTableName.append('')
        return casaCmd

    intentSources = sfsdr.getIntentsAndSourceNames(msName)

    sciSourceId = intentSources['OBSERVE_TARGET']['sourceid']
    sciSourceId1 = list(dict.fromkeys(sciSourceId).keys())
    if sciSourceId1[0] != '':
        sciSourceName = intentSources['OBSERVE_TARGET']['name'][sciSourceId.index(min(sciSourceId1))]
        sciSourceId = min(sciSourceId1)
        phaseCal = sfsdr.getPhaseCal(msName, valueMaps=valueMaps)

    if remcloud == True:
        casaCmd = casaCmd + "import recipes.remove_cloud as rc\n\n"

    casaCmd = casaCmd + "os.system('rm -rf %s.wvr') \n\n"%(msName)
    if remcloud == True:
        casaCmd = casaCmd + "os.system('rm -rf %s.cloud_offsets') \n\n"%(msName)
    casaCmd = casaCmd + "os.system('rm -rf %s.wvrgcal') \n\n"%(msName)

    sciSpwInfo = sfsdr.getSpwInfo(msName, intent='OBSERVE_TARGET|CALIBRATE_BANDPASS', caching=True)
    spwInfo = sciSpwInfo.copy()
    if isB2B:
        refSpwInfo = sfsdr.getSpwInfo(msName, intent='CALIBRATE_DIFFGAIN#REFERENCE', caching=True)
        spwInfo.update(refSpwInfo)

    integTime = []
    for i in sciSpwInfo: 
        integTime.append(sciSpwInfo[i]['integTime'])
    integTime = list(dict.fromkeys(integTime).keys())
    if len(integTime) != 1: 
        casaCmd = casaCmd + "# Warning: more than one integration time found on science data, I'm picking the lowest value. Please check this is right.\n\n"
    integTime = min(integTime)

    if remcloud == True:
        casaCmd = casaCmd + "rc.remove_cloud(vis='"+msName+"', offsetstable='"+msName+".cloud_offsets')\n\n"

    casaCmd = casaCmd + "mylogfile = casalog.logfile()\n"
    casaCmd = casaCmd + "casalog.setlogfile('"+msName+".wvrgcal')\n\n"
    casaCmd = casaCmd + "wvrgcal(vis = '"+msName+"',\n"

    if remcloud == True:
        casaCmd = casaCmd + "  offsetstable = '"+msName+".cloud_offsets',\n"

    casaCmd = casaCmd + "  caltable = '"+msName+".wvr',\n"

    spwIds = sorted(spwInfo.keys())
    casaCmd = casaCmd + "  spw = "+str(spwIds)+",\n"

    if smooth == True and integTime > 1.152:
        casaCmd = casaCmd + "  smooth = '"+str(integTime)+"s',\n"

    casaCmd = casaCmd + "  toffset = "+str(wvrTimeOffset)
    if sciSourceId1[0] != '':
        casaCmd = casaCmd + ",\n  tie = "+str([','.join([i, phaseCal[i]['phaseCalName']]) for i in phaseCal])+",\n"
        casaCmd = casaCmd + "  statsource = '"+sciSourceName+"')\n\n"
    else:
        casaCmd = casaCmd + ")\n\n"
    casaCmd = casaCmd + "casalog.setlogfile(mylogfile)\n\n"

    calTableName1 = msName+'.wvr'


    calTableName.append(calTableName1)

    if doplot and isB2B:
        sciSpw0 = sorted(sciSpwInfo.keys())[0] 
        refSpw0 = sorted(refSpwInfo.keys())[0] 
        casaCmd = casaCmd + "if applyonly != True: aU.plotWVRSolutions(caltable='%s', spw='%s', antenna='%s',\n" %(calTableName1,sciSpw0,refant)
        casaCmd = casaCmd + "  yrange=[-199,199],subplot=22, interactive=False,\n"
        casaCmd = casaCmd + "  figfile='%s') \n\n" %(calTableName1+'.plots/'+calTableName1.split('/')[-1]+'SignalSPWs')
        casaCmd = casaCmd + "if applyonly != True: aU.plotWVRSolutions(caltable='%s', spw='%s', antenna='%s',\n" %(calTableName1,refSpw0,refant)
        casaCmd = casaCmd + "  yrange=[-199,199],subplot=22, interactive=False,\n"
        casaCmd = casaCmd + "  figfile='%s') \n\n" %(calTableName1+'.plots/'+calTableName1.split('/')[-1]+'ReferenceSPWs')
        casaCmd = casaCmd + "#Note: If you see wraps in these plots, try changing yrange or unwrap=True \n"
        casaCmd = casaCmd + "#Note: If all plots look strange, it may be a bad WVR on the reference antenna.\n"
        casaCmd = casaCmd + "#      To check, you can set antenna='' to show all baselines.\n"

    elif doplot:
        sciSpw0 = sorted(sciSpwInfo.keys())[0] 
        casaCmd = casaCmd + "if applyonly != True: aU.plotWVRSolutions(caltable='%s', spw='%s', antenna='%s',\n" %(calTableName1,sciSpw0,refant)
        casaCmd = casaCmd + "  yrange=[-199,199],subplot=22, interactive=False,\n"
        casaCmd = casaCmd + "  figfile='%s') \n\n" %(calTableName1+'.plots/'+calTableName1.split('/')[-1])
        casaCmd = casaCmd + "#Note: If you see wraps in these plots, try changing yrange or unwrap=True \n"
        casaCmd = casaCmd + "#Note: If all plots look strange, it may be a bad WVR on the reference antenna.\n"
        casaCmd = casaCmd + "#      To check, you can set antenna='' to show all baselines.\n"



    return casaCmd

###################################

def doRunFixPlanets(msName):
    """Generate code for running fixplanets on fields with (0,0) coordinates"""

    print('\n*** doRunFixPlanets ***')

    fieldIds = sfsdr.getFieldsForFixPlanets(msName)

    if len(fieldIds) != 0:

        casaCmd = ''
        mytb = aU.createCasaTool(tbtool)
        mytb.open(msName+'/FIELD')
        fieldNames = mytb.getcol('NAME')
        mytb.close()

        fieldNames = ['%s' %fieldNames[i] for i in fieldIds]
        fieldNames = ','.join(fieldNames)
        fieldIds = ['%s' %i for i in fieldIds]
        fieldIds = ','.join(fieldIds)

        casaCmd = casaCmd + "fixplanets(vis = '"+msName+"',\n"
        casaCmd = casaCmd + "  field = '"+fieldIds+"', # "+fieldNames+"\n"
        casaCmd = casaCmd + "  fixuvw = True)\n"

        return casaCmd

####################################

def doGenerateTsysCalTable(msName, calTableName=[], doplot=True, isB2B=False):
    """Generate code for the Tsys table generation step of a calibration script."""

    print('\n*** doGenerateTsysCalTable ***')

    casaCmd = ''

    sciSpwInfo = sfsdr.getSpwInfo(msName, caching=True)

    tsysNumChans = []
    tsysSpwInfo = sfsdr.getSpwInfo(msName, intent='CALIBRATE_ATMOSPHERE', caching=True)
    for i in tsysSpwInfo: 
        tsysNumChans.append(tsysSpwInfo[i]['numChans'])
    tsysNumChans = sorted(dict.fromkeys(tsysNumChans).keys())

    tsysNumChans = tsysNumChans[0]

    casaCmd = casaCmd + "os.system('rm -rf %s.tsys') \n"%(msName)  # Added by CLB

    casaCmd = casaCmd + "gencal(vis = '"+msName+"',\n"
    casaCmd = casaCmd + "  caltable = '"+msName+".tsys',\n"
    casaCmd = casaCmd + "  caltype = 'tsys')\n\n"

    calTableName1 = msName+'.tsys'

    chanEdge = 0.03125 # this is for 128ch/2GHz

    spwSpec = ''
    for i in sorted(tsysSpwInfo.keys()):
        if tsysSpwInfo[i]['numChans'] <= 256:
            if spwSpec != '': spwSpec = spwSpec+','
            chanEdge1 = chanEdge * tsysSpwInfo[i]['numChans'] / 128.
            spwSpec = spwSpec+str(i)+':0~'+str(np.long(tsysSpwInfo[i]['numChans']*chanEdge1-1))+';'+str(np.long(tsysSpwInfo[i]['numChans']-tsysSpwInfo[i]['numChans']*chanEdge1))+'~'+str(tsysSpwInfo[i]['numChans']-1)

    if spwSpec != '':
        casaCmd = casaCmd + "# Flagging edge channels\n\n"
        casaCmd = casaCmd + "flagdata(vis = '"+calTableName1+"',\n"
        casaCmd = casaCmd + "  mode = 'manual',\n"
        casaCmd = casaCmd + "  spw = '"+spwSpec+"',\n"
        casaCmd = casaCmd + "  flagbackup = False)\n\n"

    if isB2B:
        spwHighInt0 = sorted(sciSpwInfo.keys())[0]
        ref_freq = sciSpwInfo[spwHighInt0]['refFreq']
        if ref_freq > 600E9:
            casalog.post('B2B mode in Band 9 or 10: will add code to mirror Tsys', 'WARN')
            
            casaCmd = casaCmd + "# Mirroring Tsys\n"
            casaCmd = casaCmd + "tb.open('"+calTableName1+"')\n"
            casaCmd = casaCmd + "TsysSpws=np.unique(tb.getcol('SPECTRAL_WINDOW_ID'))\n"
            casaCmd = casaCmd + "tb.close()\n"
            casaCmd = casaCmd + "\n"  
            casaCmd = casaCmd + "TsysSpwSW=[]\n"
            casaCmd = casaCmd + "TsysSpwFreq=[]\n"
            casaCmd = casaCmd + "tb.open('"+calTableName1+"/SPECTRAL_WINDOW')\n"
            casaCmd = casaCmd + "for TsysSpw in TsysSpws:\n"
            casaCmd = casaCmd + "    spwFreqTsys = tb.getcell('REF_FREQUENCY',TsysSpw)\n"
            casaCmd = casaCmd + "    TsysSpwFreq.append(spwFreqTsys)\n"
            casaCmd = casaCmd + "    SWname = tb.getcell('NAME',TsysSpw)\n"
            casaCmd = casaCmd + "    TsysSpwSW.append(SWname[22:32])\n"
            casaCmd = casaCmd + "tb.close()\n"
            casaCmd = casaCmd + "\n"
            casaCmd = casaCmd + "ref_freq = "+str(ref_freq)+"\n"
            casaCmd = casaCmd + "\n"    
            casaCmd = casaCmd + "TsysIdsLF=np.where(np.array(TsysSpwFreq)<ref_freq/2)[0]\n"
            casaCmd = casaCmd + "TsysSpwsLF=TsysSpws[TsysIdsLF]\n"
            casaCmd = casaCmd + "TsysSpwsSWLF=np.array(TsysSpwSW)[TsysIdsLF]\n"
            casaCmd = casaCmd + "\n"
            casaCmd = casaCmd + "# now we want to create real and fake DSB pairings\n"
            casaCmd = casaCmd + "# e.g BB01 and SW01 with BB01 and SW02\n" 
            casaCmd = casaCmd + "baseBands = ['BB_1','BB_2','BB_3','BB_4']\n"
            casaCmd = casaCmd + "for baseBand in baseBands:\n"
            casaCmd = casaCmd + "    TsysPair=[spwNo for spwId,spwNo in enumerate(TsysSpwsLF) if baseBand in TsysSpwsSWLF[spwId]]\n"
            casaCmd = casaCmd + "    TsysData=[]\n"
            casaCmd = casaCmd + "    for TsysSpwPair in TsysPair:\n"
            casaCmd = casaCmd + "        tb.open('"+calTableName1+"', nomodify=False)\n"
            casaCmd = casaCmd + "        tsysdata = tb.query('SPECTRAL_WINDOW_ID == %s' %TsysSpwPair)\n"
            casaCmd = casaCmd + "        tsysdataSpw = tsysdata.getcol('FPARAM')\n"
            casaCmd = casaCmd + "        TsysData.append(tsysdataSpw)\n"
            casaCmd = casaCmd + "        tb.close()\n"
            casaCmd = casaCmd + "\n"
            casaCmd = casaCmd + "    if np.mean(TsysData[0][0][8:-8]) < np.mean(TsysData[1][0][8:-8]):\n"
            casaCmd = casaCmd + "        spwRep = 1\n"
            casaCmd = casaCmd + "        spwKeep = 0\n"
            casaCmd = casaCmd + "    else:\n"
            casaCmd = casaCmd + "        spwRep = 0\n"
            casaCmd = casaCmd + "        spwKeep =1\n"
            casaCmd = casaCmd + "    #now make the replacement\n"
            casaCmd = casaCmd + "    tb.open('"+calTableName1+"', nomodify=False)\n"
            casaCmd = casaCmd + "    tsysdataRep = tb.query('SPECTRAL_WINDOW_ID == %s' %TsysPair[spwRep])\n"
            casaCmd = casaCmd + "    tsysdataRep.putcol('FPARAM',TsysData[spwKeep])\n"
            casaCmd = casaCmd + "    tb.close()\n\n"


    if doplot:
        chanrange = '92.1875%'

        showimage = False
        for i in sorted(sciSpwInfo.keys()):
            if sciSpwInfo[i]['refFreq'] > 550e9: showimage = True

        if isB2B: # temporary workaround for a problem in plotbandpass
            casaCmd = casaCmd + "try: # protect against failure of plotbandpass in case of Tsys timestamp issues\n"
            casaCmd = casaCmd + "  if applyonly != True: aU.plotbandpass(caltable='%s', overlay='time', \n" %(calTableName1)
            casaCmd = casaCmd + "    xaxis='freq', yaxis='amp', subplot=22, buildpdf=False, interactive=False,\n"
            casaCmd = casaCmd + "    showatm=True,pwv='auto',chanrange='"+chanrange+"',showfdm=True, showBasebandNumber=True, showimage="+str(showimage)+", \n"
            casaCmd = casaCmd + "    field='', figfile='%s') \n\n" %(calTableName1+'.plots.overlayTime/'+calTableName1.split('/')[-1])
            casaCmd = casaCmd + "except:\n"
            casaCmd = casaCmd + "  print('Error in plotbandpass. Skipping the time-overlay Tsys plots.')\n"

        else:
            casaCmd = casaCmd + "if applyonly != True: aU.plotbandpass(caltable='%s', overlay='time', \n" %(calTableName1)
            casaCmd = casaCmd + "  xaxis='freq', yaxis='amp', subplot=22, buildpdf=False, interactive=False,\n"
            casaCmd = casaCmd + "  showatm=True,pwv='auto',chanrange='"+chanrange+"',showfdm=True, showBasebandNumber=True, showimage="+str(showimage)+", \n"
            casaCmd = casaCmd + "  field='', figfile='%s') \n\n" %(calTableName1+'.plots.overlayTime/'+calTableName1.split('/')[-1])

    calTableName.append(calTableName1)
    if doplot:
        casaCmd = casaCmd + "\nif applyonly != True: es.checkCalTable('"+calTableName1+"', msName='"+msName+"', interactive=False) \n"

    return casaCmd

#################################

def doApplyAprioriCalTables(msName, tsys='', wvr='', antpos='', tsysmap='', tsysChanTol='', tsysPerField=False, valueMaps={}):
    """Generate code for the applycal step (apriori calibration: WVR, Tsys, and antpos) of a calibration script."""

    print('\n*** doApplyAprioriCalTables ***')

    casaCmd = ''

    if tsys=='' and wvr=='' and antpos=='': 
        casalog.post('ERROR: No cal table specified.', 'SEVERE')
        return False

    gainTable = []
    gainTable.append(tsys)
    gainTable.append(wvr)
    gainTable.append(antpos)
    gainTable = ['%s' %i for i in gainTable if i != '']

    spwInfo = sfsdr.getSpwInfo(msName, intent='OBSERVE_TARGET|CALIBRATE_BANDPASS|CALIBRATE_DIFFGAIN', caching=True)
    spwIds = sorted(spwInfo.keys())
    spwIds1 = ','.join(['%s' %i for i in spwIds])

    if tsys=='':

        casaCmd = casaCmd + "\n\napplycal(vis = '"+msName+"',\n"
        casaCmd = casaCmd + "  spw = '"+spwIds1+"',\n"
        casaCmd = casaCmd + "  gaintable = "+str(gainTable)+",\n"
        if re.search('^3.3', aU.getCasaVersion()) == None:
            casaCmd = casaCmd + "  interp = 'linear,linear',\n"
        else:
            casaCmd = casaCmd + "  interp = 'linear',\n"
        if aU.getCasaVersion() <= '4.2.2':
            casaCmd = casaCmd + "  calwt = False,\n"
        else:
            casaCmd = casaCmd + "  calwt = True,\n"
        casaCmd = casaCmd + "  flagbackup = False)\n"

    else:

        if re.search('^3.3', aU.getCasaVersion()) == None:
            if tsysmap == '':

                if tsysPerField == True:

                    casaCmd = casaCmd + "\n\nfrom almahelpers_localcopy import tsysspwmap\n"

                    if tsysChanTol == '':
                        casaCmd = casaCmd + "tsysmap = tsysspwmap(vis = '"+msName+"', tsystable = '"+tsys+"', perField=True)\n\n"
                    else:
                        tsysChanTol = int(tsysChanTol)
                        casaCmd = casaCmd + "tsysmap = tsysspwmap(vis = '"+msName+"', tsystable = '"+tsys+"', tsysChanTol = "+str(tsysChanTol)+", perField=True)\n\n"

                else:
                    if aU.getCasaVersion() < '5.9.9':
                        casaCmd = casaCmd + "\n\nfrom recipes.almahelpers import tsysspwmap\n"
                    else:
                        casaCmd = casaCmd + "\n\nfrom casarecipes.almahelpers import tsysspwmap\n"

                    if tsysChanTol == '':
                        casaCmd = casaCmd + "tsysmap = tsysspwmap(vis = '"+msName+"', tsystable = '"+tsys+"')\n\n"
                    else:
                        tsysChanTol = int(tsysChanTol)
                        casaCmd = casaCmd + "tsysmap = tsysspwmap(vis = '"+msName+"', tsystable = '"+tsys+"', tsysChanTol = "+str(tsysChanTol)+")\n\n"

            else:
                casaCmd = casaCmd + "\n\ntsysmap = "+str(tsysmap)+"\n\n"
        mytb = aU.createCasaTool(tbtool)
        mytb.open(msName+'/FIELD')
        sourceIds = mytb.getcol('SOURCE_ID')
        sourceNames = mytb.getcol('NAME')
        mytb.close()

        sourceIds1 = sorted(dict.fromkeys(sourceIds).keys())

        phaseCal = sfsdr.getPhaseCal(msName, valueMaps=valueMaps)

        intentSources = sfsdr.getIntentsAndSourceNames(msName)
        fieldIds = intentSources['CALIBRATE_ATMOSPHERE']['id']

        for i in sourceIds1:

            fieldIds1 = (np.where(sourceIds == i))[0]
            sourceName = sourceNames[fieldIds1[0]]

            if tsys != '':

                spwIds2 = []
                sourceIntents = []
                mymsmd = msmdtool()
                mymsmd.open(msName)
                for j in fieldIds1:
                    for k in mymsmd.spwsforfield(j):
                        spwIds2.append(k)
                    sourceIntents.append(mymsmd.intentsforfield(j))
                mymsmd.close()

                spwIds2 = np.unique(spwIds2)
                spwIds1 = ','.join(['%s' %j for j in spwIds if j in spwIds2])

                sourceIntents = np.unique(np.hstack(np.array(sourceIntents))).tolist()

                found = 0

                for j in range(len(sourceIntents)):
                    if re.search('CALIBRATE_ATMOSPHERE#[A-Z_]|CALIBRATE_WVR#[A-Z_]', sourceIntents[j]) == None:
                        found = 1
                        break

                if found == 0: continue

            if len(fieldIds1) > 1:
                j0 = 0
                fieldIds2 = str(fieldIds1[j0])
                for j in range(len(fieldIds1)-1):
                    if fieldIds1[j+1] == fieldIds1[j]+1: continue
                    fieldIds2 = fieldIds2 + '~' + str(fieldIds1[j])
                    j0 = j+1
                    fieldIds2 = fieldIds2 + ',' + str(fieldIds1[j0])
                fieldIds2 = fieldIds2 + '~' + str(fieldIds1[j+1])
            else:
                fieldIds2 = str(fieldIds1[0])

            fieldIds3 = [j for j in fieldIds1 if j in fieldIds]

            if len(fieldIds3) > 1:
                print('WARNING: Too many Tsys fields per source.')
                fieldIds3[0] = ','.join(['%d' %j for j in fieldIds3])

            if len(fieldIds3) == 0 or i in intentSources['OBSERVE_CHECK']['sourceid']:
                if i in intentSources['OBSERVE_TARGET']['sourceid']:

                    found = 0
                    for j in list(phaseCal.keys()):
                        if j == sourceName: continue
                        if phaseCal[j]['phaseCalId'] == phaseCal[sourceName]['phaseCalId']:
                            sourceIds2 = np.unique(phaseCal[j]['sciSourceIds'])
                            if len(sourceIds2) != 1: 
                                casalog.post('ERROR: no or more than one source ID','SEVERE')
                                return False
                            fieldIds3 = []
                            for k in range(len(intentSources['CALIBRATE_ATMOSPHERE']['sourceid'])):
                                if intentSources['CALIBRATE_ATMOSPHERE']['sourceid'][k] == sourceIds2:
                                    fieldIds3.append(intentSources['CALIBRATE_ATMOSPHERE']['id'][k])
                            if (len(fieldIds3) > 0):
                                fieldIds3[0] = ','.join(['%d' %k for k in fieldIds3])
                                found = 1
                                break
                    if found == 1:
                        casaCmd = casaCmd + "\n\n# Note: "+sourceName+" didn't have any Tsys measurement, so I used the one made on "+j+". This is probably Ok."
                    else:

                        if phaseCal[sourceName]['phaseCalId'] in fieldIds:
                            fieldIds3 = [phaseCal[sourceName]['phaseCalId']]
                            casaCmd = casaCmd + "\n\n# Note: "+sourceName+" didn't have any Tsys measurement, so I used the one made on "+phaseCal[sourceName]['phaseCalName']+". This is probably Ok."
                        else:
                            casaCmd = casaCmd + "\n\n# Warning: "+sourceName+" didn't have any Tsys measurement, and I couldn't find any close measurement. This is a science target, so this is probably *NOT* Ok."
                            continue

                elif i in intentSources['CALIBRATE_PHASE']['sourceid']:
                    if len(fieldIds1) != 1: 
                        casalog.post('ERROR: no or more than one field ID','SEVERE')
                        return False
                    found = 0
                    fieldIds3 = []
                    fieldIds3Names = []
                    for j in sorted(phaseCal.keys()):
                        if phaseCal[j]['phaseCalId'] == fieldIds1[0]:
                            sourceIds2 = np.unique(phaseCal[j]['sciSourceIds'])
                            if len(sourceIds2) != 1: 
                                casalog.post('ERROR: no or more than one source ID','SEVERE')
                                return False
                            for k in range(len(intentSources['CALIBRATE_ATMOSPHERE']['sourceid'])):
                                if intentSources['CALIBRATE_ATMOSPHERE']['sourceid'][k] == sourceIds2:
                                    fieldIds3.append(intentSources['CALIBRATE_ATMOSPHERE']['id'][k])
                                    fieldIds3Names.append(j)
                    if (len(fieldIds3) > 0):
                        fieldIds3[0] = ','.join(['%d' %k for k in sorted(fieldIds3)])
                        fieldIds3Names = ','.join(fieldIds3Names)
                        found = 1


                    if found == 1:
                        casaCmd = casaCmd + "\n\n# Note: "+sourceName+" didn't have any Tsys measurement, so I used the one made on "+fieldIds3Names+". This is probably Ok."
                    else:
                        casaCmd = casaCmd + "\n\n# Warning: "+sourceName+" didn't have any Tsys measurement, and I couldn't find any close measurement. This is a phase calibrator, so this is probably *NOT* Ok."
                        continue
                elif i in intentSources['OBSERVE_CHECK']['sourceid']:
                    if len(fieldIds1) != 1: 
                        casalog.post('ERROR: no or more than one field ID','SEVERE')
                        return False
                    found = 0
                    for j in list(phaseCal.keys()):
                        if j == sourceName: continue
                        if phaseCal[j]['phaseCalId'] == phaseCal[sourceName]['phaseCalId']:
                            found = 1
                            break
                    if found == 1:
                        sourceIds2 = np.unique(phaseCal[j]['sciSourceIds'])
                        if len(sourceIds2) != 1: 
                            casalog.post('ERROR: no or more than one source ID','SEVERE')
                            return False
                        fieldIds3 = []
                        for k in range(len(intentSources['CALIBRATE_ATMOSPHERE']['sourceid'])):
                            if intentSources['CALIBRATE_ATMOSPHERE']['sourceid'][k] == sourceIds2:
                                fieldIds3.append(intentSources['CALIBRATE_ATMOSPHERE']['id'][k])

                        if (len(fieldIds3) > 0):
                            fieldIds3[0] = ','.join(['%d' %k for k in fieldIds3])
                        casaCmd = casaCmd + "\n\n# Note: "+sourceName+" didn't have any Tsys measurement, so I used the one made on "+j+". This is probably Ok."
                    else:
                        casaCmd = casaCmd + "\n\n# Warning: "+sourceName+" didn't have any Tsys measurement, and I couldn't find any close measurement. This is a check source, so this is probably *NOT* Ok."
                        continue
                else:

                    found = 0

                    if len(fieldIds1) == 1:
                        fieldIds1 = fieldIds1[0]
                        if phaseCal is not None:
                            for j in phaseCal:
                                if phaseCal[j]['phaseCalId'] == fieldIds1:
                                    sciSourceIds1 = phaseCal[j]['sciSourceIds']
                                    sciSourceIds1 = sorted(dict.fromkeys(sciSourceIds1).keys())
                                    if len(sciSourceIds1) == 1:
                                        sciSourceIds1 = sciSourceIds1[0]
                                        ij = np.where(np.array(intentSources['CALIBRATE_ATMOSPHERE']['sourceid']) == sciSourceIds1)[0]
                                        if len(ij) == 1:
                                            ij = ij[0]
                                            fieldIds3 = [intentSources['CALIBRATE_ATMOSPHERE']['id'][ij]]
                                            found = 1
                                            break

                    if found == 0:

                        if tsys != '':

                            fieldIds4 = (np.where(sourceIds == i))[0]
                            mymsmd = msmdtool()
                            mymsmd.open(msName)
                            fieldIds5 = mymsmd.fieldsforname(sourceName)

                            fieldIds5 = [j for j in fieldIds5 if j not in fieldIds4]

                            fieldIds6 = []
                            for j in fieldIds5:
                                fieldIntents = mymsmd.intentsforfield(j)
                                if 'CALIBRATE_ATMOSPHERE#ON_SOURCE' in fieldIntents and 'CALIBRATE_ATMOSPHERE#OFF_SOURCE' in fieldIntents:
                                    fieldIds6.append(j)

                            mymsmd.close()

                            if len(fieldIds6) != 0:

                                fieldIds3 = [','.join(['%d' %k for k in fieldIds6])]
                                casaCmd = casaCmd + "\n\n# Note: "+sourceName+" didn't have any Tsys measurement, so I used the one made on another source with the same name. This is probably Ok."
                                found = 1

                        if found == 0:

                            casaCmd = casaCmd + "\n\n# Note: "+sourceName+" didn't have any Tsys measurement, and I couldn't find any close measurement. But this is not a science target, so this is probably Ok."
                            continue

            gainField = []
            for j in range(len(gainTable)): gainField.append('')

            if (len(fieldIds3) > 0):
                gainField[0] = str(fieldIds3[0])
            else:
                gainField[0] = intentSources['CALIBRATE_ATMOSPHERE']['idstring'][0]

            gainSpwMap = []
            for j in range(len(gainTable)): gainSpwMap.append("[]")

            if tsysPerField == True:
                gainSpwMap[0] = "tsysmap['"+str(gainField[0])+"']"
            else:
                gainSpwMap[0] = 'tsysmap'

            gainSpwMap = ','.join(gainSpwMap)

            casaCmd = casaCmd + "\n\napplycal(vis = '"+msName+"',\n"
            casaCmd = casaCmd + "  field = '"+fieldIds2+"',\n"
            casaCmd = casaCmd + "  spw = '"+spwIds1+"',\n"
            casaCmd = casaCmd + "  gaintable = "+str(gainTable)+",\n"
            casaCmd = casaCmd + "  gainfield = "+str(gainField)+",\n"
            if re.search('^3.3', aU.getCasaVersion()) == None:
                casaCmd = casaCmd + "  interp = 'linear,linear',\n"
            else:
                casaCmd = casaCmd + "  interp = 'linear',\n"
            if re.search('^3.3', aU.getCasaVersion()) == None: casaCmd = casaCmd + "  spwmap = ["+gainSpwMap+"],\n"
            casaCmd = casaCmd + "  calwt = True,\n"
            casaCmd = casaCmd + "  flagbackup = False)\n\n"

        casaCmd = casaCmd + "\n\nif applyonly != True: es.getCalWeightStats('"+msName+"') \n"

    return casaCmd


##################################

def doSplitOut(msName, msName1='', outMsName='', splitMyScienceSpw=False, timebin=0., iHaveSplitMyScienceSpw=False, allowHybrid=True, intentsToDiscard='', reindexMyScienceSpw=False):
    """Generate code for the split-out the corrected data."""

    print('\n*** doSplitOut ***')

    if aU.getCasaVersion() >= '5.4':
        useMStransform = True
    else:
        useMStransform = False

    casaCmd = ''

    if msName1 == '': msName1 = msName
    if outMsName == '': outMsName = msName1+'.split'

    spwInfo = sfsdr.getSpwInfo(msName, intent='OBSERVE_TARGET|CALIBRATE_BANDPASS|CALIBRATE_DIFFGAIN', caching=True)
    spwIds = sorted(spwInfo.keys())

    if intentsToDiscard != '':

        mymsmd = msmdtool()
        mymsmd.open(msName)

        if iHaveSplitMyScienceSpw == True:

            listOfIntents = []

            for i in spwIds:
                for j in mymsmd.intentsforspw(i):
                    listOfIntents.append(j)

            listOfIntents = np.unique(listOfIntents).tolist()

        else:

            listOfIntents = mymsmd.intents()

        mymsmd.close()

        listOfIntents = [i for i in listOfIntents if re.search(intentsToDiscard, i, re.IGNORECASE) == None]

    if iHaveSplitMyScienceSpw == True: 
        spwIds = list(range(len(spwIds)))
    spwIds = ['%d' %i for i in spwIds]
    spwIds = ','.join(spwIds)

    numChans = []
    for i in sorted(spwInfo.keys()): numChans.append(spwInfo[i]['numChans'])
    if max(numChans) <= 256 and timebin != 0: casaCmd = casaCmd + "# Important note: the correlator mode for this dataset was TDM, you may want not to do any time averaging.\n\n"

    if allowHybrid != True:
        mytb = aU.createCasaTool(tbtool)
        mytb.open(msName+'/ANTENNA')
        antDiam = mytb.getcol('DISH_DIAMETER')
        antNames = mytb.getcol('NAME')
        mytb.close()

        antDiam1 = np.unique(antDiam)

        antNames2 = ''

        if len(antDiam1) != 1:

            numAnts = {}
            for i in range(len(antDiam1)):
                numAnts[antDiam1[i]] = len(np.where(antDiam == antDiam1[i])[0])

            numAntsMax = max(numAnts.values())

            antDiam2 = []
            for i in range(len(antDiam1)):
                if numAnts[antDiam1[i]] == numAntsMax:
                    antDiam2.append(antDiam1[i])
            if len(antDiam2) != 1: 
                casalog.post('ERROR: no or more than one antenna diameter','SEVERE')
                return False
            antDiam2 = antDiam2[0]

            ij = np.where(antDiam == antDiam2)
            antNames1 = antNames[ij]

            antNames2 = []
            for i in range(len(antNames1)):
                antNames3 = re.findall('^[a-z]+', antNames1[i], re.IGNORECASE)
                if len(antNames3) == 0: continue
                antNames2.append(antNames3[0]+'*')
            antNames2 = ','.join(np.unique(antNames2).tolist())+'&'

    casaCmd = casaCmd + "os.system('rm -rf %s') \n"%(outMsName)
    casaCmd = casaCmd + "os.system('rm -rf %s.flagversions') \n\n"%(outMsName)
    if intentsToDiscard != '': casaCmd = casaCmd + "listOfIntents = %s\n\n"%(pprint.pformat(listOfIntents))
    if useMStransform == True:
        casaCmd = casaCmd + "mstransform(vis = '"+msName1+"',\n"
    else:
        casaCmd = casaCmd + "split(vis = '"+msName1+"',\n"
    casaCmd = casaCmd + "  outputvis = '"+outMsName+"',\n"
    casaCmd = casaCmd + "  datacolumn = 'corrected',\n"
    if splitMyScienceSpw == True:
        casaCmd = casaCmd + "  spw = '"+spwIds+"',\n"
        if useMStransform == True: casaCmd = casaCmd + "  reindex = "+str(reindexMyScienceSpw)+",\n"
    if intentsToDiscard != '': casaCmd = casaCmd + "  intent = ','.join(listOfIntents),\n"        
    if timebin != 0.:
        if useMStransform == True: casaCmd = casaCmd + "  timeaverage = True,\n"
        casaCmd = casaCmd + "  timebin = '"+str(timebin)+"s',\n"
    if allowHybrid != True and antNames2 != '': casaCmd = casaCmd + "  antenna = '"+antNames2+"',\n"
    casaCmd = casaCmd + "  keepflags = True)\n\n"

    return casaCmd


###################################

def doSaveFlags(msName, name=''):
    """Generate code for the flag-saving step of a calibration script in preparation of
    a subsequent step which modifies the flags.

    name - The flag version name (obligatory)"""

    print('\n*** doSaveFlags ***')

    if name == '': 
        casalog.post('ERROR: Missing version name.','SEVERE')
        return False

    casaCmd = ''

    if name == 'Original':
        casaCmd = casaCmd + "\nif not os.path.exists('"+msName+".flagversions/Original.flags'):\n"
        casaCmd = casaCmd + "  flagmanager(vis = '"+msName+"',\n"
        casaCmd = casaCmd + "    mode = 'save',\n"
        casaCmd = casaCmd + "    versionname = '"+name+"')\n\n"
    else:
        casaCmd = casaCmd + "\nflagmanager(vis = '"+msName+"',\n"
        casaCmd = casaCmd + "  mode = 'save',\n"
        casaCmd = casaCmd + "  versionname = '"+name+"')\n\n"

    return casaCmd

####################################

def doClearPointingTable(msName):

    print('\n*** doClearPointingTable ***')

    casaCmd = ''

    casaCmd = casaCmd + "tb.open('"+msName+"/POINTING', nomodify = False)\n"
    casaCmd = casaCmd + "a = tb.rownumbers()\n"
    casaCmd = casaCmd + "tb.removerows(a)\n"
    casaCmd = casaCmd + "tb.close()\n"

    return casaCmd

####################################

def doInitialFlagging(msName, msName1='', chanEdge=0.0625, thresh=0.2, iHaveSplitMyScienceSpw=False):
    """Generate code for the initial flagging step of a calibration script."""

    print('\n*** doInitialFlagging ***')

    specLines = {'Neptune': [[114.00,116.50], [227.00,234.50], [340.00,351.50], [455.00,467.50], [686.00,696.50], [803.00,810.50]], # CO
        'Titan': [[114.93,115.66], [229.51,231.71], [343.86,347.58], [458.34,463.74], [687.83,694.58], [803.55,809.76], # CO
        [110.19,110.21], [220.30,220.50], [330.39,330.78], [440.47,441.10], [660.68,661.46], [770.74,771.63], [880.83,881.72], # 13CO
        [88.46,88.80], [176.75,177.78], [264.99,266.78], [353.33,355.68], [441.79,444.47], [618.52,622.09], [707.09,710.66], [795.65,799.22], [883.92,887.75], # HCN
        [86.05,86.06], [172.07,172.14], [258.07,258.24], [430.07,430.40], [602.02,602.53], [773.97,774.55], [859.93,860.52], # HC15N
        [86.34,86.34], [172.65,172.71], [258.94,259.09], [431.49,431.83], [604.04,604.49], [776.56,777.08], [862.81,863.33] # H13CN
        ]}

    if msName1 == '': msName1 = msName

    casaCmd = ''

    casaCmd = casaCmd + "# Flagging shadowed data\n\n"
    casaCmd = casaCmd + "flagdata(vis = '"+msName1+"',\n"
    casaCmd = casaCmd + "  mode = 'shadow',\n"
    casaCmd = casaCmd + "  flagbackup = False)\n\n"

    spwInfo = sfsdr.getSpwInfo(msName, intent='OBSERVE_TARGET|CALIBRATE_BANDPASS|CALIBRATE_DIFFGAIN', caching=True)
    spwIds = sorted(spwInfo.keys())

    sciNumChans = []
    for i in sorted(spwInfo.keys()): sciNumChans.append(spwInfo[i]['numChans'])
    if len(list(dict.fromkeys(sciNumChans).keys())) != 1:
        print('WARNING: This seems to be a mixed-mode dataset, the script generator has been updated, but for some time, please check carefully the script.')

    spwSpec = ''

    mymsmd = msmdtool()
    mymsmd.open(msName)
    TDMspws = mymsmd.tdmspws()
    mymsmd.close()

    for i in range(len(spwIds)):
        if spwIds[i] in TDMspws:
            if spwSpec != '': spwSpec = spwSpec+','
            if iHaveSplitMyScienceSpw == True:
                spwSpec = spwSpec+str(i)
            else:
                spwSpec = spwSpec+str(spwIds[i])
            spwSpec = spwSpec+':0~'+str(np.long(sciNumChans[i]*chanEdge-1))+';'+str(np.long(sciNumChans[i]-sciNumChans[i]*chanEdge))+'~'+str(sciNumChans[i]-1)


    if spwSpec != '':
        casaCmd = casaCmd + "# Flagging edge channels\n\n"
        casaCmd = casaCmd + "flagdata(vis = '"+msName1+"',\n"
        casaCmd = casaCmd + "  mode = 'manual',\n"
        casaCmd = casaCmd + "  spw = '"+spwSpec+"',\n"
        casaCmd = casaCmd + "  flagbackup = False)\n\n"

    spwInfo1 = sfsdr.getSpwInfo(msName, intent='CALIBRATE_AMPLI|CALIBRATE_FLUX', caching=True)

    if len(spwInfo1) != 0:

        spwIds = sorted(spwInfo1.keys())

        intentSources = sfsdr.getIntentsAndSourceNames(msName)
        calFieldIds = intentSources['CALIBRATE_AMPLI']['id'] + intentSources['CALIBRATE_FLUX']['id']
        calFieldIds = [i for i in calFieldIds if i != '']
        calFieldNames = intentSources['CALIBRATE_AMPLI']['name'] + intentSources['CALIBRATE_FLUX']['name']
        calFieldNames = [i for i in calFieldNames if i != '']

        for i in range(len(calFieldIds)):
            if calFieldNames[i] in list(specLines.keys()):
                for j in specLines[calFieldNames[i]]:
                    for k in range(len(spwIds)):
                        spwId1 = int(sorted(spwInfo1.keys())[k])
                        chanRange = aU.getChanRangeFromFreqRange(vis = msName, spwid = spwId1, minf = j[0]*1.e9, maxf = j[1]*1.e9)
                        if chanRange == [-1, -1]: continue

                        if iHaveSplitMyScienceSpw == True:
                            spwId2 = sorted(spwInfo.keys()).index(spwId1)
                        else:
                            spwId2 = spwId1

                        if (chanRange[1]-chanRange[0]) / (spwInfo[spwId1]['numChans']*1.) > thresh: print('# Warning: more than '+str(thresh*100)+'% of spw '+str(spwId2)+' on '+calFieldNames[i]+' will be flagged due to atmospheric line.')
                        spwSpec = str(spwId2)+':'+str(chanRange[0])+'~'+str(chanRange[1])

                        casaCmd = casaCmd + "# Flagging atmospheric line(s)\n\n"
                        casaCmd = casaCmd + "flagdata(vis = '"+msName1+"',\n"
                        casaCmd = casaCmd + "  mode = 'manual',\n"
                        casaCmd = casaCmd + "  spw = '"+spwSpec+"',\n"
                        casaCmd = casaCmd + "  field = '"+str(calFieldIds[i])+"',\n"
                        casaCmd = casaCmd + "  flagbackup = False)\n\n"

    return casaCmd

#####################################

def doRunSetjy(msName, msName1='', iHaveSplitMyScienceSpw=False, useCalibratorService=False, valueMaps={}):
    """Generate code for the setjy step of a calibration script,
    i.e. for setting a model for the flux calibrator(s)."""

    print('\n*** doRunSetjy ***')

    if msName1 == '': msName1 = msName

    casaCmd = ''

    fieldIds = sfsdr.getFieldsForSetjy(msName)
    mytb = aU.createCasaTool(tbtool)

    mytb.open(msName+'/FIELD')
    fieldNames = mytb.getcol('NAME')
    ephemerisIds = mytb.getcol('EPHEMERIS_ID')
    phaseDirKeywords = mytb.getcolkeywords('PHASE_DIR')
    phaseDirRef = mytb.getcol('PhaseDir_Ref')
    mytb.close()

    ij = np.where(phaseDirKeywords['MEASINFO']['TabRefTypes'] == 'ICRS')[0][0]
    icrscode = phaseDirKeywords['MEASINFO']['TabRefCodes'][ij]

    if fieldIds != []:

        fieldNames1 = ['%s' %fieldNames[i] for i in fieldIds]
        fieldNames = ','.join(fieldNames1)
        fieldIds = ['%s' %i for i in fieldIds]
        fieldIds1 = ','.join(fieldIds)

        spwInfo = sfsdr.getSpwInfo(msName, intent='CALIBRATE_AMPLI|CALIBRATE_FLUX', caching=True)
        spwIds = sorted(spwInfo.keys())

        if iHaveSplitMyScienceSpw == True:
            spwInfo1 = sfsdr.getSpwInfo(msName, intent='OBSERVE_TARGET|CALIBRATE_BANDPASS|CALIBRATE_DIFFGAIN', caching=True)
            spwIds1 = sorted(spwInfo1.keys())
            spwIds = [spwIds1.index(i) for i in spwIds]
        spwIds = ['%s' %i for i in spwIds]
        spwIds = ','.join(spwIds)

        if aU.getCasaVersion() == '4.5.0':

            for i in range(len(fieldIds)):

                casaCmd = casaCmd + "setjy(vis = '"+msName1+"',\n"
                casaCmd = casaCmd + "  field = '"+fieldIds[i]+"', # "+fieldNames1[i]+"\n"
                casaCmd = casaCmd + "  spw = '"+spwIds+"',\n"

                if ephemerisIds[int(fieldIds[i])] != -1 and phaseDirRef[int(fieldIds[i])] == icrscode:
                    casaCmd = casaCmd + "  standard = 'Butler-JPL-Horizons 2012',\n"
                    casaCmd = casaCmd + "  useephemdir = True)\n\n"
                else:
                    casaCmd = casaCmd + "  standard = 'Butler-JPL-Horizons 2012')\n\n"

        else:

            casaCmd = casaCmd + "setjy(vis = '"+msName1+"',\n"
            casaCmd = casaCmd + "  field = '"+fieldIds1+"', # "+fieldNames+"\n"
            casaCmd = casaCmd + "  spw = '"+spwIds+"',\n"
            haveEphemFluxcal=False
            for i in range(len(fieldIds)):
                if ephemerisIds[int(fieldIds[i])] != -1 and phaseDirRef[int(fieldIds[i])] == icrscode:
                    haveEphemFluxcal=True
                    casalog.post('There are ephemeris objects among the flux calibrators: field id '+str(fieldIds[i]), 'WARN')

            casaCmd = casaCmd + "  usescratch = True,\n" # because of bug in virt. model (see SCIREQ-2035)
            casaCmd = casaCmd + "  standard = 'Butler-JPL-Horizons 2012')\n\n"

        casaCmd = casaCmd + "if applyonly != True:\n"
        casaCmd = casaCmd + "  os.system('rm -rf %s.setjy.field*.png') \n"%(msName1)
        casaCmd = casaCmd + "  for i in "+str(fieldIds)+":\n"
        casaCmd = casaCmd + "    plotms(vis = '"+msName1+"',\n"
        casaCmd = casaCmd + "      xaxis = 'uvdist',\n"
        casaCmd = casaCmd + "      yaxis = 'amp',\n"
        casaCmd = casaCmd + "      ydatacolumn = 'model',\n"
        casaCmd = casaCmd + "      field = str(i),\n"
        casaCmd = casaCmd + "      spw = '"+spwIds+"',\n"
        casaCmd = casaCmd + "      avgchannel = '9999',\n"
        casaCmd = casaCmd + "      coloraxis = 'spw',\n"
        casaCmd = casaCmd + "      plotfile = '"+msName1+".setjy.field'+i+'.png')\n"

    else:

        intentSources = sfsdr.getIntentsAndSourceNames(msName)
        intentSources1 = intentSources['CALIBRATE_FLUX']

        if len(intentSources1['sourceid']) != 0:

            found = 0

            fluxCalSourceId = intentSources1['sourceid']
            fluxCalSourceNames = intentSources1['name']

            found = 0

            sourceFluxes = aU.getALMAFluxForMS(msName, useCalibratorService=useCalibratorService)

            if len(sourceFluxes) != 0:

                for j in list(sourceFluxes.keys()):
                    if j not in fluxCalSourceNames:
                        sourceFluxes.pop(j)

                if len(sourceFluxes) != 0:

                    found = 1

                    if len(list(sourceFluxes.keys())) > 1:
                        print("WARNING: THERE IS MORE THAN ONE FLUX CALIBRATOR. I WILL PICK THE FIRST ONE. THIS MAY BE WRONG.")

                    fluxCalSourceName = list(sourceFluxes.keys())[0]
                    fluxCalId = list(fieldNames).index(fluxCalSourceName)
                    haveEphemFluxcal=False
                    if ephemerisIds[fluxCalId] != -1 and phaseDirRef[fluxCalId] == icrscode:
                        haveEphemFluxcal=True
                        casalog.post('  The flux calibrator is an ephemeris object: '+fluxCalSourceName, 'WARN')

                    casaCmd = casaCmd + "setjy(vis = '"+msName1+"',\n"
                    casaCmd = casaCmd + "  standard = 'manual',\n"
                    casaCmd = casaCmd + "  field = '"+fluxCalSourceName+"',\n"

                    casaCmd = casaCmd + "  usescratch = True,\n" # because of bug in virt. model (see SCIREQ-2035)

                    casaCmd = casaCmd + "  fluxdensity = ["+str(sourceFluxes[fluxCalSourceName]['fluxDensity'])+", 0, 0, 0],\n"
                    casaCmd = casaCmd + "  spix = "+str(sourceFluxes[fluxCalSourceName]['spectralIndex'])+",\n"
                    casaCmd = casaCmd + "  reffreq = '"+str(sourceFluxes[fluxCalSourceName]['frequency']/1.e9)+"GHz')\n\n"

                    for tag in ['fluxDensityUncertainty', 'meanAge']: casaCmd = casaCmd + "# "+tag+" = "+str(sourceFluxes[fluxCalSourceName][tag])+"\n"

            if found == 0:

                fluxCalSourceId = intentSources1['sourceid']

                sourceFluxes = sfsdr.getFluxesFromSourceTable(msName)

                if len(fluxCalSourceId) > 1:
                    print("WARNING: THERE IS MORE THAN ONE FLUX CALIBRATOR. I WILL PICK THE FIRST ONE. THIS MAY BE WRONG.")
                    fluxCalSourceId = [j for j in fluxCalSourceId if j in list(sourceFluxes.keys())]

                if len(fluxCalSourceId) == 0: 
                    casalog.post("ERROR: There is no flux calibrator.",'SEVERE')
                    return False

                fluxCalSourceId = fluxCalSourceId[0]

                fluxCalSourceName = intentSources1['name'][intentSources1['sourceid'].index(fluxCalSourceId)]

                if len(intentSources1['sourceid']) > 1:
                    mytb.open(msName+'/FIELD')
                    tb1 = mytb.query('SOURCE_ID == '+str(fluxCalSourceId))
                    fluxCalFieldIds1 = tb1.rownumbers().tolist()
                    tb1.close()
                    mytb.close()
                    fluxCalFieldIds = [j for j in fluxCalFieldIds1 if j in intentSources1['id']]
                else:
                    fluxCalFieldIds = intentSources1['id']

                if fluxCalSourceId in sourceFluxes:

                    if fluxCalSourceName != sourceFluxes[fluxCalSourceId]['sourceName']: 
                        casalog.post("ERROR: Source names do not match.",'SEVERE')
                        return False

                    haveEphemFluxcal=False
                    for i in range(len(fluxCalFieldIds)):
                        if ephemerisIds[int(fluxCalFieldIds[i])] != -1 and phaseDirRef[int(fluxCalFieldIds[i])] == icrscode:
                            haveEphemFluxcal=True
                            casalog.post('There are ephemeris objects among the flux calibrators: field id '+str(fluxCalFieldIds[i]), 'WARN')

                    fluxCalFieldIds = ['%s' %i for i in fluxCalFieldIds]
                    fluxCalFieldIds = ','.join(fluxCalFieldIds)

                    if msName in valueMaps.keys():
                        vm = valueMaps[msName]
                        print('Using canned ValueMap.')
                    else:
                        vm = aU.ValueMapping(msName)
                        valueMaps[msName] = vm

                    spwInfo = sfsdr.getSpwInfo(msName, intent='CALIBRATE_FLUX', caching=True)
                    spwIds = sorted(spwInfo.keys())

                    spwMeanFreq = []
                    for j in spwIds: spwMeanFreq.append(vm.spwInfo[j]['meanFreq'])

                    if iHaveSplitMyScienceSpw == True: 
                        spwIds = list(range(len(spwIds)))

                    for j in range(len(spwIds)):

                        frequency1 = []
                        for k in sourceFluxes[fluxCalSourceId]['frequency']: frequency1.append(abs(k-spwMeanFreq[j]))
                        ij = frequency1.index(min(frequency1))

                        frequency1 = sourceFluxes[fluxCalSourceId]['frequency'][ij]
                        flux1 = sourceFluxes[fluxCalSourceId]['flux'][ij]

                        casaCmd = casaCmd + "setjy(vis = '"+msName1+"',\n"
                        casaCmd = casaCmd + "  field = '"+fluxCalFieldIds+"', # source name = "+fluxCalSourceName+"\n"
                        casaCmd = casaCmd + "  spw = '"+str(spwIds[j])+"', # center frequency of spw = "+str(spwMeanFreq[j]/1.e9)+"GHz\n"
                        casaCmd = casaCmd + "  usescratch = True,\n" # because of bug in virt. model (see SCIREQ-2035)
                        casaCmd = casaCmd + "  standard = 'manual',\n"
                        casaCmd = casaCmd + "  fluxdensity = ["+str(flux1)+", 0, 0, 0]) # frequency of measurement = "+str(frequency1/1.e9)+"GHz\n\n"


    return casaCmd

#######################################

def doBandpassCalibration(msName, msName1='', bpassCalId='', chanAvg=1.0, refant='', iHaveSplitMyScienceSpw=False, 
                          calTableName=[], lowSNR=False, doplot=True, phaseDiff=False, solnorm=True, lbc=False, 
                          isB2B=None, isBWSW=None, isFullP=None, valueMaps={}):
    """Generate code for the bandpass calibration step of a calibration script."""

    print('\n*** doBandpassCalibration ***')

    casaCmd = ''

    if msName1 == '': 
        msName1 = msName
    if refant == '': 
        casalog.post('ERROR: No reference antenna specified.','SEVERE')
        return False
    if chanAvg > 1: 
        casalog.post('ERROR: The channel averaging bandwidth must be specified as a fraction of the total bandwidth.','SEVERE')
        return False
    if lowSNR == True: 
        chanAvg = 1.0

    if isB2B==None and isBWSW==None: # neither officially B2B nor BWSW but we double-check in case this method is called from outside the scriptgen
        isB2B, isBWSW = isB2BorBWSW(msName, valueMaps)

    if isB2B: 
        print('WARNING: TREATING THIS AS A B2B-TRANSFER OBSERVATION.')
        solnorm = True
        phaseDiff = True
    elif isBWSW:
        print('WARNING: TREATING THIS AS A BW-SWITCHING OBSERVATION.')
        print('WARNING: Forcing solnorm to False.')
        solnorm = False
        phaseDiff = True
    elif phaseDiff == True and solnorm == True:
        print('WARNING: phaseDiff True and not B2B, forcing solnorm to False.')
        solnorm = False

    if isFullP==None: # not officially full pol but we double-check in case this method is called from outside the scriptgen
        isFullP = isFullPol(msName, valueMaps)


    mytb = aU.createCasaTool(tbtool)
    mytb.open(msName+'/FIELD')
    fieldNames = mytb.getcol('NAME')
    mytb.close()

    ###

    print('Selecting bandpass calibrator ...')

    intentSources = sfsdr.getIntentsAndSourceNames(msName)
    ampCalId = intentSources['CALIBRATE_AMPLI']['id'] + intentSources['CALIBRATE_FLUX']['id']
    ampCalId = np.unique([i for i in ampCalId if i != '']).tolist()

    if len(ampCalId) > 1:
        casaCmd = casaCmd + "# Note: there is more than one flux calibrator, picking the first one: "+fieldNames[ampCalId[0]]+".\n"
        ampCalId = ampCalId[0]

    if bpassCalId == '':

        bpassCalId = intentSources['CALIBRATE_BANDPASS']['id']

        if bpassCalId[0] != '':

            if len(bpassCalId) != 1: casaCmd = casaCmd + "# Note: there is more than one bandpass calibrator, picking the first one: "+fieldNames[bpassCalId[0]]+".\n"
            bpassCalId = bpassCalId[0]

        elif isB2B:
            casaCmd = casaCmd + "# Note: there are no bandpass calibrators. Will try to use the diffgain calibrator instead.\n"
            phaseCalId = intentSources['CALIBRATE_DIFFGAIN']['id']
            if len(phaseCalId) > 0:
                bpassCalId = phaseCalId[0]
            else:
                casalog.post('ERROR: no bandpass or diffgain calibrator found in this alleged B2B observation.','SEVERE')
                return False
        else:
            casaCmd = casaCmd + "# Note: there are no bandpass calibrators. Will try to use a phase calibrator instead.\n"
            phaseCalId = intentSources['CALIBRATE_PHASE']['id']
            phaseOnlyCalId = [i for i in phaseCalId if i not in ampCalId]
            if len(phaseOnlyCalId) != 1: casaCmd = casaCmd + "# Note: there is more than one phase calibrator, picking the first one: "+fieldNames[phaseOnlyCalId[0]]+".\n"
            bpassCalId = phaseOnlyCalId[0]

    print('Will use field ID '+str(bpassCalId))

    ###

    spwInfo = sfsdr.getSpwInfo(msName, intent='OBSERVE_TARGET|CALIBRATE_BANDPASS', caching=True)
    spwIds = sorted(spwInfo.keys())

    sciNumChans = []
    for i in spwIds: 
        sciNumChans.append(spwInfo[i]['numChans'])
    if not isB2B and len(list(dict.fromkeys(sciNumChans).keys())) != 1:
        print('WARNING: This seems to be a mixed-mode dataset, please double-check the script.')

    spwSpec = ''
    for i in range(len(spwIds)):
        if spwSpec != '': spwSpec = spwSpec+','
        startChan = int((spwInfo[spwIds[i]]['numChans'] / 2.) * (1-chanAvg))
        endChan = int((spwInfo[spwIds[i]]['numChans'] / 2.) * (1+chanAvg))-1
        if iHaveSplitMyScienceSpw == True:
            spwSpec = spwSpec+str(i)
        else:
            spwSpec = spwSpec+str(spwIds[i])
        spwSpec = spwSpec+':'+str(startChan)+'~'+str(endChan)

    hasNoLFbpB2B = False
    if isB2B:
        spwInfoB = sfsdr.getSpwInfo(msName, intent='CALIBRATE_DIFFGAIN', caching=True)
        spwBIds = sorted(spwInfoB.keys())
        if spwBIds != spwIds:
            casalog.post('This B2B observation has no LF bandpass cal. Will use DiffGainCal.', 'WARN')
            hasNoLFbpB2B = True

            dgCalId = intentSources['CALIBRATE_DIFFGAIN']['id'][0]

            sciBNumChans = []
            for i in spwBIds: 
                sciBNumChans.append(spwInfoB[i]['numChans'])

            spwBSpec = ''
            for i in range(len(spwBIds)):
                if spwBSpec != '': spwBSpec = spwBSpec+','
                startChan = int((spwInfoB[spwBIds[i]]['numChans'] / 2.) * (1-chanAvg))
                endChan = int((spwInfoB[spwBIds[i]]['numChans'] / 2.) * (1+chanAvg))-1
                if iHaveSplitMyScienceSpw == True:
                    spwBSpec = spwBSpec+str(i)
                else:
                    spwBSpec = spwBSpec+str(spwBIds[i])
                spwBSpec = spwBSpec+':'+str(startChan)+'~'+str(endChan)

    ###

    print('Determining scan list ...')

    bpassCalScanList = []

    mymsmd = msmdtool()
    mymsmd.open(msName)
    if 'CALIBRATE_BANDPASS#ON_SOURCE' in mymsmd.intentsforfield(bpassCalId):
        bpassCalScanList = mymsmd.scansforintent('CALIBRATE_BANDPASS#ON_SOURCE') 
    else:
        bpassCalScanList = mymsmd.scansforfield(bpassCalId) 

    atmCalScanList = mymsmd.scansforintent('CALIBRATE_ATMOSPHERE*')
    bpassCalScanList = [str(i) for i in bpassCalScanList if i not in atmCalScanList]

    print(bpassCalScanList)

    bpassCalScanList = ','.join(bpassCalScanList)

    if isB2B:
        diffGainCalScanListLow  = ','.join([str(i) for i in mymsmd.scansforintent('CALIBRATE_DIFFGAIN#REFERENCE')])

    mymsmd.close()

    print('Writing code ...')

    calTableName1 = msName1+'.bandpass'
    casaCmd = casaCmd + "os.system('rm -rf %s.ap_pre_bandpass') \n"%(msName1)
    casaCmd = casaCmd + "\ngaincal(vis = '"+msName1+"',\n"
    casaCmd = casaCmd + "  caltable = '"+msName1+".ap_pre_bandpass',\n"
    casaCmd = casaCmd + "  field = '"+str(bpassCalId)+"', # "+fieldNames[int(bpassCalId)]+"\n"
    casaCmd = casaCmd + "  spw = '"+spwSpec+"',\n"
    if bpassCalScanList != '': 
        casaCmd = casaCmd + "  scan = '"+bpassCalScanList+"',\n"
    casaCmd = casaCmd + "  solint = 'int',\n"
    casaCmd = casaCmd + "  refant = '"+refant+"',\n"
    if isB2B:
        casaCmd = casaCmd + "  refantmode = 'strict',\n"
        casaCmd = casaCmd + "  minsnr = 1.0,\n"
    elif isFullP:
        casaCmd = casaCmd + "  refantmode = 'strict',\n"
    casaCmd = casaCmd + "  calmode = 'p')\n"

    if hasNoLFbpB2B:
        casaCmd = casaCmd + "\ngaincal(vis = '"+msName1+"',\n"
        casaCmd = casaCmd + "  caltable = '"+msName1+".ap_pre_bandpass',\n"
        casaCmd = casaCmd + "  field = '"+str(dgCalId)+"', # "+fieldNames[int(dgCalId)]+"\n"
        casaCmd = casaCmd + "  spw = '"+spwBSpec+"',\n"
        casaCmd = casaCmd + "  scan = '"+diffGainCalScanListLow+"',\n"
        casaCmd = casaCmd + "  solint = 'int',\n"
        casaCmd = casaCmd + "  minsnr = 1.0,\n"
        casaCmd = casaCmd + "  refant = '"+refant+"',\n"
        casaCmd = casaCmd + "  refantmode = 'strict',\n"
        casaCmd = casaCmd + "  calmode = 'p',\n"
        casaCmd = casaCmd + "  append = True)\n"

    if doplot:
        casaCmd = casaCmd + "\nif applyonly != True: es.checkCalTable('"+msName1+".ap_pre_bandpass', msName='"+msName1+"', interactive=False) \n\n"

    casaCmd = casaCmd + "os.system('rm -rf %s.bandpass') \n"%(msName1)
    casaCmd = casaCmd + "bandpass(vis = '"+msName1+"',\n"
    casaCmd = casaCmd + "  caltable = '"+calTableName1+"',\n"
    casaCmd = casaCmd + "  field = '"+str(bpassCalId)+"', # "+fieldNames[int(bpassCalId)]+"\n"
    if bpassCalScanList != '': 
        casaCmd = casaCmd + "  scan = '"+bpassCalScanList+"',\n"
    casaCmd = casaCmd + "  solint = 'inf',\n"
    if isB2B:
        casaCmd = casaCmd + "  minsnr = 1.0,\n"
    casaCmd = casaCmd + "  combine = 'scan',\n"
    casaCmd = casaCmd + "  refant = '"+refant+"',\n"
    casaCmd = casaCmd + "  solnorm = "+str(solnorm)+",\n"
    casaCmd = casaCmd + "  bandtype = 'B',\n"
    casaCmd = casaCmd + "  gaintable = '"+msName1+".ap_pre_bandpass')\n"

    if hasNoLFbpB2B:
        casaCmd = casaCmd + "bandpass(vis = '"+msName1+"',\n"
        casaCmd = casaCmd + "  caltable = '"+calTableName1+"',\n"
        casaCmd = casaCmd + "  field = '"+str(dgCalId)+"', # "+fieldNames[int(dgCalId)]+"\n"
        casaCmd = casaCmd + "  scan = '"+diffGainCalScanListLow+"',\n"
        casaCmd = casaCmd + "  solint = 'inf',\n"
        casaCmd = casaCmd + "  minsnr = 1.0,\n"
        casaCmd = casaCmd + "  combine = 'scan',\n"
        casaCmd = casaCmd + "  refant = '"+refant+"',\n"
        casaCmd = casaCmd + "  solnorm = "+str(solnorm)+",\n"
        casaCmd = casaCmd + "  bandtype = 'B',\n"
        casaCmd = casaCmd + "  gaintable = '"+msName1+".ap_pre_bandpass',\n"
        casaCmd = casaCmd + "  append = True)\n"

    if doplot:
        casaCmd = casaCmd + "\nif applyonly != True: es.checkCalTable('"+calTableName1+"', msName='"+msName1+"', interactive=False) \n"

    minSciNumChans = min(sciNumChans)

    if minSciNumChans > 256 or isBWSW:
        casaCmd = casaCmd + "\nos.system('rm -rf %s.bandpass_smooth20ch') \n"%(msName1)
        casaCmd = casaCmd + "\nbandpass(vis = '"+msName1+"',\n"
        casaCmd = casaCmd + "  caltable = '"+calTableName1+'_smooth20ch'+"',\n"
        casaCmd = casaCmd + "  field = '"+str(bpassCalId)+"', # "+fieldNames[int(bpassCalId)]+"\n"
        if bpassCalScanList != '': 
            casaCmd = casaCmd + "  scan = '"+bpassCalScanList+"',\n"
        if lbc or isBWSW or isB2B:
            casaCmd = casaCmd + "  solint = 'inf,8MHz',\n"
        else:
            casaCmd = casaCmd + "  solint = 'inf,20ch',\n"
        casaCmd = casaCmd + "  combine = 'scan',\n"
        casaCmd = casaCmd + "  refant = '"+refant+"',\n"
        casaCmd = casaCmd + "  solnorm = "+str(solnorm)+",\n"
        casaCmd = casaCmd + "  bandtype = 'B',\n"
        casaCmd = casaCmd + "  gaintable = '"+msName1+".ap_pre_bandpass')\n"

        if hasNoLFbpB2B:
            casaCmd = casaCmd + "\nbandpass(vis = '"+msName1+"',\n"
            casaCmd = casaCmd + "  caltable = '"+calTableName1+'_smooth20ch'+"',\n"
            casaCmd = casaCmd + "  field = '"+str(dgCalId)+"', # "+fieldNames[int(dgCalId)]+"\n"
            casaCmd = casaCmd + "  scan = '"+diffGainCalScanListLow+"',\n"
            casaCmd = casaCmd + "  solint = 'inf,8MHz',\n"
            casaCmd = casaCmd + "  minsnr = 1.0,\n"
            casaCmd = casaCmd + "  combine = 'scan',\n"
            casaCmd = casaCmd + "  refant = '"+refant+"',\n"
            casaCmd = casaCmd + "  solnorm = "+str(solnorm)+",\n"
            casaCmd = casaCmd + "  bandtype = 'B',\n"
            casaCmd = casaCmd + "  gaintable = '"+msName1+".ap_pre_bandpass',\n"
            casaCmd = casaCmd + "  append = True)\n"

        if doplot:
            casaCmd = casaCmd + "\nif applyonly != True: es.checkCalTable('"+calTableName1+'_smooth20ch'+"', msName='"+msName1+"', interactive=False) \n"


    if minSciNumChans > 256:
        calTableName1 = calTableName1+'_smooth20ch'

    calTableName.append(calTableName1)

    if phaseDiff == True:
        if bpassCalId != ampCalId:

            casaCmd = casaCmd + "\n\n"

            fluxscaleDictName = []
            casaCmd = casaCmd + doGainCalibration(msName, msName1=msName1, refant=refant, bandpass=calTableName1, calmode2='a', phaseDiff=False, fluxscaleDictName=fluxscaleDictName, iHaveSplitMyScienceSpw=iHaveSplitMyScienceSpw, isB2B=False, isBWSW=False, isFullP=isFullP, valueMaps=valueMaps)

            casaCmd = casaCmd + "\nsetjy(vis = '"+msName1+"',\n"
            casaCmd = casaCmd + "  field = '"+str(bpassCalId)+"', # "+fieldNames[int(bpassCalId)]+"\n"
            casaCmd = casaCmd + "  standard = 'manual',\n"
            casaCmd = casaCmd + "  spw = '',\n"
            casaCmd = casaCmd + "  fluxdensity = "+fluxscaleDictName[0]+"['"+str(bpassCalId)+"']['fitFluxd'],\n"
            casaCmd = casaCmd + "  spix = "+fluxscaleDictName[0]+"['"+str(bpassCalId)+"']['spidx'][1],\n"  # Added trailing [1] - T. Hunter 2014-08-11
            casaCmd = casaCmd + "  reffreq = '%fGHz'%(1e-9*"+fluxscaleDictName[0]+"['"+str(bpassCalId)+"']['fitRefFreq']))\n\n" # T. Hunter 2014-08-11

            calTableName1 = msName1+'.bandpass2'
            casaCmd = casaCmd + "os.system('rm -rf %s.bandpass2') \n"%(msName1) # Added by CLB
            casaCmd = casaCmd + "bandpass(vis = '"+msName1+"',\n"
            casaCmd = casaCmd + "  caltable = '"+calTableName1+"',\n"
            casaCmd = casaCmd + "  field = '"+str(bpassCalId)+"', # "+fieldNames[int(bpassCalId)]+"\n"
            if bpassCalScanList != '': 
                casaCmd = casaCmd + "  scan = '"+bpassCalScanList+"',\n"
            casaCmd = casaCmd + "  solint = 'inf',\n"
            casaCmd = casaCmd + "  combine = 'scan',\n"
            casaCmd = casaCmd + "  refant = '"+refant+"',\n"
            casaCmd = casaCmd + "  solnorm = False,\n"
            casaCmd = casaCmd + "  bandtype = 'B',\n"
            casaCmd = casaCmd + "  gaintable = '"+msName1+".ap_pre_bandpass')\n"

            minSciNumChans = min(sciNumChans)

            if minSciNumChans > 256:
                casaCmd = casaCmd + "\nos.system('rm -rf %s.bandpass2_smooth20ch') \n"%(msName1)
                casaCmd = casaCmd + "\nbandpass(vis = '"+msName1+"',\n"
                casaCmd = casaCmd + "  caltable = '"+calTableName1+'_smooth20ch'+"',\n"
                casaCmd = casaCmd + "  field = '"+str(bpassCalId)+"', # "+fieldNames[int(bpassCalId)]+"\n"
                if bpassCalScanList != '': 
                    casaCmd = casaCmd + "  scan = '"+bpassCalScanList+"',\n"
                casaCmd = casaCmd + "  solint = 'inf,20ch',\n"
                casaCmd = casaCmd + "  combine = 'scan',\n"
                casaCmd = casaCmd + "  refant = '"+refant+"',\n"
                casaCmd = casaCmd + "  solnorm = False,\n"
                casaCmd = casaCmd + "  bandtype = 'B',\n"
                casaCmd = casaCmd + "  gaintable = '"+msName1+".ap_pre_bandpass')\n"
                if doplot == True:
                    casaCmd = casaCmd + "\nif applyonly != True: es.checkCalTable('"+calTableName1+'_smooth20ch'+"', msName='"+msName1+"', interactive=False) \n"

            if doplot == True:
                casaCmd = casaCmd + "\nif applyonly != True: es.checkCalTable('"+calTableName1+"', msName='"+msName1+"', interactive=False) \n"

            if minSciNumChans > 256:
                calTableName1 = calTableName1+'_smooth20ch'

            calTableName[0] = calTableName1

    return casaCmd

##################################

def doGainCalibration(msName, msName1='', refant='', bandpass='', gaintypeForAmp='T', doplot=True, calFieldsOnly=True, calmode2='ap', phaseDiff='', phaseDiffCalTableName=[], fluxscaleDictName=[], ampForSci=[], iHaveSplitMyScienceSpw=False, phaseDiffPerSpwSetup=False, isB2B=None, isBWSW=None, isFullP=None, valueMaps={}):
    """Generate code for the gain calibration step of a calibration script."""

    print('\n*** doGainCalibration ***')

    if msName1 == '': msName1 = msName
    if refant == '': 
        casalog.post('ERROR: No reference antenna specified.', 'SEVERE')
        return False
    if bandpass == '': 
        casalog.post('ERROR: No bandpass cal table specified.', 'SEVERE')
        return False

    fluxscaleDictName.append('fluxscaleDict')

    mytb = aU.createCasaTool(tbtool)
    mytb.open(msName+'/FIELD')
    fieldNames = mytb.getcol('NAME')
    mytb.close()

    fieldIds = list(range(len(fieldNames)))

    intentSources = sfsdr.getIntentsAndSourceNames(msName)
    sciFieldIds = intentSources['OBSERVE_TARGET']['id']
    if 'OBSERVE_CHECK' in list(intentSources.keys()): 
        sciFieldIds += intentSources['OBSERVE_CHECK']['id']
    if sciFieldIds[0] == '': 
        casalog.post('THERE SEEMS TO BE NO SCIENCE FIELD', 'WARN')

    if calFieldsOnly == True:
        calFieldIds = [i for i in fieldIds if i not in sciFieldIds]
    else:
        calFieldIds = [i for i in fieldIds]

    mymsmd = msmdtool()
    mymsmd.open(msName)
    hasdata = []
    for i in calFieldIds:
        calFieldIntents = mymsmd.intentsforfield(i)
        hasdata1 = 0
        for j in calFieldIntents:
            if re.search('^CALIBRATE_(POINTING|ATMOSPHERE|WVR)', j) == None:
                hasdata1 = 1
                break
        hasdata.append(hasdata1)
    mymsmd.close()

    calFieldIds = [calFieldIds[i] for i in range(len(calFieldIds)) if hasdata[i] == 1]


    if len(calFieldIds) == '': 
        casalog.post('ERROR: There seems to be no calibrator field.', 'SEVERE')
        return False

    calFieldNames = [fieldNames[i] for i in calFieldIds]
    calFieldNames = ','.join(calFieldNames)

    if len(calFieldIds) > 1:
        j0 = 0
        calFieldIds1 = str(calFieldIds[j0])
        for j in range(len(calFieldIds)-1):
            if calFieldIds[j+1] == calFieldIds[j]+1: continue
            calFieldIds1 = calFieldIds1 + '~' + str(calFieldIds[j])
            j0 = j+1
            calFieldIds1 = calFieldIds1 + ',' + str(calFieldIds[j0])
        calFieldIds1 = calFieldIds1 + '~' + str(calFieldIds[j+1])
    else:
        calFieldIds1 = str(calFieldIds[0])

    spwInfo = sfsdr.getSpwInfo(msName, caching=True)
    spwIds = sorted(spwInfo.keys())

    ###

    if isB2B==None and isBWSW==None: # neither officially B2B nor BWSW but we double-check in case this method is called from outside the scriptgen
        isB2B, isBWSW = isB2BorBWSW(msName, valueMaps)

    if isB2B == True: 
        casalog.post('You have set parameter isB2B to True. doGainCalibration does not support that. Use doB2BGainCalibrationPartI() and ...PartII() instead of doGainCalibration()', 'SEVERE')
        return False
    elif isBWSW:
        casalog.post('TREATING THIS AS A BW-SWITCHING OBSERVATION.','WARN')
        print('WARNING: Forcing phaseDiff=True.')
        phaseDiff = True

    if isFullP==None: # not officially full pol but we double-check in case this method is called from outside the scriptgen
        isFullP = isFullPol(msName, valueMaps)

    ###

    casaCmd = ''

    if phaseDiff == True:

        if sciFieldIds[0] != '':

            if phaseDiffPerSpwSetup == True:

                bpassCalId = intentSources['CALIBRATE_BANDPASS']['id']
                if bpassCalId[0] == '': 
                    casalog.post('ERROR: There are no bandpass calibrator.', 'SEVERE')
                    return False
                if len(bpassCalId) != 1: 
                    casaCmd = casaCmd + "# Note: there is more than one bandpass calibrator, I'm picking the first one: "+fieldNames[bpassCalId[0]]+".\n"
                bpassCalId = bpassCalId[0]

                diffGainCalId = ''
                diffGainCalScanList = ''
                if 'CALIBRATE_DIFFGAIN' in list(intentSources.keys()):
                    diffGainCalId = intentSources['CALIBRATE_DIFFGAIN']['id']
                    if len(diffGainCalId) != 1: 
                        casaCmd = casaCmd + "# Note: there is more than one diffgain calibrator, I'm picking the first one: "+fieldNames[diffGainCalId[0]]+".\n"
                    diffGainCalId = diffGainCalId[0]
                if diffGainCalId == '':
                    diffGainCalId = bpassCalId
                else:
                    mymsmd = msmdtool()
                    mymsmd.open(msName)
                    diffGainCalScanList = mymsmd.scansforintent('CALIBRATE_DIFFGAIN#*').tolist()
                    mymsmd.close()
                    diffGainCalScanList = [str(j) for j in diffGainCalScanList]
                    diffGainCalScanList = ','.join(diffGainCalScanList)

                casaCmd = casaCmd + "os.system('rm -rf %s.phasediff_inf') \n"%(msName1)
                casaCmd = casaCmd + "gaincal(vis = '"+msName1+"',\n"
                casaCmd = casaCmd + "  caltable = '"+msName1+".phasediff_inf',\n"
                casaCmd = casaCmd + "  field = '"+str(diffGainCalId)+"',\n"
                if diffGainCalScanList != '': 
                    casaCmd = casaCmd + "  scan = '"+diffGainCalScanList+"',\n"
                casaCmd = casaCmd + "  solint = 'inf',\n"
                casaCmd = casaCmd + "  combine = 'scan',\n"
                casaCmd = casaCmd + "  refant = '"+refant+"',\n"
                if isFullP:
                    casaCmd = casaCmd + "  refantmode = 'strict',\n"
                casaCmd = casaCmd + "  gaintype = 'G',\n"
                casaCmd = casaCmd + "  calmode = 'p',\n"
                casaCmd = casaCmd + "  gaintable = '"+bandpass+"')\n\n"

                phaseDiffCalTableName.append(msName1+'.phasediff_inf')

                if doplot == True: 
                    casaCmd = casaCmd + "if applyonly != True: es.checkCalTable('"+msName1+".phasediff_inf', msName='"+msName1+"', interactive=False) \n\n"

                ###
                mymsmd = msmdtool()
                mymsmd.open(msName)

                spwIds3 = mymsmd.spwsforintent('CALIBRATE_BANDPASS*').tolist()+mymsmd.spwsforintent('OBSERVE_TARGET*').tolist()
                spwIds3 = np.unique([j for j in spwIds3 if j not in mymsmd.chanavgspws() and j not in mymsmd.wvrspws()]).tolist()

                spwSetups = {}
                for i in spwIds3:
                    scanList3 = str(mymsmd.scansforspw(i).tolist())
                    if scanList3 not in list(spwSetups.keys()): 
                        spwSetups[scanList3] = []
                    spwSetups[scanList3].append(i)
                spwSetups1 = []
                for i in list(spwSetups.values()):
                    if i not in spwSetups1: 
                        spwSetups1.append(i)
                spwSetups1.sort(key=lambda x:x[0])

                calspwmap = []
                for i in range(len(spwSetups1)):
                    for j in range(len(spwSetups1[i])):
                        calspwmap.append(min(spwSetups1[i]))

                if iHaveSplitMyScienceSpw == True: ###HERE

                    for i in range(len(spwSetups1)):
                        for j in range(len(spwSetups1[i])):
                            spwSetups1[i][j] = spwIds3.index(spwSetups1[i][j])

                    for i in range(len(calspwmap)):
                        calspwmap[i] = spwIds3.index(calspwmap[i])

                casaCmd = casaCmd + "calspwmap = "+str(calspwmap)+"\n\n"

                mymsmd.close()

                ###

                casaCmd = casaCmd + "os.system('rm -rf %s.phase_int') \n"%(msName1)
                casaCmd = casaCmd + "for spw in "+str(spwSetups1)+":\n"
                casaCmd = casaCmd + "  gaincal(vis = '"+msName1+"',\n"
                casaCmd = casaCmd + "    caltable = '"+msName1+".phase_int',\n"
                casaCmd = casaCmd + "    field = '"+calFieldIds1+"', # "+calFieldNames+"\n"
                casaCmd = casaCmd + "    solint = 'int',\n"
                casaCmd = casaCmd + "    spw = ','.join([str(j) for j in spw]),\n"
                casaCmd = casaCmd + "    combine = 'spw',\n"
                casaCmd = casaCmd + "    refant = '"+refant+"',\n"
                if isFullP:
                    casaCmd = casaCmd + "    refantmode = 'strict',\n"
                casaCmd = casaCmd + "    gaintype = 'G',\n"
                casaCmd = casaCmd + "    calmode = 'p',\n"
                casaCmd = casaCmd + "    append = True,\n"
                casaCmd = casaCmd + "    gaintable = ['"+bandpass+"', '"+msName1+".phasediff_inf'])\n\n"

                if doplot == True: 
                    casaCmd = casaCmd + "if applyonly != True: es.checkCalTable('"+msName1+".phase_int', msName='"+msName1+"', interactive=False) \n\n"

                casaCmd = casaCmd + "os.system('rm -rf %s.ampli_inf') \n"%(msName1)
                casaCmd = casaCmd + "for spw in "+str(spwSetups1)+":\n"
                casaCmd = casaCmd + "  gaincal(vis = '"+msName1+"',\n"
                casaCmd = casaCmd + "    caltable = '"+msName1+".ampli_inf',\n"
                casaCmd = casaCmd + "    field = '"+calFieldIds1+"', # "+calFieldNames+"\n"
                casaCmd = casaCmd + "    solint = 'inf',\n"
                casaCmd = casaCmd + "    spw = ','.join([str(j) for j in spw]),\n"
                casaCmd = casaCmd + "    combine = 'spw',\n"
                casaCmd = casaCmd + "    refant = '"+refant+"',\n"
                if isFullP:
                    casaCmd = casaCmd + "    refantmode = 'strict',\n"
                casaCmd = casaCmd + "    gaintype = '"+gaintypeForAmp+"',\n"
                casaCmd = casaCmd + "    calmode = 'a',\n"
                casaCmd = casaCmd + "    append = True,\n"
                casaCmd = casaCmd + "    gaintable = ['"+bandpass+"', '"+msName1+".phasediff_inf', '"+msName1+".phase_int'],\n"
                casaCmd = casaCmd + "    spwmap = [[], [], calspwmap])\n\n"

                ampForSci.append(msName1+'.ampli_inf')

                if doplot == True: 
                    casaCmd = casaCmd + "if applyonly != True: es.checkCalTable('"+msName1+".ampli_inf', msName='"+msName1+"', interactive=False) \n\n"

                casaCmd = casaCmd + "os.system('rm -rf %s.flux_inf') \n"%(msName1)
                casaCmd = casaCmd + "os.system('rm -rf %s.fluxscale') \n"%(msName1)
                casaCmd = casaCmd + "mylogfile = casalog.logfile()\n"
                casaCmd = casaCmd + "casalog.setlogfile('"+msName1+".fluxscale')\n\n"
                casaCmd = casaCmd + fluxscaleDictName[0] + " = fluxscale(vis = '"+msName1+"',\n"
                casaCmd = casaCmd + "  caltable = '"+msName1+".ampli_inf',\n"
                casaCmd = casaCmd + "  fluxtable = '"+msName1+".flux_inf',\n"
                casaCmd = casaCmd + "  reference = '"+str(bpassCalId)+"', # "+fieldNames[bpassCalId]+"\n"
                casaCmd = casaCmd + "  refspwmap = calspwmap,\n"
                casaCmd = casaCmd + "  incremental = True)\n\n"
                casaCmd = casaCmd + "casalog.setlogfile(mylogfile)\n\n"
                casaCmd = casaCmd + "if applyonly != True: es.fluxscale2(caltable = '"+msName1+".ampli_inf', removeOutliers=True, msName='"+msName+"', writeToFile=True, preavg=10000)\n\n"

                casaCmd = casaCmd + "os.system('rm -rf %s.phase_inf') \n"%(msName1)
                casaCmd = casaCmd + "for spw in "+str(spwSetups1)+":\n"
                casaCmd = casaCmd + "  gaincal(vis = '"+msName1+"',\n"
                casaCmd = casaCmd + "    caltable = '"+msName1+".phase_inf',\n"
                casaCmd = casaCmd + "    field = '"+calFieldIds1+"', # "+calFieldNames+"\n"
                casaCmd = casaCmd + "    solint = 'inf',\n"
                casaCmd = casaCmd + "    spw = ','.join([str(j) for j in spw]),\n"
                casaCmd = casaCmd + "    combine = 'spw',\n"
                casaCmd = casaCmd + "    refant = '"+refant+"',\n"
                if isFullP:
                    casaCmd = casaCmd + "    refantmode = 'strict',\n"
                casaCmd = casaCmd + "    gaintype = 'G',\n"
                casaCmd = casaCmd + "    calmode = 'p',\n"
                casaCmd = casaCmd + "    append = True,\n"
                casaCmd = casaCmd + "    gaintable = ['"+bandpass+"', '"+msName1+".phasediff_inf'])\n\n"

                if doplot == True: 
                    casaCmd = casaCmd + "if applyonly != True: es.checkCalTable('"+msName1+".phase_inf', msName='"+msName1+"', interactive=False) \n"

            else:

                bpassCalId = intentSources['CALIBRATE_BANDPASS']['id']
                if bpassCalId[0] == '': 
                    casalog.post('ERROR: There are no bandpass calibrator.', 'SEVERE')
                    return False
                if len(bpassCalId) != 1: 
                    casaCmd = casaCmd + "# Note: there is more than one bandpass calibrator, I'm picking the first one: "+fieldNames[bpassCalId[0]]+".\n"
                bpassCalId = bpassCalId[0]

                diffGainCalId = ''
                diffGainCalScanList = ''
                if 'CALIBRATE_DIFFGAIN' in list(intentSources.keys()):
                    diffGainCalId = intentSources['CALIBRATE_DIFFGAIN']['id']
                    if len(diffGainCalId) != 1: 
                        casaCmd = casaCmd + "# Note: there is more than one diffgain calibrator, I'm picking the first one: "+fieldNames[diffGainCalId[0]]+".\n"
                    diffGainCalId = diffGainCalId[0]
                if diffGainCalId == '':
                    diffGainCalId = bpassCalId
                else:
                    mymsmd = msmdtool()
                    mymsmd.open(msName)
                    diffGainCalScanList = mymsmd.scansforintent('CALIBRATE_DIFFGAIN#*').tolist()
                    mymsmd.close()
                    diffGainCalScanList = [str(j) for j in diffGainCalScanList]
                    diffGainCalScanList = ','.join(diffGainCalScanList)

                casaCmd = casaCmd + "os.system('rm -rf %s.phasediff_inf') \n"%(msName1)
                casaCmd = casaCmd + "gaincal(vis = '"+msName1+"',\n"
                casaCmd = casaCmd + "  caltable = '"+msName1+".phasediff_inf',\n"
                casaCmd = casaCmd + "  field = '"+str(diffGainCalId)+"',\n"
                if diffGainCalScanList != '': 
                    casaCmd = casaCmd + "  scan = '"+diffGainCalScanList+"',\n"
                casaCmd = casaCmd + "  solint = 'inf',\n"
                casaCmd = casaCmd + "  combine = 'scan',\n"
                casaCmd = casaCmd + "  refant = '"+refant+"',\n"
                if isFullP:
                    casaCmd = casaCmd + "  refantmode = 'strict',\n"
                casaCmd = casaCmd + "  gaintype = 'G',\n"
                casaCmd = casaCmd + "  calmode = 'p',\n"
                casaCmd = casaCmd + "  gaintable = '"+bandpass+"')\n\n"

                phaseDiffCalTableName.append(msName1+'.phasediff_inf')

                if doplot == True: 
                    casaCmd = casaCmd + "if applyonly != True: es.checkCalTable('"+msName1+".phasediff_inf', msName='"+msName1+"', interactive=False) \n\n"

                casaCmd = casaCmd + "os.system('rm -rf %s.phase_int') \n"%(msName1)
                casaCmd = casaCmd + "for i in "+str(calFieldIds)+": # "+calFieldNames+"\n"
                casaCmd = casaCmd + "  gaincal(vis = '"+msName1+"',\n"
                casaCmd = casaCmd + "    caltable = '"+msName1+".phase_int',\n"
                casaCmd = casaCmd + "    field = str(i),\n"
                casaCmd = casaCmd + "    solint = 'int',\n"
                casaCmd = casaCmd + "    combine = 'spw',\n"
                casaCmd = casaCmd + "    refant = '"+refant+"',\n"
                if isFullP:
                    casaCmd = casaCmd + "    refantmode = 'strict',\n"
                casaCmd = casaCmd + "    gaintype = 'G',\n"
                casaCmd = casaCmd + "    calmode = 'p',\n"
                casaCmd = casaCmd + "    append = True,\n"
                casaCmd = casaCmd + "    gaintable = ['"+bandpass+"', '"+msName1+".phasediff_inf'])\n\n"

                if doplot == True: 
                    casaCmd = casaCmd + "if applyonly != True: es.checkCalTable('"+msName1+".phase_int', msName='"+msName1+"', interactive=False) \n\n"

                ###
                mymsmd = msmdtool()
                mymsmd.open(msName)

                spwInfo3 = sfsdr.getSpwInfo(msName, intent='OBSERVE_TARGET|CALIBRATE_BANDPASS', caching=True)
                spwIds3 = sorted(spwInfo3.keys())
                if iHaveSplitMyScienceSpw == True:
                    numSpws = len(spwIds3)
                else:
                    numSpws = mymsmd.nspw()

                calspwmap = {}

                for i in calFieldIds:

                    fieldSpws = mymsmd.spwsforfield(i)

                    if iHaveSplitMyScienceSpw == True:
                        fieldSpws = [spwIds3.index(j) for j in fieldSpws if j in spwIds3]
                    else:
                        fieldSpws = [j for j in fieldSpws if j in spwIds3]

                    calspwmap[i] = [min(fieldSpws)] * numSpws

                mymsmd.close()

                casaCmd = casaCmd + "calspwmap = "+repr(calspwmap)+"\n\n"

                ###

                casaCmd = casaCmd + "os.system('rm -rf %s.ampli_inf') \n"%(msName1)
                casaCmd = casaCmd + "for i in "+str(calFieldIds)+": # "+calFieldNames+"\n"
                casaCmd = casaCmd + "  gaincal(vis = '"+msName1+"',\n"
                casaCmd = casaCmd + "    caltable = '"+msName1+".ampli_inf',\n"
                casaCmd = casaCmd + "    field = str(i),\n"
                casaCmd = casaCmd + "    solint = 'inf',\n"
                casaCmd = casaCmd + "    combine = 'spw',\n"
                casaCmd = casaCmd + "    refant = '"+refant+"',\n"
                if isFullP:
                    casaCmd = casaCmd + "    refantmode = 'strict',\n"
                casaCmd = casaCmd + "    gaintype = '"+gaintypeForAmp+"',\n"
                casaCmd = casaCmd + "    calmode = 'a',\n"
                casaCmd = casaCmd + "    append = True,\n"
                casaCmd = casaCmd + "    gaintable = ['"+bandpass+"', '"+msName1+".phasediff_inf', '"+msName1+".phase_int'],\n"
                casaCmd = casaCmd + "    spwmap = [[], [], calspwmap[i]])\n\n"

                ampForSci.append(msName1+'.ampli_inf')

                if doplot == True: 
                    casaCmd = casaCmd + "if applyonly != True: es.checkCalTable('"+msName1+".ampli_inf', msName='"+msName1+"', interactive=False) \n\n"

                casaCmd = casaCmd + "os.system('rm -rf %s.flux_inf') \n"%(msName1)
                casaCmd = casaCmd + "os.system('rm -rf %s.fluxscale') \n"%(msName1)
                casaCmd = casaCmd + "mylogfile = casalog.logfile()\n"
                casaCmd = casaCmd + "casalog.setlogfile('"+msName1+".fluxscale')\n\n"
                casaCmd = casaCmd + fluxscaleDictName[0] + " = fluxscale(vis = '"+msName1+"',\n"
                casaCmd = casaCmd + "  caltable = '"+msName1+".ampli_inf',\n"
                casaCmd = casaCmd + "  fluxtable = '"+msName1+".flux_inf',\n"
                casaCmd = casaCmd + "  reference = '"+str(bpassCalId)+"', # "+fieldNames[bpassCalId]+"\n"
                casaCmd = casaCmd + "  refspwmap = calspwmap["+str(bpassCalId)+"],\n"
                casaCmd = casaCmd + "  incremental = True)\n\n"
                casaCmd = casaCmd + "casalog.setlogfile(mylogfile)\n\n"
                casaCmd = casaCmd + "if applyonly != True: es.fluxscale2(caltable = '"+msName1+".ampli_inf', removeOutliers=True, msName='"+msName+"', writeToFile=True, preavg=10000)\n\n"

                casaCmd = casaCmd + "os.system('rm -rf %s.phase_inf') \n"%(msName1)
                casaCmd = casaCmd + "for i in "+str(calFieldIds)+": # "+calFieldNames+"\n"
                casaCmd = casaCmd + "  gaincal(vis = '"+msName1+"',\n"
                casaCmd = casaCmd + "    caltable = '"+msName1+".phase_inf',\n"
                casaCmd = casaCmd + "    field = str(i),\n"
                casaCmd = casaCmd + "    solint = 'inf',\n"
                casaCmd = casaCmd + "    combine = 'spw',\n"
                casaCmd = casaCmd + "    refant = '"+refant+"',\n"
                if isFullP:
                    casaCmd = casaCmd + "    refantmode = 'strict',\n"
                casaCmd = casaCmd + "    gaintype = 'G',\n"
                casaCmd = casaCmd + "    calmode = 'p',\n"
                casaCmd = casaCmd + "    append = True,\n"
                casaCmd = casaCmd + "    gaintable = ['"+bandpass+"', '"+msName1+".phasediff_inf'])\n\n"

                if doplot == True: 
                    casaCmd = casaCmd + "if applyonly != True: es.checkCalTable('"+msName1+".phase_inf', msName='"+msName1+"', interactive=False) \n"
        # endif there are scifields

    else: # phaseDiff != True
        fluxCalId = sfsdr.getFieldsForSetjy(msName)

        if len(fluxCalId) == 0:
            casaCmd = casaCmd + "os.system('rm -rf %s.phase_int') \n"%(msName1)
            casaCmd = casaCmd + "gaincal(vis = '"+msName1+"',\n"
            casaCmd = casaCmd + "  caltable = '"+msName1+".phase_int',\n"
            casaCmd = casaCmd + "  field = '"+calFieldIds1+"', # "+calFieldNames+"\n"
            casaCmd = casaCmd + "  solint = 'int',\n"
            casaCmd = casaCmd + "  refant = '"+refant+"',\n"
            if isFullP:
                casaCmd = casaCmd + "  refantmode = 'strict',\n"
            casaCmd = casaCmd + "  gaintype = 'G',\n"
            casaCmd = casaCmd + "  calmode = 'p',\n"
            casaCmd = casaCmd + "  gaintable = '"+bandpass+"')\n\n"

            if doplot == True: 
                casaCmd = casaCmd + "if applyonly != True: es.checkCalTable('"+msName1+".phase_int', msName='"+msName1+"', interactive=False) \n\n"

            intentSources1 = intentSources['CALIBRATE_FLUX']

            if len(intentSources1['sourceid']) != 0:

                sourceFluxes = sfsdr.getFluxesFromSourceTable(msName)

                fluxCalSourceId = intentSources1['sourceid']

                if len(fluxCalSourceId) > 1:
                    casalog.post("THERE IS MORE THAN ONE FLUX CALIBRATOR. I WILL PICK THE FIRST ONE. THIS MAY BE WRONG.", 'WARN')
                    fluxCalSourceId = [j for j in fluxCalSourceId if j in list(sourceFluxes.keys())]

                if len(fluxCalSourceId) == 0: 
                    casalog.post("ERROR: There is no flux calibrator.", 'SEVERE')
                    return False

                fluxCalSourceId = fluxCalSourceId[0]

                fluxCalSourceName = intentSources1['name'][intentSources1['sourceid'].index(fluxCalSourceId)]

                if len(intentSources1['sourceid']) > 1:
                    mytb.open(msName+'/FIELD')
                    tb1 = mytb.query('SOURCE_ID == '+str(fluxCalSourceId))
                    fluxCalFieldIds1 = tb1.rownumbers().tolist()
                    tb1.close()
                    mytb.close()
                    fluxCalFieldIds = [j for j in fluxCalFieldIds1 if j in intentSources1['id']]
                else:
                    fluxCalFieldIds = intentSources1['id']

                if fluxCalSourceId in sourceFluxes:

                    if fluxCalSourceName != sourceFluxes[fluxCalSourceId]['sourceName']: 
                        casalog.post("ERROR: Source names do not match.", 'SEVERE')
                        return False

                    fluxCalId = fluxCalFieldIds[:]

            if len(fluxCalId) != 0:

                if len(fluxCalId) > 1: 
                    casaCmd = casaCmd + "# Note: there is more than one flux calibrator in this dataset, I'm using the first one.\n\n"
                fluxCalId = fluxCalId[0]

                casaCmd = casaCmd + "os.system('rm -rf %s.ampli_inf') \n"%(msName1)
                casaCmd = casaCmd + "gaincal(vis = '"+msName1+"',\n"
                casaCmd = casaCmd + "  caltable = '"+msName1+".ampli_inf',\n"
                casaCmd = casaCmd + "  field = '"+calFieldIds1+"', # "+calFieldNames+"\n"
                casaCmd = casaCmd + "  solint = 'inf',\n"
                casaCmd = casaCmd + "  refant = '"+refant+"',\n"
                if isFullP:
                    casaCmd = casaCmd + "  refantmode = 'strict',\n"
                casaCmd = casaCmd + "  gaintype = '"+gaintypeForAmp+"',\n"
                casaCmd = casaCmd + "  calmode = 'a',\n"
                casaCmd = casaCmd + "  gaintable = ['"+bandpass+"', '"+msName1+".phase_int'])\n\n"

                if doplot == True: 
                    casaCmd = casaCmd + "if applyonly != True: es.checkCalTable('"+msName1+".ampli_inf', msName='"+msName1+"', interactive=False) \n\n"

                casaCmd = casaCmd + "os.system('rm -rf %s.flux_inf') \n"%(msName1)
                casaCmd = casaCmd + "os.system('rm -rf %s.fluxscale') \n"%(msName1)
                casaCmd = casaCmd + "mylogfile = casalog.logfile()\n"
                casaCmd = casaCmd + "casalog.setlogfile('"+msName1+".fluxscale')\n\n"
                casaCmd = casaCmd + fluxscaleDictName[0] + " = fluxscale(vis = '"+msName1+"',\n"
                casaCmd = casaCmd + "  caltable = '"+msName1+".ampli_inf',\n"
                casaCmd = casaCmd + "  fluxtable = '"+msName1+".flux_inf',\n"
                casaCmd = casaCmd + "  reference = '"+str(fluxCalId)+"') # "+fieldNames[fluxCalId]+"\n\n"
                casaCmd = casaCmd + "casalog.setlogfile(mylogfile)\n\n"
                casaCmd = casaCmd + "if applyonly != True: es.fluxscale2(caltable = '"+msName1+".ampli_inf', removeOutliers=True, msName='"+msName+"', writeToFile=True, preavg=10000)\n\n"

            else:

                casaCmd = casaCmd + "os.system('rm -rf %s.flux_inf') \n"%(msName1)
                casaCmd = casaCmd + "gaincal(vis = '"+msName1+"',\n"
                casaCmd = casaCmd + "  caltable = '"+msName1+".flux_inf',\n"
                casaCmd = casaCmd + "  field = '"+calFieldIds1+"', # "+calFieldNames+"\n"
                casaCmd = casaCmd + "  solint = 'inf',\n"
                casaCmd = casaCmd + "  refant = '"+refant+"',\n"
                if isFullP:
                    casaCmd = casaCmd + "  refantmode = 'strict',\n"
                casaCmd = casaCmd + "  gaintype = '"+gaintypeForAmp+"',\n"
                casaCmd = casaCmd + "  calmode = 'a',\n"
                casaCmd = casaCmd + "  gaintable = ['"+bandpass+"', '"+msName1+".phase_int'])\n\n"

                if doplot == True: casaCmd = casaCmd + "if applyonly != True: es.checkCalTable('"+msName1+".flux_inf', msName='"+msName1+"', interactive=False) \n\n"

        else:

            if len(fluxCalId) > 1: 
                casaCmd = casaCmd + "# Note: there is more than one Solar system object in this dataset, I'm using the first one as flux calibrator.\n\n"
            fluxCalId = fluxCalId[0]
            mytb.open(msName+'/ANTENNA')
            antList = mytb.getcol('NAME')
            mytb.close()
            print("Running es.getAntennasForFluxscale2('%s', fluxCalId='%s', refant='%s')" % (msName,str(fluxCalId),refant))
            antList1 = sfsdr.getAntennasForFluxscale2(msName, fluxCalId=str(fluxCalId), refant=refant)

            if len(antList) == len(antList1):

                casaCmd = casaCmd + "os.system('rm -rf %s.phase_int') \n"%(msName1)
                casaCmd = casaCmd + "\ngaincal(vis = '"+msName1+"',\n"
                casaCmd = casaCmd + "  caltable = '"+msName1+".phase_int',\n"
                casaCmd = casaCmd + "  field = '"+calFieldIds1+"', # "+calFieldNames+"\n"
                casaCmd = casaCmd + "  solint = 'int',\n"
                casaCmd = casaCmd + "  refant = '"+refant+"',\n"
                if isFullP:
                    casaCmd = casaCmd + "  refantmode = 'strict',\n"
                casaCmd = casaCmd + "  gaintype = 'G',\n"
                casaCmd = casaCmd + "  calmode = 'p',\n"
                casaCmd = casaCmd + "  gaintable = '"+bandpass+"')\n\n"

                if doplot == True: 
                    casaCmd = casaCmd + "if applyonly != True: es.checkCalTable('"+msName1+".phase_int', msName='"+msName1+"', interactive=False) \n\n"

                casaCmd = casaCmd + "os.system('rm -rf %s.ampli_inf') \n"%(msName1)
                casaCmd = casaCmd + "gaincal(vis = '"+msName1+"',\n"
                casaCmd = casaCmd + "  caltable = '"+msName1+".ampli_inf',\n"
                casaCmd = casaCmd + "  field = '"+calFieldIds1+"', # "+calFieldNames+"\n"
                casaCmd = casaCmd + "  solint = 'inf',\n"
                casaCmd = casaCmd + "  refant = '"+refant+"',\n"
                if isFullP:
                    casaCmd = casaCmd + "  refantmode = 'strict',\n"
                casaCmd = casaCmd + "  gaintype = '"+gaintypeForAmp+"',\n"
                casaCmd = casaCmd + "  calmode = 'a',\n"
                casaCmd = casaCmd + "  gaintable = ['"+bandpass+"', '"+msName1+".phase_int'])\n\n"

                if doplot == True: 
                    casaCmd = casaCmd + "if applyonly != True: es.checkCalTable('"+msName1+".ampli_inf', msName='"+msName1+"', interactive=False) \n\n"

                casaCmd = casaCmd + "os.system('rm -rf %s.flux_inf') \n"%(msName1)
                casaCmd = casaCmd + "os.system('rm -rf %s.fluxscale') \n"%(msName1)
                casaCmd = casaCmd + "mylogfile = casalog.logfile()\n"
                casaCmd = casaCmd + "casalog.setlogfile('"+msName1+".fluxscale')\n\n"
                casaCmd = casaCmd + fluxscaleDictName[0] + " = fluxscale(vis = '"+msName1+"',\n"
                casaCmd = casaCmd + "  caltable = '"+msName1+".ampli_inf',\n"
                casaCmd = casaCmd + "  fluxtable = '"+msName1+".flux_inf',\n"
                casaCmd = casaCmd + "  reference = '"+str(fluxCalId)+"') # "+fieldNames[fluxCalId]+"\n\n"
                casaCmd = casaCmd + "casalog.setlogfile(mylogfile)\n\n"
                casaCmd = casaCmd + "if applyonly != True: es.fluxscale2(caltable = '"+msName1+".ampli_inf', removeOutliers=True, msName='"+msName+"', writeToFile=True, preavg=10000)\n\n"

            else:

                if sciFieldIds[0] != '':
                    phaseCal = sfsdr.getPhaseCal(msName, valueMaps=valueMaps)  # Added by CLB
                    phaseCalNames = []  # Added by CLB
                    phaseCalIds = []
                    for i in phaseCal:  # Added by CLB
                        phaseCalNames.append(phaseCal[i]['phaseCalName']) # Added by CLB
                        phaseCalIds.append(phaseCal[i]['phaseCalId'])

                if len(antList1) < 2:
                    print('WARNING: THE SOLAR SYSTEM OBJECT SEEMS TO BE EXTREMELY RESOLVED')
                    print('WARNING: I COULD NOT FIND A SUBSET OF ANTENNAS ON WHICH TO RUN GAINCAL')
                    print('WARNING: YOU SHOULD LOOK AT THE DATA, AND THEN UPDATE THE SCRIPT')

                casaCmd += "# Note: the Solar system object used for flux calibration is highly resolved on some baselines.\n"
                casaCmd += "# Note: we will first determine the flux of the phase calibrator(s) on a subset of antennas.\n\n"

                casaCmd += "delmod"

                if sciFieldIds[0] != '':
                    casaCmd += "('%s',field='%s')\n\n" % (msName1, ",".join(map(str,list(np.unique(phaseCalIds))))) # Added by CLB
                else:
                    casaCmd += "('%s',field='%s')\n\n" % (msName1, ",".join(map(str,list(np.unique(calFieldIds)))))

                numAntList1 = len(antList1)
                antList1 = ','.join(antList1)

                casaCmd = casaCmd + "os.system('rm -rf %s.phase_short_int') \n"%(msName1)
                casaCmd = casaCmd + "gaincal(vis = '"+msName1+"',\n"
                casaCmd = casaCmd + "  caltable = '"+msName1+".phase_short_int',\n"
                casaCmd = casaCmd + "  field = '"+str(fluxCalId)+"', # "+fieldNames[fluxCalId]+"\n"
                casaCmd = casaCmd + "  selectdata = True,\n"
                casaCmd = casaCmd + "  antenna = '"+str(antList1)+"&',\n"
                casaCmd = casaCmd + "  solint = 'int',\n"
                casaCmd = casaCmd + "  refant = '"+refant+"',\n"
                if isFullP:
                    casaCmd = casaCmd + "  refantmode = 'strict',\n"

                if numAntList1 < 5:
                    casaCmd = casaCmd + "  minblperant = "+str(numAntList1-1)+",\n"
                    casaCmd = casaCmd + "  minsnr = 2.0,\n"

                casaCmd = casaCmd + "  gaintype = 'G',\n"
                casaCmd = casaCmd + "  calmode = 'p',\n"
                casaCmd = casaCmd + "  gaintable = '"+bandpass+"')\n\n"

                ###

                calFieldIds1 = [str(i) for i in calFieldIds if i != fluxCalId]
                calFieldIds1 = ','.join(calFieldIds1)

                calFieldNames = [fieldNames[i] for i in calFieldIds if i != fluxCalId]
                calFieldNames = ','.join(calFieldNames)

                casaCmd = casaCmd + "gaincal(vis = '"+msName1+"',\n"
                casaCmd = casaCmd + "  caltable = '"+msName1+".phase_short_int',\n"
                casaCmd = casaCmd + "  field = '"+calFieldIds1+"', # "+calFieldNames+"\n"
                casaCmd = casaCmd + "  selectdata = True,\n"
                casaCmd = casaCmd + "  solint = 'int',\n"
                casaCmd = casaCmd + "  refant = '"+refant+"',\n"
                if isFullP:
                    casaCmd = casaCmd + "  refantmode = 'strict',\n"

                if numAntList1 < 5:
                    casaCmd = casaCmd + "  minblperant = "+str(numAntList1-1)+",\n"
                    casaCmd = casaCmd + "  minsnr = 2.0,\n"

                casaCmd = casaCmd + "  gaintype = 'G',\n"
                casaCmd = casaCmd + "  calmode = 'p',\n"
                casaCmd = casaCmd + "  append = True,\n"
                casaCmd = casaCmd + "  gaintable = '"+bandpass+"')\n\n"

                ###

                if doplot == True: 
                    casaCmd = casaCmd + "if applyonly != True: es.checkCalTable('"+msName1+".phase_short_int', msName='"+msName1+"', interactive=False) \n\n"

                calFieldIds1 = [str(i) for i in calFieldIds]
                calFieldIds1 = ','.join(calFieldIds1)

                calFieldNames = [fieldNames[i] for i in calFieldIds]
                calFieldNames = ','.join(calFieldNames)

                casaCmd = casaCmd + "os.system('rm -rf %s.ampli_short_inf') \n"%(msName1)
                casaCmd = casaCmd + "gaincal(vis = '"+msName1+"',\n"
                casaCmd = casaCmd + "  caltable = '"+msName1+".ampli_short_inf',\n"
                casaCmd = casaCmd + "  field = '"+calFieldIds1+"', # "+calFieldNames+"\n"
                casaCmd = casaCmd + "  selectdata = True,\n"
                casaCmd = casaCmd + "  solint = 'inf',\n"
                casaCmd = casaCmd + "  refant = '"+refant+"',\n"
                if isFullP:
                    casaCmd = casaCmd + "  refantmode = 'strict',\n"

                if numAntList1 < 5:
                    casaCmd = casaCmd + "  minblperant = "+str(numAntList1-1)+",\n"
                    casaCmd = casaCmd + "  minsnr = 2.0,\n"

                casaCmd = casaCmd + "  gaintype = '"+gaintypeForAmp+"',\n"
                casaCmd = casaCmd + "  calmode = 'a',\n"
                casaCmd = casaCmd + "  gaintable = ['"+bandpass+"', '"+msName1+".phase_short_int'])\n\n"

                if doplot == True: casaCmd = casaCmd + "if applyonly != True: es.checkCalTable('"+msName1+".ampli_short_inf', msName='"+msName1+"', interactive=False) \n\n"

                casaCmd = casaCmd + "os.system('rm -rf %s.flux_short_inf') \n"%(msName1)
                casaCmd = casaCmd + "os.system('rm -rf %s.fluxscale') \n"%(msName1)
                casaCmd = casaCmd + "mylogfile = casalog.logfile()\n"
                casaCmd = casaCmd + "casalog.setlogfile('"+msName1+".fluxscale')\n\n"
                casaCmd = casaCmd + fluxscaleDictName[0] + " = fluxscale(vis = '"+msName1+"',\n"
                casaCmd = casaCmd + "  caltable = '"+msName1+".ampli_short_inf',\n"
                casaCmd = casaCmd + "  fluxtable = '"+msName1+".flux_short_inf',\n"
                casaCmd = casaCmd + "  reference = '"+str(fluxCalId)+"') # "+fieldNames[fluxCalId]+"\n\n"
                casaCmd = casaCmd + "casalog.setlogfile(mylogfile)\n\n"
                casaCmd = casaCmd + "if applyonly != True: es.fluxscale2(caltable = '"+msName1+".ampli_short_inf', removeOutliers=True, msName='"+msName+"', writeToFile=True, preavg=10000)\n\n"

                if sciFieldIds[0] != '':

                    casaCmd = casaCmd + "f = open('"+msName1+".fluxscale')\n"
                    casaCmd = casaCmd + "fc = f.readlines()\n"
                    casaCmd = casaCmd + "f.close()\n\n"

                    phaseCal = sfsdr.getPhaseCal(msName, valueMaps=valueMaps)
                    phaseCalNames = []
                    for i in phaseCal:
                        phaseCalNames.append(phaseCal[i]['phaseCalName'])

                    casaCmd = casaCmd + "for phaseCalName in "+str(list(set(phaseCalNames)))+":\n"
                    casaCmd = casaCmd + "  for i in range(len(fc)):\n"
                    casaCmd = casaCmd + "    if fc[i].find('Flux density for '+phaseCalName) != -1 and re.search('in SpW=[0-9]+(?: \(.*?\))? is: [0-9]+\.[0-9]+', fc[i], re.DOTALL|re.IGNORECASE) is not None:\n"

                    casaCmd = casaCmd + "      line = (re.search('in SpW=[0-9]+(?: \(.*?\))? is: [0-9]+\.[0-9]+', fc[i], re.DOTALL|re.IGNORECASE)).group(0)\n"
                    casaCmd = casaCmd + "      spwId = (line.split('='))[1].split()[0]\n"
                    casaCmd = casaCmd + "      flux = float((line.split(':'))[1].split()[0])\n"
                    casaCmd = casaCmd + "      setjy(vis = '"+msName1+"',\n"
                    casaCmd = casaCmd + "        field = phaseCalName.replace(';','*;').split(';')[0],\n"
                    casaCmd = casaCmd + "        spw = spwId,\n"
                    casaCmd = casaCmd + "        standard = 'manual',\n"
                    casaCmd = casaCmd + "        fluxdensity = [flux,0,0,0])\n\n"


                    casaCmd = casaCmd + "os.system('rm -rf %s.phase_int') \n"%(msName1)
                    casaCmd = casaCmd + "gaincal(vis = '"+msName1+"',\n"
                    casaCmd = casaCmd + "  caltable = '"+msName1+".phase_int',\n"
                    casaCmd = casaCmd + "  field = '"+calFieldIds1+"', # "+calFieldNames+"\n"
                    casaCmd = casaCmd + "  solint = 'int',\n"
                    casaCmd = casaCmd + "  refant = '"+refant+"',\n"
                    if isFullP:
                        casaCmd = casaCmd + "  refantmode = 'strict',\n"
                    casaCmd = casaCmd + "  gaintype = 'G',\n"
                    casaCmd = casaCmd + "  calmode = 'p',\n"
                    casaCmd = casaCmd + "  gaintable = '"+bandpass+"')\n\n"

                    if doplot == True: 
                        casaCmd = casaCmd + "if applyonly != True: es.checkCalTable('"+msName1+".phase_int', msName='"+msName1+"', interactive=False) \n\n"

                    casaCmd = casaCmd + "os.system('rm -rf %s.flux_inf') \n"%(msName1)
                    casaCmd = casaCmd + "gaincal(vis = '"+msName1+"',\n"
                    casaCmd = casaCmd + "  caltable = '"+msName1+".flux_inf',\n"
                    casaCmd = casaCmd + "  field = '"+calFieldIds1+"', # "+calFieldNames+"\n"
                    casaCmd = casaCmd + "  solint = 'inf',\n"
                    casaCmd = casaCmd + "  refant = '"+refant+"',\n"
                    if isFullP:
                        casaCmd = casaCmd + "  refantmode = 'strict',\n"
                    casaCmd = casaCmd + "  gaintype = '"+gaintypeForAmp+"',\n"
                    casaCmd = casaCmd + "  calmode = 'a',\n"
                    casaCmd = casaCmd + "  gaintable = ['"+bandpass+"', '"+msName1+".phase_int'])\n\n"

                    if doplot == True: 
                        casaCmd = casaCmd + "if applyonly != True: es.checkCalTable('"+msName1+".flux_inf', msName='"+msName1+"', interactive=False) \n\n"


        if sciFieldIds[0] != '' and calmode2 == 'ap':
            casaCmd = casaCmd + "os.system('rm -rf %s.phase_inf') \n"%(msName1)
            casaCmd = casaCmd + "gaincal(vis = '"+msName1+"',\n"
            casaCmd = casaCmd + "  caltable = '"+msName1+".phase_inf',\n"
            casaCmd = casaCmd + "  field = '"+calFieldIds1+"', # "+calFieldNames+"\n"
            casaCmd = casaCmd + "  solint = 'inf',\n"
            casaCmd = casaCmd + "  refant = '"+refant+"',\n"
            if isFullP:
                casaCmd = casaCmd + "  refantmode = 'strict',\n"
            casaCmd = casaCmd + "  gaintype = 'G',\n"
            casaCmd = casaCmd + "  calmode = 'p',\n"
            casaCmd = casaCmd + "  gaintable = '"+bandpass+"')\n\n"

            if doplot == True: 
                casaCmd = casaCmd + "if applyonly != True: es.checkCalTable('"+msName1+".phase_inf', msName='"+msName1+"', interactive=False) \n"

    return casaCmd

#####################################
def doB2BGainCalibrationPartI(msName, msName1='', refant='', bandpass='', doplot=True, iHaveSplitMyScienceSpw=False, valueMaps={}):
    """Generate code for the first gain calibration step of a calibration script for B2B data."""

    print('\n*** doB2BGainCalibrationPartI ***')

    if msName1 == '': msName1 = msName
    if refant == '': 
        casalog.post('ERROR: No reference antenna specified.', 'SEVERE')
        return False
    if bandpass == '': 
        casalog.post('ERROR: No bandpass cal table specified.', 'SEVERE')
        return False

    ### determine the field, scan, and spw ids to be used in the code

    if msName in valueMaps.keys():
        vm = valueMaps[msName]
        print('Using canned ValueMap.')
    else:
        vm = aU.ValueMapping(msName)
        valueMaps[msName] = vm

    intentSources = sfsdr.getIntentsAndSourceNames(msName)

    diffGainCalId = intentSources['CALIBRATE_DIFFGAIN']['id'][0]

    spwsDiffgainsig = vm.getSpwsForIntent('CALIBRATE_DIFFGAIN#SIGNAL')
    if spwsDiffgainsig == []:
        spwsDiffgainsig = vm.getSpwsForIntent('CALIBRATE_DIFFGAIN#ON_SOURCE')
        sigintent = 'CALIBRATE_DIFFGAIN#ON_SOURCE'
    else:
        sigintent = 'CALIBRATE_DIFFGAIN#SIGNAL'

    mymsmd = msmdtool()
    mymsmd.open(msName)

    diffGainCalScanListLow  = ','.join([str(i) for i in mymsmd.scansforintent('CALIBRATE_DIFFGAIN#REFERENCE')])
    diffGainCalScanListHigh = ','.join([str(i) for i in mymsmd.scansforintent(sigintent)])

    mymsmd.close()

    spwHigh = sorted(sfsdr.getSpwInfo(msName,intent=sigintent, caching=True).keys())
    spwLow = sorted(sfsdr.getSpwInfo(msName,intent='CALIBRATE_DIFFGAIN#REFERENCE', caching=True).keys())

    if iHaveSplitMyScienceSpw == True:
        spwInfo = sfsdr.getSpwInfo(msName, intent='OBSERVE_TARGET|CALIBRATE_BANDPASS', caching=True)
        spwIds = sorted(spwInfo.keys())

        spwHighSplit = []
        for myspw in spwHigh:
            spwHighSplit.append(spwIds.index(myspw))
        spwHighStr = ','.join([str(i) for i in spwHighSplit])

        spwLowSplit = []
        for myspw in spwLow:
            spwLowSplit.append(spwIds.index(myspw))
        spwLowStr = ','.join([str(i) for i in spwLowSplit])

        spwB2B = list(range(max(spwHighSplit)+1))
        for spwId,spwRep in enumerate(spwHighSplit):
            spwB2B[spwRep] = spwLowSplit[spwId]

    else: # no reindexing took place
        spwHighStr = ','.join([str(i) for i in spwHigh])

        spwB2B = list(range(max(spwHigh)+1))
        for spwId,spwRep in enumerate(spwHigh):
            spwB2B[spwRep] = spwLow[spwId]
    ###

    print('diffGainCalScanListLow ', diffGainCalScanListLow)
    print('diffGainCalScanListHigh ', diffGainCalScanListHigh)
    print('spwHighStr ', spwHighStr)
    print('spwB2B ', str(spwB2B))

    ###

    print('Writing code ...')

    casaCmd = ''

    casaCmd = casaCmd + "# do a phase offset for LF so all SPW can be merged for better SNR\n" 
    casaCmd = casaCmd + "os.system('rm -rf "+msName1+".DGCphaselow_phasediff')\n"
    casaCmd = casaCmd + "gaincal(vis = '"+msName1+"',\n"
    casaCmd = casaCmd + "  caltable = '"+msName1+".DGCphaselow_phasediff',\n"
    casaCmd = casaCmd + "  field = '"+str(diffGainCalId)+"',\n"   
    casaCmd = casaCmd + "  scan = '"+diffGainCalScanListLow+"',\n"
    casaCmd = casaCmd + "  solint = 'inf',\n"
    casaCmd = casaCmd + "  refant = '"+refant+"',\n"
    casaCmd = casaCmd + "  gaintype = 'G',\n"
    casaCmd = casaCmd + "  calmode = 'p',\n"
    casaCmd = casaCmd + "  combine = 'scan',\n"  
    casaCmd = casaCmd + "  minsnr = 1.0,\n"
    casaCmd = casaCmd + "  gaintable = '"+bandpass+"')\n"

    if doplot:
        casaCmd = casaCmd + "\nif applyonly != True: es.checkCalTable('"+msName1+".DGCphaselow_phasediff', msName='"+msName1+"', interactive=False)\n" 


    casaCmd = casaCmd + "\n# LF temporal solutions fast\n"
    casaCmd = casaCmd + "os.system('rm -rf "+msName1+".DGCphaselow_int')\n" 

    casaCmd = casaCmd + "gaincal(vis = '"+msName1+"',\n"
    casaCmd = casaCmd + "  caltable = '"+msName1+".DGCphaselow_int',\n"
    casaCmd = casaCmd + "  field = '"+str(diffGainCalId)+"',\n"
    casaCmd = casaCmd + "  scan = '"+diffGainCalScanListLow+"',\n"
    casaCmd = casaCmd + "  solint = 'int',\n"
    casaCmd = casaCmd + "  refant = '"+refant+"',\n"
    casaCmd = casaCmd + "  gaintype = 'G',\n"
    casaCmd = casaCmd + "  calmode = 'p',\n"
    casaCmd = casaCmd + "  minsnr = 1.0,\n"
    casaCmd = casaCmd + "  #combine = 'spw', ## uncomment if SNR 'too low'\n"
    casaCmd = casaCmd + "  refantmode = 'strict',\n"
    casaCmd = casaCmd + "  interp = ['linear','linear'],\n"
    casaCmd = casaCmd + "  gaintable = ['"+bandpass+"', '"+msName1+".DGCphaselow_phasediff'],\n"
    casaCmd = casaCmd + "  gainfield = ['',''])\n"

    if doplot:
        casaCmd = casaCmd + "\nif applyonly != True: es.checkCalTable('"+msName1+".DGCphaselow_int', msName='"+msName1+"', interactive=False)\n" 

    casaCmd = casaCmd + "\n# LF temporal solutions slow\n"
    casaCmd = casaCmd + "os.system('rm -rf "+msName1+".DGCphaselow_inf')\n"
    casaCmd = casaCmd + "gaincal(vis = '"+msName1+"',\n"
    casaCmd = casaCmd + "  caltable = '"+msName1+".DGCphaselow_inf',\n"
    casaCmd = casaCmd + "  field = '"+str(diffGainCalId)+"',\n"
    casaCmd = casaCmd + "  scan = '"+diffGainCalScanListLow+"',\n"
    casaCmd = casaCmd + "  solint = 'inf',\n"
    casaCmd = casaCmd + "  refant = '"+refant+"',\n"
    casaCmd = casaCmd + "  gaintype = 'G',\n"
    casaCmd = casaCmd + "  calmode = 'p',\n"
    casaCmd = casaCmd + "  minsnr = 1.0,\n"
    casaCmd = casaCmd + "  #combine = 'spw', ## uncomment if SNR is 'too low'\n"
    casaCmd = casaCmd + "  refantmode = 'strict',\n"
    casaCmd = casaCmd + "  interp = ['linear','linear'],\n"
    casaCmd = casaCmd + "  gaintable = ['"+bandpass+"','"+msName1+".DGCphaselow_phasediff'],\n"
    casaCmd = casaCmd + "  gainfield = ['',''])\n"

    if doplot:
        casaCmd = casaCmd + "\nif applyonly != True: es.checkCalTable('"+msName1+".DGCphaselow_inf', msName='"+msName1+"', interactive=False)\n"


    casaCmd = casaCmd + "\n## HF fast solutions\n"

    casaCmd = casaCmd + "os.system('rm -rf "+msName1+".DGCphasehigh_int')\n"

    casaCmd = casaCmd + "gaincal(vis = '"+msName1+"',\n"
    casaCmd = casaCmd + "  caltable = '"+msName1+".DGCphasehigh_int',\n"
    casaCmd = casaCmd + "  field = '"+str(diffGainCalId)+"',\n"
    casaCmd = casaCmd + "  scan= '"+diffGainCalScanListHigh+"',\n"  
    casaCmd = casaCmd + "  solint = 'int',\n"
    casaCmd = casaCmd + "  refant = '"+refant+"',\n"
    casaCmd = casaCmd + "  gaintype = 'G',\n"
    casaCmd = casaCmd + "  calmode = 'p',\n"
    casaCmd = casaCmd + "  minsnr = 1.0,\n"
    casaCmd = casaCmd + "  #combine = 'spw',  ## uncomment if SNR is 'too low'\n" 
    casaCmd = casaCmd + "  refantmode = 'strict',\n"
    casaCmd = casaCmd + "  interp = ['linear','linear'],\n"
    casaCmd = casaCmd + "  gaintable = '"+bandpass+"',\n"
    casaCmd = casaCmd + "  gainfield = [''])\n"

    if doplot:
        casaCmd = casaCmd + "\nif applyonly != True: es.checkCalTable('"+msName1+".DGCphasehigh_int', msName='"+msName1+"', interactive=False)\n" 


    casaCmd = casaCmd + "\n# get the DGC band-offset\n"
    casaCmd = casaCmd + "# apply the LF to the HF and find the offset for each DGC 'group'\n"
    casaCmd = casaCmd + "# if there are multiple DGC sequences - this does each one independently (hence 'multi')\n"

    casaCmd = casaCmd + "os.system('rm -rf "+msName1+".DGCoffset_multi')\n"
    casaCmd = casaCmd + "gaincal(vis = '"+msName1+"',\n"
    casaCmd = casaCmd + "  caltable = '"+msName1+".DGCoffset_multi',\n"
    casaCmd = casaCmd + "  field = '"+str(diffGainCalId)+"',\n"
    casaCmd = casaCmd + "  scan = '"+diffGainCalScanListHigh+"',\n"  
    casaCmd = casaCmd + "  spw = '"+spwHighStr+"',\n"
    casaCmd = casaCmd + "  solint = '8min',  # max. duration of one DGC visit block\n"
    casaCmd = casaCmd + "  refant = '"+refant+"',\n"
    casaCmd = casaCmd + "  gaintype = 'G',\n"
    casaCmd = casaCmd + "  calmode = 'p',\n"
    casaCmd = casaCmd + "  minsnr=1.0,\n"
    casaCmd = casaCmd + "  combine='scan',\n"
    casaCmd = casaCmd + "  interp=['linear','linearPD'],\n"
    casaCmd = casaCmd + "  spwmap=[[],"+str(spwB2B)+"],\n"
    casaCmd = casaCmd + "  gainfield = ['',''],\n"  
    casaCmd = casaCmd + "  gaintable = ['"+bandpass+"','"+msName1+".DGCphaselow_inf'])\n"

    if doplot:
        casaCmd = casaCmd + "\nif applyonly != True: es.checkCalTable('"+msName1+".DGCoffset_multi', msName='"+msName1+"', interactive=False)\n"

        casaCmd = casaCmd + "\nif applyonly != True:\n"
        casaCmd = casaCmd + "  # QA2 check to verify the slow (inf) LF to HF and offset worked\n"
        casaCmd = casaCmd + "  # apply the LF to the HF and the multi offset\n"
        casaCmd = casaCmd + "  os.system('rm -rf "+msName1+".DGCresidual_offset_multi')\n"
        casaCmd = casaCmd + "  gaincal(vis = '"+msName1+"',\n"
        casaCmd = casaCmd + "    caltable = '"+msName1+".DGCresidual_offset_multi',\n"
        casaCmd = casaCmd + "    field = '"+str(diffGainCalId)+"',\n"
        casaCmd = casaCmd + "    scan = '"+diffGainCalScanListHigh+"',\n"
        casaCmd = casaCmd + "    spw = '"+spwHighStr+"',\n"
        casaCmd = casaCmd + "    solint = 'inf',\n"
        casaCmd = casaCmd + "    refant = '"+refant+"',\n"
        casaCmd = casaCmd + "    gaintype = 'G',\n"
        casaCmd = casaCmd + "    calmode = 'p',\n"
        casaCmd = casaCmd + "    minsnr=1.0,\n"
        casaCmd = casaCmd + "    interp=['linear','linearPD','linear'],\n"
        casaCmd = casaCmd + "    spwmap=[[],"+str(spwB2B)+",[]],\n"
        casaCmd = casaCmd + "    gainfield = ['','',''],\n"
        casaCmd = casaCmd + "    gaintable = ['"+bandpass+"','"+msName1+".DGCphaselow_inf','"+msName1+".DGCoffset_multi'])\n"

        casaCmd = casaCmd + "\nif applyonly != True: es.checkCalTable('"+msName1+".DGCresidual_offset_multi', msName='"+msName1+"', interactive=False)\n"

        casaCmd = casaCmd + "\nif applyonly != True:\n"
        casaCmd = casaCmd + "  # QA2 check to verify the fast (int) LF to HF and offset worked\n"
        casaCmd = casaCmd + "  os.system('rm -rf "+msName1+".DGCresidual_offset_multi_int')\n" 
        casaCmd = casaCmd + "  gaincal(vis = '"+msName1+"',\n"
        casaCmd = casaCmd + "    caltable = '"+msName1+".DGCresidual_offset_multi_int',\n"
        casaCmd = casaCmd + "    field = '"+str(diffGainCalId)+"',\n"
        casaCmd = casaCmd + "    scan = '"+diffGainCalScanListHigh+"',\n"
        casaCmd = casaCmd + "    spw = '"+spwHighStr+"',\n"
        casaCmd = casaCmd + "    solint = 'int',\n"
        casaCmd = casaCmd + "    refant = '"+refant+"',\n"
        casaCmd = casaCmd + "    gaintype = 'G',\n"
        casaCmd = casaCmd + "    calmode = 'p',\n"
        casaCmd = casaCmd + "    minsnr=1.0,\n"
        casaCmd = casaCmd + "    interp=['linear','linearPD','linear'],\n"
        casaCmd = casaCmd + "    spwmap=[[],"+str(spwB2B)+",[]],\n"
        casaCmd = casaCmd + "    gainfield = ['','',''],\n"
        casaCmd = casaCmd + "    gaintable = ['"+bandpass+"','"+msName1+".DGCphaselow_inf','"+msName1+".DGCoffset_multi'])\n"

        casaCmd = casaCmd + "\nif applyonly != True: es.checkCalTable('"+msName1+".DGCresidual_offset_multi_int', msName='"+msName1+"', interactive=False)\n"
    #endif doplot


    return casaCmd

#####################################
def doB2BGainCalibrationPartII(msName, msName1='', refant='', bandpass='', doplot=True, ampForSci=[], iHaveSplitMyScienceSpw=False, valueMaps={}):
    """Generate code for the second gain calibration step of a calibration script for B2B data."""

    print('\n*** doB2BGainCalibrationPartII ***')

    if msName1 == '': msName1 = msName
    if refant == '': 
        casalog.post('ERROR: No reference antenna specified.', 'SEVERE')
        return False
    if bandpass == '': 
        casalog.post('ERROR: No bandpass cal table specified.', 'SEVERE')
        return False

    ### determine the scan lists and the field and spw ids to be used in the code

    if msName in valueMaps.keys():
        vm = valueMaps[msName]
        print('Using canned ValueMap.')
    else:
        vm = aU.ValueMapping(msName)
        valueMaps[msName] = vm

    intentSources = sfsdr.getIntentsAndSourceNames(msName)

    diffGainCalId = intentSources['CALIBRATE_DIFFGAIN']['id'][0]
    phaseCalId  = intentSources['CALIBRATE_PHASE']['id'][0]
    i = 0
    while phaseCalId == diffGainCalId and i < len(intentSources['CALIBRATE_PHASE']['id'])-1:
        i += 1
        phaseCalId  = intentSources['CALIBRATE_PHASE']['id'][i]

    fluxCalId  = intentSources['CALIBRATE_FLUX']['id'][0]
    bpCalId  = intentSources['CALIBRATE_BANDPASS']['id'][0]

    fieldCal = set((diffGainCalId, phaseCalId))
    fieldCalStr = ','.join([str(i) for i in fieldCal])
    fieldCal2 = set((fluxCalId, bpCalId, diffGainCalId, phaseCalId))
    fieldCal2Str = ','.join([str(i) for i in fieldCal2])


    spwsDiffgainsig = vm.getSpwsForIntent('CALIBRATE_DIFFGAIN#SIGNAL')
    if spwsDiffgainsig == []:
        spwsDiffgainsig = vm.getSpwsForIntent('CALIBRATE_DIFFGAIN#ON_SOURCE')
        sigintent = 'CALIBRATE_DIFFGAIN#ON_SOURCE'
    else:
        sigintent = 'CALIBRATE_DIFFGAIN#SIGNAL'

    mymsmd = msmdtool()
    mymsmd.open(msName)

    fluxCalScanList = ','.join([str(i) for i in mymsmd.scansforintent('CALIBRATE_FLUX#ON_SOURCE')])

    mymsmd.close()

    spwHigh = sorted(sfsdr.getSpwInfo(msName,intent=sigintent, caching=True).keys())
    spwLow = sorted(sfsdr.getSpwInfo(msName,intent='CALIBRATE_DIFFGAIN#REFERENCE', caching=True).keys())

    if iHaveSplitMyScienceSpw == True:
        spwInfo = sfsdr.getSpwInfo(msName, intent='OBSERVE_TARGET|CALIBRATE_BANDPASS', caching=True)
        spwIds = sorted(spwInfo.keys())

        spwHighSplit = []
        for myspw in spwHigh:
            spwHighSplit.append(spwIds.index(myspw))
        spwHighStr = ','.join([str(i) for i in spwHighSplit])

        spwLowSplit = []
        for myspw in spwLow:
            spwLowSplit.append(spwIds.index(myspw))
        spwLowStr = ','.join([str(i) for i in spwLowSplit])

    else: # no reindexing took place
        spwHighStr = ','.join([str(i) for i in spwHigh])
        spwLowStr = ','.join([str(i) for i in spwLow])



    ###

    print('fluxCalScanList ', fluxCalScanList)
    print('spwHighStr ', spwHighStr)
    print('spwLowStr ', spwLowStr)

    ###

    print('Writing code ...')

    casaCmd = ''

    casaCmd = casaCmd + "if applyonly != True:" 
    casaCmd = casaCmd + "  # LF fast phase as a QA2 check for stability scan drop-outs\n"
    casaCmd = casaCmd + "  os.system('rm -rf "+msName1+".phaselow_int')\n" 
    casaCmd = casaCmd + "  gaincal(vis = '"+msName1+"',\n"
    casaCmd = casaCmd + "    caltable = '"+msName1+".phaselow_int',\n" 
    casaCmd = casaCmd + "    field = '"+fieldCalStr+"',\n"
    casaCmd = casaCmd + "    spw = '"+spwLowStr+"',\n"
    casaCmd = casaCmd + "    solint = 'int',\n"
    casaCmd = casaCmd + "    refant = '"+refant+"',\n"
    casaCmd = casaCmd + "    refantmode = 'strict',\n"
    casaCmd = casaCmd + "    #combine='spw', # uncomment for low SNR on phase cal\n"
    casaCmd = casaCmd + "    gaintype = 'G',\n"
    casaCmd = casaCmd + "    calmode = 'p',\n"
    casaCmd = casaCmd + "    minsnr = 1.0,\n"
    casaCmd = casaCmd + "    gaintable = ['"+bandpass+"','"+msName1+".DGCphaselow_phasediff'])\n"

    casaCmd = casaCmd + "\nif applyonly != True: es.checkCalTable('"+msName1+".phaselow_int', msName='"+msName1+"', interactive=False)\n" 

    casaCmd = casaCmd + "\n# LF slow phase solution\n"
    casaCmd = casaCmd + "os.system('rm -rf "+msName1+".phaselow_inf')\n" 
    casaCmd = casaCmd + "gaincal(vis = '"+msName1+"',\n"
    casaCmd = casaCmd + "  caltable = '"+msName1+".phaselow_inf',\n"
    casaCmd = casaCmd + "  field = '"+fieldCalStr+"',\n"    
    casaCmd = casaCmd + "  spw = '"+spwLowStr+"',\n"
    casaCmd = casaCmd + "  solint = 'inf',\n"  
    casaCmd = casaCmd + "  refant = '"+refant+"',\n"
    casaCmd = casaCmd + "  refantmode='strict',\n"
    casaCmd = casaCmd + "  #combine='spw', # uncomment for low SNR on phase cal\n"
    casaCmd = casaCmd + "  gaintype = 'G',\n"
    casaCmd = casaCmd + "  calmode = 'p',\n"
    casaCmd = casaCmd + "  minsnr = 1.0,\n"
    casaCmd = casaCmd + "  gaintable = ['"+bandpass+"','"+msName1+".DGCphaselow_phasediff'])\n"
 
    casaCmd = casaCmd + "\nif applyonly != True: es.checkCalTable('"+msName1+".phaselow_inf', msName='"+msName1+"', interactive=False)\n"

    casaCmd = casaCmd + "\n## HF phase up on all strong sources, i.e. the BP, flux cal, and DGC for amp gain table\n"
    casaCmd = casaCmd + "os.system('rm -rf "+msName1+".phasehigh_int')\n" 
    casaCmd = casaCmd + "gaincal(vis = '"+msName1+"',\n"
    casaCmd = casaCmd + "  caltable = '"+msName1+".phasehigh_int',\n" 
    casaCmd = casaCmd + "  field = '"+fieldCal2Str+"',\n"    
    casaCmd = casaCmd + "  spw = '"+spwHighStr+"',\n"
    casaCmd = casaCmd + "  solint = 'int',\n"  
    casaCmd = casaCmd + "  refant = '"+refant+"',\n"
    casaCmd = casaCmd + "  refantmode = 'strict',\n"
    casaCmd = casaCmd + "  gaintype = 'G',\n"
    casaCmd = casaCmd + "  calmode = 'p',\n"
    casaCmd = casaCmd + "  minsnr = 1.0,\n"
    casaCmd = casaCmd + "  gaintable = '"+bandpass+"')\n"
        
    casaCmd = casaCmd + "\nif applyonly != True: es.checkCalTable('"+msName1+".phasehigh_int', msName='"+msName1+"', interactive=False)\n" 

    casaCmd = casaCmd + "\n# Amplitude calibration\n"
    casaCmd = casaCmd + "os.system('rm -rf "+msName1+".ampli_inf')\n" 
    casaCmd = casaCmd + "gaincal(vis = '"+msName1+"',\n"
    casaCmd = casaCmd + "  caltable = '"+msName1+".ampli_inf',\n"  
    casaCmd = casaCmd + "  field = '"+str(fluxCalId)+"',\n"
    casaCmd = casaCmd + "  scan = '"+fluxCalScanList+"',\n" # will only pick the HF should only use the scan of the 'fluxcal' if same as DGC source? TBD
    casaCmd = casaCmd + "  spw = '"+spwHighStr+"',\n"
    casaCmd = casaCmd + "  solint = 'inf',\n"  
    casaCmd = casaCmd + "  combine = 'scan',\n"  
    casaCmd = casaCmd + "  refant = '"+refant+"',\n" 
    casaCmd = casaCmd + "  gaintype = 'T',\n"
    casaCmd = casaCmd + "  calmode = 'a',\n"
    casaCmd = casaCmd + "  minsnr = 1.0,\n"
    casaCmd = casaCmd + "  gaintable = ['"+bandpass+"','"+msName1+".phasehigh_int'],\n"
    casaCmd = casaCmd + "  gainfield = ['"+str(bpCalId)+"',''])\n"

    casaCmd = casaCmd + "\nif applyonly != True: es.checkCalTable('"+msName1+".ampli_inf', msName='"+msName1+"', interactive=False)\n" 


    return casaCmd

#####################################

def doApplyBandpassAndGainCalTables(msName, msName1='', iHaveSplitMyScienceSpw=False, bandpass='', phaseForCal='', phaseForSci='', flux='', phaseDiffCalTableName='', ampForSci='', useForLoop=True, phaseDiffPerSpwSetup=False, valueMaps={}):
    """Generate code for the final applycal step for the given MS (standard cal setup and BWSW case)"""

    print('\n*** doApplyBandpassAndGainCalTables ***')

    casaCmd = ''

    if bandpass == '' or phaseForCal == '' or phaseForSci == '' or flux == '': sys.exit('ERROR: Missing table(s).')
    if msName1 == '': msName1 = msName
    mytb = aU.createCasaTool(tbtool)
    mytb.open(msName+'/FIELD')
    fieldNames = mytb.getcol('NAME')
    mytb.close()

    fieldIds = list(range(len(fieldNames)))

    intentSources = sfsdr.getIntentsAndSourceNames(msName)
    sciFieldIds = intentSources['OBSERVE_TARGET']['id']
    if sciFieldIds[0] == '': sys.exit('ERROR: There seems to be no science field.')

    checkFieldIds = intentSources['OBSERVE_CHECK']['id']
    if checkFieldIds[0] != '':
        sciFieldIds += intentSources['OBSERVE_CHECK']['id']

    calFieldIds = [i for i in fieldIds if i not in sciFieldIds]
    if len(calFieldIds) == '': sys.exit('ERROR: There seems to be no calibrator field.')

    mymsmd = msmdtool()
    mymsmd.open(msName)
    hasdata = []
    for i in calFieldIds:
        calFieldIntents = mymsmd.intentsforfield(i)
        hasdata1 = 0
        for j in calFieldIntents:
            if re.search('^CALIBRATE_(POINTING|ATMOSPHERE|WVR)', j) == None:
                hasdata1 = 1
                break
        hasdata.append(hasdata1)
    mymsmd.close()

    calFieldIds = [calFieldIds[i] for i in range(len(calFieldIds)) if hasdata[i] == 1]

    if len(phaseDiffCalTableName) != 0:

        if phaseDiffPerSpwSetup == True:
            mymsmd = msmdtool()
            mymsmd.open(msName)

            spwIds3 = mymsmd.spwsforintent('CALIBRATE_BANDPASS*').tolist()+mymsmd.spwsforintent('OBSERVE_TARGET*').tolist()
            spwIds3 = np.unique([j for j in spwIds3 if j not in mymsmd.chanavgspws() and j not in mymsmd.wvrspws()]).tolist()

            spwSetups = {}
            for i in spwIds3:
                scanList3 = str(mymsmd.scansforspw(i).tolist())
                if scanList3 not in list(spwSetups.keys()): spwSetups[scanList3] = []
                spwSetups[scanList3].append(i)
            spwSetups1 = []
            for i in list(spwSetups.values()):
                if i not in spwSetups1: spwSetups1.append(i)
            spwSetups1.sort(key=lambda x:x[0])

            calspwmap = []
            for i in range(len(spwSetups1)):
                for j in range(len(spwSetups1[i])):
                    calspwmap.append(min(spwSetups1[i]))

            if iHaveSplitMyScienceSpw == True: ###HERE

                for i in range(len(spwSetups1)):
                    for j in range(len(spwSetups1[i])):
                        spwSetups1[i][j] = spwIds3.index(spwSetups1[i][j])

                for i in range(len(calspwmap)):
                    calspwmap[i] = spwIds3.index(calspwmap[i])

            casaCmd = casaCmd + "calspwmap = "+str(calspwmap)+"\n\n"

            mymsmd.close()

        else:
            mymsmd = msmdtool()
            mymsmd.open(msName)

            spwInfo3 = sfsdr.getSpwInfo(msName, intent='OBSERVE_TARGET|CALIBRATE_BANDPASS', caching=True)
            spwIds3 = sorted(spwInfo3.keys())
            if iHaveSplitMyScienceSpw == True:
                numSpws = len(spwIds3)
            else:
                numSpws = mymsmd.nspw()

            calspwmap = {}

            for i in calFieldIds:

                fieldSpws = mymsmd.spwsforfield(i)

                if iHaveSplitMyScienceSpw == True:
                    fieldSpws = [spwIds3.index(j) for j in fieldSpws if j in spwIds3]
                else:
                    fieldSpws = [j for j in fieldSpws if j in spwIds3]

                calspwmap[i] = [min(fieldSpws)] * numSpws

            mymsmd.close()

            casaCmd = casaCmd + "calspwmap = "+repr(calspwmap)+"\n\n"

    phaseCal = sfsdr.getPhaseCal(msName, valueMaps=valueMaps)
    phaseCalFieldIds = []
    for j in phaseCal:
        phaseCalFieldIds.append(phaseCal[j]['phaseCalId'])
    phaseCalFieldIds = sorted(dict.fromkeys(phaseCalFieldIds).keys())
    calFieldIds = [i for i in calFieldIds if i not in phaseCalFieldIds]
    calFieldIds1 = [str(i) for i in calFieldIds]

    calFieldNames = [fieldNames[i] for i in calFieldIds]
    calFieldNames = ','.join(calFieldNames)

    gainTable = []
    gainTable.append(bandpass)

    if len(phaseDiffCalTableName) == 0:

        gainTable.append(phaseForCal)
        gainTable.append(flux)

        if useForLoop == True:
            casaCmd = casaCmd + "for i in "+str(calFieldIds1)+": # "+calFieldNames+"\n"
            casaCmd = casaCmd + "  applycal(vis = '"+msName1+"',\n"
            casaCmd = casaCmd + "    field = str(i),\n"
            casaCmd = casaCmd + "    gaintable = "+str(gainTable)+",\n"
            casaCmd = casaCmd + "    gainfield = ['', i, i],\n"
            if re.search('^3.3', aU.getCasaVersion()) == None:
                casaCmd = casaCmd + "    interp = 'linear,linear',\n"
            else:
                casaCmd = casaCmd + "    interp = 'linear',\n"

            casaCmd = casaCmd + "    calwt = True,\n"
            casaCmd = casaCmd + "    flagbackup = False)\n"
        else:
            for i in calFieldIds:
                casaCmd = casaCmd + "applycal(vis = '"+msName1+"',\n"
                casaCmd = casaCmd + "  field = '"+str(i)+"', # "+fieldNames[i]+"\n"
                casaCmd = casaCmd + "  gaintable = "+str(gainTable)+",\n"
                casaCmd = casaCmd + "  gainfield = ['', '"+str(i)+"', '"+str(i)+"'],\n"
                if re.search('^3.3', aU.getCasaVersion()) == None:
                    casaCmd = casaCmd + "  interp = 'linear,linear',\n"
                else:
                    casaCmd = casaCmd + "  interp = 'linear',\n"
            
                casaCmd = casaCmd + "  calwt = True,\n"
                casaCmd = casaCmd + "  flagbackup = False)\n\n"

    else:

            gainTable.append(phaseDiffCalTableName[0])
            gainTable.append(phaseForCal)

            bpassCalId = intentSources['CALIBRATE_BANDPASS']['id']

            if bpassCalId[0] != '':

                if len(bpassCalId) != 1: casaCmd = casaCmd + "# Note: there is more than one bandpass calibrator, I'm picking the first one: "+fieldNames[bpassCalId[0]]+".\n"
                bpassCalId = bpassCalId[0]

            else:

                casaCmd = casaCmd + "# Note: there are no bandpass calibrator, I'm picking a phase calibrator.\n"
                phaseCalId = intentSources['CALIBRATE_PHASE']['id']
                ampCalId = intentSources['CALIBRATE_AMPLI']['id'] + intentSources['CALIBRATE_FLUX']['id']
                ampCalId = [i for i in ampCalId if i != '']
                phaseOnlyCalId = [i for i in phaseCalId if i not in ampCalId]
                if len(phaseOnlyCalId) != 1: casaCmd = casaCmd + "# Note: there is more than one phase calibrator, I'm picking the first one: "+fieldNames[phaseOnlyCalId[0]]+".\n"
                bpassCalId = phaseOnlyCalId[0]

            casaCmd = casaCmd + "applycal(vis = '"+msName1+"',\n"
            casaCmd = casaCmd + "  field = '"+str(bpassCalId)+"', # "+fieldNames[bpassCalId]+"\n"
            casaCmd = casaCmd + "  gaintable = "+str(gainTable)+",\n"
            casaCmd = casaCmd + "  gainfield = ['"+str(bpassCalId)+"', '', '"+str(bpassCalId)+"'],\n"
            casaCmd = casaCmd + "  interp = [],\n"

            if phaseDiffPerSpwSetup == True:
                casaCmd = casaCmd + "  spwmap = [[], [], calspwmap],\n"
            else:
                casaCmd = casaCmd + "  spwmap = [[], [], calspwmap["+str(bpassCalId)+"]],\n"

            casaCmd = casaCmd + "  calwt = True,\n"
            casaCmd = casaCmd + "  flagbackup = False)\n\n"

            gainTable.append(ampForSci[0])

            for i in calFieldIds:

                if i == bpassCalId: continue

                casaCmd = casaCmd + "applycal(vis = '"+msName1+"',\n"
                casaCmd = casaCmd + "  field = '"+str(i)+"', # "+fieldNames[i]+"\n"
                casaCmd = casaCmd + "  gaintable = "+str(gainTable)+",\n"
                casaCmd = casaCmd + "  gainfield = ['', '', '"+str(i)+"', '"+str(i)+"'],\n"
                casaCmd = casaCmd + "  interp = ['', 'nearest', 'linearPD', ''],\n"

                if phaseDiffPerSpwSetup == True:
                    casaCmd = casaCmd + "  spwmap = [[], [], calspwmap, calspwmap],\n"
                else:
                    casaCmd = casaCmd + "  spwmap = [[], [], calspwmap["+str(i)+"], calspwmap["+str(i)+"]],\n"

                casaCmd = casaCmd + "  calwt = True,\n"
                casaCmd = casaCmd + "  flagbackup = False)\n\n"

    gainTable = []
    gainTable.append(bandpass)

    if re.search('^3.3', aU.getCasaVersion()) == None:
        gainTableInterp = "'linear,linear'"
    else:
        gainTableInterp = "'linear'"

    if len(phaseDiffCalTableName) != 0:

        if len(ampForSci) != 1: sys.exit('ERROR: missing table')

        gainTable.append(phaseDiffCalTableName[0])
        gainTable.append(phaseForSci)
        gainTable.append(ampForSci[0])
        gainTable.append(flux)

        gainTableInterp = "['', 'nearest', 'linearPD', '', '']"

    else:

        gainTable.append(phaseForSci)
        gainTable.append(flux)

    for i in phaseCalFieldIds:

        sciFieldIds = []
        sciFieldNames = []
        for j in phaseCal:
            if phaseCal[j]['phaseCalId'] == i:
                sciFieldIds1 = phaseCal[j]['sciFieldIds']
                for k in sciFieldIds1: sciFieldIds.append(k)
                sciFieldNames.append(j)
        sciFieldIds = sorted(sciFieldIds)
        sciFieldNames = ','.join(sciFieldNames)

        if len(sciFieldIds) > 1:
            j0 = 0
            sciFieldIds1 = str(sciFieldIds[j0])
            for j in range(len(sciFieldIds)-1):
                if sciFieldIds[j+1] == sciFieldIds[j]+1: continue
                sciFieldIds1 = sciFieldIds1 + '~' + str(sciFieldIds[j])
                j0 = j+1
                sciFieldIds1 = sciFieldIds1 + ',' + str(sciFieldIds[j0])
            sciFieldIds1 = sciFieldIds1 + '~' + str(sciFieldIds[j+1])
        else:
            sciFieldIds1 = str(sciFieldIds[0])

        if phaseDiffPerSpwSetup == True:
            mymsmd = msmdtool()
            mymsmd.open(msName)

            spwIds4 = mymsmd.spwsforfield(i).tolist()
            spwIds4 = [j for j in spwIds4 if j not in mymsmd.chanavgspws() and j in spwIds3]

            calspwmap = []
            for k in range(len(spwSetups1)):
                for j in range(len(spwSetups1[k])):
                    calspwmap.append(min(spwIds4))

            if iHaveSplitMyScienceSpw == True: ###HERE

                for k in range(len(calspwmap)):
                    calspwmap[k] = spwIds3.index(calspwmap[k])

            casaCmd = casaCmd + "calspwmap = "+str(calspwmap)+"\n\n"

            mymsmd.close()

        casaCmd = casaCmd + "\napplycal(vis = '"+msName1+"',\n"
        casaCmd = casaCmd + "  field = '"+str(i)+","+sciFieldIds1+"', # "+sciFieldNames+"\n"
        casaCmd = casaCmd + "  gaintable = "+str(gainTable)+",\n"
        if len(phaseDiffCalTableName) != 0:
            casaCmd = casaCmd + "  gainfield = ['', '', '"+str(i)+"', '"+str(i)+"', '"+str(i)+"'], # "+fieldNames[i]+"\n"
        else:
            casaCmd = casaCmd + "  gainfield = ['', '"+str(i)+"', '"+str(i)+"'], # "+fieldNames[i]+"\n"
        casaCmd = casaCmd + "  interp = "+gainTableInterp+",\n"
        if len(phaseDiffCalTableName) != 0:
            if phaseDiffPerSpwSetup == True:
                casaCmd = casaCmd + "  spwmap = [[], [], calspwmap, calspwmap, calspwmap],\n"
            else:
                casaCmd = casaCmd + "  spwmap = [[], [], calspwmap["+str(i)+"], calspwmap["+str(i)+"], calspwmap["+str(i)+"]],\n"

        casaCmd = casaCmd + "  calwt = True,\n"
        casaCmd = casaCmd + "  flagbackup = False)\n"

    return casaCmd

#####################################
def doApplyB2BBandpassAndGainCalTables(msName, msName1='', iHaveSplitMyScienceSpw=False, bandpass='', valueMaps={}):
    """Generate code for the final applycal step for the given MS (B2B case)"""

    print('\n*** doApplyB2BBandpassAndGainCalTables ***')

    if msName1 == '': msName1 = msName

    if bandpass == '': 
        casalog.post('ERROR: No bandpass cal table specified.', 'SEVERE')
        return False

    ### determine the scan lists and the field and spw ids to be used in the code

    if msName in valueMaps.keys():
        vm = valueMaps[msName]
        print('Using canned ValueMap.')
    else:
        vm = aU.ValueMapping(msName)
        valueMaps[msName] = vm

    intentSources = sfsdr.getIntentsAndSourceNames(msName)

    targetIds = intentSources['OBSERVE_TARGET']['id']
    targetIdStr = ','.join([str(i) for i in targetIds])
    checkSourceId  = intentSources['OBSERVE_CHECK']['id'][0]
    diffGainCalId = intentSources['CALIBRATE_DIFFGAIN']['id'][0]
    phaseCalId  = intentSources['CALIBRATE_PHASE']['id'][0]
    i = 0
    while phaseCalId == diffGainCalId and i < len(intentSources['CALIBRATE_PHASE']['id'])-1:
        i += 1
        phaseCalId  = intentSources['CALIBRATE_PHASE']['id'][i]

    fluxCalId  = intentSources['CALIBRATE_FLUX']['id'][0]
    bpCalId  = intentSources['CALIBRATE_BANDPASS']['id'][0]

    spwsDiffgainsig = vm.getSpwsForIntent('CALIBRATE_DIFFGAIN#SIGNAL')
    if spwsDiffgainsig == []:
        spwsDiffgainsig = vm.getSpwsForIntent('CALIBRATE_DIFFGAIN#ON_SOURCE')
        sigintent = 'CALIBRATE_DIFFGAIN#ON_SOURCE'
    else:
        sigintent = 'CALIBRATE_DIFFGAIN#SIGNAL'

    mymsmd = msmdtool()
    mymsmd.open(msName)

    bpCalScanList = ','.join([str(i) for i in mymsmd.scansforintent('CALIBRATE_BANDPASS#ON_SOURCE')])
    fluxCalScanList = ','.join([str(i) for i in mymsmd.scansforintent('CALIBRATE_FLUX#ON_SOURCE')])
    diffGainCalScanListLow  = ','.join([str(i) for i in mymsmd.scansforintent('CALIBRATE_DIFFGAIN#REFERENCE')])
    diffGainCalScanListHigh = ','.join([str(i) for i in mymsmd.scansforintent(sigintent)])

    mymsmd.close()

    spwHigh = sorted(sfsdr.getSpwInfo(msName,intent=sigintent, caching=True).keys())
    spwLow = sorted(sfsdr.getSpwInfo(msName,intent='CALIBRATE_DIFFGAIN#REFERENCE', caching=True).keys())

    if iHaveSplitMyScienceSpw == True:
        spwInfo = sfsdr.getSpwInfo(msName, intent='OBSERVE_TARGET|CALIBRATE_BANDPASS', caching=True)
        spwIds = sorted(spwInfo.keys())

        spwHighSplit = []
        for myspw in spwHigh:
            spwHighSplit.append(spwIds.index(myspw))
        spwHighStr = ','.join([str(i) for i in spwHighSplit])

        spwLowSplit = []
        for myspw in spwLow:
            spwLowSplit.append(spwIds.index(myspw))
        spwLowStr = ','.join([str(i) for i in spwLowSplit])

        spwB2B = list(range(max(spwHighSplit)+1))
        for spwId,spwRep in enumerate(spwHighSplit):
            spwB2B[spwRep] = spwLowSplit[spwId]

    else: # no reindexing took place
        spwHighStr = ','.join([str(i) for i in spwHigh])
        spwLowStr = ','.join([str(i) for i in spwLow])

        spwB2B = list(range(max(spwHigh)+1))
        for spwId,spwRep in enumerate(spwHigh):
            spwB2B[spwRep] = spwLow[spwId]


    print('bpCalScanList ', bpCalScanList)
    print('fluxCalScanList ', fluxCalScanList)
    print('diffGainCalScanListLow ', diffGainCalScanListLow)
    print('diffGainCalScanListHigh ', diffGainCalScanListHigh)
    print('spwHighStr ', spwHighStr)
    print('spwLowStr ', spwLowStr)
    print('spwB2B ', spwB2B)

    ###

    print('Writing code ...')

    casaCmd = ''

    if bpCalId != fluxCalId:

        casaCmd = casaCmd + "# calibrating the bandpass calibrator\n"
        casaCmd = casaCmd + "applycal(vis = '"+msName1+"',\n"
        casaCmd = casaCmd + "  field = '"+str(bpCalId)+"',\n"
        casaCmd = casaCmd + "  spw = '"+spwHighStr+"',\n"
        casaCmd = casaCmd + "  scan = '"+bpCalScanList+"',\n"
        casaCmd = casaCmd + "  interp=['linear','linear','linear'],\n"
        casaCmd = casaCmd + "  gaintable = ['"+bandpass+"','"+msName1+".phasehigh_int','"+msName1+".ampli_inf'],\n"
        casaCmd = casaCmd + "  gainfield = ['"+str(bpCalId)+"', '"+str(bpCalId)+"', '"+str(fluxCalId)+"'])\n"

        casaCmd = casaCmd + "\n# calibrating the fluxcalibrator\n"
        casaCmd = casaCmd + "applycal(vis = '"+msName1+"',\n"
        casaCmd = casaCmd + "  field = '"+str(fluxCalId)+"',\n"
        casaCmd = casaCmd + "  spw = '"+spwHighStr+"',\n" 
        casaCmd = casaCmd + "  scan = '"+fluxCalScanList+"',\n"
        casaCmd = casaCmd + "  interp=['linear','linear','linear'],\n"
        casaCmd = casaCmd + "  gaintable = ['"+bandpass+"','"+msName1+".phasehigh_int','"+msName1+".ampli_inf'],\n"
        casaCmd = casaCmd + "  gainfield = ['"+str(bpCalId)+"', '"+str(fluxCalId)+"', '"+str(fluxCalId)+"'])\n"

    else:
        casaCmd = casaCmd + "# calibrating the bandpass/fluxcalibrator\n"
        casaCmd = casaCmd + "applycal(vis = '"+msName1+"',\n"
        casaCmd = casaCmd + "  field = '"+str(bpCalId)+"',\n"
        casaCmd = casaCmd + "  spw = '"+spwHighStr+"',\n"
        casaCmd = casaCmd + "  scan = '"+bpCalScanList+"',\n"
        casaCmd = casaCmd + "  interp=['linear','linear','linear'],\n"
        casaCmd = casaCmd + "  gaintable = ['"+bandpass+"','"+msName1+".phasehigh_int','"+msName1+".ampli_inf'],\n"
        casaCmd = casaCmd + "  gainfield = ['"+str(bpCalId)+"', '"+str(bpCalId)+"', '"+str(fluxCalId)+"'])\n"

    # DGC is corrected with the LF phases
    # note we need to specify the scans, because if the DGC = BP, then the
    # previous BP correction is over written

    casaCmd = casaCmd + "\n# calibrating the DGC with the LF phases\n"
    casaCmd = casaCmd + "applycal(vis = '"+msName1+"',\n"
    casaCmd = casaCmd + "  field = '"+str(diffGainCalId)+"',\n"
    casaCmd = casaCmd + "  spw = '"+spwHighStr+"',\n"
    casaCmd = casaCmd + "  scan = '"+diffGainCalScanListHigh+"',\n"
    casaCmd = casaCmd + "  interp=['linear','linearPD','linear','linear'],\n"
    casaCmd = casaCmd + "  gaintable = ['"+bandpass+"','"+msName1+".phaselow_inf','"+msName1+".DGCoffset_multi','"+msName1+".ampli_inf'],\n"
    casaCmd = casaCmd + "  spwmap = [[],"+str(spwB2B)+",[],[]],\n"
    casaCmd = casaCmd + "  gainfield = ['"+str(bpCalId)+"','"+str(diffGainCalId)+"','"+str(diffGainCalId)+"','"+str(fluxCalId)+"'])\n"

    casaCmd = casaCmd + "\n## calibrating the target\n"
    casaCmd = casaCmd + "applycal(vis = '"+msName1+"',\n"
    casaCmd = casaCmd + "  field = '"+targetIdStr+"',\n"
    casaCmd = casaCmd + "  spw = '"+spwHighStr+"',\n" 
    casaCmd = casaCmd + "  interp = ['linear','linearPD', 'linear','linear'],\n"
    casaCmd = casaCmd + "  gaintable = ['"+bandpass+"','"+msName1+".phaselow_inf','"+msName1+".DGCoffset_multi','"+msName1+".ampli_inf'],\n"
    casaCmd = casaCmd + "  spwmap = [[],"+str(spwB2B)+",[],[]],\n"
    casaCmd = casaCmd + "  gainfield = ['"+str(bpCalId)+"', '"+str(phaseCalId)+"','"+str(diffGainCalId)+"','"+str(fluxCalId)+"'])\n"

    casaCmd = casaCmd + "\n## calibrating the checksource\n"
    casaCmd = casaCmd + "applycal(vis = '"+msName1+"',\n"
    casaCmd = casaCmd + "  field = '"+str(checkSourceId)+"',\n"
    casaCmd = casaCmd + "  spw = '"+spwHighStr+"',\n" 
    casaCmd = casaCmd + "  interp = ['linear','linearPD','linear','linear'],\n"
    casaCmd = casaCmd + "  gaintable = ['"+bandpass+"','"+msName1+".phaselow_inf','"+msName1+".DGCoffset_multi','"+msName1+".ampli_inf'],\n"
    casaCmd = casaCmd + "  spwmap = [[],"+str(spwB2B)+",[],[]],\n"
    casaCmd = casaCmd + "  gainfield = ['"+str(bpCalId)+"','"+str(phaseCalId)+"','"+str(diffGainCalId)+"','"+str(fluxCalId)+"'])\n"


    return casaCmd

#####################################

def doFluxCalibration(msNames, fluxFile='allFluxes.txt', refant='', valueMaps={}):
    """Generate code for the 'flux equalisation' of a set of calibrated MSs."""

    print('\n*** doFluxCalibration ***')

    if type(msNames).__name__ == 'str': msNames = [msNames]

    if os.path.exists(fluxFile) == False: 
        casalog.post('ERROR: Flux file '+fluxFile+' does not seem to exist in the current directory.','SEVERE')
        return False

    casaCmd = 'print "# Flux calibration of the data."\n\n'

    f = open(fluxFile, 'r')
    fc = f.readlines()
    f.close()

    msName = []
    fieldName = []
    spwId = []
    fluxVal = []

    for line in fc:

        if len(line) == 0 or line.isspace() == True or line.lstrip()[0] == '#': continue

        casaCmd = casaCmd + '# ' + line

        line = line.split('"')
        fieldName.append(line[1])
        line = line[2].split()
        spwId.append(line[0])
        fluxVal.append(line[3])
        msName.append(line[5])

    msName = np.array(msName)
    fieldName = np.array(fieldName)

    casaCmd = casaCmd + '\n'

    for i in range(len(msName)):

        if msName[i] not in msNames: 
            casalog.post('ERROR: Missing dataset.','SEVERE')
            return False

        casaCmd = casaCmd + "setjy(vis = '"+msName[i]+"',\n"
        casaCmd = casaCmd + "  field = '"+fieldName[i]+"',\n"
        casaCmd = casaCmd + "  spw = '"+spwId[i]+"',\n"
        casaCmd = casaCmd + "  standard = 'manual',\n"
        casaCmd = casaCmd + "  fluxdensity = ["+fluxVal[i]+", 0, 0, 0])\n\n"

    for i in range(len(msNames)):

        calFieldNames = np.unique(fieldName[np.where(msName == msNames[i])])
        calFieldNames = ','.join(calFieldNames)

        myRefAnt = refant
        if myRefAnt == '': myRefAnt = sfsdr.getRefAntenna(msNames[i])
        casaCmd = casaCmd + "os.system('rm -rf %s.ampli_inf') \n"%(msNames[i])
        casaCmd = casaCmd + "gaincal(vis = '"+msNames[i]+"',\n"
        casaCmd = casaCmd + "  caltable = '"+msNames[i]+".ampli_inf',\n"
        casaCmd = casaCmd + "  field = '"+calFieldNames+"',\n"
        casaCmd = casaCmd + "  solint = 'inf',\n"
        casaCmd = casaCmd + "  combine = 'scan',\n"
        casaCmd = casaCmd + "  refant = '"+myRefAnt+"',\n"
        casaCmd = casaCmd + "  gaintype = 'T',\n"
        casaCmd = casaCmd + "  calmode = 'a')\n\n"

        phaseCal = sfsdr.getPhaseCal(msNames[i], valueMaps=valueMaps)

        for sciFieldName in list(phaseCal.keys()):

            sciFieldIds = phaseCal[sciFieldName]['sciFieldIds']

            if len(sciFieldIds) > 1:
                j0 = 0
                sciFieldIds1 = str(sciFieldIds[j0])
                for j in range(len(sciFieldIds)-1):
                    if sciFieldIds[j+1] == sciFieldIds[j]+1: continue
                    sciFieldIds1 = sciFieldIds1 + '~' + str(sciFieldIds[j])
                    j0 = j+1
                    sciFieldIds1 = sciFieldIds1 + ',' + str(sciFieldIds[j0])
                sciFieldIds1 = sciFieldIds1 + '~' + str(sciFieldIds[j+1])
            else:
                sciFieldIds1 = str(sciFieldIds[0])

            casaCmd = casaCmd + "applycal(vis = '"+msNames[i]+"',\n"
            casaCmd = casaCmd + "  field = '"+str(phaseCal[sciFieldName]['phaseCalId'])+","+sciFieldIds1+"', # "+phaseCal[sciFieldName]['phaseCalName']+","+sciFieldName+"\n"
            casaCmd = casaCmd + "  gaintable = '"+msNames[i]+".ampli_inf',\n"
            casaCmd = casaCmd + "  gainfield = '"+str(phaseCal[sciFieldName]['phaseCalId'])+"', # "+phaseCal[sciFieldName]['phaseCalName']+"\n"
            casaCmd = casaCmd + "  calwt = False,\n"
            casaCmd = casaCmd + "  flagbackup = False)\n\n"

    if len(msNames) > 1:
        casaCmd = casaCmd + 'print "# Concatenating the data."\n\n'
        casaCmd = casaCmd + "concat(vis = "+str([i for i in msNames])+",\n"
        casaCmd = casaCmd + "  concatvis = 'calibrated.ms')\n\n"

    return casaCmd

###################################

def SDdoFillTsysSolutions(asapName, msName='', spwIds='', tsysCalTableName='', tsysmap='', iHaveSplitMyScienceSpw=False, doplot=False, sky=False, calmode=''):
    """Generate code for the Tsys solution step of an SD calibration script.

    spwIds must be specified as a string, e.g. spwIds = '1,3,5,7'"""

    if msName == '':
        if spwIds == '' and tsysmap == '': 
            casalog.post('ERROR: you have not specified neither msName, or spwIds and tsysmap.','SEVERE')
            return False
        if doplot == True: 
            casalog.post('ERROR: you have not specified msName, so I cannot do any plot.', 'SEVERE')
            return False

    if type(asapName).__name__ == 'str': asapName = [asapName]

    casaCmd = ''

    if sky == False:

        calTableName1 = msName + '.tsys'

        casaCmd = casaCmd + "os.system('rm -Rf "+calTableName1+"')\n\n"
        casaCmd = casaCmd + "gencal(vis = '"+msName+"',\n"
        casaCmd = casaCmd + "  caltable = '"+calTableName1+"',\n"
        casaCmd = casaCmd + "  caltype = 'tsys')\n\n"

    else:

        calTableName1 = msName + '.sky'

        casaCmd = casaCmd + "os.system('rm -Rf "+calTableName1+"')\n\n"
        casaCmd = casaCmd + "sdcal(infile = '"+msName+"',\n"
        casaCmd = casaCmd + "  outfile = '"+calTableName1+"',\n"
        casaCmd = casaCmd + "  calmode = '"+calmode+"')\n\n"

    tsysCalTableName.append(calTableName1)

    if doplot == True:
        chanrange = '92.1875%'

        casaCmd = casaCmd + "plotbandpass(caltable='%s', overlay='time', \n" %(calTableName1)
        casaCmd = casaCmd + "  xaxis='freq', yaxis='amp', subplot=22, buildpdf=False, interactive=False,\n"
        casaCmd = casaCmd + "  showatm=True,pwv='auto',chanrange='"+chanrange+"',showfdm=True, \n"
        casaCmd = casaCmd + "  field='', figfile='%s') \n" %(calTableName1+'.plots.overlayTime/'+calTableName1.split('/')[-1])

        casaCmd = casaCmd + "\nif applyonly != True: es.checkCalTable('"+calTableName1+"', msName='"+msName+"', interactive=False)\n"

    return casaCmd

#######################################

def SDdoCalibration(asapName, msName='', spwIds='', calmode='ps', tsysCalTableName='', tsysmap='', iHaveSplitMyScienceSpw=False, doplot=True, skyCalTableName=''):
    """Generate code for the sdcal step of an SD  calibration script.

       spwIds must be specified as a string, e.g. spwIds = '1,3,5,7'"""

    if msName == '' and spwIds == '': 
        casalog.post('ERROR: you have not specified neither msName, or spwIds.','SEVERE')
        return False

    if type(asapName).__name__ == 'str': asapName = [asapName]

    if msName != '':
        spwInfo = sfsdr.getSpwInfo(msName, caching=True)
        spwIds1 = sorted(spwInfo.keys())
        spwIds1 = [int(i) for i in spwIds1]
        if iHaveSplitMyScienceSpw == True: 
            spwIds1 = list(range(len(spwIds1)))
        spwIds = ','.join([str(i) for i in spwIds1])
    else:
        spwIds1 = spwIds.split(',')
        spwIds1 = [int(i) for i in spwIds1]

    casaCmd = ''

    if tsysmap == '':
        if tsysCalTableName == '': 
            casalog.post('ERROR: you have not specified a Tsys cal table.','SEVERE')
            return False
        if aU.getCasaVersion() < '5.9.9':
            casaCmd = casaCmd + "from recipes.almahelpers import tsysspwmap\n"
        else:
            casaCmd = casaCmd + "from casarecipes.almahelpers import tsysspwmap\n"
        casaCmd = casaCmd + "tsysmap = tsysspwmap(vis = '"+msName+"', tsystable = '"+tsysCalTableName+"', trim = False)\n\n"
    else:
        casaCmd = casaCmd + "tsysmap = "+str(tsysmap)+"\n\n"

    if aU.getCasaVersion() < '5.0':
        mymsmd = msmdtool()
        mymsmd.open(msName)
        tsysspw = [i for i in mymsmd.spwsforintent('CALIBRATE_ATMOSPHERE#ON_SOURCE') if i not in mymsmd.chanavgspws().tolist()+mymsmd.wvrspws().tolist()]
        if (tsysspw == []):
            print("Will use CALIBRATE_ATMOSPHERE#HOT instead.")
            tsysspw = [i for i in mymsmd.spwsforintent('CALIBRATE_ATMOSPHERE#HOT') if i not in mymsmd.chanavgspws().tolist()+mymsmd.wvrspws().tolist()]

        mymsmd.close()

        casaCmd = casaCmd + "spwmap = {}\n"
        casaCmd = casaCmd + "for i in "+str(spwIds1)+":\n"
        casaCmd = casaCmd + "  if not tsysmap[i] in spwmap.keys():\n"
        casaCmd = casaCmd + "    spwmap[tsysmap[i]] = []\n"
        casaCmd = casaCmd + "  spwmap[tsysmap[i]].append(i)\n\n"

    for i in asapName:

        if aU.getCasaVersion() < '5.0':

            casaCmd = casaCmd + "os.system('rm -Rf "+i+".cal')\n\n"

            calmode1 =  calmode+',tsys,apply'

            casaCmd = casaCmd + "sdcal2(infile = '"+i+"',\n"
            casaCmd = casaCmd + "  calmode = '"+calmode1+"',\n"
            casaCmd = casaCmd + "  spw = '"+','.join([str(j) for j in sorted(spwIds1+tsysspw)])+"',\n"
            casaCmd = casaCmd + "  tsysspw = '"+','.join([str(j) for j in tsysspw])+"',\n"
            casaCmd = casaCmd + "  spwmap = spwmap,\n"
            casaCmd = casaCmd + "  outfile = '"+i+".cal',\n"
            casaCmd = casaCmd + "  overwrite = True)\n\n"

        else:
            mymsmd = msmdtool()
            mymsmd.open(msName)
            targetFieldIds = mymsmd.fieldsforintent('OBSERVE_TARGET#ON_SOURCE')
            mymsmd.close()

            casaCmd = casaCmd + "for i in "+str([str(j) for j in targetFieldIds])+":\n"
            casaCmd = casaCmd + "  applycal(vis = '"+i+"',\n"
            casaCmd = casaCmd + "    applymode = 'calflagstrict',\n"
            casaCmd = casaCmd + "    spw = '"+','.join([str(j) for j in sorted(spwIds1)])+"',\n"
            casaCmd = casaCmd + "    field = i,\n"
            casaCmd = casaCmd + "    gaintable = ['"+tsysCalTableName+"', '"+skyCalTableName+"'],\n"
            casaCmd = casaCmd + "    gainfield = ['nearest', i],\n"
            casaCmd = casaCmd + "    spwmap = tsysmap)\n\n"

        if doplot == True:
            if aU.getCasaVersion() < '5.0':
                casaCmd = casaCmd + "if applyonly != True: es.SDcheckSpectra(asapName='"+i+".cal', spwIds='"+spwIds+"', interactive=False)\n\n"
            else:
                casaCmd = casaCmd + "if applyonly != True: es.SDcheckSpectra(msName='"+i+"', spwIds='"+spwIds+"', intent='OBSERVE_TARGET#ON_SOURCE', interactive=False)\n\n"

    return casaCmd


##################################################

def SDdoBaselineSubtraction(asapName, msName='', spwIds='', iHaveSplitMyScienceSpw=False, doplot=True):
    """Generate code for the baseline subtraction step of an SD calibration script.

       spwIds must be specified as a string, e.g. spwIds = '1,3,5,7'"""

    if msName == '' and spwIds == '': 
        casalog.post('ERROR: you have not specified neither msName, or spwIds.','SEVERE')

    if type(asapName).__name__ == 'str': asapName = [asapName]

    if msName != '':
        spwInfo = sfsdr.getSpwInfo(msName, caching=True)
        spwIds1 = sorted(spwInfo.keys())
        spwIds1 = [int(i) for i in spwIds1]
        if iHaveSplitMyScienceSpw == True: 
            spwIds1 = list(range(len(spwIds1)))
        spwIds = ','.join([str(i) for i in spwIds1])
    else:
        spwIds1 = spwIds.split(',')
        spwIds1 = [int(i) for i in spwIds1]

    casaCmd = ''

    for i in asapName:

        casaCmd = casaCmd + "os.system('rm -Rf "+i+".bl')\n\n"

        if aU.getCasaVersion() >= '5.0':

            casaCmd = casaCmd + "sdbaseline(infile = '"+i+"',\n"
            casaCmd = casaCmd + "  datacolumn = 'corrected',\n"
            casaCmd = casaCmd + "  spw = '"+','.join([str(j) for j in spwIds1])+"',\n"
            casaCmd = casaCmd + "  maskmode = 'auto',\n"
            casaCmd = casaCmd + "  thresh = 5.0,\n"
            casaCmd = casaCmd + "  avg_limit = 4,\n"
            casaCmd = casaCmd + "  blfunc = 'poly',\n"
            casaCmd = casaCmd + "  order = 1,\n"
            casaCmd = casaCmd + "  outfile = '"+i+".bl')\n\n"

        else:

            casaCmd = casaCmd + "sdbaseline(infile = '"+i+"',\n"
            casaCmd = casaCmd + "  spw = '"+','.join([str(j) for j in spwIds1])+"',\n"
            casaCmd = casaCmd + "  maskmode = 'auto',\n"
            casaCmd = casaCmd + "  thresh = 5.0,\n"
            casaCmd = casaCmd + "  avg_limit = 4,\n"
            casaCmd = casaCmd + "  blfunc = 'poly',\n"
            casaCmd = casaCmd + "  order = 1,\n"
            casaCmd = casaCmd + "  outfile = '"+i+".bl',\n"
            casaCmd = casaCmd + "  overwrite = True)\n\n"

        if doplot == True:
            if aU.getCasaVersion() < '5.0':
                casaCmd = casaCmd + "if applyonly != True: es.SDcheckSpectra(asapName='"+i+".bl', spwIds='"+spwIds+"', interactive=False)\n\n"
            else:
                spwIds = list(range(len(spwIds.split(','))))
                spwIds = ','.join([str(j) for j in spwIds])
                casaCmd = casaCmd + "if applyonly != True: es.SDcheckSpectra(msName='"+i+".bl', spwIds='"+spwIds+"', intent='OBSERVE_TARGET#ON_SOURCE', interactive=False)\n\n"

    return casaCmd

def isFullPol(msName, valueMaps={}):
    """Determine if the given MS is a full polarisation dataset.
    Return True if so."""

    if msName in valueMaps.keys():
        vm = valueMaps[msName]
        print('Using canned ValueMap.')
    else:
        vm = aU.ValueMapping(msName)
        valueMaps[msName] = vm

    polcalSpws = vm.getSpwsForIntent('CALIBRATE_POLARIZATION#ON_SOURCE')
    if polcalSpws == []:
        return False
    else:
        scienceSpws = vm.getSpwsForIntent('OBSERVE_TARGET#ON_SOURCE')
        for myspw in scienceSpws:
            if not myspw in polcalSpws:
                casalog.post("This dataset seems to be FULL POLARISATION (it has polcal data) but the polcal was not observed for all science SPWs.",
                             'WARN')
                break

    return True



def isB2BorBWSW(msName, valueMaps={}):
    """Determine if the given dataset uses band-to-band phase transfer
       or bandwidth switching. Returns two bool values
       isB2B, isBWSW"""

    if msName in valueMaps.keys():
        vm = valueMaps[msName]
        print('Using canned ValueMap.')
    else:
        vm = aU.ValueMapping(msName)
        valueMaps[msName] = vm

    spwsPhasecal = vm.getSpwsForIntent('CALIBRATE_PHASE#ON_SOURCE')
    spwsDiffgainsig = vm.getSpwsForIntent('CALIBRATE_DIFFGAIN#SIGNAL')
    if spwsDiffgainsig == []:
        spwsDiffgainsig = vm.getSpwsForIntent('CALIBRATE_DIFFGAIN#ON_SOURCE')
        diffGainSignalIntent = 'CALIBRATE_DIFFGAIN#ON_SOURCE'
    else:
        diffGainSignalIntent = 'CALIBRATE_DIFFGAIN#SIGNAL'
    spwsDiffgainref = vm.getSpwsForIntent('CALIBRATE_DIFFGAIN#REFERENCE')

    isB2B = False
    isBWSW = False

    if spwsPhasecal != []:
        if (spwsDiffgainsig != []) and (spwsDiffgainref != []):
            print('Found intents '+diffGainSignalIntent+' and CALIBRATE_DIFFGAIN#REFERENCE.')
            casalog.post('Found intents '+diffGainSignalIntent+' and CALIBRATE_DIFFGAIN#REFERENCE.', 'INFO')

            print('spwsDiffgainsig '+str(spwsDiffgainsig))
            print('spwsDiffgainref '+str(spwsDiffgainref))

            if spwsPhasecal == spwsDiffgainref:
                minFreq=1E99
                maxFreq=0
                sigMaxbw=0
                refMaxbw=0
                for myspw in spwsDiffgainsig:
                    if myspw in vm.spwInfo.keys():
                        thefreq = vm.spwInfo[myspw]['meanFreq']
                        thebw = vm.spwInfo[myspw]['bandwidth']
                        thenumchan = vm.spwInfo[myspw]['numChannels']
                        if thefreq < minFreq:
                            minFreq = thefreq
                        if thefreq > maxFreq:
                            maxFreq = thefreq
                        if thebw > sigMaxbw and thenumchan > 4:
                            sigMaxbw = thebw
                for myspw in spwsDiffgainref:
                    if myspw in vm.spwInfo.keys():
                        thefreq = vm.spwInfo[myspw]['meanFreq']
                        thebw = vm.spwInfo[myspw]['bandwidth']
                        thenumchan = vm.spwInfo[myspw]['numChannels']
                        if thefreq < minFreq:
                            minFreq = thefreq
                        if thefreq > maxFreq:
                            maxFreq = thefreq
                        if thebw > refMaxbw and thenumchan > 4 and 'CALIBRATE_POINTING#ON_SOURCE' not in vm.getIntentsForSpw(myspw):
                            refMaxbw = thebw

                if maxFreq > 1.662* minFreq: # 1.661 is the largest possible ratio within one band
                    print('minFreq '+str(minFreq))
                    print('maxFreq '+str(maxFreq))
                    print('The SPWs for intent CALIBRATE_DIFFGAIN fall into different bands. This is an observation with band-to-band phase transfer.')
                    casalog.post('This is an observation with band-to-band phase transfer.', 'INFO')
                    isB2B = True

                elif refMaxbw > 1.5*sigMaxbw:
                    print('minFreq '+str(minFreq))
                    print('maxFreq '+str(maxFreq))
                    print('sigMaxbw '+str(sigMaxbw))
                    print('refMaxbw '+str(refMaxbw))
                    print('The SPWs for CALIBRATE_DIFFGAIN#REFERENCE are wider than for '+diffGainSignalIntent)
                    print('This is an observation with bandwidth switching.')
                    casalog.post('This is an observation with bandwidth switching.', 'INFO')
                    isBWSW = True
            else:
                casalog.post('Phasecal scans use different SPWs than the scans with intent CALIBRATE_DIFFGAIN#REFERENCE!', 'WARN')
                casalog.post('Will try to treat this as an observation with bandwidth switching but it may fail ...', 'WARN')
                isBWSW = True

        elif (spwsDiffgainsig != []):
            casalog.post('Found intents '+diffGainSignalIntent+' but not CALIBRATE_DIFFGAIN#REFERENCE.', 'WARN')
        elif (spwsDiffgainref != []):
            casalog.post('Found intents CALIBRATE_DIFFGAIN#REFERENCE but not CALIBRATE_DIFFGAIN#SIGNAL nor CALIBRATE_DIFFGAIN#ON_SOURCE.', 'WARN')

    return isB2B, isBWSW
            

#######################################


