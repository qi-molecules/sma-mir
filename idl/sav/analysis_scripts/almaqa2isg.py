# ALMA Quality Assurance Software
# QA2 Imaging Script Generator
# D. Petry (ESO)
# A. Borkar (EU ARC Node, Czech Republic)
# G. Bendo (EU ARC Node, UK)
# $Id: almaqa2isg.py,v 1.23 2021/03/11 10:16:46 dpetry Exp $
#
"""
The ALMA QA2 Imaging Script Generator
"""

from __future__ import print_function

import os
import sys
import numpy as np
import glob
import re
import analysisUtils as aU
sfsdr = aU.stuffForScienceDataReduction()

try:  # Python 3
    from casatasks import importasdm
    from casatasks import gencal
    from casatasks import casalog
    from casatools import table as tbtool
    from casatools import ms as mstool
    from casatools import msmetadata as msmdtool
    from casatools import quanta as qatool

    import builtins as exceptions

except ImportError:  # Python 2
    from taskinit import *
    from importasdm_cli import importasdm_cli as importasdm
    from gencal_cli import gencal_cli as gencal

    import exceptions

def version(short=False):
    """
    Returns the CVS revision number.
    """
    myversion = "$Id: almaqa2isg.py,v 1.23 2021/03/11 10:16:46 dpetry Exp $"
    if (short):
        myversion = myversion.split()[2]
    return myversion
    

def generateImagingScript(vis="", draft_threshold="", reqchanwidth=1, makecubes=True, docontsub=None, chanwidthtol=0.05,
                          restfreqs={}, additionalimg=['CALIBRATE_POLARIZATION', 'OBSERVE_CHECK_SOURCE'], spwmap=[],
                          perchanweightdensity=False):

    """
    The ALMA QA2 Imaging Script Generator

    vis - the MS(s) to image, can be a list. 
          If wildcards are used, a list is created automatically.

    draft_threshold - the cleaning threshold to use as a initial value in the cleaning (as a string e.g. "5 mJy")

    reqchanwidth - the requested channel width (for cubes) as a string (e.g. "200MHz", "20km/s")
                   or as an integer which indicates units of original channels;
                   if == None, the draft_threshold is used for the agg. bw.
                   default: 1

    makecubes - Boolean to indicate whether cubes should be made (set to False for continuum only)
                default: True

    docontsub - Boolean to indicate whether continuum subtraction should be done
                (only relevant if makecubes==True)
                default: True for standard polarisation, False for full polarisation data sets

    chanwidthtol - the tolerance in units of channel widths to apply when deciding whether the
                grids of corresponding SPWs from two different MSs are aligned
                default: 0.05 (i.e. 5%)

    restfreqs - the restfrequencies to be used for the restfreq parameter in the cube cleaning
                in the format of a dictionary, e.g. {25: '230GHz', 27: '231GHz', ... } with
                one entry for each science SPW (optional).
                default: {} (empty dictionary; the restfreqs, if needed, will be extracted from
                            the SOURCE table or, if not available there, the central freqs will be used)

    additionalimg - optional list of additional intents to be imaged.
                 For each field found with the given intent (give name omitting the "#ON_SOURCE"),
                 code for making an aggregate bandwidth image is going to be added.
                 Possible values: 'CALIBRATE_PHASE', 'CALIBRATE_BANDPASS', 'CALIBRATE_FLUX',
                      'OBSERVE_CHECK_SOURCE', 'CALIBRATE_POLARIZATION'
                 default: ['CALIBRATE_POLARIZATION', 'OBSERVE_CHECK_SOURCE'] (image check source(s) and/or polcal)
                 example: ['CALIBRATE_BANDPASS', 'CALIBRATE_PHASE']

    spwmap     - SPW IDs to be used in the final image names.
                 If a non-empty list is specified, it must contain at least as many 
                 elements as there are science SPWs in the input MS. The given SPW IDs are then
                 used to replace in the image names the ones used in the MS,
                 e.g. if there are science spws [0,1,2,3] in the MS, 
                 then setting spwmap=[19,21,23,25] will result in ID 19
                 being used instead of 0 in the image names, 21 instead of 1 etc.
                 default: [] (empty list) - use the SPW IDs as they are in the MS.

    perchanweightdensity - the setting of the tclean parameter perchanweightdensity
                 default: False

    Example: 
      import almaqa2isg as isg
      isg.generateImagingScript('uid*.ms.split.cal', draft_threshold='0.1mJy', reqchanwidth='20km/s', spwmap=[17,19,21,23])

    Discussion:
      You can run the Imaging Script Generator (ISG) like so:

         import almaqa2isg as isg
         cd <the directory where the MS(s) are which you want to image, e.g. "calibrated">
         isg.generateImagingScript(vis='uid___A00*.ms.split.cal', draft_threshold='0.1mJy', reqchanwidth='20km/s')

      The vis parameter can be a single string or a list. If it is a list, each element must be the name of an 
      existing MS. If it is a single string, it can either be a single name of an existing MS or an expression 
      using wildcards "*" or "?" to specify a group of MSs. This will internally be converted to a list of MSs.

      If makecubes is true (default), code for cleaning cubes for each science SPW is created.

      If docontsub is true, code for continuum subtraction (separate for each science field) is created. 
      This parameter is only active is makecubes is true.

      draft_threshold specifies the threshold which is entered into the cleaning commands. It is related to
      the required rms from the MOUS proposal. It is used as the cleaning threshold in cube 
      cleaning if makecubes is true (then the threshold for the agg. bw. image is left for editing by the analyst). 
      If makecubes==False, then draft_threshold is used as the cleaning threshold for the agg. bw. image.

      reqchanwidth is the bandwidth on which draft_threshold is defined. It is used as the channel width for the cubes if 
      makecubes is true.

      Like for the calibration script generator, the ISG is not meant to produce a script which one can run 
      blindly. We are not trying to reinvent the imaging pipeline. So, if different channel widths are required 
      for different SPWs, the analyst needs to edit the channel width (and the corresponding thresholds) for some 
      of the cubes. A general solution to permit different thresholds and widths for different SPWs was deemed too 
      complex for the moment.

      NOTE that if there is more than one input MS and docontsub is True, then a concat step will be inserted such 
      that the uvcontsub commands can operate on the concatenated MS. If the SPWs are not yet reindexed to have 
      IDs starting at 0, uvcontsub will do this reindexing. The ISG will take this into account and use reindexed 
      SPW IDs when making the cubes but leave the old SPW ids in the image names (this is what archive needs).

      Furthermore, if constinuum subtraction is requested, the ISG will check if the corresponding SPWs of the MSs 
      are shifted w.r.t. each other and if so, find the largest common grid for each SPW and generate mstransform code 
      to transform all science SPWs of all the MSs into these grids before concatenation.

      At the end of the isg run, you obtain a file "scriptForImaging.py". On the terminal (in CASA) you will also 
      get messages which explain what you need to do to complete this script before you can run it. These messages 
      look, e.g., like this:

        Script generation completed. Please find scriptForImaging.py in the current directory.

        NOTE that you still need to edit the script:
          - You need to edit the array "therestfreqs" to set the rest frequencies for the cubes of each SPW.
          - You need to adjust the threshold for the aggregate bandwidth/continuum image(s).
          - You need to edit the fitspw parameter in each uvcontsub command.

      Iterative use for continuum identification
      ------------------------------------------

      If cubes are to be created and a decision needs to be taken about whether contsub is necessary and how it 
      should be done, one generally can go two routes:

      a) use plotms to generate amp vs. channel plots to identify lines and line-free regions

      b) create image cubes for each spw once to see if they contain detectable spectral lines and identify 
         continuum channels.

      When going route (b), it is recommended that the user runs the ISG twice, first with docontsub=False and then 
      with docontsub=True: With the first version, one can create some shallowly cleaned cubes to check for lines 
      and then move the first script version and the created images to a different directory and start again with 
      docontsub=True.

      In order to make sure that a new run of the ISG does not destroy your previous version of the script, the 
      ISG prevents you from overwriting a pre-existing scriptForImaging.py

      SPW Numbering
      -------------

      If you have used split or mstransform *with* reindexing to split out certain SPWs to obtain the input MS(s) to the ISG,
      i.e. if your input MS(s) do *not* use the same SPW numbering as the original ASDM, you need to use the parameter
      "spwmap" to get the archive-compliant numbering in the image names. 
      The spwmap parameter should contain an ordered list of the *original* SPW IDs of *all* the science SPWs 
      in the input MS(s). The input MSs, if there is more than one, need to all use the same numbering. 
      A typical example is [19,21,23,25]. You need to look at the original raw MS after importasdm to obtain this information.

      Full Polarization support
      -------------------------

      The ISG will detect whether an input MS contains full polarisation data. In this case it will use stokes='IQUV' and
      dconvolver='clarkstokes'. Furthermore, if the intent CALIBRATE_POLARISATION is present, it will by default make
      the appropriate calibrator image and add operations to calculate polcal parameters.

    """

    casalog.origin('generateImagingScript')
    casalog.post( "QA2 Imaging Script Generator "+version())
    print("QA2 Imaging Script Generator "+version())
    
    scriptname = 'scriptForImaging.py'

    myms = mstool()
    mytb = tbtool()
    mymsmd = msmdtool()
    myqa = qatool()


    if os.path.exists(scriptname):
        print("ERROR: "+scriptname+" already exists. Please remove and try again.")
        return False

    # verify input
    if(vis=="" or (type(vis)!=str and type(vis)!=list)):
        print("ERROR: Invalid vis parameter. Must be non-empty string or list of strings.")
        return False
    
    try:
        mydraft_threshold = myqa.quantity(draft_threshold)
        if mydraft_threshold['value'] <= 0.:
            print("ERROR: Invalid draft_threshold parameter. Must have positive value.")
            return False
        if not (mydraft_threshold['unit'] in ["Jy", "mJy", "uJy"]):
            print("ERROR: Invalid draft_threshold parameter. Must have unit Jy, mJy, or uJy.")
            return False
    except:
        print("ERROR: Invalid draft_threshold parameter. Must be string quantity, e.g. 25mJy")
        return False

    myreqchanwidth=None
    if type(reqchanwidth)==int:
        if reqchanwidth<=0:
            print("ERROR: Invalid reqchanwidth parameter. Must be integer or string quantity")
            return False
        myreqchanwidth = {'unit': 'channel', 'value': reqchanwidth} 
    elif type(reqchanwidth)==str:
        try:
            myreqchanwidth = myqa.quantity(reqchanwidth)
            if myreqchanwidth['value'] <= 0.:
                print("ERROR: Invalid reqchanwidth parameter. Must have positive value.")
                return False
            if not (myreqchanwidth['unit'] in ["Hz", "kHz", "MHz", "GHz", "m/s", "km/s"]):
                print("ERROR: Invalid reqchanwidth parameter. Must have unit Hz, kHz, MHz, GHz, m/s, or km/s.")
                return False
        except:
            print("ERROR: Invalid reqchanwidth parameter. Must be integer or string quantity, e.g. 25mJy")
            return False
    elif reqchanwidth!=None:
        print("ERROR: Invalid reqchanwidth parameter. Must be integer or string quantity, e.g. 25mJy, or None")
        return False
        
    if not type(makecubes)==bool:
        print("ERROR: Invalid makecubes parameter. Must be True or False.")
        return False

    if not type(docontsub)==bool and not docontsub==None:
        print("ERROR: Invalid docontsub parameter. Must be True or False.")
        return False

    if not type(chanwidthtol)==float or chanwidthtol<0.:
        print("ERROR: Invalid chanwidthtol parameter. Must be a float >= 0.")
        return False

    myviss = vis
    if type(vis)==str:
        if ('*' in vis) or ('?' in vis):
             myviss = glob.glob(vis)
             if len(myviss) == 0:
                 print("ERROR: Invalid vis parameter. No MSs found matching "+vis)
                 return False
        else:
            myviss = [vis]

    # make sure the list is sorted chronologically (this is important for judging the need for mstransform before concat);
    # at the same time, check the presence of the intent CALIBRATE_POLARIZATION

    visspwsandtimes = []
    addfullpolanalysis = False

    if len(myviss)>1:
        print("Sorting list of input MSs by observation time ...")
    for myvis in myviss: # need to go over this even when len(myviss)==1 since we need the science SPWs
        if type(myvis) != str:
            print("ERROR: Invalid vis parameter. Must be non-empty string or list of strings.")
            return False
        try: # open MS as a test and at the same time extract first science obs time and science SPWs
            mymsmd.open(myvis)
        except:
            print("ERROR: Invalid vis parameter. Could not open MS "+str(myvis))
            return False
        try:
            onsourcetimes = mymsmd.timesforintent("OBSERVE_TARGET#ON_SOURCE")
            thetargetintent = "OBSERVE_TARGET#ON_SOURCE"

            if len(onsourcetimes) == 0:
                print("ERROR: There is no data with intent OBSERVE_TARGET#ON_SOURCE in MS "+str(myvis))
                myintents = mymsmd.intents()
                for myintent in myintents:
                    if 'OBSERVE_TARGET' in myintent:
                        thetargetintent = myintent
                print("Will try to continue using the times and spws for intent "+thetargetintent+" ...")

            onsourcetimes = mymsmd.timesforintent(thetargetintent)
            firstonsourcetime = onsourcetimes[0]
            otos_spwids = list(mymsmd.spwsforintent(thetargetintent))

            spwids = []
            for myspw in otos_spwids:
                if mymsmd.nchan(myspw)>4:
                    spwids.append(myspw)

            if 'CALIBRATE_POLARIZATION#ON_SOURCE' in mymsmd.intents():
                addfullpolanalysis = True # we will need to add the full pol analysis step at the end before the fitsexport

            mymsmd.close()
            spwids.sort() # should not be necessary but let's stay on the safe side
            visspwsandtimes.append((firstonsourcetime, myvis, spwids))
        except:
            print("ERROR trying to determine on-source times and SPWs for MS "+str(myvis)+": "+str(sys.exc_info()))
            return False
    # now sort myviss by the times in visspwsandtimes, i.e. chronologically
    sortedvisspwsandtimes = sorted(visspwsandtimes)
    myviss = [myvis for i, myvis, myspws in sortedvisspwsandtimes]
    # and make an accordingly sorted list of the spws
    sciencespws = [myspws for i, myvis, myspws in sortedvisspwsandtimes]

    hascorrected = []
    print("Science SPWs in the input MS(s):")
    for myvisspwsandtimes in sortedvisspwsandtimes:
        myvis = myvisspwsandtimes[1]
        print("  "+myvis+": "+str(myvisspwsandtimes[2]) )
        try:
            mytb.open(myvis)
            mycols = mytb.colnames()
            hascorrected.append("CORRECTED_DATA" in mycols)
            mytb.close()
        except:
            print("ERROR when accessing MS "+str(myvis))
            return False

    for myspwids in sciencespws:
        if not myspwids == sciencespws[0]:
            print("ERROR: The input MSs have different science SPW IDs!")
            return False
        
    sciencespws = sciencespws[0]

    if True in hascorrected:
        if False in hascorrected:
            print("ERROR: inhomogenous data columns in input MS: some have CORRECTED_DATA, some don't.")
            return False

    if not type(restfreqs) == dict:
        print("ERROR: Invalid parameter restfreqs. Must be a dictionary, e.g. {25: '230GHz', 27: '231GHz', ...}")
        return False

    possibleadtlimg = ['CALIBRATE_PHASE', 
                       'CALIBRATE_BANDPASS', 
                       'CALIBRATE_FLUX',
                       'OBSERVE_CHECK_SOURCE',
                       'CALIBRATE_POLARIZATION']

    if not type(additionalimg) == list:
        print("ERROR: Invalid parameter additionalimg. Must be a list, e.g. ['CALIBRATE_BANDPASS', 'OBSERVE_CHECK_SOURCE']")
        print("Possible elements are: ")
        print(possibleadtlimg)
        return False
    else:
        for myintent in additionalimg:
            if not myintent in possibleadtlimg:
                print("ERROR: Invalid parameter additionalimg. Possible elements are: ")
                print(possibleadtlimg)
                return False
                
    if not type(spwmap) == list:
        print("ERROR: invalid spwmap, must be list: "+str(spwmap))
        return False

    # check that spwmap covers all science SPWs
    if len(spwmap) < len(sciencespws):
        if spwmap == []:
            myspwmap = sciencespws
        else:
            print("ERROR: invalid spwmap, must cover all science SPWs in the MS.")
    else:
        myspwmap = spwmap
        myspwmap.sort()
        for myspw in myspwmap:
            if not (type(myspw) == int and myspw>=0):
                print("ERROR: invalid spwmap, must only contain positive integers.")
                return False

    # determine default value of docontsub if not explicitly set

    if docontsub == None:
        if addfullpolanalysis:
            docontsub = False
            if makecubes:
                print("\nBy default, no contsub for full pol. data.")
        else:
            docontsub = True
            

    print("vis  = "+str(myviss))
    print("draft_threshold = "+str(mydraft_threshold))
    print("reqchanwidth = "+str(myreqchanwidth))
    print("makecubes = "+str(makecubes))
    print("docontsub = "+str(docontsub))
    print("chanwidthtol = "+str(chanwidthtol))
    print("restfreqs = "+str(restfreqs))
    print("additionalimg = "+str(additionalimg))
    print("spwmap = "+str(spwmap))
    print("perchanweightdensity = "+str(perchanweightdensity))
    print
    
    # parameters verified

    # gather input parameters for getimgpars using findfields

    representativems=0
    targetfielddicts = findfields(myviss[representativems], thetargetintent, myspwmap)

    if len(targetfielddicts)==0:
        print("ERROR: Could not find any target fields in the given MSs.")
        return False

    discrepantfields=[]
    if len(myviss)>1: # for the case of a mosaic and multiple input MSs, double-check that all MSs have same mosaic fields
        reprmschanged=False
        for mydict in targetfielddicts:
            if mydict['ismosaic']:
                print("Checking whether target field setup for mosaic field "+mydict['fieldname']+" agrees between input MSs ...")
                for i in range(0,len(myviss)):
                    if i==representativems:
                        continue
                    otherdicts = findfields(myviss[i], thetargetintent, myspwmap) 
                    for myotherdict in otherdicts:
                        if myotherdict['fieldname'] == mydict['fieldname']:
                            if not sorted(myotherdict['fieldids']) == sorted(mydict['fieldids']):
                                casalog.post("Not all input MSs have same field ids for field "+mydict['fieldname'], 'WARN') 
                                casalog.post(myviss[representativems]+": "+str(sorted(mydict['fieldids'])), 'WARN')
                                casalog.post(myviss[i]+": "+str(sorted(myotherdict['fieldids'])), 'WARN')
                                if len(myotherdict['fieldids'])>len(mydict['fieldids']):
                                    casalog.post("Switching to "+myviss[i]+" as representative MS. ", 'WARN')
                                    mydict['fieldids'] = myotherdict['fieldids']
                                    representativems = i
                                    reprmschanged=True
                                print
                                discrepantfields.append(mydict['fieldname'])
                            break # there can be no other dict for the same fieldname
                        # end if
                    # end for
                # end for
            # end if
        # end for
        if reprmschanged:
            targetfielddicts = findfields(myviss[representativems], thetargetintent, myspwmap)

    # initialise the output script file
    scriptfile = open(scriptname, 'w')
    mystepindent = "  "
    mystepdict = {}

    haveshifts = False
    shiftsHz = {}
    shiftsChannels = {}
    haverestfreqs = True
    yourthresholdhere = False

    # write necessary import statements to script

    stext = "import os"
    scriptfile.write("\n"+stext)
    stext = "import sys"
    scriptfile.write("\n"+stext+"\n")

    # write global var defs to script

    # thevis
    stext = printstrarray(myviss, "thevis")
    scriptfile.write("\n"+stext)

    # therestfreqs
    if makecubes:

        # get the nominal restfreqs from the MS
        try:
            nomrestfreqs = aU.restFrequencies(myviss[0], showSpwFreq=True)
        except:
            casalog.post("ERROR: in call to aU.restFrequencies - "+str(sys.exc_info()), 'WARN')
            casalog.post("       Your version of the analysisUtils may be broken or this is not ALMA data.", 'WARN')
            casalog.post("       Will try to continue with user-provided or central freqs ...", 'WARN')
            nomrestfreqs = []
            

        stext = "# put restfreqs in this dictionary,\n# one for each SPW ID, e.g. {17: '350GHz', 19: '356GHz'}\n"
        stext = stext+"therestfreqs = {"
        idnt = len("therestfreqs = {")*" "
        for myspw in targetfielddicts[0]['spwids']:
            if myspw in restfreqs:
                if type(restfreqs[myspw]) == str:
                    stext = stext+str(myspw)+": '"+restfreqs[myspw]+"',\n"+idnt
                else:
                    print("ERROR: Invalid parameter restfreqs. Dictionary entries must be strings, e.g. {25: '230GHz', 27: '231GHz', ...}")
                    return False
            else: # the user did not provide a restfreq; try to use nominal
                if len(nomrestfreqs)>0 and len(nomrestfreqs[myspw]['frequency']) > 0:
                    print("No restfreq provided by user for spw "+str(myspw)+". Will use value from SOURCE table.")
                    stext = stext+str(myspw)+": '"+str(round(nomrestfreqs[myspw]['frequency'][0]/1E9,10))+"GHz',\n"+idnt
                else:
                    print("No restfreq provided by user for spw "+str(myspw)+" and no value in SOURCE table. Will use central freq.")
                    mymsmd.open(myviss[0])
                    mycenterfreq = mymsmd.meanfreq(myspw)
                    mymsmd.close()
                    stext = stext+str(myspw)+": '"+str(round(mycenterfreq/1E9,10))+"GHz',\n"+idnt
                    haverestfreqs = False

        stext = stext[0:len(stext)-len(",\n"+idnt)]
        stext = stext+"}"
        scriptfile.write(stext+"\n\n")

        if(targetfielddicts[0]['isephem']) and aU.getCasaVersion() < '5.4.0':
            print("ERROR: handling of ephemeris target objects not supported for CASA versions < 5.4.0 .")
            return False            

        if docontsub:
            if(targetfielddicts[0]['isephem'] and len(myviss)>1):
                print("ERROR: continuum subtraction for ephemeris objects and multiple input MSs is not yet supported.")
                print("       Use docontsub=False")
                return False

            haveshifts, shiftsHz, shiftsChannels = haveShiftedSPWs(myviss, targetfielddicts[0]['spwids'], chanwidthtol)

            if haveshifts:
                thevislsrk = []
                for myvis in myviss:
                    for myspw in targetfielddicts[0]['spwids']:
                        thevislsrk.append(myvis+".lsrk.spw"+str(myspw))
                stext = printstrarray(thevislsrk, "thevislsrk")
                scriptfile.write(stext+"\n\n")

    if spwmap != []: # non-trivial spwmap: assume that this is to get the SPW IDs right for the archive
        stext = "# NOTE: the SPW IDs in the _image_names_ in this script use the numbering as in the original ASDM(s),\n"
        stext = stext+"#       not the numbering used in the MS(s) processed directly by this script.\n"
        stext = stext+"#       This is a requirement of the ALMA archive.\n"
        scriptfile.write(stext+"\n")

    # do contsub and concat if needed
    contsubinfile=""
    contsuboutfile=""
    usereindexedspwids=False
    usereindexedspwidsincontsub=False
    usereindexedfieldids=False

    if makecubes and docontsub:

        if len(myviss)>1:
            
            if haveshifts:
                print("\nNOTE: The SPWs in the different input MSs are shifted by more than "+str(chanwidthtol)+" chanwidths w.r.t. each other.")
                print("        Will add code to mstransform them before concatenation ...")
                print("        (You can override this using ISG parameter 'chanwidthtol'.)")

                thetnchan, thetwidth, thetstart =  largestCommonSPWsLSRK(myviss, targetfielddicts[0]['spwids'])

                for myspw in targetfielddicts[0]['spwids']:

                    print("New common grid for SPW "+str(myspw)+":")
                    print("  nchan = "+str(thetnchan[myspw])+", start ="+str(thetstart[myspw]/1E9)+" GHz, width = "+str(thetwidth[myspw]/1000.)+" kHz") 

                    mypardict = {'taskname': 'mstransform',
                                 'vis': 'VARmyvis',
                                 'outputvis': "VARmyvis+'.lsrk.spw"+str(myspw)+"'",
                                 'outframe': 'LSRK',
                                 'spw': str(myspw),
                                 'mode': 'frequency',
                                 'nchan': thetnchan[myspw],
                                 'width': str(thetwidth[myspw]/1000.)+'kHz',
                                 'start': str(thetstart[myspw]/1E9)+'GHz',
                                 'regridms': True,
                                 'datacolumn': 'data',
                                 'reindex': True}

                    usereindexedspwidsincontsub=True

                    if True in hascorrected:
                        mypardict['datacolumn'] = 'corrected'

                    stext = "for myvis in thevis:\n"
                    stext += mystepindent + "os.system('rm -rf '+myvis+'.lsrk.spw"+str(myspw)+"')\n"
                    stext += mystepindent + printtask(mypardict, mypardict['taskname'], mystepindent)
                    # write command
                    sfsdr.addReducScriptStep(scriptfile, mystepdict, 
                                          "Transform SPW "+str(myspw)+" for all MSs to common largest LSRK grid before concat", 
                                          stext, mystepindent)

                print("\nCreating concat command for mstransformed MSs ...")
                contsubinfile = 'concat.ms'

                mypardict = {'taskname': 'concat',
                             'vis': 'VARlist(thevislsrk)',
                             'concatvis': contsubinfile}
                #if not targetfielddicts[0]['ismosaic']:
                if True:
                    mypardict['copypointing'] = False

                stext = "os.system('rm -rf "+contsubinfile+"')\n"
                stext += printtask(mypardict, mypardict['taskname'])                
                stext += "\n"

                # write command
                sfsdr.addReducScriptStep(scriptfile, mystepdict, 
                                      "Concatenation of mstransformed MSs", 
                                      stext, mystepindent)

            else: # SPWs are already aligned within chanwidthtol

                print("\nCreating concat command ...")
                contsubinfile = 'concat.ms'

                theshifts = []
                for myshift in shiftsHz:
                    theshifts.append(shiftsHz[myshift])
                theshifts.append(1.) # final tolerance should not be smaller than 1 Hz
                myfreqtolHz = np.max(theshifts)

                mypardict = {'taskname': 'concat',
                             'vis': 'VARthevis',
                             'concatvis': contsubinfile,
                             'freqtol': str(round(myfreqtolHz*1.01))+'Hz'} # increase tol by 1% to avoid borderline effects
                if not targetfielddicts[0]['ismosaic']:
                    mypardict['copypointing'] = False

                stext = printtask(mypardict, mypardict['taskname'])                
                stext += "\n"

                # write command
                sfsdr.addReducScriptStep(scriptfile, mystepdict, 
                                      "Concatenation", 
                                      stext, mystepindent)

        else:
            contsubinfile = myviss[0]

        if targetfielddicts[0]['spwids'][0] > 11: # looks like the calibrated MS uses the original SPW IDs
            # uvcontsub will renumber these to consecutive IDs starting at 0
            usereindexedspwids=True
        if targetfielddicts[0]['ismosaic']:
            # since uvcontsub reindexes the fields, need to pay attention to the phase center
            usereindexedfieldids=True


    ### Imaging ###

    myimages = set([])        

    # create agg. bw. imaging commands for additional fields

    theintent_extension = thetargetintent[14:] # in the case of "OBSERVE_TARGET#ON_SOURCE" this will be "#ON_SOURCE"

    for myintent in additionalimg:
         adtlfielddicts = findfields(myviss[representativems], myintent+theintent_extension, myspwmap)

         if len(adtlfielddicts)==0:
             print("\nNOTE: Could not find any fields with intent "+myintent+theintent_extension+" in the given MSs.\n")
             continue
   
         for adtldict in adtlfielddicts:

             print("\nWorking on field "+str(adtldict['fieldname'])+"\n")

             print(adtldict)

             print("\nCreating aggregate bandwidth imaging command ...")

             try:
                 mypardict =  getimgpars(vis=myviss[representativems], 
                                         fieldname=str(adtldict['fieldname']), 
                                         intent=adtldict['intent'], 
                                         mode='mfs',
                                         isMosaic=adtldict['ismosaic'],
                                         isFullPol=adtldict['isfullpol'],
                                         field=adtldict['fieldids'], 
                                         spw=adtldict['spwids'],
                                         spwmap=adtldict['spwmap'])
             except exceptions.KeyboardInterrupt:
                 print("ERROR: "+str(sys.exc_info()))
                 return False
             except:
                 print("ERROR: "+str(sys.exc_info()))
                 casalog.post("ERROR: could not get imaging parameters. Will try to continue ...", 'WARN')
                 continue

             mypardict['vis'] = 'VARthevis'
             mypardict['threshold'] = '<your threshold here>'
             yourthresholdhere = True

             stext = "\nos.system('rm -rf "+mypardict['imagename']+"*')\n"

             stext += printtask(mypardict, mypardict['taskname'])
                 
             stext += "\n"

             # move to the standard names so the fits export picks up the right images
             if mypardict['nterms'] > 1:

                 stext += "\nos.system('mv "+mypardict['imagename']+".image.tt0.pbcor "+mypardict['imagename']+".image.pbcor')\n"
                 stext += "os.system('ln -sf "+mypardict['imagename']+".image.pbcor "+mypardict['imagename']+".image.tt0.pbcor')\n"
                 stext += "os.system('mv "+mypardict['imagename']+".pb.tt0 "+mypardict['imagename']+".pb')\n"
                 stext += "os.system('ln -sf "+mypardict['imagename']+".pb "+mypardict['imagename']+".pb.tt0')\n"

             # write command
             sfsdr.addReducScriptStep(scriptfile, mystepdict, 
                                   "Agg. bandwidth image for non-science target "+adtldict['fieldname']+" (intent "+myintent+"), spws "+str(adtldict['spwids']), 
                                   stext, mystepindent)
             # memorize image name for later fits export
             myimages.add(mypardict['imagename'])

    # call getimpars to obtain tclean parameters and write tclean commands for science target(s)

    mightconstrainchannels=False

    for tfdict in targetfielddicts:

        print("\nWorking on field "+str(tfdict['fieldname'])+"\n")

        print(tfdict)

        mymode = 'mfs'
        if docontsub:
            print("\nCreating continuum imaging command ...\n")
            mymode = 'cont'
        else:
            print("\nCreating aggregate bandwidth imaging command ...\n")
        
        try:
            mypardict =  getimgpars(vis=myviss[representativems], 
                                    fieldname=str(tfdict['fieldname']), 
                                    intent=tfdict['intent'], 
                                    mode=mymode,
                                    isEphem=tfdict['isephem'],
                                    isMosaic=tfdict['ismosaic'],
                                    isFullPol=tfdict['isfullpol'],
                                    field=tfdict['fieldids'], 
                                    spw=tfdict['spwids'],
                                    spwmap=tfdict['spwmap'])
        except exceptions.KeyboardInterrupt:
            print("ERROR: "+str(sys.exc_info()))
            return False
        except:
            print("ERROR: "+str(sys.exc_info()))
            casalog.post("ERROR: could not get imaging parameters. Will try to continue ...", 'WARN')
            continue

        mypardict['vis'] = 'VARthevis'

        if myreqchanwidth==None or not makecubes:
            mypardict['threshold'] = draft_threshold
            
        stext = "\nos.system('rm -rf "+mypardict['imagename']+"*')\n"

        stext += printtask(mypardict, mypardict['taskname'])

        stext += "\n"

        if docontsub:
            stext += "# NOTE: enter the continuum channel selection in the spw parameter!\n"

        # move the standard names so the fits export picks up the right images
        if mypardict['nterms'] > 1:
            stext += "\nos.system('mv "+mypardict['imagename']+".image.tt0.pbcor "+mypardict['imagename']+".image.pbcor')\n"
            stext += "os.system('ln -sf "+mypardict['imagename']+".image.pbcor "+mypardict['imagename']+".image.tt0.pbcor')\n"
            stext += "os.system('mv "+mypardict['imagename']+".pb.tt0 "+mypardict['imagename']+".pb')\n"
            stext += "os.system('ln -sf "+mypardict['imagename']+".pb "+mypardict['imagename']+".pb.tt0')\n"

        # write command
        if docontsub:
            sfsdr.addReducScriptStep(scriptfile, mystepdict, 
                                  "Continuum image for target "+tfdict['fieldname']+", spws "+str(tfdict['spwids']), 
                                  stext, mystepindent)
        else:
            sfsdr.addReducScriptStep(scriptfile, mystepdict, 
                                  "Agg. bandwidth image for target "+tfdict['fieldname']+", spws "+str(tfdict['spwids']), 
                                  stext, mystepindent)

        # memorize image name for later fits export
        myimages.add(mypardict['imagename'])


        if makecubes:

            if docontsub:

                print("\nCreating uvcontsub command ...")

                contsubspwids = tfdict['spwids'] 
                if usereindexedspwidsincontsub:
                    contsubspwids = []
                    for myspw in tfdict['spwids']:
                        contsubspwids.append(tfdict['spwids'].index(myspw))

                mypardict = getcontsubpars(contsubspwids, '<your channel selection here>')
                mypardict['vis'] = contsubinfile
                mypardict['field'] = str(tfdict['fieldname'])
                contsuboutfile = contsubinfile+'_'+mypardict['field']+'.contsub'
                    
                stext = printtask(mypardict, mypardict['taskname'])                
                stext += "\nos.system('rm -rf "+contsuboutfile+"')\n"
                stext += "os.system('mv "+contsubinfile+".contsub "+contsuboutfile+"')\n"
                stext += "\n"

                # write command
                sfsdr.addReducScriptStep(scriptfile, mystepdict, 
                                      "Continuum subtraction for field "+mypardict['field'], 
                                      stext, mystepindent)


            print("\nCreating cube imaging commands ...\n")

            tfdict['spwids'].sort()
            
            for myspw in tfdict['spwids']:

                try:
                    mypardict =  getimgpars(vis=myviss[representativems], 
                                            fieldname=str(tfdict['fieldname']), 
                                            intent=tfdict['intent'], 
                                            mode='cube',
                                            isEphem=tfdict['isephem'], 
                                            isMosaic=tfdict['ismosaic'],
                                            isFullPol=tfdict['isfullpol'],
                                            field=tfdict['fieldids'], 
                                            spw=[myspw],
                                            spwmap=[myspwmap[sciencespws.index(myspw)]])
                except exceptions.KeyboardInterrupt:
                    print("ERROR: "+str(sys.exc_info()))
                    break
                except:
                    print("ERROR: "+str(sys.exc_info()))
                    casalog.post("ERROR: could not get cube imaging parameters. Will try to continue ...", 'WARN')
                    continue

                if "start" in mypardict: # this image has a large number of channels
                    mightconstrainchannels=True

                if docontsub:
                    mypardict['vis']= contsuboutfile
                    if usereindexedspwids:
                        print("NOTE: using reindexed SPWs when imaging contsub cube but leaving original SPW in image name.")
                        mypardict['spw'] = str(tfdict['spwids'].index(myspw))
                    if usereindexedfieldids:
                        if tfdict['ismosaic'] and not tfdict['isephem']:
                            oldphasecenter = mypardict['phasecenter']
                            mypardict['phasecenter'] = tfdict['fieldids'].index(oldphasecenter)
                        # also: remove the comment from the field parameter
                        mypardict['field'] = str(tfdict['fieldname'])
                else:
                    mypardict['vis'] = 'VARthevis'

                if myreqchanwidth==None:
                    mypardict['threshold'] = "<estimate from requested continuum sensitiviy>" 
                    yourthresholdhere = True
                else:
                    mypardict['threshold'] = str(mydraft_threshold['value'])+mydraft_threshold['unit']
                    if mypardict['width'] == '':
                        if myreqchanwidth['unit'] == 'channel':
                            mypardict['width'] = myreqchanwidth['value']
                        else:
                            mypardict['width'] = str(myreqchanwidth['value'])+myreqchanwidth['unit']
                        
                mypardict['restfreq'] = "VARtherestfreqs["+str(myspw)+"]"

                stext = "os.system('rm -rf "+mypardict['imagename']+"*')\n"
                stext += printtask(mypardict, mypardict['taskname'])
                stext += "\n"

                # write command
                sfsdr.addReducScriptStep(scriptfile, mystepdict, 
                                      "Cube for target "+tfdict['fieldname']+", spw "+str(myspw), 
                                      stext, mystepindent)
                # memorize image name for later fits export
                myimages.add(mypardict['imagename'])

    # end for tfdict in targetfielddicts

    # add full polarisation analysis step if needed
    if addfullpolanalysis:
        print("\nCreating full polarisation RMS analysis commands ...")
        stext = printfullpolanalysis();
        sfsdr.addReducScriptStep(scriptfile, mystepdict, 
                              "Polarization rms analysis", 
                              stext, mystepindent)
        # add the polarisation angle and intensity images to the list to export
        presentimages = myimages.copy()
        for imname in presentimages:
            if ("_sci" in imname or "_polleak" in imname) and ("mfs.IQUV" in imname or "cont,IQUV" in imname):
                newname = imname.replace('IQUV', 'A')
                myimages.add(newname)
                newname = imname.replace('IQUV', 'P')
                myimages.add(newname)

    # final fits export step

    print("\nCreating exportfits commands ...")

    stext = ""
    if spwmap != []: # non-trivial spwmap: assume that this is to get the SPW IDs right for the archive
        stext = "# NOTE: the SPW IDs in the image names in this script use the numbering as in the original ASDM(s)\n\n"

    myimageslist = list(myimages)
    myimageslist.sort()
    stext = stext + printstrarray(myimageslist, "myimages")

    stext = stext + "for myimagebase in myimages:\n"

    mypardict = { 'taskname': 'exportfits',
                  'imagename': "VARmyimagebase+'.image.pbcor'",
                  'fitsimage': "VARmyimagebase+'.pbcor.fits'",
                  'overwrite': True }
    stext = stext +  mystepindent + printtask(mypardict, mypardict['taskname'], mystepindent)

    mypardict['imagename'] = "VARmyimagebase+'.pb'"
    mypardict['fitsimage'] = "VARmyimagebase+'.pb.fits'"

    stext = stext +  mystepindent + "if os.path.exists(myimagebase+'.pb'):\n"

    stext = stext + 2*mystepindent + printtask(mypardict, mypardict['taskname'], 2*mystepindent)

    # write command
    sfsdr.addReducScriptStep(scriptfile, mystepdict, 
                          "Export images to FITS format", 
                          stext, mystepindent)
 

    # write script header including isg version
    sfsdr.prependReducScriptHeader(scriptfile, mystepdict, "Created using "+version(), mystepindent)

    scriptfile.close()

    print("Script generation completed. Please find "+scriptname+" in the current directory.\n")
    if len(discrepantfields)>0:
        print
        print("For the following mosaic fields, the field ids did not agree between the input MS:")
        print("    "+str(discrepantfields))
        print("  This may mean that the mosaic coverage was incomplete in some of the MSs. Please check.")
        print
    if makecubes:
        print("NOTE that you need still need to edit the script:")
        if len(discrepantfields)>0:
            print("  - for mosaic fields "+str(discrepantfields)+" you may need to adjust the phasecenter")
            print("    since the discrepant field setup of the individual input MSs may have led to an")
            print("    incorrect phase center choice. To set it explicitly use the coordinate syntax,")
            print("    example: phasecenter = 'ICRS 05:02:02.3290 -069.33.45.547'")
        if yourthresholdhere:
            print("  - in some imaging commands, you need to set the threshold")
        if not haverestfreqs:
            print("  - You may need to edit the array \"therestfreqs\" to set the rest frequencies for the cubes of each SPW.")
            print("    In at least some cases, you did not provide a value on the command line and none could be extracted")
            print("    from the SOURCE table. In those cases, the center frequency was used (see log comments above).")
        print("  - You need to adjust the threshold for the aggregate bandwidth/continuum image(s).") 
        if docontsub:
            print("  - You need to edit the spw parameter in each continuum imaging command to select the continuum channels.")
            print("  - You need to edit the fitspw parameter in each uvcontsub command.")
        if mightconstrainchannels:
            print("  - There are cubes with a potentially large number of channels. In order to speed up processing,")
            print("    you might want to consider to set the start and nchan parameters to constrain them.")
        print
        print("Generally: scrutinise the code for each step before you execute it!")
        print("  There is no guarantee that the generator will always have done exactly the right thing.")
        print("Report bugs and suggestions for improvement on SCOPS-5183.")

    return True


def findfields(vis=None, intent='OBSERVE_TARGET#ON_SOURCE', spwmap=[]):

    """
    Will investigate the MS and return a _list_ of dictionaries with the following outline,
    one dictionary for each field name:

    {'fieldname': value, # e.g. 'NGC253'
     'intent': value,    # e.g. 'OBSERVE_TARGET#ON_SOURCE'
     'isfullpol': value, # True or False
     'isephem': value,   # True of False
     'ismosaic': value,  # True or False
     'fieldids': value,  # list of numerical field ids
     'spwids': value,    # list of numerical spw ids
     'spwmap': value}    # the spwmap for genImageName corresponding to spwids
    """
    rval = []

    print("findfields for intent "+str(intent))

    if vis==None:
        casalog.post("ERROR: findfields called with invalid vis parameter", 'WARN')
        return rval
    if type(intent)!=str:
        casalog.post("ERROR: findfields called with invalid intent. Must be str.", 'WARN')
        return rval
    if type(spwmap)!=list:
        casalog.post("ERROR: findfields called with invalid spwmap. Must be list.", 'WARN')
        return rval
        
    mymsmd = msmdtool()
    mytb = tbtool()

    try:
        mytb.open(vis+'/FIELD')
        ephemIDs = np.zeros(mytb.nrows()) - 1 # all ids == -1 by default
        if ('EPHEMERIS_ID' in mytb.colnames()):
            ephemIDs = mytb.getcol('EPHEMERIS_ID')
        mytb.close()

        mymsmd.open(vis)
        theintents = mymsmd.intents()
        if not intent in theintents:
            casalog.post("findfields intent '"+intent+"' not present in MS", 'WARN')
            mymsmd.close()
            return rval

        myfieldnames = list(mymsmd.fieldsforintent(intent, asnames=True))
        myuniquefieldnames = set(myfieldnames)

        for myfieldname in myuniquefieldnames:
            # determine list of field ids which have the given intent and name
            # at the same time check if an ephemeris is attached
            fieldids = []
            isephem = False
            intendedfieldids = list(mymsmd.fieldsforintent(intent, asnames=False))
            for myfieldid in list(mymsmd.fieldsforname(myfieldname)):
                if myfieldid in intendedfieldids:
                   fieldids.append(myfieldid)
                   if ephemIDs[myfieldid] >= 0:
                       isephem = True
                       print("Field "+str(myfieldid)+" has an ephemeris attached.")
 
            allspwids =  list(mymsmd.spwsforfield(myfieldname))
            intendedspwids = list(mymsmd.spwsforintent(intent))
            spwids = []
            scispwids = []
            myspwmap = []
            for myspw in intendedspwids:
                if mymsmd.nchan(myspw)>4:
                    scispwids.append(myspw)
                    if (myspw in allspwids):
                        spwids.append(myspw)
            if spwmap == []:
                myspwmap = spwids
            else: # find the corresponding original SPW ID in the input spwmap
                for myspw in spwids:
                    try:
                        myspwmap.append(spwmap[scispwids.index(myspw)])
                    except:
                        print("ERROR: "+str(sys.exc_info()))
                        casalog.post("      findfields called with invalid spwmap: "+str(spwmap), 'WARN')
                        return []

            # determine if full pol
            isfullpol = False
            for thespw in spwids:
                ddids = mymsmd.datadescids(spw=thespw)
                for ddid in ddids:
                    polid = mymsmd.polidfordatadesc(ddid)
                    corrtypes = list(mymsmd.corrtypesforpol(polid))
                    ctname = {'6':'RL', '7':'LR', '10':'XY', '11':'YX'}
                    for ct in [6,7,10,11]:
                        if corrtypes.count(ct)>0:
                            casalog.post("Found corr type "+ctname[str(ct)]+" for SPW "+str(thespw)+". This dataset is FULL POLARISATION",
                                         'WARN')
                            isfullpol = True
                            break
                    if isfullpol:
                        break
                if isfullpol:
                    break
            
            # determine if an ephemeris is attached

            ismosaic = (len(fieldids)>1)
        
            fielddict = {'fieldname': myfieldname,
                         'intent': intent,
                         'isfullpol': isfullpol,
                         'isephem': isephem,
                         'ismosaic': ismosaic,
                         'fieldids': fieldids,
                         'spwids': spwids,
                         'spwmap': myspwmap
                         }                
            rval.append(fielddict)

    except exceptions.KeyboardInterrupt:
        print("ERROR: "+str(sys.exc_info()))
        return []
    except:
        casalog.post("ERROR: in findfields"+str(sys.exc_info())+". Returned list may be incomplete.", 'WARN')
        mymsmd.close()
        return rval

    mymsmd.close()

    # to make this reproducible in different Python versions, sort the array
    # of field dicts in rval by the first field ID:
    fieldids = []
    for mydicts in rval:
        fieldids.append(mydicts['fieldids'][0])

    sortedindices = np.argsort(fieldids)
    sortedrval = []
    for myidx in sortedindices:
        sortedrval.append(rval[myidx])

    return sortedrval

def getimgpars(vis='', fieldname='', intent='', mode='', isMosaic=False, isEphem=False, isFullPol=False, field=[], spw=[], 
               spwmap=[], perchanweightdensity=False):

    """
    Generate image parameters to be added to the imaging script.
    Take fieldnames and other relevant parameters from output of findfields
    and generate a dictionary of all tclean parameters for each targetfield
    
    Input parameters and values:
      fieldname     = name of the field source. e.g. 'NGC253'.
      intent        = Observation intent (same as intent in listobs). e.g. 'OBSERVE_TARGET#ON_SOURCE'.
      mode          = imaging mode. 'mfs' or 'cube'.
      isMosaic      = if the target is mosaic or not. Boolean, default: False.
      isEphem       = if the target has an ephemeris attached. Boolean, default: False.
      isFullPol     = if the MS contains full polarisation data. Boolean, default: False
      field         = field IDs to image. List of numerical field ids.
      spw           = spectral windows to image. List of numerical spw ids. only int or list of int.
                      If you want to image specific channels from the spws, 
                      you have to edit the spw parameter of tclean manually.
      spwmap        = spwmap to be used in aU.genImageName
      perchanweightdensity = the setting of the perchanweightdensity parameter of tclean (CASA 5.6+)

      Output parameters and values:
      vis           = Input visibilities (.ms files)
      imagename     = Output image name. From aU.genImageName().
      field         = field IDs to image. List of numerical field ids.
      intent        = the intent selected in the input (only included if isMosaic==True)
      spw           = spectral windows to image. List of numerical spw ids. Int or Str.
      deconvolver   = 'hogbom' if intent == 'Target'; 'mtmfs' if intent == 'Calibration' or mode == mfs or cont and 
                      fractional bandwidth >= 10%; 'clarkstokes' for full polarisation
      imsize        = Obtained from au.pickCellSize
      cell          = Obtained from au.pickCellSize
      phasecenter   = Obtained from au.pickCellSize
      niter         = default to 100
      nchan         = default: -1
      weighting     = 'Briggs'
      robust        = default to 0.5
      nterms        = default to 1; if the fractional bandwidth is >= 10%, it is set to 2
      stokes        = I
      threshold     = Threshold at which to stop cleaning. Equal to requested RMS.  default=''. 
                      To be edited by data analyst, or to be taken as input parameter for 'generateImagingScript'.
      width         = channel width for spectral line imaging, default=''. 
                      To be edited by data analyst, or to be taken as input parameter for 'generateImagingScript'.
      start         = default to ''.
      restfreq      = Rest frequency of the output image. default=''. 
                      To be edited by data analyst, or to be taken as input parameter for 'generateImagingScript'.
      specmode      = Spectral definition mode. = mode (mfs/cube)
      gridder       = Gridding options. 'standard' or 'mosaic'
      pbcor         = always True
      outframe      = default to LSRK.
      taskname      = which task should be used. Options = ['tclean', 'clean'], default = 'tclean'

      All other tclean parameters left as default.
      
    """

    mymsmd = msmdtool()

    thevis = ''
    thefieldname = ''
    thefieldids = []
    thespw = []
    thespwmap = []
    theintent = ''
    theintents = ['OBSERVE_TARGET', 
                  'CALIBRATE_PHASE',
                  'CALIBRATE_BANDPASS',
                  'CALIBRATE_FLUX',
                  'OBSERVE_CHECK_SOURCE',
                  'CALIBRATE_POLARIZATION'
                  ]
    themodes = ['mfs', 'cube', 'cont']
    thespecmode = ''
    thegridder = ''
    thegridders = ['standard', 'mosaic']
    theimsize = ''
    thecellsize = ''
    thephasecenter = ''
    thethreshold = ''
    thewidth = ''
    therestfreq = ''
    theniter = 100
    thenchan = -1
    theweighting = 'briggs'
    therobust = 0.5
    thenterms = 1
    thestokes = 'I'
    theimagenames = []
    thedeconvolver = ''
    theoutframe = 'LSRK'
    thetaskname = 'tclean'
    thetasknames = ['tclean', 'clean']

    smallnumberofchannels=1000 # above this number, we will suggest to constrain the number of channels

    # Get calibrated measurement sets
    
    if vis=="" or type(vis)!=str:
          raise Exception("Error: invalid vis parameter. Must be non-empty str.")
    else:
        thevis = vis

    # Get target object name
    if fieldname != '':
        thefieldname = fieldname
    else:
        print("ERROR: fieldname is empty ")
        raise  Exception("ERROR: fieldname is empty ")

    # Get field ids
    if field != []:
        thefieldids = field
    else:
        print("ERROR: field ids left blank ")
        raise  Exception("ERROR: field ids left blank ")

    if type(isMosaic) != type(True):
        print("ERROR: invalid isMosaic option "+str(isMosaic))
        print("  Valid options are True, False")
        raise  Exception("ERROR: invalid isMosaic option ", isMosaic)

    thefinalfield = ""
    for myfield in thefieldids:
        thefinalfield += str(myfield)+','
    thefinalfield = thefinalfield[:len(thefinalfield)-1]
    if isMosaic:
        thefinalfield = "'"+thefieldname+"', # IDs from representative MS: '"+thefinalfield+"'"
    else:
        thefinalfield = thefieldname

    # Get spectral window ids
    if type(spw) != type(1) and type(spw) != type([]):
        print("Error: wrong type for parameter spw "+str(spw))
        print("Valid parameter types are integer or list of integers.")
        raise Exception("Error: wrong type for parameter spw ", spw)
    else:
        thespw = spw
        if type(thespw) != type([]):
            thespw = [thespw]

    sp = ""
    for mysp in thespw:
        sp += str(mysp)+','
    sp = sp[:len(sp)-1]

    if type(spwmap) != type([]) or len(spwmap)<len(thespw):
        print("Error: invalid spwmap "+spw(spwmap))
        raise Exception("Error: spwmap must be list at least as long as parameter spw ")
    else:
        thespwmap = spwmap

    # Get specmode

    if mode in themodes:
        thespecmode = mode
    else:
        print("ERROR: invalid specmode "+str(mode))
        print("  Valid entries are "+str(themodes))
        raise Exception("ERROR: invalid targettype "+str(mode))

    # for mode mfs, determine fractional bandwidth
    thefracbw = 0.
    if (thespecmode=='mfs' or thespecmode=='cont'):
        mymsmd.open(thevis)
        myspwbws = mymsmd.bandwidths(thespw)
        myfreqs = []
        for mysp in thespw:
            mycf = mymsmd.chanfreqs(mysp)
            myfreqs.append(min(mycf))
            myfreqs.append(max(mycf))

        mymsmd.close()

        mytotalbwhz = max(myfreqs) - min(myfreqs)
        thefracbw = mytotalbwhz * 2./(min(myfreqs)+max(myfreqs))

        print("SPWs "+str(thespw)+" span a total bandwidth of "+str(mytotalbwhz/1E9)+" GHz, fractional bandwidth "+str(thefracbw))

        if thefracbw >= 0.1 and not isFullPol:
            print("Will use mtmfs with nterms = 2.")
            thenterms = 2

    # Determine gridder
    if isMosaic:
        thegridder = 'mosaic'
    else:
        thegridder = 'standard'

    # Check intent
    # in case of full polarisation use clarkstokes instead of hogbom for cubes

    found=False
    for myintent in theintents:
        if myintent in intent:
            found=True
            break
    if found:
        theintent = intent
    else:
        print("ERROR: invalid intent: "+str(intent))
        print("  Valid entries start with one of the following strings: "+str(theintents))
        raise  Exception("ERROR: invalid intent "+str(intent))

    thepbcor = True # always True since tclean produces both images anyway when pbcor is True

    if 'OBSERVE_TARGET' in theintent:
        thedeconvolver = 'hogbom'
        if isFullPol:
            thedeconvolver = 'clarkstokes'
            thestokes = 'IQUV'
        elif thenterms==2:
            thedeconvolver = 'mtmfs'
        thetargettype = 'sci'
    elif 'CALIBRATE_POLARIZATION' in theintent:
        if isFullPol:
            thedeconvolver = 'clarkstokes'
            thestokes = 'IQUV'
        else:
            raise  Exception("ERROR: found intent CALIBRATED_POLARIZATION but data is not full polarization.")
        thenterms=1
        thetargettype = 'polleak'
    elif 'CALIBRATE_PHASE' in theintent:
        thedeconvolver = 'hogbom'
        if isFullPol:
            thedeconvolver = 'clarkstokes'
            thestokes = 'IQUV'
        elif thenterms==2:
            thedeconvolver = 'mtmfs'
        thetargettype = 'ph'
    elif 'CALIBRATE_BANDPASS' in theintent:
        thedeconvolver = 'hogbom'
        if isFullPol:
            thedeconvolver = 'clarkstokes'
            thestokes = 'IQUV'
        elif thenterms==2:
            thedeconvolver = 'mtmfs'
        thetargettype = 'bp'
    elif 'CALIBRATE_FLUX' in theintent:
        thedeconvolver = 'hogbom'
        thetargettype = 'amp'
        if isFullPol:
            thedeconvolver = 'clarkstokes'
            thestokes = 'IQUV'
        elif thenterms==2:
            thedeconvolver = 'mtmfs'
    elif 'OBSERVE_CHECK_SOURCE' in theintent:
        thedeconvolver = 'hogbom'
        if isFullPol:
            thedeconvolver = 'clarkstokes'
            thestokes = 'IQUV'
        elif thenterms==2:
            thedeconvolver = 'mtmfs'
        thetargettype = 'chk'

    else:
        raise  Exception("ERROR: internal problem: invalid intent "+theintent)        

        
    # Get the imagename
    thestokes = 'I'
    if isFullPol:
        thestokes = 'IQUV'

    theimagename = aU.genImageName(vis=thevis, 
                                   spw=thespw, 
                                   field=thefieldname, 
                                   imtype=thespecmode, 
                                   targettype=thetargettype,
                                   stokes=thestokes,
                                   spwmap=thespwmap)


    # Get optimum cell size, image size, and central field
    mypblevel=0.2 # the default of aU.pickCellSize
    if isMosaic:
        mypblevel=0.1 # to make sure the mosaic size is large enough

    try:
        thecellsize, theimsize, thephasecenter = aU.pickCellSize(vis=thevis, 
                                                                 #spw=int(thespw[0]), # not giving spw will use mean freq of all 
                                                                 intent=theintent, 
                                                                 imsize=True, 
                                                                 cellstring=True,
                                                                 pblevel=mypblevel,
                                                                 sourcename=thefieldname,
                                                                 verbose=False)
    except:
        casalog.post("ERROR: in call to aU.pickCellSize - "+str(sys.exc_info()), 'WARN')
        casalog.post("       Your version of the analysisUtils may be broken.", 'WARN')
        raise  Exception("ERROR: call to aU.pickCellSize failed.")


    # special treatment for ephemeris objects
    if isEphem:
        thephasecenter = 'TRACKFIELD'
        theoutframe = ''
        if thespecmode == 'cube':
            thespecmode = 'cubesource'
        
    # Write all parameters in tclean format to dictionary
    out_dict = {'vis': thevis,
                'imagename': theimagename,
                'field': thefinalfield,
                'spw': sp,
                'imsize': theimsize,
                'cell': thecellsize,
                'deconvolver': thedeconvolver,
                'gridder': thegridder,
                'threshold': thethreshold,
                'niter': theniter,
                'weighting': theweighting,
                'pbcor': thepbcor,
                'specmode': thespecmode,
                'robust': therobust,
                'stokes': thestokes,
                'outframe': theoutframe,
                'mask': '',
                'interactive': True,
                'taskname': thetaskname}

    if isMosaic or isEphem:
        out_dict['phasecenter'] = thephasecenter

    if isMosaic:
        out_dict['intent'] = theintent

    if thespecmode=='cube' or thespecmode=='cubesource': 

        if aU.getCasaVersion() > '5.5.0': # parameter perchanweightdensity only became available in 5.6
            if out_dict['weighting'] == 'briggs':
                out_dict['perchanweightdensity'] = perchanweightdensity

        out_dict['width'] = thewidth
        out_dict['nchan'] = thenchan
        out_dict['restfreq'] = therestfreq
        mymsmd.open(thevis)
        for mysp in thespw:
            mynchan = mymsmd.nchan(mysp)
            if mynchan>smallnumberofchannels: # in case the number of channels is high, provide the start and nchan parameters
                out_dict['start']= "'', # native number of channels > "+str(smallnumberofchannels)+" (not taking into account width parameter)"
                out_dict['nchan']= "-1, # use ms.cvelfreqs to check; possibly constrain start and nchan to speed up processing"
                break
            elif mynchan==128: # a TDM SPW
                out_dict['width'] = 1
                out_dict['start']= "'', # this is a TDM SPW, you might want to set start and nchan"
                out_dict['nchan']= "-1, # to avoid the flagged edge channels"
        mymsmd.close()
    else:
        out_dict['nterms'] = thenterms


    return out_dict

def getcontsubpars(spws='', fitspw=''):
    
    sp = ""
    for mysp in spws:
        sp += str(mysp)+',' 
    sp = sp[:len(sp)-1] 

    out_dict = { 'taskname': 'uvcontsub',
                 'vis': 'calibrated.ms',
                 'spw': sp,
                 'fitspw': fitspw,
                 'excludechans': False,
                 'combine': '',
                 'solint': 'int',
                 'fitorder': 1,
                 'want_cont': False}

    return out_dict


def printtask(pardict=None, taskstr="", addidnt=""):

    """
    Will take a dictionary generated by getimgpars and print(the necessary lines of Python code
    into the string which is then returned.
    Will return a string containing "ERROR" if unsuccessful.
    NOTE: if a string value is meant as a _variable_ name, it needs to be prepended with VAR, e.g. VARthevis
    """

    if type(pardict) != dict:
        return "ERROR"

    thekeys = list(pardict.keys())
    if len(thekeys)==0:
        return "ERROR"

    if ("taskname" in thekeys):
        thekeys.remove("taskname")
        if taskstr=="":
            taskstr = pardict("taskname")

    if type(taskstr)!=str or taskstr=="":
        return "ERROR"

    # predefined order for particular tasks

    parameterorder = {'tclean': ['vis',
                                 'imagename',
                                 'field',
                                 'intent',
                                 'phasecenter',
                                 'stokes',
                                 'spw',
                                 'outframe',
                                 'restfreq',
                                 'specmode',
                                 'nterms',
                                 'imsize',
                                 'cell',
                                 'deconvolver',
                                 'niter',
                                 'weighting',
                                 'robust',
                                 'mask',
                                 'gridder',
                                 'pbcor',
                                 'threshold',
                                 'width',
                                 'start',
                                 'nchan',
                                 'interactive'],
                      'exportfits': ['imagename',
                                     'fitsimage',
                                     'overwrite'],
                      'uvcontsub': ['vis',
                                    'field',
                                    'spw',
                                    'fitspw',
                                    'fitorder',
                                    'solint',
                                    'combine',
                                    'excludechans',
                                    'want_cont'],
                      'mstransform': ['vis',
                                      'outputvis',
                                      'outframe',
                                      'spw',
                                      'mode',
                                      'nchan',
                                      'width',
                                      'start',
                                      'regridms',
                                      'datacolumn']
                      }

    rval = taskstr+"("
    firstkey=''

    # determine order in which to print the parameters

    if taskstr in parameterorder: # we have a predefined order for this task
        originalkeys = thekeys
        thekeys = []
        for mykey in parameterorder[taskstr]:
            if mykey in originalkeys:
                thekeys.append(mykey)
                originalkeys.remove(mykey)
        originalkeys.sort()
        thekeys += originalkeys # parameters without predefined position are appended
        firstkey = thekeys[0]
    else: # use general rules for the order
        thekeys.sort()
        if 'vis' in thekeys: # if present, print vis first
            firstkey = 'vis'
        else:
            firstkey = thekeys[0]
            
    thekeys.remove(firstkey)

    rval += firstkey + " = "
    thevalue = pardict[firstkey]
    if type(thevalue) == str:
        if thevalue[:3] == "VAR": # this is meant as a variable name 
            rval += thevalue[3:] + ",\n"
        elif ", " in thevalue: # this is a string with comma already contained
            rval += thevalue + "\n"
        else:
            rval += "'"+thevalue+"',\n"
    else:
        rval += str(thevalue) + ",\n"       

    indnt = addidnt + (len(taskstr)+1)*" "

    thevalue=''
    for thekey in thekeys:
        rval += indnt + thekey + " = "
        thevalue = pardict[thekey]
        if type(thevalue) == str:
            if thevalue[:3] == "VAR": # this is meant as a variable name 
                rval += thevalue[3:] + ",\n"
            elif ", " in thevalue: # this is a string with comma already contained
                rval += thevalue + "\n"
            else:
                rval += "'"+thevalue+"',\n"
        else:
            rval += str(thevalue) + ",\n"       

    if (type(thevalue) == str) and (", " in thevalue):
        rval = rval[0:len(rval)-1] # remove last "\n"    
    else:
        rval = rval[0:len(rval)-2] # remove last ",\n"

    rval += "\n" + addidnt + len(taskstr)*" " + " )\n"

    return rval


def printstrarray(myarray, myname, startchar = '[', endchar = ']'):

    """
    Returns string containing the Python code lines to assign the list myarray
    to a variable names myname.
    """

    rval = myname+" = "+startchar
    idnt = len(rval)*" "
    for myelement in myarray:
        if not type(myelement) == str:
            casalog.post('ERROR: printstrarray can only print arrays of stings', 'SEVERE')
            return ''
        rval = rval+"'"+myelement+"',\n"+idnt
    rval = rval[0:len(rval)-len(",\n"+idnt)]
    rval = rval+endchar+"\n\n"
    return rval


def haveShiftedSPWs(viss=[], spwids=[], freqtolchanwid=0.1):

    """
    Return True if the SPWs given by spwids in the MSs given by viss
    do _not_ all have identical grids, otherwise False.
    As second return value, the maximum discrepancy (Hz) is given for each SPW.
    And as third return value, the same discrepancy in units of channel widths.
    """

    mymsmd = msmdtool()

    rval=False
    rval2 = {}
    rval3 = {}
    for myspw in spwids:
        rval2[myspw] = 0
        rval3[myspw] = 0

    if len(viss)<=1:
        return rval, rval2, rval3

    thegrids = []
    thechanwidth = []
    for i in range(0, len(viss)):
        thegrids.append({})
        thechanwidth.append({})
        try:
            mymsmd.open(viss[i])
            for myspw in spwids:
                thegrids[i][myspw] = mymsmd.chanfreqs(myspw)
                thechanwidth[i][myspw] = mymsmd.chanwidths(myspw)[0]
            mymsmd.close()
        except:
            print("ERROR: "+str(sys.exc_info()))
            casalog.post('ERROR when accessing '+viss[i], 'SEVERE')
            raise


    for i in range(1, len(viss)):
        for myspw in spwids:
            freqtolhz =  np.abs(thechanwidth[i][myspw] * freqtolchanwid)
            maxdiscr = np.max(np.abs(thegrids[i][myspw] - thegrids[0][myspw]))
            if maxdiscr> rval2[myspw]:
                rval2[myspw] = maxdiscr
                rval3[myspw] = maxdiscr/np.abs(thechanwidth[0][myspw]) 
            if not np.allclose(thegrids[i][myspw], thegrids[0][myspw], 0., freqtolhz, True):
                rval=True
                print("Grid of SPW "+str(myspw)+" in vis "+str(i)+" differs from that in vis 0 by > "+str(freqtolchanwid)+" * chanwidth = "+str(freqtolhz)+\
                      " Hz, max discr. == "+str(maxdiscr)+" Hz == "+str(rval3[myspw])+" channelwidths")
            else:
                print("Grid of SPW "+str(myspw)+" in vis "+str(i)+" differs from that in vis 0 by at most "+str(maxdiscr)+" Hz =="+str(rval3[myspw])+" channelwidths")


    return rval, rval2, rval3


def largestCommonSPWsLSRK(viss=[], spwids=[], fieldid=0):

    """
    Returns three dictionaries which give the nchan, width, and start values
    for the common grids for each science SPW.
    """

    rval_nchan = {}
    rval_width = {}
    rval_start = {}

    if viss==[] or spwids==[]:
        casalog.post('ERROR: called largestCommonSPWsLSRK with empty input', 'SEVERE')
        raise 
        
    myms = mstool()

    lsrkgrids = []
    lsrkchanwidth = []
    for i in range(0, len(viss)):

        print("Original LSRK (!) grids for "+str(viss[i]))

        lsrkgrids.append({})
        lsrkchanwidth.append({})
        try:
            myms.open(viss[i])
            for myspw in spwids:
                lsrkgrids[i][myspw] = myms.cvelfreqs(spwids=[myspw],
                                                     fieldids=[fieldid],
                                                     outframe='LSRK')

                thelen = len(lsrkgrids[i][myspw])                     
                if not thelen>1:
                    casalog.post('ERROR: SPW '+str(myspw)+' only has one channel. Cannot determine LSRK channel width.', 'SEVERE')
                    raise 
                    
                lsrkchanwidth[i][myspw] = np.max([np.ceil(np.abs(lsrkgrids[i][myspw][1] - lsrkgrids[i][myspw][0])*1.00002),
                                                  np.ceil(np.abs(lsrkgrids[i][myspw][thelen-1] - lsrkgrids[i][myspw][thelen-2])*1.00002)
                                                  ])

                print(str(myspw)+":  nchan = "+str(thelen)+", start = "+str(np.min(lsrkgrids[i][myspw])/1E9)+" GHz, width = "+str(lsrkchanwidth[i][myspw]/1000.)+" kHz")

            myms.close()
        except:
            print("ERROR: "+str(sys.exc_info()))
            casalog.post('ERROR when accessing '+viss[i], 'SEVERE')
            raise 

    for myspw in spwids:
        minupperedge = np.max(lsrkgrids[0][myspw]) # start value for search
        maxloweredge = np.min(lsrkgrids[0][myspw]) # start value for search
        maxwidth = lsrkchanwidth[0][myspw] # start value for search
        for i in range(1, len(lsrkgrids)):
            if np.max(lsrkgrids[i][myspw])<minupperedge:
                minupperedge = np.max(lsrkgrids[i][myspw])
            if np.min(lsrkgrids[i][myspw])>maxloweredge:
                maxloweredge = np.min(lsrkgrids[i][myspw])
            if lsrkchanwidth[i][myspw]>maxwidth:
                maxwidth = lsrkchanwidth[i][myspw]

        maxloweredge += maxwidth*0.1 # increase by one tenth channel width to avoid rounding errors 

        rval_width[myspw] = maxwidth
        rval_start[myspw] = maxloweredge
        if maxwidth>0.:
            rval_nchan[myspw] = int(np.floor((minupperedge - maxloweredge)/maxwidth))
        else:
            casalog.post('ERROR: found zero channel width for vis '+str(maxloweredgeindex)+', spw '+str(myspw), 'SEVERE')
            raise exceptions.ArithmeticError
            
    return rval_nchan, rval_width, rval_start

def printfullpolanalysis():

    """
    Create the text for the full polarisation RMS analysis step
    """

    rval = "import glob\n"
    rval += "polaCutoff = 100  # in sigmas of the residual RMS.\n"
    rval += "imagescicontIQUV = glob.glob('*_sci.*.cont.IQUV.manual.image.pbcor')\n"
    rval += "imagescimfsIQUV = glob.glob('*_sci.*.mfs.IQUV.manual.image.pbcor')\n"
    rval += "imagepolcalIQUV = glob.glob('*_polleak.*.mfs.IQUV.manual.image.pbcor')\n"
    rval += "stokescomps_todo = ['I','Q','U','V']\n"
    rval += "\n"
    rval += "print('  Putting I,Q,U,V components from IQUV images (sci cont, sci mfs, and polcal) into separate images ...')\n"
    rval += "sep_stokesc_files = []\n"
    rval += "for inimage in imagescicontIQUV+imagescimfsIQUV+imagepolcalIQUV:\n"
    rval += "  print('    '+inimage)\n"
    rval += "  for stokescomp in stokescomps_todo:\n"
    rval += "    outimage = inimage.replace('.IQUV.', '.'+stokescomp+'.')\n"
    rval += "    os.system('rm -rf '+outimage)\n"
    rval += "    immath(imagename=inimage, outfile=outimage, expr='IM0', stokes=stokescomp)\n"
    rval += "    sep_stokesc_files.append(outimage)\n"
    rval += "\n"
    rval += "print('  Computing pol fluxes and errors on polcal continuum images ...')\n"
    rval += "results = {}\n"
    rval += "for inimageIQUV in imagepolcalIQUV:\n"
    rval += "  results[inimageIQUV] = {}\n"
    rval += "  res = {}\n"
    rval += "  for stokescomp, stokeslbl in enumerate(stokescomps_todo):\n"
    rval += "    inimage = inimageIQUV.replace('.IQUV.', '.'+stokeslbl+'.')\n"
    rval += "    print('    Fitting Gaussian to '+inimage)\n"
    rval += "    myheader = imhead(inimage, mode='list')\n"
    rval += "    rasize=myheader['shape'][0]\n"
    rval += "    decsize=myheader['shape'][1]\n"
    rval += "    mybox=str(rasize*0.35)+', '+str(decsize*0.35)+', '+str(rasize*0.65)+', '+str(decsize*0.65)\n"
    rval += "    res[stokeslbl] = {}\n"
    rval += "    resout=imfit(imagename = inimage, box=mybox)\n"
    rval += "    if not resout['converged'][0]:\n"
    rval += "      print('      ERROR: Cannot fit gaussian in image {}'.format(inimage))\n"
    rval += "      print('             Maybe extended emission or image too noisy? You will need to deal with this image manually.')\n"
    rval += "      casalog.post( 'ERROR: Cannot fit gaussian in image '+inimage, 'WARN')\n"
    rval += "      res[stokeslbl]['flux']=0\n"
    rval += "      res[stokeslbl]['error']=0\n"
    rval += "    else:\n"
    rval += "      res[stokeslbl]['flux']=resout['results']['component0']['flux']['value'][stokescomp]\n"
    rval += "      res[stokeslbl]['error']=resout['results']['component0']['flux']['error'][stokescomp]\n"
    rval += "\n"
    rval += "  fluxQ, fluxU = res['Q']['flux'], res['U']['flux']\n"
    rval += "  errorQ, errorU = res['Q']['error'], res['U']['error']\n"
    rval += "  results[inimageIQUV]['fluxPI'] = sqrt(fluxQ**2+fluxU**2)\n"
    rval += "  fluxPI = results[inimageIQUV]['fluxPI']\n"
    rval += "  if fluxPI == 0.0:\n"
    rval += "    print('Warning: fluxPI==0.0')\n"
    rval += "    fluxPI = -1.0  # In lieu of NaN\n"
    rval += "  results[inimageIQUV]['errorPI'] = sqrt( (fluxQ*errorQ)**2 + (fluxU*errorU)**2 ) / fluxPI\n"
    rval += "  errorPI = results[inimageIQUV]['errorPI']\n"
    rval += "  results[inimageIQUV]['polAngle'] = 0.5 * degrees( atan2(fluxU,fluxQ) )\n"
    rval += "  results[inimageIQUV]['errPA'] = 0.5 * degrees( errorPI / fluxPI )\n"
    rval += "\n"        
    rval += "print('  Computing image RMS ...')\n"
    rval += "imagecontresidual = glob.glob('*_sci*.cont.IQUV.manual.residual')\n"
    rval += "imagemfsresidual = glob.glob('*_sci*.mfs.IQUV.manual.residual')\n"
    rval += "imagepolcalresidual = glob.glob('*_polleak*.mfs.IQUV.manual.residual')\n"
    rval += "for inimage in imagecontresidual+imagemfsresidual+imagepolcalresidual:\n"
    rval += "  print('    '+inimage)\n"
    rval += "  calstat=imstat(imagename=inimage, axes=[0,1])\n"
    rval += "  rms = calstat['rms']\n"
    rval += "  results[inimage] = {}\n"
    rval += "  results[inimage]['rms'] = rms\n"
    rval += "  results[inimage]['prms'] = (rms[1]**2. + rms[2]**2.)**0.5\n"
    rval += "print(results)"        
    rval += "\n"        
    rval += "print(' Creating polarization I and A images ...')\n"
    rval += "\n"        
    rval += "for inimage in imagescicontIQUV+imagescimfsIQUV+imagepolcalIQUV:\n"
    rval += "  print('    '+inimage)\n"
    rval += "  poliimage = inimage.replace('.IQUV.', '.P.')\n"
    rval += "  os.system('rm -rf '+poliimage)\n"
    rval += "  immath(outfile=poliimage,\n"
    rval += "     mode='poli',\n"
    rval += "     imagename = inimage,\n"
    rval += "     sigma='0.0Jy/beam')\n"
    rval += "\n"        
    rval += "  polaimage = inimage.replace('.IQUV.', '.A.')\n"
    rval += "  os.system('rm -rf '+polaimage)\n"
    rval += "  rmsimage = inimage.replace('.image.pbcor', '.residual')\n"
    rval += "  prms=results[rmsimage]['prms']\n"
    rval += "  immath(outfile=polaimage,\n"
    rval += "     mode='pola',\n"
    rval += "     imagename = inimage,\n"
    rval += "     polithresh='%.8fJy/beam'%(5.0*prms))\n"
    rval += "\n"        
    rval += "print('  Saving results to file pol_results.out ...')\n"
    rval += "with open('pol_results.out', 'w') as f:\n"
    rval += "  stdout_orig = sys.stdout\n"
    rval += "  sys.stdout = f\n"
    rval += "  print(results)\n"
    rval += "  sys.stdout = stdout_orig\n"
    rval += "\n"        

    return rval
