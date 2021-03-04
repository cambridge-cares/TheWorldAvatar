import sys
import numpy
import scipy
import pypower

from pypower.api import case9, ppoption, runpf, runopf, printpf
from numpy import array

#ppc assigned from text files (which can have their contents directly copied in from excell prodived it is all numbers and there is no missing information)
#13 21 13 (number of elements per bus/gen/branch)
#Also note that buses count from 0 here in python, but from 1 in matlab (this is a pretty important difference so keep this in mind). 
#Read in shtuffity shtuff from text files(names set at the start of the program if these need to be changed for some reason)
def readText(baseMVAName, busName, genName, branchName, splitCharacter, optimal, areasName, genCostName):
    ppc = {"version": '2'}
    #baseMVA
    # system MVA base
    file = open(baseMVAName, "r")
    singleLine = file.readline()
    singleLine = singleLine.strip()
    items = singleLine.split(splitCharacter)
    ppc["baseMVA"] = numpy.float(items[0])
    file.close()
    #bus (length)
    length = 0
    file = open(busName, "r")
    readLoop = 1
    while (readLoop):
        singleLine = file.readline()
        singleLine = singleLine.strip()
        if (len(singleLine) < 4):
            readLoop = 0
            file.close()
        else:
            length += 1
    ppc["bus"] = numpy.zeros((length, 13), dtype=numpy.float)
    #bus (values)
    # bus_i type Pd Qd Gs Bs area Vm Va baseKV zone Vmax Vmin
    file = open(busName, "r")
    readLoop = 0
    while (readLoop < length):
        singleLine = file.readline()
        singleLine = singleLine.strip()
        items = singleLine.split(splitCharacter)
        ppc["bus"][readLoop][0] = numpy.float(float(items[0])-1.0)
        valueLoop = 1
        while (valueLoop < 13):
            ppc["bus"][readLoop][valueLoop] = numpy.float(items[valueLoop])
            valueLoop += 1
        readLoop += 1
    file.close()
    #gen (length)
    length = 0
    file = open(genName, "r")
    readLoop = 1
    while (readLoop):
        singleLine = file.readline()
        singleLine = singleLine.strip()
        if (len(singleLine) < 4):
            readLoop = 0
            file.close()
        else:
            length += 1
    ppc["gen"] = numpy.zeros((length, 21), dtype=numpy.float)
    #gen (values)
    # bus, Pg, Qg, Qmax, Qmin, Vg, mBase, status, Pmax, Pmin, Pc1, Pc2,
    # Qc1min, Qc1max, Qc2min, Qc2max, ramp_agc, ramp_10, ramp_30, ramp_q, apf
    file = open(genName, "r")
    readLoop = 0
    while (readLoop < length):
        singleLine = file.readline()
        singleLine = singleLine.strip()
        items = singleLine.split(splitCharacter)
        ppc["gen"][readLoop][0] = numpy.float(float(items[0])-1.0)
        valueLoop = 1
        while (valueLoop < 21):
            ppc["gen"][readLoop][valueLoop] = numpy.float(items[valueLoop])
            valueLoop += 1
        readLoop += 1
    file.close()
    #branch (length)
    length = 0
    file = open(branchName, "r")
    readLoop = 1
    while (readLoop):
        singleLine = file.readline()
        singleLine = singleLine.strip()
        if (len(singleLine) < 4):
            readLoop = 0
            file.close()
        else:
            length += 1
    ppc["branch"] = numpy.zeros((length, 13), dtype=numpy.float)
    #branch (values)
    #fbus, tbus, r, x, b, rateA, rateB, rateC, ratio, angle, status, angmin, angmax
    file = open(branchName, "r")
    readLoop = 0
    while (readLoop < length):
        singleLine = file.readline()
        singleLine = singleLine.strip()
        items = singleLine.split(splitCharacter)
        ppc["branch"][readLoop][0] = numpy.float(float(items[0])-1.0)
        ppc["branch"][readLoop][1] = numpy.float(float(items[1])-1.0)
        valueLoop = 2
        while (valueLoop < 13):
            ppc["branch"][readLoop][valueLoop] = numpy.float(items[valueLoop])
            valueLoop += 1
        readLoop += 1
    file.close()
    
    if (optimal == 1):
        #Put in some values for now
        #EXAMPLE OF AREAS (currently unused)
        #ppc["areas"] = array([[1, 0]])
        #EXAMPLE OF GENCOST
        # 1 startup shutdown n x1 y1 ... xn yn
        # 2 startup shutdown n c(n-1) ... c0
        '''
        ppc["gencost"] = array([
            [2, 0, 0, 3, 0.02, 2, 0],
            [2, 0, 0, 3, 0.02, 2, 0],
            [2, 0, 0, 3, 0.02, 2, 0],
            [2, 0, 0, 3, 0.02, 2, 0],
            [2, 0, 0, 3, 0.02, 2, 0],
            [2, 0, 0, 3, 0.02, 2, 0],
            [2, 0, 0, 3, 0.02, 2, 0],
            [2, 0, 0, 3, 0.02, 2, 0],
            [2, 0, 0, 3, 0.02, 2, 0],
            [2, 0, 0, 3, 0.02, 2, 0],
            [2, 0, 0, 3, 0.02, 2, 0],
            [2, 0, 0, 3, 0.02, 2, 0],
            [2, 0, 0, 3, 0.02, 2, 0],
            [2, 0, 0, 3, 0.02, 2, 0]
        ])
        '''
        #read the number of lines (y), make an array y long where each value (x) is the length of the lines. Generate the y,x array. Read in values.
        file = open(genCostName, "r")
        readLoop = 1
        length = 0
        while (readLoop):
            singleLine = file.readline()
            singleLine = singleLine.strip()
            if (len(singleLine) < 4):
                readLoop = 0
                file.close()
            else:
                length += 1
        file.close()
        lengths = numpy.zeros((length), dtype=numpy.float)
        file = open(genCostName, "r")
        readLoop = 0
        while (readLoop > -1):
            singleLine = file.readline()
            singleLine = singleLine.strip()
            items = singleLine.split(splitCharacter)
            if (len(singleLine) < 4):
                readLoop = -1
                file.close()
            else:
                lengths[readLoop] = len(items)
                if ((readLoop > 0) and (lengths[readLoop] != lengths[readLoop - 1])):
                    print("genCost item lists must all have the same length.")
                readLoop += 1
        file.close()
        ppc["gencost"] = numpy.zeros((length, int(lengths[0])), dtype=numpy.float)
        file = open(genCostName, "r")
        readLoop = 0
        while (readLoop < length):
            singleLine = file.readline()
            singleLine = singleLine.strip()
            items = singleLine.split(splitCharacter)
            valueLoop = 0
            while (valueLoop < int(lengths[0])):
                ppc["gencost"][readLoop][valueLoop] = numpy.float(items[valueLoop])
                valueLoop += 1
            readLoop += 1
        file.close()
        #print(ppc["gencost"])
    return ppc

#Just shneaky standard function
def absDiff(num1, num2):
    return abs(abs(num1) - abs(num2))

def mainJAPowerFlow(baseMVAName, busName, genName, branchName, splitCharacter, outputBusName, outputBranchName, outputGenName, printOutput, optimal, areasName, genCostName, convergedOutputName):
    #Variables (by for testing)
    #baseMVAName = "baseMVA.txt"
    #busName = "bus.txt"
    #genName = "gen.txt"
    #branchName = "branch.txt"
    #splitCharacter = '	'
    #outputBusName = "outputBus.txt"
    #outputBranchName = "outputBranch.txt"
    #outputBranchName = "outputGen.txt"
    #printOutput = 0 #printOutput = 0 or 1, 0 for no stdout printed output, 1 if it is wanted. Note that both still output to the text files. 
    #optimal = 0 #optimal = 0 or 1, 0 for power flow, 1 for optimal power flow. NOTE: All following inputs are only used in optimal power flow, OPF, analysis (optimal = 1), but some values are still required as inputs, even if they are not used in the event of PF (optimal = 0). 
    #areasName = "areas.txt"
    #genCostName = "genCost.txt"
    #convergedOutputName = "outputStatus.txt", which will have three lines. The first will just be the inputs variables given (inputs to mainJAPowerFLow). The second of which will state "0" (if it did not converge) or "1" (if it converged), while the third line will state this in text as "Converged!" (if it converged) or "Diverged!" (if it did not). Note that the "'s are not printed, just to show the contents. Also note that the second and third lines show the same information in different formats. 
    
    #Assign ppc
    ppc = readText(baseMVAName, busName, genName, branchName, splitCharacter, optimal, areasName, genCostName)
    #ppc = casetest()
    
    #Set pf test type
    #ppopt = ppoption(PF_ALG=1) #Includes printing output (of standard pf)
    #ppopt = ppoption(OUT_ALL=0, VERBOSE=0) #These options prevent printing output (so prints to terminal if 1 and does not if 0)
    #ppopt = ppoption(PF_ALG=1, OUT_ALL=0, VERBOSE=0)
    if (printOutput == 1):
        ppopt = ppoption(OUT_ALL=1, VERBOSE=1)
    elif (printOutput == 0):
        ppopt = ppoption(OUT_ALL=0, VERBOSE=0)
    else:
        print("printOutput must be 0 or 1, 0 for no stdout printed output, and 1 if that is desired. Both still output to text files.")
    
    #Run pf or opf test
    if (optimal == 0): 
        r = runpf(ppc, ppopt)
    elif (optimal == 1):
        r = runopf(ppc, ppopt)
    else:
        print("optimal must be 0 or 1, 0 for pf and 1 for opf.")

    #Output Metadata, which outputs the input variables to the function as well as if the model converged as described in the convergedOutputName description above.
    out = open(convergedOutputName, 'w')
    out.write(baseMVAName + splitCharacter + busName + splitCharacter + genName + splitCharacter + branchName + splitCharacter + splitCharacter + splitCharacter + outputBusName + splitCharacter + outputBranchName + splitCharacter + outputGenName + splitCharacter + str(printOutput) + splitCharacter + str(optimal) + splitCharacter + areasName + splitCharacter + genCostName + splitCharacter + convergedOutputName + '\n')
    if (r['success'] != True):
        print("Did not converge.")
        out.write("0\n")
        out.write("Diverged!\n")
    else:
        print("Converged.")
        out.write("1\n")
        out.write("Converged!\n")
    out.close()
    
    #Now clear the output files (method of writing to them might be altered, so doing this is to be sure
    open(outputBusName, 'w').close()
    open(outputBranchName, 'w').close()
    open(outputGenName, 'w').close()
    
    #For Optimal Power Flow
    if (optimal == 1):
        #Establish lengths
        busCount = len(r['bus'])
        branchCount = len(r['branch'])
        genCount = len(r['gen'])
        #Find Generator Per Bus Output
        busGenP = numpy.zeros(busCount, dtype=numpy.float)
        busGenQ = numpy.zeros(busCount, dtype=numpy.float)
        f = open(outputGenName, 'w')
        i = 0
        while (i < genCount):
            #For Gen Output
            f.write(str(i+1) + splitCharacter + str(r['gen'][i][1]) + splitCharacter + str(r['gen'][i][2]) + '\n')
            #For Bus Output
            busGenP[int(r['gen'][i][0])] += r['gen'][i][1]
            busGenQ[int(r['gen'][i][0])] += r['gen'][i][2]
            i += 1
        f.close()
        
        #Print Bus Output
        f = open(outputBusName, 'w')
        i = 0
        while (i < busCount):
            f.write(str(i+1) + splitCharacter + str(r['bus'][i][7]) + splitCharacter + str(r['bus'][i][8]) + splitCharacter + str(busGenP[i]) + splitCharacter + str(busGenQ[i]) + splitCharacter + str(r['bus'][i][2]) + splitCharacter + str(r['bus'][i][3]) + '\n')
            i += 1
        f.close()
        
        #Print Branch Output
        f = open(outputBranchName, 'w')
        i = 0
        while (i < branchCount):
            PAve = (r['branch'][i][15] + r['branch'][i][13])/2.0
            QAve = (r['branch'][i][14] + r['branch'][i][16])/2.0
            SAve = numpy.sqrt(((PAve * PAve) + (QAve * QAve)))
            f.write(str(i+1) + splitCharacter + str(absDiff(r['branch'][i][15], r['branch'][i][13])) + splitCharacter + str(absDiff(r['branch'][i][16], r['branch'][i][14]))+ splitCharacter + str(PAve) + splitCharacter + str(QAve) + splitCharacter + str(SAve) + '\n')
            i += 1
        f.close()
    
    #For Standard Power Flow
    elif (optimal == 0):
        #Establish lengths
        busCount = len(r[0]['bus'])
        branchCount = len(r[0]['branch'])
        #print(branchCount)
        genCount = len(r[0]['gen'])
        #Find Generator Per Bus Output
        busGenP = numpy.zeros(busCount, dtype=numpy.float)
        busGenQ = numpy.zeros(busCount, dtype=numpy.float)
        f = open(outputGenName, 'w')
        i = 0
        while (i < genCount):
            #For Gen Output
            f.write(str(i+1) + splitCharacter + str(r[0]['gen'][i][1]) + splitCharacter + str(r[0]['gen'][i][2]) + '\n')
            #For Bus Output
            busGenP[int(r[0]['gen'][i][0])] += r[0]['gen'][i][1]
            busGenQ[int(r[0]['gen'][i][0])] += r[0]['gen'][i][2]
            i += 1
        f.close()
        
        #Print Bus Output
        f = open(outputBusName, 'w')
        i = 0
        while (i < busCount):
            f.write(str(i+1) + splitCharacter + str(r[0]['bus'][i][7]) + splitCharacter + str(r[0]['bus'][i][8]) + splitCharacter + str(busGenP[i]) + splitCharacter + str(busGenQ[i]) + splitCharacter + str(r[0]['bus'][i][2]) + splitCharacter + str(r[0]['bus'][i][3]) + '\n')
            i += 1
        f.close()
        
        #Print Branch Output
        f = open(outputBranchName, 'w')
        i = 0
        while (i < branchCount):
            #P, Q and S (average)
            PAve = (r[0]['branch'][i][15] + r[0]['branch'][i][13])/2.0
            QAve = (r[0]['branch'][i][14] + r[0]['branch'][i][16])/2.0
            SAve = numpy.sqrt(((PAve * PAve) + (QAve * QAve)))
            #Print P loss, Q loss, aveP, aveQ and aveS. 
            f.write(str(i+1) + splitCharacter + str(absDiff(r[0]['branch'][i][15], r[0]['branch'][i][13])) + splitCharacter + str(absDiff(r[0]['branch'][i][16], r[0]['branch'][i][14])) + splitCharacter + str(PAve) + splitCharacter + str(QAve) + splitCharacter + str(SAve) + '\n')
            i += 1
        f.close()
    else:
        print("optimal must be 0 or 1, 0 for pf and 1 for opf.")

#MAIN
#Run the main function here (all variables after the 'optimal' 0/1 are only needed for the optimal power flow analysis, but some unused input is still required). 
#mainJAPowerFlow("baseMVA.txt", "bus.txt", "gen.txt", "branch.txt", '	', "outputBusPF.txt", "outputBranchPF.txt", "outputGenPF.txt", 1, 0, "areas.txt", "genCost.txt", "outputStatus.txt")
#mainJAPowerFlow("baseMVA.txt", "bus.txt", "gen.txt", "branch.txt", '	', "outputBusOPF.txt", "outputBranchOPF.txt", "outputGenOPF.txt", 1, 1, "areas.txt", "genCost.txt", "outputStatus.txt")
#mainJAPowerFlow("baseMVA.txt", "bus.txt", "gen.txt", "branch.txt", '	', "outputBus.txt", "outputBranch.txt", "outputGen.txt", 0, 0, "areas.txt", "genCost.txt", "outputStatus.txt")

#To be run via Java using CMD.
if __name__ == "__main__":
    #For the separation character, as sending it directly can cause issues in how it is read a numerical input is instead used allowing you to select the separation character.
    #If it is set to 1 or 2 then the tab character will be selected as the splitCharacter (these should do the same thing, but if there's an issue the other can be tried).
    #Currently JPS (via Java) uses '\t', copying from excel uses '    ' and nothing uses the space (' ' - if it is set to 3) or comma (',' - if it is set to anything else, eg. 0). 
    if  (sys.argv[5] == "1"):
            spc = '\t'
    elif (sys.argv[5] == "2"):
            spc = '    '
    elif (sys.argv[5] == "3"):
            spc = ' '
    else:
            spc = ','
    mainJAPowerFlow(sys.argv[1],sys.argv[2],sys.argv[3] ,sys.argv[4], spc,sys.argv[6],sys.argv[7],sys.argv[8], int(sys.argv[9]), int(sys.argv[10]), sys.argv[11], sys.argv[12], sys.argv[13]) # Runs OPF
    sys.exit()

sys.exit()

########
#GENERAL NOTES:
#OPF equations need to be in equation for (type 2) due to PyPower restrictions. Modeling is done on this end for both PF and OPF, so now the software and ontology ends need to integrate this.
#PV Solar can now be considered along with other extra considerations. 
########

########
#Discussion Stuff
#NOTE ABOUT IMPORTS: sys is included in python2.7.13, then you must path to the folder as it is in C:, not users. Then use pip to install the compiled binaries for numpy and scipy (not using the general format, but by installing the pre-compiled versions). Finally, pypower can be installed in the normal way by pip.
#angles in and out are in degrees.
########
