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
        if (len(singleLine) < 3):
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
        if (len(singleLine) < 3):
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
        if (len(singleLine) < 3):
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
        #ppc["areas"] = array([[1, 0]])
        
        # 1 startup shutdown n x1 y1 ... xn yn
        # 2 startup shutdown n c(n-1) ... c0
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
    
    return ppc

#Just shneaky standard function
def absDiff(num1, num2):
    return abs(abs(num1) - abs(num2))

def mainJAPowerFlow(baseMVAName, busName, genName, branchName, splitCharacter, outputBusName, outputBranchName, outputGenName, optimal, printOutput, areasName, genCostName):
    #Variables (by for testing)
    #baseMVAName = "baseMVA.txt"
    #busName = "bus.txt"
    #genName = "gen.txt"
    #branchName = "branch.txt"
    #splitCharacter = '	'
    #outputBusName = "outputBus.txt"
    #outputBranchName = "outputBranch.txt"
    #outputBranchName = "outputGen.txt"
    #optimal = 0 #optimal = 0 or 1, 0 for power flow, 1 for optimal power flow.
    #printOutput = 0 #printOutput = 0 or 1, 0 for no stdout printed output, 1 if it is wanted. Note that both still output to the text files. 
    #areasName = "areas.txt"
    #genCostName = "genCost.txt" 
    
    #Assign ppc
    ppc = readText(baseMVAName, busName, genName, branchName, splitCharacter, optimal, areasName, genCostName)
    #ppc = casetest()
    
    #Set pf test type
    #ppopt = ppoption(PF_ALG=1) #Includes printing output (of standard pf)
    #ppopt = ppoption(OUT_ALL=0, VERBOSE=0) #These options prevent printing output
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
            f.write(str(i+1) + splitCharacter + str(absDiff(r['branch'][i][15], r['branch'][i][13])) + splitCharacter + str(absDiff(r['branch'][i][16], r['branch'][i][14])) + '\n')
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
            f.write(str(i+1) + splitCharacter + str(absDiff(r[0]['branch'][i][15], r[0]['branch'][i][13])) + splitCharacter + str(absDiff(r[0]['branch'][i][16], r[0]['branch'][i][14])) + '\n')
            i += 1
        f.close()
    else:
        print("optimal must be 0 or 1, 0 for pf and 1 for opf.")

#MAIN
#Run the main function here
mainJAPowerFlow("baseMVA.txt", "bus.txt", "gen.txt", "branch.txt", '	', "outputBusPF.txt", "outputBranchPF.txt", "outputGenPF.txt", 0, 0, "areas.txt", "genCost.txt")
mainJAPowerFlow("baseMVA.txt", "bus.txt", "gen.txt", "branch.txt", '	', "outputBusOPF.txt", "outputBranchOPF.txt", "outputGenOPF.txt", 1, 0, "areas.txt", "genCost.txt")

#mainJAPowerFlow("baseMVA.txt", "bus.txt", "gen.txt", "branch.txt", '	', "outputBus.txt", "outputBranch.txt", "outputGen.txt", 0, 0, "areas.txt", "genCost.txt")
sys.exit()

########
#GENERAL NOTES:
#automatically prints the big formatted table,
#(can be supressed by commenting out line 291 in runpf.py, or using the line above
#- I think, if something goes wrong try the commenting way to be sure with the original line)
#eg. if you wanted to try opf then this should work as another method if you can't find a non-printing method
########

########
#Discussion Stuff
#Find the book in question and check the example
#
#COMPARE pypower result to matpower & Remy's MatLab solver. Note, for this example my NR and PyPower have the same result so I might be putting the infomation into Remy's solver incorrectly??
#It works!
#
#If it is found to be useful, then try on the full system
#Figure out what output method is desired by J-Park and intigrate accordingly
#
#NOTE ABOUT IMPORTS: sys is included in python2.7.13, then you must path to the folder as it is in C:, not users. Then use pip to install the compiled binaries for numpy and scipy (not using the general format, but by installing the pre-compiled versions). Finally, pypower can be installed in the normal way by pip.
#angles in and out are in degrees.
########
