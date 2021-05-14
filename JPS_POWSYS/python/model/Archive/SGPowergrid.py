# coding: iso-8859-1
import sys
import numpy
import scipy
import pypower
#import xlsxwriter

from pypower.api import ppoption, runpf, runopf, printpf
from numpy import array

#ppc assigned from text files (which can have their contents directly copied in from excel provided it is all numbers and there is no missing information)
#13 21 13 (number of elements per bus/gen/branch)
#Also note that buses count from 0 here in python, but from 1 in matlab (this is a pretty important difference so keep this in mind). 
#Read in shtuffity shtuff from text files(names set at the start of the program if these need to be changed for some reason)
def readText(baseMVAName, busName, genName, branchName, splitCharacter, optimal, areasName, genCostName):
    ppc = {"version": '2'}
    
# 1) baseMVA
# system MVA base
    file = open(baseMVAName, "r")
    singleLine = file.readline()
    singleLine = singleLine.strip()
    items = singleLine.split(splitCharacter)
    ppc["baseMVA"] = numpy.float(items[0])
    file.close()
    
# 2a) bus (length)
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
    
# 2b) bus (values)
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
    
# 3a) gen (length)
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
    
# 3b) gen (values)
# bus, Pg, Qg, Qmax, Qmin, Vg, mBase, status, Pmax, Pmin, Pc1, Pc2 Qc1min, Qc1max, Qc2min, Qc2max, ramp_agc, ramp_10, ramp_30, ramp_q, apf
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
    
# 4a) branch (length)
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
    
# 4b) branch (values)
# fbus, tbus, r, x, b, rateA, rateB, rateC, ratio, angle, status, angmin, angmax
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
        
        # 5a) gencost (lengths)
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
        
        # 5b) gencost (values)
         # 1 startup shutdown n x1 y1 ... xn yn
        # 2 startup shutdown n c(n-1) ... c0
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

# Standard Function
def absDiff(num1, num2):
    return abs(abs(num1) - abs(num2))

def mainJAPowerFlow(baseMVAName, busName, genName, branchName, splitCharacter, outputBusName, outputBranchName, outputGenName, printOutput, optimal, areasName, genCostName):
    # Variables (by for testing)
    # baseMVAName = "baseMVA.txt"
    # busName = "bus.txt"
    # genName = "gen.txt"
    # branchName = "branch.txt"
    # splitCharacter = '    '
    # outputBusName = "outputBus.txt"
    # outputBranchName = "outputBranch.txt"
    # outputBranchName = "outputGen.txt"
    
    # printOutput = 0 
    ##### printOutput = 0 or 1, 0 for no stdout printed output, 1 if it is wanted.
    ##### Note that both still output to the text files.
    
    # optimal = 0 
    ##### optimal = 0 or 1, 0 for power flow, 1 for optimal power flow. 
    ##### NOTE: All following inputs are only used in optimal power flow, OPF, analysis (optimal = 1), 
    ##### but some values are still required as inputs, even if they are not used in the event of PF (optimal = 0). 
    
    # areasName = "areas.txt"
    # genCostName = "genCost.txt" 
    

    # Assign ppc
    ppc = readText(baseMVAName, busName, genName, branchName, splitCharacter, optimal, areasName, genCostName)
    #ppc = casetest()
    

    # Set pf test type
    # ppopt = ppoption(PF_ALG=1)                ---> Power Flow algorithm: 1- NR Method
    # ppopt = ppoption(OUT_ALL=0, VERBOSE=0)    ---> These options prevent printing output
    # ppopt = ppoption(PF_ALG=1, OUT_ALL=0, VERBOSE=0)


    if (printOutput == 1):
        ppopt = ppoption(OUT_ALL=1, VERBOSE=1)
    elif (printOutput == 0):
        ppopt = ppoption(OUT_ALL=0, VERBOSE=0)
    else:
        print("printOutput must be 0 or 1, 0 for no stdout printed output, and 1 if that is desired. Both still output to text files.")
    
    # (A) --pf_alg = PF_ALG  # power flow algorithm : 
    # 1 - Newton’s method, 
    # 2 - FastDecoupled (XB version), 
    # 3 - Fast-Decoupled (BX
    # version), 
    # 4 - Gauss Seidel 
    # [default: 1]

    # (B) --verbose = VERBOSE # amount of progress info printed: 
    # 0 - print no progress info, 
    # 1 - print a little progress info, 
    # 2 - print a lot of progress info, 
    # 3 - print all progress info
    # [default: 1]

    # (C) --out_all = OUT_ALL # controls printing of results: 
    # -1 - individual flags control what prints, 
    # 0 - don’t print anything
    # (overrides individual flags, except OUT_RAW), 
    # 1 - print everything (overrides individual flags, except OUT_RAW) 
    # [default: -1]


    # Run pf or opf test
    if (optimal == 0): 
        r = runpf(ppc, ppopt)
    elif (optimal == 1):
        r = runopf(ppc, ppopt)
    else:
        print("optimal must be 0 or 1, 0 for pf and 1 for opf.")
    

# Check if voltage constraints are met?
# Check if solution converged? 
    if (optimal == 1):
    
        
        genCount = len(r['gen']) #check no. of generators = 16
        busCount = len(r['bus']) #check no. of buses = 15
        branchCount = len(r['branch']) #check no. of branches = 25
        
        print("\nNo. of gen= ", genCount)
        print("\nbus= ", busCount)
        print("\nbranch=", branchCount)

        busGenP = numpy.zeros(genCount, dtype=numpy.float) #create a zero column vector (16x1) for P generation
        busLoadP = numpy.zeros(busCount, dtype=numpy.float) #create a zero column vector (15x1) for P load
        branchLoss = numpy.zeros(branchCount, dtype=numpy.float) #create a zero column vector (25x1) for branch losses

        
        i=0
        while (i< genCount):
            
            #busGenP[int(r['gen'][i][1])] += r['gen'][i][1]
            busGenP[i] += r['gen'][i][1]
            i += 1
        
        j=0
        while (j < busCount):
            busLoadP[j] += r['bus'][j][2]
            j += 1
        k=0
        while (k<branchCount):
            branchLoss[k] += absDiff(r['branch'][k][15],r['branch'][k][13])
            k += 1
        
        
        

        # print("\nReal power generated= \n",busGenP)
        # print("\nReal power demand= \n", busLoadP)
        # print ("\nBranch Losses= \n", branchLoss)


        sumGen = round(busGenP.sum(),2) # round off up to 2 decimal points.
        sumLoad = round(busLoadP.sum(),2)
        sumLoss = round(branchLoss.sum(),2)

        print("\n")

        
        print("\nTotal Real Power Generation= ", sumGen)
        print("\nTotal Real Power Load= ", sumLoad)
        print("\nTotal Real Power Losses= ", sumLoss)
        print("\n")

        


            # Clears the output files (method of writing to them might be altered, so doing this is to be sure
        open(outputBusName, 'w').close()
        open(outputBranchName, 'w').close()
        open(outputGenName, 'w').close()

            # For Optimal Power Flow

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
                #Print Gen Output
            f.write(str(i+1) + splitCharacter + str(r['gen'][i][1]) + splitCharacter + str(r['gen'][i][2]) + '\n')
                #For Bus Output --> assign values for busGenP and busGenQ
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
            #P, Q and S (average)
            PAve = (r['branch'][i][15] + r['branch'][i][13])/2.0
            QAve = (r['branch'][i][14] + r['branch'][i][16])/2.0
            SAve = numpy.sqrt(((PAve * PAve) + (QAve * QAve)))
            f.write(str(i+1) + splitCharacter + str(absDiff(r['branch'][i][15], r['branch'][i][13])) + splitCharacter + str(absDiff(r['branch'][i][16], r['branch'][i][14]))+ splitCharacter + str(PAve) + splitCharacter + str(QAve) + splitCharacter + str(SAve) + '\n')
            i += 1
        f.close()
        


    




    # # For Power Flow
    # elif (optimal == 0):
    #     #Establish lengths
    #     busCount = len(r[0]['bus'])
    #     branchCount = len(r[0]['branch'])
    #     #print(branchCount)
    #     genCount = len(r[0]['gen'])
    #     #Find Generator Per Bus Output
    #     busGenP = numpy.zeros(busCount, dtype=numpy.float)
    #     busGenQ = numpy.zeros(busCount, dtype=numpy.float)
    #     f = open(outputGenName, 'w')
    #     i = 0
    #     while (i < genCount):
    #         #For Gen Output
    #         f.write(str(i+1) + splitCharacter + str(r[0]['gen'][i][1]) + splitCharacter + str(r[0]['gen'][i][2]) + '\n')
    #         #For Bus Output
    #         busGenP[int(r[0]['gen'][i][0])] += r[0]['gen'][i][1]
    #         busGenQ[int(r[0]['gen'][i][0])] += r[0]['gen'][i][2]
    #         i += 1
    #     f.close()
        
    #     #Print Bus Output
    #     f = open(outputBusName, 'w')
    #     i = 0
    #     while (i < busCount):
    #         f.write(str(i+1) + splitCharacter + str(r[0]['bus'][i][7]) + splitCharacter + str(r[0]['bus'][i][8]) + splitCharacter + str(busGenP[i]) + splitCharacter + str(busGenQ[i]) + splitCharacter + str(r[0]['bus'][i][2]) + splitCharacter + str(r[0]['bus'][i][3]) + '\n')
    #         i += 1
    #     f.close()
        
    #     #Print Branch Output
    #     f = open(outputBranchName, 'w')
    #     i = 0
    #     while (i < branchCount):
    #         #P, Q and S (average)
    #         PAve = (r[0]['branch'][i][15] + r[0]['branch'][i][13])/2.0
    #         QAve = (r[0]['branch'][i][14] + r[0]['branch'][i][16])/2.0
    #         SAve = numpy.sqrt(((PAve * PAve) + (QAve * QAve)))
    #         #Print P loss, Q loss, aveP, aveQ and aveS. 
    #         f.write(str(i+1) + splitCharacter + str(absDiff(r[0]['branch'][i][15], r[0]['branch'][i][13])) + splitCharacter + str(absDiff(r[0]['branch'][i][16], r[0]['branch'][i][14])) + splitCharacter + str(PAve) + splitCharacter + str(QAve) + splitCharacter + str(SAve) + '\n')
    #         i += 1
    #     f.close()
    # else:
    #     print("optimal must be 0 or 1, 0 for pf and 1 for opf.")

#MAIN
# Run the main function here (all variables after the 'optimal' 0/1 are only needed for the optimal power flow analysis, but some unused input is still required). 
# mainJAPowerFlow("baseMVA.txt", "bus.txt", "gen.txt", "branch.txt", '    ', "outputBusPF.txt", "outputBranchPF.txt", "outputGenPF.txt", 1, 0, "areas.txt", "genCost.txt") # Runs PF
mainJAPowerFlow("baseMVA.txt", "bus.txt", "gen.txt", "branch.txt", '\t', "outputBusOPF.txt", "outputBranchOPF.txt", "outputGenOPF.txt", 1, 1, "areas.txt", "genCost.txt") # Runs OPF
# mainJAPowerFlow("baseMVA.txt", "bus.txt", "gen.txt", "branch.txt", '    ', "outputBus.txt", "outputBranch.txt", "outputGen.txt", 0, 0, "areas.txt", "genCost.txt")
sys.exit()

########
# GENERAL NOTES:
# OPF equations need to be in equation for (type 2) due to PyPower restrictions. Modelling is done on this end for both PF and OPF, so now the software and ontology ends need to integrate this.
# PV Solar can now be considered along with other extra considerations. 
########

########
#NOTE - ABOUT IMPORTS: 
# sys is included in python2.7.13, then you must path to the folder as it is in C:, not users. 
# Then use pip to install the compiled binaries for numpy and scipy (not using the general format, but by installing the pre-compiled versions). 
# Finally, pypower can be installed in the normal way by pip.
# angles in and out are in degrees.
########

