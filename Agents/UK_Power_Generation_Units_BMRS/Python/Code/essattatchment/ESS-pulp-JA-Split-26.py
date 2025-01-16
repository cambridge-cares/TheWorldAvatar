import sys
import numpy
import scipy
import pulp
#from pulp import *

#Makes the string of a number, such that is has a set number of characters (charLen). 
#This way numerical order and dictionary order are the same, due to the preceeding zeroes.
def zeroString(i, charLen):
    a = str(i) #i is the int from the for loop used, so need this in string form to use as a dictionary index
    while (len(a) < charLen):
            a = ("0" + a)
    return a #by putting a set amount of '0's (zeroes) before the first number the alphabetical and numerical orders are the same (otherwise 2 comes after 11 as 2 is alphabetically after 1, even though 11>2, while 02 and 11 works better as 0 comes before 11 and 2<11).
#for this reason every dictionary entry needs the same string length (eg. 001, 024), which is set by charLen to be based off the length of L, such that the string length (+1 to be safe) used is not smaller then the number of digits,
#so if L is of length 1-9, then charLen is 2, if L is of length 10-99, then charLen is 3, etc...

#Takes the efficiency measurements (USED ONLY FOR FINAL SOC CALCULATION & OBJECTIVE CHECK (from print/debug option), NOT IN LINEAR PROGRAMMING SETUP)
def efficiency(x, chargeEff, dischargeEff):
    xEff = x
    if (x < 0):
        #Charging
        xEff = chargeEff * x
    elif (x > 0):
        #Discharging
        xEff = dischargeEff  * x
    return xEff

#Function to use buying and selling retail price (USED ONLY FOR OBJECTIVE CHECK (from print/debug option), NOT IN LINEAR PROGRAMMING SETUP)
def price(diff, Cb, Cs, i):
    #Diff is the exchange with the grid
    C = 0
    if (diff > 0):
        C = Cb[i] * diff
    elif (diff < 0):
        C = Cs[i] * diff
    return C

#solver function
def ESSpulpJA(L, S, Cb, Cs, capacity, chargeEff, dischargeEff, Xmin, Xmax, SOCmin, SOCmax, SOC0, SOCend, gridExport, gridImport, automaticGridImportBasedOnLandS, canChargeESSFromGrid, includeCurtailment, curtailments, OutputFlow0SOC1, printOutput):
    #Numerical to Dics Conversion Setup#
    charLen = len(str(len(L))) + 2 #used to define the dictonary to matrix conversion required by this library which therefore requires preceeding '0's to ensure numberical and alphabetical order are the same
    #+2 just to be safe there's enough (+1 would be required to ensure there is enough to double the vars for +ve and -ve, the extra 0 is just to be safe, so should always start with at least one 0. 
    
    #% to fraction Conversions#
    chargeEff = chargeEff/100.0 #Convert from % to fraction
    dischargeEff = dischargeEff/100.0 #Convert from % to fraction
    
    #Capacity Conversions, by changing SOCmin, SOCmax, SOC0 and SOCend from % to decimal (/100) and by multiplying them by capacity#
    SOCmin = (SOCmin * capacity / 100.0)
    SOCmax = (SOCmax * capacity / 100.0)
    SOC0 = (SOC0 * capacity / 100.0)
    SOCend = (SOCend * capacity / 100.0)
    
    #Length Standardisation#
    #This simulation requires LISTS: L, S, Cb and Cs to all have the same number of elements. The definitive length used is the length of the L array.
    #Usually the others mentioned here should be the same length, but what if a different number of elements was used? We will make all these standardised just to be sure.
    #For Cb and Cs we will take the first element given and use it for the entire length (effectively making prices constant, also meaning that if a single value was given, that the code still work on that too).
    #Firstly, if a single float / int, etc... value is given, first convert it to a list of 1 element, or else even things like len won't work. 
    if (type(Cb) != list):
        Cb = [Cb]
    while (len(L) > len(Cb)):
        Cb.append(Cb[0])
    if (type(Cs) != list):
        Cs = [Cs]
    while (len(L) > len(Cs)):
        Cs.append(Cs[0])
    #For S we will make an array where all elements constant, assuming no solar (this also means that if a value of 0 was given at the start, that the code will still work on that too, setting all to 0).
    #So the same thing here as for Cb and Cs, just wanted to also note that this means that this method means putting in a 0 will work. 
    if (type(S) != list):
        S = [S]
    while(len(L) > len(S)):
        S.append(S[0])
    
    #Automatically Set Grid Import Limit to max power imported if no ESS, thus meaning an ESS isn't allowed to creater a higher peak demand:
    if (automaticGridImportBasedOnLandS == 1):
        grid = []
        for i in range(len(L)):
            grid.append(L[i] - S[i])
        if (max(grid) < gridImport):
            gridImport = max(grid)
    
    #MAIN PROGRAM#
    dispatch_prob = pulp.LpProblem("ESSPulpJA") #LP minimizes by default
    
    #PowerFromESS
    #Initiates variables with ESS charging / discharging constraints
    #theGrid (starts with 't' so alphabetically after pos and neg usage for convenience) 
    theGrid_pos_times = ["" for i in range(len(L))]
    theGrid_neg_times = ["" for i in range(len(L))]
    #Flow
    pos_times = ["" for i in range(len(L))]
    neg_times = ["" for i in range(len(L))]
    #Curtailments (output, input curtailments and exports are summed to make the generated amounts)
    cur_times = ["" for i in range(len(L))]

    for i in range(len(L)):
        theGrid_pos_times[i] = (zeroString(i, charLen))
        theGrid_neg_times[i] = (zeroString(i, charLen))
        pos_times[i] = (zeroString(i, charLen))
        neg_times[i] = (zeroString(i, charLen))
        cur_times[i] = (zeroString(i, charLen))
    
    #Sets gridImport and gridExport (grid inflow and outflow limits)
    tGpos = pulp.LpVariable.dicts('theGridPos', theGrid_pos_times, lowBound = 0, upBound = gridImport, cat = 'Continuous')
    absGridExport = abs(gridExport)
    tGneg = pulp.LpVariable.dicts('theGridNeg', theGrid_neg_times, lowBound = 0, upBound = absGridExport, cat = 'Continuous')
    #Sets Xmax and Xmin (power inflow and outflow limits)
    Ppos = pulp.LpVariable.dicts('pos', pos_times, lowBound = 0, upBound = Xmax, cat = 'Continuous')
    absXmin = abs(Xmin)
    Pneg = pulp.LpVariable.dicts('neg', neg_times, lowBound = 0, upBound = absXmin, cat = 'Continuous')
    #Sets curtailment
    tCur = pulp.LpVariable.dicts('tCurtailment', cur_times, lowBound = 0, upBound = max(S), cat = 'Continuous')
    
    #Objective Function
    #Initialises objective function (sums costs of each period) Sum net power consumption by selling price if exporting, or buying price if importing. 
    #dispatch_prob += sum(price((L[i] - S[i] - x[zeroString(i, charLen)]), Cb, Cs, i) for i in range(len(L)))
    #dispatch_prob += sum(((Cb[i] * (Pneg[zeroString(i, charLen)]) + L[i]) - Cs[i] * (Ppos[zeroString(i, charLen)] + S[i])) for i in range(len(L)))
    #dispatch_prob += sum((((Cb[i] * Pneg[zeroString(i, charLen)])) - (Cs[i] * Ppos[zeroString(i, charLen)])) for i in range(len(L)))
    #dispatch_prob += sum(price((L[i] - S[i] - Ppos[zeroString(i, charLen)] + Pneg[zeroString(i, charLen)]), Cb, Cs, i) for i in range(len(L)))
    dispatch_prob += sum((Cb[i] * tGpos[zeroString(i, charLen)] - Cs[i] * tGneg[zeroString(i, charLen)]) for i in range(len(L)))
    
    ###Constraints###
    
    #Curtailment Constraint
    if (includeCurtailment == 1):
        for i in range(len(L)):
            if (curtailments[i] != 0):
                dispatch_prob += 0 <= S[i] - abs(curtailments[i]) + tGpos[zeroString(i, charLen)] - tGneg[zeroString(i, charLen)] #Note that S[i] is not the same as the input, as curtailments were added to S already, so curtailments are subtracted here to return to generation excl. curtailment. 
    
    #SOC Minimum and Maximum Constraints
    #Sets SOCmin <= SOC <= SOCmax as well as SOC[0] = SOC0 and SOC[max time] = SOCmax. 
    #dispatch_prob += SOC0 - sum(efficiency(x[zeroString(i, charLen)], chargeEff, (1/dischargeEff)) for i in range(len(L))) == SOCend
    dispatch_prob += SOC0 - sum((chargeEff * Pneg[zeroString(i, charLen)] - Ppos[zeroString(i, charLen)] * 1.0 / dischargeEff) for i in range(len(L))) == SOCend
    
    #for j in range(len(L)):
    #    dispatch_prob += SOC0 - sum(efficiency(x[zeroString(i, charLen)], chargeEff, (1/dischargeEff)) for i in range(j + 1)) >= SOCmin
    for j in range(len(L)):
        dispatch_prob += SOC0 - sum((chargeEff * Pneg[zeroString(i, charLen)] - Ppos[zeroString(i, charLen)] * 1.0 / dischargeEff) for i in range(j + 1)) >= SOCmin
    
    #for j in range(len(L)):
    #    dispatch_prob += SOC0 - sum(efficiency(x[zeroString(i, charLen)], chargeEff, (1/dischargeEff)) for i in range(j + 1)) <= SOCmax
    for j in range(len(L)):
        dispatch_prob += SOC0 - sum((chargeEff * Pneg[zeroString(i, charLen)] - Ppos[zeroString(i, charLen)] * 1.0 / dischargeEff) for i in range(j + 1)) <= SOCmax
    
    #ESS Power Only Comes from Panels Constraint (doesn't store and later return energy based on price differences on / off peak) <-> So comment this section of constraints out if that is not wanted
    #Sets constraint such that the ESS can never charge bymore than the solar output and thus that it can only be charged from PV, not from the grid. 
    #for i in range(len(L)):
    #    dispatch_prob += 0 <= S[i] + x[zeroString(i, charLen)] #efficiency not relevant to this constraint
    if (canChargeESSFromGrid == 0):
        for i in range(len(L)):
            dispatch_prob += 0 <= S[i] + Ppos[zeroString(i, charLen)] - Pneg[zeroString(i, charLen)]
    #ABOVE CONSTRAINT IS OPTIONAL, CAN REMOVE BY SETTING "canChargeESSFromGrid" to 0 in the inputs. 
    
    #Grid Limits (limits the amount of power that can be brought in or out from the grid)
    #Pmin and Pmax constraints for the grid input and output
    #for i in range(len(L)):
    #    dispatch_prob += L[i] - S[i] - Ppos[zeroString(i, charLen)] + Pneg[zeroString(i, charLen)] <= gridImport #Can't demand more power then the grid can supply. Efficiency not relevant to this constraint
    #for i in range(len(L)):
    #    dispatch_prob += L[i] - S[i] - Ppos[zeroString(i, charLen)] + Pneg[zeroString(i, charLen)] >= gridExport #Can't put more power into the grid then it can handle. Efficiency not relevant to this constraint
    #Alternate notation for the grid (constraint/s (Grid Limits) above may be able to be completely removed)
    for i in range(len(L)):
        dispatch_prob += tGpos[zeroString(i, charLen)] - tGneg[zeroString(i, charLen)] + Ppos[zeroString(i, charLen)] - Pneg[zeroString(i, charLen)] + S[i] - L[i] - tCur[zeroString(i, charLen)]  == 0
    
    
    #Solve
    #Solves this function
    #prob.solve()
    dispatch_prob.solve()
    #Results
    #print("Status:", pulp.LpStatus[dispatch_prob.status])
    #print(len(dispatch_prob.variables()))
    R = [0.0] #Return / Results List
    while (len(R) < len(L)):
        R.append(0.0)
    i = 0
    #print(dispatch_prob.variables())
    for v in dispatch_prob.variables():
        #print(v) #####
        #print(v.varValue) #####
        #print()
        if (i < len(R)):
            R[i] -= v.varValue #Subtract Pneg-s
        elif (i < 2*len(R)):
            R[i%len(R)] += v.varValue #Add Ppos-s
            #START OF ROUNDING SECTION
            #R[i%len(R)] *= 100000
            #R[i%len(R)] += 0.5
            #R[i%len(R)] = float(int(R[i%len(R)]))
            #R[i%len(R)] /= 100000
            #END OF ROUNDING SECTION
        i += 1
    #SOC profile
    SOC = [0.0]
    for i in range(len(L)):
        if i == 0:
            SOC[0] = (SOC0 - efficiency(R[i], chargeEff, (1/dischargeEff)))
        else:
            SOC.append(SOC[i - 1] - efficiency(R[i], chargeEff, (1/dischargeEff)))
    for i in range(0,len(SOC)):
        #Convert from power units back to % for capacity
        SOC[i] = SOC[i] * 100.0 / capacity
        #START OF ROUNDING SECTION
        #SOC[i] *= 10000
        #SOC[i] += 0.5
        #SOC[i] = float(int(SOC[i]))
        #SOC[i] /= 10000
        #END OF ROUNDING SECTION
    #Power Export Profile
    etg = [0.0]*len(R) #export to grid
    for i in range(0,len(etg)):
        etg[i] = S[i] + R[i] - L[i]
    ################
    if (printOutput == 1): 
        print("Objective from Model: " + str(dispatch_prob.objective.value()))
        print("Objective from Check: " + str(sum(price((L[i] - S[i] - R[i]), Cb, Cs, i) for i in range(len(L)))))
        print("\nESS Flow (+ve = discharge, -ve = charge): " + str(R))
        print("\nSOC: " + str(SOC))
        #print("\nExport Flow (+ve = power exported to grid, -ve = power imported from grid): " + str(etg)) #Look at #print(v) and #print(v.varValue) now directly. This method is no longer good now that curtailment is an option. 
        print("\nRenewable: " + str(S))
    ################
    #NOTE, Could also return SOC, or SOC & R (ESS-Flow), whatever you want, at present it only returns R, but SOC is calculated above, so it could be outputted just as easilly :)
    #return R #R will have the same number of elements as L (which should have the same number of elements as S, Cb & Cs all do as these are all time series).
    #RETURN DIFFERS BASED ON OutputFlow0SOC1 INPUT, if 0 then outputs ESS flow from grid-side-connection (R), if 1 returns SOC (%)
    if (OutputFlow0SOC1 == 0):
        return R
    elif (OutputFlow0SOC1 == 1):
        return SOC
    else:
        return R

################################
##EXAMPLE OF PROGRAM BEING RUN (EXAMPLE INPUTS) - NOT USED!!!
##All units are in terms of power (called MW below, but it doesn't matter so long as it is consistently used (eg. kW)), $ (again, so long as it is the same unit for all prices), %(as a fraction of energy - capacity for SOCs) and used computationally as a % for efficiency, per timestep (5, 10, 15 minutes, 1 hour, 1 day, etc... so long as it is consistent)
##number of time instances is calculated from this array (below), but all should have the same number of elements anyway. Local load can be set to 0 if unrequired. 
#L =[77.19885796, 73.75938061, 72.33769636, 71.56143678, 72.42210626, 75.13389994, 80.81005451, 86.60794998, 94.66802374, 98.34045209, 99.10576789, 100, 98.99895423, 98.27118453, 98.98791582, 97.93830274, 97.09216475, 95.5078639, 93.6916789, 93.5127757, 93.38223331, 91.48506265, 86.51187361, 82.92431975] #MW #P-Load (Load)
#S = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.721620235184, 15.4072905379, 40.8349598783, 65.5772406409, 87.5933761096, 99.0688781585, 96.9524417332, 81.0287005807, 56.0743538227, 30.9844252736, 9.62109604032, 6.81037432559e-16, 0.0, 0.0, 0.0, 0.0, 0.0] #MWh #Solar/Wind Energy Generated, but excluding curtailments (i.e. exports if it was just the generator (no load or ESS)). To add curtailment information other inputs are used (this is summed in later if required). 
##BELOW IS JUST TESTING, CAN INPUT S DIRECTLY WITH NO NEED FOR ANY ALTERATION
#Smax = max(S)
#for i in range(len(S)):
#    S[i] = S[i]/Smax * 200
##Cb = [82.01, 81.5, 82.02, 80.27, 80.28, 82.35, 82.81, 83.07, 82.88, 82.4, 81.34, 79.26, 77.22, 78.24, 78.66, 78.26, 78.67, 93.71, 92.58, 83.7, 76.18, 74.1, 72.96, 72.54]
##Cb = [116.96, 108.79, 106.38, 103.81, 103.61, 100.92, 103.58, 93.06, 110.15, 116.95, 122.39, 125.13, 130.45, 150.11, 150.52, 150.14, 140.85, 129.13, 116.04, 116.03, 108.75, 117.02, 108.8, 103.58]
#Cb = [85.58, 84.04, 83.74, 65.49, 61.83, 81.72, 85.06, 89.56, 91.18, 92.2, 93.17, 87.6, 86.9, 87.37, 88.65, 87.43, 87.42, 92.72, 90.93, 92.62, 91.22, 105.65, 100.65, 91.57] #$/MW #Cost-$ (Price) buying
##BELOW IS JUST TESTING, CAN INPUT Cb DIRECTLY WITH NO NEED FOR ANY ALTERATION
#for i in range(len(Cb)):
#    Cb[i] = Cb[i] / 1000
##Cs = [72.54, 72.54, 72.54, 72.54, 72.54, 72.54, 72.54, 72.54, 72.54, 72.54, 72.54, 72.54, 72.54, 72.54, 72.54, 72.54, 72.54, 72.54, 72.54, 72.54, 72.54, 72.54, 72.54, 72.54]
##Cs = [93.06, 93.06, 93.06, 93.06, 93.06, 93.06, 93.06, 93.06, 93.06, 93.06, 93.06, 93.06, 93.06, 93.06, 93.06, 93.06, 93.06, 93.06, 93.06, 93.06, 93.06, 93.06, 93.06, 93.06]
#Cs = [61.83, 61.83, 61.83, 61.83, 61.83, 61.83, 61.83, 61.83, 61.83, 61.83, 61.83, 61.83, 61.83, 61.83, 61.83, 61.83, 61.83, 61.83, 61.83, 61.83, 61.83, 61.83, 61.83, 61.83] #$/MW #Cost-$ (Price) selling
##BELOW IS JUST TESTING, CAN INPUT Cs DIRECTLY WITH NO NEED FOR ANY ALTERATION
#for i in range(len(Cs)):
#    Cs[i] = Cs[i] / 1000
#
#capacity = 100 #MW #Used with SOC params (SOC input and output are % as per nomanclature convention, but MW (or whatever power unit is used), for calculation)
#chargeEff = 97 #%
#dischargeEff = 97 #%
#
#Xmin = -100 #MW/timestep
#Xmax = 100 #MW/timestep
#SOCmin = 0 #% #Independant of efficiency (efficiency value is not used when considering this)
#SOCmax = 100 #% #Independant of efficiency (efficiency value is not used when considering this)
#SOC0 = 50 #%
#SOCend = 50 #%
#
#gridExport = -9999 #MW/timestep
#gridImport = 9999 #MW/timestep
#automaticGridImportBasedOnLandS = 0 #If set to 1 will set gridImport to whatever the maximum grid power import would be if there were no ESS (i.e. max(L[time]-S[time] across every time instance) UNLESS GRIDIMPORT IS SMALLER, in which case gridImport is still used and not overwritten as it is a tighter constraint then this automatically generated one. 
#canChargeESSFromGrid = 0 #If set to 0, will apply a additional constraint to the model, preventing the charging of the ESS from the grid (so it would only be charged by S and thus charging can never exceed S)
#
#includeCurtailment = 0 #0 if no curtailment used, 1 if curtailment is used. This is used to add constraints that prevent exports in excess of generation excl. curtailment (input: S) at times of curtailment (such that curtailed energy can only be stored or remain curtailed in the time periods of curtailment). 
#curtailments = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] #Array of curtailments. This is only used if input: includeCurtailment is set to 1. 
#
#OutputFlow0SOC1 = 0 #A toggle to pick prefered output, if set to 0, the Flow (grid side connection of ESS) will be outputted. If 1 SOC of ESS (%) outputted. 
#printOutput = 1 #Prints Objective, ESS Flow (grid side) and SOC if = 1, does not print if = 0. (Doesn't effect return, just doesn't print to terminal too. 
#
#
#Calling the function
#Example of being run: 
#ESSpulpJA(L, S, Cb, Cs, capacity, chargeEff, dischargeEff, Xmin, Xmax, SOCmin, SOCmax, SOC0, SOCend, gridExport, gridImport, automaticGridImportBasedOnLandS, canChargeESSFromGrid, OutputFlow0SOC1, printOutput) #If you don't care about efficiency, set the values to 100, if you don't care about grid limits, set them to a really low gridIn (-9999...) and a really high gridOut (9999...).
################################

#ESSpulpJAfromText Inputs Section
def ESSpulpJAfromText(DispatchInput, DispatchOutput, splitCharacter):
    #Runs ESSpulpJA, but via text files (similar to PF-OPF in this way)
    #Note, the values below are placeholders, the text file will overwrite. 
    rL = [] #Load, so 0. 
    rS = [] #Solar / Wind (or other fixed power generation). Input value excl. curtailment, but it is summed in later. 
    rCb = [] #Buying price (from the grid). 
    rCs = [] #Selling price (to the grid). 
    rcapacity = 100 #Capacity. 
    rchargeEff = 100 #Charging efficiency. 
    rdischargeEff = 100 #Discharging efficiency. 
    rXmin = -100 #ESS Discharging/Charging Limit. 
    rXmax = 100 #ESS Discharging/Charging Limit. 
    rSOCmin = 0 #Min state of charge. 
    rSOCmax = 100 #Max state of charge. 
    rSOC0 = 50 #Starting state of charge. 
    rSOCend = 50 #Finishing state of charge. 
    rgridExport = -9999 #Grid import / export limit. 
    rgridImport = 9999 #Grid import / export limit. 
    rautomaticGridImportBasedOnLandS = 0
    rcanChargeESSFromGrid = 0 #Can the ESS charge from the grid (as opposed to just from the local source)?
    includeCurtailment = 1 #Include curtailments or not (1 uses curtailment and adds constraints, 0 leaves curtailment functionality unused and does not sum curtailments in later). 
    curtailments = [] #Curtailments. 
    rOutputFlow0SOC1 = 0
    rprintOutput = 1
    file = open(DispatchInput, "r")
    readLoop = 0
    while (readLoop < 19):
        readLoop = readLoop + 1
        singleLine = file.readline()
        singleline = singleLine.strip()
        if (readLoop == 1):
            rL = singleLine.split(splitCharacter)
            for i in range(len(rL)):
                rL[i] = float(rL[i])
        elif (readLoop == 2):
            rS = singleLine.split(splitCharacter)
            for i in range(len(rS)):
                rS[i] = float(rS[i])
        elif (readLoop == 3):
            rCb = singleLine.split(splitCharacter)
            for i in range(len(rCb)):
                rCb[i] = float(rCb[i])
        elif (readLoop == 4):
            rCs = singleLine.split(splitCharacter)
            for i in range(len(rCs)):
                rCs[i] = float(rCs[i])
        elif (readLoop == 5):
            rcapacity = float(singleLine.replace(splitCharacter,"").strip())
        elif (readLoop == 6):
            rchargeEff = float(singleLine.replace(splitCharacter,"").strip())
        elif (readLoop == 7):
            rdischargeEff = float(singleLine.replace(splitCharacter,"").strip())
        elif (readLoop == 8):
            rXmin = float(singleLine.replace(splitCharacter,"").strip())
        elif (readLoop == 9):
            rXmax = float(singleLine.replace(splitCharacter,"").strip())
        elif (readLoop == 10):
            rSOCmin = float(singleLine.replace(splitCharacter,"").strip())
        elif (readLoop == 11):
            rSOCmax = float(singleLine.replace(splitCharacter,"").strip())
        elif (readLoop == 12):
            rSOC0 = float(singleLine.replace(splitCharacter,"").strip())
        elif (readLoop == 13):
            rSOCend = float(singleLine.replace(splitCharacter,"").strip())
        elif (readLoop == 14):
            rgridExport = float(singleLine.replace(splitCharacter,"").strip())
        elif (readLoop == 15):
            rgridImport = float(singleLine.replace(splitCharacter,"").strip())
        elif (readLoop == 16):
            rautomaticGridImportBasedOnLandS = float(singleLine.replace(splitCharacter,"").strip())
        elif (readLoop == 17):
            rcanChargeESSFromGrid = float(singleLine.replace(splitCharacter,"").strip())
        elif (readLoop == 18):
            includeCurtailment = float(singleLine.replace(splitCharacter,"").strip())
        elif (readLoop == 19):
            curtailments = singleLine.split(splitCharacter)
            for i in range(len(curtailments)):
                curtailments[i] = float(curtailments[i])
                if (includeCurtailment) == 1:
                    rS[i] += abs(curtailments[i]) #Note this sum is used here. So the input for S is generation excl. curtailment, but S is now generation incl. curtailment, and we also have curtailments. If we want generation excl. curtailment later, we just subtract curtailment from the new S. 
        elif (readLoop == 20):
            rOutputFlow0SOC1 = float(singleLine.replace(splitCharacter,"").strip())
        elif (readLoop == 21):
            rprintOutput = float(singleLine.replace(splitCharacter,"").strip())
    file.close()
    out = []
    out = ESSpulpJA(rL, rS, rCb, rCs, rcapacity, rchargeEff, rdischargeEff, rXmin, rXmax, rSOCmin, rSOCmax, rSOC0, rSOCend, rgridExport, rgridImport, rautomaticGridImportBasedOnLandS, rcanChargeESSFromGrid, includeCurtailment, curtailments, rOutputFlow0SOC1, rprintOutput)
    ssout = str(out).strip()
    sout = []
    for i in range(len(ssout)):
        if (ssout[i] == '[' or ssout[i] == ']' or ssout[i] == ' '):
            i = i #do nothing
        else:
            if (ssout[i] == ','):
                sout.append(splitCharacter)
            else: 
                sout.append(ssout[i])
    #sout.append('\0')
    sout = ''.join(sout)
    #print(sout)
    #Now to print this output into the text file (first clear it though by opening and closing)
    open(DispatchOutput, 'w').close()
    f = open(DispatchOutput, 'w')
    f.write(sout)
    f.write('\n')
    #f.write('\n')
    f.close()    

#Example of being run: 
if __name__== '__main__':
    ESSpulpJAfromText('ESSDispatchInput.txt', 'ESSDispatchOutput.txt', '	')
    sys.exit()

#At present all time related values are assumed to be in terms of the same unit of time (eg. If timesteps are all 1 hour, then Xmin and Xmax are per hour).
#In other words, this model presumes inputs are all in terms of the same timestep (exactly what that timestep is does therefore not matter so long is it is consistent throughout inputs). 
#Should now be consistent with nomenclature. 

########################################################################
### Final Notes ###
#Try itterative test (repeat all arrays X10 or something along those lines, see if the simulation still runs - i.e. see how much pulp can handle)? - result: Works for > 1 week (hourly), longer times lengthen computation time (by >linear increase?). This is likely to depend on the technical capabilities of the device it is run from. 
#Check constraints are fulfilled using this new method - result: Appears consistent
#Comment the nomenclature into this for convenience - result: Not all characters availaible, but done so within reason
#Tidy things up :) - result: OK
#Something to round down the answers a bit due to the long decimals being a bit pointless in this application? - result: Not used for now due to potentially pointless simplificaiton or using a library which would require it to be installed
#How to write optimality equation in linear terms (without if statement?) - result: Done
#Maybe an input variable to adjust for outputting R (power flows from grid side of ESS) or SOC (ESS state of charge)? - result: Done (if you don't want this just delete the OutputFlow0SOC1 related stuff and just return R(ESS-Flow) or SOC)
#Use via text files. - result: DONE
#
#Regarding Nomanclature: Equations used follow nomanclature almost exactly (main exception being that SOC isn't a seperate and individual variable, but rather put in terms of other variables)
#NOTE: for above output type, while these are both desired output options, price was specifically stated to not be required. Regardless if this changes then this could be easilly implimented (see Output Note in 'ReadMe' for Dispatch/ESS).
########################################################################
