import numpy as np
import stdc.utils.params as p
import re

LENGTH_UNITS = ['M', 'CM', 'MM', 'MICROM', 'NM', 'A', 'A0']
TIME_UNITS = ['S', 'MIN', 'H', 'MICROS', 'NS']
MASS_UNITS = ['KG', 'T', 'DG', 'G', 'MG', 'MICROG', 'AMU']
TEMPERATURE_UNITS = ['K', 'C', 'F']
MOLE_UNITS = ['MOL', '#', 'KMOL']
ENERGY_UNITS = ['J', 'KJ', 'MJ', 'GJ', 'CAL', 'KCAL', 'MCAL', 'GCAL', 'HA', 'EV']
FREQUENCY_UNITS = ['1/S','HZ','KHZ','GHZ','MHZ']

# intertia tensor
# -----------------------------
def getInertiaTensor(aElMolWt,aXYZ):
    # ===================================
    # helper functions to populate 
    # inertia tensor
    #
    # diagonal elements    
    def getDiagMoments(a,b,m):
        MolWt = sum(m)
        sum1 = 0.0
        sum2 = 0.0
        sum3 = 0.0
        for i,rows in enumerate(m):
            sum1 = sum1 + m[i]*(a[i]*a[i]+b[i]*b[i])
            sum2 = sum2 + m[i]*a[i]
            sum3 = sum3 + m[i]*b[i]
        sum2 = sum2*sum2 * 1.0/MolWt
        sum3 = sum3*sum3 * 1.0/MolWt
        Iaa = sum1 - sum2 - sum3
        return Iaa
    # off-diagonal elements
    # -----------------------------
    def getOffDiagMoments(a,b,m):
        MolWt = sum(m)
        sum1 = 0.0
        sum2 = 0.0
        sum3 = 0.0
        for i,rows in enumerate(m):
            sum1 = sum1 + m[i]*a[i]*b[i]
            sum2 = sum2 + m[i]*a[i]
            sum3 = sum3 + m[i]*b[i]
        Iab = -sum1 + 1.0/MolWt*sum2*sum3
        return Iab
    # ===================================

    # init inertia tensor
    IT = np.empty([3,3])
    # get mass vector and X,Y,Z coordiantes
    m = aElMolWt
    X = aXYZ[:,0]
    Y = aXYZ[:,1]
    Z = aXYZ[:,2]
    # get diagonal and off-diagonal elements
    Ixx = getDiagMoments(Y,Z,m)
    Iyy = getDiagMoments(X,Z,m)
    Izz = getDiagMoments(X,Y,m)
    Ixy = getOffDiagMoments(X,Y,m)
    Iyx = Ixy
    Ixz = getOffDiagMoments(X,Z,m)
    Izx = Ixz
    Iyz = getOffDiagMoments(Y,Z,m)
    Izy = Iyz
    # put everything together
    IT = [[Ixx,Ixy,Ixz],[Iyx,Iyy,Iyz],[Izx,Izy,Izz]]
    return IT

# Get moments of Inertia 
# of a molecule
#---------------------------------
def getMomentsOfInertia(aElMolWt,aXYZ,aGeomType):
    InertiaMom = []    
    #construct the mass and XYZ np arrays
    if len(aElMolWt)>0 and len(aXYZ)>0:
        IT = getInertiaTensor(aElMolWt,aXYZ)
        eigen,V = np.linalg.eig(IT)
        InertiaMom = [eigen[0],eigen[1],eigen[2]] # in amu*A^2
    return sorted(InertiaMom,reverse=True)

# Entropy of a species from NASA polynomials
#--------------------------
def getEntropy(alow,ahigh,Trange,T):
    S = 0.0        
    if T>0.0:
        Tmid = Trange[1]
        Ta=[]
        Ta.append(np.log(T))   #0
        Ta.append(T)           #1
        Ta.append(T*T/2.0)     #2
        Ta.append(T*T*T/3.0)   #3
        Ta.append(T*T*T*T/4.0) #4
        if T<=Tmid:
            a = alow
        else:
            a = ahigh
        for i in range(len(Ta)):
            S = S + a[i]*Ta[i]
        S = (S + a[6])*p.R
    return S

# Internal Energy of a species from NASA polynomials
#--------------------------
def getInternalEnergy(alow,ahigh,Trange,T):
    H = getEnthalpy(alow,ahigh,Trange,T)
    U = H - p.R*T
    return U

# Heat Capacity Cv of a species from NASA polynomials
#--------------------------
def getHeatCapacityCv(alow,ahigh,Trange,T):
    Cv = getHeatCapacityCp(alow,ahigh,Trange,T) - p.R
    return Cv

# Heat Capacity Cp of a species from NASA polynomials
#--------------------------
def getHeatCapacityCp(alow,ahigh,Trange,T):
    Cp = 0.0
    Tmid = Trange[1]
    Ta=[]
    Ta.append(1.0)      #0
    Ta.append(T)        #1
    Ta.append(T*T)      #2
    Ta.append(T*T*T)    #3
    Ta.append(T*T*T*T)  #4
    if T<=Tmid:
        a = alow
    else:
        a = ahigh
    for i in range(len(Ta)):
        Cp = Cp + a[i]*Ta[i]
    Cp = Cp*p.R
    return Cp

# Enthalpy of a species from NASA polynomials
#--------------------------
def getEnthalpy(alow,ahigh,Trange,T):
    H = 0.0
    if T>0.0:
        Tmid = Trange[1]
        Ta=[]
        Ta.append(1.0)         #0
        Ta.append(T/2.0)       #1
        Ta.append(T*T/3.0)     #2
        Ta.append(T*T*T/4.0)   #3
        Ta.append(T*T*T*T/5.0) #4
        Ta.append(1.0/T)       #5
        if T<=Tmid:
            a = alow
        else:
            a = ahigh
        for i in range(len(Ta)):
            H = H + a[i]*Ta[i]
        H = H*p.R*T
    return H

# Gibbs Energy of a species from NASA polynomials
#--------------------------
def getGibbsEnergy(alow,ahigh,Trange,T):
    H = getEnthalpy(alow,ahigh,Trange,T)
    S = getEntropy(alow,ahigh,Trange,T)
    G = H - T*S
    return G

def chemFormulaToAtomsCounts(chemFormula, atomCounts={}, multiplier=1.0):    
    atomCounts, chemFormula = _funcGroupsAtomsCounts(chemFormula.upper(), atomCounts)
    atomCounts = _chemFormulaToAtomsCounts(chemFormula, atomCounts, multiplier)

    return atomCounts

def _funcGroupsAtomsCounts(chemFormula, atomCounts):
    funcGroupCounts = {}
    funcGroupRegex=f'(\(.*?\)\d*)'
    funcGroupsMatch = re.findall(funcGroupRegex,chemFormula)
    if funcGroupsMatch:
        for funcGroup in sorted(funcGroupsMatch,reverse=True,key=len):
            chemFormula = chemFormula.replace(funcGroup,'')        
            countMatch = re.search('\)(\d+)$',funcGroup)
            if countMatch:
                count = countMatch.groups()[0]
                funcGroup = funcGroup.replace(count, '')
            else:
                count = '1'

            funcGroup = funcGroup.replace(')','').replace('(','')
            if funcGroup not in funcGroupCounts:
                funcGroupCounts[funcGroup] = int(count)
            else:
                funcGroupCounts[funcGroup] += int(count)

        for funcGroup, funcGroupCount in funcGroupCounts.items():            
            atomCounts = _chemFormulaToAtomsCounts(funcGroup, atomCounts, funcGroupCount)
    return atomCounts, chemFormula

def _chemFormulaToAtomsCounts(chemFormula, atomCounts, multiplier):
    Elements =['H','HE','LI','BE','B','C','N','O','F','NE','NA','MG','AL',
               'SI','P','S','CL','AR','K','CA','SC','TI','V','CR','MN',
               'FE','CO','NI','CU','ZN','GA','GE','AS','SE','BR','KR',
               'RB','SR','Y','ZR','NB','MO','TC','RU','RH','PD','AG','CD',
               'IN','SN','SB','TE','IN','XE','CS','BA','LA','CE','PR',
               'ND','PM','SM','EU','GD','TB','DY','HO','ER','TM','YB',
               'LU','HF','TA','W','RE','OS','IR','PT','AU','HG','TL',
               'PB','BI','PO','AT','RN','FR','RA','AC','TH','PA','U',
               'NP','PU','AM','CM','BK','CF','ES','FM','MD','NO','LR',
               'RF','DB','SG','BH','HS','MT','DS','RG','CN','NH','FL','MC']

    atomCountsRegex=f'(['+Elements[0]
    for el in Elements[1:]:
        atomCountsRegex += f'|'+ el
    atomCountsRegex += f']\d*)'

    atomCountMatch = re.findall(atomCountsRegex,chemFormula)
    if atomCountMatch:
        for atomCount in atomCountMatch:
            countMatch = re.search('(\d+)$',atomCount)
            if countMatch:
                count = int(countMatch.groups()[0])
                atom = atomCount.replace(countMatch.groups()[0], '')
            else:
                count = 1
                atom = atomCount
            if atom not in atomCounts:
                atomCounts[atom] = int(count*multiplier)
            else:
                atomCounts[atom] += int(count*multiplier)

    return atomCounts