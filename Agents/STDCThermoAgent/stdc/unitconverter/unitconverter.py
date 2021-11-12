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

# Units conversion stuff
#
def convertLengthUnitsToSI(aunit_in,exponent=1.0):
    # SI: m
    SI_unit = 'M'
    mult_factor = 1.0
    aunit_in = aunit_in.upper()
    if aunit_in == 'M':
        mult_factor = 1.0
    elif aunit_in == 'KM':
        mult_factor = 1E3
    elif aunit_in == 'CM':
        mult_factor = 1E-2
    elif aunit_in == 'MM':
        mult_factor = 1E-3
    elif aunit_in == 'MICROM':
        mult_factor = 1E-6
    elif aunit_in == 'NM':
        mult_factor = 1E-9
    elif aunit_in == 'A':
        mult_factor = p.Angs
    elif aunit_in == 'A0':
        mult_factor = p.a0
    elif checkUnitType(aunit_in,'LENGTH^n'):
        unit_tokens ,unit_exps = extractUnits(aunit_in)
        mult_factor = convertLengthUnitsToSI(unit_tokens[0],float(unit_exps[0]))
    return mult_factor**exponent

def convertTimeUnitsToSI(aunit_in,exponent=1.0):
    # SI: s
    SI_unit = 'S'
    mult_factor = 1.0
    aunit_in = aunit_in.upper()
    if aunit_in == 'S':
        mult_factor = 1.0
    elif aunit_in == 'H':
        mult_factor = 3600.0
    elif aunit_in == 'MIN':
        mult_factor = 60
    elif aunit_in == 'MS':
        mult_factor = 1E-3
    elif aunit_in == 'MICROS':
        mult_factor = 1E-6
    elif aunit_in == 'NS':
        mult_factor = 1E-9
    elif checkUnitType(aunit_in,'TIME^n'):
        unit_tokens ,unit_exps = extractUnits(aunit_in)
        mult_factor = convertTimeUnitsToSI(unit_tokens[0],float(unit_exps[0]))
    return mult_factor**exponent

def convertMassUnitsToSI(aunit_in,exponent=1.0):
    # SI: kg
    SI_unit = 'KG'
    mult_factor = 1.0
    aunit_in = aunit_in.upper()
    if aunit_in == 'KG':
        mult_factor = 1.0
    elif aunit_in == 'T':
        mult_factor = 1E3
    elif aunit_in == 'DG':
        mult_factor = 1E3
    elif aunit_in == 'G':
        mult_factor = 1E-3
    elif aunit_in == 'MG':
        mult_factor = 1E-6
    elif aunit_in == 'MICROG':
        mult_factor = 1E-9
    elif aunit_in == 'AMU':
        mult_factor = p.amu
    elif checkUnitType(aunit_in,'MASS^n'):
        unit_tokens ,unit_exps = extractUnits(aunit_in)
        mult_factor = convertMassUnitsToSI(unit_tokens[0],float(unit_exps[0]))
    return mult_factor**exponent

def convertTemperatureUnitsToSI(aunit_in,exponent=1.0):
    # SI: K
    SI_unit = 'K'
    mult_factor = 1.0
    add_factor = 0.0
    aunit_in = aunit_in.upper()
    if aunit_in == 'K':
        mult_factor = 1.0
    elif aunit_in == 'C':
        add_factor = 273.15
    elif aunit_in == 'F':
        mult_factor = 5.0/9.0
        add_factor = 255.3722222
    elif checkUnitType(aunit_in,'TEMPERATURE^n'):
        unit_tokens ,unit_exps = extractUnits(aunit_in)
        mult_factor, add_factor = convertTemperatureUnitsToSI(unit_tokens[0],float(unit_exps[0]))
    return mult_factor**exponent, add_factor

def convertMoleUnitsToSI(aunit_in,exponent=1.0):
    # SI: 1/s or Hz
    SI_unit = 'MOL' # or NA
    mult_factor = 1.0
    aunit_in = aunit_in.upper()
    if aunit_in == 'MOL':
        mult_factor = 1.0
    elif aunit_in == '#':
        mult_factor = 1.0/p.NA
    elif aunit_in == 'KMOL':
        mult_factor = 1E3
    elif checkUnitType(aunit_in,'MOLE^n'):
        unit_tokens ,unit_exps = extractUnits(aunit_in)
        mult_factor = convertMoleUnitsToSI(unit_tokens[0],float(unit_exps[0]))
    return mult_factor**exponent

def convertFrequencyUnitsToSI(aunit_in,exponent=1.0):
    # SI: 1/s or Hz
    SI_unit = '1/S' # or Hz
    mult_factor = 1.0
    aunit_in = aunit_in.upper()
    if aunit_in == '1/S' or aunit_in == 'HZ':
        mult_factor = 1.0
    elif aunit_in == 'KHZ':
        mult_factor = 1E3
    elif aunit_in == 'MHZ':
        mult_factor = 1E6
    elif aunit_in == 'GHZ':
        mult_factor = 1E9
    elif checkUnitType(aunit_in,'TEMPERATURE'):
        mult_factor, add_factor = convertTemperatureUnitsToSI(aunit_in)
        mult_factor = mult_factor*p.kB/p.h
    elif checkUnitType(aunit_in,'TIME^-1'):
        mult_factor = convertTimeUnitsToSI(aunit_in)
    elif checkUnitType(aunit_in,'LENGTH^-1'):
        mult_factor = convertLengthUnitsToSI(aunit_in)
        mult_factor = p.c*mult_factor
    return mult_factor**exponent

def convertEnergyMoleculeUnitsToSI(aunit_in,exponent=1.0):
    # SI: J
    SI_unit = 'J'
    mult_factor = 1.0
    aunit_in = aunit_in.upper()
    if aunit_in == 'J':
        mult_factor = 1.0
    elif aunit_in == 'KJ':
        mult_factor = 1E3
    elif aunit_in == 'MJ':
        mult_factor = 1E6
    elif aunit_in == 'GJ':
        mult_factor = 1E9
    elif aunit_in == 'CAL':
        mult_factor = 4.184
    elif aunit_in == 'KCAL':
        mult_factor = 4.184E3
    elif aunit_in == 'MCAL':
        mult_factor = 4.184E6
    elif aunit_in == 'GCAL':
        mult_factor = 4.184E9
    elif aunit_in == 'HA':
        mult_factor = p.Ha
    elif aunit_in == 'EV':
        mult_factor = p.eV
    elif checkUnitType(aunit_in,'TIME^-1'):
        unit_tokens ,unit_exps = extractUnits(aunit_in)
        mult_factor = convertTimeUnitsToSI(unit_tokens[0],float(unit_exps[0]))
        mult_factor = mult_factor*p.h
    elif checkUnitType(aunit_in,'FREQUENCY'):
        mult_factor = convertFrequencyUnitsToSI(aunit_in)
        mult_factor = mult_factor*p.h
    elif checkUnitType(aunit_in,'LENGTH^-1'):
        unit_tokens ,unit_exps = extractUnits(aunit_in)
        mult_factor = convertLengthUnitsToSI(unit_tokens[0],float(unit_exps[0]))
        mult_factor = mult_factor*p.c*p.h
    elif checkUnitType(aunit_in,'TEMPERATURE'):
        mult_factor, add_factor = convertTemperatureUnitsToSI(aunit_in)
        mult_factor = p.kB*mult_factor
    return mult_factor**exponent

def convertEnergyMoleUnitsToSI(aunit_in,exponent=1.0):
    # SI: J/mol
    SI_unit = 'J/MOL'
    mult_factor = 1.0
    aunit_in = aunit_in.upper()
    if aunit_in == 'J/MOL':
        mult_factor = 1.0
    elif checkUnitType(aunit_in,'ENERGY_PER_MOLECULE'):
        mult_factor1, _ = convertUnitsToSI('ENERGY_PER_MOLECULE',aunit_in)
        mult_factor2, _ = convertUnitsToSI('MOLE','#',-1)
        mult_factor = mult_factor1*mult_factor2
    elif checkUnitType(aunit_in,'ENERGY_PER_MOLECULE,MOLE^-1'):
        unit_tokens ,unit_exps = extractUnits(aunit_in)
        for i,ut in enumerate(unit_tokens):
            mult_factor1, _ = convertUnitsToSI(getUnitType(ut),ut,float(unit_exps[i]))
            mult_factor = mult_factor*mult_factor1
    elif checkUnitType(aunit_in,'TIME^-1'):
        mult_factor = convertTimeUnitsToSI(aunit_in)
        mult_factor = mult_factor*p.h*p.NA
    elif checkUnitType(aunit_in,'LENGTH^-1'):
        mult_factor = convertLengthUnitsToSI(aunit_in)
        mult_factor = mult_factor*p.h*p.c*p.NA
    elif checkUnitType(aunit_in,'TEMPERATURE'):
        mult_factor, add_factor = convertTemperatureUnitsToSI(aunit_in)
        mult_factor = p.kB*mult_factor*p.NA
    return mult_factor**exponent

def convertInertiaUnitsToSI(aunit_in,exponent=1.0):
    # SI: kg*m^2
    SI_unit = 'KG*M^2'
    mult_factor = 1.0
    aunit_in = aunit_in.upper()
    if aunit_in == 'KG*M^2':
        mult_factor = 1.0
    elif checkUnitType(aunit_in,'MASS,LENGTH^2'):
        unit_tokens ,unit_exps = extractUnits(aunit_in)
        for i,ut in enumerate(unit_tokens):
            mult_factor1, _ = convertUnitsToSI(getUnitType(ut),ut,float(unit_exps[i]))
            mult_factor = mult_factor*mult_factor1
    return mult_factor**exponent

def convertUnitsToSI(atype,aunit_in,exponent=1.0):
    mult_factor = 1.0
    add_factor = 0.0
    aunit_in = aunit_in.upper()
    atype = atype.upper()


    if atype=='LENGTH':
        # SI: m
        mult_factor = convertLengthUnitsToSI(aunit_in,exponent)

    if atype=='TIME':
        # SI: s
        mult_factor = convertTimeUnitsToSI(aunit_in,exponent)

    if atype=='MASS':
        # SI: kg
        mult_factor = convertMassUnitsToSI(aunit_in,exponent)

    if atype=='FREQUENCIES':
        mult_factor = convertFrequencyUnitsToSI(aunit_in,exponent)

    if atype=='MOLE':
        # SI: mol or NA
        mult_factor = convertMoleUnitsToSI(aunit_in,exponent)

    if atype=='ENERGY_PER_MOLECULE':
        # SI: J
        mult_factor = convertEnergyMoleculeUnitsToSI(aunit_in,exponent)

    if atype=='ENERGY_PER_MOLE':
        # SI: J/mol
        mult_factor = convertEnergyMoleUnitsToSI(aunit_in,exponent)

    if atype=='INERTIA':
        # SI: kg*m^2
        mult_factor = convertInertiaUnitsToSI(aunit_in,exponent)

    if atype=='TEMPERATURE':
        # SI: K
        mult_factor, add_factor = convertTemperatureUnitsToSI(aunit_in,exponent)

    return mult_factor**exponent,add_factor

def getUnitType(aunit_in):
    aunit_in = aunit_in.upper()
    rtype = 'NONE'
    if compareList(aunit_in,LENGTH_UNITS):
        rtype = 'LENGTH'
    if compareList(aunit_in,TIME_UNITS):
        rtype = 'TIME'
    if compareList(aunit_in,MASS_UNITS):
        rtype = 'MASS'
    if compareList(aunit_in,TEMPERATURE_UNITS):
        rtype = 'TEMPERATURE'
    if compareList(aunit_in,MOLE_UNITS):
        rtype = 'MOLE'
    if compareList(aunit_in,ENERGY_UNITS):
        rtype = 'ENERGY_PER_MOLECULE'
    if compareList(aunit_in,FREQUENCY_UNITS):
        rtype = 'FREQUENCY'
    return rtype

def checkUnitType(aunit_in,aunit_check):
    flag = True
    # extract units and exponents
    # e.g. aunit_in = G*A^2
    # unit_intokens = G, A
    # unit_inexp = 1, 2
    unit_intokens, unit_inexp = extractUnits(aunit_in)
    # tokenize and sort aunit_check
    # e.g. aunit_check = MASS,LENGTH^2
    # unit_checktokens = TIME
    #
    if ',' in aunit_check:
        # unit_checktokens = MASS, LENGTH^2
        unit_checktokens=aunit_check.split(',')
        # indices store an alphabetical order of items in array unit_checktokens
        # unit_check_ind = 1 (MASS), 0 (LENGTH) (because [L]ENGTH comes before [M]ASS)
        unit_check_ind = sorted(range(len(unit_checktokens)), key=lambda k: unit_checktokens[k])
        # sorts array unit_checktokens = LENGTH^2, MASS
        unit_checktokens =  [unit_checktokens[x] for x in unit_check_ind]
    else:
        unit_checktokens = []
        unit_checktokens.append(aunit_check)

    # construct final aunit_in strings
    # different ways of writing the same unit information in case of ^1 power
    unit_in1 = []  # UNIT1^1, UNIT2^2 e.g. MASS^1, LENGTH^2
    unit_in2 = []  # UNIT1, UNIT2^2   e.g. MASS, LENGTH^2
    unit_in3 = []  # UNIT1^n, UNIT2^n e.g. MASS^n, LENGTH^n
    for i,ut in enumerate(unit_intokens):
        unit_in1.append(getUnitType(ut)+'^'+unit_inexp[i])
        unit_in3.append(getUnitType(ut)+'^n')
        if unit_inexp[i] != '1':
            unit_in2.append(getUnitType(ut)+'^'+unit_inexp[i])
        else:
            unit_in2.append(getUnitType(ut))
    # sort aunit_in strings
    unit_in1_ind = sorted(range(len(unit_in1)), key=lambda k: unit_in1[k])
    unit_in2_ind = sorted(range(len(unit_in2)), key=lambda k: unit_in2[k])
    unit_in3_ind = sorted(range(len(unit_in3)), key=lambda k: unit_in3[k])
    unit_in1 = [unit_in1[x] for x in unit_in1_ind]   # LENGTH^2, MASS^1
    unit_in2 =  [unit_in2[x] for x in unit_in2_ind]  # LENGTH^2, MASS
    unit_in3 =  [unit_in3[x] for x in unit_in3_ind]  # LENGTH^n, MASS^n

    # compare aunit_in strings with aunit_check
    if len(unit_checktokens)!=len(unit_in1):
        flag = False
    else:
        for i,check_units in enumerate(unit_checktokens):
            if check_units != unit_in1[i] and check_units != unit_in2[i] \
                and check_units != unit_in3[i]:
                flag = False
    return flag

def compareList(avalue,alist):
    result = False
    for items in alist:
        if items == avalue:
            result = True
            break
    return result

def extractUnits(aunit_in):
    unit_list = []
    exp_value = []
    #find all ^number patterns where number can be 1, 1.0, 1/2 or 1.0/2.0 etc..
    # --------------------------------
    match = re.findall('(\^[+-]?\d+[\/\d. ]*|\d)', aunit_in)
    # assign a temp variable
    temp_unit = aunit_in
    # if there was at least one match
    if match:
        # loop through matches removing '/' characters that may appear at the end of string
        for m in match:
            # to make sure we catched the correct string
            if m[0]=='^':
                if m[-1] == '/':
                    m = m[:-1]
                #also do not keep '^' character
                m = m[1:]
                exp_value.append(m)
                # remove found numbers from aunit_in and store it in temp unit
                temp_unit = re.sub(m, '', aunit_in)
    # --------------------------------

    # split temp_unit at '/' but keep this delimiter e.g. str= 'a/b' => ['a','/','b']
    temp_unit = re.split('(\/)',temp_unit)
    # --------------------------------
    # merge '/' with the proceeding substring e.g. ['a','/','b'] => ['a','/b']
    # --------------------------------
    add_flag = False
    for item in temp_unit:
        if add_flag:
            unit_list[-1] = unit_list[-1]+item
            add_flag = False
        else:
            # check for 1/m etc.. units, we do not want numbers here
            if not item.isdigit():
                unit_list.append(item)
        if item == '/':
            add_flag = True
    temp_unit = unit_list
    # --------------------------------

    # split temp_unit at '*'
    # --------------------------------
    temp_unit2 = []
    for units in temp_unit:
        if '*' in units:
            match = re.split('\*',units)
            for m in match:
                temp_unit2.append(m)
        else:
            temp_unit2.append(units)
    temp_unit = temp_unit2
    # --------------------------------

    final_exps = []
    final_units = []
    it = 0
    for i,token in enumerate(temp_unit):
        if '/' in token and '^' in token:
            if '-' == exp_value[it][0]:
                final_exps.append(exp_value[it][1:])
            else:
                final_exps.append(exp_value[it])
            final_units.append(re.sub('(\/|\^)','',token))
            it = it + 1
        elif '^' in token:
                final_exps.append(exp_value[it])
                final_units.append(re.sub('(\^)','',token))
                it = it + 1
        elif '/' in token:
            final_exps.append('-1')
            final_units.append(re.sub('(\/)','',token))
        else:
            final_exps.append('1')
            final_units.append(token)
    return final_units,final_exps