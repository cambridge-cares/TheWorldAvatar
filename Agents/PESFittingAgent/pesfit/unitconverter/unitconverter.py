LENGTH_UNITS = ['M', 'CM', 'MM', 'MICROM', 'NM', 'Angstrom', 'A0']
ENERGY_UNITS = ['KJ/MOL', 'KCAL/MOL', 'Hartree', 'EV']

# Units conversion stuff
#
def convertLengthUnitsToDLPunit(aunit_in):
    aunit_in = getUnit(aunit_in)
    # DL_POLY default: A
    mult_factor = 1.0
    if compareList(aunit_in,LENGTH_UNITS):
        if aunit_in == 'Angstrom':
            mult_factor = 1.0
        elif aunit_in == 'M':
            mult_factor = 1E10
        elif aunit_in == 'CM':
            mult_factor = 1E8
        elif aunit_in == 'MM':
            mult_factor = 1E7
        elif aunit_in == 'MICROM':
            mult_factor = 1E4
        elif aunit_in == 'NM':
            mult_factor = 10
        elif aunit_in == 'A0':
            mult_factor = 0.529177
    else:
        print('unit ' + aunit_in + ' not in the list')
    return mult_factor

def convertEnergyMoleUnitsToDLPunit(aunit_in):
    aunit_in = getUnit(aunit_in)
    # DLP_unit: kJ/mol
    mult_factor = 1.0
    if compareList(aunit_in,ENERGY_UNITS):
        if aunit_in == 'KJ/MOL':
            mult_factor = 1.0
        elif aunit_in == 'Hartree':
            mult_factor = 2625.5
        elif aunit_in == 'EV':
            mult_factor = 96.486
    else:
        print('unit ' + aunit_in + ' not in the list')
    return mult_factor

def compareList(avalue,alist):
    result = False
    for items in alist:
        if items == avalue:
            result = True
            break
    return result

def getUnit(aunit_in):
    unit = aunit_in.split('#', 1)
    unit = unit[1]
    return unit