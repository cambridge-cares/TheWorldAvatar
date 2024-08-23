import numpy as np
import re

# Units conversion 
#
def convertLengthUnitsToDLPunit(aunit_in,exponent=1.0):
    # SI: m
    DLpolyunit_unit = 'Angstrom'
    mult_factor = 1.0
    if aunit_in == 'NM':
        mult_factor = 10
    elif aunit_in == 'Angstrom':
        mult_factor = 1.0
    return mult_factor**exponent

def convertEnergyMoleUnitsToDLPunit(aunit_in,exponent=1.0):
    # SI: J
    DLpoly_unit = 'KJ/MOL'
    mult_factor = 1.0
    if aunit_in == 'KJ/MOL':
        mult_factor = 1.0
    elif aunit_in == 'Hartree':
        mult_factor = 2625.5
    return mult_factor**exponent