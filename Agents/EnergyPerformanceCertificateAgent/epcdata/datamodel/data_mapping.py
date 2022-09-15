################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 15 Sep 2022                            #
################################################

# The purpose of this module is to provide a mapping between retrieved 
# observation variables from Met Office (via metoffer) to corresponding
# concepts and units as defined in OntoEMS

from epcdata.datamodel.iris import *


# Mapping of observation variables to OntoEMS concepts
EPC_DATA = {
    '': '',
}

# Mapping of units to OM units and symbols
UNITS_MAPPING = {
     # original OM symbol for °C: '&#x00B0;C'
     OM_AREA: (OM_M2, 'm2'),
     OM_HEIGHT: (OM_M, 'm'),
     OM_AMOUNT_MONEY: (OM_GBP, '£')
}
