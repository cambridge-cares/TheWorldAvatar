################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 16 Oct 2022                            #
################################################

# The purpose of this module is to provide a mapping between retrieved 
# HM Land Registry's Price Paid Data property types and instantiated property
# types according to OntoBuiltEnv

# For details on PPD data, see:
# https://www.gov.uk/guidance/about-the-price-paid-data#explanations-of-column-headers-in-the-ppd

from landregistry.datamodel.iris import *


OTHER_PROPERTY_TYPE = 'OTHER'
PPD_PROPERTY_TYPES = {
    'SEMI-DETACHED': OBE_BUILDING,
    'TERRACED': OBE_BUILDING,
    'DETACHED': OBE_BUILDING,
    'FLAT-MAISONETTE': OBE_FLAT,
    'OTHER': OTHER_PROPERTY_TYPE
}
