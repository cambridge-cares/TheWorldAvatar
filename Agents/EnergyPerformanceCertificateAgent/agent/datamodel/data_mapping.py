################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 15 Sep 2022                            #
################################################

# The purpose of this module is to provide a mapping between retrieved 
# observation variables from Met Office (via metoffer) to corresponding
# concepts and units as defined in OntoEMS

from agent.datamodel.iris import *


# Mapping of EPC data keys: {key from api : key mapping}
EPC_KEYS = {
    ### Domestic EPC data
    'lmk-key': 'epc_lmkkey',
    'uprn': 'uprn',
    'current-energy-rating': 'epc_rating',
    'number-habitable-rooms': 'rooms',
    'total-floor-area': 'floor_area',
    'floor-description': 'floor_description',
    'roof-description': 'roof_description',
    'walls-description': 'wall_description',
    'windows-description': 'windows_description',
}

# Mapping of EPC data values {value from api : value mapping}
EPC_DATA = {
    ### Domestic EPC data
    # Property types
    'House': OBE_HOUSE,
    'Bungalow': OBE_BUNGALOW,
    'Park home': OBE_PARKHOME,
    'Maisonette': OBE_MAISONETTE,
    # Built forms
    'Detached': OBE_DETACHED,
    'Semi-Detached': OBE_SEMI_DETACHED,
    'Mid-Terrace': OBE_TERRACED,
    'End-Terrace': OBE_TERRACED,
    'Enclosed End-Terrace': OBE_TERRACED,
    'Enclosed Mid-Terrace': OBE_TERRACED,
    # Construction period
    '1890': ('1890-01-01T00:00:00Z', '1890-12-31T23:59:59Z'),
    '1983': ('1983-01-01T00:00:00Z', '1983-12-31T23:59:59Z'),
    '2008': ('2008-01-01T00:00:00Z', '2008-12-31T23:59:59Z'),
    '2010': ('2010-01-01T00:00:00Z', '2010-12-31T23:59:59Z'),
    '2011': ('2011-01-01T00:00:00Z', '2011-12-31T23:59:59Z'),
    '2012': ('2012-01-01T00:00:00Z', '2012-12-31T23:59:59Z'),
    '2013': ('2013-01-01T00:00:00Z', '2013-12-31T23:59:59Z'),
    '2014': ('2014-01-01T00:00:00Z', '2014-12-31T23:59:59Z'),
    '2015': ('2015-01-01T00:00:00Z', '2015-12-31T23:59:59Z'),
    '2016': ('2016-01-01T00:00:00Z', '2016-12-31T23:59:59Z'),
    '2017': ('2017-01-01T00:00:00Z', '2017-12-31T23:59:59Z'),
    '2018': ('2018-01-01T00:00:00Z', '2018-12-31T23:59:59Z'),
    '2019': ('2019-01-01T00:00:00Z', '2019-12-31T23:59:59Z'),
    '2020': ('2020-01-01T00:00:00Z', '2020-12-31T23:59:59Z'),
    '2021': ('2021-01-01T00:00:00Z', '2021-12-31T23:59:59Z'),
    '2022': ('2022-01-01T00:00:00Z', '2022-12-31T23:59:59Z'), 
    'England and Wales: 1900-1929': ('1900-01-01T00:00:00Z', '1929-12-31T23:59:59Z'),
    'England and Wales: 1930-1949': ('1930-01-01T00:00:00Z', '1949-12-31T23:59:59Z'),
    'England and Wales: 1950-1966': ('1950-01-01T00:00:00Z', '1966-12-31T23:59:59Z'),
    'England and Wales: 1967-1975': ('1967-01-01T00:00:00Z', '1975-12-31T23:59:59Z'),
    'England and Wales: 1976-1982': ('1976-01-01T00:00:00Z', '1982-12-31T23:59:59Z'),
    'England and Wales: 1983-1990': ('1983-01-01T00:00:00Z', '1990-12-31T23:59:59Z'),
    'England and Wales: 1991-1995': ('1991-01-01T00:00:00Z', '1995-12-31T23:59:59Z'),
    'England and Wales: 1996-2002': ('1996-01-01T00:00:00Z', '2002-12-31T23:59:59Z'),
    'England and Wales: 2003-2006': ('2003-01-01T00:00:00Z', '2006-12-31T23:59:59Z'), 
    'England and Wales: 2007 onwards': ('2007-01-01T00:00:00Z', None),
    'England and Wales: 2007-2011': ('2007-01-01T00:00:00Z', '2011-12-31T23:59:59Z'),
    'England and Wales: 2012 onwards': ('2012-01-01T00:00:00Z', None),
    'England and Wales: before 1900': (None, '1900-12-31T23:59:59Z')
}

# Mapping of units to OM units and symbols
UNITS_MAPPING = {
     OM_AREA: (OM_M2, 'm2'),
     OM_HEIGHT: (OM_M, 'm'),
     OM_AMOUNT_MONEY: (OM_GBP, '£')
}

# Define full coordinate reference systems (CRS) for pyproj
CRSs = {'EPSG:27700': 'urn:ogc:def:crs:EPSG::27700',
        'EPSG:4326': 'urn:ogc:def:crs:EPSG::4326',
        'crs_84': 'urn:ogc:def:crs:OGC::CRS84'
        }

# Define typical unit, property and street names (required for address extraction from EPC data)
NAMES_UNITS = ['flat', 'apartment']
NAMES_BLDGS = ['house', 'bungalow', 'farm', 'lodge', 'cottage', 'villa', 
                'chalet', 'barn', 'cabin', 'hall', 'court', 'hotel', 'grange',
                'annex']
NAMES_STREET = ['road', 'street', 'avenue', 'lane', 'close', 'way', 'court',
                'drive', 'drove', 'walk', 'square', 'place', 'lane', 'bank',
                'highway']
