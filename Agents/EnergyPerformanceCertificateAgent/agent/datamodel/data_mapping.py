################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 15 Sep 2022                            #
################################################

# The purpose of this module is to provide a mapping between retrieved data
# from EPC APIs and corresponding concepts and units as defined in OntoBuiltEnv

from agent.datamodel.iris import *


# OM/UOM SYMBOLS
GBP_PER_SM = '£ m-2'
GBP = '£'
METRE = 'm'
METRE_SQ = 'm2'

# Create IRIs for recurring property types and built forms
ABOX_UUID = '_b5997912-1035-4284-ad40-cde44e438485'
ABOX = {# Built forms
        OBE_DETACHED: OBE_DETACHED + ABOX_UUID,
        OBE_SEMI_DETACHED : OBE_SEMI_DETACHED + ABOX_UUID,
        OBE_TERRACED: OBE_TERRACED + ABOX_UUID,
        # Property types
        OBE_HOUSE: OBE_HOUSE + ABOX_UUID,
        OBE_BUNGALOW: OBE_BUNGALOW + ABOX_UUID,
        OBE_PARKHOME: OBE_PARKHOME + ABOX_UUID,
        OBE_MAISONETTE: OBE_MAISONETTE + ABOX_UUID,
        OBE_FLAT: OBE_FLAT + ABOX_UUID
}

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

    ### Non-domestic EPC data
    'asset-rating-band': 'epc_rating',
    'floor-area': 'floor_area',

    ### Display EPC data
    'operational-rating-band': 'epc_rating'
}

# Mapping of EPC data values {value from api : value mapping}
EPC_DATA = {
    ### Domestic EPC data
    # Property types
    'House': OBE_HOUSE,
    'Bungalow': OBE_BUNGALOW,
    'Park home': OBE_PARKHOME,
    'Maisonette': OBE_MAISONETTE,
    'Flat': OBE_FLAT,
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
    'England and Wales: before 1900': (None, '1900-12-31T23:59:59Z'),

    ### Non-domestic EPC data
    # Property types - mapped as OBE Property Usage
    'A1/A2 Retail and Financial/Professional services': OBE_RETAILESTABLISHMENT,                       
    'A3/A4/A5 Restaurant and Cafes/Drinking Establishments and Hot Food takeaways': OBE_EATINGESTABLISHMENT, 
    'Airport terminals': OBE_TRANSPORTFACILITY,              
    'B1 Offices and Workshop businesses': OBE_OFFICE,        
    'B2 to B7 General Industrial and Special Industrial Groups': OBE_INDUSTRIALFACILITY, 
    'B8 Storage or Distribution': OBE_INDUSTRIALFACILITY,	               
    'Bus station/train station/seaport terminal': OBE_TRANSPORTFACILITY,
    'C1 Hotels': OBE_HOTEL,                                  
    'C2 Residential Institutions - Hospitals and Care Homes': OBE_HOSPITAL,
    'C2 Residential Institutions - Residential schools': OBE_SCHOOL,
    'C2 Residential Institutions - Universities and colleges': OBE_UNIVERSITYFACILITY,
    'D1 Non-residential Institutions - Community/Day Centre': OBE_CULTURALFACILITY,
    'D1 Non-residential Institutions - Crown and County Courts': OBE_CULTURALFACILITY,
    'D1 Non-residential Institutions - Education': OBE_EDUCATION,
    'D1 Non-residential Institutions - Libraries Museums and Galleries': OBE_CULTURALFACILITY,
    'D1 Non-residential Institutions - Primary Health Care Building': OBE_CLINIC,
    'D2 General Assembly and Leisure plus Night Clubs and Theatres': OBE_DRINKINGESTABLISHMENT,
    'Emergency services': OBE_EMERGENCYSERVICE,
    'Further education universities': OBE_UNIVERSITYFACILITY,
    'Hospital': OBE_HOSPITAL,
    'Hotel': OBE_HOTEL,
    'Industrial process building': OBE_INDUSTRIALFACILITY,
    'Libraries/museums/galleries': OBE_CULTURALFACILITY,
    'Nursing residential homes and hostels': OBE_MEDICALCARE,
    'Office': OBE_OFFICE,
    'Others - Car Parks 24 hrs': OBE_NON_DOMESTIC,
    'Others - Emergency services': OBE_EMERGENCYSERVICE,
    'Others - Passenger terminals': OBE_TRANSPORTFACILITY,
    'Primary health care buildings': OBE_MEDICALCARE,
    'Primary school': OBE_SCHOOL,
    'Restaurant/public house': OBE_EATINGESTABLISHMENT,
    'Retail': OBE_RETAILESTABLISHMENT,                      
    'Retail warehouses': OBE_RETAILESTABLISHMENT,           
    'Secondary school': OBE_SCHOOL, 	                    
    'Social clubs': OBE_CULTURALFACILITY,
    'Sports centre/leisure centre': OBE_SPORTSFACILITY,
    'Sports ground arena': OBE_SPORTSFACILITY, 
    'Theatres/cinemas/music halls and auditoria': OBE_CULTURALFACILITY, 
    'C2A Secure Residential Institutions': OBE_DOMESTIC,
    'Community/day centre': OBE_CULTURALFACILITY,
 	'Crown and county courts': OBE_NON_DOMESTIC,
    'Dwelling': OBE_DOMESTIC,
    'Launderette': OBE_NON_DOMESTIC,
    'Miscellaneous 24hr activities': OBE_NON_DOMESTIC,
    'Others - Miscellaneous 24hr activities': OBE_NON_DOMESTIC,
    'Others -Telephone exchanges': OBE_NON_DOMESTIC,
    'Others - Stand alone utility block': OBE_NON_DOMESTIC,
    'Residential spaces': OBE_DOMESTIC,
    'Prisons': OBE_NON_DOMESTIC,
    'Telephone exchanges': OBE_NON_DOMESTIC,
    'Workshops/maintenance depot': OBE_INDUSTRIALFACILITY,
    'Warehouse and storage': OBE_INDUSTRIALFACILITY,
    'Retail/Financial and Professional Services': OBE_RETAILESTABLISHMENT,
    'Restaurants and Cafes/Drinking Establishments/Takeaways': OBE_EATINGESTABLISHMENT,
    'Non-residential Institutions: Libraries, Museums, and Galleries': OBE_CULTURALFACILITY,
    'General Industrial and Special Industrial Groups': OBE_INDUSTRIALFACILITY,
    'Offices and Workshop Businesses': OBE_OFFICE,
    'Hotels': OBE_HOTEL,
    'General Assembly and Leisure, Night Clubs, and Theatres': OBE_DRINKINGESTABLISHMENT,
    'Residential Institutions: Hospitals and Care Homes': OBE_HOSPITAL,
    
    ### Display EPC data 
    # Building category codes (explanation in comments)    
    'C1': OBE_OFFICE,                       # General Office
    'C2': OBE_OFFICE,                       # High Street Agency
    'C3': OBE_RETAILESTABLISHMENT,          # General Retail
    'C4': OBE_RETAILESTABLISHMENT,          # Large Non-Food Shop
    'C5': OBE_RETAILESTABLISHMENT,          # Small Food Store
    'C6': OBE_RETAILESTABLISHMENT,          # Large Food Store
    'H1': OBE_EATINGESTABLISHMENT,          # Restaurant
    'H2': OBE_DRINKINGESTABLISHMENT,        # Bar, Pub Or Licensed Club
    'H3': OBE_HOTEL,                        # Hotel
    'H4': OBE_CULTURALFACILITY,             # Cultural Activities
    'H5': OBE_CULTURALFACILITY,             # Entertainment Halls
    'H6': OBE_SPORTSFACILITY,               # Swimming Pool Centre
    'H7': OBE_SPORTSFACILITY,               # Fitness And Health Centre
    'H8': OBE_SPORTSFACILITY,               # Dry Sports And Leisure Facility
    'S1': OBE_TRANSPORTFACILITY,            # Covered Car Park
    'S2': OBE_NON_DOMESTIC,                 # Public Buildings With Light Usage
    'S3': OBE_SCHOOL,                       # Schools And Seasonal Public Buildings
    'S4': OBE_UNIVERSITYFACILITY,           # University Campus
    'S5': OBE_CLINIC,                       # Clinic
    'S6': OBE_HOSPITAL,                     # Hospital - Clinical And Research
    'S7': OBE_DOMESTIC,                     # Long Term Residential
    'S8': OBE_DOMESTIC,                     # General Accommodation
    'S9': OBE_EMERGENCYSERVICE,             # Emergency Services
    'S10': OBE_MEDICALCARE,                 # Laboratory Or Operating Theatre
    'W1': OBE_NON_DOMESTIC,                 # Public waiting or circulation
    'W2': OBE_TRANSPORTFACILITY,            # Terminal
    'W3': OBE_NON_DOMESTIC,                 # Workshop
    'W4': OBE_NON_DOMESTIC,                 # Storage Facility
    'W5': OBE_RETAILESTABLISHMENT           # Cold Storage
}

# Usage mapping for PostGIS
USAGE_MAPPING = {
    OBE_OFFICE: OBE_NON_DOMESTIC,
    OBE_RETAILESTABLISHMENT: OBE_NON_DOMESTIC,
    OBE_RELIGIOUSFACILITY: OBE_NON_DOMESTIC,
    OBE_INDUSTRIALFACILITY: OBE_NON_DOMESTIC,
    OBE_EATINGESTABLISHMENT: OBE_NON_DOMESTIC,
    OBE_DRINKINGESTABLISHMENT: OBE_NON_DOMESTIC,
    OBE_HOTEL: OBE_NON_DOMESTIC,
    OBE_SPORTSFACILITY: OBE_NON_DOMESTIC,
    OBE_CULTURALFACILITY: OBE_NON_DOMESTIC,
    OBE_TRANSPORTFACILITY: OBE_NON_DOMESTIC,
    OBE_FIRESTATION: OBE_EMERGENCYSERVICE,
    OBE_POLICESTATION: OBE_EMERGENCYSERVICE,
    OBE_HOSPITAL: OBE_MEDICALCARE,
    OBE_CLINIC: OBE_MEDICALCARE,
    OBE_SCHOOL: OBE_EDUCATION,
    OBE_UNIVERSITYFACILITY: OBE_EDUCATION,
    OBE_SINGLERESIDENTIAL: OBE_DOMESTIC,
    OBE_MULTIRESIDENTIAL: OBE_DOMESTIC
}

# Mapping of units to OM units and symbols
UNITS_MAPPING = {
     OM_AREA: OM_M2,
     OM_HEIGHT: OM_M,
     OM_AMOUNT_MONEY: OM_GBP
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

