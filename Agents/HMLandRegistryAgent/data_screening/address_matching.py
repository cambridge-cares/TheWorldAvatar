###############################################
# Author: Markus Hofmeister (mh807@cam.ac.uk) #
# Date: 14 Oct 2022                           #
###############################################

""" 
This module investigates how to best match address data between data from the 
EPC API: https://epc.opendatacommunities.org/docs/guidance#glossary_domestic
HM Land Registry Price Paid Data: https://landregistry.data.gov.uk/app/root/doc/ppd
"""


import re
import numpy as np
import pandas as pd


# File paths to previously downloaded data
epc_file = 'domestic_epcs.csv'
ppd_file = 'housing_market.csv'


#####################################################################################

# Dictionaries to match EPC & PPD property types to buildings vs units
BUILDING = 'bldg'
UNIT = 'unit'
epc_properties = {
    'HOUSE': BUILDING,
    'BUNGALOW': BUILDING,
    'PARK HOME': BUILDING,
    'FLAT': UNIT,
    'MAISONETTE': UNIT
}
ppd_properties = {
    'SEMI-DETACHED': BUILDING,
    'TERRACED': BUILDING,
    'DETACHED': BUILDING,
    'FLAT-MAISONETTE': UNIT
    # Could be both
    #'OTHER'
}

# Define typical unit, property and street names
names_units = ['flat', 'apartment']
names_bldgs = ['house', 'bungalow', 'farm', 'lodge', 'cottage', 'villa', 
               'chalet', 'barn', 'cabin', 'hall', 'court', 'hotel', 'grange',
               'annex']
names_street = ['road', 'street', 'avenue', 'lane', 'close', 'way', 'court',
                'drive', 'drove', 'walk', 'square', 'place', 'lane', 'bank',
                'highway']

def split_address_info(address_info: str):
    # Splits address text with potential number into textual and numeric part
    # If numeric parts contains '-' extract entire part as number
    if re.search(r'\d+\s*-\s*\d+', address_info):
        p = r'\d+\s*-*\s*\d+\w*'
    else:
        p = r'\d+\w*'
    splitted = re.findall(p, address_info)
    if splitted: 
        number = splitted[0]
        description = address_info.replace(number, '')
        description = description.replace(',', '').replace(';','')
        description = description.strip().upper()
        return (description, number)
    else:
        return (address_info, None)


#####################################################################################

# Read in data & keep only latest records for same addresses
df_epc = pd.read_csv(epc_file)
df_epc.sort_values(by=['inspection-date'], inplace=True)
df_epc.drop_duplicates(subset=['uprn'], keep='first', inplace=True)
df_epc = df_epc[['address1', 'address2', 'address3', 'postcode', 'property-type']]

df_ppd = pd.read_csv(ppd_file)
df_ppd.sort_values(by=['date'], inplace=True)
df_ppd.drop_duplicates(subset=['addr'], keep='first', inplace=True)
df_ppd = df_ppd[['paon', 'saon', 'street', 'postcode', 'town', 'district', 'county',
                 'property']]

# Condition data
df_epc = df_epc.astype(str).apply(lambda col: col.str.upper())
df_ppd = df_ppd.astype(str).apply(lambda col: col.str.upper())

# Fill missing data with None
df_epc = df_epc.replace('NAN', None)
df_epc = df_epc.replace('', None)
df_epc = df_epc.replace({np.nan: None})
df_ppd = df_ppd.replace('NAN', None)
df_ppd = df_ppd.replace('', None)
df_ppd = df_ppd.replace({np.nan: None})

# Initialise DataFrame to instantiate
cols = ['postcode', 'street', 'number', 'bldg_name', 'unit_name', 'match']
to_inst = []

# Ensure upper case letters for typical unit, property and street names
names_units = [i.upper() for i in names_units]
names_bldgs = [i.upper() for i in names_bldgs]
names_street = [i.upper() for i in names_street]

i = 0
n = len(df_epc['postcode'].unique())
for pc in df_epc['postcode'].unique():
    i += 1
    print(f'Postcode {i:>6}/{n:>6}: {pc}')

    epc_addr = df_epc[df_epc['postcode'] == pc]
    ppd_addr = df_ppd[df_ppd['postcode'] == pc]

    for index, row in epc_addr.iterrows():

        # Intialise data to instantiate
        row_to_inst = {c: None for c in cols}
        row_to_inst['postcode'] = pc

        # Extract address fields
        field1 = row.get('address1')
        addr2 = row.get('address2')
        addr3 = row.get('address3')

        # ASSUMPTION: Address information provided in decreasing granularity, i.e.
        # order: flat information - building information - street information
        if field1:
            # If only first address field is provided -> extract (number and) street
            if not addr2 and not addr3:
                row_to_inst['street'], row_to_inst['number'] = split_address_info(field1)
            else:
                # If two address fields are provided -> extract additional unit/building info
                if (addr2 and not addr3) or (addr3 and not addr2):
                    field2 = addr2 if addr2 else addr3
                    # 1) For buildings try to extract building name + street info
                    if epc_properties[row['property-type']] == BUILDING:
                        # If 1st address field contains typical building name and 2nd field
                        # contains splittable street information (this should cover most cases)
                        # -> field 1: building info, field 2: street info
                        if any(b in field1 for b in names_bldgs) and all(split_address_info(field2)):
                            row_to_inst['street'], row_to_inst['number'] = split_address_info(field2)
                            row_to_inst['bldg_name'] = field1
                        # If 2nd field does not contain splittable street information, but typical street name
                        # while 1st field does not contain typical street name
                        # -> field 1: building info, field 2: street info
                        elif any(s in field2 for s in names_street) and not any(s in field1 for s in names_street):
                                row_to_inst['street'], row_to_inst['number'] = split_address_info(field2)
                                row_to_inst['bldg_name'] = field1
                        else:
                            # If 2nd field does not contain typical street information
                            # -> field 1: street info and field 2 likely neglectable (i.e. town name)
                            row_to_inst['street'], row_to_inst['number'] = split_address_info(field1)
                    # 2) For units try to extract unit name + building name + street info
                    else:
                        # If 1st address field contains typical unit name and 2nd field
                        # contains splittable street information (this should cover most cases)
                        # -> field 1: unit info, field 2: street info
                        if any(u in field1 for u in names_units) and all(split_address_info(field2)):
                            row_to_inst['street'], row_to_inst['number'] = split_address_info(field2)
                            row_to_inst['unit_name'] = field1
                        # If 2nd field does not contain splittable street information, but typical street name
                        # -> field 1: unit info, field 2: street info
                        elif any(u in field1 for u in names_units) and any(s in field2 for s in names_street):
                                row_to_inst['street'], row_to_inst['number'] = split_address_info(field2)
                                row_to_inst['unit_name'] = field1
                        # If 2nd field does not contain splittable street information, but typical building name
                        # -> field 1: unit info, field 2: building info
                        elif any(u in field1 for u in names_units) and any(b in field2 for b in names_bldgs):
                                row_to_inst['bldg_name'] = field2
                                row_to_inst['unit_name'] = field1
                        else:
                            # If 2nd field does not contain typical street/building information
                            # -> field 1: street info and field 2 likely neglectable (i.e. town name)
                            row_to_inst['street'], row_to_inst['number'] = split_address_info(field1)
                else:
                    # If three address fields are provided -> extract additional unit/building info
                    field2 = addr2 
                    field3 = addr3
                    # If first 2 address fields contain typical unit and building name and 3rd field
                    # contains splittable street information (this should cover most cases)
                    # -> field 1/2: building/unit info, field 3: street info
                    if all(split_address_info(field3)):
                        if any(u in field1 for u in names_units) and any(b in field2 for b in names_bldgs):
                            row_to_inst['street'], row_to_inst['number'] = split_address_info(field3)
                            row_to_inst['bldg_name'] = field2
                            row_to_inst['unit_name'] = field1
                        elif any(u in field2 for u in names_units) and any(b in field1 for b in names_bldgs):
                            row_to_inst['street'], row_to_inst['number'] = split_address_info(field3)
                            row_to_inst['bldg_name'] = field1
                            row_to_inst['unit_name'] = field2
                    # If 3rd field does not contain splittable street information, but typical street name
                    # while 2nd field does not contain typical street name
                    # -> field 1/2: building/unit info, field 3: street info
                    elif any(s in field3 for s in names_street) and not \
                        (any(s in field2 for s in names_street) and all(split_address_info(field2))):
                        # Extract most granualar street information
                        if any(s in field2 for s in names_street):
                            if any(u in field1 for u in names_units):
                                row_to_inst['street'], row_to_inst['number'] = split_address_info(field2)
                                row_to_inst['unit_name'] = field1
                            if any(b in field1 for b in names_bldgs):
                                row_to_inst['street'], row_to_inst['number'] = split_address_info(field2)
                                row_to_inst['bldg_name'] = field1
                        else:
                            if any(u in field1 for u in names_units):
                                row_to_inst['street'], row_to_inst['number'] = split_address_info(field3)
                                row_to_inst['unit_name'] = field1
                                if any(b in field2 for b in names_bldgs):
                                    row_to_inst['bldg_name'] = field2
                            if any(b in field1 for b in names_bldgs):
                                row_to_inst['street'], row_to_inst['number'] = split_address_info(field3)
                                row_to_inst['bldg_name'] = field1
                                if any(u in field2 for u in names_units):
                                    row_to_inst['unit_name'] = field2
                    else:
                        # If 3rd field does not contain typical street information, consider only fields 1/2
                        if epc_properties[row['property-type']] == UNIT:
                            # If 1st address field contains typical unit name and 2nd field
                            # contains splittable street information (this should cover most cases)
                            # -> field 1: unit info, field 2: street info
                            if any(u in field1 for u in names_units) and all(split_address_info(field2)):
                                row_to_inst['street'], row_to_inst['number'] = split_address_info(field2)
                                row_to_inst['unit_name'] = field1
                            # If 2nd field does not contain splittable street information, but typical street name
                            # -> field 1: unit info, field 2: street info
                            elif any(u in field1 for u in names_units) and any(s in field2 for s in names_street):
                                    row_to_inst['street'], row_to_inst['number'] = split_address_info(field2)
                                    row_to_inst['unit_name'] = field1
                            # If 2nd field does not contain splittable street information, but typical building name
                            # -> field 1: unit info, field 2: building info
                            elif any(u in field1 for u in names_units) and any(b in field2 for b in names_bldgs):
                                    row_to_inst['bldg_name'] = field2
                                    row_to_inst['unit_name'] = field1
                            else:
                                # If 2nd field does not contain typical street/building information
                                # -> field 1: street info and field 2 likely neglectable (i.e. town name)
                                row_to_inst['street'], row_to_inst['number'] = split_address_info(field1)
                        else:
                            # If 1st address field contains typical building name and 2nd field
                            # contains splittable street information 
                            # -> field 1: building info, field 2: street info
                            if any(u in field1 for u in names_bldgs) and all(split_address_info(field2)):
                                row_to_inst['street'], row_to_inst['number'] = split_address_info(field2)
                                row_to_inst['bldg_name'] = field1
                            # If 2nd field does not contain splittable street information, but typical street name
                            # -> field 1: building info, field 2: street info
                            elif any(u in field1 for u in names_bldgs) and any(s in field2 for s in names_street):
                                    row_to_inst['street'], row_to_inst['number'] = split_address_info(field2)
                                    row_to_inst['bldg_name'] = field1
                            else:
                                # If 2nd field does not contain typical street/building information
                                # -> field 1: street info and field 2 likely neglectable (i.e. town name)
                                row_to_inst['street'], row_to_inst['number'] = split_address_info(field1)
        
            # Append extracted data dict to list
            to_inst.append(row_to_inst)

# Create dataframe from list of dicts
df = pd.DataFrame(columns=cols, data=to_inst)    
df.to_csv('epc_addresses_extracted.csv', index=False)
