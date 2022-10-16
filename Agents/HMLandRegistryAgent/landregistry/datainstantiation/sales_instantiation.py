################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 12 Oct 2022                            #
################################################

# The purpose of this module is to instantiate/update data retrieved from
# the HM Land Registry Open Data SPARQL endpoint according to OntoBuiltEnv

import re
import json
import uuid
import numpy as np
import pandas as pd
from fuzzywuzzy import fuzz, process

import agentlogging

from landregistry.datamodel.iris import *
from landregistry.datamodel.data_mapping import *
from landregistry.errorhandling.exceptions import KGException
from landregistry.kgutils.kgclient import KGClient
from landregistry.utils.api_endpoints import HM_SPARQL_ENDPOINT
from landregistry.utils.stack_configs import QUERY_ENDPOINT, UPDATE_ENDPOINT
from landregistry.kgutils.querytemplates import *

# Initialise logger
logger = agentlogging.get_logger("prod")


def update_transaction_records(property_iris=None, min_conf_score=None,
                               api_endpoint=HM_SPARQL_ENDPOINT,
                               query_endpoint=QUERY_ENDPOINT, 
                               update_endpoint=UPDATE_ENDPOINT, 
                               kgclient_obe=None, kgclient_hm=None):
    """
        Retrieves HM Land Registry's Price Paid data for provided properties from
        given endpoint and instantiates them in the KG according to OntoBuiltEnv

    Arguments:
        property_iris - list of property IRIs for which to retrieve sales data
                        (i.e. retrieval of sales data done via address matching)
        min_conf_score - minimum confidence score for address matching [0-100]
        api_endpoint - SPARQL endpoint for HM Land Registry Open Data
        query_endpoint/update_endpoint - SPARQL endpoint with instantiated OntoCityGml buildings
    Returns:
        Tuple of newly instantiated and updated sales transactions (new, updated)
    
    """

    # Initialise return values
    instantiated_tx = 0
    updated_tx = 0

    # Initialise KG clients
    if not kgclient_obe:
        kgclient_obe = KGClient(query_endpoint, update_endpoint)
    if not kgclient_hm:
        kgclient_hm = KGClient(api_endpoint, api_endpoint)

    # 1) Retrieve location information for properties from list
    #    (i.e. required for query to HM Land Registry SPARQL endpoint)
    query = get_instantiated_properties_with_location_info(property_iris=property_iris)
    res = kgclient_obe.performQuery(query)

    # Create DataFrame from results and condition data
    obe = create_conditioned_dataframe_obe(res)

    # Query Price Paid Data postcode by postcode (compromise between query speed and number of queries)
    for pc in obe['postcode'].unique():
        pc_data = obe[obe['postcode'] == pc].copy()
        pc_data.drop_duplicates(inplace=True)

        # 2) Retrieve Price Paid Data transaction records from HM Land Registry
        query = get_transaction_data_for_postcodes(postcodes=[pc])
        res = kgclient_hm.performQuery(query)

        # Create DataFrame from results and condition data
        ppd = create_conditioned_dataframe_ppd(res)

        # 3) Match addresses and retrieve transaction details
        matched_tx = get_best_matching_transactions(obe_data=pc_data, ppd_data=ppd,
                                                    min_match_score=min_conf_score)

        # 4) Update transaction records in KG
        for tx in matched_tx:
            if tx['tx_iri']:
                updated_tx += 1
            else:
                instantiated_tx += 1
            update_query = update_transaction_record(**tx)
            kgclient_obe.performUpdate(update_query)
    
    return instantiated_tx, updated_tx


def update_all_transaction_records(min_conf_score=None,
                                   api_endpoint=HM_SPARQL_ENDPOINT,
                                   query_endpoint=QUERY_ENDPOINT, 
                                   update_endpoint=UPDATE_ENDPOINT):
    """
    Retrieves EPC data for provided certificate from given endpoint and 
    instantiates it in the KG according to OntoBuiltEnv

    Arguments:
        lmk_key - certificate id (i.e. individual lodgement identifier)
        epc_endpoint (str) - EPC endpoint from which to retrieve data
                             ['domestic', 'non-domestic', 'display']
        ocgml_endpoint - SPARQL endpoint with instantiated OntoCityGml buildings
    Returns:
        Tuple of newly instantiated and updated EPCs (new, updated)
    """
    
    # Initialise return values
    updated_tx = 0
    updated_indices = 0

    # Initialise KG clients
    kgclient = KGClient(query_endpoint, update_endpoint)

    # 1) Retrieve all instantiated properties

    # 2) Retrieve location information for all instantiated properties
    #    (i.e. required for query to HM Land Registry SPARQL endpoint)

    # 3) Retrieve transaction records from HM Land Registry

    # 4) Update transaction records in KG

    # 5) Retrieve Property Price Index from HM Land Registry

    # 6) Update Property Price Index in KG
    
    return (updated_tx, updated_indices)


def create_conditioned_dataframe_obe(sparql_results:list) -> pd.DataFrame:
        """
            Parse SPARQL results from OBE namespace (i.e. instantiated EPC data)
            as DataFrame and condition data to ensure proper comparison with 
            HM Land Registry Data

            Arguments:
                sparql_results - list of dicts with following keys
                                 [property_iri, address_iri, postcode_iri, district_iri,
                                  property_type, postcode, street, number, bldg_name, unit_name]
            Returns:
                DataFrame with dict keys as columns
        """
        
        cols = ['property_iri', 'address_iri', 'postcode_iri', 'district_iri',
                'property_type', 'postcode', 'street', 'number', 'bldg_name', 
                'unit_name', 'tx_iri']
        df = pd.DataFrame(columns=cols, data=sparql_results)

        # Align missing values
        df = df.replace('nan', None)
        df = df.replace('NAN', None)
        df = df.replace('', None)
        df = df.replace({np.nan: None})

        # Ensure all strings are upper case
        df['postcode'] = df['postcode'].str.upper()
        df['street'] = df['street'].str.upper()
        df['number'] = df['number'].str.upper()        
        df['bldg_name'] = df['bldg_name'].str.upper()
        df['unit_name'] = df['unit_name'].str.upper()

        # Fill missing data with whitespace
        df[['street', 'number', 'bldg_name', 'unit_name']] = \
            df[['street', 'number', 'bldg_name', 'unit_name']].fillna(' ')
        # Create column with consolidated address string
        df['epc_address'] = [' '.join(a) for a in zip(df['street'], df['number'], 
                                                      df['bldg_name'], df['unit_name'])]
        # Remove unnecessary whitespaces
        df['epc_address'] = df['epc_address'].apply(lambda x: ' '.join(x.split()))

        return df


def create_conditioned_dataframe_ppd(sparql_results:list) -> pd.DataFrame:
        """
            Parse SPARQL results from HM endpoint (i.e. transaction data)
            as DataFrame and condition data to ensure proper comparison with 
            instantiated (address) data

            Arguments:
                sparql_results - list of dicts with following keys
                                 [tx_iri, price, date, property_type, tx_category,
                                  address_iri, paon, saon, street, town, postcode, 
                                  district, county]
            Returns:
                DataFrame with dict keys as columns
        """

        def replace_ands(nr_string):
            # Align representation of multiple numbers in address
            updated = nr_string
            if isinstance(updated, str):
                to_remove = ['AND', '&']
                for t in to_remove:
                    match = re.search(f'\d+\w*\s*{t}.\s*\d+', updated)
                    if match: 
                        rep = re.sub(r'[0-9]+', '', match.group())
                        updated = updated.replace(rep, '-')    
            return updated

        cols = ['tx_iri', 'price', 'date', 'property_type', 'tx_category',
                'address_iri', 'paon', 'saon', 'street', 'town', 'postcode', 
                'district', 'county']
        df = pd.DataFrame(columns=cols, data=sparql_results)

        # Align missing values
        df = df.replace('nan', None)
        df = df.replace('NAN', None)
        df = df.replace('', None)
        df = df.replace({np.nan: None})

        # Keep only latest transaction record per address
        df.sort_values(by=['date'], ascending = False, inplace=True)
        df.drop_duplicates(subset=['address_iri'], keep='first', inplace=True)

        # Ensure all strings are upper case
        df['street'] = df['street'].str.upper()
        df['paon'] = df['paon'].str.upper()        
        df['saon'] = df['saon'].str.upper()
        df['property_type'] = df['property_type'].str.upper()

        # Align representation of multiple numbers in address to 'X-Y'
        df['paon'] = df['paon'].apply(replace_ands)
        df['saon'] = df['saon'].apply(replace_ands)

        # Fill missing data with whitespace
        df[['street', 'paon', 'saon']] = df[['street', 'paon', 'saon']].fillna(' ')
        # Create column with consolidated address string
        df['ppd_address'] = [' '.join(a) for a in zip(df['street'], df['paon'],
                                                      df['saon'])]
        # Remove unnecessary whitespaces
        df['ppd_address'] = df['ppd_address'].apply(lambda x: ' '.join(x.split()))

        # Map property type to OBE property type
        df['property_type'] = df['property_type'].map(PPD_PROPERTY_TYPES)

        return df


def get_best_matching_transactions(obe_data, ppd_data, min_match_score=None) -> list:
    """
        Find best match of instantiated address (i.e. OBE address) within 
        Price Paid Data set queried from HM Land Registry and extract respective
        transaction details

        Matching procedure:
            1) MUST MATCH: postcode + property type
                           (postcode checking here not necessary as only called 
                            for data from same postcode)
            2) FUZZY MATCH: concatenated string of "street + number + building name + unit name"
               (Matching done using fuzzywuzzy based on Levenshtein Distance)

        Arguments:
            obe_data - Conditioned (address) data retrieved from KG
                       (as returned by `create_conditioned_dataframe_obe`)
            ppd_data - Conditioned (address) data retrieved from HM Land Registry
                       (as returned by `create_conditioned_dataframe_ppd`)
            min_match_score - Minimum fuzzy match score required before being
                              considered as matched address
        Returns:
            List of dict(s) with transaction details to be updated/instantiated
    """
    
    # Initialise return data, i.e. instantiated data + data to instantiate
    cols = [
        # Already instantiated data (as reference/connection point)
        'property_iri', 'address_iri', 'tx_iri',
        # Data to instantiate
        'new_tx_iri', 'price', 'date', 'ppd_address_iri'
    ]
    inst_list = []

    for index, row in obe_data.iterrows():
        # Extract consolidated address for matching
        addr = row['epc_address']

        # Initialise data to instantiate
        to_inst = {c: None for c in cols}
        to_inst['property_iri'] = row['property_iri']
        to_inst['address_iri'] = row['address_iri']
        to_inst['tx_iri'] = row['tx_iri']
        
        # Extract PPD addresses of same property type (in same postcode)
        prop = row['property_type']
        ppd_addr = ppd_data[ppd_data['property_type'].isin([prop, OTHER_PROPERTY_TYPE])]

        # Extract list of PPD addresses
        ppd_addr = ppd_addr['ppd_address'].tolist()

        # Find best match
        if min_match_score:
            matches = process.extract(addr, ppd_addr, scorer=fuzz.token_set_ratio)
            matches = [m for m in matches if m[1] >= min_match_score]
            if matches:
                matches.sort(key=lambda x: x[1], reverse=True)
                best = matches[0]
        else:
            best = process.extractOne(addr, ppd_addr, scorer=fuzz.token_set_ratio)
        if best:
            tx_data = ppd_data[ppd_data['ppd_address'] == best[0]]
            to_inst['new_tx_iri'] = tx_data['tx_iri'].values[0]
            to_inst['price'] = tx_data['price'].values[0]
            to_inst['date'] = tx_data['date'].values[0]
            to_inst['ppd_address_iri'] = tx_data['address_iri'].values[0]

        # Append to list
        inst_list.append(to_inst)

    return inst_list
