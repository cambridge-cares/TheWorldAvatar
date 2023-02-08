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
from landregistry.datamodel.data_mapping import TIME_FORMAT, DATACLASS
from landregistry.errorhandling.exceptions import KGException
from landregistry.kgutils.kgclient import KGClient
from landregistry.kgutils.tsclient import TSClient
from landregistry.utils.api_endpoints import HM_SPARQL_ENDPOINT
from landregistry.utils.stack_configs import QUERY_ENDPOINT, UPDATE_ENDPOINT
from landregistry.kgutils.querytemplates import *

# Initialise logger
logger = agentlogging.get_logger("prod")


def update_transaction_records(property_iris=None, min_conf_score=90,
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
    logger.info('Retrieving instantiated properties with location info ...')
    query = get_instantiated_properties_with_location_info(property_iris=property_iris)
    res = kgclient_obe.performQuery(query)

    # Create DataFrame from results and condition data
    obe = create_conditioned_dataframe_obe(res)

    # Query Price Paid Data postcode by postcode (compromise between query speed and number of queries)
    logger.info('Querying HM Land Registry Price Paid Data in batches of individual postcodes ...')
    for pc in obe['postcode'].unique():
        pc_data = obe[obe['postcode'] == pc].copy()
        pc_data.drop_duplicates(inplace=True)

        # 2) Retrieve Price Paid Data transaction records from HM Land Registry
        logger.info('Retrieve Price Paid Data from Open SPARQL endpoint.')
        query = get_transaction_data_for_postcodes(postcodes=[pc])
        res = kgclient_hm.performQuery(query)

        # Create DataFrame from results and condition data
        logger.info('Condition retrieved Price Paid Data in DataFrame.')
        ppd = create_conditioned_dataframe_ppd(res)

        # 3) Match addresses and retrieve transaction details
        logger.info('Match addresses between instantiated EPC data and Price Paid Data addresses ...')
        matched_tx = get_best_matching_transactions(obe_data=pc_data, ppd_data=ppd,
                                                    min_match_score=min_conf_score)

        # 4) Update transaction records in KG
        for tx in matched_tx:
            if tx['tx_iri']:
                updated_tx += 1
            else:
                instantiated_tx += 1
            update_query = update_transaction_record(**tx)
            if update_query:
                kgclient_obe.performUpdate(update_query)
    
    return instantiated_tx, updated_tx


def update_all_transaction_records(min_conf_score=90,
                                   api_endpoint=HM_SPARQL_ENDPOINT,
                                   query_endpoint=QUERY_ENDPOINT, 
                                   update_endpoint=UPDATE_ENDPOINT):
    """
    Retrieves HM Land Registry's Price Paid data for all instantiated properties from
    given endpoint and instantiates them in the KG according to OntoBuiltEnv

    Arguments:
        min_conf_score - minimum confidence score for address matching [0-100]
        api_endpoint - SPARQL endpoint for HM Land Registry Open Data
        query_endpoint/update_endpoint - SPARQL endpoint with instantiated OntoCityGml buildings
    Returns:
        Tuple of newly instantiated and updated sales transactions (new, updated)
    """
    
    # Initialise return values
    instantiated_tx = 0
    updated_tx = 0
    instantiated_ukhpi = 0
    updated_ukhpi = 0

    # Initialise KG clients
    kgclient_obe = KGClient(query_endpoint, update_endpoint)
    kgclient_hm = KGClient(api_endpoint, api_endpoint)
    # Initialise TimeSeriesClient with default settings
    ts_client = TSClient(kg_client=kgclient_obe)

    # 1) Retrieve all instantiated properties with associated postcodes
    logger.info('Retrieving instantiated properties with attached postcodes ...')
    query = get_all_properties_with_postcode()
    res = kgclient_obe.performQuery(query)

    # 2) Update transaction records in KG in postcode batches
    batch_size = 100
    df = pd.DataFrame(res)
    pcs = df['postcode'].unique().tolist()
    pcs.sort()
    postcodes = [pcs[i:i + batch_size] for i in range(0, len(pcs), batch_size)]

    logger.info('Updating transaction records (for batch of postcodes) ...')
    for pc in postcodes:
        prop_iris = df[df['postcode'].isin(pc)]['property_iri'].tolist()

        # Update transaction records for list of property IRIs
        tx_new, tx_upd = update_transaction_records(property_iris=prop_iris, 
                            min_conf_score=min_conf_score, api_endpoint=api_endpoint,
                            kgclient_obe=kgclient_obe, kgclient_hm=kgclient_hm)
        instantiated_tx += tx_new
        updated_tx += tx_upd

    # 3) Retrieve instantiated Admin Districts + potentially already instantiated
    #    Property Price Indices in OntoBuiltEnv
    logger.info('Updating UK House Price Index ...')
    districts = get_admin_district_index_dict(kgclient_obe)

    for d in districts: 
        if not d['ukhpi']:
            # Instantiate Property Price Index (i.e. time series data IRI) if
            # not existing + initialise TimeSeries
            ppi_iri = KB + 'PropertyPriceIndex_' + str(uuid.uuid4())
            d['ukhpi'] = ppi_iri
            logger.info('Instantiate UK House Price Index.')
            insert_query = instantiate_property_price_index(district_iri=d['local_authority'],
                                                            ppi_iri=ppi_iri)
            kgclient_obe.performUpdate(insert_query)
            # Initialise TimeSeries
            ts_client.tsclient.initTimeSeries([ppi_iri], [DATACLASS], TIME_FORMAT,
                                            ts_client.conn)
            instantiated_ukhpi += 1
        else:
            updated_ukhpi += 1

        # 4) Retrieve Property Price Index from HM Land Registry
        logger.info('Update UK House Price Index data.')
        ts = create_ukhpi_timeseries(local_authority_iri=d['ons_district'], 
                                     ppi_iri=d['ukhpi'], kgclient_hm=kgclient_hm)

        # 5) Update Property Price Index time series data in KG
        ts_client.tsclient.addTimeSeriesData(ts, ts_client.conn)
    
    return (instantiated_tx, updated_tx, instantiated_ukhpi, updated_ukhpi)


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
        best = None
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


def get_admin_district_index_dict(kglient) -> list:
    """
        Get dictionary of instantiated admin district IRIs and corresponding
        Property Price Index IRIs (as used by OntoBuiltEnv) 
        (including IRI of corresponding ONS admin district)

        Returns:
            List of dictionaries with keys ['local_authority', 'ons_district', 'index']
    """
    
    # Retrieve admin districts + potentially instantiated PPIs
    query = get_all_admin_districts_with_price_indices()
    res = kglient.performQuery(query)
    cols = ['local_authority', 'ons_district', 'ukhpi']
    df = pd.DataFrame(columns=cols, data=res)
    df.drop_duplicates(inplace=True)

    # Align missing values to None
    df = df.replace('nan', None)
    df = df.replace('NAN', None)
    df = df.replace('', None)
    df = df.replace({np.nan: None})

    # Create list of dictionaries
    dict_list = df.to_dict('records')

    return dict_list


def create_ukhpi_timeseries(local_authority_iri, ppi_iri, months=240,
                            api_endpoint=HM_SPARQL_ENDPOINT, kgclient_hm=None):
    """
    Retrieve UKHPI data for a given local authority ONS IRI 

    Arguments:
        local_authority_iri {str} - IRI of the local authority as used by
                                    Office for National statistics
        ppi_iri {str} - IRI of the property price index (i.e. dataIRI of timeseries)
        months {int} - Number of months for which to retrieve date
                       (default: 240 (i.e. 20 years))
    
    """

    # Initialise KG client
    if not kgclient_hm:
        kgclient_hm = KGClient(api_endpoint, api_endpoint)

    query = get_ukhpi_monthly_data_for_district(ons_local_authority_iri=local_authority_iri, 
                                                months=months)
    res = kgclient_hm.performQuery(query)

    # Condition data
    cols = ['month', 'ukhpi_value']
    df = pd.DataFrame(columns=cols, data=res)
    df.dropna(inplace=True)

    # Assign correct data types
    df['month'] = pd.to_datetime(df['month'])
    df['month'] = df['month'].dt.strftime('%Y-%m-%d')
    df['ukhpi_value'] = df['ukhpi_value'].astype(float)

    # Create time series
    time_list = df['month'].tolist()
    value_list = df['ukhpi_value'].tolist()
    ts = TSClient.create_timeseries(time_list, [ppi_iri], [value_list])

    return ts
