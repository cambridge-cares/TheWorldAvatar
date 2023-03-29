################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 12 Oct 2022                            #
################################################

# The purpose of this module is to instantiate/update data retrieved from
# the HM Land Registry Open Data SPARQL endpoint according to OntoBuiltEnv

import re
import time
import uuid
import numpy as np
import pandas as pd
from fuzzywuzzy import fuzz, process, utils

from agent.datamodel.iris import *
from agent.datamodel.data_mapping import TIME_FORMAT, DATACLASS, PPD_PROPERTY_TYPES
from agent.errorhandling.exceptions import TSException
from agent.kgutils.tsclient import TSClient
from agent.kgutils.querytemplates import *
from agent.utils.api_endpoints import HM_SPARQL_ENDPOINT
from agent.utils.stack_configs import QUERY_ENDPOINT, UPDATE_ENDPOINT
from agent.kgutils.stackclients import PostGISClient, GeoserverClient
from agent.datainstantiation.derivation_markup import retrieve_avgsqmprice_postal_code_info, \
                                                      avg_sqm_price_derivation_markup, \
                                                      retrieve_marketvalue_property_info, \
                                                      property_value_estimation_derivation_markup

from pyderivationagent import PySparqlClient
from pyderivationagent import PyDerivationClient

# Initialise logger
from py4jps import agentlogging
logger = agentlogging.get_logger("prod")


def update_transaction_records(property_iris=None, min_conf_score=90,
                               api_endpoint=HM_SPARQL_ENDPOINT,
                               query_endpoint=QUERY_ENDPOINT, 
                               update_endpoint=UPDATE_ENDPOINT, 
                               kg_client_obe=None, kg_client_hm=None,
                               derivation_client=None,
                               add_avgsqm_derivation_markup=True,
                               add_propvalue_derivation_markup=True):
    """
    Retrieves HM Land Registry's Price Paid data for provided properties from
    given endpoint and instantiates them in the KG according to OntoBuiltEnv

    Arguments:
        property_iris - list of property IRIs for which to retrieve sales data
                        (i.e. retrieval of sales data done via address matching)
        min_conf_score - minimum confidence score for address matching [0-100]
        add_avgsqm_derivation_markup - boolean flag whether to add derivation markup for 
                                       Average Square Metre Price per Postal Code
        add_propvalue_derivation_markup - boolean flag whether to add derivation markup for 
                                          Market Value Estimate per Property
        api_endpoint - SPARQL endpoint for HM Land Registry Open Data
        query_endpoint/update_endpoint - SPARQL endpoint with instantiated OntoCityGml buildings
    Returns:
        Tuple of newly instantiated and updated sales transactions (new, updated)
    
    """

    # Initialise return values
    instantiated_tx = 0
    updated_tx = 0

    # Initialise PySparqlClients instances
    if not kg_client_obe:
        kg_client_obe = PySparqlClient(query_endpoint, update_endpoint)
    if not kg_client_hm:
        kg_client_hm = PySparqlClient(api_endpoint, api_endpoint)
    # Initialise PyDerivationClient instance
    if not derivation_client:
        derivation_client = PyDerivationClient(
            derivation_instance_base_url=DERIVATION_INSTANCE_BASE_URL,
            query_endpoint=query_endpoint, update_endpoint=update_endpoint)

    # Initialise relevant Stack Clients and parameters
    postgis_client = PostGISClient()
    geoserver_client = GeoserverClient()

    # 1) Retrieve location information for properties from list
    #    (i.e. required for query to HM Land Registry SPARQL endpoint)
    logger.info('Retrieving instantiated properties with location info ...')
    query = get_instantiated_properties_with_location_info(property_iris=property_iris)
    res = kg_client_obe.performQuery(query)

    # Create DataFrame from results and condition data
    obe = create_conditioned_dataframe_obe(res)

    # Query Price Paid Data postcode by postcode (trade-off between query speed and number of queries)
    logger.info('Querying HM Land Registry Price Paid Data in batches of individual postcodes ...')
    time_stamps_to_update = []
    postcodes = list(obe['postcode'].unique())
    for pc in postcodes:
        pc_data = obe[obe['postcode'] == pc].copy()
        pc_data.drop_duplicates(inplace=True)

        # 2) Retrieve Price Paid Data transaction records from HM Land Registry
        logger.info('Retrieving Price Paid Data from Open SPARQL endpoint ...')
        query = get_transaction_data_for_postcodes(postcodes=[pc])
        res = kg_client_hm.performQuery(query)

        # Create DataFrame from results and condition data
        logger.info('Conditioning retrieved Price Paid Data in DataFrame ...')
        ppd = create_conditioned_dataframe_ppd(res)

        # 3) Match addresses and retrieve transaction details
        logger.info('Matching addresses between instantiated EPC data and Price Paid Data addresses ...')
        matched_tx = get_best_matching_transactions(obe_data=pc_data, ppd_data=ppd,
                                                    min_match_score=min_conf_score)

        # 4) Update transaction records ...
        for tx in matched_tx:
            if tx['tx_iri']:
                updated_tx += 1
            else:
                instantiated_tx += 1
            # 4.1) ... in KG
            update_query = update_transaction_record(**tx)
            if update_query:
                kg_client_obe.performUpdate(update_query) 
            
            # 4.2) ... in PostGIS (to ensure proper styling)
            # Check if table exists, if not create it
            if not postgis_client.check_table_exists():
                logger.info('Creating PostGIS table ...')
                postgis_client.create_table()
                # Create GeoServer workspace and virtual table layer
                logger.info('Creating GeoServer layer ...')
                geoserver_client.create_workspace()
                geoserver_client.create_postgis_layer()

            # Upload property sales price to PostGIS
            # (overwrites previous value if available, otherwise creates new row)
            logger.info('Uploading property price to PostGIS ...')
            postgis_client.upload_property_price(building_iri=tx.get('property_iri'),
                                                 tx_price=tx.get('price'))
            
        # 5) Update timestamps of updated pure inputs (only takes effect if instantiated)
        # NOTE 'tx_iri' is None for newly instantiated transactions and only available
        #      for pre-existing, and hence updated, transactions
        time_stamps_to_update.extend([t['tx_iri'] for t in matched_tx if t.get('tx_iri')])
    derivation_client.updateTimestamps(time_stamps_to_update)
    
    if add_avgsqm_derivation_markup:
        # 6) Add derivation markup for Average Square Metre Price per Postal Code
        #    (optional as more efficient in update_all_transaction_records considering all postcodes)
        # Retrieve relevant postal code info
        postal_code_info_lst = retrieve_avgsqmprice_postal_code_info(sparql_client=kg_client_obe,
                                                                     postcodes=postcodes)
        print(f'Adding derivation markup for {len(postal_code_info_lst)} postcodes ...')
        # Add derivation markup for each postal code
        for i in range(len(postal_code_info_lst)):
            logger.info(f"Processing postal code {i+1}/{len(postal_code_info_lst)}")
            avg_sqm_price_derivation_markup(
                derivation_client=derivation_client,
                sparql_client=kg_client_obe,
                postal_code_iri=postal_code_info_lst[i]['postal_code'],
                transaction_record_iri_lst=postal_code_info_lst[i]['tx'],
                property_price_index_iri=postal_code_info_lst[i]['ppi'],
                existing_avg_sqm_price_iri=postal_code_info_lst[i].get('asp'),
                existing_asp_derivation_iri=postal_code_info_lst[i].get('derivation'),
                existing_asp_derivation_tx_iri_lst=postal_code_info_lst[i].get('deriv_tx'),
            )
    
    if add_propvalue_derivation_markup:
        # 7) Add derivation markup for Market Value Estimate per Property
        #    (optional as more efficient in update_all_transaction_records considering all postcodes)
        # Retrieve relevant property info
        property_info_dct = retrieve_marketvalue_property_info(sparql_client=kg_client_obe,
                                                               property_iris=property_iris)
        print(f'Adding derivation markup for {len(property_info_dct)} properties ...')
        # Add derivation markup for each property
        for i, (iri, info) in enumerate(property_info_dct.items()):
            logger.info(f"Processing property {i+1}/{len(property_info_dct)}")
            property_value_estimation_derivation_markup(
                derivation_client=derivation_client,
                sparql_client=kg_client_obe,
                property_iri=iri,
                property_price_index_iri=info['ppi'],
                floor_area_iri=info['area'],
                transaction_record_iri=info['tx'],
                avg_sqm_price_iri=info['asp'],
                market_value_iri=info['mv'],
                market_value_derivation_iri=info['derivation'],
                market_value_derivation_tx_iri=info['deriv_tx']
            )
    
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
    kg_client_obe = PySparqlClient(query_endpoint, update_endpoint)
    kg_client_hm = PySparqlClient(api_endpoint, api_endpoint)
    # Initialise TimeSeriesClient with default settings
    ts_client = TSClient(kg_client=kg_client_obe)
    # Create PyDerivationClient instance
    derivation_client = PyDerivationClient(
        derivation_instance_base_url=DERIVATION_INSTANCE_BASE_URL,
        query_endpoint=query_endpoint, update_endpoint=update_endpoint)

    # 1) Retrieve instantiated Admin Districts + potentially already instantiated
    #    Property Price Indices in OntoBuiltEnv
    print('Updating UK House Price Index ...')
    #logger.info('Updating UK House Price Index ...')
    districts = get_admin_district_index_dict(kg_client_obe)
    for d in districts: 
        if not d['ukhpi']:
            # Instantiate Property Price Index (i.e. time series data IRI) if
            # not existing + initialise TimeSeries
            ppi_iri = KB + 'PropertyPriceIndex_' + str(uuid.uuid4())
            d['ukhpi'] = ppi_iri
            logger.info('Instantiate UK House Price Index.')
            insert_query = instantiate_property_price_index(district_iri=d['local_authority'],
                                                            ppi_iri=ppi_iri)
            kg_client_obe.performUpdate(insert_query)
            
            # Initialise TimeSeries with try-with-resources block
            try:
                with ts_client.connect() as conn:
                    ts_client.tsclient.initTimeSeries([ppi_iri], [DATACLASS], TIME_FORMAT,
                                                      conn)
            except Exception as ex:
                logger.error('Error initialising time series: {}'.format(ex))
                raise TSException('Error initialising time series') from ex
            instantiated_ukhpi += 1
        else:
            # Update timestamp of updated pure input (only takes effect if instantiated)
            derivation_client.updateTimestamp(d['ukhpi'])
            updated_ukhpi += 1

        # 2) Retrieve Property Price Index from HM Land Registry
        logger.info('Update UK House Price Index data.')
        ts = create_ukhpi_timeseries(local_authority_iri=d['ons_district'], 
                                     ppi_iri=d['ukhpi'], kg_client_hm=kg_client_hm)

        # 3) Update Property Price Index time series data in KG
        try:
            with ts_client.connect() as conn:
                ts_client.tsclient.addTimeSeriesData(ts, conn)
        except Exception as ex:
            logger.error('Error adding time series data: {}'.format(ex))
            raise TSException('Error adding time series data') from ex

    # 4) Retrieve all instantiated properties with associated postcodes
    print('Retrieving instantiated properties with attached postcodes ...')
    #logger.info('Retrieving instantiated properties with attached postcodes ...')
    query = get_all_properties_with_postcode()
    res = kg_client_obe.performQuery(query)

    # 5) Update transaction records in KG in postcode batches
    batch_size = 100
    df = pd.DataFrame(res)
    pcs = df['postcode'].unique().tolist()
    pcs.sort()
    postcodes = [pcs[i:i + batch_size] for i in range(0, len(pcs), batch_size)]

    print('Updating transaction records (in postcode batches) ...')
    #logger.info('Updating transaction records (for batch of postcodes) ...')
    i = 0
    for pc in postcodes:
        i += 1
        print(f'Updating batch {i:>4}/{len(postcodes):>4}')

        prop_iris = df[df['postcode'].isin(pc)]['property_iri'].tolist()

        # Update transaction records for list of property IRIs
        tx_new, tx_upd = update_transaction_records(property_iris=prop_iris, 
                            min_conf_score=min_conf_score, api_endpoint=api_endpoint,
                            kg_client_obe=kg_client_obe, kg_client_hm=kg_client_hm,
                            derivation_client = derivation_client,
                            add_avgsqm_derivation_markup=False,
                            add_propvalue_derivation_markup=False)
        instantiated_tx += tx_new
        updated_tx += tx_upd

        # 6) Add derivation markup for Average Square Metre Price per Postal Code
        # Retrieve relevant postal code info
        postal_code_info_lst = retrieve_avgsqmprice_postal_code_info(sparql_client=kg_client_obe,
                                                                     postcodes=pc)
        print(f'Adding derivation markup for {len(postal_code_info_lst)} postcodes ...')
        # Add derivation markup for each postal code
        for j in range(len(postal_code_info_lst)):
            logger.info(f"Processing postal code {j+1}/{len(postal_code_info_lst)}")
            avg_sqm_price_derivation_markup(
                derivation_client=derivation_client,
                sparql_client=kg_client_obe,
                postal_code_iri=postal_code_info_lst[j]['postal_code'],
                transaction_record_iri_lst=postal_code_info_lst[j]['tx'],
                property_price_index_iri=postal_code_info_lst[j]['ppi'],
                existing_avg_sqm_price_iri=postal_code_info_lst[j].get('asp'),
                existing_asp_derivation_iri=postal_code_info_lst[j].get('derivation'),
                existing_asp_derivation_tx_iri_lst=postal_code_info_lst[j].get('deriv_tx'),
            )
        # Allow for some time to update derivations in KG (10s is arbitrary)
        logger.info(f"Derivation markup for postcodes completed. Waiting shortly before continuing with properties ...")
        time.sleep(10)
    
        # 7) Add derivation markup for Market Value Estimate per Property
        #    (optional as more efficient in update_all_transaction_records considering all postcodes)        
        # Retrieve relevant property info
        property_info_dct = retrieve_marketvalue_property_info(sparql_client=kg_client_obe,
                                                               property_iris=prop_iris)
        print(f'Adding derivation markup for {len(property_info_dct)} properties ...')
        # Add derivation markup for each property
        for k, (iri, info) in enumerate(property_info_dct.items()):
            logger.info(f"Processing property {k+1}/{len(property_info_dct)}")
            property_value_estimation_derivation_markup(
                derivation_client=derivation_client,
                sparql_client=kg_client_obe,
                property_iri=iri,
                property_price_index_iri=info['ppi'],
                floor_area_iri=info['area'],
                transaction_record_iri=info['tx'],
                avg_sqm_price_iri=info['asp'],
                market_value_iri=info['mv'],
                market_value_derivation_iri=info['derivation'],
                market_value_derivation_tx_iri=info['deriv_tx']
            )

    return (instantiated_tx, updated_tx, instantiated_ukhpi, updated_ukhpi)


def create_conditioned_dataframe_obe(sparql_results:list) -> pd.DataFrame:
        """
        Parse SPARQL results from OBE namespace (i.e. instantiated EPC data)
        as DataFrame and condition data to ensure proper comparison with 
        HM Land Registry Data

        Arguments:
            sparql_results - list of dicts with following keys
                                [property_iri, address_iri, postcode_iri, district_iri,
                                property_type, postcode, street, number, bldg_name, unit_name,
                                tx_iri]
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

        # Remove potentially duplicated properties, i.e.
        # NOTE: It has been observed that a minor fraction of properties (<10 in 
        #       set >13000 properties) gets assigned multiple address details (e.g. 
        #       2 street names). To avoid instantiation of multiple transaction 
        #       records for property (which is based on address matching), 
        #       remove duplicated IRIs
        # TODO: Understand why multiple address details get instantiated in the 
        #       first place in EPC Agent (likely some issue with instantiation of
        #       parent building when only containing one other property/flat)
        df.drop_duplicates(subset=['property_iri'], keep='first', inplace=True)

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
        ppd_addr = ppd_data[ppd_data['property_type'].isin([prop, OBE_PROPERTY])]

        # Extract list of PPD addresses
        ppd_addr = ppd_addr['ppd_address'].tolist()

        # Find best match
        best = None
        if min_match_score:
            # NOTE: Compared Levenshtein algorithm with Damerau-Levenshtein algo, 
            #       and the latter one performed better using the 'token_set_ratio'
            #       scoring method --> used here

            if utils.full_process(addr):
                # 'Full_process' string processor validates string before actual 
                # matching, i.e. checks for availability of alphanumeric characters
                matches = process.extract(addr, ppd_addr, scorer=fuzz.token_set_ratio)
                matches = [m for m in matches if m[1] >= min_match_score]
                if matches:
                    matches.sort(key=lambda x: x[1], reverse=True)
                    best = matches[0]
        else:
            if utils.full_process(addr):
                # Only execute for strings with alphanumeric characters to avoid warning
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


def create_ukhpi_timeseries(local_authority_iri, ppi_iri, months=480,
                            api_endpoint=HM_SPARQL_ENDPOINT, kg_client_hm=None):
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
    if not kg_client_hm:
        kg_client_hm = PySparqlClient(api_endpoint, api_endpoint)

    query = get_ukhpi_monthly_data_for_district(ons_local_authority_iri=local_authority_iri, 
                                                months=months)
    res = kg_client_hm.performQuery(query)

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
