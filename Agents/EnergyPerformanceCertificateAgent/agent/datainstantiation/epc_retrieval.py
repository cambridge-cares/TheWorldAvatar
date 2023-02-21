################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 15 Sep 2022                            #
################################################

# The purpose of this module is to retrieved energy performance 
# certificates data from the EPC API endpoints

import requests
import pandas as pd
from pathlib import Path

from py4jps import agentlogging

from agent.datamodel.iris import *
from agent.kgutils.querytemplates import *
from agent.kgutils.kgclient import KGClient
from agent.errorhandling.exceptions import APIException
from agent.utils.api_endpoints import *
from agent.utils.env_configs import ENCODED_AUTH
from agent.utils.stack_configs import QUERY_ENDPOINT, UPDATE_ENDPOINT

# Initialise logger
logger = agentlogging.get_logger("prod")


def obtain_data_for_certificate(lmk_key: str, endpoint='domestic'):
    """
        Retrieves EPC data for provided certificate from given endpoint

        Arguments:
            lmk_key - certificate id (i.e. individual lodgement identifier)
            endpoint (str) - EPC endpoint from which to retrieve data
                             ['domestic', 'non-domestic', 'display']
        Returns:
            Dictionary of relevant EPC data (empty dictionary if no data available)
    """
    # Get EPC API endpoint
    endpoints = {'domestic': EPC_DOMESTIC_CERT,
                 'non-domestic': EPC_NON_DOMESTIC_CERT,
                 'display': EPC_DISPLAY_CERT}
    url = endpoints.get(endpoint)
    if not url:
        logger.error('Invalid endpoint (i.e. EPC type) provided.')
        raise ValueError('Invalid endpoint (i.e. EPC type) provided.')

    # Prepare API request
    url += str(lmk_key)
    headers = {'Authorization': 'Basic {}'.format(ENCODED_AUTH),
               'Accept': 'application/json'}
    # Retrieve EPC data
    try:
        res = requests.get(url=url, headers=headers)
        if res.status_code == 200:
            epc = res.json()
        elif res.status_code == 404:
            logger.info('No data available for provided certificate lodgement identifier.')
            epc = None
    except Exception as ex:
        logger.error('Error retrieving EPC data from API.')
        raise APIException('Error retrieving EPC data from API.') from ex
    
    # Extract relevant EPC data based on endpoint
    if (endpoint == 'domestic'):
        relevant = ['lmk-key', 'address1', 'address2', 'address3',
                    'postcode', 'local-authority', 'uprn',
                    'built-form', 'property-type', 'construction-age-band',
                    'current-energy-rating', 'number-habitable-rooms', 'total-floor-area', 
                    'floor-description', 'roof-description', 'walls-description',
                    'windows-description']
    elif (endpoint == 'non-domestic'):
        relevant = ['lmk-key', 'address1', 'address2', 'address3',
                    'postcode', 'local-authority', 'uprn', 'property-type',
                    'asset-rating-band', 'floor-area']
    elif (endpoint == 'display'):
        relevant = ['lmk-key', 'address1', 'address2', 'address3',
                    'postcode', 'local-authority', 'uprn', 'property-type', 'building-category',
                    'operational-rating-band', 'total-floor-area']

    try:
        epc_data = epc['rows'][0]
        epc_data = {r:epc_data[r] for r in relevant if r in epc_data}
    except Exception:
        epc_data = {}

    return epc_data


def obtain_latest_data_for_postcodes(postcodes: list, endpoint='domestic'):
    """
        Retrieves EPC data for provided list of postcodes from given endpoint

        Arguments:
            postcodes - list of strings of postcodes
            endpoint (str) - EPC endpoint from which to retrieve data
                             ['domestic', 'non-domestic', 'display']
        Returns:
            DataFrame of relevant EPC data (empty DataFrame if no data available)
    """
    # Get EPC API endpoint
    endpoints = {'domestic': EPC_DOMESTIC_SEARCH,
                 'non-domestic': EPC_NON_DOMESTIC_SEARCH,
                 'display': EPC_DISPLAY_SEARCH}
    url = endpoints.get(endpoint)
    if not url:
        logger.error('Invalid endpoint (i.e. EPC type) provided.')
        raise ValueError('Invalid endpoint (i.e. EPC type) provided.')

    # Prepare authentication header for EPC API
    headers = {'Authorization': 'Basic {}'.format(ENCODED_AUTH),
               'Accept': 'application/json'}

    # Initialise variables
    i = 0 
    all_dfs = []
    df = pd.DataFrame()

    for pc in postcodes:
        i += 1
        logger.info(f'Retrieving EPC {i:>4}/{len(postcodes):>4}')
        url_epc = url + '?postcode=' + str(pc)
        r = requests.get(url=url_epc, headers=headers)
        if r.status_code == 200:
            if r.text != '':
                epcs = r.json()
                # Create DataFrame for current EPC data
                df = pd.DataFrame(columns=epcs['column-names'], data=epcs['rows'])
                # Append data to list
                all_dfs.append(df)
        else:
            logger.error('Error retrieving EPC data from API.')
            raise APIException('Error retrieving EPC data from API.')

    # Construct overall DataFrame
    if len(all_dfs) > 1:
        df_all = pd.concat(all_dfs, ignore_index=True)
    else:
        df_all = df

    if df_all.empty:
        return df_all
    else:
        # Extract relevant EPC data based on endpoint
        if (endpoint == 'domestic'):
            relevant = ['lmk-key', 'address1', 'address2', 'address3',
                        'postcode', 'local-authority', 'uprn',
                        'built-form', 'property-type', 'construction-age-band',
                        'current-energy-rating', 'number-habitable-rooms', 'total-floor-area',
                        'floor-description', 'roof-description', 'walls-description',
                        'windows-description', 'lodgement-datetime']
        elif (endpoint == 'non-domestic'):
            relevant = ['lmk-key', 'address1', 'address2', 'address3',
                        'postcode', 'local-authority', 'uprn', 'property-type',
                        'asset-rating-band', 'floor-area', 'lodgement-datetime']
        elif (endpoint == 'display'):
            relevant = ['lmk-key', 'address1', 'address2', 'address3',
                        'postcode', 'local-authority', 'uprn', 'property-type',
                        'operational-rating-band', 'total-floor-area', 'lodgement-datetime',
                        'building-category']         
        epc_data = df_all[relevant].copy()

        # Keep only latest EPC data per UPRN
        epc_data['date'] = pd.to_datetime(epc_data['lodgement-datetime'], yearfirst=True, dayfirst=False)
        epc_data.sort_values(by='date', ascending=False, inplace=True)
        epc_data = epc_data[~epc_data['uprn'].duplicated()]
        epc_data = epc_data.drop(columns=['date', 'lodgement-datetime'])

        # Align missing data and reset index
        epc_data = epc_data.where(pd.notnull(epc_data), None)
        epc_data = epc_data.reset_index()

    return epc_data


def download_all_data(endpoint='domestic', rel_file_path='../../data/',
                      query_endpoint=QUERY_ENDPOINT, update_endpoint=UPDATE_ENDPOINT):
    """
        Downloads and saves all EPC information, primarily for data analysis 
        and quality assessment

        Arguments:
            endpoint (str) - type of EPC which to retrieve 
                             ['domestic', 'non-domestic', 'display']
            rel_file_path (str) - relative directory path where to store EPC data
    """

    # Verify provided EPC type
    if endpoint == 'domestic':
        url = EPC_DOMESTIC_SEARCH
        fn = 'domestic_epcs.csv'
    elif endpoint == 'non-domestic':
        url = EPC_NON_DOMESTIC_SEARCH
        fn = 'nondomestic_epcs.csv'
    elif endpoint == 'display':
        url = EPC_DISPLAY_SEARCH
        fn = 'display_epcs.csv'
    else:
        logger.error('Invalid endpoint (i.e. EPC type) provided.')
        raise ValueError('Invalid endpoint (i.e. EPC type) provided.')

    # Verify that file path exists
    root = Path(__file__).parent
    file_path = Path.joinpath(root, rel_file_path)
    if not file_path.exists():
        logger.error('Invalid file path provided.')
        raise ValueError('Invalid file path provided.')
    # Add file name to path
    file_path = Path.joinpath(file_path, fn)

    # Retrieve instantiated postcodes from KG
    kgclient = KGClient(query_endpoint, update_endpoint)
    query = instantiated_postalcodes()
    res = kgclient.performQuery(query)
    postcodes = [r['postcode'] for r in res]

    # Prepare authentication header for EPC API
    headers = {'Authorization': 'Basic {}'.format(ENCODED_AUTH),
               'Accept': 'application/json'}

    # Initialise variables
    n = []
    i = 0 
    all_dfs = []

    for pc in postcodes:
        i += 1
        logger.info(f'Retrieving EPC {i:>4}/{len(postcodes):>4}')
        url_epc = url + '?postcode=' + str(pc)
        r = requests.get(url=url_epc, headers=headers)
        if r.status_code == 200:
            if r.text != '':
                epcs = r.json()
                # Create DataFrame for current EPC data
                df = pd.DataFrame(columns=epcs['column-names'], data=epcs['rows'], dtype="string")
                # Append data to list
                all_dfs.append(df)
                # Get number of EPCs in postcode
                n.append(len(epcs['rows']))
            else:
                n.append(0)
        else:
            logger.error('Error retrieving EPC data from API.')
            raise APIException('Error retrieving EPC data from API.')
    
    print(f'Maximum number of EPCs per postcode: {max(n)}')

    # Construct overall DataFrame
    if len(all_dfs) > 1:
        df_all = pd.concat(all_dfs, ignore_index=True)
    else:
        df_all = df

    # Write DataFrame
    df_all.to_csv(file_path)


if __name__ == '__main__':

    # Download and store all Domestic EPC data from API for data analysis
    download_all_data('domestic')

    # Download and store all Non-domestic EPC data from API for data analysis
    download_all_data('non-domestic')

    # Download and store all Domestic EPC data from API for data analysis
    download_all_data('display')
