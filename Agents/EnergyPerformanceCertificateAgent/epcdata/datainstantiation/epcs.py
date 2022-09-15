################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 15 Sep 2022                            #
################################################

# The purpose of this module is to instantiate data retrieved
# from the domestic energy performance certificates

import requests
import pandas as pd
from pathlib import Path

import agentlogging

from epcdata.datamodel.iris import *
from epcdata.kgutils.querytemplates import *
from epcdata.kgutils.kgclient import KGClient
from epcdata.errorhandling.exceptions import APIException, InvalidInput
from epcdata.utils.api_endpoints import *
from epcdata.utils.env_configs import API_TOKEN
from epcdata.utils.stack_configs import QUERY_ENDPOINT, UPDATE_ENDPOINT


# Initialise logger
logger = agentlogging.get_logger("prod")


def download_all_data(endpoint='domestic', rel_file_path='../../data/'):
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
        url = EPC_DOMESTIC
        fn = 'domestic_epcs.csv'
    elif endpoint == 'non-domestic':
        url = EPC_NON_DOMESTIC
        fn = 'nondomestic_epcs.csv'
    elif endpoint == 'display':
        url = EPC_DISPLAY
        fn = 'display_epcs.csv'
    else:
        logger.error('Invalid endpoint (i.e. EPC type) provided.')
        raise InvalidInput('Invalid endpoint (i.e. EPC type) provided.')

    # Verify that file path exists
    root = Path(__file__).parent
    file_path = Path.joinpath(root, rel_file_path)
    if not file_path.exists():
        logger.error('Invalid file path provided.')
        raise InvalidInput('Invalid file path provided.')
    # Add file name to path
    file_path = Path.joinpath(file_path, fn)

    # Retrieve instantiated postcodes from KG
    kgclient =KGClient(QUERY_ENDPOINT, UPDATE_ENDPOINT)
    query = instantiated_postalcodes()
    res = kgclient.performQuery(query)
    postcodes = [r['postcode'] for r in res]

    # Prepare authentication header for EPC API
    headers = {'Authorization': 'Basic {}'.format(API_TOKEN),
               'Accept': 'application/json'}

    # Initialise variables
    n = []
    i = 0 
    all_dfs = []

    for pc in postcodes:
        i += 1
        print(f'Retrieving EPC {i:>4}/{len(postcodes):>4}')
        url_epc = url + '?postcode=' + str(pc)
        r = requests.get(url=url_epc, headers=headers)
        if r.status_code == 200:
            if r.text != '':
                epcs = r.json()
                # Create DataFrame for current EPC data
                df = pd.DataFrame(columns=epcs['column-names'], data=epcs['rows'])
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
    df_all = pd.concat(all_dfs, ignore_index=True)

    # Write DataFrame
    df_all.to_csv(file_path)


if __name__ == '__main__':

    # Download and store all Domestic EPC data from API for data analysis
    download_all_data('domestic')

    # Download and store all Non-domestic EPC data from API for data analysis
    download_all_data('non-domestic')

    # Download and store all Domestic EPC data from API for data analysis
    download_all_data('display')

