################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 14 Sep 2022                            #
################################################

# The purpose of this module is to provide functions to instantiate
# local authority district and postcode data from Office for National
# Statistics (e.g. to relate average prices and property price index)

import json
import re
import requests
import uuid
import urllib.parse
import pandas as pd

import agentlogging

from epcdata.datamodel.iris import *
from epcdata.utils.api_endpoints import *
from epcdata.errorhandling.exceptions import APIException
from epcdata.utils.stack_configs import QUERY_ENDPOINT, UPDATE_ENDPOINT
from epcdata.kgutils.kgclient import KGClient
from epcdata.kgutils.querytemplates import check_instantiated_local_authority, \
                                           ons_postcodes_per_localauthority, \
                                           instantiate_postcodes_for_district


# Initialise logger
logger = agentlogging.get_logger("prod")


def local_authority_instantiated(kgclient, local_authority_district):
    """
        Check if local authority district is already instantiated in KG

        Returns:
            True if district (incl. postcodes) is already instantiated, False otherwise
    """

    # Perform query
    query = check_instantiated_local_authority(local_authority_district)
    res = kgclient.performQuery(query)

    if res:
        return True
    else:
        return False


def retrieve_ons_postcodes(local_authority_district):
    """
        Retrieves postcodes for local authority district from ONS API (for details
        on local authority codes, please see: https://epc.opendatacommunities.org/docs/api/domestic#domestic-local-authority)

        Arguments:
            local_authority_district (str): Local authority code as used by
                                            EPC and Office for National Statistics

        Returns:
            DataFrame: ['district_iri', 'district', 'postcode_iri', 'postcode']
    """

    # Get query and URL-encode (as required by API)
    q = ons_postcodes_per_localauthority(local_authority_district)
    query = urllib.parse.quote(q)

    # Perform GET request
    url = ONS_ENDPOINT + '.json?query=' + query
    res = requests.get(url)

    if res.status_code != 200:
        logger.error('Error retrieving data from ONS API.')
        raise APIException('Error retrieving data from ONS API.')

    # Extract and unwrap results
    data = json.loads(res.text)
    df = pd.DataFrame(data['results']['bindings'])
    for c in df.columns:
        df[c] = df[c].apply(lambda x: x['value'])

    # Check valid postcode format: https://www.getthedata.com/postcode
    pattern = '[A-Z0-9]{2,4} [A-Z0-9]{3}'
    check = df['postcode'].apply(lambda x: bool(re.match(pattern, x)))
    # Exclude data with potentially erroneous post code format
    df = df[check]

    return df


def instantiate_postcodes(kgclient, ons_data, local_authority_district):

    # Create AdministrativeDistrict triple data
    district = ons_data[['district_iri', 'district']].copy()
    district = district.drop_duplicates()
    district['iri'] = ''
    district['iri'] = district['iri'].apply(lambda x: OBE_ADMIN_DISTRICT + '_' + str(uuid.uuid4()))

    # Create Postcode triple data
    postcode = ons_data[['postcode_iri', 'postcode']].copy()
    postcode = postcode.drop_duplicates()
    postcode['iri'] = ''
    postcode['iri'] = postcode['iri'].apply(lambda x: OBE_POSTALCODE + '_' + str(uuid.uuid4()))

    # Create and perform INSERT query
    query = instantiate_postcodes_for_district(local_authority_district, 
                                               district, postcode)
    kgclient.performUpdate(query)

    return len(postcode)


def initialise_postcodes(query_endpoint=QUERY_ENDPOINT, update_endpoint=UPDATE_ENDPOINT,
                         local_authority_district="E07000146"):
    """
    Instantiates postcodes for local authority district based on data from ONS API (for details
    on local authority codes, please see: https://epc.opendatacommunities.org/docs/api/domestic#domestic-local-authority)

        Arguments:
            local_authority_district (str): Local authority code as used by
                                            EPC and Office for National Statistics

        Returns:
            Number of instantiated postcodes; None if district is already instantiated
    """

    # Create KG client
    kgclient = KGClient(query_endpoint, update_endpoint)
    
    # Check if local authority (incl. postcodes) already instantiated
    logger.info('Check if local authority district (incl. postcodes) is already instantiated ...')
    instantiated = local_authority_instantiated(kgclient, local_authority_district)
    
    # Return None if already instantiated, otherwise instantiate
    if instantiated:
        return None
    else:
        # Retrieve ONS data
        logger.info('Retrieve postcode data from ONS API ...')
        data = retrieve_ons_postcodes(local_authority_district)
        
        # Instantiate ONS data
        logger.info('Instantiate postcode data in KG ...')
        n = instantiate_postcodes(kgclient, data, local_authority_district)
        
        return n

if __name__ == '__main__':

    # Instantiate postcodes for King's Lynn and West Norfolk
    initialise_postcodes()