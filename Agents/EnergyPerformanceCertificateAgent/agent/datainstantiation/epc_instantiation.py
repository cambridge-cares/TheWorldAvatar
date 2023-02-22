################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 15 Sep 2022                            #
################################################

# The purpose of this module is to instantiate data retrieved from
# the energy performance certificates API according to OntoBuiltEnv

import re
import json
import uuid
import numpy as np
import pandas as pd
from geojson_rewind import rewind

from py4jps import agentlogging

from agent.datamodel.iris import *
from agent.datamodel.data_mapping import *
from agent.errorhandling.exceptions import KGException
from agent.kgutils.kgclient import KGClient
from agent.utils.env_configs import OCGML_ENDPOINT
from agent.utils.stack_configs import QUERY_ENDPOINT, UPDATE_ENDPOINT
from agent.kgutils.querytemplates import *
from agent.utils.geo_utils import initialise_pyproj_transformer, get_coordinates, \
                                  create_geojson_feature
from agent.datainstantiation.epc_retrieval import obtain_data_for_certificate, \
                                                  obtain_latest_data_for_postcodes
from agent.kgutils.stackclients import OntopClient, PostGISClient, GdalClient, \
                                       GeoserverClient

# Initialise logger
logger = agentlogging.get_logger("prod")


def instantiate_epc_data_for_certificate(lmk_key: str, epc_endpoint='domestic',
                                         ocgml_endpoint=OCGML_ENDPOINT,
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
    updates = (0, 0)

    # Retrieve EPC data
    epc_data = obtain_data_for_certificate(lmk_key, epc_endpoint)
    # Instantiate data (if successfully retrieved)
    if epc_data:
        # Retrieve UPRN info required to match data
        try:
            uprn = epc_data.get('uprn')
        except Exception:
            logger.error('Retrieved EPC data does not have associated UPRN.')
            raise KeyError('Retrieved EPC data does not have associated UPRN.')

        # Initialise KG clients
        kgclient_epc = KGClient(query_endpoint, update_endpoint)
        kgclient_ocgml = KGClient(ocgml_endpoint, ocgml_endpoint)
        
        #
        # Check if UPRN is instantiated in OntoCityGml and whether parent building 
        # (i.e. building hosting several flats/UPRNs) shall be instantiated
        #
        uprns = retrieve_ocgml_uprns(uprn, kgclient=kgclient_ocgml)
        if uprns:
            # Relevant UPRNs are instantiated in OntoCityGml (i.e. have geospatial representation)
            if len(uprns) == 1:
                # If it is a flat, then create/retrieve parent building IRI
                if epc_data.get('property-type') in ['Flat', 'Maisonette']:
                    parent = retrieve_parent_building(uprns, kgclient=kgclient_epc)
                    # Retrieve parent building IRI if already instantiated, otherwise create            
                    if parent:
                        parent_iri = parent
                    else:
                        parent_iri = KB + 'Building_' + str(uuid.uuid4())
                else:
                    parent_iri = None

            else:
                parent = retrieve_parent_building(uprns, kgclient=kgclient_epc)
                # Retrieve parent building IRI if already instantiated, otherwise create            
                if parent:
                    parent_iri = parent
                else:
                    parent_iri = KB + 'Building_' + str(uuid.uuid4())

            #
            # Check if same EPC is already instantiated for UPRN
            #
            query = get_latest_epc_and_property_iri_for_uprn(uprn)
            instantiated = kgclient_epc.performQuery(query)

            # There are 3 different cases:
            if instantiated and instantiated[0].get('certificate') == lmk_key:
                # 1) EPC data up to date --> Do nothing
                logger.info('EPC data for UPRN still up to date. No update needed.')
                updates = (0, 0)
            else:
                # 2) & 3) Data not instantiated at all or outdated --> condition data
                if (epc_endpoint == 'domestic'):
                    data_to_instantiate = condition_epc_data(epc_data)
                elif (epc_endpoint == 'non-domestic'):
                    data_to_instantiate = condition_epc_non_domestic_data(epc_data)
                elif (epc_endpoint == 'display'):
                    data_to_instantiate = condition_epc_display_data(epc_data)
               
                if not instantiated:
                    # 2) No EPC data instantiated yet --> Instantiate data
                    # Create Property IRI
                    if (epc_endpoint == 'domestic'):
                        if epc_data.get('property-type') in ['Flat', 'Maisonette']:
                            data_to_instantiate['property_iri'] = OBE_FLAT + '_' + str(uuid.uuid4())
                        else:
                            data_to_instantiate['property_iri'] = KB + 'Building_' + str(uuid.uuid4())
                    elif (epc_endpoint == 'non-domestic' or epc_endpoint == 'display'):
                        data_to_instantiate['property_iri'] = KB + 'Property_' + str(uuid.uuid4())

                    # Add postcode and district IRIs
                    postcode = epc_data.get('postcode')
                    local_authority_code = epc_data.get('local-authority')
                    if postcode and local_authority_code:
                        query = get_postcode_and_district_iris(postcode, local_authority_code)
                        geographies = kgclient_epc.performQuery(query)
                        if geographies:
                            data_to_instantiate['postcode_iri'] = geographies[0].get('postcode')
                            data_to_instantiate['district_iri'] = geographies[0].get('district')
                    # Add potential parent building
                    data_to_instantiate['parent_iri'] = parent_iri
                    logger.info('No EPC data instantiated for UPRN. Instantiate data ... ')
                    insert_query = instantiate_epc_data(**data_to_instantiate)
                    kgclient_epc.performUpdate(insert_query)
                    updates = (1, 0)

                else:
                    # 3) EPC data instantiated, but outdated --> Update data
                    # Initialise list of properties to update
                    # NOTE: Not all relationships/IRIs are updated, i.e. UPRN and parent building, 
                    # address data incl. links to postcode and admin district
                    # TODO: Current updating only considers already instantiated information;
                    # potentially include instantiation of previously not available data
                    if (epc_endpoint == 'domestic'):
                        data_to_update = ['epc_lmkkey', 'built_form_iri', 
                                          'property_type_iri','usage_iri', 'usage_label',
                                          'construction_end','floor_description', 
                                          'roof_description','wall_description', 
                                          'windows_description','floor_area', 'epc_rating', 'rooms', 'weightage']
                    elif (epc_endpoint in ['non-domestic', 'display']):
                        data_to_update = ['epc_lmkkey','property_type_iri','usage_iri', 'usage_label',
                                          'floor_area', 'epc_rating', 'weightage']

                    data_to_update = {k: v for k, v in data_to_instantiate.items() if k in data_to_update}
                    data_to_update['property_iri'] = instantiated[0].get('property')

                    logger.info('Instantiated EPC data for UPRN outdated. Updated data ... ')
                    update_query = update_epc_data(**data_to_update)
                    kgclient_epc.performUpdate(update_query)
                    updates = (0, 1)
        else:
            logger.info('No associated UPRNs are instantiated in OntoCityGml endpoint.')
    else:
        logger.info('No EPC data available for provided lodgement identifier.')

    return updates


def instantiate_epc_data_for_postcodes(postcodes: list, epc_endpoint='domestic',
                                       ocgml_endpoint=OCGML_ENDPOINT,
                                       query_endpoint=QUERY_ENDPOINT, 
                                       update_endpoint=UPDATE_ENDPOINT,
                                       kgclient_epc=None, kgclient_ocgml=None,
                                       summarise=True):
    """
    Retrieves EPC data for provided list of postcodes from given endpoint and 
    instantiates them in the KG according to OntoBuiltEnv

    Arguments:
        postcodes - list of strings of postcodes
        epc_endpoint (str) - EPC endpoint from which to retrieve data
                             ['domestic', 'non-domestic', 'display']
        ocgml_endpoint - SPARQL endpoint with instantiated OntoCityGml buildings
        kgclient - KG Client to interact with OntoBuiltEnv SPARQL endpoint
        summarise (bool) - boolean flag whether to summarise EPC data for parent buildings
    Returns:
        Tuple of newly instantiated and updated EPCs (new, updated)
        Tuple of newly summarized and updated EPC summaries (new, updated)
    """

    # Initialise return values
    new_epcs = 0
    updated_epcs = 0
    new_summaries = 0
    updated_summaries = 0
    
    # Retrieve DataFrame with EPC data
    logger.info('Retrieving EPC data from API ...')
    epc_data = obtain_latest_data_for_postcodes(postcodes, epc_endpoint)
    n_epcs = len(epc_data)

    # Initialise KG clients
    if not kgclient_epc:
        kgclient_epc = KGClient(query_endpoint, update_endpoint)
    if not kgclient_ocgml:
        kgclient_ocgml = KGClient(ocgml_endpoint, ocgml_endpoint)

    # Iterate through DataFrame
    for index, row in epc_data.iterrows():
        logger.info(f'Instantiating EPC {index+1:>6}/{n_epcs:>6} ...')
        # Retrieve UPRN info required to match data
        if row.get('uprn'):
            uprn = row['uprn']
        else:
            logger.info('Retrieved EPC data does not have associated UPRN.')
            continue

        #
        # Check if UPRN is instantiated in OntoCityGml and whether parent building 
        # (i.e. building hosting several flats/UPRNs) shall be instantiated
        #
        uprns = retrieve_ocgml_uprns(uprn, kgclient=kgclient_ocgml)
        if uprns:
            # Initialise boolean flag whether property is inside another (parent) building
            is_child = False
            # Relevant UPRNs are instantiated in OntoCityGml (i.e. have geospatial representation)
            if len(uprns) == 1:
                # If it is a flat, then create/retrieve parent building IRI
                if row.get('property-type') in ['Flat', 'Maisonette']:
                    is_child = True
                    parent = retrieve_parent_building(uprns, kgclient=kgclient_epc)
                    # Retrieve parent building IRI if already instantiated, otherwise create            
                    if parent:
                        parent_iri = parent
                    else:
                        parent_iri = KB + 'Building_' + str(uuid.uuid4())
                else:
                    parent_iri = None
            else:
                # Current property belongs to building which hosts multiple UPRNs
                is_child = True
                parent = retrieve_parent_building(uprns, kgclient=kgclient_epc)
                # Retrieve parent building IRI if already instantiated, otherwise create            
                if parent:
                    parent_iri = parent
                else:
                    parent_iri = KB + 'Building_' + str(uuid.uuid4())

            #
            # Check if same EPC is already instantiated for UPRN
            #
            query = get_latest_epc_and_property_iri_for_uprn(uprn)
            instantiated = kgclient_epc.performQuery(query)

            # There are 3 different cases:
            if instantiated and instantiated[0].get('certificate') == row.get('lmk-key'):
                # 1) EPC data up to date --> Do nothing
                logger.info('EPC data for UPRN still up to date. No update needed.')
            else:
                # 2) & 3) Data not instantiated at all or outdated --> condition data
                if (epc_endpoint=='domestic'):
                    data_to_instantiate = condition_epc_data(row)
                elif (epc_endpoint == 'non-domestic'):
                    data_to_instantiate = condition_epc_non_domestic_data(row)
                elif (epc_endpoint == 'display'):
                    data_to_instantiate = condition_epc_display_data(row)

                if not instantiated:                 
                    # 2) No EPC data instantiated yet --> Instantiate data
                    # Create Property IRI
                    if (epc_endpoint == 'domestic'):
                        if is_child:
                            if row.get('property-type') in ['Flat', 'Maisonette']:
                                data_to_instantiate['property_iri'] = KB + 'Flat_' + str(uuid.uuid4())
                            else:
                                data_to_instantiate['property_iri'] = KB + 'Property_' + str(uuid.uuid4())
                        else:
                            data_to_instantiate['property_iri'] = KB + 'Building_' + str(uuid.uuid4())
                    elif (epc_endpoint == 'non-domestic' or epc_endpoint == 'display'):
                        data_to_instantiate['property_iri'] = KB + 'Property_' + str(uuid.uuid4())

                    # Add postcode and district IRIs
                    postcode = row.get('postcode')
                    local_authority_code = row.get('local-authority')
                    if postcode and local_authority_code:
                        query = get_postcode_and_district_iris(postcode, local_authority_code)
                        geographies = kgclient_epc.performQuery(query)
                        if geographies:
                            data_to_instantiate['postcode_iri'] = geographies[0].get('postcode')
                            data_to_instantiate['district_iri'] = geographies[0].get('district')
                    # Add potential parent building
                    data_to_instantiate['parent_iri'] = parent_iri
                    logger.info('No EPC data instantiated for UPRN. Instantiate data ... ')
                    insert_query = instantiate_epc_data(**data_to_instantiate)
                    kgclient_epc.performUpdate(insert_query)
                    new_epcs += 1

                else:
                    # 3) EPC data instantiated, but outdated --> Update data
                    # Initialise list of properties to update
                    # NOTE: Not all relationships/IRIs are updated, i.e. UPRN and parent building, 
                    # address data incl. links to postcode and admin district
                    # TODO: Current updating only considers already instantiated information;
                    # potentially include instantiation of previously not available data
                    if (epc_endpoint == 'domestic'):
                        data_to_update = ['epc_lmkkey', 'built_form_iri', 
                                          'property_type_iri','usage_iri', 'usage_label',
                                          'construction_end','floor_description', 
                                          'roof_description','wall_description', 
                                          'windows_description','floor_area', 'epc_rating', 'rooms', 'weightage']
                    elif (epc_endpoint in ['non-domestic', 'display']):
                        data_to_update = ['epc_lmkkey','property_type_iri','usage_iri', 'usage_label',
                                          'floor_area', 'epc_rating', 'weightage']

                    data_to_update = {k: v for k, v in data_to_instantiate.items() if k in data_to_update}
                    data_to_update['property_iri'] = instantiated[0].get('property')

                    logger.info('Instantiated EPC data for UPRN outdated. Updated data ... ')
                    update_query = update_epc_data(**data_to_update)

                    kgclient_epc.performUpdate(update_query)
                    updated_epcs += 1
        else:
            logger.info('No associated UPRNs are instantiated in OntoCityGml endpoint.')

    if summarise:
        # Potentially "summarise" EPCs for parent buildings
        new_summaries, updated_summaries = instantiate_epcs_for_parent_buildings(kgclient=kgclient_epc, epc_endpoint=epc_endpoint)    

    return (new_epcs, updated_epcs), (new_summaries, updated_summaries)


def instantiate_epc_data_for_all_postcodes(epc_endpoint=None,
                                           ocgml_endpoint=OCGML_ENDPOINT,
                                           query_endpoint=QUERY_ENDPOINT, 
                                           update_endpoint=UPDATE_ENDPOINT):
    """
    Retrieves EPC data for all instantiated postcodes from given endpoint and 
    instantiates them in the KG according to OntoBuiltEnv
    (if no 'epc_endpoint' is specified, data is retrieved from all endpoints)

    Arguments:
        epc_endpoint (str) - EPC endpoint from which to retrieve data
                             ['domestic', 'non-domestic', 'display']
                             (None --> retrieve data from all endpoints)
        ocgml_endpoint - SPARQL endpoint with instantiated OntoCityGml buildings
    Returns:
        Tuple of newly instantiated and updated EPCs (new, updated)
    """

    # Retrieve EPC data from all endpoints subsequently if none explicitly specified
    if not epc_endpoint:
        epc_endpoints = ['domestic', 'non-domestic', 'display']
    else:
        epc_endpoints = [epc_endpoint]

    # Initialise return values
    all_epcs = (0, 0)

    # Initialise KG clients
    kgclient_epc = KGClient(query_endpoint, update_endpoint)
    kgclient_ocgml = KGClient(ocgml_endpoint, ocgml_endpoint)

    # Retrieve instantiated postcodes from KG
    query = instantiated_postalcodes()
    res = kgclient_epc.performQuery(query)
    postcodes = [r['postcode'] for r in res]
    postcodes = [p for p in postcodes if p]

    # Split list of postcodes in chunks of max. size n
    n = 500
    postcodes = [postcodes[i:i + n] for i in range(0, len(postcodes), n)]

    # Instantiate EPC data for each API endpoint
    for epc_endpoint in epc_endpoints:
        print(f'Instantiating EPC data for \"{epc_endpoint}\" endpoint ... ')
        #logger.info(f'Instantiating EPC data for \"{epc_endpoint}\" endpoint ... ')

        i = 0
        for pc in postcodes:
            i += 1
            print(f'Instantiating EPC data chunk {i:>4}/{len(postcodes):>4}')
            #logger.info(f'Instantiating EPC data chunk {i:>4}/{len(postcodes):>4}')

            # Instantiate EPC data for postcodes
            epcs, _ = instantiate_epc_data_for_postcodes(postcodes=pc,
                                    epc_endpoint=epc_endpoint, ocgml_endpoint=ocgml_endpoint, 
                                    query_endpoint=query_endpoint, update_endpoint=update_endpoint,
                                    kgclient_epc=kgclient_epc, kgclient_ocgml=kgclient_ocgml,
                                    summarise=False)

            # Update number of amended EPC instances
            all_epcs = tuple([sum(x) for x in zip(all_epcs, epcs)])

    # Summarise EPCs for parent building
    print('Instantiating EPC data for parent buildings ... ')
    #logger.info('Instantiating EPC data for parent buildings ... ')    
    summaries = instantiate_epcs_for_parent_buildings(kgclient=kgclient_epc) 
     
    # Return number of newly instantiated and updated EPCs (single and summaries)
    return (all_epcs, summaries)


def condition_epc_data(data):
    """
        Condition (clean and extract) data returned from EPC API

        Arguments:
            data - dictionary of data as returned from API
    """

    # Initialise dictionary of data to instantiate
    data_to_instantiate = {
        'epc_lmkkey': data.get('lmk-key'),
        'uprn': data.get('uprn'),
        'address_iri': None, 
        'addr_street': None, 
        'addr_number': None,
        'addr_bldg_name': None,
        'addr_unit_name': None,
        'postcode_iri': None, 
        'district_iri': None, 
        'built_form_iri': None, 
        'property_type_iri': None,
        'usage_iri': None, 
        'usage_label': None,
        'construction_start': None, 
        'construction_end': None,
        'floor_description': None, 
        'roof_description': None,
        'wall_description': None, 
        'windows_description': None,
        'floor_area': None, 
        'epc_rating': None, 
        'rooms': None
    }
    
    # Extract address information from provided address fields --> assumption that address 
    # information is provided in decreasing granularity, i.e. line 1 most detailed, etc.
    # order: flat/unit information - building information - street information
    addressinfo = extract_address_information(line1=data.get('address1'), line2=data.get('address2'),
                                              line3=data.get('address3'), prop_type=data.get('property-type'),
                                              epc_endpoint='domestic')
    data_to_instantiate['address_iri'] = KB + 'Address_' + str(uuid.uuid4())
    data_to_instantiate['addr_street'] = addressinfo['street']
    data_to_instantiate['addr_number'] = addressinfo['number']
    data_to_instantiate['addr_bldg_name'] = addressinfo['bldg_name']
    data_to_instantiate['addr_unit_name'] = addressinfo['unit_name']
  
    # Property type and built form
    built_form = data.get('built-form')
    if built_form in EPC_DATA:
        data_to_instantiate['built_form_iri'] = str(EPC_DATA.get(built_form))
    
    data_to_instantiate['property_type_iri'] = str(EPC_DATA.get(data.get('property-type'))) 

    # Usage: assumption that all EPC from domestic API are single residential usage
    data_to_instantiate['usage_iri'] = OBE_SINGLERESIDENTIAL 

    # Energy rating 
    data_to_instantiate[EPC_KEYS.get('current-energy-rating')] = data.get('current-energy-rating')

    # Number of rooms
    try:
        r = data.get('number-habitable-rooms')
        data_to_instantiate[EPC_KEYS.get('number-habitable-rooms')] = int(float(r))
    except Exception:
        # Leave number or rooms as None in case of potential conversion issues
        pass

    # Floor area
    try:
        f = data.get('total-floor-area')
        data_to_instantiate[EPC_KEYS.get('total-floor-area')] = float(f)
    except Exception:
        # Leave floor area as None in case of potential conversion issues
        pass

    # Construction period
    period = EPC_DATA.get(data.get('construction-age-band'))
    if period:
        data_to_instantiate['construction_start'] = period[0]
        data_to_instantiate['construction_end'] = period[1]

    # Construction components
    components = ['floor-description', 'roof-description', 'walls-description',
                  'windows-description']
    for c in components:
        data_to_instantiate[EPC_KEYS.get(c)] = data.get(c)

    # Drop keys with empty values
    data_to_instantiate = {k: v for k, v in data_to_instantiate.items() if v}

    return data_to_instantiate


def condition_epc_non_domestic_data(data):
    """
        Condition (clean and extract) data returned from EPC non-domestic API

        Arguments:
            data - dictionary of data as returned from API
    """

    # Initialise dictionary of data to instantiate
    data_to_instantiate = {
        'epc_lmkkey': data.get('lmk-key'),
        'uprn': data.get('uprn'),
        'address_iri': None, 
        'addr_street': None, 
        'addr_number': None,
        'addr_bldg_name': None,
        'addr_unit_name': None,
        'postcode_iri': None, 
        'district_iri': None, 
        'property_type_iri': None,
        'usage_iri': None, 
        'usage_label': None,
        'floor_area': None, 
        'epc_rating': None, 
    }

    # Extract address information from provided address fields --> assumption that address 
    # information is provided in decreasing granularity, i.e. line 1 most detailed, etc.
    # order: flat/unit information - building information - street information
    addressinfo = extract_address_information(line1=data.get('address1'), line2=data.get('address2'),
                                              line3=data.get('address3'), prop_type=data.get('property-type'),
                                              epc_endpoint = 'non-domestic')
    data_to_instantiate['address_iri'] = KB + 'Address_' + str(uuid.uuid4())
    data_to_instantiate['addr_street'] = addressinfo['street']
    data_to_instantiate['addr_number'] = addressinfo['number']
    data_to_instantiate['addr_bldg_name'] = addressinfo['bldg_name']
    data_to_instantiate['addr_unit_name'] = addressinfo['unit_name']

    # Usage: 'property-type' category from API data describes the building usage for non-domestic EPC
    data_to_instantiate['usage_iri'] = str(EPC_DATA.get((data.get('property-type'))))

    data_to_instantiate['usage_label'] = data.get('property-type')

    # Energy rating 
    data_to_instantiate['epc_rating'] = data.get('asset-rating-band')

    # Floor area
    try:
        f = data.get('floor-area')
        data_to_instantiate[EPC_KEYS.get('floor-area')] = float(f)
    except Exception:
        # Leave floor area as None in case of potential conversion issues
        pass

    # Drop keys with empty values
    data_to_instantiate = {k: v for k, v in data_to_instantiate.items() if v}

    return data_to_instantiate


def condition_epc_display_data(data):
    """
        Condition (clean and extract) data returned from EPC Display API

        Arguments:
            data - dictionary of data as returned from API
    """

    # Initialise dictionary of data to instantiate
    data_to_instantiate = {
        'epc_lmkkey': data.get('lmk-key'),
        'uprn': data.get('uprn'),
        'address_iri': None, 
        'addr_street': None, 
        'addr_number': None,
        'addr_bldg_name': None,
        'addr_unit_name': None,
        'postcode_iri': None, 
        'district_iri': None, 
        'property_type_iri': None,
        'usage_iri': None, 
        'usage_label': None,
        'floor_area': None, 
        'epc_rating': None,
        'weightage': None, 
    }

    # Extract address information from provided address fields --> assumption that address 
    # information is provided in decreasing granularity, i.e. line 1 most detailed, etc.
    # order: flat/unit information - building information - street information
    addressinfo = extract_address_information(line1=data.get('address1'), line2=data.get('address2'),
                                              line3=data.get('address3'), prop_type=data.get('property-type'),
                                              epc_endpoint = 'non-domestic')
    data_to_instantiate['address_iri'] = KB + 'Address_' + str(uuid.uuid4())
    data_to_instantiate['addr_street'] = addressinfo['street']
    data_to_instantiate['addr_number'] = addressinfo['number']
    data_to_instantiate['addr_bldg_name'] = addressinfo['bldg_name']
    data_to_instantiate['addr_unit_name'] = addressinfo['unit_name']

    # Usage: Defined by "Building Category" codes, there can be multiple usage categories for a single building
    # Preprocess the usage retrieved from API by removing any whitespace and splitting into individual usages 
    usage_list = (data.get('building-category')).replace(" ", "").split(';')

    # Empty space ('') in data retrieved from API can be counted as a usage, 
    # hence removing all empty space ('') from the list of usages retrieved
    if '' in usage_list : usage_list.remove('')
    usage = []
    weightage = []
    len_usage = len(usage_list)

    # Only calculate weights if there are multiple usages
    if len_usage > 1:
        usage_label_list = (data.get('property-type')).replace(" ", "").split(';')
        if '' in usage_label_list : usage_label_list.remove('')

        for item in usage_list:
            count = usage_list.count(item)
            weight = count/len_usage
            if (EPC_DATA.get(item) not in usage):
                usage.append(EPC_DATA.get(item))                  
                weightage.append(weight)

        # Usage_iri here is a list of usages
        data_to_instantiate['usage_iri'] = usage 
        # Usage Share (list of weights)
        data_to_instantiate['weightage'] = weightage

        # Labels instantiated only if usages are not aggregated
        if len(usage_label_list) == len(usage):
            data_to_instantiate['usage_label'] = usage_label_list

    else:            
        # Usage_iri for single usage
        data_to_instantiate['usage_iri'] = EPC_DATA.get(usage_list[0]) 
        data_to_instantiate['usage_label'] = data.get('property-type')

    # Energy rating 
    data_to_instantiate['epc_rating'] = data.get('operational-rating-band')

    # Floor area
    try:
        f = data.get('total-floor-area')
        data_to_instantiate[EPC_KEYS.get('total-floor-area')] = float(f)
    except Exception:
        # Leave floor area as None in case of potential conversion issues
        pass

    # Drop keys with empty values
    data_to_instantiate = {k: v for k, v in data_to_instantiate.items() if v}

    return data_to_instantiate


def extract_address_information(line1, line2, line3, prop_type, epc_endpoint):
    """
        Extracts address information from provided EPC data

        Arguments:
            line1 - first line of address
            line2 - second line of address
            line3 - third line of address
            prop_type - property type as provided by EPC API
        
        Returns:
            Dictionary of address information; keys:
            ['street', 'number', 'bldg_name', 'unit_name', 'property-type']
    """

    # Ensure upper case letters for typical unit, property and street names
    names_units = [i.upper() for i in NAMES_UNITS]
    names_bldgs = [i.upper() for i in NAMES_BLDGS]
    names_street = [i.upper() for i in NAMES_STREET]

    # This mapping is only relevant for domestic EPC endpoint
    # Mapping of detailed property types to simpler high level classification
    if (epc_endpoint == 'domestic'):
        BUILDING = 'bldg'
        UNIT = 'unit'
        epc_properties = {
            'HOUSE': BUILDING,
            'BUNGALOW': BUILDING,
            'PARK HOME': BUILDING,
            'FLAT': UNIT,
            'MAISONETTE': UNIT
        }

    # Initialise return dictionary
    cols = ['street', 'number', 'bldg_name', 'unit_name', 'property-type']
    to_inst = {c: None for c in cols}
    to_inst['property-type'] = prop_type

    # ASSUMPTION: Address information provided in decreasing granularity, i.e.
    # order: flat information - building information - street information
    field1 = line1

    if field1:
        # If only first address field is provided -> extract (number and) street
        # if line2.empty and line3.empty:
        if not line2 and not line3:
            to_inst['street'], to_inst['number'] = split_address_info(field1)
        else:
            # If two address fields are provided -> extract additional unit/building info
            # if (not line2.empty and line3.empty) or (not line3.empty and line2.empty):
            if (line2 and not line3) or (not line3 and not line2):
                # field2 = line2 if not line2.empty else line3
                field2 = line2 if line2 else line3
                # 1) For buildings try to extract building name + street info
                if (epc_endpoint == 'non-domestic') or (epc_properties[to_inst['property-type'].upper()] == BUILDING):
                    # If 1st address field contains typical building name and 2nd field
                    # contains splittable street information (this should cover most cases)
                    # -> field 1: building info, field 2: street info
                    if any(b in field1 for b in names_bldgs) and all(split_address_info(field2)):
                        to_inst['street'], to_inst['number'] = split_address_info(field2)
                        to_inst['bldg_name'] = field1
                    # If 2nd field does not contain splittable street information, but typical street name
                    # while 1st field does not contain typical street name
                    # -> field 1: building info, field 2: street info
                    elif any(s in field2 for s in names_street) and not any(s in field1 for s in names_street):
                            to_inst['street'], to_inst['number'] = split_address_info(field2)
                            to_inst['bldg_name'] = field1
                    else:
                        # If 2nd field does not contain typical street information
                        # -> field 1: street info and field 2 likely neglectable (i.e. town name)
                        to_inst['street'], to_inst['number'] = split_address_info(field1)
                # 2) For units try to extract unit name + building name + street info
                else:
                    # If 1st address field contains typical unit name and 2nd field
                    # contains splittable street information (this should cover most cases)
                    # -> field 1: unit info, field 2: street info
                    if any(u in field1 for u in names_units) and all(split_address_info(field2)):
                        to_inst['street'], to_inst['number'] = split_address_info(field2)
                        to_inst['unit_name'] = field1
                    # If 2nd field does not contain splittable street information, but typical street name
                    # -> field 1: unit info, field 2: street info
                    elif any(u in field1 for u in names_units) and any(s in field2 for s in names_street):
                            to_inst['street'], to_inst['number'] = split_address_info(field2)
                            to_inst['unit_name'] = field1
                    # If 2nd field does not contain splittable street information, but typical building name
                    # -> field 1: unit info, field 2: building info
                    elif any(u in field1 for u in names_units) and any(b in field2 for b in names_bldgs):
                            to_inst['bldg_name'] = field2
                            to_inst['unit_name'] = field1
                    else:
                        # If 2nd field does not contain typical street/building information
                        # -> field 1: street info and field 2 likely neglectable (i.e. town name)
                        to_inst['street'], to_inst['number'] = split_address_info(field1)
            else:
                # If three address fields are provided -> extract additional unit/building info
                field2 = line2 
                field3 = line3
                # If first 2 address fields contain typical unit and building name and 3rd field
                # contains splittable street information (this should cover most cases)
                # -> field 1/2: building/unit info, field 3: street info
                if all(split_address_info(field3)):
                    if any(u in field1 for u in names_units) and any(b in field2 for b in names_bldgs):
                        to_inst['street'], to_inst['number'] = split_address_info(field3)
                        to_inst['bldg_name'] = field2
                        to_inst['unit_name'] = field1
                    elif any(u in field2 for u in names_units) and any(b in field1 for b in names_bldgs):
                        to_inst['street'], to_inst['number'] = split_address_info(field3)
                        to_inst['bldg_name'] = field1
                        to_inst['unit_name'] = field2
                # If 3rd field does not contain splittable street information, but typical street name
                # while 2nd field does not contain typical street name
                # -> field 1/2: building/unit info, field 3: street info
                elif any(s in field3 for s in names_street) and not \
                    (any(s in field2 for s in names_street) and all(split_address_info(field2))):
                    # Extract most granualar street information
                    if any(s in field2 for s in names_street):
                        if any(u in field1 for u in names_units):
                            to_inst['street'], to_inst['number'] = split_address_info(field2)
                            to_inst['unit_name'] = field1
                        if any(b in field1 for b in names_bldgs):
                            to_inst['street'], to_inst['number'] = split_address_info(field2)
                            to_inst['bldg_name'] = field1
                    else:
                        if any(u in field1 for u in names_units):
                            to_inst['street'], to_inst['number'] = split_address_info(field3)
                            to_inst['unit_name'] = field1
                            if any(b in field2 for b in names_bldgs):
                                to_inst['bldg_name'] = field2
                        if any(b in field1 for b in names_bldgs):
                            to_inst['street'], to_inst['number'] = split_address_info(field3)
                            to_inst['bldg_name'] = field1
                            if any(u in field2 for u in names_units):
                                to_inst['unit_name'] = field2
                else:
                    # If 3rd field does not contain typical street information, consider only fields 1/2
                    if (epc_endpoint == 'domestic') and epc_properties[to_inst['property-type'].upper()] == UNIT:
                        # If 1st address field contains typical unit name and 2nd field
                        # contains splittable street information (this should cover most cases)
                        # -> field 1: unit info, field 2: street info
                        if any(u in field1 for u in names_units) and all(split_address_info(field2)):
                            to_inst['street'], to_inst['number'] = split_address_info(field2)
                            to_inst['unit_name'] = field1
                        # If 2nd field does not contain splittable street information, but typical street name
                        # -> field 1: unit info, field 2: street info
                        elif any(u in field1 for u in names_units) and any(s in field2 for s in names_street):
                                to_inst['street'], to_inst['number'] = split_address_info(field2)
                                to_inst['unit_name'] = field1
                        # If 2nd field does not contain splittable street information, but typical building name
                        # -> field 1: unit info, field 2: building info
                        elif any(u in field1 for u in names_units) and any(b in field2 for b in names_bldgs):
                                to_inst['bldg_name'] = field2
                                to_inst['unit_name'] = field1
                        else:
                            # If 2nd field does not contain typical street/building information
                            # -> field 1: street info and field 2 likely neglectable (i.e. town name)
                            to_inst['street'], to_inst['number'] = split_address_info(field1)
                    else:
                        # If 1st address field contains typical building name and 2nd field
                        # contains splittable street information 
                        # -> field 1: building info, field 2: street info
                        if any(u in field1 for u in names_bldgs) and all(split_address_info(field2)):
                            to_inst['street'], to_inst['number'] = split_address_info(field2)
                            to_inst['bldg_name'] = field1
                        # If 2nd field does not contain splittable street information, but typical street name
                        # -> field 1: building info, field 2: street info
                        elif any(u in field1 for u in names_bldgs) and any(s in field2 for s in names_street):
                                to_inst['street'], to_inst['number'] = split_address_info(field2)
                                to_inst['bldg_name'] = field1
                        else:
                            # If 2nd field does not contain typical street/building information
                            # -> field 1: street info and field 2 likely neglectable (i.e. town name)
                            to_inst['street'], to_inst['number'] = split_address_info(field1)
    
    return to_inst


def split_address_info(address_info: str):
    """
        Splits address text with potential number into textual and numeric part
    """
    # If numeric parts contains '-' extract entire part as number
    if re.search(r'\d+\s*-\s*\d+', str(address_info)):
        p = r'\d+\s*-*\s*\d+\w*'
    else:
        p = r'\d+\w*'
    splitted = re.findall(p, str(address_info))
    if splitted: 
        number = splitted[0]
        description = str(address_info).replace(number, '')
        description = description.replace(',', '').replace(';','')
        description = description.strip().upper()
        return (description, number)
    else:
        return (str(address_info), None)


def retrieve_ocgml_uprns(uprn: str = '', query_endpoint=OCGML_ENDPOINT,
                         kgclient=None):
    """
        Retrieve all UPRNs (instantiated in OntoCityGml) associated with same 
        cityobject as provided UPRN
        
        Arguments:
            uprn - UPRN to retrieve associated UPRNs (i.e. UPRNs for same building) for 
            query_endpoint - SPARQL endpoint from which to retrieve data
            kgclient - pre-initialized KG client with endpoints

        Returns:
            List of UPRNs (str) (empty list if UPRN not instantiated in OntoCityGml)
                                 
    """

    # Create KG client if not provided
    if not kgclient:
        kgclient = KGClient(query_endpoint, query_endpoint)
    
    # Retrieve UPRNs
    query = get_ocgml_uprns(uprn)
    res = kgclient.performQuery(query)

    # Unwrap results
    uprns = [r['uprns'] for r in res]

    return uprns


def retrieve_parent_building(uprns: list, query_endpoint=QUERY_ENDPOINT,
                             update_endpoint=UPDATE_ENDPOINT, kgclient=None):
    """
    Retrieve parent building for list of UPRNs (i.e. flats)
    
    Arguments:
        uprns - List of UPRNs (str) for which to retrieve parent building
        query_endpoint - SPARQL endpoint from which to retrieve data
        kgclient - pre-initialized KG client with endpoints

    Returns:
        IRI of parent building (None if no parent building instantiated)
    """

    # Initialise parent building IRI
    parent = None

    # Create KG client if not provided
    if not kgclient:
        kgclient = KGClient(query_endpoint, update_endpoint)
    
    # Retrieve parent building IRI
    query = get_parent_building(uprns)
    res = kgclient.performQuery(query)

    # Unwrap results
    bldg = [r['building'] for r in res]
    if len(bldg) == 1:
        parent = bldg[0]
    elif len(bldg) > 1:
        logger.error('No single parent building could be retrieved for list of UPRNs.')
        raise KGException('No single parent building could be retrieved for list of UPRNs.')

    return parent


def instantiate_epcs_for_parent_buildings(query_endpoint=QUERY_ENDPOINT,
                                          update_endpoint=UPDATE_ENDPOINT,
                                          kgclient=None):
    """
    Retrieves instantiated EPC information for "child" properties and
    summarizes and instantiates them for the parent building
    """

    # Initialise return values
    new_epcs = 0
    updated_epcs = 0

    # Create KG client if not provided
    if not kgclient:
        kgclient = KGClient(query_endpoint, update_endpoint)

    # Retrieve EPC data for childen properties and parent buildings
    epcs_data = retrieve_epcs_child_and_parent_buildings(kgclient=kgclient)
    # Summarize EPC information for parent buildings
    summarized = summarize_epc_data(epcs_data)

    columns_to_instantiate = ['property_iri', 'uprn',
        'address_iri', 'addr_street', 'addr_number', 'addr_bldg_name', 'addr_unit_name',
        'postcode_iri', 'district_iri', 'property_type_iri', 'usage_iri', 'usage_label',
        'floor_area', 'epc_rating', 'weightage',            
        # Domestic specific
        'built_form_iri', 'construction_start', 'construction_end',
        'floor_description', 'roof_description', 'wall_description',
        'windows_description', 'rooms']
    
    columns_to_update = ['property_iri', 'property_type_iri', 'usage_iri', 'usage_label',
        'floor_area', 'epc_rating', 'weightage',
        # Domestic specific
        'built_form_iri', 'construction_end', 'floor_description', 'roof_description', 
        'wall_description', 'windows_description', 'rooms']

    # Instantiate / Update EPC information for parent buildings
    for index, row in summarized.iterrows():
        # Instantiate new parent building data
        if row['created']:           
            data_to_instantiate = {k: v for k, v in row.items() if k in columns_to_instantiate}
            if (row['usage_iri'] and (';' in row['usage_iri'])):
                split_usage = row['usage_iri'].split(';')
                split_weight = row['weightage'].split(';')
                data_to_instantiate['usage_iri'] = split_usage
                data_to_instantiate['weightage'] = split_weight
            logger.info('Parent building data not yet instantiated. Instantiate data ... ')
            insert_query = instantiate_epc_data(**data_to_instantiate)
            kgclient.performUpdate(insert_query)
            new_epcs += 1
      
        # Update parent building data
        else:
            data_to_update = {k: v for k, v in row.items() if k in columns_to_update}
            if (row['usage_iri'] and (';' in row['usage_iri'])):
                split_usage = row['usage_iri'].split(';')
                split_weight = row['weightage'].split(';')
                data_to_update['usage_iri'] = split_usage
                data_to_update['weightage'] = split_weight
            logger.info('Parent building data already instantiated. Updated data ... ')
            update_query = update_epc_data(**data_to_update)
            kgclient.performUpdate(update_query)
            updated_epcs += 1

    return (new_epcs, updated_epcs)


def retrieve_epcs_child_and_parent_buildings(query_endpoint=QUERY_ENDPOINT,
                                             update_endpoint=UPDATE_ENDPOINT, 
                                             kgclient=None):
    """
    Retrieves instantiated EPC information for all parent buildings
    and associated children properties from KG

    Returns:
        DataFrame with following columns:
        ['addr_number', 'addr_street', 'addr_bldg_name', 'addr_unit_name', 'address_iri', 
        'built_form_iri', 'construction_end', 'construction_start', 'district_iri', 
        'epc_rating', 'floor_area', 'floor_description', 'parent_iri', 'parent_id', 
        'postcode_iri', 'property_iri', 'property_type_iri', 'roof_description', 'rooms', 
        'usage_iri', 'usage_label', 'wall_description', 'windows_description', 'weightage'] 
    """

    # Create KG client if not provided
    if not kgclient:
        kgclient = KGClient(query_endpoint, update_endpoint)

    # Retrieve EPC data
    query = get_children_and_parent_building_properties()
    res = kgclient.performQuery(query)

    # Unwrap results and create DataFrame
    cols = ['addr_number', 'addr_street', 'addr_bldg_name', 'addr_unit_name', 'address_iri', 
            'built_form_iri', 'construction_end', 'construction_start', 'district_iri', 
            'epc_rating', 'floor_area', 'floor_description', 'parent_iri', 'parent_id', 
            'postcode_iri', 'property_iri', 'property_type_iri', 'roof_description', 'rooms', 
            'usage_iri', 'usage_label', 'wall_description', 'windows_description', 'weightage'] 
    df = pd.DataFrame(columns=cols, data=res)

    # Cast data types
    for c in ['addr_number', 'addr_street', 'addr_bldg_name', 'addr_unit_name', 'address_iri', 
              'built_form_iri', 'district_iri', 'epc_rating', 'floor_description', 'parent_iri', 
              'parent_id', 'postcode_iri', 'property_iri', 'property_type_iri', 'roof_description', 
              'usage_iri', 'usage_label', 'wall_description', 'windows_description'] :
        df[c] = df[c].astype('string')
    df['construction_start'] = pd.to_datetime(df['construction_start'], yearfirst=True, dayfirst=False)
    df['construction_end'] = pd.to_datetime(df['construction_end'], yearfirst=True, dayfirst=False)
    df['floor_area'] = df['floor_area'].astype('float')
    df['weightage'] = df['weightage'].astype('float')
    df['rooms'] = df['rooms'].astype('Int64')

    # Fill missing data with None
    df = df.replace('nan', None)
    df = df.replace('', None)
    df = df.replace({np.nan: None})

    return df


def summarize_epc_data(data):
    """
    Summarizes provided EPC data for "children" properties into aggregate
    values for parent buildings

    Arguments:
        data - DataFrame with following columns:
                ['addr_number', 'addr_street', 'addr_bldg_name', 'addr_unit_name', 'address_iri', 
                'built_form_iri', 'construction_end', 'construction_start', 'district_iri', 
                'epc_rating', 'floor_area', 'floor_description', 'parent_iri', 'parent_id', 
                'postcode_iri', 'property_iri', 'property_type_iri', 'roof_description', 'rooms', 
                'usage_iri', 'usage_label', 'wall_description', 'windows_description', 'weightage'] 
    """

    # Initialise return DataFrame
    cols = ['uprn', 'address_iri', 'addr_street', 'addr_number', 'addr_bldg_name', 'postcode_iri', 
            'district_iri', 'property_type_iri', 'usage_iri', 'floor_area', 'epc_rating',  'created', 
            'weightage',
            # Additional info for domestic buildings
            'built_form_iri', 'rooms', 'construction_start', 'construction_end', 'floor_description', 
            'roof_description', 'wall_description', 'windows_description',]
    df = pd.DataFrame(columns=cols)

    #
    # How to summarize values
    #
    # 1) Sum up actual values of children properties
    sum_up = ['rooms', 'floor_area']
    # 2) Use most common value of children properties
    most_common = ['epc_rating', 'built_form_iri', 'property_type_iri']
    # 3) Aggregate / concatenate distinct values
    agg = ['usage_iri',  'floor_description', 'roof_description',
           'wall_description', 'windows_description']

    # Summarize data per parent building
    parents = data['parent_iri'].unique()
    for p in parents:
        d = data[data['parent_iri'] == p].copy()
    
        # Sum up
        for i in sum_up:
            df.loc[p, i] = d[i].sum(min_count=1)
        # Most common
        for i in most_common:
            if not d[i].value_counts().empty:
                # Retrieve most common value (i.e. value with highest count)
                df.loc[p, i] = d[i].value_counts().index[0]
        # Replace invalid maisonette property type
        if df.loc[p, 'property_type_iri'] == OBE_MAISONETTE:
            df.loc[p, 'property_type_iri'] = OBE_HOUSE

        # Concatenate distinct values
        for i in agg:
            # Assess list of usages and respetive weightages
            if (i == 'usage_iri'):
                usage_list = list(d['usage_iri'])
                total_usages = len(usage_list)

                vals = list(d['usage_iri'].unique())
                weightage = []
                if len(vals) > 1:
                    for item in vals:
                        count = (usage_list.count(item))
                        weight = count/total_usages
                        weightage.append((weight))
                    df.loc[p, 'weightage'] = ';'.join([str(w) for w in weightage])

                vals = [v for v in vals if v]                
                
                if vals:
                    concatenated = ';'.join(vals)
                    # Replace invalid single residential usage
                    concatenated_fixed = concatenated.replace(OBE_SINGLERESIDENTIAL, OBE_MULTIRESIDENTIAL)
                    df.loc[p, 'usage_iri'] = concatenated_fixed
            # Simply concatenate all other values
            else:
                vals = list(d[i].unique())
                vals = [v for v in vals if v]
                if vals:
                    concatenated = ';'.join(vals)
                    df.loc[p, i] = concatenated

        # Retrieve construction dates
        try:
            # Earliest construction start
            df.loc[p, 'construction_start'] = d['construction_start'].min()
            # Latest construction end
            df.loc[p, 'construction_end'] = d['construction_end'].max()
        except Exception:
            logger.info('No construction date data could be obtained.')
        
        # Retrieve postcode and admin district IRIs
        try:
            df.loc[p, 'district_iri'] = d['district_iri'].unique()[0]
        except Exception:
            logger.info('No AdministrativeDistrict IRI could be obtained.')
        try:
            df.loc[p, 'postcode_iri'] = d['postcode_iri'].unique()[0]
        except Exception:
            logger.info('No PostalCode IRI could be obtained.')

        # Retrieve/create unique identifier for parent building ("UPRN equivalent")
        uprn = d['parent_id'].unique()[0]
        if not uprn: 
            uprn = str(uuid.uuid4())
            df.loc[p, 'created'] = True
        df.loc[p, 'uprn'] = uprn

        # Retrieve/create address information for parent building
        addresses = list(d['address_iri'].unique())
        addresses = [a for a in addresses if a]
        if len(addresses) == 1:
            # in case all children have same address
            address = addresses[0]
        else:
            # in case children have different / no address
            address = KB + 'Address_' + str(uuid.uuid4())
        df.loc[p, 'address_iri'] = address

        # Extract street and property number as well as building name
        try:
            street = sorted(d['addr_street'].unique())
            street = [s for s in street if s]
            # Remove leading and trailing single letters, i.e. "A King Street", "B King Street"
            street = [re.sub('^[a-zA-Z] ', '', s) for s in street]
            street = [re.sub(' [a-zA-Z]$', '', s) for s in street]
            street = list(set(street))
            if street:
                concatenated = '; '.join(street)         
                df.loc[p, 'addr_street'] = concatenated
        except Exception:
            logger.info('No Street name information could be obtained.')
        
        try:
            nr = sorted(d['addr_number'].unique())
            nr = [n for n in nr if n]
            if nr:
                concatenated = ', '.join(nr)
                df.loc[p, 'addr_number'] = concatenated
        except Exception:
            logger.info('No Property number information could be obtained.')

        try:
            bldg = sorted(d['addr_bldg_name'].unique())
            bldg = [n for n in bldg if n]
            # Remove leading and trailing whitespaces and numbers, i.e. "8 King Castle", " King Castle"
            bldg = [re.sub('^\d*\s*', '', s) for s in bldg]
            bldg = [re.sub('\s*\d*$', '', s) for s in bldg]
            bldg = list(set(bldg))
            if bldg:
                concatenated = '; '.join(nr)
                df.loc[p, 'addr_bldg_name'] = concatenated
        except Exception:
            logger.info('No Building name information could be obtained.')


    # Create 'property_iri' column
    df['property_iri'] = df.index
    df.reset_index(drop=True)
    # Fill missing data with None
    df = df.replace('nan', None)
    df = df.replace('', None)
    df = df.replace({np.nan: None})

    return df


def add_ocgml_building_data(query_endpoint=QUERY_ENDPOINT, 
                            update_endpoint=UPDATE_ENDPOINT, 
                            ocgml_endpoint=OCGML_ENDPOINT,
                            kgclient_epc=None, kgclient_ocgml=None):
    '''
    Retrieve relevant building information (i.e. footprint, elevation) from
    OntoCityGml SPARQl endpoint and instantiate/update according to OntoBuiltEnv
    (elevation as triples, footprint uploaded to PostGIS)

    Arguments:
        query_endpoint - Blazegraph endpoint with EPC KG
        ocgml_endpoint - Blazegraph endpoint with instantiated OntoCityGml buildings
        trans - pyproj transformation object
        kgclient_epc, kgclient_ocgml - KGClient instances for EPC and OCGML endpoints

    '''
    # Initialise return values
    footprints_new = 0
    footprints_dup = 0
    elevations_new = 0
    elevations_old = 0

    # Initialise relevant Stack Clients and parameters
    postgis_client = PostGISClient()
    gdal_client = GdalClient()
    geoserver_client = GeoserverClient()
    feature_type = 'Building'

    # Create KG clients if not provided
    if not kgclient_epc:
        kgclient_epc = KGClient(query_endpoint, update_endpoint)
    if not kgclient_ocgml:
        kgclient_ocgml = KGClient(ocgml_endpoint, ocgml_endpoint)

    #
    # 1) Retrieve Coordinate Reference System form OCGML endpoint
    #
    trans, target_crs = initialise_pyproj_projection(kgclient_epc=kgclient_epc, 
                                                     kgclient_ocgml=kgclient_ocgml)

    #
    # 2) Query information for matched buildings
    #
    query = get_matched_buildings()
    try:
        kg_bldgs = kgclient_epc.performQuery(query)
        obe_bldg_iris = [b['obe_bldg'] for b in kg_bldgs]
    except KGException as ex:
        logger.error('Unable to retrieve matched buildings from endpoints.')
        raise KGException('Unable to retrieve matched buildings from endpoints.') from ex
    # Check if buildings have been matched yet
    if not obe_bldg_iris:
        logger.warn('No relationships between OntoBuiltEnv and OntoCityGml buildings ' + \
                    'instances could be retrieved. Please run Building Matching Agent first.')
    else: 
        #
        # 3) Process buildings' information in chunks of max. n buildings
        #
        n = 500
        bldg_iris = [obe_bldg_iris[i:i + n] for i in range(0, len(obe_bldg_iris), n)]

        i = 0
        for iri_chunk in bldg_iris:
            i += 1
            print(f'Instantiating OCGML data chunk {i:>4}/{len(bldg_iris):>4}')

            logger.info(f'Retrieving OCGML data for chunk  {i:>4}/{len(bldg_iris):>4} ...')
            query = get_matched_ocgml_information(obe_endpoint=query_endpoint,
                                                  ocgml_endpoint=ocgml_endpoint,
                                                  bldg_iris=iri_chunk) 
            try:
                # Return format
                # [{'obe_bldg': '...', 'surf': '...', 'datatype': '...', 'geom': '...', 'height': '...' }, ...]
                ocgml_data = kgclient_epc.performQuery(query)
            except KGException as ex:
                logger.error('Unable to retrieve information for matched buildings.')
                raise KGException('Unable to retrieve information for matched buildings.') from ex

            # Create DataFrame from dictionary list
            cols = ['obe_bldg', 'surf', 'datatype', 'geom', 'height', 'elevation']
            data = pd.DataFrame(columns=cols, data=ocgml_data)

            # Iterate through all buildings (each building represents one geospatial feature)
            for b in data['obe_bldg'].unique():
                # Get building usage
                query = get_buildings_usage(b)
                try:
                    retrieved_usage = kgclient_epc.performQuery(query)
                except KGException as ex:
                    logger.error('Unable to retrieve building usage category.')
                    raise KGException('Unable to retrieve building usage category.') from ex
                
                usages = []
                # Map usage(s)
                if len(retrieved_usage) == 1:
                    primary_usage = retrieved_usage[0].get('usage')
                    usages.append(primary_usage)
                    # Get usage category for detailed primary usage; in case primary usage
                    # is already "general" (i.e. not a key in mapping dict), use it directly
                    usage_category = USAGE_MAPPING.get(primary_usage, primary_usage)
                else:
                    max_weight = 0
                    for u in retrieved_usage:
                        usages.append(u.get('usage'))
                        query = get_usage_share(u.get('usage_iri'))
                        try:
                            weight = (kgclient_epc.performQuery(query))
                        except KGException as ex:
                            logger.error('Unable to retrieve usage share.')
                            raise KGException('Unable to retrieve usage share.') from ex
                        if float(weight[0].get('share')) > max_weight:
                            #NOTE: In case of equal usage weights, the first usage is used
                            max_weight = float(weight[0].get('share'))
                            primary_usage = u.get('usage')
                    usage_category = USAGE_MAPPING.get(primary_usage, primary_usage)
                # Convert list of usages to concatenated string (to be JSON compatible)
                usages = ';'.join(usages)

                # Extract all floor surface geometries for this building
                surf = data[data['obe_bldg'] == b]
                # Initialise list of surface geometry coordinates (polygons)
                all_polygons = []

                # Iterate through all surface geometries
                for s in surf['surf'].unique():
                    # Extract list of linear rings forming this surface polygon
                    polytype = surf[surf['surf'] == s]['datatype'].values[0]
                    polydata = surf[surf['surf'] == s]['geom'].values[0]
                    # Initialise base elevation
                    base_elevation = np.inf
                    # Transform coordinates for surface geometry
                    polygon, zmin = get_coordinates(polydata, polytype, trans, 
                                                    dimensions=3)
                    # Append transformed polygon coordinate list as sublist to overall list for building
                    all_polygons.append(polygon)
                    # Potentially update min elevation
                    if zmin < base_elevation:
                        base_elevation = zmin
                    data.loc[data['obe_bldg'] == b, 'elevation'] = base_elevation

                #
                # a) Instantiate/update building footprint (in PostGIS)
                #
                # Prepare GeoJSON to upload to PostGIS (each building is represented by 2D base polygon)
                logger.info('Creating GeoJSON feature ...')
                base_polygon = [[]]
                for p in all_polygons:
                    # Get entire base polygon (incl. interior rings) and convert to 2D
                    for poly in p:
                        # Trim dimensions from 3D to 2D by dropping Z value
                        poly = np.array(poly)[:, :2]
                        # Create list to allow for composite ground surfaces
                        base_polygon[0].append(poly.tolist())

                # Define GeoJSON properties
                props = {
                    # Initially required by DTVF (potentially outdated, but kept for reference)
                    'iri': b,
                    'name': b.split('/')[-1].replace('>',''),
                    'endpoint': QUERY_ENDPOINT,
                    # Required for Ontop
                    'geom_iri': b + '/geometry',
                    # Optional (for styling)
                    'type': feature_type,
                    'primary_usage': primary_usage,
                    'primary_usage_category': usage_category,
                    'usages': usages,
                }
                if surf.get('height').any():
                    props['building height'] = float(surf['height'].iloc[0])

                # Create GeoJSON Feature
                feature = create_geojson_feature(base_polygon, props,
                                                 crs_name=target_crs)

                # Ensure that ALL linear rings follow the right-hand rule, i.e. exterior rings specified counterclockwise
                # as required per standard: https://datatracker.ietf.org/doc/html/rfc7946#section-3.1.6
                geojson = rewind(feature)
                # Extract 'geometry' node and ensure GeoJSON objects are converted to String
                # (sometimes rewind returns string and sometimes dict)
                if type(geojson) is not str:
                    geojson_geom_str = json.dumps(geojson['geometry'])
                    geojson_str = json.dumps(geojson)
                else:
                    geojson_str = geojson
                    geojson_geom_str = json.dumps(json.loads(geojson_str)['geometry'])
                
                # Upload OBDA mapping and create Geoserver layer when first geospatial
                # data is uploaded to PostGIS
                if not postgis_client.check_table_exists():
                    logger.info('Uploading OBDA mapping ...')
                    OntopClient.upload_ontop_mapping()
                    # Initial data upload required to create postGIS table and Geoserver layer            
                    logger.info('Uploading GeoJSON to PostGIS ...')
                    gdal_client.uploadGeoJSON(geojson_str)
                    footprints_new += 1
                    logger.info('Creating layer in Geoserver ...')
                    geoserver_client.create_workspace()
                    geoserver_client.create_postgis_layer()
                else:        
                    # Upload new geospatial information                    
                    if not postgis_client.check_polygon_feature_exists(geojson_geom_str, feature_type):
                        logger.info('Uploading GeoJSON to PostGIS ...')
                        gdal_client.uploadGeoJSON(geojson_str)
                        footprints_new += 1
                    else:
                        logger.info(f'Feature with same geometry and type already exists when processing <{b}>.')
                        footprints_dup += 1

            #
            # b) Instantiate/update building elevation
            #
            # Delete (potentially) old building height triples
            iris = list(data['obe_bldg'].unique())
            logger.info('Deleting (potentially) outdated elevation triples ...')
            delete_query = delete_old_building_elevation(iris)
            kgclient_epc.performUpdate(delete_query)
            elevations_old += len(iris)

            # Instantiate retrieved building elevation for linked buildings
            batch = data[['obe_bldg', 'elevation']].copy()
            batch = batch.drop_duplicates(subset=['obe_bldg'])
            batch['unit'] = 'm'
            batch = batch.to_dict('records')
            logger.info('Instantiating latest elevation triples ...')
            insert_query = instantiate_building_elevation(batch)
            kgclient_epc.performUpdate(insert_query)
            elevations_new += len(batch)

    return (footprints_new, footprints_dup, elevations_new, elevations_old)


def initialise_pyproj_projection(query_endpoint=QUERY_ENDPOINT,
                                 ocgml_endpoint=OCGML_ENDPOINT,
                                 kgclient_epc=None, kgclient_ocgml=None):
    '''
    Initialise pyproj projection for coordinate transformation from OCGML CRS to 
    PostGIS (i.e. EPSG:4326)

    Arguments:
        query_endpoint - Blazegraph endpoint with EPC KG
        ocgml_endpoint - Blazegraph endpoint with instantiated OntoCityGml buildings
        kgclient_epc, kgclient_ocgml - KGClient instances for EPC and OCGML endpoints
    
    Retruns:
        pyproj projection instance
        ocgml_crs - OCGML coordinate reference system
        target_crs - Target coordinate reference system (i.e. EPSG:4326)
    '''

    # Create KG clients if not provided
    if not kgclient_epc:
        kgclient_epc = KGClient(query_endpoint, query_endpoint)
    if not kgclient_ocgml:
        kgclient_ocgml = KGClient(ocgml_endpoint, ocgml_endpoint)

    # Retrieve Coordinate Reference System from OCGML endpoint
    query = get_ocgml_crs()
    try:
        kg_crs = kgclient_ocgml.performQuery(query)
        # Unpack queried CRS result to extract coordinate reference system
        if len(kg_crs) != 1:
            logger.error('No or multiple CRS detected in SPARQL query result.')
            raise ValueError('No or multiple CRS detected in SPARQL query result.')
        else:
            crs = kg_crs[0]['crs']
    except KGException as ex:
        logger.error('Unable to retrieve CRS from OCGML endpoint.')
        raise KGException('Unable to retrieve CRS from OCGML endpoint.') from ex
    try:
        # Initialise Pyproj projection from OCGML CRS to EPSG:4326 (current Postgis default)
        ocgml_crs = CRSs[crs]
        target_crs = CRSs['EPSG:4326']
        trans = initialise_pyproj_transformer(current_crs=ocgml_crs, 
                                              target_crs=target_crs)
    except Exception as ex:
        logger.error('Unable to initialise Pyproj transformation object.')
        raise RuntimeError('Unable to initialise Pyproj transformation object.') from ex

    return trans, target_crs


if __name__ == '__main__':

    epcs, summaries = instantiate_epc_data_for_all_postcodes()
    print(f'Newly instantiated EPCs: {epcs[0]}')
    print(f'Updated EPCs: {epcs[1]}')
    print(f'Newly instantiated EPC summaries: {summaries[0]}')
    print(f'Updated EPC summaries: {summaries[1]}')
