################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 15 Sep 2022                            #
################################################

# The purpose of this module is to instantiate data retrieved from
# the energy performance certificates API according to OntoBuiltEnv

import re
import uuid
import numpy as np
import pandas as pd

import agentlogging

from epcdata.datamodel.iris import *
from epcdata.datamodel.data_mapping import *
from epcdata.errorhandling.exceptions import KGException
from epcdata.kgutils.kgclient import KGClient
from epcdata.utils.env_configs import OCGML_ENDPOINT
from epcdata.utils.stack_configs import QUERY_ENDPOINT, UPDATE_ENDPOINT
from epcdata.kgutils.querytemplates import *
from epcdata.datainstantiation.epc_retrieval import obtain_data_for_certificate, \
                                                    obtain_latest_data_for_postcodes


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

    # Retrieve EPC data
    epc_data = obtain_data_for_certificate(lmk_key, epc_endpoint)
    # Instantiate data (if successfully retrieved)
    if epc_data:
        # Retrieve UPRN info required to match data
        try:
            uprn = epc_data.get('uprn')
        except:
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
                #TODO: Potentially incorporate more thorough check here, i.e. to
                # avoid Flats (according to EPC data) having OCGML building representation
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
                data_to_instantiate = condition_epc_data(epc_data)

                if not instantiated:
                    # 2) No EPC data instantiated yet --> Instantiate data
                    # Create Property IRI
                    if epc_data.get('property-type') in ['Flat', 'Maisonette']:
                        data_to_instantiate['property_iri'] = OBE_FLAT + '_' + str(uuid.uuid4())
                    else:
                        data_to_instantiate['property_iri'] = KB + 'Building_' + str(uuid.uuid4())

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
                    data_to_update = ['epc_lmkkey', 'built_form_iri', 
                                      'property_type_iri','usage_iri', 'usage_label',
                                      'construction_end','floor_description', 
                                      'roof_description','wall_description', 
                                      'windows_description','floor_area', 'epc_rating', 'rooms']
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
            # Relevant UPRNs are instantiated in OntoCityGml (i.e. have geospatial representation)
            if len(uprns) == 1:
                #TODO: Potentially incorporate more thorough check here, i.e. to
                # avoid Flats (according to EPC data) having OCGML building representation
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
            if instantiated and instantiated[0].get('certificate') == row.get('lmk-key'):
                # 1) EPC data up to date --> Do nothing
                logger.info('EPC data for UPRN still up to date. No update needed.')
                updates = (0, 0)
            else:
                # 2) & 3) Data not instantiated at all or outdated --> condition data
                data_to_instantiate = condition_epc_data(row)

                if not instantiated:
                    # 2) No EPC data instantiated yet --> Instantiate data
                    # Create Property IRI
                    if row.get('property-type') in ['Flat', 'Maisonette']:
                        data_to_instantiate['property_iri'] = OBE_FLAT + '_' + str(uuid.uuid4())
                    else:
                        data_to_instantiate['property_iri'] = KB + 'Building_' + str(uuid.uuid4())

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
                    data_to_update = ['epc_lmkkey', 'built_form_iri', 
                                      'property_type_iri','usage_iri', 'usage_label',
                                      'construction_end','floor_description', 
                                      'roof_description','wall_description', 
                                      'windows_description','floor_area', 'epc_rating', 'rooms']
                    data_to_update = {k: v for k, v in data_to_instantiate.items() if k in data_to_update}
                    data_to_update['property_iri'] = instantiated[0].get('property')

                    logger.info('Instantiated EPC data for UPRN outdated. Updated data ... ')
                    update_query = update_epc_data(**data_to_update)
                    kgclient_epc.performUpdate(update_query)
                    updated_epcs += 1
        else:
            logger.info('No associated UPRNs are instantiated in OntoCityGml endpoint.')
    else:
        logger.info('No EPC data available for provided lodgement identifier.')

    if summarise:
        # Potentially "summarise" EPCs for parent buildings
        new_summaries, updated_summaries = instantiate_epcs_for_parent_buildings(kgclient=kgclient_epc)    

    return (new_epcs, updated_epcs), (new_summaries, updated_summaries)


def instantiate_epc_data_for_all_postcodes(epc_endpoint='domestic',
                                           ocgml_endpoint=OCGML_ENDPOINT,
                                           query_endpoint=QUERY_ENDPOINT, 
                                           update_endpoint=UPDATE_ENDPOINT):
    """
    Retrieves EPC data for all instantiated postcodes from given endpoint and 
    instantiates them in the KG according to OntoBuiltEnv

    Arguments:
        epc_endpoint (str) - EPC endpoint from which to retrieve data
                             ['domestic', 'non-domestic', 'display']
        ocgml_endpoint - SPARQL endpoint with instantiated OntoCityGml buildings
    Returns:
        Tuple of newly instantiated and updated EPCs (new, updated)
    """

    # Initialise return values
    all_epcs = (0, 0)
    all_summaries = (0, 0)

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
    
    i = 0
    for pc in postcodes:
        i += 1
        print(f'Instantiating EPC data chunk {i:>4}/{len(postcodes):>4}')

        # Instantiate EPC data for postcodes
        epcs, _ = instantiate_epc_data_for_postcodes(postcodes=pc,
                                epc_endpoint=epc_endpoint, ocgml_endpoint=ocgml_endpoint, 
                                query_endpoint=query_endpoint, update_endpoint=update_endpoint,
                                kgclient_epc=kgclient_epc, kgclient_ocgml=kgclient_ocgml,
                                summarise=False)

        # Update number of amended EPC instances
        all_epcs = tuple([sum(x) for x in zip(all_epcs, epcs)])

    # Summarise EPCs for parent building
    summaries = instantiate_epcs_for_parent_buildings(kgclient=kgclient_epc)  
    all_summaries = tuple([sum(x) for x in zip(all_summaries, summaries)])    
    
    # Return number of newly instantiated and updated EPCs (single and summaries)
    return (all_epcs, all_summaries)


def condition_epc_data(data):
    """
        Condition (clean and extract) data returned from EPC API

        Arguments:
            data - dictionary of data as returned from API
    """

    # Initialise dictionary of data to Instantiate
    data_to_instantiate = {
        'epc_lmkkey': data.get('lmk-key'),
        'uprn': data.get('uprn'),
        'address_iri': None, 
        'addr_street': None, 
        'addr_number': None,
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

    # Extract address information
    # There are 3 address fields --> assumption that street and number are provided
    # in same field; Street names to be stored in all capital letters
    address_strings = [data.get('address1'), data.get('address2'), data.get('address3')]
    street, nr = extract_street_and_number(address_strings)
    data_to_instantiate['address_iri'] = KB + 'Address_' + str(uuid.uuid4())
    data_to_instantiate['addr_street'] = street
    data_to_instantiate['addr_number'] = nr
  
    # Property type and built form
    data_to_instantiate['built_form_iri'] = EPC_DATA.get(data.get('built-form'))
    data_to_instantiate['property_type_iri'] = EPC_DATA.get(data.get('property-type'))

    # Usage: assumption that all EPC from domestic API are single residential usage
    data_to_instantiate['usage_iri'] = OBE_SINGLERESIDENTIAL

    # Energy rating 
    data_to_instantiate[EPC_KEYS.get('current-energy-rating')] = data.get('current-energy-rating')

    # Number of rooms
    try:
        r = data.get('number-habitable-rooms')
        data_to_instantiate[EPC_KEYS.get('number-habitable-rooms')] = int(float(r))
    except:
        # Leave number or rooms as None in case of potential conversion issues
        pass

    # Floor area
    try:
        f = data.get('total-floor-area')
        data_to_instantiate[EPC_KEYS.get('total-floor-area')] = float(f)
    except:
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


def extract_street_and_number(address_strings):
    # Drop None elements
    address_strings = [a for a in address_strings if a]

    street = None
    nr = None
    for addr in address_strings:
        nr = re.findall(r'\d+', addr)
        if nr: 
            nr = nr[0]
            street = addr.replace(nr, '')
            street = street.replace(',', '').replace(';','')
            street = street.strip().upper()
        if street and nr:
            break

    return street, nr


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
                                          kgclient=None, ):
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
        'address_iri', 'addr_street', 'addr_number', 'postcode_iri',
        'district_iri', 'built_form_iri', 'property_type_iri',
        'usage_iri', 'usage_label', 'construction_start', 'construction_end',
        'floor_description', 'roof_description', 'wall_description',
        'windows_description', 'floor_area', 'epc_rating', 'rooms']
    
    columns_to_update = ['property_iri', 'built_form_iri', 'property_type_iri',
                         'usage_iri', 'usage_label', 'construction_end',
                         'floor_description', 'roof_description', 'wall_description', 
                         'windows_description','floor_area', 'epc_rating', 'rooms']

    # Instantiate / Update EPC information for parent buildings
    for index, row in summarized.iterrows():
        # Instantiate new parent building data
        if row['created']:
            data_to_instantiate = {k: v for k, v in row.items() if k in columns_to_instantiate}
            logger.info('Parent building data not yet instantiated. Instantiate data ... ')
            insert_query = instantiate_epc_data(**data_to_instantiate)
            kgclient.performUpdate(insert_query)
            new_epcs += 1
      
        # Update parent building data
        else:
            data_to_update = {k: v for k, v in row.items() if k in columns_to_update}
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
            ['addr_number', 'addr_street', 'address_iri', 'built_form_iri', 'construction_end', 
            'construction_start', 'district_iri', 'epc_rating', 'floor_area', 'floor_description', 
            'parent_iri', 'parent_id', 'postcode_iri', 'property_iri', 'property_type_iri', 
            'roof_description', 'rooms', 'usage_iri', 'usage_label', 'wall_description', 
            'windows_description'] 
    """

    # Create KG client if not provided
    if not kgclient:
        kgclient = KGClient(query_endpoint, update_endpoint)

    # Retrieve EPC data
    query = get_children_and_parent_building_properties()
    res = kgclient.performQuery(query)

    # Unwrap results and create DataFrame
    cols = ['addr_number', 'addr_street', 'address_iri', 'built_form_iri', 'construction_end', 
            'construction_start', 'district_iri', 'epc_rating', 'floor_area', 'floor_description', 
            'parent_iri', 'parent_id', 'postcode_iri', 'property_iri', 'property_type_iri', 
            'roof_description', 'rooms', 'usage_iri', 'usage_label', 'wall_description', 
            'windows_description'] 
    df = pd.DataFrame(columns=cols, data=res)

    # Cast data types
    for c in ['addr_number', 'addr_street', 'address_iri', 'built_form_iri', 
              'district_iri', 'epc_rating', 'floor_description', 'parent_iri', 'parent_id', 
              'postcode_iri', 'property_iri', 'property_type_iri', 'roof_description', 
              'usage_iri', 'usage_label', 'wall_description', 'windows_description'] :
        df[c] = df[c].astype('string')
    df['construction_start'] = pd.to_datetime(df['construction_start'], yearfirst=True, dayfirst=False)
    df['construction_end'] = pd.to_datetime(df['construction_end'], yearfirst=True, dayfirst=False)
    df['floor_area'] = df['floor_area'].astype('float')
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
                  ['addr_number', 'addr_street', 'address_iri', 'built_form_iri', 'construction_end', 
                  'construction_start', 'district_iri', 'epc_rating', 'floor_area', 'floor_description', 
                  'parent_iri', 'parent_id', 'postcode_iri', 'property_iri', 'property_type_iri', 
                  'roof_description', 'rooms', 'usage_iri', 'usage_label', 'wall_description',
                  'windows_description'] 
    """

    # Initialise return DataFrame
    cols = ['uprn', 'address_iri', 'addr_street', 'addr_number', 'postcode_iri', 'district_iri',
            'built_form_iri', 'property_type_iri', 'usage_iri', 'usage_label', 
            'construction_start', 'construction_end', 'floor_description', 'roof_description', 
            'wall_description', 'windows_description', 'floor_area', 'epc_rating', 'rooms', 'created']
    df = pd.DataFrame(columns=cols)

    #
    # How to summarize values
    #
    # 1) Sum up actual values of children properties
    sum_up = ['rooms', 'floor_area']
    # 2) Use most common value of children properties
    most_common = ['epc_rating', 'built_form_iri', 'property_type_iri']
    # 3) Aggregate / concatenate distinct values
    agg = ['usage_iri', 'usage_label', 'floor_description', 'roof_description', 
           'wall_description', 'windows_description']

    # Summarize data per parent building
    parents = data['parent_iri'].unique()
    for p in parents:
        d = data[data['parent_iri'] == p].copy()
       
        # sum up
        for i in sum_up:
            df.loc[p, i] = d[i].sum()
        # most common
        for i in most_common:
            if not d[i].value_counts().empty:
                # Retrieve most common value (i.e. value with highest count)
                df.loc[p, i] = d[i].value_counts().index[0]
        # Replace invalid maisonette property type
        if df.loc[p, 'property_type_iri'] == OBE_MAISONETTE:
            df.loc[p, 'property_type_iri'] = OBE_HOUSE
        # concatenate distinct values
        for i in agg:
            vals = list(d[i].unique())
            vals = [v for v in vals if v]
            if vals:
                concatenated = '; '.join(vals)
                # Replace invalid single residential usage
                if i == 'usage_iri':
                    concatenated = concatenated.replace(OBE_SINGLERESIDENTIAL, OBE_MULTIRESIDENTIAL)
                df.loc[p, i] = concatenated

        # Retrieve construction dates
        try:
            # Earliest construction start
            df.loc[p, 'construction_start'] = d['construction_start'].min()
            # Latest construction end
            df.loc[p, 'construction_end'] = d['construction_end'].max()
        except:
            logger.info('No construction date data could be obtained.')
        
        # Retrieve postcode and admin district IRIs
        try:
            df.loc[p, 'district_iri'] = d['district_iri'].unique()[0]
        except:
            logger.info('No AdministrativeDistrict IRI could be obtained.')
        try:
            df.loc[p, 'postcode_iri'] = d['postcode_iri'].unique()[0]
        except:
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

        # Extract street and property number
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
        except:
            logger.info('No Street name information be obtained.')
        
        try:
            nr = sorted(d['addr_number'].unique())
            nr = [n for n in nr if n]
            if nr:
                concatenated = ', '.join(nr)
                df.loc[p, 'addr_number'] = concatenated
        except:
            logger.info('No Property number information be obtained.')

    # Create 'property_iri' column
    df['property_iri'] = df.index
    df.reset_index(drop=True)

    # Fill missing data with None
    df = df.replace('nan', None)
    df = df.replace('', None)
    df = df.replace({np.nan: None})

    return df


def update_building_elevation(query_endpoint=QUERY_ENDPOINT,
                              update_endpoint=UPDATE_ENDPOINT, 
                              ocgml_endpoint=OCGML_ENDPOINT,
                              kgclient_epc=None, kgclient_ocgml=None):
    '''
        Retrieve building elevation from OntoCityGml SPARQl endpoint and
        instantiate/update according to OntoBuiltEnv
    '''
    elevations = 0

    # Create KG clients if not provided
    if not kgclient_epc:
        kgclient_epc = KGClient(query_endpoint, update_endpoint)
    if not kgclient_ocgml:
        kgclient_ocgml = KGClient(ocgml_endpoint, ocgml_endpoint)

    # Query information from matched buildings
    # [{'unit': '...', 'obe_bldg': '...', 'height': '...'}, ...]
    query = get_matched_ocgml_information(obe_endpoint=query_endpoint,
                                          ocgml_endpoint=ocgml_endpoint)
    matches = kgclient_epc.performQuery(query)

    # Check if buildings have been matched yet
    if not matches:
        logger.warn('No relationships between OntoBuiltEnv and OntoCityGml buildings ' + \
                    'instances could be retrieved. Please run Building Matching Agent first.')
    else:
        # Split matches into chunks of max. size n
        n = 1000
        batches = [matches[i:i + n] for i in range(0, len(matches), n)]

        # Update each chunk of matched buildings
        i = 0
        for batch in batches:
            i += 1
            print(f'Updating building elevation chunk {i:>3}/{len(batches):>3}')
            elevations += len(batch)
            # Extract building IRIs
            iris = [b['obe_bldg'] for b in batch]

            # Delete (potentially) old building height triples
            logger.info('Deleting (potentially) outdated elevation triples.')
            delete_query = delete_old_building_elevation(iris)
            kgclient_epc.performUpdate(delete_query)

            # Instantiate retrieved building elevation for linked buildings
            logger.info('Instantiating latest elevation triples.')
            insert_query = instantiate_building_elevation(batch)
            kgclient_epc.performUpdate(insert_query)

    return elevations


if __name__ == '__main__':

    epcs, summaries = instantiate_epc_data_for_all_postcodes()
    print(f'Newly instantiated EPCs: {epcs[0]}')
    print(f'Updated EPCs: {epcs[1]}')
    print(f'Newly instantiated EPC summaries: {summaries[0]}')
    print(f'Updated EPC summaries: {summaries[1]}')
