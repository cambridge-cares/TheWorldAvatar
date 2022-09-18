################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 15 Sep 2022                            #
################################################

# The purpose of this module is to instantiate data retrieved from
# the energy performance certificates API according to OntoBuiltEnv

import re
import uuid

import agentlogging
from epcdata.datamodel.iris import *
from epcdata.datamodel.data_mapping import *
from epcdata.errorhandling.exceptions import KGException

from epcdata.kgutils.kgclient import KGClient
from epcdata.utils.stack_configs import QUERY_ENDPOINT, UPDATE_ENDPOINT
from epcdata.kgutils.querytemplates import instantiate_epc_data, update_epc_data, \
                                           get_latest_epc_and_property_iri_for_uprn, \
                                           get_postcode_and_district_iris, get_ocgml_uprns, \
                                           get_parent_building
from epcdata.datainstantiation.epc_retrieval import obtain_data_for_certificate


# Initialise logger
logger = agentlogging.get_logger("prod")


def instantiate_data_for_certificate(lmk_key: str, epc_endpoint='domestic',
                                     ocgml_endpoint=None,
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

        # Initialise KG client
        kgclient = KGClient(query_endpoint, update_endpoint)
        
        #
        # Check if UPRN is instantiated in OntoCityGml and whether parent building 
        # (i.e. building hosting several flats/UPRNs) shall be instantiated
        #
        uprns = retrieve_ocgml_uprns(uprn, ocgml_endpoint)
        if uprns:
            # Relevant UPRNs are instantiated in OntoCityGml (i.e. have geospatial representation)
            if len(uprns) == 1:
                #TODO: Potentially incorporate more thorough check here, i.e. to
                # avoid Flats (according to EPC data) having OCGML building representation
                parent_iri = None
            else:
                parent = retrieve_parent_building(uprns, kgclient=kgclient)
                # Retrieve parent building IRI if already instantiated, otherwise create            
                if parent:
                    parent_iri = parent
                else:
                    parent_iri = OBE + 'Building_' + str(uuid.uuid4())

            #
            # Check if same EPC is already instantiated for UPRN
            #
            query = get_latest_epc_and_property_iri_for_uprn(uprn)
            instantiated = kgclient.performQuery(query)

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
                        geographies = kgclient.performQuery(query)
                        if geographies:
                            data_to_instantiate['postcode_iri'] = geographies[0].get('postcode')
                            data_to_instantiate['district_iri'] = geographies[0].get('district')
                    # Add potential parent building
                    data_to_instantiate['parent_iri'] = parent_iri

                    logger.info('No EPC data instantiated for UPRN. Instantiate data ... ')
                    insert_query = instantiate_epc_data(**data_to_instantiate)
                    kgclient.performUpdate(insert_query)
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
                    kgclient.performUpdate(update_query)
                    updates = (0, 1)
        else:
            logger.info('No associated UPRNs are instantiated in OntoCityGml endpoint.')
    else:
        logger.info('No EPC data available for provided lodgement identifier.')

    return updates
    

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
        if nr: nr = nr[0]
        street = addr.replace(nr, '')
        street = street.replace(',', '').replace(';','')
        street = street.strip().upper()
        if street and nr:
            break

    return street, nr


def retrieve_ocgml_uprns(uprn: str = '', query_endpoint: str = '',
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


if __name__ == '__main__':

    ocgml_endpoint = 'http://localhost:9999/blazegraph/namespace/kings-lynn/sparql'

    # Retrieve individual EPC data for flats within same parent building
    # UPRN 10013002176
    a,b = instantiate_data_for_certificate('815707079922012071918412040218942', ocgml_endpoint=ocgml_endpoint)
    # UPRN 10013002177
    c,d = instantiate_data_for_certificate('1587310959332017110616393580078797', ocgml_endpoint=ocgml_endpoint)
    # UPRN 10013002178
    e,f = instantiate_data_for_certificate('323066450042009070814263262410988', ocgml_endpoint=ocgml_endpoint)
    # UPRN 10013002181
    g,h = instantiate_data_for_certificate('1793915247032020032008085476978708', ocgml_endpoint=ocgml_endpoint)
    # UPRN 10013002179
    # UPRN 10013002180


    #uprns = retrieve_ocgml_uprns('10013004624', 'http://localhost:9999/blazegraph/namespace/kings-lynn/sparql')
    #bldg = retrieve_parent_building(uprns)
